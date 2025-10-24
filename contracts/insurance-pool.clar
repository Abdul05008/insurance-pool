;; insurance-pool.clar
;; Decentralized Insurance Pool for Smart Contracts on Stacks
;; - Users stake STX to provide insurance liquidity.
;; - Projects register insured contracts with premium payments.
;; - Claims can be submitted and approved by governance (admin or DAO in real version).
;; - Approved claims pay STX compensation from pool.
;; - Liquidity providers can withdraw their share of pool balance.

;; -------------------------------
;; CONSTANTS & ERRORS
;; -------------------------------

(define-constant BPS u10000)
(define-constant ERR_NOT_ADMIN u100)
(define-constant ERR_ZERO_AMOUNT u101)
(define-constant ERR_TRANSFER_FAIL u102)
(define-constant ERR_NOT_REGISTERED u103)
(define-constant ERR_ALREADY_REGISTERED u104)
(define-constant ERR_UNAUTHORIZED u105)
(define-constant ERR_INVALID_CLAIM u106)
(define-constant ERR_INSUFFICIENT_FUNDS u107)
(define-constant ERR_NOT_PROVIDER u108)
(define-constant ERR_NOT_APPROVED u109)
(define-constant ERR_ALREADY_PAID u110)

;; -------------------------------
;; STATE VARIABLES
;; -------------------------------

(define-data-var admin principal tx-sender)

;; total liquidity in pool (microSTX)
(define-data-var total-pool-balance uint u0)

;; total provider shares (for withdrawal proportionality)
(define-data-var total-shares uint u0)

;; mapping: liquidity provider -> share amount
(define-map providers { who: principal } { shares: uint })

;; mapping: insured contracts -> { owner, premium, coverage, active }
(define-map insured-contracts
  { contract: principal }
  { owner: principal, premium: uint, coverage: uint, active: bool })

;; claims registry
(define-map claims
  { claim-id: uint }
  { claimant: principal, contract: principal, amount: uint, approved: bool, paid: bool })

(define-data-var next-claim-id uint u1)

;; -------------------------------
;; ADMIN FUNCTIONS
;; -------------------------------

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (is-some (some new-admin)) (err ERR_NOT_ADMIN))
    (var-set admin new-admin)
    (ok true)))

;; Approve a claim (admin governance action)
(define-public (approve-claim (claim-id uint))
  (begin 
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (>= claim-id u0) (err ERR_INVALID_CLAIM))
    (match (map-get? claims { claim-id: claim-id })
      claim (ok (begin 
                 (map-set claims 
                         { claim-id: claim-id }
                         (merge claim { approved: true }))
                 (print { event: "claim-approved", id: claim-id })
                 true))
      (err ERR_INVALID_CLAIM))))


;; -------------------------------
;; LIQUIDITY PROVIDER FUNCTIONS
;; -------------------------------

;; Provide STX to the pool and receive shares
(define-public (deposit (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let ((total (var-get total-pool-balance))
          (current-shares (var-get total-shares)))
      (let ((shares (if (is-eq current-shares u0)
                        amount
                        (/ (* amount current-shares) total))))
        (map-set providers { who: tx-sender }
                 { shares: (+ shares (default-to u0 (get shares (map-get? providers { who: tx-sender })))) })
        (var-set total-pool-balance (+ total amount))
        (var-set total-shares (+ current-shares shares))
        (print { event: "liquidity-deposited", who: tx-sender, amount: amount, shares: shares })
        (ok shares)))))

;; Withdraw proportional share of the pool
(define-public (withdraw (share-amount uint))
  (begin
    (asserts! (> share-amount u0) (err ERR_ZERO_AMOUNT))
    (let ((prov? (map-get? providers { who: tx-sender })))
      (asserts! (is-some prov?) (err ERR_NOT_PROVIDER))
      (let ((prov (unwrap-panic prov?))
            (current-total-shares (var-get total-shares))
            (pool (var-get total-pool-balance)))
        (asserts! (>= (get shares prov) share-amount) (err ERR_INSUFFICIENT_FUNDS))
        (let ((withdrawal-amount (/ (* pool share-amount) current-total-shares)))
          ;; update provider + pool
          (map-set providers { who: tx-sender } { shares: (- (get shares prov) share-amount) })
          (var-set total-shares (- current-total-shares share-amount))
          (var-set total-pool-balance (- pool withdrawal-amount))
          ;; send STX
          (try! (stx-transfer? withdrawal-amount (as-contract tx-sender) tx-sender))
          (print { event: "liquidity-withdrawn", who: tx-sender, amount: withdrawal-amount })
          (ok withdrawal-amount))))))


;; -------------------------------
;; INSURANCE REGISTRATION
;; -------------------------------

;; Register a contract for insurance coverage by paying a premium
(define-public (register-contract (contract principal) (coverage uint) (premium uint))
  (begin
    (asserts! (> premium u0) (err ERR_ZERO_AMOUNT))
    (asserts! (> coverage u0) (err ERR_ZERO_AMOUNT))
    (asserts! (is-none (map-get? insured-contracts { contract: contract })) (err ERR_ALREADY_REGISTERED))
    (asserts! (is-some (some contract)) (err ERR_NOT_REGISTERED))
    ;; pay premium to pool
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (let ((contract-data { owner: tx-sender, premium: premium, coverage: coverage, active: true }))
      (map-set insured-contracts { contract: contract } contract-data)
      (var-set total-pool-balance (+ (var-get total-pool-balance) premium))
      (print { event: "contract-insured", contract: contract, owner: tx-sender, coverage: coverage })
      (ok true))))


;; Cancel insurance (no refund of premium)
(define-public (cancel-insurance (contract principal))
  (begin
    (asserts! (is-some (some contract)) (err ERR_NOT_REGISTERED))
    (match (map-get? insured-contracts { contract: contract })
      info (begin
             (asserts! (is-eq (get owner info) tx-sender) (err ERR_UNAUTHORIZED))
             (ok (begin
                  (map-set insured-contracts 
                           { contract: contract }
                           (merge info { active: false }))
                  (print { event: "insurance-cancelled", contract: contract })
                  true)))
      (err ERR_NOT_REGISTERED))))


;; -------------------------------
;; CLAIM HANDLING
;; -------------------------------

;; Submit a claim request (user reports contract failure)
(define-public (submit-claim (contract principal) (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    (asserts! (is-some (some contract)) (err ERR_NOT_REGISTERED))
    (match (map-get? insured-contracts { contract: contract })
      info (begin
            (asserts! (get active info) (err ERR_INVALID_CLAIM))
            (asserts! (<= amount (get coverage info)) (err ERR_INVALID_CLAIM))
            (let ((claim-id (var-get next-claim-id))
                  (claim-data { claimant: tx-sender, 
                              contract: contract, 
                              amount: amount, 
                              approved: false, 
                              paid: false }))
              (var-set next-claim-id (+ claim-id u1))
              (map-set claims { claim-id: claim-id } claim-data)
              (print { event: "claim-submitted", id: claim-id, claimant: tx-sender, amount: amount })
              (ok claim-id)))
      (err ERR_NOT_REGISTERED))))

;; Payout for approved claim
(define-public (payout (claim-id uint))
  (begin
    (asserts! (>= claim-id u0) (err ERR_INVALID_CLAIM))
    (match (map-get? claims { claim-id: claim-id })
      claim (begin
             (asserts! (get approved claim) (err ERR_NOT_APPROVED))
             (asserts! (not (get paid claim)) (err ERR_ALREADY_PAID))
             (let ((pool (var-get total-pool-balance)))
               (asserts! (>= pool (get amount claim)) (err ERR_INSUFFICIENT_FUNDS))
               (try! (stx-transfer? (get amount claim) (as-contract tx-sender) (get claimant claim)))
               (ok (begin 
                    (var-set total-pool-balance (- pool (get amount claim)))
                    (map-set claims 
                            { claim-id: claim-id }
                            (merge claim { paid: true }))
                    (print { event: "claim-paid", id: claim-id, amount: (get amount claim) })
                    true))))
      (err ERR_INVALID_CLAIM))))


;; -------------------------------
;; VIEW FUNCTIONS
;; -------------------------------

(define-read-only (get-pool-stats)
  (ok {
    total-balance: (var-get total-pool-balance),
    total-shares: (var-get total-shares)
  }))

(define-read-only (get-provider (who principal))
  (map-get? providers { who: who }))

(define-read-only (get-contract-insurance (contract principal))
  (map-get? insured-contracts { contract: contract }))

(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id }))
