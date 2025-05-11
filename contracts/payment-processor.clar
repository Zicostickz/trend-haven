;; payment-processor
;; A contract that securely manages the flow of funds between buyers, sellers, and the platform
;; with escrow functionality for the Trend Haven marketplace
;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-ESCROW (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-DOES-NOT-EXIST (err u103))
(define-constant ERR-INVALID-STATE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-INVALID-TOKEN (err u106))
(define-constant ERR-INSUFFICIENT-BALANCE (err u107))
(define-constant ERR-PAYMENT-FAILED (err u108))
(define-constant ERR-REFUND-FAILED (err u109))
;; Payment states
(define-constant STATE-PENDING u1)
(define-constant STATE-COMPLETED u2)
(define-constant STATE-REFUNDED u3)
(define-constant STATE-DISPUTED u4)
(define-constant STATE-RESOLVED u5)
;; Platform configuration
(define-data-var platform-admin principal tx-sender)
(define-data-var platform-fee-percent uint u250) ;; 2.5% (represented as basis points: 250/10000)
(define-data-var platform-treasury principal tx-sender)
(define-data-var escrow-timeout uint u1440) ;; Default escrow timeout in blocks (approximately 10 days)
;; Data structures
;; Stores information about a payment in escrow
(define-map escrow-payments
  { payment-id: (string-ascii 36) }
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    fee-amount: uint,
    token-contract: (optional principal),
    token-id: (optional uint),
    state: uint,
    created-at-block: uint,
    completed-at-block: (optional uint),
    note: (optional (string-utf8 256)),
  }
)
;; Tracks all payment IDs for a user (as buyer)
(define-map user-purchases
  { user: principal }
  { payment-ids: (list 100 (string-ascii 36)) }
)
;; Tracks all payment IDs for a user (as seller)
(define-map user-sales
  { user: principal }
  { payment-ids: (list 100 (string-ascii 36)) }
)
;; Supported SIP-010 tokens
(define-map supported-tokens
  { token-contract: principal }
  { enabled: bool }
)
;; Private functions
;; Calculate platform fee for a given amount
(define-private (calculate-fee (amount uint))
  (/ (* amount (var-get platform-fee-percent)) u10000)
)


;; Check if a token is supported
(define-private (is-token-supported (token-contract principal))
  (default-to false
    (get enabled (map-get? supported-tokens { token-contract: token-contract }))
  )
)




;; Check if caller is the platform admin
(define-private (is-admin)
  (is-eq tx-sender (var-get platform-admin))
)

;; Read-only functions
;; Get payment details
(define-read-only (get-payment-details (payment-id (string-ascii 36)))
  (map-get? escrow-payments { payment-id: payment-id })
)

;; Get all purchases for a user
(define-read-only (get-user-purchases (user principal))
  (default-to { payment-ids: (list) } (map-get? user-purchases { user: user }))
)

;; Get all sales for a user
(define-read-only (get-user-sales (user principal))
  (default-to { payment-ids: (list) } (map-get? user-sales { user: user }))
)

;; Get current platform fee percentage
(define-read-only (get-platform-fee)
  (var-get platform-fee-percent)
)

;; Check if token is supported
(define-read-only (check-token-support (token-contract principal))
  (is-token-supported token-contract)
)

;; Get platform admin
(define-read-only (get-platform-admin)
  (var-get platform-admin)
)

;; Admin functions
;; Add supported token (admin only)
(define-public (add-supported-token (token-contract principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (map-set supported-tokens { token-contract: token-contract } { enabled: true })
    (ok true)
  )
)

;; Remove supported token (admin only)
(define-public (remove-supported-token (token-contract principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (map-set supported-tokens { token-contract: token-contract } { enabled: false })
    (ok true)
  )
)

;; Update platform fee percentage (admin only)
(define-public (set-platform-fee (new-fee-percent uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    ;; Ensure fee is reasonable (max 10%)
    (asserts! (<= new-fee-percent u1000) ERR-INVALID-AMOUNT)
    (var-set platform-fee-percent new-fee-percent)
    (ok true)
  )
)

;; Update platform treasury address (admin only)
(define-public (set-platform-treasury (new-treasury principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (var-set platform-treasury new-treasury)
    (ok true)
  )
)

;; Update platform admin (current admin only)
(define-public (set-platform-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (var-set platform-admin new-admin)
    (ok true)
  )
)

;; Update escrow timeout (admin only)
(define-public (set-escrow-timeout (new-timeout uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (var-set escrow-timeout new-timeout)
    (ok true)
  )
)
