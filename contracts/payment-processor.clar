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
    note: (optional (string-utf8 256))
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

;; Get the fee details from a payment
(define-private (get-payment (payment-id (string-ascii 36)))
  (match (map-get? escrow-payments { payment-id: payment-id })
    payment payment
    (begin
      (print (concat "Payment not found: " payment-id))
      none
    )
  )
)

;; Check if a token is supported
(define-private (is-token-supported (token-contract principal))
  (default-to false (get enabled (map-get? supported-tokens { token-contract: token-contract })))
)

;; Add a payment ID to a user's list (as buyer)
(define-private (add-purchase (user principal) (payment-id (string-ascii 36)))
  (match (map-get? user-purchases { user: user })
    existing-data (map-set user-purchases 
                          { user: user }
                          { payment-ids: (append (get payment-ids existing-data) payment-id) })
    (map-set user-purchases
            { user: user }
            { payment-ids: (list payment-id) })
  )
)

;; Add a payment ID to a user's list (as seller)
(define-private (add-sale (user principal) (payment-id (string-ascii 36)))
  (match (map-get? user-sales { user: user })
    existing-data (map-set user-sales 
                          { user: user }
                          { payment-ids: (append (get payment-ids existing-data) payment-id) })
    (map-set user-sales
            { user: user }
            { payment-ids: (list payment-id) })
  )
)

;; Check if caller is the platform admin
(define-private (is-admin)
  (is-eq tx-sender (var-get platform-admin))
)

;; Transfer SIP-010 tokens
(define-private (transfer-token (token-contract principal) (amount uint) (sender principal) (recipient principal))
  (contract-call? token-contract transfer amount sender recipient none)
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

;; Public functions

;; Create a new escrow payment with STX
(define-public (create-stx-payment (payment-id (string-ascii 36)) (seller principal) (amount uint) (note (optional (string-utf8 256))))
  (let
    (
      (fee (calculate-fee amount))
      (total-amount (+ amount fee))
      (buyer tx-sender)
    )
    ;; Validations
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? escrow-payments { payment-id: payment-id })) ERR-ALREADY-EXISTS)
    
    ;; Transfer STX to contract
    (match (stx-transfer? total-amount buyer (as-contract tx-sender))
      success (begin
        ;; Create payment record
        (map-set escrow-payments
          { payment-id: payment-id }
          {
            buyer: buyer,
            seller: seller,
            amount: amount,
            fee-amount: fee,
            token-contract: none,
            token-id: none,
            state: STATE-PENDING,
            created-at-block: block-height,
            completed-at-block: none,
            note: note
          }
        )
        
        ;; Update user records
        (add-purchase buyer payment-id)
        (add-sale seller payment-id)
        
        (ok true)
      )
      error (err ERR-PAYMENT-FAILED)
    )
  )
)

;; Create a new escrow payment with SIP-010 token
(define-public (create-token-payment 
                (payment-id (string-ascii 36)) 
                (seller principal) 
                (amount uint) 
                (token-contract principal)
                (note (optional (string-utf8 256))))
  (let
    (
      (fee (calculate-fee amount))
      (total-amount (+ amount fee))
      (buyer tx-sender)
    )
    ;; Validations
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-token-supported token-contract) ERR-INVALID-TOKEN)
    (asserts! (is-none (map-get? escrow-payments { payment-id: payment-id })) ERR-ALREADY-EXISTS)
    
    ;; Transfer tokens to contract
    (match (transfer-token token-contract total-amount buyer (as-contract tx-sender))
      success (begin
        ;; Create payment record
        (map-set escrow-payments
          { payment-id: payment-id }
          {
            buyer: buyer,
            seller: seller,
            amount: amount,
            fee-amount: fee,
            token-contract: (some token-contract),
            token-id: none,
            state: STATE-PENDING,
            created-at-block: block-height,
            completed-at-block: none,
            note: note
          }
        )
        
        ;; Update user records
        (add-purchase buyer payment-id)
        (add-sale seller payment-id)
        
        (ok true)
      )
      error (err ERR-PAYMENT-FAILED)
    )
  )
)

;; Confirm delivery and release payment to seller
(define-public (confirm-delivery (payment-id (string-ascii 36)))
  (match (map-get? escrow-payments { payment-id: payment-id })
    payment (begin
      ;; Validations
      (asserts! (is-eq (get buyer payment) tx-sender) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get state payment) STATE-PENDING) ERR-INVALID-STATE)
      
      ;; Process payment
      (if (is-some (get token-contract payment))
        ;; Handle token payment
        (let 
          (
            (token-contract (unwrap! (get token-contract payment) ERR-INVALID-TOKEN))
          )
          (try! (as-contract (transfer-token token-contract (get amount payment) tx-sender (get seller payment))))
          (try! (as-contract (transfer-token token-contract (get fee-amount payment) tx-sender (var-get platform-treasury))))
        )
        ;; Handle STX payment
        (begin
          (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get seller payment))))
          (try! (as-contract (stx-transfer? (get fee-amount payment) tx-sender (var-get platform-treasury))))
        )
      )
      
      ;; Update payment state
      (map-set escrow-payments
        { payment-id: payment-id }
        (merge payment {
          state: STATE-COMPLETED,
          completed-at-block: (some block-height)
        })
      )
      
      (ok true)
    )
    (err ERR-DOES-NOT-EXIST)
  )
)

;; Request a refund (can only be initiated by buyer)
(define-public (request-refund (payment-id (string-ascii 36)) (reason (string-utf8 256)))
  (match (map-get? escrow-payments { payment-id: payment-id })
    payment (begin
      ;; Validations
      (asserts! (is-eq (get buyer payment) tx-sender) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get state payment) STATE-PENDING) ERR-INVALID-STATE)
      
      ;; Update payment state
      (map-set escrow-payments
        { payment-id: payment-id }
        (merge payment {
          state: STATE-DISPUTED,
          note: (some reason)
        })
      )
      
      (ok true)
    )
    (err ERR-DOES-NOT-EXIST)
  )
)

;; Process refund (requires seller or admin approval)
(define-public (process-refund (payment-id (string-ascii 36)))
  (match (map-get? escrow-payments { payment-id: payment-id })
    payment (begin
      ;; Validations
      (asserts! (or 
                  (is-eq (get seller payment) tx-sender) 
                  (is-admin)
                ) ERR-UNAUTHORIZED)
      (asserts! (or 
                  (is-eq (get state payment) STATE-DISPUTED) 
                  (is-eq (get state payment) STATE-PENDING)
                ) ERR-INVALID-STATE)
      
      ;; Process refund
      (if (is-some (get token-contract payment))
        ;; Handle token refund
        (let 
          (
            (token-contract (unwrap! (get token-contract payment) ERR-INVALID-TOKEN))
            (total-amount (+ (get amount payment) (get fee-amount payment)))
          )
          (try! (as-contract (transfer-token token-contract total-amount tx-sender (get buyer payment))))
        )
        ;; Handle STX refund
        (let
          (
            (total-amount (+ (get amount payment) (get fee-amount payment)))
          )
          (try! (as-contract (stx-transfer? total-amount tx-sender (get buyer payment))))
        )
      )
      
      ;; Update payment state
      (map-set escrow-payments
        { payment-id: payment-id }
        (merge payment {
          state: STATE-REFUNDED,
          completed-at-block: (some block-height)
        })
      )
      
      (ok true)
    )
    (err ERR-DOES-NOT-EXIST)
  )
)

;; Admin can resolve a dispute (admin only)
(define-public (resolve-dispute 
                (payment-id (string-ascii 36)) 
                (in-favor-of-buyer bool)
                (resolution-note (string-utf8 256)))
  (match (map-get? escrow-payments { payment-id: payment-id })
    payment (begin
      ;; Validations
      (asserts! (is-admin) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get state payment) STATE-DISPUTED) ERR-INVALID-STATE)
      
      (if in-favor-of-buyer
        ;; Refund to buyer
        (if (is-some (get token-contract payment))
          ;; Handle token refund
          (let 
            (
              (token-contract (unwrap! (get token-contract payment) ERR-INVALID-TOKEN))
              (total-amount (+ (get amount payment) (get fee-amount payment)))
            )
            (try! (as-contract (transfer-token token-contract total-amount tx-sender (get buyer payment))))
          )
          ;; Handle STX refund
          (let
            (
              (total-amount (+ (get amount payment) (get fee-amount payment)))
            )
            (try! (as-contract (stx-transfer? total-amount tx-sender (get buyer payment))))
          )
        )
        ;; Release to seller
        (if (is-some (get token-contract payment))
          ;; Handle token payment
          (let 
            (
              (token-contract (unwrap! (get token-contract payment) ERR-INVALID-TOKEN))
            )
            (try! (as-contract (transfer-token token-contract (get amount payment) tx-sender (get seller payment))))
            (try! (as-contract (transfer-token token-contract (get fee-amount payment) tx-sender (var-get platform-treasury))))
          )
          ;; Handle STX payment
          (begin
            (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get seller payment))))
            (try! (as-contract (stx-transfer? (get fee-amount payment) tx-sender (var-get platform-treasury))))
          )
        )
      )
      
      ;; Update payment state
      (map-set escrow-payments
        { payment-id: payment-id }
        (merge payment {
          state: STATE-RESOLVED,
          completed-at-block: (some block-height),
          note: (some resolution-note)
        })
      )
      
      (ok true)
    )
    (err ERR-DOES-NOT-EXIST)
  )
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