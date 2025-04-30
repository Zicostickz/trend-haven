;; Reputation Contract
;; This contract manages reputation scores for buyers and sellers in the TrendHaven marketplace
;; It tracks user reputation based on transaction history, reviews, and platform behavior
;; and enables users to build trust within the ecosystem over time.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-USER-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-RATING (err u1002))
(define-constant ERR-SELF-RATING (err u1003))
(define-constant ERR-DUPLICATE-REVIEW (err u1004))
(define-constant ERR-NO-ASSOCIATED-TRANSACTION (err u1005))
(define-constant ERR-MINIMUM-REPUTATION-REQUIRED (err u1006))
(define-constant ERR-ADMIN-ONLY (err u1007))

;; Data variables
;; Platform admin - initially set to contract deployer
(define-data-var contract-admin principal tx-sender)

;; Constants
(define-constant INITIAL-REPUTATION u50) ;; Initial reputation score for new users (0-100 scale)
(define-constant MIN-RATING u1)
(define-constant MAX-RATING u5)
(define-constant RATING-WEIGHT u3) ;; Multiplier for ratings in reputation calculation
(define-constant TRANSACTION-WEIGHT u2) ;; Multiplier for completed transactions
(define-constant SELLER-MULTIPLIER u2) ;; Sellers build reputation faster with this multiplier
(define-constant BUYER-MULTIPLIER u1) ;; Buyers reputation weight

;; Data maps
;; Main reputation storage for all users
(define-map user-reputation 
  { user: principal } 
  { 
    score: uint,                    ;; Current reputation score (0-100)
    total-transactions: uint,       ;; Number of completed transactions
    ratings-received: uint,         ;; Total number of ratings received
    cumulative-rating: uint,        ;; Sum of all ratings (used for avg calculation)
    is-seller: bool,                ;; Whether this user is a seller
    last-updated: uint              ;; Block height of last update
  }
)

;; Records of reviews given
(define-map reviews
  { reviewer: principal, reviewee: principal }
  {
    rating: uint,                   ;; Rating given (1-5)
    transaction-id: (optional principal), ;; Associated transaction ID if applicable
    timestamp: uint                 ;; Block height when review was given
  }
)

;; Maps transaction IDs to participants
(define-map transaction-participants
  { transaction-id: principal }
  {
    buyer: principal,
    seller: principal,
    is-completed: bool
  }
)

;; Private functions

;; Initialize a new user in the reputation system
(define-private (initialize-user (user principal) (is-seller bool))
  (map-insert user-reputation 
    { user: user }
    { 
      score: INITIAL-REPUTATION,
      total-transactions: u0,
      ratings-received: u0,
      cumulative-rating: u0,
      is-seller: is-seller,
      last-updated: block-height
    }
  )
)

;; Calculate the new reputation score based on current data
(define-private (calculate-reputation (current-score uint) 
                                     (total-transactions uint) 
                                     (ratings-received uint) 
                                     (cumulative-rating uint)
                                     (is-seller bool))
  (let
    (
      (avg-rating (if (> ratings-received u0)
                     (/ cumulative-rating ratings-received)
                     u0))
      (transaction-factor (if (> total-transactions u0) 
                            (* total-transactions TRANSACTION-WEIGHT)
                            u0))
      (rating-factor (if (> ratings-received u0)
                        (* avg-rating RATING-WEIGHT)
                        u0))
      (role-multiplier (if is-seller SELLER-MULTIPLIER BUYER-MULTIPLIER))
      (base-calculation (+ transaction-factor rating-factor))
    )
    ;; Cap at 100, ensure score is at least 1 unless explicitly banned (0)
    (if (> current-score u0)
      (min u100 (+ (/ base-calculation u10) (* role-multiplier (/ current-score u2))))
      u0)
  )
)

;; Read-only functions

;; Get a user's current reputation data
(define-read-only (get-reputation (user principal))
  (match (map-get? user-reputation { user: user })
    rep-data rep-data
    (begin
      (initialize-user user false)
      (map-get? user-reputation { user: user })
    )
  )
)

;; Get a user's reputation score only
(define-read-only (get-reputation-score (user principal))
  (match (map-get? user-reputation { user: user })
    rep-data (get score rep-data)
    INITIAL-REPUTATION
  )
)

;; Get the average rating a user has received
(define-read-only (get-average-rating (user principal))
  (match (map-get? user-reputation { user: user })
    rep-data (if (> (get ratings-received rep-data) u0)
                (/ (get cumulative-rating rep-data) (get ratings-received rep-data))
                u0)
    u0
  )
)

;; Check if a specific review exists
(define-read-only (has-reviewed (reviewer principal) (reviewee principal))
  (is-some (map-get? reviews { reviewer: reviewer, reviewee: reviewee }))
)

;; Check if a transaction exists and its participants
(define-read-only (get-transaction-participants (transaction-id principal))
  (map-get? transaction-participants { transaction-id: transaction-id })
)

;; Check contract admin
(define-read-only (get-contract-admin)
  (var-get contract-admin)
)

;; Public functions

;; Register a new transaction between buyer and seller
(define-public (register-transaction (transaction-id principal) (buyer principal) (seller principal))
  (begin
    ;; Only admin can register transactions for now
    ;; This would likely be connected to the marketplace contract in a real implementation
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-ADMIN-ONLY)
    
    ;; Register the transaction
    (map-set transaction-participants
      { transaction-id: transaction-id }
      {
        buyer: buyer,
        seller: seller,
        is-completed: false
      }
    )
    
    ;; Make sure both users are initialized in the system
    (match (map-get? user-reputation { user: buyer })
      buyer-data true
      (initialize-user buyer false)
    )
    
    (match (map-get? user-reputation { user: seller })
      seller-data true
      (initialize-user seller true)
    )
    
    (ok true)
  )
)

;; Mark a transaction as complete
(define-public (complete-transaction (transaction-id principal))
  (let
    (
      (tx-data (unwrap! (map-get? transaction-participants { transaction-id: transaction-id }) ERR-USER-NOT-FOUND))
      (is-admin (is-eq tx-sender (var-get contract-admin)))
    )
    ;; Only admin or seller can mark as complete
    (asserts! (or is-admin (is-eq tx-sender (get seller tx-data))) ERR-NOT-AUTHORIZED)
    
    ;; Ensure transaction isn't already completed
    (asserts! (not (get is-completed tx-data)) ERR-DUPLICATE-REVIEW)
    
    ;; Update transaction status
    (map-set transaction-participants
      { transaction-id: transaction-id }
      (merge tx-data { is-completed: true })
    )
    
    ;; Update buyer reputation
    (match (map-get? user-reputation { user: (get buyer tx-data) })
      buyer-data
      (let
        (
          (updated-tx-count (+ (get total-transactions buyer-data) u1))
          (new-score (calculate-reputation 
                      (get score buyer-data)
                      updated-tx-count
                      (get ratings-received buyer-data)
                      (get cumulative-rating buyer-data)
                      false))
        )
        (map-set user-reputation
          { user: (get buyer tx-data) }
          (merge buyer-data {
            score: new-score,
            total-transactions: updated-tx-count,
            last-updated: block-height
          })
        )
      )
      (initialize-user (get buyer tx-data) false)
    )
    
    ;; Update seller reputation
    (match (map-get? user-reputation { user: (get seller tx-data) })
      seller-data
      (let
        (
          (updated-tx-count (+ (get total-transactions seller-data) u1))
          (new-score (calculate-reputation 
                      (get score seller-data)
                      updated-tx-count
                      (get ratings-received seller-data)
                      (get cumulative-rating seller-data)
                      true))
        )
        (map-set user-reputation
          { user: (get seller tx-data) }
          (merge seller-data {
            score: new-score,
            total-transactions: updated-tx-count,
            last-updated: block-height
          })
        )
      )
      (initialize-user (get seller tx-data) true)
    )
    
    (ok true)
  )
)

;; Submit a review for another user connected to a transaction
(define-public (submit-transaction-review (transaction-id principal) (rating uint) (reviewee principal))
  (let
    (
      (tx-data (unwrap! (map-get? transaction-participants { transaction-id: transaction-id }) ERR-NO-ASSOCIATED-TRANSACTION))
      (valid-participant (or (is-eq reviewee (get buyer tx-data)) (is-eq reviewee (get seller tx-data))))
      (is-reviewer-participant (or (is-eq tx-sender (get buyer tx-data)) (is-eq tx-sender (get seller tx-data))))
    )
    ;; Validations
    (asserts! valid-participant ERR-USER-NOT-FOUND)
    (asserts! is-reviewer-participant ERR-NOT-AUTHORIZED)
    (asserts! (get is-completed tx-data) ERR-NO-ASSOCIATED-TRANSACTION)
    (asserts! (not (is-eq tx-sender reviewee)) ERR-SELF-RATING)
    (asserts! (and (>= rating MIN-RATING) (<= rating MAX-RATING)) ERR-INVALID-RATING)
    (asserts! (not (has-reviewed tx-sender reviewee)) ERR-DUPLICATE-REVIEW)
    
    ;; Record the review
    (map-set reviews
      { reviewer: tx-sender, reviewee: reviewee }
      {
        rating: rating,
        transaction-id: (some transaction-id),
        timestamp: block-height
      }
    )
    
    ;; Update reviewee's reputation data
    (match (map-get? user-reputation { user: reviewee })
      user-data
      (let
        (
          (new-ratings-count (+ (get ratings-received user-data) u1))
          (new-cumulative-rating (+ (get cumulative-rating user-data) rating))
          (is-seller-role (get is-seller user-data))
          (new-score (calculate-reputation
                      (get score user-data)
                      (get total-transactions user-data)
                      new-ratings-count
                      new-cumulative-rating
                      is-seller-role))
        )
        (map-set user-reputation
          { user: reviewee }
          (merge user-data {
            score: new-score,
            ratings-received: new-ratings-count,
            cumulative-rating: new-cumulative-rating,
            last-updated: block-height
          })
        )
      )
      (initialize-user reviewee (is-eq reviewee (get seller tx-data)))
    )
    
    (ok true)
  )
)

;; Submit a general review (not tied to a specific transaction)
(define-public (submit-general-review (reviewee principal) (rating uint))
  (let
    (
      (reviewer-score (get-reputation-score tx-sender))
    )
    ;; Validations
    (asserts! (not (is-eq tx-sender reviewee)) ERR-SELF-RATING)
    (asserts! (and (>= rating MIN-RATING) (<= rating MAX-RATING)) ERR-INVALID-RATING)
    (asserts! (not (has-reviewed tx-sender reviewee)) ERR-DUPLICATE-REVIEW)
    ;; Require some reputation to prevent fake accounts from submitting general reviews
    (asserts! (>= reviewer-score u60) ERR-MINIMUM-REPUTATION-REQUIRED)
    
    ;; Record the review
    (map-set reviews
      { reviewer: tx-sender, reviewee: reviewee }
      {
        rating: rating,
        transaction-id: none,
        timestamp: block-height
      }
    )
    
    ;; Update reviewee's reputation data
    (match (map-get? user-reputation { user: reviewee })
      user-data
      (let
        (
          (new-ratings-count (+ (get ratings-received user-data) u1))
          (new-cumulative-rating (+ (get cumulative-rating user-data) rating))
          (is-seller-role (get is-seller user-data))
          (new-score (calculate-reputation
                      (get score user-data)
                      (get total-transactions user-data)
                      new-ratings-count
                      new-cumulative-rating
                      is-seller-role))
        )
        (map-set user-reputation
          { user: reviewee }
          (merge user-data {
            score: new-score,
            ratings-received: new-ratings-count,
            cumulative-rating: new-cumulative-rating,
            last-updated: block-height
          })
        )
      )
      (initialize-user reviewee false)
    )
    
    (ok true)
  )
)

;; Admin function to update a user's seller status
(define-public (set-user-as-seller (user principal) (is-seller bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-ADMIN-ONLY)
    
    (match (map-get? user-reputation { user: user })
      user-data
      (map-set user-reputation
        { user: user }
        (merge user-data {
          is-seller: is-seller,
          last-updated: block-height
        })
      )
      (initialize-user user is-seller)
    )
    
    (ok true)
  )
)

;; Admin function to transfer admin privileges
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-ADMIN-ONLY)
    (var-set contract-admin new-admin)
    (ok true)
  )
)

;; Admin function to adjust a user's reputation in case of policy violations
(define-public (admin-adjust-reputation (user principal) (new-score uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-ADMIN-ONLY)
    (asserts! (<= new-score u100) ERR-INVALID-RATING)
    
    (match (map-get? user-reputation { user: user })
      user-data
      (map-set user-reputation
        { user: user }
        (merge user-data {
          score: new-score,
          last-updated: block-height
        })
      )
      (initialize-user user false)
    )
    
    (ok true)
  )
)