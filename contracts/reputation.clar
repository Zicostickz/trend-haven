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
    score: uint, ;; Current reputation score (0-100)
    total-transactions: uint, ;; Number of completed transactions
    ratings-received: uint, ;; Total number of ratings received
    cumulative-rating: uint, ;; Sum of all ratings (used for avg calculation)
    is-seller: bool, ;; Whether this user is a seller
    last-updated: uint, ;; Block height of last update
  }
)
;; Records of reviews given
(define-map reviews
  {
    reviewer: principal,
    reviewee: principal,
  }
  {
    rating: uint, ;; Rating given (1-5)
    transaction-id: (optional principal), ;; Associated transaction ID if applicable
    timestamp: uint, ;; Block height when review was given
  }
)
;; Maps transaction IDs to participants
(define-map transaction-participants
  { transaction-id: principal }
  {
    buyer: principal,
    seller: principal,
    is-completed: bool,
  }
)
;; Private functions
;; Initialize a new user in the reputation system
(define-private (initialize-user
    (user principal)
    (is-seller bool)
  )
  (map-insert user-reputation { user: user } {
    score: INITIAL-REPUTATION,
    total-transactions: u0,
    ratings-received: u0,
    cumulative-rating: u0,
    is-seller: is-seller,
    last-updated: block-height,
  })
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
      u0
    )
    u0
  )
)

;; Check if a specific review exists
(define-read-only (has-reviewed
    (reviewer principal)
    (reviewee principal)
  )
  (is-some (map-get? reviews {
    reviewer: reviewer,
    reviewee: reviewee,
  }))
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
(define-public (register-transaction
    (transaction-id principal)
    (buyer principal)
    (seller principal)
  )
  (begin
    ;; Only admin can register transactions for now
    ;; This would likely be connected to the marketplace contract in a real implementation
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-ADMIN-ONLY)
    ;; Register the transaction
    (map-set transaction-participants { transaction-id: transaction-id } {
      buyer: buyer,
      seller: seller,
      is-completed: false,
    })
    ;; Make sure both users are initialized in the system
    (match (map-get? user-reputation { user: buyer })
      buyer-data
      true (initialize-user buyer false)
    )
    (match (map-get? user-reputation { user: seller })
      seller-data
      true (initialize-user seller true)
    )
    (ok true)
  )
)

;; Admin function to update a user's seller status
(define-public (set-user-as-seller
    (user principal)
    (is-seller bool)
  )
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-ADMIN-ONLY)
    (match (map-get? user-reputation { user: user })
      user-data (map-set user-reputation { user: user }
        (merge user-data {
          is-seller: is-seller,
          last-updated: block-height,
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
(define-public (admin-adjust-reputation
    (user principal)
    (new-score uint)
  )
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-ADMIN-ONLY)
    (asserts! (<= new-score u100) ERR-INVALID-RATING)
    (match (map-get? user-reputation { user: user })
      user-data (map-set user-reputation { user: user }
        (merge user-data {
          score: new-score,
          last-updated: block-height,
        })
      )
      (initialize-user user false)
    )
    (ok true)
  )
)
