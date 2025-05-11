;; trend-tracker
;;
;; This contract implements algorithms and data structures to identify trending products
;; in the TrendHaven marketplace based on user activity metrics. The system analyzes 
;; product views, purchase frequency, and user engagement to calculate and maintain
;; trend scores, automatically refreshing trending status at configurable intervals.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PARAMETERS (err u102))
(define-constant ERR-ALREADY-TRACKED (err u103))
(define-constant ERR-TOO-EARLY-REFRESH (err u104))
;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant VIEW-WEIGHT u5) ;; Weight factor for views in trend algorithm
(define-constant PURCHASE-WEIGHT u20) ;; Weight factor for purchases in trend algorithm
(define-constant ENGAGEMENT-WEIGHT u10) ;; Weight factor for engagement in trend algorithm
(define-constant DEFAULT-TREND-THRESHOLD u1000) ;; Default threshold to be considered trending
(define-constant SECONDS-IN-DAY u86400) ;; Number of seconds in a day for time calculations
;; Data maps and vars
(define-map products
  { product-id: uint }
  {
    name: (string-ascii 100),
    creator: principal,
    view-count: uint,
    purchase-count: uint,
    engagement-count: uint,
    trend-score: uint,
    is-trending: bool,
    last-updated: uint,
    created-at: uint,
  }
)
(define-map trending-products
  { score: uint }
  { product-id: uint }
)
(define-data-var trend-threshold uint DEFAULT-TREND-THRESHOLD)
(define-data-var last-trend-refresh uint u0)
(define-data-var refresh-interval uint SECONDS-IN-DAY)
(define-data-var admin principal CONTRACT-OWNER)
(define-data-var total-tracked-products uint u0)
;; Private functions
;; Calculate trend score based on weighted metrics
(define-private (calculate-trend-score
    (views uint)
    (purchases uint)
    (engagement uint)
  )
  (+ (* views VIEW-WEIGHT) (* purchases PURCHASE-WEIGHT)
    (* engagement ENGAGEMENT-WEIGHT)
  )
)

;; Check if the given principal is authorized as admin
(define-private (is-authorized (user principal))
  (or
    (is-eq user CONTRACT-OWNER)
    (is-eq user (var-get admin))
  )
)

;; Update trending status based on current trend score
(define-private (update-trend-status
    (product-id uint)
    (trend-score uint)
  )
  (let (
      (current-threshold (var-get trend-threshold))
      (is-trend (>= trend-score current-threshold))
      (product-data (unwrap! (map-get? products { product-id: product-id })
        ERR-PRODUCT-NOT-FOUND
      ))
    )
    ;; If trending status changed, update the trending-products map
    (if (not (is-eq is-trend (get is-trending product-data)))
      (begin
        (if is-trend
          ;; Add to trending products
          (map-set trending-products { score: trend-score } { product-id: product-id })
          ;; Remove from trending products if no longer trending
          (map-delete trending-products { score: (get trend-score product-data) })
        )
        ;; Update the product data with new trending status
        (ok (map-set products { product-id: product-id }
          (merge product-data {
            trend-score: trend-score,
            is-trending: is-trend,
            last-updated: block-height,
          })
        ))
      )
      ;; Just update the trend score
      (ok (map-set products { product-id: product-id }
        (merge product-data {
          trend-score: trend-score,
          last-updated: block-height,
        })
      ))
    )
  )
)

;; Read-only functions
;; Get detailed product data
(define-read-only (get-product-details (product-id uint))
  (map-get? products { product-id: product-id })
)

;; Get product trend score
(define-read-only (get-product-trend-score (product-id uint))
  (match (map-get? products { product-id: product-id })
    product-data (ok (get trend-score product-data))
    (err ERR-PRODUCT-NOT-FOUND)
  )
)

;; Check if product is currently trending
(define-read-only (is-product-trending (product-id uint))
  (match (map-get? products { product-id: product-id })
    product-data (ok (get is-trending product-data))
    (err ERR-PRODUCT-NOT-FOUND)
  )
)

;; Return current trend threshold
(define-read-only (get-trend-threshold)
  (var-get trend-threshold)
)

;; Get total number of products being tracked
(define-read-only (get-total-tracked-products)
  (var-get total-tracked-products)
)

;; Get when trends were last refreshed
(define-read-only (get-last-trend-refresh)
  (var-get last-trend-refresh)
)

;; Public functions
;; Add a new product to be tracked
(define-public (register-product
    (product-id uint)
    (name (string-ascii 100))
  )
  (let ((existing-product (map-get? products { product-id: product-id })))
    ;; Check if product already exists
    (asserts! (is-none existing-product) ERR-ALREADY-TRACKED)
    ;; Ensure name is not empty
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)
    ;; Register the new product
    (map-set products { product-id: product-id } {
      name: name,
      creator: tx-sender,
      view-count: u0,
      purchase-count: u0,
      engagement-count: u0,
      trend-score: u0,
      is-trending: false,
      last-updated: block-height,
      created-at: block-height,
    })
    ;; Increment total tracked products
    (var-set total-tracked-products (+ (var-get total-tracked-products) u1))
    (ok true)
  )
)

;; Record a view event for a product
(define-public (record-product-view (product-id uint))
  (let (
      (product-data (unwrap! (map-get? products { product-id: product-id })
        ERR-PRODUCT-NOT-FOUND
      ))
      (new-view-count (+ (get view-count product-data) u1))
    )
    ;; Update product with increased view count
    (map-set products { product-id: product-id }
      (merge product-data { view-count: new-view-count })
    )
    ;; Calculate and update trend score
    (update-trend-status product-id
      (calculate-trend-score new-view-count (get purchase-count product-data)
        (get engagement-count product-data)
      ))
  )
)

;; Record a purchase event for a product
(define-public (record-product-purchase (product-id uint))
  (let (
      (product-data (unwrap! (map-get? products { product-id: product-id })
        ERR-PRODUCT-NOT-FOUND
      ))
      (new-purchase-count (+ (get purchase-count product-data) u1))
    )
    ;; Update product with increased purchase count
    (map-set products { product-id: product-id }
      (merge product-data { purchase-count: new-purchase-count })
    )
    ;; Calculate and update trend score
    (update-trend-status product-id
      (calculate-trend-score (get view-count product-data) new-purchase-count
        (get engagement-count product-data)
      ))
  )
)

;; Record an engagement event for a product (comments, shares, etc.)
(define-public (record-product-engagement (product-id uint))
  (let (
      (product-data (unwrap! (map-get? products { product-id: product-id })
        ERR-PRODUCT-NOT-FOUND
      ))
      (new-engagement-count (+ (get engagement-count product-data) u1))
    )
    ;; Update product with increased engagement count
    (map-set products { product-id: product-id }
      (merge product-data { engagement-count: new-engagement-count })
    )
    ;; Calculate and update trend score
    (update-trend-status product-id
      (calculate-trend-score (get view-count product-data)
        (get purchase-count product-data) new-engagement-count
      ))
  )
)

;; Refresh trending status for all products
(define-public (refresh-trending-status)
  (let (
      (current-time block-height)
      (last-refresh (var-get last-trend-refresh))
      (interval (var-get refresh-interval))
    )
    ;; Check if enough time has passed since last refresh
    (asserts!
      (or
        (is-eq last-refresh u0)
        (>= (- current-time last-refresh) interval)
        (is-authorized tx-sender)
      )
      ERR-TOO-EARLY-REFRESH
    )
    ;; Update last refresh time
    (var-set last-trend-refresh current-time)
    ;; Note: In a real implementation, we would iterate through all products
    ;; and recalculate trends here. Since Clarity doesn't support loops,
    ;; this would require multiple transactions or batch processing.
    ;; For this contract, we'll assume trending status is updated
    ;; incrementally with each interaction.
    (ok true)
  )
)

;; Admin function to update the trend threshold
(define-public (set-trend-threshold (new-threshold uint))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> new-threshold u0) ERR-INVALID-PARAMETERS)
    (var-set trend-threshold new-threshold)
    (ok true)
  )
)

;; Admin function to update refresh interval
(define-public (set-refresh-interval (new-interval uint))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> new-interval u0) ERR-INVALID-PARAMETERS)
    (var-set refresh-interval new-interval)
    (ok true)
  )
)

;; Admin function to transfer admin rights
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (var-set admin new-admin)
    (ok true)
  )
)
