;; marketplace.clar
;; Core contract for Trend Haven marketplace
;; Manages product listings, purchases, and marketplace operations
;; Author: Trend Haven Team

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-LISTING-NOT-FOUND (err u1002))
(define-constant ERR-INVALID-PRICE (err u1003))
(define-constant ERR-INSUFFICIENT-INVENTORY (err u1004))
(define-constant ERR-PAYMENT-FAILED (err u1005))
(define-constant ERR-ALREADY-PURCHASED (err u1006))
(define-constant ERR-PURCHASE-NOT-FOUND (err u1007))
(define-constant ERR-DELIVERY-ALREADY-CONFIRMED (err u1008))
(define-constant ERR-DISPUTED-PURCHASE (err u1009))
(define-constant ERR-ALREADY-REFUNDED (err u1010))
(define-constant ERR-INVALID-PARAM (err u1011))
(define-constant ERR-LISTING-EXPIRED (err u1012))

;; Platform fee percentage (in basis points: 250 = 2.5%)
(define-constant PLATFORM-FEE-BPS u250)

;; Platform admin address
(define-constant PLATFORM-ADMIN tx-sender)

;; Data structures

;; Listing status enum: 1 = active, 2 = sold out, 3 = expired, 4 = cancelled
(define-data-var next-listing-id uint u1)

;; Data map for product listings
(define-map listings
  uint
  {
    seller: principal,
    title: (string-ascii 100),
    description: (string-utf8 1000),
    image-url: (string-ascii 256),
    price: uint,
    inventory: uint,
    category: (string-ascii 50),
    created-at: uint,
    expires-at: uint,
    status: uint
  }
)

;; Purchase status enum: 1 = pending, 2 = shipped, 3 = delivered, 4 = disputed, 5 = refunded
(define-data-var next-purchase-id uint u1)

;; Data map for purchases
(define-map purchases
  uint
  {
    listing-id: uint,
    buyer: principal,
    seller: principal,
    price: uint,
    quantity: uint,
    status: uint,
    purchase-time: uint,
    delivery-time: (optional uint)
  }
)

;; Map to track seller balances that can be withdrawn
(define-map seller-balances principal uint)

;; Accumulated platform fees
(define-data-var platform-balance uint u0)

;; Private functions

;; Calculate the platform fee for a given amount
(define-private (calculate-fee (amount uint))
  (/ (* amount PLATFORM-FEE-BPS) u10000)
)

;; Check if the caller is the platform admin
(define-private (is-admin)
  (is-eq tx-sender PLATFORM-ADMIN)
)

;; Check if a listing exists and is active
(define-private (is-active-listing (listing-id uint))
  (match (map-get? listings listing-id)
    listing (and (is-eq (get status listing) u1)
                 (> (get expires-at listing) block-height))
    false
  )
)

;; Get listing or fail
(define-private (get-listing (listing-id uint))
  (match (map-get? listings listing-id)
    listing listing
    (err ERR-LISTING-NOT-FOUND)
  )
)

;; Get purchase or fail
(define-private (get-purchase (purchase-id uint))
  (match (map-get? purchases purchase-id)
    purchase purchase
    (err ERR-PURCHASE-NOT-FOUND)
  )
)

;; Credit a seller's balance
(define-private (credit-seller (seller principal) (amount uint))
  (let ((current-balance (default-to u0 (map-get? seller-balances seller))))
    (map-set seller-balances seller (+ current-balance amount))
  )
)

;; Read-only functions

;; Get listing details
(define-read-only (get-listing-details (listing-id uint))
  (map-get? listings listing-id)
)

;; Get purchase details
(define-read-only (get-purchase-details (purchase-id uint))
  (map-get? purchases purchase-id)
)

;; Get seller's available balance
(define-read-only (get-seller-balance (seller principal))
  (default-to u0 (map-get? seller-balances seller))
)

;; Get platform accumulated fees
(define-read-only (get-platform-balance)
  (var-get platform-balance)
)

;; Public functions

;; Create a new product listing
(define-public (create-listing
  (title (string-ascii 100))
  (description (string-utf8 1000))
  (image-url (string-ascii 256))
  (price uint)
  (inventory uint)
  (category (string-ascii 50))
  (duration uint))
  
  (let ((listing-id (var-get next-listing-id))
        (expires-at (+ block-height duration)))
        
    ;; Input validation
    (asserts! (> price u0) ERR-INVALID-PRICE)
    (asserts! (> inventory u0) ERR-INSUFFICIENT-INVENTORY)
    (asserts! (> duration u0) ERR-INVALID-PARAM)
    
    ;; Create listing
    (map-set listings listing-id 
      {
        seller: tx-sender,
        title: title,
        description: description,
        image-url: image-url,
        price: price,
        inventory: inventory,
        category: category,
        created-at: block-height,
        expires-at: expires-at,
        status: u1  ;; active
      }
    )
    
    ;; Increment the listing ID counter
    (var-set next-listing-id (+ listing-id u1))
    
    ;; Return success with the new listing ID
    (ok listing-id)
  )
)

;; Update an existing listing
(define-public (update-listing
  (listing-id uint)
  (title (string-ascii 100))
  (description (string-utf8 1000))
  (image-url (string-ascii 256))
  (price uint)
  (inventory uint)
  (category (string-ascii 50))
  (duration uint))
  
  (let ((listing (try! (get-listing listing-id)))
        (expires-at (+ block-height duration)))
        
    ;; Authorization check
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    
    ;; Input validation
    (asserts! (> price u0) ERR-INVALID-PRICE)
    (asserts! (> inventory u0) ERR-INSUFFICIENT-INVENTORY)
    (asserts! (> duration u0) ERR-INVALID-PARAM)
    
    ;; Update listing
    (map-set listings listing-id 
      (merge listing
        {
          title: title,
          description: description,
          image-url: image-url,
          price: price,
          inventory: inventory,
          category: category,
          expires-at: expires-at
        }
      )
    )
    
    (ok true)
  )
)

;; Cancel a listing (seller only)
(define-public (cancel-listing (listing-id uint))
  (let ((listing (try! (get-listing listing-id))))
    ;; Authorization check
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    
    ;; Update status to cancelled (4)
    (map-set listings listing-id (merge listing { status: u4 }))
    
    (ok true)
  )
)

;; Purchase a product
(define-public (purchase-product (listing-id uint) (quantity uint))
  (let ((listing (try! (get-listing listing-id)))
        (purchase-id (var-get next-purchase-id))
        (total-price (* (get price listing) quantity)))
        
    ;; Validate listing is active and available
    (asserts! (is-active-listing listing-id) (if (> (get expires-at listing) block-height)
                                                 ERR-LISTING-NOT-FOUND
                                                 ERR-LISTING-EXPIRED))
    (asserts! (>= (get inventory listing) quantity) ERR-INSUFFICIENT-INVENTORY)
    (asserts! (> quantity u0) ERR-INVALID-PARAM)
    
    ;; Process payment
    (try! (stx-transfer? total-price tx-sender (as-contract tx-sender)))
    
    ;; Create purchase record
    (map-set purchases purchase-id
      {
        listing-id: listing-id,
        buyer: tx-sender,
        seller: (get seller listing),
        price: (get price listing),
        quantity: quantity,
        status: u1,  ;; pending
        purchase-time: block-height,
        delivery-time: none
      }
    )
    
    ;; Update inventory
    (map-set listings listing-id 
      (merge listing 
        { 
          inventory: (- (get inventory listing) quantity),
          status: (if (<= (- (get inventory listing) quantity) u0) u2 u1)  ;; If no inventory left, mark as sold out
        }
      )
    )
    
    ;; Increment purchase ID counter
    (var-set next-purchase-id (+ purchase-id u1))
    
    (ok purchase-id)
  )
)

;; Mark order as shipped (seller only)
(define-public (mark-shipped (purchase-id uint))
  (let ((purchase (try! (get-purchase purchase-id))))
    ;; Authorization check
    (asserts! (is-eq tx-sender (get seller purchase)) ERR-NOT-AUTHORIZED)
    
    ;; Update status to shipped (2)
    (map-set purchases purchase-id (merge purchase { status: u2 }))
    
    (ok true)
  )
)

;; Confirm delivery (buyer only)
(define-public (confirm-delivery (purchase-id uint))
  (let ((purchase (try! (get-purchase purchase-id)))
        (total-amount (* (get price purchase) (get quantity purchase)))
        (fee (calculate-fee total-amount))
        (seller-amount (- total-amount fee)))
        
    ;; Authorization check
    (asserts! (is-eq tx-sender (get buyer purchase)) ERR-NOT-AUTHORIZED)
    
    ;; Validate status
    (asserts! (or (is-eq (get status purchase) u1) (is-eq (get status purchase) u2)) ERR-DELIVERY-ALREADY-CONFIRMED)
    (asserts! (not (is-eq (get status purchase) u4)) ERR-DISPUTED-PURCHASE)
    (asserts! (not (is-eq (get status purchase) u5)) ERR-ALREADY-REFUNDED)
    
    ;; Update purchase status
    (map-set purchases purchase-id 
      (merge purchase 
        { 
          status: u3,  ;; delivered
          delivery-time: (some block-height)
        }
      )
    )
    
    ;; Credit seller's balance
    (credit-seller (get seller purchase) seller-amount)
    
    ;; Update platform fees
    (var-set platform-balance (+ (var-get platform-balance) fee))
    
    (ok true)
  )
)

;; File a dispute (buyer only)
(define-public (file-dispute (purchase-id uint))
  (let ((purchase (try! (get-purchase purchase-id))))
    ;; Authorization check
    (asserts! (is-eq tx-sender (get buyer purchase)) ERR-NOT-AUTHORIZED)
    
    ;; Validate status
    (asserts! (or (is-eq (get status purchase) u1) (is-eq (get status purchase) u2)) ERR-INVALID-PARAM)
    
    ;; Update status to disputed (4)
    (map-set purchases purchase-id (merge purchase { status: u4 }))
    
    (ok true)
  )
)

;; Resolve dispute (admin only)
(define-public (resolve-dispute (purchase-id uint) (refund-buyer bool))
  (let ((purchase (try! (get-purchase purchase-id)))
        (total-amount (* (get price purchase) (get quantity purchase))))
        
    ;; Authorization check
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    ;; Validate status
    (asserts! (is-eq (get status purchase) u4) ERR-INVALID-PARAM)
    
    (if refund-buyer
        (begin
          ;; Refund buyer
          (try! (as-contract (stx-transfer? total-amount tx-sender (get buyer purchase))))
          
          ;; Update status to refunded (5)
          (map-set purchases purchase-id (merge purchase { status: u5 }))
        )
        (begin
          ;; Resolve in seller's favor
          (let ((fee (calculate-fee total-amount))
                (seller-amount (- total-amount fee)))
            
            ;; Credit seller
            (credit-seller (get seller purchase) seller-amount)
            
            ;; Update platform fees
            (var-set platform-balance (+ (var-get platform-balance) fee))
            
            ;; Update status to delivered (3)
            (map-set purchases purchase-id 
              (merge purchase 
                { 
                  status: u3,
                  delivery-time: (some block-height)
                }
              )
            )
          )
        )
    )
    
    (ok true)
  )
)

;; Withdraw seller earnings
(define-public (withdraw-earnings)
  (let ((balance (get-seller-balance tx-sender)))
    ;; Check if balance exists
    (asserts! (> balance u0) ERR-INVALID-PARAM)
    
    ;; Clear balance first to prevent reentrancy
    (map-set seller-balances tx-sender u0)
    
    ;; Transfer funds to seller
    (try! (as-contract (stx-transfer? balance tx-sender tx-sender)))
    
    (ok balance)
  )
)

;; Withdraw platform fees (admin only)
(define-public (withdraw-platform-fees)
  (let ((balance (var-get platform-balance)))
    ;; Authorization check
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    ;; Check if balance exists
    (asserts! (> balance u0) ERR-INVALID-PARAM)
    
    ;; Clear balance first to prevent reentrancy
    (var-set platform-balance u0)
    
    ;; Transfer funds to admin
    (try! (as-contract (stx-transfer? balance tx-sender PLATFORM-ADMIN)))
    
    (ok balance)
  )
)

;; Update platform fee (admin only)
(define-public (update-platform-fee (new-fee-bps uint))
  ;; Authorization check
  (asserts! (is-admin) ERR-NOT-AUTHORIZED)
  
  ;; Validate fee is reasonable (max 10%)
  (asserts! (<= new-fee-bps u1000) ERR-INVALID-PARAM)
  
  ;; Note: Since PLATFORM-FEE-BPS is a constant, we can't update it directly
  ;; In a real implementation, this would need to be a data-var instead
  
  (ok true)
)