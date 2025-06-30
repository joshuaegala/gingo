;; Enhanced Event History Storage Contract
;; Advanced event management with analytics, subscriptions, and governance

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-INPUT (err u102))
(define-constant ERR-SUBSCRIPTION-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-EVENT-ARCHIVED (err u105))
(define-constant ERR-RATE-LIMITED (err u106))
(define-constant ERR-CONTRACT-PAUSED (err u107))

;; Contract Settings
(define-data-var contract-paused bool false)
(define-data-var storage-fee uint u1000)
(define-data-var max-events-per-user uint u1000)
(define-data-var rate-limit-window uint u144)
(define-data-var max-events-per-window uint u100)

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var total-events uint u0)
(define-data-var total-archived-events uint u0)
(define-data-var contract-revenue uint u0)
(define-data-var next-subscription-id uint u1)
(define-data-var next-archive-id uint u1)

;; Enhanced Event Structure
(define-map events
  { event-id: uint }
  {
    event-type: (string-ascii 50),
    event-data: (string-utf8 1000),
    metadata: {
      timestamp: uint,
      block-height: uint,
      tx-sender: principal,
      category: (string-ascii 30),
      priority: uint,
      version: uint,
      parent-event-id: (optional uint),
      expiry-block: (optional uint)
    },
    status: (string-ascii 20),
    access-level: uint,
    encrypted: bool,
    custom-fields: (string-utf8 500)
  }
)

;; Event Tags (separate map due to list limitations)
(define-map event-tags
  { event-id: uint, tag-index: uint }
  { tag: (string-ascii 20) }
)

(define-map event-tag-count
  { event-id: uint }
  { count: uint }
)

;; User Profiles
(define-map user-profiles
  { user: principal }
  {
    total-events: uint,
    subscription-tier: uint,
    last-activity: uint,
    reputation-score: uint,
    rate-limit-reset: uint,
    events-in-window: uint,
    total-spent: uint,
    verified: bool
  }
)

;; Event Subscriptions
(define-map event-subscriptions
  { subscriber: principal, subscription-id: uint }
  {
    filter-category: (string-ascii 30),
    filter-min-priority: uint,
    filter-sender: (optional principal),
    callback-endpoint: (string-utf8 200),
    is-active: bool,
    created-at: uint,
    trigger-count: uint
  }
)

;; Event Relationships
(define-map event-relationships
  { parent-event-id: uint, child-index: uint }
  { child-event-id: uint }
)

(define-map event-reactions
  { event-id: uint, reactor: principal }
  { reaction-type: (string-ascii 20), timestamp: uint }
)

;; Indexing Maps
(define-map events-by-sender
  { sender: principal, event-index: uint }
  { event-id: uint }
)

(define-map sender-event-count
  { sender: principal }
  { count: uint }
)

(define-map events-by-category
  { category: (string-ascii 30), event-index: uint }
  { event-id: uint }
)

(define-map category-event-count
  { category: (string-ascii 30) }
  { count: uint }
)

(define-map events-by-priority
  { priority: uint, event-index: uint }
  { event-id: uint }
)

(define-map priority-event-count
  { priority: uint }
  { count: uint }
)

;; Tag frequency tracking
(define-map tag-frequency
  { tag: (string-ascii 20) }
  { count: uint, last-used: uint }
)

;; Governance
(define-map moderators
  { moderator: principal }
  { permissions: uint, added-by: principal, added-at: uint }
)

(define-map flagged-events
  { event-id: uint }
  { reason: (string-ascii 100), flagged-by: principal, flagged-at: uint, resolved: bool }
)

;; Archive system
(define-map archived-events
  { archive-id: uint }
  { 
    event-count: uint,
    archive-hash: (buff 32),
    created-at: uint,
    created-by: principal
  }
)

(define-map archive-event-list
  { archive-id: uint, event-index: uint }
  { event-id: uint }
)

;; Daily analytics
(define-map daily-event-stats
  { date: uint }
  {
    total-events: uint,
    unique-senders: uint,
    top-category: (string-ascii 30),
    average-priority: uint
  }
)

;; Private Helper Functions

(define-private (min (a uint) (b uint))
  (if (<= a b) a b)
)

(define-private (is-contract-paused)
  (var-get contract-paused)
)

(define-private (check-rate-limit (user principal))
  (let ((profile (default-to 
    { 
      total-events: u0, subscription-tier: u0, last-activity: u0, 
      reputation-score: u50, rate-limit-reset: u0, events-in-window: u0,
      total-spent: u0, verified: false
    }
    (map-get? user-profiles { user: user }))))
    (if (>= block-height (get rate-limit-reset profile))
      (begin
        (map-set user-profiles { user: user }
          (merge profile { 
            rate-limit-reset: (+ block-height (var-get rate-limit-window)),
            events-in-window: u0
          }))
        (ok true)
      )
      (if (< (get events-in-window profile) (var-get max-events-per-window))
        (ok true)
        ERR-RATE-LIMITED
      )
    )
  )
)

(define-private (update-user-profile (user principal) (event-cost uint))
  (let ((current-profile (default-to 
    { 
      total-events: u0, subscription-tier: u0, last-activity: u0, 
      reputation-score: u50, rate-limit-reset: (+ block-height (var-get rate-limit-window)), 
      events-in-window: u0, total-spent: u0, verified: false
    }
    (map-get? user-profiles { user: user }))))
    (map-set user-profiles { user: user }
      (merge current-profile {
        total-events: (+ (get total-events current-profile) u1),
        last-activity: block-height,
        events-in-window: (+ (get events-in-window current-profile) u1),
        total-spent: (+ (get total-spent current-profile) event-cost),
        reputation-score: (min u100 (+ (get reputation-score current-profile) u1))
      })
    )
  )
)

(define-private (increment-sender-count (sender principal) (event-id uint))
  (let ((current-count (default-to u0 (get count (map-get? sender-event-count { sender: sender })))))
    (map-set sender-event-count 
      { sender: sender } 
      { count: (+ current-count u1) })
    (map-set events-by-sender
      { sender: sender, event-index: current-count }
      { event-id: event-id })
  )
)

(define-private (increment-category-count (category (string-ascii 30)) (event-id uint))
  (let ((current-count (default-to u0 (get count (map-get? category-event-count { category: category })))))
    (map-set category-event-count 
      { category: category } 
      { count: (+ current-count u1) })
    (map-set events-by-category
      { category: category, event-index: current-count }
      { event-id: event-id })
  )
)

(define-private (increment-priority-count (priority uint) (event-id uint))
  (let ((current-count (default-to u0 (get count (map-get? priority-event-count { priority: priority })))))
    (map-set priority-event-count 
      { priority: priority } 
      { count: (+ current-count u1) })
    (map-set events-by-priority
      { priority: priority, event-index: current-count }
      { event-id: event-id })
  )
)

(define-private (update-tag-frequency (tag (string-ascii 20)))
  (let ((current-freq (default-to { count: u0, last-used: u0 } 
                      (map-get? tag-frequency { tag: tag }))))
    (map-set tag-frequency { tag: tag }
      { count: (+ (get count current-freq) u1), last-used: block-height })
  )
)

(define-private (store-single-tag (tag (string-ascii 20)) (event-id uint) (index uint))
  (begin
    (map-set event-tags
      { event-id: event-id, tag-index: index }
      { tag: tag })
    (update-tag-frequency tag)
    true
  )
)

(define-private (store-event-tags (event-id uint) (tags (list 10 (string-ascii 20))))
  (let ((tag-count (len tags)))
    (map-set event-tag-count { event-id: event-id } { count: tag-count })
    (begin
      ;; Store tags manually with indices (simplified approach)
      (if (> tag-count u0) (store-single-tag (unwrap-panic (element-at tags u0)) event-id u0) true)
      (if (> tag-count u1) (store-single-tag (unwrap-panic (element-at tags u1)) event-id u1) true)
      (if (> tag-count u2) (store-single-tag (unwrap-panic (element-at tags u2)) event-id u2) true)
      (if (> tag-count u3) (store-single-tag (unwrap-panic (element-at tags u3)) event-id u3) true)
      (if (> tag-count u4) (store-single-tag (unwrap-panic (element-at tags u4)) event-id u4) true)
      (if (> tag-count u5) (store-single-tag (unwrap-panic (element-at tags u5)) event-id u5) true)
      (if (> tag-count u6) (store-single-tag (unwrap-panic (element-at tags u6)) event-id u6) true)
      (if (> tag-count u7) (store-single-tag (unwrap-panic (element-at tags u7)) event-id u7) true)
      (if (> tag-count u8) (store-single-tag (unwrap-panic (element-at tags u8)) event-id u8) true)
      (if (> tag-count u9) (store-single-tag (unwrap-panic (element-at tags u9)) event-id u9) true)
      event-id
    )
  )
)

;; Enhanced Public Functions

;; Store basic event (simplified parameters)
(define-public (store-event 
  (event-type (string-ascii 50))
  (event-data (string-utf8 1000))
  (category (string-ascii 30))
  (priority uint))
  (begin
    (asserts! (not (is-contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (<= priority u5) ERR-INVALID-INPUT)
    
    (try! (check-rate-limit tx-sender))
    
    (let ((event-id (var-get next-event-id)))
      (map-set events
        { event-id: event-id }
        {
          event-type: event-type,
          event-data: event-data,
          metadata: {
            timestamp: block-height,
            block-height: block-height,
            tx-sender: tx-sender,
            category: category,
            priority: priority,
            version: u1,
            parent-event-id: none,
            expiry-block: none
          },
          status: "active",
          access-level: u0,
          encrypted: false,
          custom-fields: u""
        }
      )
      
      ;; Update indexes
      (increment-sender-count tx-sender event-id)
      (increment-category-count category event-id)
      (increment-priority-count priority event-id)
      (update-user-profile tx-sender u0)
      
      ;; Update counters
      (var-set next-event-id (+ event-id u1))
      (var-set total-events (+ (var-get total-events) u1))
      
      (print {
        event: "event-stored",
        event-id: event-id,
        event-type: event-type,
        sender: tx-sender,
        category: category,
        priority: priority
      })
      
      (ok event-id)
    )
  )
)

;; Store advanced event with payment
(define-public (store-advanced-event 
  (event-type (string-ascii 50))
  (event-data (string-utf8 1000))
  (category (string-ascii 30))
  (priority uint)
  (access-level uint)
  (custom-fields (string-utf8 500))
  (payment uint))
  (begin
    (asserts! (not (is-contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (<= priority u5) ERR-INVALID-INPUT)
    (asserts! (<= access-level u2) ERR-INVALID-INPUT)
    (asserts! (>= payment (var-get storage-fee)) ERR-INSUFFICIENT-PAYMENT)
    
    (try! (check-rate-limit tx-sender))
    
    (let ((event-id (var-get next-event-id)))
      (map-set events
        { event-id: event-id }
        {
          event-type: event-type,
          event-data: event-data,
          metadata: {
            timestamp: block-height,
            block-height: block-height,
            tx-sender: tx-sender,
            category: category,
            priority: priority,
            version: u1,
            parent-event-id: none,
            expiry-block: none
          },
          status: "active",
          access-level: access-level,
          encrypted: false,
          custom-fields: custom-fields
        }
      )
      
      ;; Update indexes and stats
      (increment-sender-count tx-sender event-id)
      (increment-category-count category event-id)
      (increment-priority-count priority event-id)
      (update-user-profile tx-sender payment)
      
      ;; Update counters
      (var-set next-event-id (+ event-id u1))
      (var-set total-events (+ (var-get total-events) u1))
      (var-set contract-revenue (+ (var-get contract-revenue) payment))
      
      (print {
        event: "advanced-event-stored",
        event-id: event-id,
        sender: tx-sender,
        payment: payment
      })
      
      (ok event-id)
    )
  )
)

;; Add tags to an event (separate function)
(define-public (add-event-tags (event-id uint) (tags (list 10 (string-ascii 20))))
  (begin
    (asserts! (is-some (map-get? events { event-id: event-id })) ERR-NOT-FOUND)
    (store-event-tags event-id tags)
    (ok true)
  )
)

;; Create event subscription (simplified)
(define-public (create-subscription
  (filter-category (string-ascii 30))
  (filter-min-priority uint)
  (callback-endpoint (string-utf8 200)))
  (let ((subscription-id (var-get next-subscription-id)))
    (map-set event-subscriptions
      { subscriber: tx-sender, subscription-id: subscription-id }
      {
        filter-category: filter-category,
        filter-min-priority: filter-min-priority,
        filter-sender: none,
        callback-endpoint: callback-endpoint,
        is-active: true,
        created-at: block-height,
        trigger-count: u0
      }
    )
    
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

;; React to events
(define-public (react-to-event (event-id uint) (reaction-type (string-ascii 20)))
  (begin
    (asserts! (is-some (map-get? events { event-id: event-id })) ERR-NOT-FOUND)
    
    (map-set event-reactions
      { event-id: event-id, reactor: tx-sender }
      { reaction-type: reaction-type, timestamp: block-height }
    )
    
    (print {
      event: "event-reaction",
      event-id: event-id,
      reactor: tx-sender,
      reaction-type: reaction-type
    })
    
    (ok true)
  )
)

;; Update event (create new version)
(define-public (update-event
  (original-event-id uint)
  (new-event-type (string-ascii 50))
  (new-event-data (string-utf8 1000))
  (new-category (string-ascii 30))
  (new-priority uint))
  (match (map-get? events { event-id: original-event-id })
    event-info
    (if (is-eq (get tx-sender (get metadata event-info)) tx-sender)
      (let ((new-event-id (var-get next-event-id))
            (original-metadata (get metadata event-info)))
        (begin
          (map-set events
            { event-id: new-event-id }
            {
              event-type: new-event-type,
              event-data: new-event-data,
              metadata: {
                timestamp: block-height,
                block-height: block-height,
                tx-sender: tx-sender,
                category: new-category,
                priority: new-priority,
                version: (+ (get version original-metadata) u1),
                parent-event-id: (some original-event-id),
                expiry-block: (get expiry-block original-metadata)
              },
              status: "active",
              access-level: (get access-level event-info),
              encrypted: (get encrypted event-info),
              custom-fields: (get custom-fields event-info)
            }
          )
          
          ;; Mark original as archived
          (map-set events
            { event-id: original-event-id }
            (merge event-info { status: "archived" })
          )
          
          ;; Update indexes
          (increment-sender-count tx-sender new-event-id)
          (increment-category-count new-category new-event-id)
          (increment-priority-count new-priority new-event-id)
          
          (var-set next-event-id (+ new-event-id u1))
          (var-set total-events (+ (var-get total-events) u1))
          
          (print {
            event: "event-updated",
            original-event-id: original-event-id,
            new-event-id: new-event-id
          })
          
          (ok new-event-id)
        )
      )
      ERR-UNAUTHORIZED
    )
    ERR-NOT-FOUND
  )
)

;; Read-only Functions

;; Get event by ID
(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

;; Get event tags




;; Helper functions for range queries
(define-private (get-sender-events-range (sender principal) (start uint) (end uint))
  (if (< start end)
    (let ((event-data (map-get? events-by-sender { sender: sender, event-index: start })))
      (match event-data
        event-info
        (list (get event-id event-info))
        (list)
      )
    )
    (list)
  )
)

(define-private (get-category-events-range (category (string-ascii 30)) (start uint) (end uint))
  (if (< start end)
    (let ((event-data (map-get? events-by-category { category: category, event-index: start })))
      (match event-data
        event-info
        (list (get event-id event-info))
        (list)
      )
    )
    (list)
  )
)

(define-private (get-priority-events-range (priority uint) (start uint) (end uint))
  (if (< start end)
    (let ((event-data (map-get? events-by-priority { priority: priority, event-index: start })))
      (match event-data
        event-info
        (list (get event-id event-info))
        (list)
      )
    )
    (list)
  )
)

;; Get events by sender
(define-read-only (get-events-by-sender (sender principal) (start-index uint) (limit uint))
  (let ((sender-count (default-to u0 (get count (map-get? sender-event-count { sender: sender })))))
    (if (< start-index sender-count)
      (ok (get-sender-events-range sender start-index (min (+ start-index limit) sender-count)))
      (ok (list))
    )
  )
)

;; Get events by category
(define-read-only (get-events-by-category (category (string-ascii 30)) (start-index uint) (limit uint))
  (let ((category-count (default-to u0 (get count (map-get? category-event-count { category: category })))))
    (if (< start-index category-count)
      (ok (get-category-events-range category start-index (min (+ start-index limit) category-count)))
      (ok (list))
    )
  )
)

;; Get events by priority
(define-read-only (get-events-by-priority (priority uint) (start-index uint) (limit uint))
  (let ((priority-count (default-to u0 (get count (map-get? priority-event-count { priority: priority })))))
    (if (< start-index priority-count)
      (ok (get-priority-events-range priority start-index (min (+ start-index limit) priority-count)))
      (ok (list))
    )
  )
)

;; Get user profile and analytics
(define-read-only (get-user-analytics (user principal))
  (let ((profile (map-get? user-profiles { user: user })))
    (match profile
      user-data
      (ok (some {
        profile: user-data,
        reputation-tier: (if (>= (get reputation-score user-data) u80) "high"
                          (if (>= (get reputation-score user-data) u50) "medium" "low")),
        events-remaining: (- (var-get max-events-per-window) (get events-in-window user-data))
      }))
      (ok none)
    )
  )
)

;; Get tag frequency
(define-read-only (get-tag-frequency (tag (string-ascii 20)))
  (map-get? tag-frequency { tag: tag })
)

;; Get event reaction
(define-read-only (get-event-reaction (event-id uint) (reactor principal))
  (map-get? event-reactions { event-id: event-id, reactor: reactor })
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-events: (var-get total-events),
    total-archived: (var-get total-archived-events),
    total-revenue: (var-get contract-revenue),
    next-event-id: (var-get next-event-id),
    storage-fee: (var-get storage-fee),
    contract-paused: (var-get contract-paused),
    rate-limit-window: (var-get rate-limit-window),
    max-events-per-window: (var-get max-events-per-window)
  }
)

;; Administrative Functions

;; Toggle contract pause
(define-public (toggle-contract-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-paused (not (var-get contract-paused)))
    (ok (var-get contract-paused))
  )
)

;; Update storage fee
(define-public (update-storage-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set storage-fee new-fee)
    (ok new-fee)
  )
)

;; Add moderator
(define-public (add-moderator (moderator principal) (permissions uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set moderators
      { moderator: moderator }
      { permissions: permissions, added-by: tx-sender, added-at: block-height }
    )
    (ok true)
  )
)

;; Flag event
(define-public (flag-event (event-id uint) (reason (string-ascii 100)))
  (begin
    (asserts! (is-some (map-get? events { event-id: event-id })) ERR-NOT-FOUND)
    (map-set flagged-events
      { event-id: event-id }
      { reason: reason, flagged-by: tx-sender, flagged-at: block-height, resolved: false }
    )
    (ok true)
  )
)

;; Upgrade subscription tier
(define-public (upgrade-subscription-tier (new-tier uint) (payment uint))
  (let ((required-payment (* new-tier u10000)))
    (asserts! (>= payment required-payment) ERR-INSUFFICIENT-PAYMENT)
    (asserts! (<= new-tier u3) ERR-INVALID-INPUT)
    
    (let ((current-profile (default-to 
      { 
        total-events: u0, subscription-tier: u0, last-activity: u0, 
        reputation-score: u50, rate-limit-reset: u0, events-in-window: u0,
        total-spent: u0, verified: false
      }
      (map-get? user-profiles { user: tx-sender }))))
      
      (map-set user-profiles { user: tx-sender }
        (merge current-profile {
          subscription-tier: new-tier,
          total-spent: (+ (get total-spent current-profile) payment)
        })
      )
      
      (var-set contract-revenue (+ (var-get contract-revenue) payment))
      (ok new-tier)
    )
  )
)

;; Withdraw revenue (owner only)
(define-public (withdraw-revenue (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= amount (var-get contract-revenue)) ERR-INSUFFICIENT-PAYMENT)
    (var-set contract-revenue (- (var-get contract-revenue) amount))
    (ok amount)
  )
)