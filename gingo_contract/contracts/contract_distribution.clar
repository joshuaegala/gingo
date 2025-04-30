
;; title: contract_fund_distribution

(define-map milestones
  { milestone-id: uint }
  {
    description: (string-utf8 100),
    target-amount: uint,
    is-achieved: bool,
    funds-allocated: uint
  }
)

(define-map milestone-funders
  { milestone-id: uint, funder: principal }
  { amount-contributed: uint }
)

;; Storage for total contract funds
(define-data-var total-contract-funds uint u0)

;; Error constants
(define-constant ERR-MILESTONE-EXISTS (err u1))
(define-constant ERR-MILESTONE-NOT-FOUND (err u2))
(define-constant ERR-MILESTONE-ALREADY-ACHIEVED (err u3))
(define-constant ERR-INSUFFICIENT-FUNDS (err u4))
(define-constant ERR-UNAUTHORIZED (err u5))

;; Create a new milestone
(define-public (create-milestone 
  (milestone-id uint) 
  (description (string-utf8 100)) 
  (target-amount uint)
)
  (begin
    ;; Check if milestone already exists
    (asserts! (is-none (map-get? milestones { milestone-id: milestone-id })) ERR-MILESTONE-EXISTS)
    
    ;; Create milestone
    (map-set milestones 
      { milestone-id: milestone-id }
      {
        description: description,
        target-amount: target-amount,
        is-achieved: false,
        funds-allocated: u0
      }
    )
    
    (ok true)
  )
)

;; Contribute funds to a specific milestone
(define-public (contribute-to-milestone 
  (milestone-id uint) 
  (amount uint)
)
  (let 
    (
      (milestone (unwrap! 
        (map-get? milestones { milestone-id: milestone-id }) 
        ERR-MILESTONE-NOT-FOUND
      ))
    )
    ;; Ensure milestone is not already achieved
    (asserts! (not (get is-achieved milestone)) ERR-MILESTONE-ALREADY-ACHIEVED)
    
    ;; Update milestone funds
    (map-set milestones 
      { milestone-id: milestone-id }
      (merge milestone { 
        funds-allocated: (+ (get funds-allocated milestone) amount) 
      })
    )
    
    ;; Track individual contributor
    (map-set milestone-funders 
      { milestone-id: milestone-id, funder: tx-sender }
      { amount-contributed: amount }
    )
    
    ;; Update total contract funds
    (var-set total-contract-funds 
      (+ (var-get total-contract-funds) amount)
    )
    
    (ok true)
  )
)

;; Mark milestone as achieved
(define-public (mark-milestone-achieved 
  (milestone-id uint)
)
  (let 
    (
      (milestone (unwrap! 
        (map-get? milestones { milestone-id: milestone-id }) 
        ERR-MILESTONE-NOT-FOUND
      ))
    )
    ;; Ensure milestone funds meet target
    (asserts! 
      (>= (get funds-allocated milestone) (get target-amount milestone)) 
      ERR-INSUFFICIENT-FUNDS
    )
    
    ;; Update milestone status
    (map-set milestones 
      { milestone-id: milestone-id }
      (merge milestone { is-achieved: true })
    )
    
    (ok true)
  )
)

;; Withdraw funds for an achieved milestone
(define-public (withdraw-milestone-funds 
  (milestone-id uint) 
  (recipient principal)
)
  (let 
    (
      (milestone (unwrap! 
        (map-get? milestones { milestone-id: milestone-id }) 
        ERR-MILESTONE-NOT-FOUND
      ))
    )
    ;; Ensure milestone is achieved
    (asserts! (get is-achieved milestone) ERR-MILESTONE-ALREADY-ACHIEVED)
    
    ;; Transfer funds
    (try! (stx-transfer? 
      (get funds-allocated milestone) 
      (as-contract tx-sender) 
      recipient
    ))
    
    (ok true)
  )
)

;; View total contract funds
(define-read-only (get-total-contract-funds)
  (var-get total-contract-funds)
)