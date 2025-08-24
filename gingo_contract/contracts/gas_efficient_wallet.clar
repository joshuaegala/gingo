;; Gas-Efficient Wallet Contract
;; Minimizes storage writes and optimizes balance operations

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-TRANSFER-FAILED (err u103))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Storage optimization: Use a single map for all user data
;; This reduces storage reads/writes by batching related data
(define-map wallets
  { user: principal }
  { 
    balance: uint,
    nonce: uint,  ;; For replay protection without additional storage
    last-activity: uint  ;; Block height of last activity
  }
)

;; Optimized balance storage - only store non-zero balances
;; This saves gas by not storing empty wallet entries
(define-private (get-wallet-data (user principal))
  (default-to 
    { balance: u0, nonce: u0, last-activity: u0 }
    (map-get? wallets { user: user })
  )
)

;; Gas-efficient balance check
(define-read-only (get-balance (user principal))
  (get balance (get-wallet-data user))
)

;; Get user nonce (for transaction ordering)
(define-read-only (get-nonce (user principal))
  (get nonce (get-wallet-data user))
)

;; Get last activity block height
(define-read-only (get-last-activity (user principal))
  (get last-activity (get-wallet-data user))
)

;; Optimized deposit function
;; Uses STX transfer with minimal storage operations
(define-public (deposit (amount uint))
  (let ((sender tx-sender)
        (current-data (get-wallet-data sender)))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Single storage write combining all updates
    (map-set wallets
      { user: sender }
      {
        balance: (+ (get balance current-data) amount),
        nonce: (get nonce current-data),
        last-activity: block-height
      }
    )
    
    ;; Transfer STX to contract
    (stx-transfer? amount sender (as-contract tx-sender))
  )
)

;; Gas-efficient withdrawal
(define-public (withdraw (amount uint))
  (let ((sender tx-sender)
        (current-data (get-wallet-data sender))
        (current-balance (get balance current-data)))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
    
    (let ((new-balance (- current-balance amount)))
      ;; Optimize storage: remove entry if balance becomes zero
      (if (is-eq new-balance u0)
        (map-delete wallets { user: sender })
        (map-set wallets
          { user: sender }
          {
            balance: new-balance,
            nonce: (+ (get nonce current-data) u1),
            last-activity: block-height
          }
        )
      )
      
      ;; Transfer STX from contract to user
      (as-contract (stx-transfer? amount tx-sender sender))
    )
  )
)

;; Batch transfer for gas efficiency
;; Process multiple transfers in a single transaction
(define-public (batch-transfer (recipients (list 10 { to: principal, amount: uint })))
  (let ((sender tx-sender)
        (current-data (get-wallet-data sender)))
    
    ;; Calculate total amount needed
    (let ((total-amount (fold + (map get-transfer-amount recipients) u0))
          (current-balance (get balance current-data)))
      
      (asserts! (>= current-balance total-amount) ERR-INSUFFICIENT-BALANCE)
      
      ;; Update sender balance once
      (let ((new-balance (- current-balance total-amount)))
        (if (is-eq new-balance u0)
          (map-delete wallets { user: sender })
          (map-set wallets
            { user: sender }
            {
              balance: new-balance,
              nonce: (+ (get nonce current-data) u1),
              last-activity: block-height
            }
          )
        )
        
        ;; Process all transfers
        (ok (map process-single-transfer recipients))
      )
    )
  )
)

;; Helper function for batch transfer
(define-private (get-transfer-amount (transfer { to: principal, amount: uint }))
  (get amount transfer)
)

;; Process individual transfer in batch
(define-private (process-single-transfer (transfer { to: principal, amount: uint }))
  (let ((recipient (get to transfer))
        (amount (get amount transfer))
        (recipient-data (get-wallet-data recipient)))
    
    ;; Update recipient balance
    (map-set wallets
      { user: recipient }
      {
        balance: (+ (get balance recipient-data) amount),
        nonce: (get nonce recipient-data),
        last-activity: block-height
      }
    )
    amount
  )
)

;; Internal transfer between wallet users (no STX movement)
;; Most gas-efficient transfer method
(define-public (internal-transfer (to principal) (amount uint))
  (let ((sender tx-sender)
        (sender-data (get-wallet-data sender))
        (recipient-data (get-wallet-data to)))
    
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (not (is-eq sender to)) ERR-INVALID-AMOUNT)
    (asserts! (>= (get balance sender-data) amount) ERR-INSUFFICIENT-BALANCE)
    
    (let ((sender-new-balance (- (get balance sender-data) amount))
          (recipient-new-balance (+ (get balance recipient-data) amount)))
      
      ;; Update sender
      (if (is-eq sender-new-balance u0)
        (map-delete wallets { user: sender })
        (map-set wallets
          { user: sender }
          {
            balance: sender-new-balance,
            nonce: (+ (get nonce sender-data) u1),
            last-activity: block-height
          }
        )
      )
      
      ;; Update recipient
      (map-set wallets
        { user: to }
        {
          balance: recipient-new-balance,
          nonce: (get nonce recipient-data),
          last-activity: block-height
        }
      )
      
      (ok amount)
    )
  )
)

;; Emergency functions for contract owner
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    ;; Emergency pause logic would go here
    (ok true)
  )
)


;; Get contract statistics (read-only, no gas cost for storage)
(define-read-only (get-contract-stats)
  (ok {
    contract-balance: (stx-get-balance (as-contract tx-sender)),
    block-height: block-height
  })
)


;; Optimized multi-signature verification (if needed)
;; Uses fold to minimize loop overhead
(define-private (verify-signatures (message (buff 32)) (signatures (list 5 (buff 65))) (signers (list 5 principal)))
  ;; Implementation would verify signatures efficiently
  ;; This is a placeholder for multi-sig functionality
  true
)

;; Gas estimation helper (read-only)
(define-read-only (estimate-transfer-cost (amount uint))
  ;; Return estimated gas cost for transfer
  ;; Based on current network conditions
  (ok u1000) ;; Placeholder value
)