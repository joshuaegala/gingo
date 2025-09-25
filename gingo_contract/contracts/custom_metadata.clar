;; Multi-Send with Custom Metadata Smart Contract
;; Allows batch sending of STX with custom metadata for tracking and context

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_TRANSFER_FAILED (err u103))
(define-constant ERR_INVALID_RECIPIENT (err u104))
(define-constant ERR_EMPTY_BATCH (err u105))
(define-constant ERR_METADATA_TOO_LONG (err u106))

;; Data Variables
(define-data-var contract-enabled bool true)
(define-data-var batch-counter uint u0)
(define-data-var max-metadata-length uint u500)

;; Data Maps
;; Store batch metadata
(define-map batch-metadata
    { batch-id: uint }
    {
        sender: principal,
        timestamp: uint,
        total-amount: uint,
        recipient-count: uint,
        batch-metadata: (string-ascii 500),
        block-height: uint
    }
)

;; Store individual transfer metadata
(define-map transfer-metadata
    { batch-id: uint, transfer-index: uint }
    {
        recipient: principal,
        amount: uint,
        metadata: (string-ascii 200),
        success: bool
    }
)

;; Helper Functions

;; Check if contract is enabled
(define-private (is-contract-enabled)
    (var-get contract-enabled)
)

;; Validate metadata length
(define-private (validate-metadata (metadata (string-ascii 500)))
    (<= (len metadata) (var-get max-metadata-length))
)

;; Calculate total amount for batch
(define-private (calculate-total-amount (transfers (list 50 { recipient: principal, amount: uint, metadata: (string-ascii 200) })))
    (fold + (map get-transfer-amount transfers) u0)
)

(define-private (get-transfer-amount (transfer { recipient: principal, amount: uint, metadata: (string-ascii 200) }))
    (get amount transfer)
)

;; Validate single transfer
(define-private (validate-transfer (transfer { recipient: principal, amount: uint, metadata: (string-ascii 200) }))
    (and
        (> (get amount transfer) u0)
        (is-standard (get recipient transfer))
        (<= (len (get metadata transfer)) u200)
    )
)

;; Validate all transfers in batch
(define-private (validate-all-transfers (transfers (list 50 { recipient: principal, amount: uint, metadata: (string-ascii 200) })))
    (fold and (map validate-transfer transfers) true)
)

;; Execute single transfer
(define-private (execute-transfer 
    (transfer { recipient: principal, amount: uint, metadata: (string-ascii 200) })
    (context { batch-id: uint, index: uint, sender: principal })
)
    (let
        (
            (transfer-result (stx-transfer? (get amount transfer) (get sender context) (get recipient transfer)))
            (success (is-ok transfer-result))
            (new-index (+ (get index context) u1))
        )
        ;; Store transfer metadata
        (map-set transfer-metadata
            { batch-id: (get batch-id context), transfer-index: (get index context) }
            {
                recipient: (get recipient transfer),
                amount: (get amount transfer),
                metadata: (get metadata transfer),
                success: success
            }
        )
        ;; Emit transfer event
        (print {
            event: "transfer-executed",
            batch-id: (get batch-id context),
            transfer-index: (get index context),
            recipient: (get recipient transfer),
            amount: (get amount transfer),
            metadata: (get metadata transfer),
            success: success
        })
        ;; Return updated context
        (merge context { index: new-index })
    )
)

;; Public Functions

;; Multi-send function with metadata
(define-public (multi-send-with-metadata
    (transfers (list 50 { recipient: principal, amount: uint, metadata: (string-ascii 200) }))
    (batch-description (string-ascii 500))
)
    (let
        (
            (sender tx-sender)
            (current-batch-id (var-get batch-counter))
            (new-batch-id (+ current-batch-id u1))
            (total-amount (calculate-total-amount transfers))
            (recipient-count (len transfers))
            (current-balance (stx-get-balance sender))
        )
        ;; Validate inputs
        (asserts! (is-contract-enabled) ERR_UNAUTHORIZED)
        (asserts! (> recipient-count u0) ERR_EMPTY_BATCH)
        (asserts! (validate-metadata batch-description) ERR_METADATA_TOO_LONG)
        (asserts! (validate-all-transfers transfers) ERR_INVALID_RECIPIENT)
        (asserts! (>= current-balance total-amount) ERR_INSUFFICIENT_BALANCE)
        
        ;; Store batch metadata
        (map-set batch-metadata
            { batch-id: new-batch-id }
            {
                sender: sender,
                timestamp: block-height,
                total-amount: total-amount,
                recipient-count: recipient-count,
                batch-metadata: batch-description,
                block-height: block-height
            }
        )
        
        ;; Execute all transfers
        (fold execute-transfer 
            transfers 
            { batch-id: new-batch-id, index: u0, sender: sender }
        )
        
        ;; Update batch counter
        (var-set batch-counter new-batch-id)
        
        ;; Emit batch completion event
        (print {
            event: "batch-completed",
            batch-id: new-batch-id,
            sender: sender,
            total-amount: total-amount,
            recipient-count: recipient-count,
            batch-metadata: batch-description,
            timestamp: block-height
        })
        
        (ok {
            batch-id: new-batch-id,
            total-amount: total-amount,
            recipient-count: recipient-count
        })
    )
)

;; Read-only Functions

;; Get batch metadata
(define-read-only (get-batch-metadata (batch-id uint))
    (map-get? batch-metadata { batch-id: batch-id })
)

;; Get transfer metadata
(define-read-only (get-transfer-metadata (batch-id uint) (transfer-index uint))
    (map-get? transfer-metadata { batch-id: batch-id, transfer-index: transfer-index })
)

;; Get current batch counter
(define-read-only (get-batch-counter)
    (var-get batch-counter)
)

;; Get batch history for sender (simplified implementation)
(define-read-only (get-sender-batches (sender principal) (start-batch uint) (end-batch uint))
    (let
        (
            (current-batch (var-get batch-counter))
            (search-end (if (<= end-batch current-batch) end-batch current-batch))
            (fold-result (fold collect-sender-batches
                            (generate-batch-range start-batch search-end)
                            { sender: sender, results: (list) }))
        )
        ;; Note: This is a simplified implementation
        ;; In production, you'd want to implement proper pagination
        (get results fold-result)
    )
)

;; Helper to collect batches for a specific sender
(define-private (collect-sender-batches 
    (batch-id uint) 
    (acc { sender: principal, results: (list 10 { batch-id: uint, data: { sender: principal, timestamp: uint, total-amount: uint, recipient-count: uint, batch-metadata: (string-ascii 500), block-height: uint } }) })
)
    (let
        (
            (batch-data (get-batch-metadata batch-id))
            (target-sender (get sender acc))
            (current-results (get results acc))
        )
        (match batch-data
            found-data
                (if (is-eq (get sender found-data) target-sender)
                    (let
                        (
                            (new-item { batch-id: batch-id, data: found-data })
                            (updated-results (match (as-max-len? (append current-results new-item) u10)
                                                some-list some-list
                                                current-results))
                        )
                        { sender: target-sender, results: updated-results }
                    )
                    acc
                )
            acc
        )
    )
)



;; Generate batch range (simplified - returns list of batch IDs)
(define-private (generate-batch-range (start uint) (end uint))
    ;; Simplified implementation - in production, implement proper range generation
    ;; For now, return a small range for demonstration
    (if (<= start end)
        (list start (+ start u1) (+ start u2) (+ start u3) (+ start u4))
        (list)
    )
)

;; Contract status check
(define-read-only (get-contract-status)
    {
        enabled: (var-get contract-enabled),
        total-batches: (var-get batch-counter),
        max-metadata-length: (var-get max-metadata-length)
    }
)

;; Administrative Functions

;; Toggle contract enabled status (owner only)
(define-public (toggle-contract-status)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-enabled (not (var-get contract-enabled)))
        (ok (var-get contract-enabled))
    )
)

;; Update max metadata length (owner only)
(define-public (update-max-metadata-length (new-length uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (and (> new-length u0) (<= new-length u1000)) ERR_INVALID_AMOUNT)
        (var-set max-metadata-length new-length)
        (ok new-length)
    )
)