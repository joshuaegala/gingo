;; Enhanced Weighted Voting Smart Contract
;; Advanced voting system with delegation, time limits, quorum, and governance features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-INVALID-WEIGHT (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u104))
(define-constant ERR-VOTING-NOT-STARTED (err u105))
(define-constant ERR-INVALID-TIME (err u106))
(define-constant ERR-QUORUM-NOT-MET (err u107))
(define-constant ERR-ALREADY-EXECUTED (err u108))
(define-constant ERR-EXECUTION-FAILED (err u109))
(define-constant ERR-INVALID-THRESHOLD (err u110))
(define-constant ERR-SELF-DELEGATION (err u111))
(define-constant ERR-DELEGATION-CYCLE (err u112))
(define-constant ERR-INSUFFICIENT-BALANCE (err u113))
(define-constant ERR-INVALID-PROPOSAL-TYPE (err u114))

;; Contract owner and governance settings
(define-constant CONTRACT-OWNER tx-sender)
(define-data-var governance-token (optional principal) none)
(define-data-var min-proposal-stake uint u1000)
(define-data-var default-voting-period uint u1440) ;; blocks (~10 days)
(define-data-var quorum-threshold uint u2000) ;; minimum total weight needed
(define-data-var approval-threshold uint u50) ;; percentage needed to pass

;; Proposal types
(define-constant PROPOSAL-TYPE-STANDARD u1)
(define-constant PROPOSAL-TYPE-CONSTITUTIONAL u2)
(define-constant PROPOSAL-TYPE-TREASURY u3)
(define-constant PROPOSAL-TYPE-UPGRADE u4)

;; Data structures
(define-map voter-weights principal uint)
(define-map vote-delegations principal principal) ;; delegator => delegate
(define-map delegation-chains principal (list 10 principal)) ;; track delegation chains
(define-map voter-stakes principal uint) ;; track staked tokens for voting power

(define-map proposals 
  uint 
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: uint,
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint,
    voting-start: uint,
    voting-end: uint,
    execution-delay: uint,
    voting-open: bool,
    executed: bool,
    created-by: principal,
    required-threshold: uint,
    treasury-amount: uint,
    treasury-recipient: (optional principal)
  }
)

(define-map votes 
  {proposal-id: uint, voter: principal} 
  {vote: uint, weight: uint, timestamp: uint} ;; vote: 0=no, 1=yes, 2=abstain
)

(define-map proposal-snapshots
  uint
  {total-eligible-weight: uint, snapshot-block: uint}
)

;; Treasury and staking
(define-map treasury-allocations principal uint)
(define-data-var total-treasury uint u0)
(define-data-var proposal-counter uint u0)

;; Events (using print for logging)
(define-private (log-event (event-type (string-ascii 50)) (data (string-ascii 200)))
  (print {event: event-type, data: data, block: block-height})
)

;; Utility functions

;; Get effective voting weight (including delegations)
(define-private (get-effective-weight (voter principal))
  (let (
    (base-weight (default-to u0 (map-get? voter-weights voter)))
    (staked-amount (default-to u0 (map-get? voter-stakes voter)))
  )
    (+ base-weight staked-amount)
  )
)

;; Helper function for filtering delegation chains
(define-private (not-equal-to-caller (addr principal))
  (not (is-eq addr tx-sender))
)

;; Calculate total delegated weight for a delegate
(define-private (calculate-delegated-weight (delegate principal))
  (let ((chain (default-to (list) (map-get? delegation-chains delegate))))
    (fold add-weight chain u0)
  )
)

;; Helper function for folding weights
(define-private (add-weight (addr principal) (total uint))
  (+ total (get-effective-weight addr))
)

;; Check if delegation creates a cycle
(define-private (check-delegation-cycle (delegator principal) (delegate principal))
  (let ((chain (default-to (list) (map-get? delegation-chains delegate))))
    (not (is-some (index-of chain delegator)))
  )
)

;; Governance functions

;; Set governance parameters (only owner)
(define-public (set-governance-params 
  (min-stake uint) 
  (voting-period uint) 
  (quorum uint) 
  (threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= threshold u100) ERR-INVALID-THRESHOLD)
    (var-set min-proposal-stake min-stake)
    (var-set default-voting-period voting-period)
    (var-set quorum-threshold quorum)
    (var-set approval-threshold threshold)
    (log-event "governance-updated" "parameters modified")
    (ok true)
  )
)

;; Set governance token contract
(define-public (set-governance-token (token principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set governance-token (some token))
    (ok true)
  )
)

;; Voter management

;; Set voter weight with expiration
(define-public (set-voter-weight (voter principal) (weight uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> weight u0) ERR-INVALID-WEIGHT)
    (map-set voter-weights voter weight)
    (log-event "weight-set" "voter weight updated")
    (ok true)
  )
)

;; Stake tokens for additional voting power
(define-public (stake-for-voting (amount uint))
  (let ((current-stake (default-to u0 (map-get? voter-stakes tx-sender))))
    (asserts! (>= amount (var-get min-proposal-stake)) ERR-INSUFFICIENT-BALANCE)
    (map-set voter-stakes tx-sender (+ current-stake amount))
    (log-event "tokens-staked" "voting power increased")
    (ok true)
  )
)

;; Unstake tokens
(define-public (unstake-tokens (amount uint))
  (let ((current-stake (default-to u0 (map-get? voter-stakes tx-sender))))
    (asserts! (>= current-stake amount) ERR-INSUFFICIENT-BALANCE)
    (map-set voter-stakes tx-sender (- current-stake amount))
    (log-event "tokens-unstaked" "voting power decreased")
    (ok true)
  )
)

;; Delegation system

;; Delegate voting power
(define-public (delegate-vote (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) ERR-SELF-DELEGATION)
    (asserts! (check-delegation-cycle tx-sender delegate) ERR-DELEGATION-CYCLE)
    
    ;; Update delegation
    (map-set vote-delegations tx-sender delegate)
    
    ;; Update delegation chain
    (let ((current-chain (default-to (list) (map-get? delegation-chains delegate))))
      (map-set delegation-chains delegate 
        (unwrap-panic (as-max-len? (append current-chain tx-sender) u10)))
    )
    
    (log-event "vote-delegated" "voting power delegated")
    (ok true)
  )
)

;; Remove delegation
(define-public (remove-delegation)
  (match (map-get? vote-delegations tx-sender)
    delegate 
    (begin
      (map-delete vote-delegations tx-sender)
      ;; Update delegation chain by removing the delegator
      (let ((current-chain (default-to (list) (map-get? delegation-chains delegate))))
        (map-set delegation-chains delegate 
          (filter not-equal-to-caller current-chain))
      )
      (log-event "delegation-removed" "voting power reclaimed")
      (ok true)
    )
    (ok false)
  )
)

;; Proposal management

;; Create enhanced proposal
(define-public (create-proposal 
  (title (string-ascii 100)) 
  (description (string-ascii 500))
  (proposal-type uint)
  (voting-period uint)
  (execution-delay uint)
  (treasury-amount uint)
  (treasury-recipient (optional principal)))
  
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (current-weight (get-effective-weight tx-sender))
    (voting-start (+ block-height u10)) ;; start voting in 10 blocks
    (voting-end (+ voting-start voting-period))
    (required-threshold (if (is-eq proposal-type PROPOSAL-TYPE-CONSTITUTIONAL) u66 (var-get approval-threshold)))
  )
    ;; Check minimum stake requirement
    (asserts! (>= current-weight (var-get min-proposal-stake)) ERR-INSUFFICIENT-BALANCE)
    (asserts! (<= proposal-type PROPOSAL-TYPE-UPGRADE) ERR-INVALID-PROPOSAL-TYPE)
    (asserts! (> voting-period u0) ERR-INVALID-TIME)
    
    ;; Create proposal
    (map-set proposals proposal-id {
      title: title,
      description: description,
      proposal-type: proposal-type,
      yes-votes: u0,
      no-votes: u0,
      abstain-votes: u0,
      voting-start: voting-start,
      voting-end: voting-end,
      execution-delay: execution-delay,
      voting-open: true,
      executed: false,
      created-by: tx-sender,
      required-threshold: required-threshold,
      treasury-amount: treasury-amount,
      treasury-recipient: treasury-recipient
    })
    
    ;; Create voting power snapshot
    (map-set proposal-snapshots proposal-id {
      total-eligible-weight: u0, ;; Calculate this when voting starts
      snapshot-block: voting-start
    })
    
    (var-set proposal-counter proposal-id)
    (log-event "proposal-created" "new proposal submitted")
    (ok proposal-id)
  )
)

;; Enhanced voting with abstain option
(define-public (vote (proposal-id uint) (vote-choice uint)) ;; 0=no, 1=yes, 2=abstain
  (let (
    (voter-weight (get-effective-weight tx-sender))
    (delegated-weight (calculate-delegated-weight tx-sender))
    (total-weight (+ voter-weight delegated-weight))
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (vote-key {proposal-id: proposal-id, voter: tx-sender})
  )
    ;; Validation checks
    (asserts! (>= block-height (get voting-start proposal)) ERR-VOTING-NOT-STARTED)
    (asserts! (<= block-height (get voting-end proposal)) ERR-VOTING-CLOSED)
    (asserts! (get voting-open proposal) ERR-VOTING-CLOSED)
    (asserts! (> total-weight u0) ERR-NOT-AUTHORIZED)
    (asserts! (<= vote-choice u2) ERR-INVALID-PROPOSAL-TYPE)
    (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
    
    ;; Record the vote
    (map-set votes vote-key {
      vote: vote-choice, 
      weight: total-weight,
      timestamp: block-height
    })
    
    ;; Update proposal vote counts
    (let ((updated-proposal 
      (if (is-eq vote-choice u1)
        ;; Yes vote
        (merge proposal {yes-votes: (+ (get yes-votes proposal) total-weight)})
        (if (is-eq vote-choice u0) 
          ;; No vote
          (merge proposal {no-votes: (+ (get no-votes proposal) total-weight)})
          ;; Abstain vote
          (merge proposal {abstain-votes: (+ (get abstain-votes proposal) total-weight)})
        )
      )))
      (map-set proposals proposal-id updated-proposal)
    )
    
    (log-event "vote-cast" "vote recorded with weight")
    (ok true)
  )
)

;; Execute proposal (after voting ends and delay period)
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    (asserts! (>= block-height (+ (get voting-end proposal) (get execution-delay proposal))) ERR-VOTING-NOT-STARTED)
    (asserts! (not (get executed proposal)) ERR-ALREADY-EXECUTED)
    
    (let (
      (total-votes (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)))
      (yes-percentage (if (> total-votes u0) (/ (* (get yes-votes proposal) u100) total-votes) u0))
    )
      ;; Check quorum
      (asserts! (>= total-votes (var-get quorum-threshold)) ERR-QUORUM-NOT-MET)
      
      ;; Check if proposal passes
      (if (>= yes-percentage (get required-threshold proposal))
        (begin
          ;; Execute proposal based on type
          (try! (execute-proposal-action proposal-id proposal))
          (map-set proposals proposal-id (merge proposal {executed: true}))
          (log-event "proposal-executed" "proposal successfully executed")
          (ok true)
        )
        (begin
          (map-set proposals proposal-id (merge proposal {executed: true}))
          (log-event "proposal-failed" "proposal failed to meet threshold")
          (ok false)
        )
      )
    )
  )
)

;; Execute specific proposal actions
(define-private (execute-proposal-action (proposal-id uint) (proposal {title: (string-ascii 100), description: (string-ascii 500), proposal-type: uint, yes-votes: uint, no-votes: uint, abstain-votes: uint, voting-start: uint, voting-end: uint, execution-delay: uint, voting-open: bool, executed: bool, created-by: principal, required-threshold: uint, treasury-amount: uint, treasury-recipient: (optional principal)}))
  (let ((prop-type (get proposal-type proposal)))
    (if (is-eq prop-type PROPOSAL-TYPE-TREASURY)
      ;; Treasury proposal
      (match (get treasury-recipient proposal)
        recipient 
        (begin
          (asserts! (<= (get treasury-amount proposal) (var-get total-treasury)) ERR-INSUFFICIENT-BALANCE)
          (map-set treasury-allocations recipient 
            (+ (default-to u0 (map-get? treasury-allocations recipient)) (get treasury-amount proposal)))
          (var-set total-treasury (- (var-get total-treasury) (get treasury-amount proposal)))
          (ok true)
        )
        ERR-EXECUTION-FAILED
      )
      ;; Other proposal types (placeholder for future implementation)
      (ok true)
    )
  )
)

;; Emergency functions

;; Emergency pause (only owner)
(define-public (emergency-pause (proposal-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (map-get? proposals proposal-id)
      proposal 
      (begin
        (map-set proposals proposal-id (merge proposal {voting-open: false}))
        (log-event "emergency-pause" "proposal paused")
        (ok true)
      )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)

;; Batch operations

;; Batch set voter weights
(define-public (batch-set-weights (voters (list 20 {voter: principal, weight: uint})))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (fold batch-set-weight voters (ok true))
  )
)

(define-private (batch-set-weight (voter-data {voter: principal, weight: uint}) (result (response bool uint)))
  (match result
    success (set-voter-weight (get voter voter-data) (get weight voter-data))
    error (err error)
  )
)

;; Read-only functions

;; Get comprehensive voter info
(define-read-only (get-voter-info (voter principal))
  (let (
    (base-weight (default-to u0 (map-get? voter-weights voter)))
    (staked-amount (default-to u0 (map-get? voter-stakes voter)))
    (delegation (map-get? vote-delegations voter))
    (delegated-weight (calculate-delegated-weight voter))
  )
    {
      base-weight: base-weight,
      staked-amount: staked-amount,
      effective-weight: (+ base-weight staked-amount),
      delegated-to: delegation,
      delegated-weight: delegated-weight,
      total-voting-power: (+ base-weight staked-amount delegated-weight)
    }
  )
)

;; Get enhanced proposal details
(define-read-only (get-proposal-details (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal 
    (let (
      (total-votes (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)))
      (yes-percentage (if (> total-votes u0) (/ (* (get yes-votes proposal) u100) total-votes) u0))
      (voting-status (if (< block-height (get voting-start proposal)) "not-started"
                      (if (<= block-height (get voting-end proposal)) "active" "ended")))
    )
      (some {
        proposal: proposal,
        total-votes: total-votes,
        yes-percentage: yes-percentage,
        voting-status: voting-status,
        can-execute: (and 
          (>= block-height (+ (get voting-end proposal) (get execution-delay proposal)))
          (not (get executed proposal))
          (>= total-votes (var-get quorum-threshold))
          (>= yes-percentage (get required-threshold proposal))
        ),
        time-remaining: (if (<= block-height (get voting-end proposal)) 
                         (- (get voting-end proposal) block-height) u0)
      })
    )
    none
  )
)

;; Get governance parameters
(define-read-only (get-governance-params)
  {
    min-proposal-stake: (var-get min-proposal-stake),
    default-voting-period: (var-get default-voting-period),
    quorum-threshold: (var-get quorum-threshold),
    approval-threshold: (var-get approval-threshold),
    governance-token: (var-get governance-token),
    total-treasury: (var-get total-treasury)
  }
)

;; Get delegation chain
(define-read-only (get-delegation-chain (delegate principal))
  (default-to (list) (map-get? delegation-chains delegate))
)

;; Get treasury allocation
(define-read-only (get-treasury-allocation (recipient principal))
  (default-to u0 (map-get? treasury-allocations recipient))
)

;; Get active proposals (last 10)
(define-read-only (get-active-proposals)
  (let ((current-counter (var-get proposal-counter)))
    (map get-proposal-details 
      (list 
        (if (> current-counter u9) (- current-counter u9) u1)
        (if (> current-counter u8) (- current-counter u8) u1)
        (if (> current-counter u7) (- current-counter u7) u1)
        (if (> current-counter u6) (- current-counter u6) u1)
        (if (> current-counter u5) (- current-counter u5) u1)
        (if (> current-counter u4) (- current-counter u4) u1)
        (if (> current-counter u3) (- current-counter u3) u1)
        (if (> current-counter u2) (- current-counter u2) u1)
        (if (> current-counter u1) (- current-counter u1) u1)
        current-counter
      )
    )
  )
)

;; Legacy read-only functions (maintaining compatibility)
(define-read-only (get-voter-weight (voter principal))
  (get effective-weight (get-voter-info voter))
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-proposal-results (proposal-id uint))
  (match (get-proposal-details proposal-id)
    details (some (get proposal details))
    none
  )
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes {proposal-id: proposal-id, voter: voter}))
)