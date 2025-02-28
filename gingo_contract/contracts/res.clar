
;; title: community_governance

;; Community DAO Governance Contract
;; This contract enables a decentralized governance mechanism for community-driven decision-making

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-VOTED (err u3))
(define-constant ERR-VOTING-ENDED (err u4))
(define-constant ERR-INSUFFICIENT-TOKENS (err u5))
(define-constant ERR-PROPOSAL-FAILED (err u6))

;; Governance Token - Represents voting power
(define-fungible-token governance-token u10000000)

;; Proposal Structure
(define-map proposals
    {proposal-id: uint}
    {
        proposer: principal,
        description: (string-utf8 500),
        vote-start: uint,
        vote-end: uint,
        proposed-changes: (string-utf8 200),
        total-votes-for: uint,
        total-votes-against: uint,
        status: (string-ascii 20),
        vote-threshold: uint
    }
)

;; Track proposal IDs
(define-data-var next-proposal-id uint u0)

;; Voter tracking to prevent multiple votes
(define-map voter-votes 
    {proposal-id: uint, voter: principal}
    {has-voted: bool}
)

;; Mint governance tokens to initial participants
(define-public (mint-governance-tokens (recipient principal) (amount uint))
    (begin
        (try! (ft-mint? governance-token amount recipient))
        (ok true)
    )
)

;; Create a new proposal
(define-public (create-proposal 
    (description (string-utf8 500))
    (proposed-changes (string-utf8 200))
    (vote-duration uint)
)
    (let 
        (
            (proposal-id (var-get next-proposal-id))
            (current-block block-height)
        )
        ;; Require minimum token balance to create proposal
        (asserts! (>= (ft-get-balance governance-token tx-sender) u100) ERR-INSUFFICIENT-TOKENS)
        
        ;; Store proposal
        (map-set proposals 
            {proposal-id: proposal-id}
            {
                proposer: tx-sender,
                description: description,
                vote-start: current-block,
                vote-end: (+ current-block vote-duration),
                proposed-changes: proposed-changes,
                total-votes-for: u0,
                total-votes-against: u0,
                status: "ACTIVE",
                vote-threshold: u5000 ;; 50% threshold
            }
        )
        
        ;; Increment proposal ID
        (var-set next-proposal-id (+ proposal-id u1))
        
        (ok proposal-id)
    )
)

;; Cast a vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let 
        (
            (proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) ERR-PROPOSAL-NOT-FOUND))
            (voter-token-balance (ft-get-balance governance-token tx-sender))
            (current-block block-height)
        )
        ;; Check proposal is still active
        (asserts! (< current-block (get vote-end proposal)) ERR-VOTING-ENDED)
        
        ;; Check voter hasn't already voted
        (asserts! 
            (match (map-get? voter-votes {proposal-id: proposal-id, voter: tx-sender})
                existing-vote false
                true
            )
            ERR-ALREADY-VOTED
        )
        
        ;; Record vote
        (map-set voter-votes 
            {proposal-id: proposal-id, voter: tx-sender}
            {has-voted: true}
        )
        
        ;; Update proposal vote counts
        (if vote-for
            (map-set proposals 
                {proposal-id: proposal-id}
                (merge proposal {
                    total-votes-for: (+ (get total-votes-for proposal) voter-token-balance)
                })
            )
            (map-set proposals 
                {proposal-id: proposal-id}
                (merge proposal {
                    total-votes-against: (+ (get total-votes-against proposal) voter-token-balance)
                })
            )
        )
        
        (ok true)
    )
)

;; Finalize a proposal
(define-public (finalize-proposal (proposal-id uint))
    (let 
        (
            (proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) ERR-PROPOSAL-NOT-FOUND))
            (current-block block-height)
            (total-votes (+ (get total-votes-for proposal) (get total-votes-against proposal)))
            (vote-percentage (if (> total-votes u0)
                (/ (* (get total-votes-for proposal) u10000) total-votes)
                u0
            ))
        )
        ;; Check voting period has ended
        (asserts! (>= current-block (get vote-end proposal)) ERR-VOTING-ENDED)
        
        ;; Calculate voting results
        (if (>= vote-percentage (get vote-threshold proposal))
            ;; Proposal passes
            (begin
                (map-set proposals 
                    {proposal-id: proposal-id}
                    (merge proposal {status: "PASSED"})
                )
                (ok true)
            )
            ;; Proposal fails
            (begin
                (map-set proposals 
                    {proposal-id: proposal-id}
                    (merge proposal {status: "FAILED"})
                )
                (ok false)
            )
        )
    )
)

;; Get proposal details
(define-read-only (get-proposal-details (proposal-id uint))
    (map-get? proposals {proposal-id: proposal-id})
)

;; Check voting power of an account
(define-read-only (get-voting-power (account principal))
    (ft-get-balance governance-token account)
)

;; Initialize the contract with some initial governance tokens
(define-public (initialize-governance)
    (begin
        ;; Mint initial tokens to contract creator
        (try! (ft-mint? governance-token u1000 CONTRACT-OWNER))
        (ok true)
    )
)

;; Initialize contract on deploy
(initialize-governance)