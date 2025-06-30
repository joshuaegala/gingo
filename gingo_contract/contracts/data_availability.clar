;; Enhanced Data Availability Smart Contract
;; Comprehensive decentralized storage with advanced features

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NODE_NOT_FOUND (err u101))
(define-constant ERR_FILE_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_STAKE (err u103))
(define-constant ERR_PROOF_EXPIRED (err u104))
(define-constant ERR_INVALID_PROOF (err u105))
(define-constant ERR_ALREADY_SLASHED (err u106))
(define-constant ERR_INSUFFICIENT_BALANCE (err u107))
(define-constant ERR_CONTRACT_PAUSED (err u108))
(define-constant ERR_INVALID_SIGNATURE (err u109))
(define-constant ERR_AUCTION_ENDED (err u110))
(define-constant ERR_BID_TOO_LOW (err u111))
(define-constant ERR_INSURANCE_CLAIM_DENIED (err u112))
(define-constant ERR_BANDWIDTH_EXCEEDED (err u113))
(define-constant ERR_INVALID_ENCRYPTION (err u114))
(define-constant ERR_GOVERNANCE_THRESHOLD (err u115))

;; Configuration constants
(define-constant MIN_STAKE u1000000) ;; 1 STX minimum stake
(define-constant PROOF_INTERVAL u144) ;; ~24 hours in blocks
(define-constant SLASH_PERCENTAGE u20) ;; 20% of stake slashed
(define-constant REDUNDANCY_REWARD u100000) ;; 0.1 STX reward per copy
(define-constant MIN_REDUNDANCY u3) ;; Minimum 3 copies
(define-constant MAX_FILE_SIZE u1000000000) ;; 1GB max file size
(define-constant REPUTATION_DECAY_RATE u1) ;; Reputation decay per missed proof
(define-constant INSURANCE_PREMIUM_RATE u5) ;; 5% of file value
(define-constant BANDWIDTH_LIMIT_PER_NODE u10000000) ;; 10MB per block per node
(define-constant GOVERNANCE_QUORUM u51) ;; 51% required for governance
(define-constant AUCTION_DURATION u1008) ;; 7 days in blocks
(define-constant RETRIEVAL_REWARD_BASE u10000) ;; Base retrieval reward

;; Contract state variables
(define-data-var contract-paused bool false)
(define-data-var total-staked uint u0)
(define-data-var total-files uint u0)
(define-data-var contract-balance uint u0)
(define-data-var insurance-pool uint u0)
(define-data-var governance-proposal-id uint u0)
(define-data-var network-utilization uint u0)
(define-data-var base-storage-price uint u1000) ;; Dynamic pricing

;; Enhanced data structures
(define-map nodes
    { node-id: principal }
    {
        stake: uint,
        reputation: uint,
        last-proof-block: uint,
        is-active: bool,
        slash-count: uint,
        total-files: uint,
        bandwidth-used: uint,
        geographic-region: (string-ascii 10),
        node-type: (string-ascii 20), ;; "full", "light", "archive"
        performance-score: uint,
        uptime-percentage: uint,
        last-ping: uint,
        specializations: (list 5 (string-ascii 20)) ;; e.g., ["video", "documents"]
    }
)

(define-map files
    { file-hash: (buff 32) }
    {
        owner: principal,
        size: uint,
        created-block: uint,
        required-redundancy: uint,
        current-redundancy: uint,
        reward-per-copy: uint,
        file-type: (string-ascii 20),
        encryption-type: (string-ascii 20),
        access-frequency: uint,
        priority-level: uint, ;; 1-5, higher = more important
        insurance-value: uint,
        retrieval-count: uint,
        content-hash: (buff 32), ;; For integrity verification
        expiry-block: (optional uint),
        access-permissions: (list 10 principal) ;; Authorized accessors
    }
)

(define-map file-storage
    { file-hash: (buff 32), node-id: principal }
    {
        stored-block: uint,
        last-proof-block: uint,
        proof-hash: (buff 32),
        is-valid: bool,
        retrieval-count: uint,
        bandwidth-contributed: uint,
        storage-tier: (string-ascii 10), ;; "hot", "warm", "cold"
        replication-factor: uint
    }
)

;; Storage auctions for efficient allocation
(define-map storage-auctions
    { auction-id: uint }
    {
        file-hash: (buff 32),
        creator: principal,
        min-price: uint,
        current-winner: (optional principal),
        current-bid: uint,
        end-block: uint,
        is-active: bool,
        required-specs: (string-ascii 100)
    }
)

(define-map auction-bids
    { auction-id: uint, bidder: principal }
    {
        bid-amount: uint,
        bid-block: uint,
        node-specs: (string-ascii 100)
    }
)

;; Insurance system
(define-map insurance-policies
    { file-hash: (buff 32) }
    {
        premium-paid: uint,
        coverage-amount: uint,
        policy-start: uint,
        policy-end: uint,
        claim-count: uint,
        is-active: bool
    }
)

(define-map insurance-claims
    { claim-id: uint }
    {
        file-hash: (buff 32),
        claimant: principal,
        claim-amount: uint,
        claim-reason: (string-ascii 50),
        evidence-hash: (buff 32),
        claim-block: uint,
        is-approved: (optional bool),
        investigator: (optional principal)
    }
)

;; Governance system
(define-map governance-proposals
    { proposal-id: uint }
    {
        proposer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposal-type: (string-ascii 20), ;; "parameter", "upgrade", "treasury"
        target-parameter: (optional (string-ascii 50)),
        new-value: (optional uint),
        votes-for: uint,
        votes-against: uint,
        voting-end: uint,
        is-executed: bool,
        minimum-stake-required: uint
    }
)

(define-map governance-votes
    { proposal-id: uint, voter: principal }
    {
        vote: bool, ;; true = for, false = against
        stake-weight: uint,
        vote-block: uint
    }
)

;; Dynamic pricing and load balancing
(define-map regional-metrics
    { region: (string-ascii 10) }
    {
        total-capacity: uint,
        used-capacity: uint,
        node-count: uint,
        avg-latency: uint,
        price-multiplier: uint
    }
)

;; Content delivery network features
(define-map cdn-caches
    { file-hash: (buff 32), region: (string-ascii 10) }
    {
        cache-node: principal,
        cache-time: uint,
        hit-count: uint,
        last-access: uint,
        cache-score: uint
    }
)

;; Retrieval marketplace
(define-map retrieval-requests
    { request-id: uint }
    {
        file-hash: (buff 32),
        requester: principal,
        reward-offered: uint,
        max-latency: uint,
        preferred-region: (optional (string-ascii 10)),
        request-block: uint,
        fulfilled-by: (optional principal),
        completion-time: (optional uint)
    }
)

;; Events and metrics
(define-data-var event-id uint u0)
(define-data-var auction-id uint u0)
(define-data-var claim-id uint u0)
(define-data-var request-id uint u0)

;; Helper functions
(define-private (get-next-event-id)
    (let ((current-id (var-get event-id)))
        (var-set event-id (+ current-id u1))
        current-id
    )
)

(define-private (calculate-dynamic-price (file-size uint) (region (string-ascii 10)) (priority uint))
    (let (
        (base-price (var-get base-storage-price))
        (utilization-multiplier (+ u100 (var-get network-utilization)))
        (priority-multiplier (+ u100 (* priority u20)))
        (size-cost (* file-size u10))
    )
        (/ (* base-price utilization-multiplier priority-multiplier size-cost) u1000000)
    )
)

(define-private (update-reputation (node-id principal) (change int))
    (match (map-get? nodes { node-id: node-id })
        node-data 
        (let ((current-rep (get reputation node-data)))
            (map-set nodes
                { node-id: node-id }
                (merge node-data {
                    reputation: (if (> change 0)
                        (+ current-rep (to-uint change))
                        (let ((negative-change (to-uint (- 0 change))))
                            (if (> current-rep negative-change)
                                (- current-rep negative-change)
                                u0)))
                })
            )
            true
        )
        false
    )
)

;; Enhanced node management
(define-public (register-node-advanced (stake-amount uint) (region (string-ascii 10)) (node-type (string-ascii 20)) (specializations (list 5 (string-ascii 20))))
    (let (
        (node-data (map-get? nodes { node-id: tx-sender }))
    )
        (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
        (asserts! (>= stake-amount MIN_STAKE) ERR_INSUFFICIENT_STAKE)
        (asserts! (is-none node-data) (err u200)) ;; Node already registered
        
        ;; Transfer stake to contract
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        ;; Register enhanced node
        (map-set nodes
            { node-id: tx-sender }
            {
                stake: stake-amount,
                reputation: u100,
                last-proof-block: block-height,
                is-active: true,
                slash-count: u0,
                total-files: u0,
                bandwidth-used: u0,
                geographic-region: region,
                node-type: node-type,
                performance-score: u100,
                uptime-percentage: u100,
                last-ping: block-height,
                specializations: specializations
            }
        )
        
        ;; Update regional metrics
        (match (map-get? regional-metrics { region: region })
            regional-data 
            (map-set regional-metrics
                { region: region }
                (merge regional-data { node-count: (+ (get node-count regional-data) u1) })
            )
            (map-set regional-metrics
                { region: region }
                {
                    total-capacity: u0,
                    used-capacity: u0,
                    node-count: u1,
                    avg-latency: u100,
                    price-multiplier: u100
                }
            )
        )
        
        (var-set total-staked (+ (var-get total-staked) stake-amount))
        
        (print {
            event: "node-registered-advanced",
            node-id: tx-sender,
            stake: stake-amount,
            region: region,
            node-type: node-type,
            specializations: specializations
        })
        
        (ok true)
    )
)

;; File registration with advanced features
(define-public (register-file-advanced 
    (file-hash (buff 32)) 
    (content-hash (buff 32))
    (file-size uint) 
    (file-type (string-ascii 20))
    (encryption-type (string-ascii 20))
    (redundancy uint) 
    (priority uint)
    (expiry-blocks (optional uint))
    (access-permissions (list 10 principal))
    (insurance-coverage uint))
    (let (
        (file-data (map-get? files { file-hash: file-hash }))
        (dynamic-price (calculate-dynamic-price file-size "global" priority))
    )
        (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
        (asserts! (is-none file-data) (err u201)) ;; File already registered
        (asserts! (>= redundancy MIN_REDUNDANCY) (err u202))
        (asserts! (<= file-size MAX_FILE_SIZE) (err u203))
        (asserts! (<= priority u5) (err u204))
        
        ;; Calculate total cost including insurance
        (let (
            (storage-cost (* redundancy dynamic-price))
            (insurance-premium (if (> insurance-coverage u0) 
                (/ (* insurance-coverage INSURANCE_PREMIUM_RATE) u100) 
                u0))
            (total-cost (+ storage-cost insurance-premium))
        )
            ;; Transfer payment
            (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
            
            ;; Register file
            (map-set files
                { file-hash: file-hash }
                {
                    owner: tx-sender,
                    size: file-size,
                    created-block: block-height,
                    required-redundancy: redundancy,
                    current-redundancy: u0,
                    reward-per-copy: dynamic-price,
                    file-type: file-type,
                    encryption-type: encryption-type,
                    access-frequency: u0,
                    priority-level: priority,
                    insurance-value: insurance-coverage,
                    retrieval-count: u0,
                    content-hash: content-hash,
                    expiry-block: (match expiry-blocks
                        exp-blocks (some (+ block-height exp-blocks))
                        none),
                    access-permissions: access-permissions
                }
            )
            
            ;; Set up insurance if requested
            (if (> insurance-coverage u0)
                (begin
                    (map-set insurance-policies
                        { file-hash: file-hash }
                        {
                            premium-paid: insurance-premium,
                            coverage-amount: insurance-coverage,
                            policy-start: block-height,
                            policy-end: (+ block-height u52560), ;; 1 year
                            claim-count: u0,
                            is-active: true
                        }
                    )
                    (var-set insurance-pool (+ (var-get insurance-pool) insurance-premium))
                )
                true
            )
            
            (var-set contract-balance (+ (var-get contract-balance) storage-cost))
            (var-set total-files (+ (var-get total-files) u1))
            
            (print {
                event: "file-registered-advanced",
                file-hash: file-hash,
                owner: tx-sender,
                size: file-size,
                type: file-type,
                priority: priority,
                insurance: insurance-coverage
            })
            
            (ok file-hash)
        )
    )
)

;; Storage auction system
(define-public (create-storage-auction (file-hash (buff 32)) (min-price uint) (required-specs (string-ascii 100)))
    (let (
        (file-data (unwrap! (map-get? files { file-hash: file-hash }) ERR_FILE_NOT_FOUND))
        (current-auction-id (var-get auction-id))
    )
        (asserts! (is-eq (get owner file-data) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
        
        ;; Create auction
        (map-set storage-auctions
            { auction-id: current-auction-id }
            {
                file-hash: file-hash,
                creator: tx-sender,
                min-price: min-price,
                current-winner: none,
                current-bid: u0,
                end-block: (+ block-height AUCTION_DURATION),
                is-active: true,
                required-specs: required-specs
            }
        )
        
        (var-set auction-id (+ current-auction-id u1))
        
        (print {
            event: "auction-created",
            auction-id: current-auction-id,
            file-hash: file-hash,
            min-price: min-price
        })
        
        (ok current-auction-id)
    )
)

(define-public (place-auction-bid (target-auction-id uint) (bid-amount uint) (node-specs (string-ascii 100)))
    (let (
        (auction-data (unwrap! (map-get? storage-auctions { auction-id: target-auction-id }) ERR_FILE_NOT_FOUND))
        (node-data (unwrap! (map-get? nodes { node-id: tx-sender }) ERR_NODE_NOT_FOUND))
    )
        (asserts! (get is-active auction-data) ERR_AUCTION_ENDED)
        (asserts! (< block-height (get end-block auction-data)) ERR_AUCTION_ENDED)
        (asserts! (> bid-amount (get current-bid auction-data)) ERR_BID_TOO_LOW)
        (asserts! (>= bid-amount (get min-price auction-data)) ERR_BID_TOO_LOW)
        (asserts! (get is-active node-data) (err u205))
        
        ;; Return previous winner's bid if exists
        (match (get current-winner auction-data)
            prev-winner 
            (begin
                (try! (as-contract (stx-transfer? (get current-bid auction-data) tx-sender prev-winner)))
                true
            )
            true
        )
        
        ;; Transfer new bid
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        ;; Record bid
        (map-set auction-bids
            { auction-id: target-auction-id, bidder: tx-sender }
            {
                bid-amount: bid-amount,
                bid-block: block-height,
                node-specs: node-specs
            }
        )
        
        ;; Update auction
        (map-set storage-auctions
            { auction-id: target-auction-id }
            (merge auction-data {
                current-winner: (some tx-sender),
                current-bid: bid-amount
            })
        )
        
        (print {
            event: "bid-placed",
            auction-id: target-auction-id,
            bidder: tx-sender,
            amount: bid-amount
        })
        
        (ok true)
    )
)

;; Enhanced proof system with performance metrics
(define-public (submit-proof-with-metrics 
    (file-hash (buff 32)) 
    (proof-hash (buff 32))
    (bandwidth-used uint)
    (latency-ms uint))
    (let (
        (node-data (unwrap! (map-get? nodes { node-id: tx-sender }) ERR_NODE_NOT_FOUND))
        (storage-data (unwrap! (map-get? file-storage { file-hash: file-hash, node-id: tx-sender }) ERR_FILE_NOT_FOUND))
    )
        (asserts! (get is-active node-data) (err u206))
        (asserts! (get is-valid storage-data) (err u207))
        (asserts! (<= bandwidth-used BANDWIDTH_LIMIT_PER_NODE) ERR_BANDWIDTH_EXCEEDED)
        
        ;; Update proof and metrics
        (map-set file-storage
            { file-hash: file-hash, node-id: tx-sender }
            (merge storage-data {
                last-proof-block: block-height,
                proof-hash: proof-hash,
                bandwidth-contributed: bandwidth-used
            })
        )
        
        ;; Update node performance
        (let (
            (performance-bonus (if (< latency-ms u100) u5 u0))
            (calculated-performance (+ (get performance-score node-data) performance-bonus))
            (new-performance (if (> calculated-performance u200) u200 calculated-performance))
        )
            (map-set nodes
                { node-id: tx-sender }
                (merge node-data {
                    last-proof-block: block-height,
                    bandwidth-used: (+ (get bandwidth-used node-data) bandwidth-used),
                    performance-score: new-performance,
                    last-ping: block-height
                })
            )
        )
        
        ;; Reward for proof submission
        (try! (as-contract (stx-transfer? u1000 tx-sender tx-sender)))
        
        (print {
            event: "proof-submitted-with-metrics",
            file-hash: file-hash,
            node-id: tx-sender,
            bandwidth: bandwidth-used,
            latency: latency-ms
        })
        
        (ok true)
    )
)

;; Insurance claim system
(define-public (file-insurance-claim (file-hash (buff 32)) (claim-amount uint) (reason (string-ascii 50)) (evidence-hash (buff 32)))
    (let (
        (file-data (unwrap! (map-get? files { file-hash: file-hash }) ERR_FILE_NOT_FOUND))
        (insurance-policy (unwrap! (map-get? insurance-policies { file-hash: file-hash }) ERR_INSURANCE_CLAIM_DENIED))
        (current-claim-id (var-get claim-id))
    )
        (asserts! (is-eq (get owner file-data) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (get is-active insurance-policy) ERR_INSURANCE_CLAIM_DENIED)
        (asserts! (<= claim-amount (get coverage-amount insurance-policy)) ERR_INSURANCE_CLAIM_DENIED)
        (asserts! (< block-height (get policy-end insurance-policy)) ERR_INSURANCE_CLAIM_DENIED)
        
        ;; Create claim
        (map-set insurance-claims
            { claim-id: current-claim-id }
            {
                file-hash: file-hash,
                claimant: tx-sender,
                claim-amount: claim-amount,
                claim-reason: reason,
                evidence-hash: evidence-hash,
                claim-block: block-height,
                is-approved: none,
                investigator: none
            }
        )
        
        (var-set claim-id (+ current-claim-id u1))
        
        (print {
            event: "insurance-claim-filed",
            claim-id: current-claim-id,
            file-hash: file-hash,
            amount: claim-amount,
            reason: reason
        })
        
        (ok current-claim-id)
    )
)

;; Retrieval marketplace
(define-public (request-file-retrieval (file-hash (buff 32)) (reward-offered uint) (max-latency uint) (preferred-region (optional (string-ascii 10))))
    (let (
        (file-data (unwrap! (map-get? files { file-hash: file-hash }) ERR_FILE_NOT_FOUND))
        (current-request-id (var-get request-id))
    )
        ;; Transfer reward to contract
        (try! (stx-transfer? reward-offered tx-sender (as-contract tx-sender)))
        
        ;; Create retrieval request
        (map-set retrieval-requests
            { request-id: current-request-id }
            {
                file-hash: file-hash,
                requester: tx-sender,
                reward-offered: reward-offered,
                max-latency: max-latency,
                preferred-region: preferred-region,
                request-block: block-height,
                fulfilled-by: none,
                completion-time: none
            }
        )
        
        (var-set request-id (+ current-request-id u1))
        
        (print {
            event: "retrieval-requested",
            request-id: current-request-id,
            file-hash: file-hash,
            reward: reward-offered
        })
        
        (ok current-request-id)
    )
)

(define-public (fulfill-retrieval-request (target-request-id uint) (delivery-time uint))
    (let (
        (request-data (unwrap! (map-get? retrieval-requests { request-id: target-request-id }) ERR_FILE_NOT_FOUND))
        (node-data (unwrap! (map-get? nodes { node-id: tx-sender }) ERR_NODE_NOT_FOUND))
    )
        (asserts! (is-none (get fulfilled-by request-data)) (err u208))
        (asserts! (<= delivery-time (get max-latency request-data)) (err u209))
        (asserts! (get is-active node-data) (err u210))
        
        ;; Update request
        (map-set retrieval-requests
            { request-id: target-request-id }
            (merge request-data {
                fulfilled-by: (some tx-sender),
                completion-time: (some delivery-time)
            })
        )
        
        ;; Pay reward
        (try! (as-contract (stx-transfer? (get reward-offered request-data) tx-sender tx-sender)))
        
        ;; Update reputation
        (update-reputation tx-sender 5)
        
        (print {
            event: "retrieval-fulfilled",
            request-id: target-request-id,
            fulfiller: tx-sender,
            delivery-time: delivery-time
        })
        
        (ok true)
    )
)

;; Governance system
(define-public (create-governance-proposal 
    (title (string-ascii 100)) 
    (description (string-ascii 500))
    (proposal-type (string-ascii 20))
    (target-parameter (optional (string-ascii 50)))
    (new-value (optional uint)))
    (let (
        (node-data (unwrap! (map-get? nodes { node-id: tx-sender }) ERR_NODE_NOT_FOUND))
        (proposal-id (var-get governance-proposal-id))
    )
        (asserts! (>= (get stake node-data) (* MIN_STAKE u10)) ERR_GOVERNANCE_THRESHOLD) ;; Need 10x min stake
        (asserts! (get is-active node-data) (err u211))
        
        ;; Create proposal
        (map-set governance-proposals
            { proposal-id: proposal-id }
            {
                proposer: tx-sender,
                title: title,
                description: description,
                proposal-type: proposal-type,
                target-parameter: target-parameter,
                new-value: new-value,
                votes-for: u0,
                votes-against: u0,
                voting-end: (+ block-height u1008), ;; 7 days
                is-executed: false,
                minimum-stake-required: MIN_STAKE
            }
        )
        
        (var-set governance-proposal-id (+ proposal-id u1))
        
        (print {
            event: "governance-proposal-created",
            proposal-id: proposal-id,
            proposer: tx-sender,
            title: title
        })
        
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
    (let (
        (proposal-data (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR_FILE_NOT_FOUND))
        (node-data (unwrap! (map-get? nodes { node-id: tx-sender }) ERR_NODE_NOT_FOUND))
        (existing-vote (map-get? governance-votes { proposal-id: proposal-id, voter: tx-sender }))
    )
        (asserts! (< block-height (get voting-end proposal-data)) (err u212))
        (asserts! (is-none existing-vote) (err u213)) ;; Already voted
        (asserts! (>= (get stake node-data) (get minimum-stake-required proposal-data)) ERR_GOVERNANCE_THRESHOLD)
        
        ;; Record vote
        (map-set governance-votes
            { proposal-id: proposal-id, voter: tx-sender }
            {
                vote: vote,
                stake-weight: (get stake node-data),
                vote-block: block-height
            }
        )
        
        ;; Update proposal totals
        (map-set governance-proposals
            { proposal-id: proposal-id }
            (merge proposal-data {
                votes-for: (if vote 
                    (+ (get votes-for proposal-data) (get stake node-data))
                    (get votes-for proposal-data)),
                votes-against: (if vote 
                    (get votes-against proposal-data)
                    (+ (get votes-against proposal-data) (get stake node-data)))
            })
        )
        
        (print {
            event: "governance-vote-cast",
            proposal-id: proposal-id,
            voter: tx-sender,
            vote: vote,
            stake-weight: (get stake node-data)
        })
        
        (ok true)
    )
)

;; Content Delivery Network caching
(define-public (cache-popular-file (file-hash (buff 32)) (region (string-ascii 10)))
    (let (
        (file-data (unwrap! (map-get? files { file-hash: file-hash }) ERR_FILE_NOT_FOUND))
        (node-data (unwrap! (map-get? nodes { node-id: tx-sender }) ERR_NODE_NOT_FOUND))
    )
        (asserts! (is-eq (get geographic-region node-data) region) (err u214))
        (asserts! (>= (get access-frequency file-data) u10) (err u215)) ;; Must be popular
        
        ;; Create cache entry
        (map-set cdn-caches
            { file-hash: file-hash, region: region }
            {
                cache-node: tx-sender,
                cache-time: block-height,
                hit-count: u0,
                last-access: block-height,
                cache-score: u100
            }
        )
        
        ;; Reward for caching
        (try! (as-contract (stx-transfer? u50000 tx-sender tx-sender)))
        
        (print {
            event: "file-cached",
            file-hash: file-hash,
            region: region,
            cache-node: tx-sender
        })
        
        (ok true)
    )
)

;; Enhanced view functions
(define-read-only (get-node-detailed-info (node-id principal))
    (map-get? nodes { node-id: node-id })
)

(define-read-only (get-file-detailed-info (file-hash (buff 32)))
    (map-get? files { file-hash: file-hash })
)

(define-read-only (get-network-stats)
    {
        total-staked: (var-get total-staked),
        total-files: (var-get total-files),
        contract-balance: (var-get contract-balance),
        insurance-pool: (var-get insurance-pool),
        network-utilization: (var-get network-utilization),
        current-block: block-height
    }
)

(define-read-only (calculate-slash-amount (node-id principal))
    (match (map-get? nodes { node-id: node-id })
        node-data (/ (* (get stake node-data) SLASH_PERCENTAGE) u100)
        u0
    )
)

(define-read-only (get-auction-info (target-auction-id uint))
    (map-get? storage-auctions { auction-id: target-auction-id })
)

(define-read-only (get-insurance-policy (file-hash (buff 32)))
    (map-get? insurance-policies { file-hash: file-hash })
)

(define-read-only (get-governance-proposal (proposal-id uint))
    (map-get? governance-proposals { proposal-id: proposal-id })
)

(define-read-only (get-regional-metrics (region (string-ascii 10)))
    (map-get? regional-metrics { region: region })
)

;; Admin functions
(define-public (update-min-stake (new-min-stake uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        ;; This would require a contract upgrade to implement
        (ok true)
    )
)

(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (emergency-unpause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused false)
        (ok true)
    )
)