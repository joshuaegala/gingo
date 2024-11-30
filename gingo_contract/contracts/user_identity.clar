
;; title: user_identity

;; User Authentication and Identity Management Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-not-found (err u103))
(define-constant err-invalid-role (err u104))

;; Define roles as uint values
(define-constant ROLE-PATIENT u1)
(define-constant ROLE-DOCTOR u2)
(define-constant ROLE-RESEARCHER u3)
(define-constant ROLE-ADMIN u4)

;; Data structures
(define-map user-identities
    principal
    {
        did: (string-utf8 64),              ;; Decentralized Identity
        role: uint,                         ;; User role
        verification-status: bool,          ;; Whether the identity is verified
        metadata: (string-utf8 256),        ;; Additional identity metadata
        registration-time: uint,            ;; When the identity was registered
        last-updated: uint                  ;; Last update timestamp
    }
)

(define-map role-permissions
    uint
    {
        can-verify-others: bool,
        can-update-permissions: bool,
        can-access-anonymized-data: bool
    }
)

(define-map verification-requests
    principal
    {
        proof-document: (string-utf8 64),
        requested-role: uint,
        status: (string-utf8 20),
        verifier: (optional principal)
    }
)

;; Initialize role permissions
(map-set role-permissions ROLE-PATIENT
    {
        can-access-anonymized-data: false,
        can-verify-others: false,
        can-update-permissions: false
    }
)

(map-set role-permissions ROLE-DOCTOR
    {
        can-access-anonymized-data: true,
        can-verify-others: false,
        can-update-permissions: false
    }
)

(map-set role-permissions ROLE-RESEARCHER
    {
        can-access-anonymized-data: true,
        can-verify-others: false,
        can-update-permissions: false
    }
)

(map-set role-permissions ROLE-ADMIN
    {
        can-access-anonymized-data: true,
        can-verify-others: true,
        can-update-permissions: true
    }
)

;; Private functions
(define-private (is-admin (user principal))
    (let ((identity (unwrap! (map-get? user-identities user) false)))
        (and
            (is-eq (get role identity) ROLE-ADMIN)
            (get verification-status identity)
        )
    )
)

(define-private (can-verify (user principal))
    (let ((identity (unwrap! (map-get? user-identities user) false)))
        (let ((permissions (unwrap! (map-get? role-permissions (get role identity)) false)))
            (and
                (get verification-status identity)
                (get can-verify-others permissions)
            )
        )
    )
)

;; Public functions

;; Register new identity
(define-public (register-identity (did (string-utf8 64)) (role uint) (metadata (string-utf8 256)))
    (let ((existing-identity (map-get? user-identities tx-sender)))
        (asserts! (is-none existing-identity) (err err-already-registered))
        (asserts! (or (is-eq role ROLE-PATIENT)
                     (is-eq role ROLE-DOCTOR)
                     (is-eq role ROLE-RESEARCHER))
                 (err err-invalid-role))
        
        (ok (map-set user-identities
            tx-sender
            {
                did: did,
                role: role,
                verification-status: (is-eq role ROLE-PATIENT),  ;; Auto-verify patients
                metadata: metadata,
                registration-time: block-height,
                last-updated: block-height
            }
        ))
    )
)

;; Submit verification request
(define-public (submit-verification-request (proof-document (string-utf8 64)))
    (let ((identity (unwrap! (map-get? user-identities tx-sender)
                            (err err-not-found))))
        (ok (map-set verification-requests
            tx-sender
            {
                proof-document: proof-document,
                requested-role: (get role identity),
                status: u"pending",
                verifier: none  ;; Simply use none for optional values
            }
        ))
    )
)

;; Verify identity (admin only)
(define-public (verify-identity (user principal))
    (let ((request (unwrap! (map-get? verification-requests user)
                           (err err-not-found))))
        (asserts! (can-verify tx-sender) (err err-unauthorized))
        
        (map-set verification-requests user
            (merge request {
                status: u"approved",  ;; Changed to string-utf8
                verifier: (some tx-sender)
            })
        )
        
        (let ((identity (unwrap! (map-get? user-identities user)
                                (err err-not-found))))
            (ok (map-set user-identities user
                (merge identity {
                    verification-status: true,
                    last-updated: block-height
                })
            ))
        )
    )
)

;; Update identity metadata
(define-public (update-metadata (new-metadata (string-utf8 256)))
    (let ((identity (unwrap! (map-get? user-identities tx-sender)
                            (err err-not-found))))
        (ok (map-set user-identities
            tx-sender
            (merge identity {
                metadata: new-metadata,
                last-updated: block-height
            })
        ))
    )
)

;; Check if user has specific permission
(define-read-only (has-permission (user principal) (permission-key (string-utf8 64)))
    (let ((identity (unwrap! (map-get? user-identities user) false)))
        (let ((permissions (unwrap! (map-get? role-permissions (get role identity)) false)))
            (and
                (get verification-status identity)
                (if (is-eq permission-key u"can-verify-others")
                    (get can-verify-others permissions)
                    (if (is-eq permission-key u"can-update-permissions")
                        (get can-update-permissions permissions)
                        (if (is-eq permission-key u"can-access-anonymized-data")
                            (get can-access-anonymized-data permissions)
                            false  ;; default case for unknown permissions
                        )
                    )
                )
            )
        )
    )
)

;; Get identity details
(define-read-only (get-identity (user principal))
    (map-get? user-identities user)
)

;; Get verification request status
(define-read-only (get-verification-request (user principal))
    (map-get? verification-requests user)
)

;; Check if user is verified
(define-read-only (is-verified (user principal))
    (let ((identity (unwrap! (map-get? user-identities user) false)))
        (get verification-status identity)
    )
)

;; Get role permissions
(define-read-only (get-role-permissions (role uint))
    (map-get? role-permissions role)
)