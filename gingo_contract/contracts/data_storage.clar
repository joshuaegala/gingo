
;; title: data_storage
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; Health Data Storage and Access Control Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-not-found (err u103))

;; Data Variables
(define-map health-records
    principal
    {
        data-hash: (string-utf8 64),        ;; IPFS hash or encrypted data reference
        is-active: bool,
        last-updated: uint
    }
)

(define-map access-permissions
    {owner: principal, accessor: principal}
    {
        can-read: bool,
        can-write: bool,
        expiration: uint
    }
)

;; Private Functions
(define-private (is-authorized (owner principal) (accessor principal))
    (let ((permissions (unwrap! (map-get? access-permissions {owner: owner, accessor: accessor})
                               false)))
        (and (get can-read permissions)
             (> (get expiration permissions) block-height))
    )
)

;; Public Functions

;; Store or update health data
(define-public (store-health-data (data-hash (string-utf8 64)))
    (let ((existing-record (map-get? health-records tx-sender)))
        (ok (map-set health-records
            tx-sender
            {
                data-hash: data-hash,
                is-active: true,
                last-updated: block-height
            }
        ))
    )
)

;; Grant access to another principal
(define-public (grant-access (accessor principal) (can-read bool) (can-write bool) (duration uint))
    (ok (map-set access-permissions
        {owner: tx-sender, accessor: accessor}
        {
            can-read: can-read,
            can-write: can-write,
            expiration: (+ block-height duration)
        }
    ))
)

;; Revoke access from a principal
(define-public (revoke-access (accessor principal))
    (ok (map-delete access-permissions
        {owner: tx-sender, accessor: accessor}
    ))
)

;; Read health data (only if authorized)
(define-public (read-health-data (owner principal))
    (let ((record (unwrap! (map-get? health-records owner)
                          (err err-not-found))))
        (if (or (is-eq tx-sender owner)
                (is-authorized owner tx-sender))
            (ok record)
            (err err-unauthorized)
        )
    )
)

;; Delete health data
(define-public (delete-health-data)
    (ok (map-delete health-records tx-sender))
)

;; Check if has access
(define-read-only (check-access (owner principal) (accessor principal))
    (let ((permissions (map-get? access-permissions {owner: owner, accessor: accessor})))
        (if (is-none permissions)
            false
            (let ((unwrapped-permissions (unwrap-panic permissions)))
                (and (get can-read unwrapped-permissions)
                     (> (get expiration unwrapped-permissions) block-height))
            )
        )
    )
)

;; Get access details
(define-read-only (get-access-details (owner principal) (accessor principal))
    (map-get? access-permissions {owner: owner, accessor: accessor})
)