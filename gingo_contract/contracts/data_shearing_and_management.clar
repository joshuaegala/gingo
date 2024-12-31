
;; title: data_shearing_and_management
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



(define-map consents 
  { owner: principal, requester: principal } 
  { 
    granted: bool, 
    purpose: (string-ascii 50), 
    expiration: uint,
    data-types: (list 10 (string-ascii 20))
  }
)

;; Error constants
(define-constant err-not-owner (err u100))
(define-constant err-already-granted (err u101))
(define-constant err-not-granted (err u102))
(define-constant err-expired (err u103))

;; Function to grant consent
(define-public (grant-consent (requester principal) (purpose (string-ascii 50)) (duration uint) (data-types (list 10 (string-ascii 20))))
  (let ((consent-key { owner: tx-sender, requester: requester }))
    (asserts! (is-eq tx-sender (get owner consent-key)) err-not-owner)
    (asserts! (is-none (map-get? consents consent-key)) err-already-granted)
    (ok (map-set consents 
      consent-key
      {
        granted: true,
        purpose: purpose,
        expiration: (+ block-height duration),
        data-types: data-types
      }
    ))
  )
)

;; Function to revoke consent
(define-public (revoke-consent (requester principal))
  (let ((consent-key { owner: tx-sender, requester: requester }))
    (asserts! (is-eq tx-sender (get owner consent-key)) err-not-owner)
    (asserts! (is-some (map-get? consents consent-key)) err-not-granted)
    (ok (map-delete consents consent-key))
  )
)

;; Function to check if consent is granted
(define-read-only (check-consent (owner principal) (requester principal))
  (match (map-get? consents { owner: owner, requester: requester })
    consent-data (if (and (get granted consent-data) (< block-height (get expiration consent-data)))
                   (ok consent-data)
                   (err u403))
    (err u404)
  )
)

;; Function to get all data types consented for a specific requester
(define-read-only (get-consented-data-types (owner principal) (requester principal))
  (match (map-get? consents { owner: owner, requester: requester })
    consent-data (if (and (get granted consent-data) (< block-height (get expiration consent-data)))
                   (ok (get data-types consent-data))
                   (err u403))
    (err u404)
  )
)

;; Function to update consent (revoke and grant in one transaction)
(define-public (update-consent (requester principal) (purpose (string-ascii 50)) (duration uint) (data-types (list 10 (string-ascii 20))))
  (begin
    (try! (revoke-consent requester))
    (grant-consent requester purpose duration data-types)
  )
)