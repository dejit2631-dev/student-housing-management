;; Student Housing Management System
;; Manages dormitory and off-campus housing with roommate matching, lease management, and maintenance requests

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-deposit (err u104))
(define-constant err-lease-expired (err u105))
(define-constant err-already-exists (err u106))
(define-constant err-invalid-input (err u107))

;; Data Variables
(define-data-var property-counter uint u0)
(define-data-var lease-counter uint u0)
(define-data-var maintenance-counter uint u0)
(define-data-var roommate-match-counter uint u0)

;; Data Maps
;; Property Management
(define-map properties
    { property-id: uint }
    {
        owner: principal,
        property-type: (string-ascii 20), ;; "dormitory" or "off-campus"
        address: (string-ascii 100),
        total-rooms: uint,
        available-rooms: uint,
        rent-per-room: uint,
        security-deposit: uint,
        amenities: (string-ascii 200),
        status: (string-ascii 20) ;; "active", "inactive", "maintenance"
    }
)

;; Lease Management
(define-map leases
    { lease-id: uint }
    {
        property-id: uint,
        tenant: principal,
        room-number: uint,
        start-date: uint,
        end-date: uint,
        monthly-rent: uint,
        security-deposit: uint,
        deposit-paid: bool,
        status: (string-ascii 20) ;; "active", "pending", "terminated", "expired"
    }
)

;; Maintenance Requests
(define-map maintenance-requests
    { request-id: uint }
    {
        property-id: uint,
        room-number: uint,
        tenant: principal,
        description: (string-ascii 500),
        priority: (string-ascii 10), ;; "low", "medium", "high", "urgent"
        status: (string-ascii 20), ;; "pending", "in-progress", "completed", "cancelled"
        created-at: uint,
        resolved-at: (optional uint)
    }
)

;; Roommate Matching System
(define-map roommate-profiles
    { profile-id: uint }
    {
        student: principal,
        property-id: uint,
        preferences: (string-ascii 300),
        lifestyle: (string-ascii 200),
        budget-range: { min: uint, max: uint },
        status: (string-ascii 20), ;; "seeking", "matched", "inactive"
        created-at: uint
    }
)

(define-map roommate-matches
    { match-id: uint }
    {
        profile-id-1: uint,
        profile-id-2: uint,
        property-id: uint,
        compatibility-score: uint, ;; 1-100
        status: (string-ascii 20), ;; "pending", "accepted", "rejected"
        created-at: uint
    }
)

;; Security Deposits Tracking
(define-map security-deposits
    { lease-id: uint }
    {
        amount: uint,
        status: (string-ascii 20), ;; "held", "returned", "forfeited"
        deductions: uint,
        return-date: (optional uint)
    }
)

;; User mappings for easy lookups
(define-map tenant-leases { tenant: principal } { lease-ids: (list 10 uint) })
(define-map property-owner-properties { owner: principal } { property-ids: (list 20 uint) })

;; Public Functions

;; Property Management
(define-public (add-property 
    (property-type (string-ascii 20))
    (address (string-ascii 100))
    (total-rooms uint)
    (rent-per-room uint)
    (security-deposit uint)
    (amenities (string-ascii 200))
)
    (let (
        (property-id (+ (var-get property-counter) u1))
    )
        (asserts! (> total-rooms u0) err-invalid-input)
        (asserts! (> rent-per-room u0) err-invalid-input)
        (asserts! (> security-deposit u0) err-invalid-input)
        (map-set properties
            { property-id: property-id }
            {
                owner: tx-sender,
                property-type: property-type,
                address: address,
                total-rooms: total-rooms,
                available-rooms: total-rooms,
                rent-per-room: rent-per-room,
                security-deposit: security-deposit,
                amenities: amenities,
                status: "active"
            }
        )
        (var-set property-counter property-id)
        (ok property-id)
    )
)

;; Lease Management
(define-public (create-lease
    (property-id uint)
    (room-number uint)
    (tenant principal)
    (start-date uint)
    (end-date uint)
)
    (let (
        (lease-id (+ (var-get lease-counter) u1))
        (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
    )
        (asserts! (is-eq tx-sender (get owner property)) err-unauthorized)
        (asserts! (is-eq (get status property) "active") err-invalid-status)
        (asserts! (> (get available-rooms property) u0) err-not-found)
        (asserts! (< start-date end-date) err-invalid-input)
        (asserts! (<= room-number (get total-rooms property)) err-invalid-input)
        
        (map-set leases
            { lease-id: lease-id }
            {
                property-id: property-id,
                tenant: tenant,
                room-number: room-number,
                start-date: start-date,
                end-date: end-date,
                monthly-rent: (get rent-per-room property),
                security-deposit: (get security-deposit property),
                deposit-paid: false,
                status: "pending"
            }
        )
        
        ;; Update available rooms
        (map-set properties
            { property-id: property-id }
            (merge property { available-rooms: (- (get available-rooms property) u1) })
        )
        
        (var-set lease-counter lease-id)
        (ok lease-id)
    )
)

;; Security Deposit Payment
(define-public (pay-security-deposit (lease-id uint))
    (let (
        (lease (unwrap! (map-get? leases { lease-id: lease-id }) err-not-found))
    )
        (asserts! (is-eq tx-sender (get tenant lease)) err-unauthorized)
        (asserts! (is-eq (get status lease) "pending") err-invalid-status)
        (asserts! (is-eq (get deposit-paid lease) false) err-already-exists)
        
        ;; In a real implementation, this would handle STX transfer
        ;; For now, we just update the status
        (map-set leases
            { lease-id: lease-id }
            (merge lease { deposit-paid: true, status: "active" })
        )
        
        (map-set security-deposits
            { lease-id: lease-id }
            {
                amount: (get security-deposit lease),
                status: "held",
                deductions: u0,
                return-date: none
            }
        )
        
        (ok true)
    )
)

;; Maintenance Request System
(define-public (submit-maintenance-request
    (property-id uint)
    (room-number uint)
    (description (string-ascii 500))
    (priority (string-ascii 10))
)
    (let (
        (request-id (+ (var-get maintenance-counter) u1))
        (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
    )
        (asserts! (<= room-number (get total-rooms property)) err-invalid-input)
        
        (map-set maintenance-requests
            { request-id: request-id }
            {
                property-id: property-id,
                room-number: room-number,
                tenant: tx-sender,
                description: description,
                priority: priority,
                status: "pending",
                created-at: stacks-block-height,
                resolved-at: none
            }
        )
        
        (var-set maintenance-counter request-id)
        (ok request-id)
    )
)

(define-public (update-maintenance-status
    (request-id uint)
    (new-status (string-ascii 20))
)
    (let (
        (request (unwrap! (map-get? maintenance-requests { request-id: request-id }) err-not-found))
        (property (unwrap! (map-get? properties { property-id: (get property-id request) }) err-not-found))
    )
        (asserts! (is-eq tx-sender (get owner property)) err-unauthorized)
        
        (map-set maintenance-requests
            { request-id: request-id }
            (merge request {
                status: new-status,
                resolved-at: (if (is-eq new-status "completed") (some stacks-block-height) (get resolved-at request))
            })
        )
        
        (ok true)
    )
)

;; Roommate Matching System
(define-public (create-roommate-profile
    (property-id uint)
    (preferences (string-ascii 300))
    (lifestyle (string-ascii 200))
    (min-budget uint)
    (max-budget uint)
)
    (let (
        (profile-id (+ (var-get roommate-match-counter) u1))
    )
        (asserts! (is-some (map-get? properties { property-id: property-id })) err-not-found)
        (asserts! (<= min-budget max-budget) err-invalid-input)
        
        (map-set roommate-profiles
            { profile-id: profile-id }
            {
                student: tx-sender,
                property-id: property-id,
                preferences: preferences,
                lifestyle: lifestyle,
                budget-range: { min: min-budget, max: max-budget },
                status: "seeking",
                created-at: stacks-block-height
            }
        )
        
        (var-set roommate-match-counter profile-id)
        (ok profile-id)
    )
)

;; Terminate Lease
(define-public (terminate-lease (lease-id uint))
    (let (
        (lease (unwrap! (map-get? leases { lease-id: lease-id }) err-not-found))
        (property (unwrap! (map-get? properties { property-id: (get property-id lease) }) err-not-found))
    )
        (asserts! (or (is-eq tx-sender (get tenant lease)) (is-eq tx-sender (get owner property))) err-unauthorized)
        (asserts! (is-eq (get status lease) "active") err-invalid-status)
        
        ;; Update lease status
        (map-set leases
            { lease-id: lease-id }
            (merge lease { status: "terminated" })
        )
        
        ;; Increase available rooms
        (map-set properties
            { property-id: (get property-id lease) }
            (merge property { available-rooms: (+ (get available-rooms property) u1) })
        )
        
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-property (property-id uint))
    (map-get? properties { property-id: property-id })
)

(define-read-only (get-lease (lease-id uint))
    (map-get? leases { lease-id: lease-id })
)

(define-read-only (get-maintenance-request (request-id uint))
    (map-get? maintenance-requests { request-id: request-id })
)

(define-read-only (get-roommate-profile (profile-id uint))
    (map-get? roommate-profiles { profile-id: profile-id })
)

(define-read-only (get-security-deposit (lease-id uint))
    (map-get? security-deposits { lease-id: lease-id })
)

(define-read-only (get-property-counter)
    (var-get property-counter)
)

(define-read-only (get-lease-counter)
    (var-get lease-counter)
)

(define-read-only (get-maintenance-counter)
    (var-get maintenance-counter)
)

(define-read-only (get-roommate-counter)
    (var-get roommate-match-counter)
)
