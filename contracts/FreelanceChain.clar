;; FreelanceChain: Decentralized Freelance Marketplace Platform
;; Version: 1.0.0
;; Connects skilled freelancers with clients for project-based work and service delivery

(define-data-var platform-manager principal tx-sender)

(define-map freelancer-profiles
  { freelancer-id: uint }
  {
    provider: principal,
    hourly-rate: uint,
    skill-category: (string-ascii 50),
    portfolio-description: (string-ascii 500),
    experience-years: uint,
    verified: bool
  })

(define-map project-contracts
  { freelancer-id: uint, contract-id: uint }
  {
    client: principal,
    contract-time: uint,
    project-scope: (string-ascii 20)
  })

(define-data-var next-freelancer-id uint u1)

(define-map contract-tracker
  { freelancer-id: uint }
  { contracts: uint })

;; Register as a freelancer
(define-public (register-freelancer (category-input (string-ascii 50)) (description-input (string-ascii 500)) (years-input uint) (rate-input uint))
  (let
    (
      (freelancer-id (var-get next-freelancer-id))
      (contract-id u0)
      (category category-input)
      (description description-input)
      (years years-input)
      (rate rate-input)
    )
    ;; Input validation
    (asserts! (> rate u0) (err u1))
    (asserts! (> (len category) u0) (err u5))
    (asserts! (> (len description) u0) (err u6))
    (asserts! (> years u0) (err u7))
    
    (map-set freelancer-profiles
      { freelancer-id: freelancer-id }
      {
        provider: tx-sender,
        hourly-rate: rate,
        skill-category: category,
        portfolio-description: description,
        experience-years: years,
        verified: false
      }
    )
    (map-set project-contracts
      { freelancer-id: freelancer-id, contract-id: contract-id }
      {
        client: tx-sender,
        contract-time: freelancer-id,
        project-scope: "registered"
      }
    )
    (map-set contract-tracker
      { freelancer-id: freelancer-id }
      { contracts: u1 }
    )
    (var-set next-freelancer-id (+ freelancer-id u1))
    (ok freelancer-id)
  ))

;; Hire a freelancer
(define-public (hire-freelancer (freelancer-id-input uint))
  (let
    (
      (freelancer-id freelancer-id-input)
      (freelancer-info (unwrap! (map-get? freelancer-profiles { freelancer-id: freelancer-id }) (err u2)))
      (rate (get hourly-rate freelancer-info))
      (provider (get provider freelancer-info))
      (contract-data (default-to { contracts: u0 } (map-get? contract-tracker { freelancer-id: freelancer-id })))
      (contract-id (get contracts contract-data))
      (new-contract-id (+ contract-id u1))
    )
    ;; Input validation
    (asserts! (> freelancer-id u0) (err u8))
    (asserts! (not (is-eq tx-sender provider)) (err u3))
    
    (try! (stx-transfer? rate tx-sender provider))
    (map-set project-contracts
      { freelancer-id: freelancer-id, contract-id: contract-id }
      {
        client: tx-sender,
        contract-time: (var-get next-freelancer-id),
        project-scope: "hired"
      }
    )
    (map-set contract-tracker
      { freelancer-id: freelancer-id }
      { contracts: new-contract-id }
    )
    (ok true)
  ))

;; Verify a freelancer (manager only)
(define-public (verify-freelancer (freelancer-id-input uint))
  (let
    (
      (freelancer-id freelancer-id-input)
      (freelancer-info (unwrap! (map-get? freelancer-profiles { freelancer-id: freelancer-id }) (err u2)))
      (contract-data (default-to { contracts: u0 } (map-get? contract-tracker { freelancer-id: freelancer-id })))
      (contract-id (get contracts contract-data))
      (new-contract-id (+ contract-id u1))
    )
    ;; Input validation
    (asserts! (> freelancer-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get platform-manager)) (err u4))
    
    (map-set freelancer-profiles
      { freelancer-id: freelancer-id }
      (merge freelancer-info { verified: true })
    )
    (map-set project-contracts
      { freelancer-id: freelancer-id, contract-id: contract-id }
      {
        client: (get provider freelancer-info),
        contract-time: (var-get next-freelancer-id),
        project-scope: "verified"
      }
    )
    (map-set contract-tracker
      { freelancer-id: freelancer-id }
      { contracts: new-contract-id }
    )
    (ok true)
  ))

;; Get freelancer profile
(define-read-only (get-freelancer (freelancer-id uint))
  (map-get? freelancer-profiles { freelancer-id: freelancer-id }))

;; Get project contract record
(define-read-only (get-contract-record (freelancer-id uint) (contract-id uint))
  (map-get? project-contracts { freelancer-id: freelancer-id, contract-id: contract-id }))

;; Get total contracts for a freelancer
(define-read-only (get-contract-count (freelancer-id uint))
  (let
    (
      (contract-data (default-to { contracts: u0 } (map-get? contract-tracker { freelancer-id: freelancer-id })))
    )
    (get contracts contract-data)
  ))

;; Get platform stats
(define-read-only (get-platform-stats)
  {
    manager: (var-get platform-manager),
    total-freelancers: (- (var-get next-freelancer-id) u1)
  })