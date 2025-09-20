;; Food Processing Facility Management System
;; HACCP monitoring, inspection scheduling, batch tracking, and recall coordination

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-INPUT (err u102))
(define-constant ERR-BATCH-CONTAMINATED (err u103))
(define-constant ERR-INSPECTION-FAILED (err u104))

;; Data variables
(define-data-var facility-manager principal tx-sender)
(define-data-var next-batch-id uint u1)
(define-data-var next-inspection-id uint u1)

;; HACCP status levels
(define-constant STATUS-SAFE u1)
(define-constant STATUS-CRITICAL u2)
(define-constant STATUS-CONTAMINATED u3)

;; Data maps
(define-map production-batches
  { batch-id: uint }
  {
    product-name: (string-ascii 100),
    production-date: uint,
    expiry-date: uint,
    quantity: uint,
    haccp-status: uint,
    temperature-log: (string-ascii 200),
    quality-inspector: principal,
    released: bool
  }
)

(define-map haccp-monitoring
  { batch-id: uint, checkpoint: (string-ascii 50) }
  {
    temperature: uint,
    ph-level: uint,
    moisture-content: uint,
    recorded-at: uint,
    inspector: principal,
    compliant: bool
  }
)

(define-map facility-inspections
  { inspection-id: uint }
  {
    inspection-date: uint,
    inspector: principal,
    inspection-type: (string-ascii 50),
    compliance-score: uint,
    violations: (string-ascii 500),
    corrective-actions: (string-ascii 300),
    passed: bool
  }
)

(define-map recall-notices
  { batch-id: uint }
  {
    recall-reason: (string-ascii 300),
    severity-level: uint,
    initiated-at: uint,
    affected-quantity: uint,
    status: (string-ascii 50)
  }
)

(define-map authorized-inspectors
  { inspector: principal }
  { authorized: bool, certification: (string-ascii 100) }
)

;; Public functions
(define-public (create-production-batch (product-name (string-ascii 100)) (quantity uint) (shelf-life-days uint))
  (let
    (
      (batch-id (var-get next-batch-id))
      (production-date stacks-block-height)
      (expiry-date (+ production-date shelf-life-days))
    )
    (asserts! (is-authorized-inspector tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> (len product-name) u0) ERR-INVALID-INPUT)
    (asserts! (> quantity u0) ERR-INVALID-INPUT)
    
    ;; Create batch record
    (map-set production-batches
      { batch-id: batch-id }
      {
        product-name: product-name,
        production-date: production-date,
        expiry-date: expiry-date,
        quantity: quantity,
        haccp-status: STATUS-SAFE,
        temperature-log: "",
        quality-inspector: tx-sender,
        released: false
      }
    )
    
    ;; Increment batch ID
    (var-set next-batch-id (+ batch-id u1))
    
    (ok batch-id)
  )
)

(define-public (record-haccp-checkpoint (batch-id uint) (checkpoint (string-ascii 50)) (temperature uint) (ph-level uint) (moisture-content uint))
  (let
    (
      (batch (unwrap! (map-get? production-batches { batch-id: batch-id }) ERR-NOT-FOUND))
      (compliant (and (<= temperature u40) (>= ph-level u35) (<= moisture-content u15))) ;; Example thresholds
    )
    (asserts! (is-authorized-inspector tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get released batch)) ERR-INVALID-INPUT)
    
    ;; Record HACCP data
    (map-set haccp-monitoring
      { batch-id: batch-id, checkpoint: checkpoint }
      {
        temperature: temperature,
        ph-level: ph-level,
        moisture-content: moisture-content,
        recorded-at: stacks-block-height,
        inspector: tx-sender,
        compliant: compliant
      }
    )
    
    ;; Update batch status if non-compliant
    (if (not compliant)
      (map-set production-batches
        { batch-id: batch-id }
        (merge batch { haccp-status: STATUS-CRITICAL })
      )
      true
    )
    
    (ok compliant)
  )
)

(define-public (schedule-inspection (inspection-type (string-ascii 50)))
  (let
    (
      (inspection-id (var-get next-inspection-id))
    )
    (asserts! (is-eq tx-sender (var-get facility-manager)) ERR-NOT-AUTHORIZED)
    (asserts! (> (len inspection-type) u0) ERR-INVALID-INPUT)
    
    ;; Create inspection record
    (map-set facility-inspections
      { inspection-id: inspection-id }
      {
        inspection-date: stacks-block-height,
        inspector: tx-sender,
        inspection-type: inspection-type,
        compliance-score: u0,
        violations: "",
        corrective-actions: "",
        passed: false
      }
    )
    
    ;; Increment inspection ID
    (var-set next-inspection-id (+ inspection-id u1))
    
    (ok inspection-id)
  )
)

(define-public (complete-inspection (inspection-id uint) (compliance-score uint) (violations (string-ascii 500)) (corrective-actions (string-ascii 300)))
  (let
    (
      (inspection (unwrap! (map-get? facility-inspections { inspection-id: inspection-id }) ERR-NOT-FOUND))
      (passed (>= compliance-score u80)) ;; 80% threshold
    )
    (asserts! (is-authorized-inspector tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= compliance-score u100) ERR-INVALID-INPUT)
    
    ;; Update inspection results
    (map-set facility-inspections
      { inspection-id: inspection-id }
      (merge inspection {
        compliance-score: compliance-score,
        violations: violations,
        corrective-actions: corrective-actions,
        passed: passed
      })
    )
    
    (ok passed)
  )
)

(define-public (release-batch (batch-id uint))
  (let
    (
      (batch (unwrap! (map-get? production-batches { batch-id: batch-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-authorized-inspector tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get haccp-status batch) STATUS-SAFE) ERR-BATCH-CONTAMINATED)
    (asserts! (not (get released batch)) ERR-INVALID-INPUT)
    
    ;; Mark batch as released
    (map-set production-batches
      { batch-id: batch-id }
      (merge batch { released: true })
    )
    
    (ok true)
  )
)

(define-public (initiate-recall (batch-id uint) (recall-reason (string-ascii 300)) (severity-level uint))
  (let
    (
      (batch (unwrap! (map-get? production-batches { batch-id: batch-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (var-get facility-manager)) ERR-NOT-AUTHORIZED)
    (asserts! (get released batch) ERR-INVALID-INPUT)
    (asserts! (<= severity-level u5) ERR-INVALID-INPUT)
    
    ;; Create recall notice
    (map-set recall-notices
      { batch-id: batch-id }
      {
        recall-reason: recall-reason,
        severity-level: severity-level,
        initiated-at: stacks-block-height,
        affected-quantity: (get quantity batch),
        status: "active"
      }
    )
    
    ;; Update batch status
    (map-set production-batches
      { batch-id: batch-id }
      (merge batch { haccp-status: STATUS-CONTAMINATED })
    )
    
    (ok true)
  )
)

(define-public (authorize-inspector (inspector principal) (certification (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender (var-get facility-manager)) ERR-NOT-AUTHORIZED)
    (map-set authorized-inspectors
      { inspector: inspector }
      { authorized: true, certification: certification }
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-batch (batch-id uint))
  (map-get? production-batches { batch-id: batch-id })
)

(define-read-only (get-haccp-data (batch-id uint) (checkpoint (string-ascii 50)))
  (map-get? haccp-monitoring { batch-id: batch-id, checkpoint: checkpoint })
)

(define-read-only (get-inspection (inspection-id uint))
  (map-get? facility-inspections { inspection-id: inspection-id })
)

(define-read-only (get-recall-notice (batch-id uint))
  (map-get? recall-notices { batch-id: batch-id })
)

(define-read-only (is-authorized-inspector (user principal))
  (match (map-get? authorized-inspectors { inspector: user })
    some-auth (get authorized some-auth)
    false
  )
)

(define-read-only (is-batch-safe (batch-id uint))
  (match (map-get? production-batches { batch-id: batch-id })
    some-batch (is-eq (get haccp-status some-batch) STATUS-SAFE)
    false
  )
)

(define-read-only (get-next-batch-id)
  (var-get next-batch-id)
)
