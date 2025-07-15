(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-cooldown (err u102))
(define-constant err-same-parent (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-generation (err u105))

(define-non-fungible-token creature uint)

(define-data-var next-creature-id uint u1)
(define-data-var base-breeding-cost uint u1000000)
(define-data-var breeding-cooldown uint u144)

(define-map creature-data
  uint
  {
    owner: principal,
    strength: uint,
    speed: uint,
    intelligence: uint,
    vitality: uint,
    generation: uint,
    parent1: (optional uint),
    parent2: (optional uint),
    last-bred: uint,
    breed-count: uint
  }
)

(define-read-only (get-creature (creature-id uint))
  (map-get? creature-data creature-id)
)

(define-read-only (get-next-creature-id)
  (var-get next-creature-id)
)

(define-read-only (get-breeding-cost (generation uint))
  (+ (var-get base-breeding-cost) (* generation u500000))
)

(define-read-only (calculate-trait (parent1-trait uint) (parent2-trait uint) (random-factor uint))
  (let
    (
      (base-average (/ (+ parent1-trait parent2-trait) u2))
      (variation (mod random-factor u21))
      (mutation (if (< variation u10) (- variation u10) (- u10 variation)))
      (raw-trait (+ base-average mutation))
    )
    (if (< raw-trait u1) u1 (if (> raw-trait u100) u100 raw-trait))
  )
)

(define-read-only (can-breed (creature-id uint))
  (match (get-creature creature-id)
    creature-info
    (let
      (
        (last-bred (get last-bred creature-info))
        (current-block stacks-block-height)
      )
      (>= (- current-block last-bred) (var-get breeding-cooldown))
    )
    false
  )
)

(define-public (mint-genesis-creature (recipient principal))
  (let
    (
      (creature-id (var-get next-creature-id))
      (random-seed (+ stacks-block-height creature-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (nft-mint? creature creature-id recipient))
    (map-set creature-data creature-id
      {
        owner: recipient,
        strength: (+ u20 (mod random-seed u61)),
        speed: (+ u20 (mod (+ random-seed u1) u61)),
        intelligence: (+ u20 (mod (+ random-seed u2) u61)),
        vitality: (+ u20 (mod (+ random-seed u3) u61)),
        generation: u0,
        parent1: none,
        parent2: none,
        last-bred: u0,
        breed-count: u0
      }
    )
    (var-set next-creature-id (+ creature-id u1))
    (ok creature-id)
  )
)

(define-public (breed-creatures (parent1-id uint) (parent2-id uint))
  (let
    (
      (parent1 (unwrap! (get-creature parent1-id) err-not-found))
      (parent2 (unwrap! (get-creature parent2-id) err-not-found))
      (creature-id (var-get next-creature-id))
      (generation (+ (if (> (get generation parent1) (get generation parent2)) (get generation parent1) (get generation parent2)) u1))
      (breeding-cost (get-breeding-cost generation))
      (random-base (+ stacks-block-height creature-id parent1-id parent2-id))
    )
    (asserts! (not (is-eq parent1-id parent2-id)) err-same-parent)
    (asserts! (is-eq tx-sender (get owner parent1)) err-unauthorized)
    (asserts! (is-eq tx-sender (get owner parent2)) err-unauthorized)
    (asserts! (can-breed parent1-id) err-insufficient-cooldown)
    (asserts! (can-breed parent2-id) err-insufficient-cooldown)
    (asserts! (< generation u10) err-invalid-generation)
    
    (try! (stx-transfer? breeding-cost tx-sender contract-owner))
    (try! (nft-mint? creature creature-id tx-sender))
    
    (map-set creature-data creature-id
      {
        owner: tx-sender,
        strength: (calculate-trait (get strength parent1) (get strength parent2) (mod random-base u100)),
        speed: (calculate-trait (get speed parent1) (get speed parent2) (mod (+ random-base u1) u100)),
        intelligence: (calculate-trait (get intelligence parent1) (get intelligence parent2) (mod (+ random-base u2) u100)),
        vitality: (calculate-trait (get vitality parent1) (get vitality parent2) (mod (+ random-base u3) u100)),
        generation: generation,
        parent1: (some parent1-id),
        parent2: (some parent2-id),
        last-bred: u0,
        breed-count: u0
      }
    )
    
    (map-set creature-data parent1-id
      (merge parent1 { 
        last-bred: stacks-block-height, 
        breed-count: (+ (get breed-count parent1) u1) 
      })
    )
    
    (map-set creature-data parent2-id
      (merge parent2 { 
        last-bred: stacks-block-height, 
        breed-count: (+ (get breed-count parent2) u1) 
      })
    )
    
    (var-set next-creature-id (+ creature-id u1))
    (ok creature-id)
  )
)

(define-public (transfer-creature (creature-id uint) (recipient principal))
  (let
    (
      (creature-info (unwrap! (get-creature creature-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner creature-info)) err-unauthorized)
    (try! (nft-transfer? creature creature-id tx-sender recipient))
    (map-set creature-data creature-id (merge creature-info { owner: recipient }))
    (ok true)
  )
)

(define-public (set-breeding-cost (new-cost uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set base-breeding-cost new-cost)
    (ok true)
  )
)

(define-public (set-breeding-cooldown (new-cooldown uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set breeding-cooldown new-cooldown)
    (ok true)
  )
)

(define-read-only (get-creature-power (creature-id uint))
  (match (get-creature creature-id)
    creature-info
    (+ (get strength creature-info) 
       (get speed creature-info) 
       (get intelligence creature-info) 
       (get vitality creature-info))
    u0
  )
)

(define-read-only (get-breeding-cooldown)
  (var-get breeding-cooldown)
)

(define-read-only (get-base-breeding-cost)
  (var-get base-breeding-cost)
)
