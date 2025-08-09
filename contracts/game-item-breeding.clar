(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-cooldown (err u102))
(define-constant err-same-parent (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-generation (err u105))
(define-constant err-creature-exhausted (err u106))
(define-constant err-same-creature (err u107))
(define-constant err-insufficient-health (err u108))

(define-non-fungible-token creature uint)

(define-data-var next-creature-id uint u1)
(define-data-var base-breeding-cost uint u1000000)
(define-data-var breeding-cooldown uint u144)
(define-data-var battle-cooldown uint u72)
(define-data-var battle-reward uint u100000)
(define-data-var next-battle-id uint u1)

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
    breed-count: uint,
    last-battle: uint,
    battle-wins: uint,
    battle-losses: uint,
    current-health: uint
  }
)

(define-map battle-results
  uint
  {
    attacker-id: uint,
    defender-id: uint,
    winner-id: uint,
    attacker-damage: uint,
    defender-damage: uint,
    block-height: uint,
    reward-amount: uint
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

(define-read-only (can-battle (creature-id uint))
  (match (get-creature creature-id)
    creature-info
    (let
      (
        (last-battle (get last-battle creature-info))
        (current-block stacks-block-height)
        (current-health (get current-health creature-info))
      )
      (and 
        (>= (- current-block last-battle) (var-get battle-cooldown))
        (> current-health u0)
      )
    )
    false
  )
)

(define-read-only (get-battle-result (battle-id uint))
  (map-get? battle-results battle-id)
)

(define-read-only (calculate-damage (attacker-power uint) (defender-vitality uint) (random-factor uint))
  (let
    (
      (base-damage (/ (* attacker-power u80) u100))
      (defense-reduction (/ (* defender-vitality u20) u100))
      (randomness (mod random-factor u21))
      (damage-variation (if (< randomness u10) (- randomness u10) (- u10 randomness)))
      (final-damage (+ (- base-damage defense-reduction) damage-variation))
    )
    (if (< final-damage u1) u1 final-damage)
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
        breed-count: u0,
        last-battle: u0,
        battle-wins: u0,
        battle-losses: u0,
        current-health: (+ u20 (mod (+ random-seed u3) u61))
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
        breed-count: u0,
        last-battle: u0,
        battle-wins: u0,
        battle-losses: u0,
        current-health: (calculate-trait (get vitality parent1) (get vitality parent2) (mod (+ random-base u4) u100))
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

(define-public (battle-creatures (attacker-id uint) (defender-id uint))
  (let
    (
      (attacker (unwrap! (get-creature attacker-id) err-not-found))
      (defender (unwrap! (get-creature defender-id) err-not-found))
      (battle-id (var-get next-battle-id))
      (random-seed (+ stacks-block-height battle-id attacker-id defender-id))
      (attacker-power (+ (get strength attacker) (get speed attacker) (get intelligence attacker)))
      (defender-power (+ (get strength defender) (get speed defender) (get intelligence defender)))
      (attacker-damage (calculate-damage attacker-power (get vitality defender) (mod random-seed u100)))
      (defender-damage (calculate-damage defender-power (get vitality attacker) (mod (+ random-seed u1) u100)))
      (attacker-new-health (if (> (get current-health attacker) defender-damage) (- (get current-health attacker) defender-damage) u0))
      (defender-new-health (if (> (get current-health defender) attacker-damage) (- (get current-health defender) attacker-damage) u0))
      (winner-id (if (> attacker-new-health defender-new-health) attacker-id defender-id))
      (reward (var-get battle-reward))
    )
    (asserts! (not (is-eq attacker-id defender-id)) err-same-creature)
    (asserts! (is-eq tx-sender (get owner attacker)) err-unauthorized)
    (asserts! (can-battle attacker-id) err-creature-exhausted)
    (asserts! (can-battle defender-id) err-creature-exhausted)
    (asserts! (> (get current-health attacker) u0) err-insufficient-health)
    (asserts! (> (get current-health defender) u0) err-insufficient-health)
    
    (map-set battle-results battle-id
      {
        attacker-id: attacker-id,
        defender-id: defender-id,
        winner-id: winner-id,
        attacker-damage: attacker-damage,
        defender-damage: defender-damage,
        block-height: stacks-block-height,
        reward-amount: reward
      }
    )
    
    (map-set creature-data attacker-id
      (merge attacker {
        last-battle: stacks-block-height,
        current-health: attacker-new-health,
        battle-wins: (if (is-eq winner-id attacker-id) (+ (get battle-wins attacker) u1) (get battle-wins attacker)),
        battle-losses: (if (is-eq winner-id defender-id) (+ (get battle-losses attacker) u1) (get battle-losses attacker))
      })
    )
    
    (map-set creature-data defender-id
      (merge defender {
        last-battle: stacks-block-height,
        current-health: defender-new-health,
        battle-wins: (if (is-eq winner-id defender-id) (+ (get battle-wins defender) u1) (get battle-wins defender)),
        battle-losses: (if (is-eq winner-id attacker-id) (+ (get battle-losses defender) u1) (get battle-losses defender))
      })
    )
    
    (if (is-eq winner-id attacker-id)
      (try! (stx-transfer? reward contract-owner tx-sender))
      true
    )
    
    (var-set next-battle-id (+ battle-id u1))
    (ok { winner: winner-id, battle-id: battle-id, reward: (if (is-eq winner-id attacker-id) reward u0) })
  )
)

(define-public (heal-creature (creature-id uint))
  (let
    (
      (creature-info (unwrap! (get-creature creature-id) err-not-found))
      (healing-cost (* (get vitality creature-info) u1000))
      (full-health (get vitality creature-info))
    )
    (asserts! (is-eq tx-sender (get owner creature-info)) err-unauthorized)
    (asserts! (< (get current-health creature-info) full-health) err-insufficient-health)
    (try! (stx-transfer? healing-cost tx-sender contract-owner))
    (map-set creature-data creature-id (merge creature-info { current-health: full-health }))
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

(define-public (set-battle-cooldown (new-cooldown uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set battle-cooldown new-cooldown)
    (ok true)
  )
)

(define-public (set-battle-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set battle-reward new-reward)
    (ok true)
  )
)

(define-read-only (get-battle-cooldown)
  (var-get battle-cooldown)
)

(define-read-only (get-battle-reward)
  (var-get battle-reward)
)

(define-read-only (get-creature-battle-stats (creature-id uint))
  (match (get-creature creature-id)
    creature-info
    (some {
      battle-wins: (get battle-wins creature-info),
      battle-losses: (get battle-losses creature-info),
      current-health: (get current-health creature-info),
      can-battle: (can-battle creature-id)
    })
    none
  )
)
