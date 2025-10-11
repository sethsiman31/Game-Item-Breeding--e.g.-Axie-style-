# 🧬 Game Item Breeding Contract

> **Axie-style creature breeding with on-chain genetic algorithms** 🔬⚡

## 🎮 Overview

A Clarity smart contract implementing genetic breeding mechanics similar to Axie Infinity. Creatures have four genetic traits that combine through breeding to create unique offspring with inherited characteristics.

## ✨ Features

- 🧪 **Genetic Traits**: Strength, Speed, Intelligence, Vitality (1-100 range)
- 🔄 **Breeding Algorithm**: Trait averaging with random mutations
- 🌟 **Generation System**: Higher generations cost more to breed
- ⏰ **Breeding Cooldown**: Prevents spam breeding (144 blocks default)
- 🎯 **NFT-based Ownership**: Each creature is a unique NFT
- 💰 **Dynamic Pricing**: Breeding costs increase with generation

## 🚀 Quick Start

### Deploy Contract
```bash
clarinet deploy
```

### Mint Genesis Creatures
```clarity
(contract-call? .game-item-breeding mint-genesis-creature 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### Breed Two Creatures
```clarity
(contract-call? .game-item-breeding breed-creatures u1 u2)
```

### View Creature Data
```clarity
(contract-call? .game-item-breeding get-creature u1)
```

## 🧬 Genetic System

### Trait Calculation
- **Base**: Average of parent traits
- **Mutation**: Random variation (-10 to +10)
- **Range**: Final traits clamped to 1-100

### Generation Rules
- Genesis creatures: Generation 0
- Offspring: Max parent generation + 1
- Max generation: 10 (prevents infinite breeding)

## 📊 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `mint-genesis-creature` | 🎲 Create base creature | `recipient: principal` |
| `breed-creatures` | 🔄 Breed two creatures | `parent1-id: uint`, `parent2-id: uint` |
| `transfer-creature` | 📦 Transfer ownership | `creature-id: uint`, `recipient: principal` |
| `set-breeding-cost` | 💰 Update base cost (owner only) | `new-cost: uint` |
| `set-breeding-cooldown` | ⏱️ Update cooldown (owner only) | `new-cooldown: uint` |

### Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-creature` | 📋 Get creature data | Creature info or none |
| `get-user-creatures` | 👤 List user's creatures | List of creature IDs |
| `get-creature-power` | ⚡ Total trait sum | Combined power score |
| `can-breed` | ✅ Check breeding eligibility | Boolean |
| `get-breeding-cost` | 💵 Cost for generation | STX amount |

## 💎 Creature Structure

```clarity
{
  owner: principal,           // Current owner
  strength: uint,            // Combat trait (1-100)
  speed: uint,               // Agility trait (1-100)
  intelligence: uint,        // Magic trait (1-100)
  vitality: uint,            // Health trait (1-100)
  generation: uint,          // Breeding generation
  parent1: (optional uint),  // First parent ID
  parent2: (optional uint),  // Second parent ID
  last-bred: uint,           // Last breeding block
  breed-count: uint          // Times used for breeding
}
```

## ⚙️ Configuration

- **Base Breeding Cost**: 1,000,000 µSTX
- **Generation Multiplier**: +500,000 µSTX per generation
- **Breeding Cooldown**: 144 blocks (~24 hours)
- **Max Creatures per User**: 100
- **Max Generation**: 10

## 🔧 Testing

```bash
# Run all tests
clarinet test

# Check contract syntax
clarinet check
```

## 🎯 Example Workflow

1. **Deploy**: `clarinet deploy`
2. **Mint**: Create genesis creatures with random traits
3. **Breed**: Combine two creatures to create offspring
4. **Evolve**: Higher generation creatures have better potential
5. **Trade**: Transfer creatures between players

## 🛡️ Security Features

- Owner-only genesis minting
- Breeding cooldown prevents spam
- Generation limits prevent infinite breeding
- Proper ownership validation
- STX payment required for breeding

## 📈 Economics

- Breeding costs increase exponentially with generation
- Cooldown creates scarcity and planning strategy
- Higher generation creatures are more expensive but potentially more powerful

---

*Built with ❤️ using Clarity and Stacks blockchain*
