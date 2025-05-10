# Cryptoeconomic Primitive Library

A comprehensive Clarity smart contract implementing various cryptoeconomic primitives for DeFi applications on the Stacks blockchain.

## Overview

This smart contract provides a robust framework for building decentralized finance applications with features such as bonding curves, liquidity pools, token swapping, and staking mechanisms. It's designed to be flexible, secure, and gas-efficient, with comprehensive error handling and administrative controls.

## Features

- **Multi-token pool management**: Create and manage liquidity pools for different tokens
- **Customizable bonding curves**: Support for linear, exponential, and constant bonding curves
- **Token swapping**: Weighted swap calculations based on pool reserves and weights
- **Staking system**: Time-locked staking with reward calculation
- **Liquidity provider tracking**: Manage liquidity providers and their contributions
- **Configurable protocol fees**: Set and adjust protocol fees for revenue generation
- **Administrative controls**: Owner-only functions for contract management
- **Emergency mechanisms**: Safety functions for unexpected scenarios

## Contract Structure

The contract is organized into several functional sections:

1. **Constants and Error Codes**: Predefined values and standardized error codes
2. **Contract Variables**: Global state variables
3. **Data Maps**: Storage structures for various contract data
4. **Read-only Functions**: Query functions that don't modify state
5. **Public Functions**: State-modifying functions accessible to users
6. **Private Helper Functions**: Internal utilities for contract operations
7. **Emergency Functions**: Special functions for handling critical situations

## Usage

### Initialization

Before using the contract, it must be initialized with an owner address:

```clarity
(contract-call? .cryptoeconomic-primitives initialize tx-sender)
```

### Creating a Token Pool

To create a new token pool:

```clarity
(contract-call? .cryptoeconomic-primitives create-pool u1 u1000000 u500000)
```

This creates a pool for token ID 1 with an initial reserve of 1,000,000 and a weight of 50% (500,000 out of a maximum of 1,000,000).

### Setting a Bonding Curve

Configure the pricing mechanism for a token:

```clarity
;; Linear curve with slope 10 and y-intercept 1000
(contract-call? .cryptoeconomic-primitives set-bonding-curve u1 "linear" (list u10 u1000 u0 u0 u0))

;; Exponential curve with parameters a=100, b=1050000 (1.05 with 6 decimal precision)
(contract-call? .cryptoeconomic-primitives set-bonding-curve u2 "exponential" (list u100 u1050000 u0 u0 u0))

;; Constant curve with fixed price of 500
(contract-call? .cryptoeconomic-primitives set-bonding-curve u3 "constant" (list u500 u0 u0 u0 u0))
```

### Adding Liquidity

Contribute tokens to a pool:

```clarity
(contract-call? .cryptoeconomic-primitives add-liquidity u1 u5000)
```

This adds 5,000 units of token ID 1 to the corresponding pool.

### Swapping Tokens

Execute a token swap:

```clarity
(contract-call? .cryptoeconomic-primitives swap u1 u2 u1000)
```

This swaps 1,000 units of token ID 1 for token ID 2, based on the current pool reserves and pricing.

### Staking Tokens

Stake tokens to earn rewards:

```clarity
;; Stake 10,000 tokens for 1,000 blocks
(contract-call? .cryptoeconomic-primitives stake u1 u10000 u1000)
```

### Claiming Rewards

Collect rewards without unstaking:

```clarity
(contract-call? .cryptoeconomic-primitives claim-rewards u1)
```

### Unstaking Tokens

Withdraw staked tokens after the lock period:

```clarity
(contract-call? .cryptoeconomic-primitives unstake u1)
```

## Administrative Functions

These functions are restricted to the contract owner:

### Update Contract Owner

```clarity
(contract-call? .cryptoeconomic-primitives set-owner <new-owner-principal>)
```

### Set Protocol Fee

```clarity
;; Set fee to 0.7% (7 in 0.1% units)
(contract-call? .cryptoeconomic-primitives set-protocol-fee u7)
```

### Activate/Deactivate Contract

```clarity
;; Deactivate contract
(contract-call? .cryptoeconomic-primitives set-contract-status false)

;; Activate contract
(contract-call? .cryptoeconomic-primitives set-contract-status true)
```

### Emergency Withdrawal

```clarity
(contract-call? .cryptoeconomic-primitives emergency-withdraw u1 u5000 <recipient-principal>)
```

## Error Codes

| Code | Description |
|------|-------------|
| u100 | Not authorized |
| u101 | Insufficient balance |
| u102 | Invalid parameter |
| u103 | Pool depleted |
| u104 | Owner only function |
| u105 | Contract not active |
| u106 | Already initialized |
| u107 | Time lock not expired |

## Best Practices

When integrating with this contract:

1. **Handle errors properly**: Always check for and handle error responses from function calls
2. **Calculate prices before swapping**: Use the `calculate-swap-price` function to estimate output before executing a swap
3. **Monitor staking positions**: Track the end block for staking positions to know when they can be unstaked
4. **Watch for contract status**: The contract can be deactivated by the owner, which will prevent most operations

## Technical Considerations

- **Precision**: The contract uses a precision factor of 1,000,000 (6 decimal places) for calculations
- **Weights**: Token weights in pools are specified on a scale from 0 to 1,000,000 (representing 0-100%)
- **Block Time**: The contract assumes a block time of approximately 10 minutes for time-based calculations
- **Gas Optimization**: The contract uses iterative approaches instead of recursion where possible to optimize gas usage

## Security

This contract includes several security mechanisms:

- Strict access control for administrative functions
- Comprehensive input validation
- Balance checks before operations
- Emergency functions for critical situations
- Contract activation toggle

## Development and Testing

For local development and testing, we recommend using Clarinet, the Clarity development toolkit. Set up a local Clarinet project:

```bash
# Install Clarinet
npm install -g @hirosystems/clarinet

# Create a new project
clarinet new my-project
cd Curve-Exchange

# Add the contract to your project
cp /path/to/Market-Maker

# Test the contract
clarinet test