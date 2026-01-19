# Market Dynamics Examples - Chatman Equation

This directory contains TOML-based examples demonstrating the Chatman Equation **A = μ(O)** for market dynamics modeling.

## Overview

Each example demonstrates:
- **Observations (O)**: Raw market data and metrics
- **Closure Operator (μ)**: Transformation rules that process observations
- **Artifacts (A)**: Predicted outcomes and insights from μ(O)
- **Verification**: Proofs of determinism, idempotence, and composability

## Examples

### 1. Order Book Divergence (`order-book-divergence.toml`)

**Domain**: Market Microstructure
**Equation**: `Opportunities = μ(OrderBookSnapshots)`

Demonstrates how bid-ask spread divergence across exchanges creates arbitrage opportunities.

**Key Features**:
- Cross-exchange arbitrage detection
- Spread normalization
- Volume-weighted opportunity scoring
- Execution plan generation

**Observations**:
- Order book snapshots from 3 exchanges (BTC/USD)
- Bid/ask prices and volumes
- Spread in basis points

**Artifacts**:
- 1 arbitrage opportunity detected
- Buy at ExchangeC ($45,000), sell at ExchangeB ($45,020)
- Net profit: 34.4 bps after fees

**Properties**:
- Deterministic: 100/100 runs identical output
- Idempotent: μ(μ(O)) = μ(O) verified
- Convergence guaranteed

---

### 2. Network Effects (`network-effects.toml`)

**Domain**: Platform Economics
**Equation**: `NetworkValue = μ(UserActivityGraph)`

Models how user growth creates non-linear value emergence via Metcalfe's Law.

**Key Features**:
- Metcalfe's Law (V = k × n²) application
- Critical mass detection
- Engagement clustering analysis
- Growth phase classification

**Observations**:
- 10,000 total users, 7,500 daily active
- User connection graph with 125,000 edges
- Network density: 0.0025
- Clustering coefficient: 0.34

**Artifacts**:
- Total network value: $1,875,000
- Value per user: $187.50
- Critical mass achieved (>5,000 users)
- Growth phase: Exponential

**Projections**:
- 3 months: 15,000 users, $4.2M value
- 6 months: 22,500 users, $9.5M value

**Properties**:
- Deterministic: 1000/1000 runs identical
- Idempotent: First and second application yield same value
- Composable: Graph unions respect network effects

---

### 3. Timing Windows (`timing-windows.toml`)

**Domain**: Algorithmic Trading
**Equation**: `TradingSignals = μ(PriceWindows)`

Detects optimal entry/exit points via closure operator on price action windows.

**Key Features**:
- Momentum breakout detection
- Mean reversion setup identification
- MACD convergence signals
- Volume confirmation
- Risk management (stop-loss, take-profit)

**Observations**:
- 5-minute AAPL candles (20-period window)
- Technical indicators: SMA, EMA, RSI, MACD, Bollinger Bands
- Volume profile with point of control

**Artifacts**:
- 2 signals generated (1 high confidence, 1 below threshold)
- Mean reversion long signal at $185.70
- Confidence: 75%
- Risk/reward ratio: 2.3

**Properties**:
- Deterministic: 500/500 runs identical signals
- Idempotent: Re-running on same window produces identical signals
- Temporal consistency: 92% across overlapping windows

---

### 4. Moat Formation (`moat-formation.toml`)

**Domain**: Strategic Positioning
**Equation**: `CompetitiveAdvantage = μ(MarketPositionData)`

Analyzes competitive barriers and moat strength through multi-dimensional closure.

**Key Features**:
- Network effects moat calculation
- Scale economies analysis
- Switching costs measurement
- Intangible assets (IP, brand) valuation
- Threat assessment and reinforcement opportunities

**Observations**:
- Market share: 38%
- Gross margin: 72%
- 125,000 customers (42% enterprise)
- 450 patents, 12 regulatory approvals
- Net retention rate: 125%

**Artifacts**:
- Overall moat strength: 8.7/10 (Wide moat)
- Components:
  - Network effects: 35% contribution
  - Switching costs: 30%
  - Scale economies: 25%
  - Intangible assets: 10%
- Estimated moat duration: 12 years

**Properties**:
- Deterministic: 200/200 runs identical score
- Idempotent: Moat analysis stable across applications
- Temporal stability: 95% confidence interval [8.4, 9.0]

---

## Configuration-Driven Execution

All examples are **purely configuration-driven** with no hand-coded logic:

1. **Load TOML**: Parse configuration file
2. **Validate Schema**: Zod validation of structure
3. **Apply μ Operator**: Execute rules in closure operator
4. **Generate Artifacts**: Produce predicted outcomes
5. **Verify Properties**: Test determinism, idempotence, composability

```javascript
import { loadAndGenerateEquation } from '@unrdf/chatman-equation/generator';

// Load TOML and create executable equation
const equation = await loadAndGenerateEquation('./order-book-divergence.toml');

// Execute: A = μ(O)
const result = equation.execute();
console.log(result.artifacts);

// Verify properties
const verification = equation.verify(100);
console.log(verification.determinism.score); // 1.0
console.log(verification.idempotence.idempotent); // true
```

## Mathematical Properties

All examples satisfy:

### 1. Determinism
**μ(O)** always produces the same **A** for the same **O**

```toml
[verification.determinism]
run_count = 100
unique_outputs = 1
determinism_score = 1.0
```

### 2. Idempotence
**μ(μ(O)) = μ(O)**

```toml
[verification.idempotence]
first_run = <result>
second_run = <result>
idempotent = true
```

### 3. Composability
**μ₁ ∘ μ₂** produces valid results

```toml
[verification.composability]
composable = true
note = "μ(O₁ ∪ O₂) respects union semantics"
```

### 4. Invariants Preservation
Domain-specific invariants hold across transformations

```toml
[verification.invariants]
profit_positive = true
network_value_positive = true
moat_strength_bounded = true
```

## Running Tests

```bash
# Run all market dynamics tests
pnpm test test/market-dynamics.test.mjs

# Test specific property
pnpm test test/market-dynamics.test.mjs -t "Determinism"

# Verify all examples load
pnpm test test/market-dynamics.test.mjs -t "Integration"
```

## Adding New Examples

To create a new market dynamics example:

1. **Create TOML file** in this directory
2. **Define structure**:
   ```toml
   [metadata]
   name = "Example Name"
   description = "..."
   domain = "..."
   equation = "A = μ(O)"

   [observations]
   # Your observations here

   [closure_operator]
   name = "operator_name"
   type = "operator_type"
   idempotent = true
   deterministic = true

   [[closure_operator.rules]]
   name = "rule_1"
   condition = "..."
   action = "..."

   [artifacts]
   # Expected outcomes

   [verification]
   method = "..."
   [verification.determinism]
   run_count = 100
   unique_outputs = 1
   determinism_score = 1.0
   ```

3. **Add test** in `test/market-dynamics.test.mjs`
4. **Verify properties** using test suite

## Implementation Details

### Closure Operators

Each example implements a closure operator **μ** with:
- **Rules**: Condition-action pairs
- **Parameters**: Configurable thresholds and coefficients
- **Idempotence**: Guaranteed by design
- **Determinism**: No randomness, timestamps optional

### Artifacts

Outputs include:
- **Primary metrics**: Numerical results
- **Execution plans**: Step-by-step actions
- **Projections**: Future state predictions
- **Confidence scores**: Uncertainty quantification

### Verification

Each example includes:
- **Method**: How to verify correctness
- **Assertions**: Boolean checks
- **Invariants**: Preserved properties
- **Determinism test**: Hash comparison across runs
- **Idempotence test**: μ(μ(O)) = μ(O) verification

## References

- [Chatman Equation Documentation](../../docs/chatman-equation.md)
- [Metcalfe's Law](https://en.wikipedia.org/wiki/Metcalfe%27s_law)
- [Market Microstructure](https://en.wikipedia.org/wiki/Market_microstructure)
- [Economic Moats](https://en.wikipedia.org/wiki/Economic_moat)

## License

MIT
