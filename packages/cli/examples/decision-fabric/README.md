# Decision Fabric CLI Examples

This directory contains example feature sets for testing the Hyperdimensional Decision Fabric CLI commands.

## Example Files

### `kgc4d-features.json`

**KGC 4D Feature Set** - From empirical validation (thesis-bigbang-80-20.tex)

- 8 features implementing 4D temporal event logging
- Pareto frontier: 5 features (62.5%) delivering 75.7% of value
- **Meets Big Bang 80/20 criteria**: H_spec = 2.85 bits (≤16 bits threshold)
- **Expected implementation time**: 2-3 hours
- **Expected correctness**: ≥99.99%

**Use cases**:
- Validating BB80/20 applicability
- Testing Pareto frontier computation
- Full integrated workflow demonstration

**Example command**:
```bash
# Analyze features
unrdf pareto examples/decision-fabric/kgc4d-features.json

# Process strategic decision
unrdf decision "Implement KGC 4D-style event logging" \
  --features examples/decision-fabric/kgc4d-features.json
```

**Expected output**:
- Methodology: Big Bang 80/20
- Pareto features: BigInt Time, Event Log, Named Graphs, Receipt, Time-Travel
- Value captured: ~75.7%
- Recommendation: Implement 5 Pareto-optimal features in 2-3 hours

---

### `fraud-detection.json`

**Fraud Detection System Feature Set**

- 5 features for real-time fraud detection
- Demonstrates feature prioritization for security systems

**Use cases**:
- Real-world feature analysis
- Pareto optimization for security systems

**Example command**:
```bash
# Analyze fraud detection features
unrdf pareto examples/decision-fabric/fraud-detection.json

# Process decision with Socratic analysis
unrdf decision "Implement real-time fraud detection" \
  --features examples/decision-fabric/fraud-detection.json
```

**Expected analysis**:
- Transaction Monitoring: High value, moderate cost (Pareto-optimal)
- Alert System: Good value/cost ratio (Pareto-optimal)
- ML Model Training: High value, high cost (may not be on frontier)
- Dashboard: Lower priority (below Pareto frontier)

---

## CLI Commands

### `decision` - Strategic Decision Processing

Process strategic decisions through the complete Hyperdimensional Decision Fabric workflow.

**Workflow**:
1. **Socratic Analysis**: Extract assumptions, detect vague claims
2. **Pareto Analysis**: Identify optimal features (if features provided)
3. **μ-Operator Validation**: 8 semantic operators for intent→outcome transformation
4. **Integrated Recommendation**: IMPLEMENT_BB8020 or PROCEED

**Examples**:
```bash
# Simple decision (no features)
unrdf decision "Implement OAuth2 authentication"

# Decision with features (recommended)
unrdf decision "Implement fraud detection" \
  --features examples/decision-fabric/fraud-detection.json

# JSON output for programmatic processing
unrdf decision "Implement feature X" --output json
```

**Exit codes**:
- 0: ACCEPTED (proceed with implementation)
- 1: REJECTED or BLOCKED (address challenges first)

---

### `pareto` - Feature Analysis

Analyze feature sets for Pareto optimality using the Big Bang 80/20 methodology.

**Capabilities**:
- Pareto frontier computation
- 80/20 rule validation
- Specification entropy analysis (H_spec)
- BB80/20 applicability determination

**Examples**:
```bash
# Basic analysis
unrdf pareto examples/decision-fabric/kgc4d-features.json

# JSON output
unrdf pareto examples/decision-fabric/fraud-detection.json --output json

# Chart data for visualization
unrdf pareto examples/decision-fabric/kgc4d-features.json --output chart
```

**Output sections**:
- Specification Entropy: H_spec (bits)
- Pareto Frontier: Count and features
- 80/20 Validation: Value percentage
- Cost Analysis: Savings and efficiency gain
- Recommendation: BB80/20 or Iterative

**Exit codes**:
- 0: BB80/20 applicable (H_spec ≤ 16 bits)
- 1: High entropy, iterative approach recommended

---

### `socratic` - Assumption Analysis

Challenge assumptions using Socratic AI reasoning to prevent groupthink.

**Detected patterns**:
- **Causal assumptions**: "X will solve Y"
- **Need assumptions**: "We need to X"
- **Optimization (vague)**: "Optimize X"
- **Absolute claims**: "Always", "Never", "All"
- **Conditionals**: "If X then Y"

**Challenge types**:
- `CLARIFICATION`: Vague/ambiguous terms need definition
- `EVIDENCE`: Unvalidated assumptions need support
- `LOGIC`: Contradictory/weak reasoning detected

**Severity levels**:
- `HIGH`: Blocks decision (unvalidated/refuted assumptions)
- `MEDIUM`: Warns but allows (weak evidence)
- `LOW`: Informational only

**Examples**:
```bash
# Detect vague optimization (BLOCKED)
unrdf socratic "We need to optimize the conversion rate"
# Output: HIGH SEVERITY - Clarification needed: By "optimize," do you mean...?

# Detect causal assumption (BLOCKED)
unrdf socratic "Adding feature X will solve problem Y"
# Output: UNVALIDATED assumption - No evidence for causal link

# Well-formed statement (PROCEED)
unrdf socratic "Implement OAuth2 authentication with JWT tokens"
# Output: PROCEED - No critical assumptions detected

# JSON output
unrdf socratic "Optimize the system" --output json
```

**Exit codes**:
- 0: PROCEED (statement is well-formed)
- 1: DO NOT PROCEED (address challenges first)

---

## Integrated Workflow Example

**Full Big Bang 80/20 workflow** (3 steps):

```bash
# Step 1: Socratic analysis - Challenge assumptions
unrdf socratic "Implement KGC 4D-style event logging"
# Output: ✅ PROCEED - Statement is well-formed

# Step 2: Pareto analysis - Identify optimal features
unrdf pareto examples/decision-fabric/kgc4d-features.json
# Output: 🚀 Big Bang 80/20 - 5 Pareto features, 2-3 hours, ≥99.99% correctness

# Step 3: Decision processing - Full workflow with recommendation
unrdf decision "Implement KGC 4D-style event logging" \
  --features examples/decision-fabric/kgc4d-features.json
# Output:
#   ✅ ACCEPTED
#   Recommendation: IMPLEMENT_BB8020
#   Expected time: 2-3 hours
#   Expected correctness: ≥99.99%
#   Next steps: Implement 5 Pareto-optimal features using...
```

---

## Feature JSON Format

Features files must follow this structure:

```json
{
  "features": [
    {
      "id": 1,
      "name": "Feature Name",
      "value": 95,
      "cost": 50,
      "description": "Optional description"
    }
  ]
}
```

**Fields**:
- `id` (number, required): Unique feature identifier
- `name` (string, required): Feature name
- `value` (number, required): Business value (0-100 scale)
- `cost` (number, required): Implementation cost (LoC or hours)
- `description` (string, optional): Feature description

---

## Performance Targets

Based on 2030 vision for Hyperdimensional Decision Fabric:

- **Throughput**: 1.17M ops/sec (approaching information-theoretic limit)
- **Latency**: 0.853μs per operator × 8 = 6.824μs total
- **Entropy Reduction**: 50 nats (intent) → ≤1 nat (outcome)
- **Correctness**: 99.997% for H_spec ≤ 16 bits
- **Speedup**: 50-100x vs traditional development
- **Idiot Index**: ~1.05 (theoretical minimum efficiency)

---

## Theoretical Foundation

The Decision Fabric implements:

1. **μ(O) Calculus**: 8 semantic operators (μ₁...μ₈) for intent validation
2. **Big Bang 80/20**: Single-pass implementation with 99.997% correctness
3. **Socratic AI**: Assumption extraction and evidence-based reasoning
4. **Hyperdimensional Information Theory**: 100K-dimension semantic spaces

**Mathematical guarantees**:
- Exactly 8 operators necessary/sufficient: n_min = ⌈(H(Λ) - H(A)) / C⌉ = 8
- Error bound: P(Error) ≤ 2^(-H_s) + (1-r)×10^(-3) + (1-c)×10^(-2)
- BB80/20 applicability: H_spec ≤ 16 bits

**References**:
- `/home/user/unrdf/docs/vision/2030-HYPERDIMENSIONAL-DECISION-FABRIC.md` - Full 2030 vision
- `packages/kgc-4d/docs/explanation/thesis-bigbang-80-20.tex` - BB80/20 methodology
- `packages/hooks/docs/thesis/knowledge-hooks-phd-thesis.tex` - μ(O) calculus
- `packages/decision-fabric/README.md` - Implementation documentation
