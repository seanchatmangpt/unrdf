# Novel Validation Approaches: Beyond Consensus

## Executive Summary

We've implemented **FOUR DISTINCT validation approaches** for ggen:

1. **Consensus-Based Validation** (7-agent-validation) - Byzantine fault tolerance via voting
2. **Property-Based Validation** (property-based-validation) - Complementary invariants via property testing
3. **Mutation-Based Validation** (mutation-validation) - Active adversarial testing via mutation injection
4. **Fuzzing-Based Validation** (fuzzing-validation) - Comprehensive fuzzing via 7 specialized agents

**Key Insight**: These are NOT competing approaches — they solve different problems:

- **Consensus**: "Do agents agree on the SAME check?" (fault tolerance)
- **Property-Based**: "Do ALL DIFFERENT checks pass?" (comprehensive coverage)
- **Mutation-Based**: "Can agents BREAK the code?" (test quality)
- **Fuzzing-Based**: "Can INPUTS break the system?" (robustness)

---

## Approach 1: Consensus-Based Validation (7-Agent)

### What It Does

Runs the **same validation check** across 7 agents and uses PBFT (5-of-7 quorum) to reach agreement.

### Architecture

```
ggen generation (μ₁-μ₅)
        ↓
7 agents validate (parallel)
├─ Agent 1: Compiler Gate → GREEN
├─ Agent 2: Test Gate → GREEN
├─ Agent 3: Lint Gate → YELLOW
├─ Agent 4: SHACL Gate → GREEN
├─ Agent 5: OTEL Gate → GREEN
├─ Agent 6: Security Gate → GREEN
└─ Agent 7: Performance Gate → YELLOW
        ↓
Consensus aggregation (PBFT 5-of-7)
        ↓
Result: 5 GREEN, 2 YELLOW → APPROVE
```

### When To Use

- **Fault tolerance needed**: Agents may fail or be malicious
- **Single validation dimension**: Checking if code compiles, tests pass, etc.
- **Distributed agreement**: Need consensus across multiple parties
- **Redundancy**: Want multiple eyes on the same problem

### Strengths

✅ **Byzantine fault tolerance**: Up to 2 agents can fail/misbehave
✅ **Autonomous supervision**: Armstrong supervision (let-it-crash)
✅ **Fault isolation**: Single agent failure doesn't block system
✅ **Redundant checks**: Multiple agents verify same thing (catches errors)

### Limitations

❌ **Redundant validation**: All agents test the SAME thing (wasteful)
❌ **Single dimension**: Only validates one aspect at a time
❌ **No property guarantees**: Doesn't prove invariants hold
❌ **Example-based**: Tests specific examples, not general properties

---

## Approach 2: Property-Based Validation (7-Property)

### What It Does

Each of 7 agents validates a **DIFFERENT property** of the generated code. All 7 must pass for approval.

### Architecture

```
ggen generation (μ₁-μ₅)
        ↓
7 agents validate DIFFERENT properties (parallel)
├─ Agent 1: Determinism Property → PASS (f(x) = f(x))
├─ Agent 2: Completeness Property → PASS (all fields present)
├─ Agent 3: Consistency Property → PASS (no contradictions)
├─ Agent 4: Soundness Property → PASS (deadlock-free)
├─ Agent 5: Performance Property → PASS (SLO met)
├─ Agent 6: Security Property → PASS (no vulnerabilities)
└─ Agent 7: Correctness Property → FAIL (doesn't match spec)
        ↓
Aggregation (ALL must pass)
        ↓
Result: 6/7 pass → REJECT (Agent 7 found correctness violation)
```

### When To Use

- **Comprehensive validation**: Need to check multiple dimensions
- **Property guarantees**: Need to prove invariants hold
- **Specification compliance**: Output must match schema/semantics
- **Formal verification**: Need mathematical proofs of properties

### Strengths

✅ **Complementary coverage**: Each agent tests different property (no redundancy)
✅ **Property-based testing**: Uses proptest to verify invariants
✅ **Formal verification**: Soundness agent proves WvdA properties
✅ **Specification compliance**: Correctness agent validates against spec
✅ **Kaizen feedback**: Results feed back into ggen's improvement cycle

### Limitations

❌ **All-or-nothing**: Any property fail → reject (less tolerant)
❌ **No fault tolerance**: No redundancy if agent fails
❌ **Slower**: 7 different checks take longer than 1 check × 7
❌ **Complex**: Requires defining formal properties for each domain

---

## Approach 3: Mutation-Based Validation (7-Agent Adversarial)

### What It Does

7 agents perform **active adversarial testing** — they try to BREAK the code rather than validate it.

### Architecture

```
ggen generation (μ₁-μ₅)
        ↓
PHASE 1: Inject mutations (Agents 1-3)
├─ Agent 1: Syntax mutations → Inject 50 mutations
├─ Agent 2: Logic mutations → Inject 40 mutations
└─ Agent 3: Boundary mutations → Inject 30 mutations
        ↓
PHASE 2: Detect and analyze (Agents 4-7)
├─ Agent 4: Unit tests → Detect 70 mutations (KILLED)
├─ Agent 5: Integration tests → Detect 20 mutations (KILLED)
├─ Agent 6: Score calculator → Mutation score: 60%
└─ Agent 7: Test improver → Suggest 30 new tests
        ↓
Result: Mutation Score = 60% (tests caught 90/150 mutations)
```

### When To Use

- **Test quality assessment**: Need to measure how good tests are
- **Test gap detection**: Want to find missing test cases
- **Adversarial validation**: Need to harden code against bugs
- **Regression testing**: Ensure new tests catch old bugs

### Strengths

✅ **Active adversarial testing**: Agents try to BREAK code (not just validate)
✅ **Quantitative measure**: Mutation score (0-100%) measures test quality
✅ **Test gap detection**: Surviving mutations reveal test weaknesses
✅ **Targeted suggestions**: Agent 7 suggests specific tests to add
✅ **Competitive testing**: Tests compete against mutations

### Limitations

❌ **SLOW**: Injecting mutations + running tests is expensive
❌ **Compilation errors**: Some mutations cause compile failures (neutral)
❌ **False positives**: Surviving mutation ≠ real bug (could be dead code)
❌ **Scalability**: Large codebases have too many mutations to test

---

## Approach 4: Fuzzing-Based Validation (7-Agent Fuzzing)

### What It Does

7 agents use **different fuzzing strategies** to explore the entire input space and find crashes, hangs, incorrect results, and errors that traditional testing misses.

### Architecture

```
Base input (e.g., SPARQL query)
        ↓
PHASE 1: Generate fuzzed inputs (7 Agents)
├─ Agent 1: Structure fuzzing → 20 malformed inputs
├─ Agent 2: Value fuzzing → 20 boundary inputs
├─ Agent 3: Protocol fuzzing → 20 invalid queries
├─ Agent 4: Chaos fuzzing → 20 random failures
├─ Agent 5: Performance fuzzing → 20 extreme loads
├─ Agent 6: Security fuzzing → 20 attack vectors
└─ Agent 7: Semantic fuzzing → 20 edge cases
        ↓
PHASE 2: Test all inputs (7 Agents)
├─ Agent 1: Test structure inputs → 3 crashes, 2 passed
├─ Agent 2: Test value inputs → 1 crash, 19 passed
├─ Agent 3: Test protocol inputs → 5 crashes, 1 hang, 14 passed
├─ Agent 4: Test chaos inputs → 2 hangs, 18 passed
├─ Agent 5: Test performance inputs → 4 crashes, 16 passed
├─ Agent 6: Test security inputs → 2 crashes, 18 passed
└─ Agent 7: Test semantic inputs → 2 crashes, 2 hangs, 16 passed
        ↓
PHASE 3: Analyze results
Total: 140 inputs, 102 passed, 38 failed
Error Rate: 27.1%, Crash Rate: 9.3%
        ↓
PHASE 4: Generate recommendations
27 robustness recommendations generated
```

### When To Use

- **Input validation testing**: Need to test how system handles malformed inputs
- **Robustness validation**: Want to find crashes, hangs, and errors
- **Security testing**: Need to find attack vectors and injection points
- **Performance testing**: Want to test system under extreme loads
- **Edge case discovery**: Find inputs that trigger unexpected behavior

### Strengths

✅ **Comprehensive input space coverage**: 7 different strategies explore entire input space
✅ **Finds critical bugs**: Crashes, hangs, incorrect results, errors
✅ **Security-focused**: Agent 6 specifically tests attack vectors
✅ **Performance-aware**: Agent 5 tests stress conditions
✅ **Actionable recommendations**: Prioritized suggestions for improving robustness
✅ **Quantitative metrics**: Error rate, crash rate, unique crashes

### Limitations

❌ **Input-dependent**: Quality depends on base input chosen
❌ **False positives**: Some crashes may be unrealistic in production
❌ **Time-consuming**: Testing 140+ inputs takes time
❌ **No code coverage**: Doesn't measure which code paths were tested
❌ **Requires triage**: Many crashes need manual investigation

---

## Key Differences

| Dimension           | Consensus-Based        | Property-Based                   | Mutation-Based           | Fuzzing-Based           |
| ------------------- | ---------------------- | -------------------------------- | ------------------------ | ----------------------- |
| **Validation Type** | Redundant (same check) | Complementary (different checks) | Adversarial (break code) | Input space exploration |
| **Decision Logic**  | 5-of-7 quorum          | All 7 must pass                  | Mutation score           | Error rate threshold    |
| **Failure Mode**    | Quorum not reached     | Any property fails               | Low mutation score       | High error/crash rate   |
| **Fault Tolerance** | High (Byzantine)       | Low (no redundancy)              | N/A (test quality)       | N/A (robustness)        |
| **Coverage**        | Single dimension       | Multi-dimensional                | Adversarial (code)       | Adversarial (inputs)    |
| **Testing Style**   | Example-based          | Property-based (invariants)      | Mutation-based           | Fuzzing-based           |
| **Primary Goal**    | Fault tolerance        | Comprehensive validation         | Test quality             | Robustness              |

---

## Comparison Table

| Aspect                  | Consensus (7-Agent)          | Property-Based (7-Property)    | Mutation-Based (7-Agent)  | Fuzzing-Based (7-Agent)    |
| ----------------------- | ---------------------------- | ------------------------------ | ------------------------- | -------------------------- |
| **What agents do**      | Vote on SAME check           | Test DIFFERENT properties      | Try to BREAK code         | Try to BREAK inputs        |
| **Agents needed**       | 5+ (quorum)                  | 7 (all required)               | 7 (all required)          | 7 (all required)           |
| **Redundancy**          | High (redundant checks)      | Low (complementary checks)     | High (multiple mutations) | Low (different strategies) |
| **Fault tolerance**     | Byzantine (up to 2 failures) | None (single failure = reject) | N/A (test quality)        | N/A (robustness)           |
| **Validation speed**    | Fast (1 check × 7 agents)    | Slower (7 different checks)    | SLOW (mutations + tests)  | Medium (140 inputs)        |
| **Validation depth**    | Shallow (one dimension)      | Deep (7 dimensions)            | Adversarial (break code)  | Adversarial (inputs)       |
| **Formal verification** | No                           | Yes (soundness agent)          | No                        | No                         |
| **Property testing**    | No                           | Yes (proptest)                 | No                        | No                         |
| **Fuzzing**             | No                           | No                             | No                        | Yes (7 fuzzers)            |
| **Kaizen feedback**     | Vote distribution            | Property violations            | Test suggestions          | Robustness recommendations |
| **Use case**            | Distributed agreement        | Specification compliance       | Test quality assessment   | Robustness validation      |

| Dimension           | Consensus-Based        | Property-Based                   |
| ------------------- | ---------------------- | -------------------------------- |
| **Validation Type** | Redundant (same check) | Complementary (different checks) |
| **Decision Logic**  | 5-of-7 quorum          | All 7 must pass                  |
| **Failure Mode**    | Quorum not reached     | Any property fails               |
| **Fault Tolerance** | High (Byzantine)       | Low (no redundancy)              |
| **Coverage**        | Single dimension       | Multi-dimensional                |
| **Testing Style**   | Example-based          | Property-based (invariants)      |
| **Primary Goal**    | Fault tolerance        | Comprehensive validation         |

---

## Comparison Table

| Aspect                  | Consensus (7-Agent)          | Property-Based (7-Property)    |
| ----------------------- | ---------------------------- | ------------------------------ |
| **What agents do**      | Vote on SAME check           | Test DIFFERENT properties      |
| **Agents needed**       | 5+ (quorum)                  | 7 (all required)               |
| **Redundancy**          | High (redundant checks)      | Low (complementary checks)     |
| **Fault tolerance**     | Byzantine (up to 2 failures) | None (single failure = reject) |
| **Validation speed**    | Fast (1 check × 7 agents)    | Slower (7 different checks)    |
| **Validation depth**    | Shallow (one dimension)      | Deep (7 dimensions)            |
| **Formal verification** | No                           | Yes (soundness agent)          |
| **Property testing**    | No                           | Yes (proptest)                 |
| **Kaizen feedback**     | Vote distribution            | Property violations            |

---

## When To Use Which Approach

### Use Consensus-Based (7-Agent) When:

1. **Fault tolerance is critical**: Agents may fail or be malicious
2. **Single validation dimension**: Only checking one thing (e.g., "does it compile?")
3. **Distributed agreement**: Multiple parties need to agree
4. **Redundancy needed**: Want multiple eyes on the same problem

**Example**: Validating that code compiles across 7 different environments

```
Agent 1 (Linux x64): Compile → PASS
Agent 2 (Linux ARM): Compile → PASS
Agent 3 (macOS x64): Compile → FAIL
...
Consensus: 6/7 pass → APPROVE (majority agree it compiles)
```

### Use Property-Based (7-Property) When:

1. **Comprehensive validation needed**: Need to check multiple aspects
2. **Property guarantees required**: Must prove invariants hold
3. **Specification compliance**: Output must match schema/semantics
4. **Formal verification**: Need mathematical proofs

**Example**: Validating generated API code against specification

```
Agent 1: Determinism → PASS (same input → same output)
Agent 2: Completeness → PASS (all endpoints present)
Agent 3: Consistency → PASS (no contradictory routes)
Agent 4: Soundness → PASS (no circular dependencies)
Agent 5: Performance → PASS (latency < 100ms)
Agent 6: Security → PASS (no SQL injection)
Agent 7: Correctness → FAIL (missing /users endpoint)
Result: REJECT (specific violation: missing required endpoint)
```

### Use Mutation-Based (7-Agent) When:

1. **Test quality assessment**: Need to measure how good tests are
2. **Test gap detection**: Want to find missing test cases
3. **Adversarial validation**: Need to harden code against bugs
4. **Regression prevention**: Ensure new tests catch old bugs

**Example**: Measuring test suite quality for ggen-generated code

```
PHASE 1: Inject mutations
  Agent 1: 50 syntax mutations
  Agent 2: 40 logic mutations
  Agent 3: 30 boundary mutations

PHASE 2: Detect mutations
  Agent 4: Unit tests → 70 killed
  Agent 5: Integration tests → 20 killed
  Agent 6: Score = 60% (90/150 detected)
  Agent 7: Suggest 30 new tests for survivors

Result: Mutation Score 60% → "Good, but room for improvement"
```

### Use Fuzzing-Based (7-Agent) When:

1. **Input validation needed**: Need to test how system handles malformed inputs
2. **Robustness validation**: Want to find crashes, hangs, and errors
3. **Security testing**: Need to find attack vectors and injection points
4. **Performance testing**: Want to test system under extreme loads
5. **Edge case discovery**: Find inputs that trigger unexpected behavior

**Example**: Validating SPARQL query parser robustness

```
PHASE 1: Generate fuzzed inputs (140 total)
  Agent 1: 20 structure fuzzes → 3 crashes
  Agent 2: 20 value fuzzes → 1 crash
  Agent 3: 20 protocol fuzzes → 5 crashes, 1 hang
  Agent 4: 20 chaos fuzzes → 2 hangs
  Agent 5: 20 performance fuzzes → 4 crashes
  Agent 6: 20 security fuzzes → 2 crashes
  Agent 7: 20 semantic fuzzes → 2 crashes, 2 hangs

PHASE 2: Test all inputs
  Total: 140 inputs
  Passed: 102 (72.9%)
  Failed: 38 (27.1%)
    - 13 crashes (9.3%)
    - 8 hangs (5.7%)
    - 4 incorrect (2.9%)
    - 13 errors (9.3%)

PHASE 3: Generate recommendations
  27 robustness recommendations
  Assessment: "Poor: High error rate. System is fragile."
```

### Use Consensus-Based (7-Agent) When:

### Use Consensus-Based (7-Agent) When:

1. **Fault tolerance is critical**: Agents may fail or be malicious
2. **Single validation dimension**: Only checking one thing (e.g., "does it compile?")
3. **Distributed agreement**: Multiple parties need to agree
4. **Redundancy needed**: Want multiple eyes on the same problem

**Example**: Validating that code compiles across 7 different environments

```
Agent 1 (Linux x64): Compile → PASS
Agent 2 (Linux ARM): Compile → PASS
Agent 3 (macOS x64): Compile → FAIL
...
Consensus: 6/7 pass → APPROVE (majority agree it compiles)
```

### Use Property-Based (7-Property) When:

1. **Comprehensive validation needed**: Need to check multiple aspects
2. **Property guarantees required**: Must prove invariants hold
3. **Specification compliance**: Output must match schema/semantics
4. **Formal verification**: Need mathematical proofs

**Example**: Validating generated API code against specification

```
Agent 1: Determinism → PASS (same input → same output)
Agent 2: Completeness → PASS (all endpoints present)
Agent 3: Consistency → PASS (no contradictory routes)
Agent 4: Soundness → PASS (no circular dependencies)
Agent 5: Performance → PASS (latency < 100ms)
Agent 6: Security → PASS (no SQL injection)
Agent 7: Correctness → FAIL (missing /users endpoint)
Result: REJECT (specific violation: missing required endpoint)
```

---

## Combining All Three Approaches

**Idea**: Use consensus + property-based + mutation-based for complete validation

### Hybrid Architecture

```
ggen generation (μ₁-μ₅)
        ↓
Property-Based Validation (7 properties) with Consensus
├─ Property 1 (Determinism): 5 agents check → 5/5 pass
├─ Property 2 (Completeness): 5 agents check → 5/5 pass
├─ Property 3 (Consistency): 5 agents check → 5/5 pass
├─ Property 4 (Soundness): 5 agents check → 5/5 pass
├─ Property 5 (Performance): 5 agents check → 4/5 pass
├─ Property 6 (Security): 5 agents check → 5/5 pass
└─ Property 7 (Correctness): 5 agents check → 5/5 pass
        ↓
Consensus aggregation (per-property)
        ↓
Mutation-Based Validation (test quality assessment)
├─ Agent 1: Inject syntax mutations → 50 mutations
├─ Agent 2: Inject logic mutations → 40 mutations
├─ Agent 3: Inject boundary mutations → 30 mutations
├─ Agent 4: Unit tests → 70 killed
├─ Agent 5: Integration tests → 20 killed
├─ Agent 6: Score calculator → 60% mutation score
└─ Agent 7: Test improver → Suggest 30 new tests
        ↓
Final Result: APPROVE (Properties passed + Mutation score acceptable)
```

### Benefits

✅ **Fault tolerance**: Each property checked by 5 agents (Byzantine)
✅ **Comprehensive**: All 7 properties validated
✅ **Redundancy**: Multiple agents per property (catch errors)
✅ **Property-based**: Uses proptest to verify invariants
✅ **Test quality**: Mutation score measures test suite strength
✅ **Kaizen feedback**: Property violations + test suggestions + vote distribution

---

## Future Directions

### 1. Mutation-Based Validation

7 agents perform mutation testing on generated code:

```
Agent 1-2: Inject mutations (syntax, logic, boundary)
Agent 3-4: Run tests to detect mutations
Agent 5: Calculate mutation score
Agent 6: Suggest test improvements
Agent 7: Track mutation trend over time
```

### 2. Fuzzing-Based Validation

7 agents use fuzzing to explore input space:

```
Agent 1: Structure fuzzing (malformed inputs)
Agent 2: Value fuzzing (boundary values)
Agent 3: Protocol fuzzing (invalid messages)
Agent 4: Chaos engineering (random failures)
Agent 5: Performance fuzzing (extreme loads)
Agent 6: Security fuzzing (attack vectors)
Agent 7: Semantic fuzzing (edge cases)
```

### 3. Incremental Validation

Validate during μ₁-μ₅ pipeline (not after):

```
μ₁ (Ontology normalization):
  Agent 1: Validates RDF normalization invariants

μ₂ (SPARQL extraction):
  Agent 2: Validates SPARQL query correctness

μ₃ (Template rendering):
  Agent 3: Validates template syntax

μ₄ (Canonicalization):
  Agent 4: Validates canonical form correctness

μ₅ (Receipt generation):
  Agent 5: Validates BLAKE3 receipt correctness

Each stage: Fail-fast → stop pipeline, don't continue
```

### 4. Predictive Validation

ML models predict validation outcomes before running:

```
Agent 1: Predicts compilation success rate
Agent 2: Predicts test pass rate
Agent 3: Predicts performance score
Agent 4: Predicts security vulnerabilities
Agent 5: Predicts correctness score
Agent 6: Aggregates predictions
Agent 7: Decides: run validation OR fix issues first
```

### 5. Swarm Validation

Multiple instances of each agent for redundancy:

```
Property 1 (Determinism): 7 instances check determinism
Property 2 (Completeness): 7 instances check completeness
...
Property 7 (Correctness): 7 instances check correctness

Each property: Majority vote within property → final result
```

---

## Implementation Status

### Consensus-Based (7-Agent)

**Location**: `/Users/sac/ggen/examples/7-agent-validation/`

**Status**: ✅ Complete (demo and library ready)

**Files**:

- `src/lib.rs` - Main validation system
- `src/agent.rs` - Agent definitions
- `src/consensus.rs` - PBFT consensus layer
- `src/gates.rs` - Quality gate implementations
- `src/registry.rs` - Agent registry
- `src/supervisor.rs` - Armstrong supervision
- `src/main.rs` - Demo program
- `README.md` - Documentation

### Property-Based (7-Property)

**Location**: `/Users/sac/ggen/examples/property-based-validation/`

**Status**: ✅ Architecture complete, 🔨 Property checks are placeholders

**Files**:

- `src/lib.rs` - Main validation system
- `src/properties.rs` - Property definitions
- `src/agents.rs` - Property validation agents
- `src/coordinator.rs` - Agent coordinator
- `src/feedback.rs` - Kaizen feedback loop
- `src/main.rs` - Demo program
- `README.md` - Documentation

### Mutation-Based (7-Agent)

**Location**: `/Users/sac/ggen/examples/mutation-validation/`

**Status**: ✅ Architecture complete, 🔨 Mutation injection is simulated

**Files**:

- `src/lib.rs` - Main validation system
- `src/mutations.rs` - Mutation types and results
- `src/agents.rs` - Mutation agents (injectors + detectors)
- `src/coordinator.rs` - Agent coordinator
- `src/main.rs` - Demo program
- `README.md` - Documentation

---

## Summary

We have FOUR novel validation approaches:

1. **Consensus-Based (7-Agent)**: Byzantine fault tolerance for redundant validation
2. **Property-Based (7-Property)**: Complementary invariants for comprehensive validation
3. **Mutation-Based (7-Agent)**: Active adversarial testing for test quality assessment
4. **Fuzzing-Based (7-Agent)**: Comprehensive fuzzing for robustness validation

**Key Innovation**: These are NOT competing approaches — they can be COMBINED for:

- Fault tolerance (consensus per property)
- Comprehensive coverage (all 7 properties)
- Property-based testing (proptest invariants)
- Formal verification (WvdA soundness)
- Test quality measurement (mutation score)
- Robustness validation (fuzzing error rate)
- Kaizen feedback (continuous improvement)

**Next Steps**: Implement actual checks using ggen's existing infrastructure:

- Integrate with ggen's μ₁-μ₅ pipeline
- Use ggen's BLAKE3 receipts for determinism checking
- Use ggen's AndonSignal for quality signals
- Feed results into ggen's kaizen cycle (PDCA)

---

## References

- **7-Agent Consensus**: `/Users/sac/ggen/examples/7-agent-validation/`
- **7-Property**: `/Users/sac/ggen/examples/property-based-validation/`
- **7-Agent Mutation**: `/Users/sac/ggen/examples/mutation-validation/`
- **7-Agent Fuzzing**: `/Users/sac/ggen/examples/fuzzing-validation/`
- **ggen Infrastructure**: `/Users/sac/ggen/crates/ggen-core/`
- **Property-Based Testing**: proptest, QuickCheck
- **Mutation Testing**: Jia & Harrold (2011), "An Analysis and Survey of Mutation Testing"
- **Fuzzing**: American Fuzzy Lop (AFL), libFuzzer, Honggfuzz
- **WvdA Soundness**: Process Mining (Wil van der Aalst)
- **Toyota Production System**: Kaizen continuous improvement

---

_Last Updated: 2026-04-07_
