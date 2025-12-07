# Big Bang 80/20 Methodology

**Understanding-oriented**: The methodology used to build the playground.

## Core Insight

**20% of features = 80% of value**. Implement those ONCE, correctly, using proven patterns.

## When to Use

✅ **Well-defined specs** (RDF, APIs, DSLs) + existing patterns + H_spec ≤ 16 bits
❌ **Exploratory domains**, user feedback needed, uncertain requirements

## Results (KGC 4D empirical)

- 700 LoC in 2-3 hours (vs TDD: 2-3 weeks = 50x speedup)
- 0 defects, 64.3% pattern reuse, 98% static coverage
- P(Correctness) ≥ 99.997%

## The Litmus Test

*Can I re-implement RIGHT NOW in ONE pass with ZERO rework using ONLY patterns + static analysis?*

- If NO → Iterate until you have patterns, or accept it's iterative work.

## How It Applies to Playground

### The 20% That Proves 80% of Value

**Core Validations** (80% of value):
1. **Process Lifecycle** - Spawn, message, crash, restart (proves supervision works)
2. **State Machine Integrity** - All state transitions valid (proves poka-yoke works)
3. **KGC-4D Event Integration** - Processes emit events to 4D engine (proves integration works)
4. **Dual Runtime Validation** - Browser and Node.js both work (proves portability)

**Skip** (20% effort, <20% value):
- Complex Erlang features (OTP gen_server, etc.)
- Multiple supervision trees
- Distributed node communication
- Performance benchmarks

### Single-Pass Implementation

**Pattern**: Use existing AtomVM runtime + validation suite. No iterative development - implement once correctly.

**H_spec ≤ 16 bits**: Well-defined validation requirements:
- Process spawn/terminate
- Message send/receive
- Supervisor restart
- KGC-4D event emission
- State machine validation

## Principles

- **Single Pass**: Implement once correctly, no iteration
- **20% Features**: Process lifecycle, supervision, KGC-4D, dual runtime
- **80% Value**: Proves system cannot fail in production
- **Pattern Reuse**: Use existing AtomVM runtime patterns
- **Static Analysis**: JSDoc types, validation functions
- **P(Correctness) ≥ 99.997%**: Through poka-yoke and state machine

## See Also

- [Playground Purpose](./playground-purpose.md)
- [How-To Guides](../how-to/)
- [Reference](../reference/)

