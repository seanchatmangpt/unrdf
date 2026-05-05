# Type Safety & Invalid State Prevention EPICs

**Domain**: Safety & Poka-Yoke Engineering  
**Objective**: Close 12 vulnerability windows, eliminate top 10 footguns  
**Scope**: Make invalid states impossible through compile-time + runtime enforcement  
**Reference**: `/home/user/unrdf/docs/poka-yoke-v6-analysis/POKA-YOKE-ANALYSIS.md`

---

## EPIC-SAFE-001: Branded Type System for ID Confusion Prevention

**Goal**: Eliminate all ID confusion bugs by making ReceiptId, EventId, UniverseId distinct types at compile-time.

**Value**: Prevents #3 footgun (passing wrong ID type) which causes 15-20% of runtime errors in current v5. Estimated 90% reduction in "wrong parameter type" bugs.

**Scope**: 
- All 12 v6 packages that use UUIDs
- 34 public APIs accepting ID parameters
- Zod schemas for ReceiptId, EventId, UniverseId, TaskId, WorkflowId

### Acceptance Criteria
- [ ] All UUID types are branded with Zod `.brand()` method
- [ ] Zero runtime errors from ID confusion in integration tests (currently 12 failing tests)
- [ ] TypeScript/JSDoc enforces distinct types (IDE shows type errors)
- [ ] Migration guide documents branded type usage for v5 → v6
- [ ] ESLint rule detects raw string usage where branded type expected
- [ ] Performance: Zero overhead (branded types are compile-time only)

### Key Stories
1. **Define Branded ID Schemas** - Create Zod schemas for 5 ID types (Receipt, Event, Universe, Task, Workflow)
2. **Migrate Core APIs** - Update `@unrdf/governance`, `@unrdf/store`, `@unrdf/workflows` to use branded types
3. **JSDoc Type Definitions** - Generate JSDoc `@typedef` for all branded types
4. **ESLint Integration** - Custom rule `@unrdf/no-raw-uuid` to enforce branded types
5. **Migration Tooling** - Script to auto-convert v5 code to v6 branded types
6. **Documentation** - Tutorial "Working with Branded IDs in v6"

### Dependencies
- Blocked by: None (foundation work)
- Blocks: EPIC-SAFE-003 (Builder Pattern needs branded types), EPIC-SAFE-007 (Phantom types use branded IDs)

### Estimated Effort
- T-shirt size: M
- Weeks: 1-2
- Engineer: 1 senior developer
- Testing: 40 hours (comprehensive ID confusion test suite)

---

## EPIC-SAFE-002: State Machine Enforcement for Lifecycle Management

**Goal**: Make invalid state transitions impossible by encoding state machines in the type system and runtime guards.

**Value**: Eliminates #4 footgun (calling appendEvent on frozen universe) and closes 2 HIGH severity vulnerabilities (concurrent freeze, race on state transitions). Reduces production incidents by 60%.

**Scope**:
- Universe lifecycle (MUTABLE → FROZEN → SEALED)
- YAWL task lifecycle (7 states: CREATED → ENABLED → EXECUTING → COMPLETED/CANCELLED/FAILED)
- Receipt chain state (GENESIS → LINKED → VERIFIED)
- Delta application state (PROPOSED → VALIDATED → APPLIED/REJECTED)

### Acceptance Criteria
- [ ] All 4 state machines implemented with explicit guards
- [ ] Type-level enforcement (FrozenUniverse has no `appendEvent` method in JSDoc)
- [ ] 100% test coverage for invalid transitions (currently 85%)
- [ ] State transition visualization in docs (Mermaid diagrams)
- [ ] Runtime guards throw clear errors: "Cannot X: Universe is FROZEN. Use Y instead."
- [ ] Zero invalid state transition errors in production (OTEL span `state.invalid_transition` = 0)
- [ ] Performance: State checks <1ms (currently 0.5ms)

### Key Stories
1. **Universe State Machine** - Harden existing MUTABLE→FROZEN→SEALED with type-level guards
2. **YAWL Lifecycle State Machine** - Refactor 7-state model with explicit transition guards
3. **Receipt Chain State Machine** - New state machine for GENESIS→LINKED→VERIFIED
4. **Delta State Machine** - New state machine for PROPOSED→VALIDATED→APPLIED/REJECTED
5. **Type-Level Guards (JSDoc)** - Phantom types for MutableUniverse, FrozenUniverse, SealedUniverse
6. **Runtime Guard Library** - Centralized `state-machine-guards.mjs` (extract from current guards.mjs)
7. **Visualization** - Mermaid state diagrams in docs for all 4 state machines
8. **Test Suite** - Comprehensive invalid transition test matrix (16 tests per state machine = 64 tests)

### Dependencies
- Blocked by: None
- Blocks: EPIC-SAFE-007 (Type-level state guards extend this)

### Estimated Effort
- T-shirt size: L
- Weeks: 2-3
- Engineer: 1 senior + 1 mid-level developer
- Testing: 60 hours (state machine test matrix)

---

## EPIC-SAFE-003: Builder Pattern for Constrained Construction

**Goal**: Make it impossible to construct invalid receipts, deltas, or workflows by requiring builder pattern for all complex objects.

**Value**: Eliminates #1 footgun (mutating receipts after creation) and #8 footgun (missing required fields). Closes 3 HIGH severity vulnerabilities (receipt mutation, delta mutation, workflow mutation).

**Scope**:
- ReceiptBuilder (9 required fields)
- DeltaBuilder (operations, source, timestamp)
- WorkflowBuilder (tasks, flows, regions)
- QueryBuilder (SPARQL construction safety)

### Acceptance Criteria
- [ ] Private constructors for Receipt, Delta, Workflow (can't use `new Receipt()`)
- [ ] Builder classes with fluent API (method chaining)
- [ ] `.build()` validates with Zod schema before returning
- [ ] All returned objects are `Object.freeze()`'d
- [ ] Zero "missing required field" errors in production (currently 8% of errors)
- [ ] Zero "mutated after creation" errors (currently 5% of errors)
- [ ] Developer experience: IDE autocomplete guides through required fields
- [ ] Performance: Builder overhead <0.1ms (currently 0.017ms for receipt creation)

### Key Stories
1. **ReceiptBuilder** - 9 required fields (id, type, t_ns, timestamp_iso, previousHash, payloadHash, receiptHash, payload, signature)
2. **DeltaBuilder** - Fluent API for addTriple(), deleteTriple(), build()
3. **WorkflowBuilder** - Task/Flow/Region builder with validation
4. **QueryBuilder** - Safe SPARQL construction (prevents injection)
5. **Immutability Enforcement** - Deep Object.freeze() for nested objects
6. **TypeScript Definitions** - Generate `.d.ts` for all builders
7. **Migration Guide** - "Migrating from v5 Constructors to v6 Builders"
8. **Test Suite** - 50+ tests for missing fields, invalid values, mutation attempts

### Dependencies
- Blocked by: EPIC-SAFE-001 (needs branded types for IDs)
- Blocks: EPIC-SAFE-004 (immutability builds on builder pattern)

### Estimated Effort
- T-shirt size: L
- Weeks: 2-3
- Engineer: 1 senior + 1 mid-level developer
- Testing: 50 hours (comprehensive builder test suite)

---

## EPIC-SAFE-004: Immutability-by-Default for Data Integrity

**Goal**: Guarantee that all public data structures are immutable using `Object.freeze()` + `Readonly<T>` types.

**Value**: Eliminates #1 footgun (mutating receipts) and closes all 3 HIGH severity state leak vulnerabilities. Prevents 100% of post-creation mutations.

**Scope**:
- All receipts (freeze, mutation, execution)
- All deltas (operations array deep-frozen)
- All query results (SPARQL results immutable)
- Internal state objects (workflow._tasks becomes private)

### Acceptance Criteria
- [ ] 100% of public returns are `Object.freeze()`'d (currently 71 calls, need 200+)
- [ ] Deep freeze for nested objects (operations arrays, payload objects)
- [ ] JSDoc `@readonly` on all public properties
- [ ] ESLint rule `@unrdf/no-mutation` detects assignment to frozen objects
- [ ] Zero mutation errors in production (OTEL span `data.mutation_error` = 0)
- [ ] Performance: Deep freeze overhead <0.5ms for receipt (currently 0.1ms)
- [ ] Defensive copies eliminated (immutability makes copies unnecessary)

### Key Stories
1. **Deep Freeze Utility** - Recursive Object.freeze() for nested structures
2. **Receipt Immutability** - Freeze all 3 receipt types (freeze, mutation, execution)
3. **Delta Immutability** - Deep freeze operations array, payload
4. **Query Result Immutability** - Freeze SPARQL result bindings
5. **Private State Encapsulation** - Make workflow._tasks, universe._state private
6. **JSDoc Readonly Types** - `@typedef {Readonly<Receipt>}` for all public types
7. **ESLint Plugin** - Detect mutations of frozen objects at lint time
8. **Test Suite** - 30+ tests for mutation attempts (all should throw)

### Dependencies
- Blocked by: EPIC-SAFE-003 (builder pattern produces frozen objects)
- Blocks: None

### Estimated Effort
- T-shirt size: M
- Weeks: 1-2
- Engineer: 1 mid-level developer
- Testing: 30 hours (mutation test suite)

---

## EPIC-SAFE-005: Capability-Based Security Model

**Goal**: Replace string-based actor fields with cryptographic capability proofs for all operations requiring authorization.

**Value**: Closes MEDIUM severity permission bypass vulnerability. Prevents unauthorized operations at type level (no capability proof = no execution). Eliminates #6 footgun (permission checks).

**Scope**:
- YAWL task operations (enableTask, startTask, completeTask)
- Delta admission (admit requires capability)
- Workflow modification (lock/unlock requires ownership proof)
- Receipt signing (signature = Ed25519 proof of authorship)

### Acceptance Criteria
- [ ] All protected operations require `CapabilityProof` parameter
- [ ] Capability = { actor: string, resource: string, action: string, signature: string, expiry: bigint }
- [ ] Ed25519 signature verification on all capability proofs
- [ ] Zero unauthorized operation errors in production
- [ ] Performance: Signature verification <2ms (currently 0.5ms for receipt signing)
- [ ] Backward compatibility: v5 string actors migrated to v6 capabilities
- [ ] Documentation: "Understanding Capabilities in v6"

### Key Stories
1. **Capability Schema** - Zod schema for CapabilityProof with Ed25519 signature
2. **Capability Verification** - Centralized `verifyCapability(proof, resource, action)` function
3. **YAWL Integration** - enableTask, startTask, completeTask require capability
4. **Delta Admission Guard** - `admit(delta, capability)` enforces permission
5. **Workflow Lock Guard** - lock/unlock require ownership capability
6. **Key Management** - KeyPair generation, storage, rotation
7. **Migration Tooling** - Convert v5 string actors to v6 capability proofs
8. **Test Suite** - 40+ tests for unauthorized access attempts (all should throw)

### Dependencies
- Blocked by: EPIC-SAFE-001 (branded types for resource IDs)
- Blocks: None

### Estimated Effort
- T-shirt size: XL
- Weeks: 3-4
- Engineer: 1 senior + 1 mid-level developer
- Testing: 70 hours (security test suite + fuzzing)

---

## EPIC-SAFE-006: Linear Types & Race Condition Prevention

**Goal**: Prevent race conditions on state transitions using ownership semantics (universe consumed on freeze, can't use twice).

**Value**: Eliminates #6 footgun (concurrent freeze) and closes MEDIUM severity race condition vulnerabilities. Reduces race-related bugs by 95%.

**Scope**:
- Universe freeze (consume on freeze, return new FrozenUniverse)
- Workflow lock (consume on lock, return LockedWorkflow)
- Delta application (consume on apply, return receipt)
- Receipt chain (consume on link, return new LinkedReceipt)

### Acceptance Criteria
- [ ] Resource tracking system (WeakMap of consumed resources)
- [ ] Consume-on-use semantics for all state transitions
- [ ] Type-level enforcement (consumed resources have no methods)
- [ ] Zero concurrent modification errors in production
- [ ] Performance: Ownership tracking overhead <0.5ms
- [ ] Mutex/CAS for critical sections (freeze, seal, lock)
- [ ] Test suite: 20+ concurrent operation tests (all should serialize or fail-fast)

### Key Stories
1. **Resource Tracker** - WeakMap-based ownership tracking system
2. **Universe Ownership** - Consume MutableUniverse on freeze, return FrozenUniverse
3. **Workflow Ownership** - Consume unlocked workflow on lock
4. **Delta Ownership** - Consume ProposedDelta on validate, ValidatedDelta on apply
5. **Mutex Implementation** - Compare-and-swap for state transitions
6. **Type-Level Ownership** - JSDoc phantom types for consumed resources
7. **Developer Guide** - "Understanding Ownership in v6"
8. **Concurrency Test Suite** - Stress tests with 100+ concurrent operations

### Dependencies
- Blocked by: EPIC-SAFE-002 (state machines), EPIC-SAFE-007 (phantom types)
- Blocks: None

### Estimated Effort
- T-shirt size: XL
- Weeks: 3-5
- Engineer: 1 senior developer (requires deep system knowledge)
- Testing: 80 hours (concurrency testing is complex)

---

## EPIC-SAFE-007: Type-Level State Guards (Phantom Types)

**Goal**: Encode state machine invariants in the type system so invalid operations are type errors, not runtime errors.

**Value**: Makes 90% of current runtime guards unnecessary. Eliminates #4 footgun (calling methods on wrong state) at compile time. Ultimate poka-yoke: "If it compiles, it's valid."

**Scope**:
- Universe state (MutableUniverse, FrozenUniverse, SealedUniverse have different methods)
- Task state (CreatedTask, EnabledTask, ExecutingTask, CompletedTask)
- Receipt state (GenesisReceipt, LinkedReceipt, VerifiedReceipt)
- Delta state (ProposedDelta, ValidatedDelta, AppliedDelta, RejectedDelta)

### Acceptance Criteria
- [ ] JSDoc phantom types for all state machines (4 total)
- [ ] Methods only exist on valid states (e.g., no `appendEvent` on FrozenUniverse)
- [ ] TypeScript `.d.ts` generation for IDE support
- [ ] Zero "operation not allowed in this state" runtime errors in test suite
- [ ] Developer experience: IDE shows only valid methods for current state
- [ ] Performance: Zero overhead (phantom types are compile-time only)
- [ ] Migration: Auto-detect state at runtime, return correct type

### Key Stories
1. **Phantom Type Library** - Generic phantom type utilities for state branding
2. **Universe Phantom Types** - MutableUniverse, FrozenUniverse, SealedUniverse interfaces
3. **Task Phantom Types** - 7 task states with distinct method sets
4. **Receipt Phantom Types** - GenesisReceipt, LinkedReceipt, VerifiedReceipt
5. **Delta Phantom Types** - 4 delta states (Proposed, Validated, Applied, Rejected)
6. **Runtime State Detection** - `assertState(obj, 'FROZEN')` validates and narrows type
7. **TypeScript Definitions** - Generate `.d.ts` with phantom types for IDE
8. **Tutorial** - "Type-Safe State Machines in v6"

### Dependencies
- Blocked by: EPIC-SAFE-002 (state machines must be defined first)
- Blocks: EPIC-SAFE-006 (linear types extend phantom types)

### Estimated Effort
- T-shirt size: L
- Weeks: 2-3
- Engineer: 1 senior developer (advanced type system knowledge required)
- Testing: 50 hours (type-level test suite)

---

## Cross-Epic Metrics

### Coverage Goals
| Vulnerability Window | Severity | Addressed By |
|---------------------|----------|--------------|
| Receipt mutation after creation | HIGH | EPIC-SAFE-003, EPIC-SAFE-004 |
| Workflow task map exposed | HIGH | EPIC-SAFE-004 |
| Delta operations array mutable | HIGH | EPIC-SAFE-003, EPIC-SAFE-004 |
| BigInt vs String confusion | MEDIUM | EPIC-SAFE-001 (branded types validate) |
| Concurrent freeze race condition | MEDIUM | EPIC-SAFE-006 |
| Permission bypass | MEDIUM | EPIC-SAFE-005 |
| Race on state transitions | MEDIUM | EPIC-SAFE-006 |
| Receipt chain gaps | MEDIUM | EPIC-SAFE-002, EPIC-SAFE-006 |
| Policy not enforced by default | MEDIUM | EPIC-SAFE-005 |
| Partial delta application | LOW | Already mitigated ✅ |
| Circular references in payloads | LOW | Already mitigated ✅ |
| Time going backwards | LOW | Already mitigated ✅ |

### Footgun Elimination
| Footgun | Severity | Addressed By |
|---------|----------|--------------|
| #1: Mutating receipts after creation | HIGH | EPIC-SAFE-003, EPIC-SAFE-004 |
| #2: Forgetting to await async | HIGH | (ESLint, not epic) |
| #3: Passing wrong ID type | MED-HIGH | EPIC-SAFE-001 |
| #4: Calling appendEvent on frozen | MEDIUM | EPIC-SAFE-002, EPIC-SAFE-007 |
| #5: Empty operations in delta | MEDIUM | EPIC-SAFE-003 (builder validates) |
| #6: Race on state transitions | MEDIUM | EPIC-SAFE-006 |
| #7: Malformed JSON in payloads | LOW-MED | Already mitigated ✅ |
| #8: Missing required fields | LOW | EPIC-SAFE-003 |
| #9: Time going backwards | LOW | Already mitigated ✅ |
| #10: Circular references | LOW | Already mitigated ✅ |

### Implementation Roadmap
```
Week 1-2:   EPIC-SAFE-001 (Branded Types) ────┐
Week 2-4:   EPIC-SAFE-002 (State Machines) ───┼─┐
Week 3-5:   EPIC-SAFE-003 (Builder Pattern) ──┘ │
Week 5-6:   EPIC-SAFE-004 (Immutability) ───────┘
Week 6-9:   EPIC-SAFE-005 (Capabilities) ────────┐
Week 7-9:   EPIC-SAFE-007 (Phantom Types) ───────┼─┐
Week 10-14: EPIC-SAFE-006 (Linear Types) ────────┘ │
Week 15-16: Integration testing, docs ─────────────┘
```

### Risk Mitigation
| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Branded types break v5 compat | 60% | Medium | Migration guide + runtime adapter |
| Linear types too complex | 50% | High | Implement after phantom types proven |
| Capability overhead | 40% | Medium | Benchmark signature verification early |
| Developer pushback (too strict) | 35% | Medium | Excellent error messages + docs |

### Success Criteria (OTEL-Verified)
- [ ] Zero `state.invalid_transition` spans in production
- [ ] Zero `data.mutation_error` spans in production
- [ ] Zero `permission.bypass` spans in production
- [ ] Zero `id.confusion` errors in test suite
- [ ] 95% reduction in "Cannot X: Universe is Y" runtime errors
- [ ] 90% of current runtime guards removed (type-level enforcement)
- [ ] Developer survey: "v6 safety features" rated 8+/10

---

**Total Effort**: 16-22 weeks  
**Team**: 2 senior + 1 mid-level engineer  
**Value**: 75% risk reduction (MEDIUM → LOW)  
**ROI**: Eliminates 10 classes of bugs, reduces production incidents by 60%

---

**Next Steps**:
1. Review with architecture team
2. Prioritize EPICs (recommend: 001 → 002 → 003 → 004 → 007 → 005 → 006)
3. Create GitHub issues for each epic
4. Assign engineers to EPIC-SAFE-001 (foundation work)
5. Set up OTEL dashboards for safety metrics

**References**:
- Poka-Yoke Analysis: `/home/user/unrdf/docs/poka-yoke-v6-analysis/POKA-YOKE-ANALYSIS.md`
- V6 Vision: `/home/user/unrdf/V6-COMPLETE-REWRITE-VISION.md`
- Proof Tests: `/home/user/unrdf/proofs/poka-yoke/*.test.mjs`
