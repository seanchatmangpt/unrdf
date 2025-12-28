# V6 P0+P1 Agent Allocation Plan

## Agent Pool (5 Specialized Agents)

### Configuration
| Agent ID | Type | Specialty | Concurrent Capacity |
|----------|------|-----------|---------------------|
| **A1** | backend-dev | KGC-4D, receipts, pure functions | 1 capsule |
| **A2** | backend-dev | Schema generation, Zod, validation | 1 capsule |
| **A3** | tester | Adversarial testing, fixtures | 1 capsule |
| **A4** | code-analyzer + production-validator | Validation, OTEL | 2-3 capsules (review) |
| **A5** | system-architect + performance-benchmarker | Config, benchmarks | 1 capsule |

---

## Phase-by-Phase Allocation

### Phase 1: P0 Critical Path (Days 1-4, 32 hours)

#### Day 1 (Hours 0-8)
| Agent | Capsule | Task | Hours | Output |
|-------|---------|------|-------|--------|
| A1 | V6-001 | Create wrapper.mjs, integrate KGC-4D | 8 | wrapWithReceipt() complete |
| A2 | V6-002 | Create CLI tool, apply to @unrdf/core | 8 | 80% complete (2h remaining) |
| A3 | PREP | Create test scaffolds for V6-001, V6-002 | 8 | Test files ready |
| A4 | PREP | Set up OTEL validation pipeline | 4 | Gate validation scripts |
| A5 | STANDBY | - | 0 | Awaiting V6-004 |

**End of Day 1**: V6-001 ✓, V6-002 80% complete

#### Day 2 (Hours 8-16)
| Agent | Capsule | Task | Hours | Output |
|-------|---------|------|-------|--------|
| A1 | V6-001 | Create examples, documentation | 2 | COMPLETE ✓ |
| A1 | V6-003 | Start adapters integration | 6 | 38% of V6-003 |
| A2 | V6-002 | Finish CLI, create examples | 2 | COMPLETE ✓ |
| A2 | V6-003 | Start migration guide | 6 | 38% of V6-003 |
| A3 | V6-001, V6-002 | Run test suites, validate | 6 | Tests 100% pass |
| A3 | V6-003 | Create test scaffolds | 2 | Test files ready |
| A4 | GATE 1 | Validate V6-001 + V6-002 | 4 | Gate 1 PASSED |
| A5 | STANDBY | - | 0 | Awaiting V6-004 |

**End of Day 2**: V6-001 ✓, V6-002 ✓, Gate 1 ✓, V6-003 76% complete

#### Day 3 (Hours 16-24)
| Agent | Capsule | Task | Hours | Output |
|-------|---------|------|-------|--------|
| A1 | V6-003 | Finish ESLint rules validation | 4 | ESLint rules tested |
| A1 | PREP | Prepare for Wave 1 (V6-012) | 4 | Setup complete |
| A2 | V6-003 | Finish migration guide | 4 | Guide complete |
| A2 | PREP | Prepare for Wave 1 (V6-010) | 4 | Setup complete |
| A3 | V6-003 | Create full test suite (20+ tests) | 8 | 20+ tests ready |
| A4 | V6-003 | Code review, validation | 4 | Review complete |
| A5 | STANDBY | - | 0 | Awaiting V6-004 |

**End of Day 3**: V6-003 95% complete

#### Day 4 (Hours 24-32)
| Agent | Capsule | Task | Hours | Output |
|-------|---------|------|-------|--------|
| A1 | V6-003 | Final fixes, polish | 2 | COMPLETE ✓ |
| A2 | V6-003 | Final validation | 1 | COMPLETE ✓ |
| A3 | V6-003 | Run all tests, OTEL validation | 2 | Tests 100% pass |
| A4 | GATE 2 | Validate V6-003 | 2 | Gate 2 PASSED |
| A5 | V6-004 | Workspace verification | 2 | COMPLETE ✓ |
| A4 | GATE 3 | Validate V6-004, P0 complete | 1 | Gate 3 PASSED |
| ALL | PREP | Prepare for Wave 1 start | 2 | Wave 1 ready |

**End of Day 4**: P0 COMPLETE ✓ (all 4 capsules)

---

### Phase 2: P1 Wave 1 (Days 5-6, 16 hours)

#### Days 5-6 (Hours 32-48)
| Agent | Capsule | Task | Hours | Output |
|-------|---------|------|-------|--------|
| A1 | V6-012 | core L2→L3: Remove Date.now(), add receipts | 14 | COMPLETE ✓ |
| A2 | V6-010 | oxigraph L3→L4: Timeouts, Zod validation | 10 | COMPLETE ✓ |
| A3 | V6-015 | kgc-4d L4→L5: Integration tests | 7 | COMPLETE ✓ |
| A4 | GATE 4 | Validate Wave 1 | 4 | Gate 4 PASSED |
| A5 | PREP | Benchmark baselines for Wave 2 | 8 | Baselines recorded |

**End of Day 6**: Wave 1 COMPLETE ✓ (3 capsules)

---

### Phase 3: P1 Wave 2 (Days 7-10, 32 hours)

#### Days 7-10 (Hours 48-80)
| Agent | Capsule | Task | Hours | Output |
|-------|---------|------|-------|--------|
| A1 | V6-013 | core L3→L4: Timeout guards, adversarial tests | 12 | COMPLETE ✓ |
| A1 | V6-016 | hooks L2→L5 (assist) | 13 | 52% complete |
| A2 | V6-017 | streaming L2→L5: AsyncIterator, receipts, tests | 27 | COMPLETE ✓ |
| A3 | V6-019 | cli L2→L5: Zod args, receipts, integration | 22 | COMPLETE ✓ |
| A3 | V6-016 | hooks L2→L5 (assist) | 10 | Help A1 complete |
| A4 | V6-018 | federation L2→L5: SPARQL, receipts, tests | 28 | COMPLETE ✓ |
| A5 | ALL | Performance benchmarks for each package | 10 | No regressions |

**Coordination**:
- A1 and A3 collaborate on V6-016 (25h total, split 13h + 12h)
- A2, A3, A4 work independently on longest capsules
- A5 runs continuous benchmarks

**End of Day 10**: Wave 2 COMPLETE ✓ (5 capsules)

---

### Phase 4: P1 Wave 3 (Days 11-12, 17 hours)

#### Days 11-12 (Hours 80-96)
| Agent | Capsule | Task | Hours | Output |
|-------|---------|------|-------|--------|
| A3 | V6-011 | oxigraph L4→L5: Integration tests with 3+ L5 packages | 7 | COMPLETE ✓ |
| A3 | V6-014 | core L4→L5: Integration tests with 5+ L5 packages | 10 | COMPLETE ✓ |
| A4 | GATE 6 | Final OTEL validation for ALL packages | 8 | Gate 6 PASSED |
| A5 | ALL | Final performance benchmarks | 5 | No regressions |
| A1, A2 | REVIEW | Code review, documentation polish | 8 | P1 COMPLETE ✓ |

**Note**: V6-011 and V6-014 are sequential (A3 does V6-011 first, then V6-014)

**End of Day 12**: P1 COMPLETE ✓ (10 capsules), ALL GATES PASSED

---

## Utilization Analysis

### Agent Utilization (96 hours wall time)
| Agent | Hours Allocated | Utilization | Idle Time | Peak Load |
|-------|-----------------|-------------|-----------|-----------|
| A1 (backend-dev) | 59h | 61% | 37h | Days 7-10 (Wave 2) |
| A2 (backend-dev) | 54h | 56% | 42h | Days 7-10 (Wave 2) |
| A3 (tester) | 66h | 69% | 30h | Days 11-12 (Wave 3) |
| A4 (validator) | 51h | 53% | 45h | Days 1-4 (P0) + Gate validation |
| A5 (architect) | 25h | 26% | 71h | Days 5-12 (Benchmarks) |

**Average Utilization**: 53% (expected due to sequential dependencies)

### Bottleneck Analysis
| Bottleneck | Duration | Waiting Agents | Mitigation |
|------------|----------|----------------|------------|
| V6-003 (16h sequential) | Days 2-3 | A5 standby (16h) | Use for prep work |
| V6-012 completion | Day 5-6 | A4, A5 (partial) | Start Gate 4 prep early |
| V6-018 (28h longest) | Days 7-10 | None | Well-matched to 4-day window |
| Wave 3 sequential | Days 11-12 | A1, A2 (partial) | Use for code review, docs |

---

## Agent Handoff Protocol

### Day-to-Day Handoffs
Each agent creates a handoff document at end of day:

```yaml
# /home/user/unrdf/.handoffs/2025-12-27-A1.yaml
agent: A1
date: 2025-12-27
capsule: V6-003
status: in_progress
progress: 76%
completed_tasks:
  - Integrated wrapWithReceipt from V6-001
  - Created adapter tests (12/20)
blocker: None
next_tasks:
  - Complete remaining 8 adapter tests
  - Validate ESLint rules on real code
handoff_to: A1 (continue) or A2 (if A1 blocked)
artifacts:
  - /packages/v6-compat/test/adapters.test.mjs
  - /packages/v6-compat/src/adapters.mjs (updated)
```

### Gate Validation Handoff (Critical)
Before any gate validation, ALL agents must:
1. Commit all code changes
2. Run local tests (`timeout 5s pnpm test`)
3. Verify no `Date.now()` or `Math.random()` in code
4. Update STATUS_DASHBOARD.md
5. Create capsule receipt in `/home/user/unrdf/.receipts/v6/`

**Gate Validator (A4) Checklist**:
- [ ] All tests pass locally
- [ ] All artifacts exist at specified paths
- [ ] OTEL validation ≥80/100
- [ ] Receipt chain valid (dependencies satisfied)
- [ ] No scope creep (compare against CAPSULE_BACKLOG)

---

## Real-Time Coordination

### Daily Standup (Adversarial PM Questions)
**Time**: Start of each day (8:00 AM)
**Duration**: 15 minutes
**Format**: Each agent answers:

1. **What did you COMPLETE yesterday?** (Show receipts, not claims)
2. **Did you RUN tests or just write code?** (Show `pnpm test` output)
3. **What's your OTEL score?** (Show validation log)
4. **What are you BLOCKING?** (Name specific agents/capsules)
5. **What's BLOCKING you?** (Name specific dependencies)

### Mid-Day Sync (As Needed)
**Trigger**: Any blocker or dependency resolution
**Participants**: Affected agents only
**Duration**: 5-10 minutes

### End-of-Day Report
Each agent updates:
- `/home/user/unrdf/docs/v6/STATUS_DASHBOARD.md`
- `/home/user/unrdf/.handoffs/<date>-<agent>.yaml`
- `/home/user/unrdf/.receipts/v6/<capsule>.json` (if capsule complete)

---

## Escalation Protocol

### Blocker Escalation
| Severity | Response Time | Escalation Path |
|----------|---------------|-----------------|
| 🔴 CRITICAL (blocks >2 agents) | <1 hour | Immediate all-hands, reassign agents |
| 🟡 HIGH (blocks 1 agent) | <4 hours | Task Orchestrator reviews, suggests mitigation |
| 🟢 MEDIUM (slows progress) | <1 day | Document, continue, address in next phase |

### OTEL Failure Escalation
If any gate validation fails (OTEL <80/100):
1. **STOP all forward progress** (do not proceed to next wave)
2. Task Orchestrator analyzes failure root cause
3. Assign 2 agents to fix (original implementer + production-validator)
4. Re-run gate validation
5. Only proceed when OTEL ≥80/100

**No Exceptions**: Gates are HARD STOPS. Manual overrides are forbidden.

---

## Post-Completion Review

### P0 Retrospective (End of Day 4)
**Attendees**: All agents
**Duration**: 30 minutes
**Topics**:
- What went well? (Concrete examples)
- What blocked us? (Root causes)
- What would we do differently in P1?
- Are agent assignments optimal?

### P1 Retrospective (End of Day 12)
**Attendees**: All agents
**Duration**: 45 minutes
**Topics**:
- Did we meet the 12-day timeline?
- Were there scope creep incidents?
- What was our actual OTEL score distribution?
- How many rework cycles did we have?
- Lessons for P2/P3 execution

### Metrics to Capture
- Actual hours vs estimated hours (per capsule)
- Test pass rate on first run (per capsule)
- OTEL scores (per capsule)
- Number of gate validation retries
- Agent idle time vs planned
- Critical path variance (planned vs actual)

---

## Agent Rotation Strategy (If Needed)

### Burnout Prevention
If any agent is overloaded (>80% utilization for >3 consecutive days):
1. Task Orchestrator reviews workload
2. Redistribute tasks to underutilized agents
3. Delay non-critical capsules if necessary
4. Prioritize critical path work only

### Skill Gaps
If an agent lacks specific skills for assigned capsule:
1. Pair with expert agent (2-4 hours knowledge transfer)
2. Use existing patterns (copy from similar capsule)
3. Last resort: Reassign capsule to different agent

---

**END OF AGENT ALLOCATION PLAN**

**Next Steps**:
1. Agents A1 and A2: Start V6-001 and V6-002 immediately
2. Agent A3: Create test scaffolds for V6-001 and V6-002
3. Agent A4: Set up OTEL validation pipeline
4. Agent A5: Standby for V6-004 (Day 4)
5. All agents: Daily updates to STATUS_DASHBOARD.md
