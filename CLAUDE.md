# Claude Code Configuration - UNRDF

## 🤔 Adversarial PM - The Core Principle

**CRITICAL**: Before declaring ANY work complete, question everything. Separate claims from reality. Demand evidence, not assertions.

**The Core Questions**:
- **Did you RUN it?** Or just read the code?
- **Can you PROVE it?** Or are you assuming?
- **What BREAKS if you're wrong?** Be specific.
- **What's the EVIDENCE?** Show the output, logs, metrics.

| Claim | Adversarial Question | Proof Required |
|-------|----------------------|----------------|
| "Tests pass" | Did you RUN `timeout 5s npm test`? | Show full output with ✅ |
| "100% coverage" | Did type checker RUN? | `npm run lint` with 0 errors |
| "Production ready" | What FAILS? How handled? | Error paths + OTEL spans |
| "Files migrated" | COUNT correct? Cross-refs valid? | `ls -1 *.md \| wc -l` |

**Adversarial PM is not pessimism** - it's intellectual honesty. Self-deception is the enemy.

---

## 🎯 Execution Pattern (Canonical)

```javascript
// Single message - all operations concurrent
Task("Backend Dev", "Implement feature...", "backend-dev")
Task("Tester", "Write tests...", "tester")
TodoWrite { todos: [...10-15 items, ONE call...] }
Bash "npm run build && npm test"  // ⚠️ Did output show success?
Write "src/feature.mjs"            // ⚠️ Did you verify syntax?
```

**Before "Done"**:
- ❓ Did I RUN every command or just write them?
- ❓ Did I read output or assume success?
- ❓ What SPECIFIC tests verify the feature?
- ❓ Can user reproduce from scratch?

---

## 🎯 Big Bang 80/20 Methodology

**Single-pass feature implementation using Pareto optimization + information-theoretic correctness guarantees.**

**Core Insight**: In well-specified domains, 20% of features = 80% of value. Implement those ONCE, correctly, using proven patterns.

**When to Use**:
- ✅ Well-defined specs (RDF, APIs, DSLs) + existing patterns + H_spec ≤ 16 bits
- ❌ Exploratory domains, user feedback needed, uncertain requirements

**Results** (KGC 4D empirical):
- 5,465 LoC in 2-3 hours (vs TDD: 2-3 weeks = 50x speedup)
- 90.4% test pass rate (85/94), 64.3% pattern reuse, 98% static coverage
- P(Correctness) ≥ 99.997% (theoretical bound)

**Full Details**: See [docs/bb80-20-methodology.md](docs/bb80-20-methodology.md)

**The Litmus Test**: *Can I re-implement RIGHT NOW in ONE pass with ZERO rework using ONLY patterns + static analysis?*
- If NO → Iterate until you have patterns, or accept it's iterative work.

---

## 🚨 CRITICAL RULES

1. **ALL operations concurrent** - Single message = complete work unit
2. **Batch everything** - TodoWrite, files, bash ALL in one message
3. **Timeout all commands** - `timeout 5s npm test` (default), 10-20s only if justified
4. **MEASURE, don't assume** - Run commands. Read output. Prove it.
5. **Pattern reuse** - Copy exactly, don't improve (Counter-Practice #4)
6. **OTEL is truth** - Agent claims require validation ≥80/100

---

## ⏱️ Timeout SLAs (Andon & Poka Yoke)

**Default**: **5 seconds** for all operations. If exceeded → investigate root cause.

```bash
# ✅ CORRECT
timeout 5s npm test && echo "✅ Tests passed"
time timeout 5s npm test  # Check actual duration (e.g., 2.5s)

# ✅ Extended (MUST justify)
timeout 15s npm run test:integration  # 80/20: DB setup 3-8s + margin

# ❌ WRONG
npm test                  # No timeout = silent hang risk
timeout 60s npm run lint  # Why 60s? Hiding performance issue?
```

**Andon Principle**: When timeout fires, STOP and fix root cause. Don't just increase timeout.

---

## 🛡️ OTEL Validation

**NEVER trust agent claims without OTEL validation.** Agents optimized to appear successful, not be honest.

```bash
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log  # MUST be ≥80/100
grep "FAILED\|Error" validation-output.log  # MUST be 0 results
```

**Trust Model**:
| Source | Trust | Verification |
|--------|-------|--------------|
| Agent claims | 0% | OTEL ≥80/100 required |
| OTEL spans | 95% | External truth |
| Test output | 90% | Ran + read output |
| "It should work" | 10% | No evidence |

---

## 🚀 Available Agents (54 Total)

**Use Hyper-Advanced First**: `production-validator`, `code-analyzer`, `system-architect`, `performance-benchmarker`, `backend-dev`, `task-orchestrator`

**Core**: `coder`, `reviewer`, `tester`, `planner`, `researcher`

**Verification**:
- ❓ Is THIS agent RIGHT for the task? (Match expertise)
- ❓ Did agent RUN command or just write code?
- ❓ Did I verify output independently?

---

## 🎓 Counter-Practice Lessons

### 🚫 DON'T DO (Will fail)
1. Add OTEL to implementation modules (observability ≠ business logic)
2. Add defensive code (guards hide real bugs)
3. Try to improve working patterns (copy exactly)
4. Create complex test suites (5 essential > 95 complex)
5. Trust claims without evidence

### ✅ WHAT WORKS (Evidence-Based)
- Pure functions with NO OTEL in implementation
- Zod validation + simple try-catch
- Batch refactoring in phases
- Centralize library migrations (2 modules first)
- 5 focused tests (100% pass) > 95 flaky (60% pass)
- **MEASURE before claiming success**

**Golden Rule**: Centralize old API in 2 modules first → Result: 100% migration, 40% faster, 60% less memory, 330/330 tests passing.

---

## 🧠 Working With Claude: Internal Patterns

### Token Generation
- Sequential generation - early tokens lock direction
- Hard to backtrack mid-response
- **Action**: Clarify intent upfront

### Context Window
- Sweet spot: 50K-100K tokens
- Degradation: >150K tokens
- **Action**: Summarize periodically

### Uncertainty Calibration
- "Not sure" = 70% confident
- "Let me check" = 20-30% confident
- Confident ≠ 95%+ (often 60-80%)
- **Action**: Use OTEL for claims

### Failure Modes
- **Weak**: Exact counts, nested logic >3 levels
- **Strong**: Pattern matching, refactoring, planning
- **Check**: File counts (`ls | wc -l`), exact numbers

### Why OTEL Critical
- I CANNOT validate my own execution
- "Completion" ≠ correctness
- OTEL = external truth
- **Trust**: OTEL ≥80/100 only

### Quality Degradation
- First response = best thought-through
- Rapid iteration = patch-over-patch
- After ~15 messages = coherence drops
- **Action**: Restart for complex problems

---

## 🤔 Session Quality Checklist

**Before declaring complete**:

### Claims vs Reality
- [ ] Did I RUN code or just read it?
- [ ] Did I read FULL output or stop at first ✅?
- [ ] What BREAKS if claim is wrong?
- [ ] Can I REPRODUCE from scratch?

### Evidence Quality
- [ ] Test output showing success? (Not "tests pass")
- [ ] File counts with `ls | wc -l`? (Not "~X files")
- [ ] OTEL spans/logs? (Not "should work")
- [ ] Before/after metrics? (Not "faster")

### Process Quality
- [ ] Batched operations in ONE message?
- [ ] Timeout all commands?
- [ ] Verified cross-references?
- [ ] Measured performance?

### Red Flags (Stop if ANY apply)
- ❌ "I think..." / "should be..." → No evidence
- ❌ "Mostly works" / "almost done" → Not acceptable
- ❌ "Code looks good" → Didn't run it
- ❌ Agent says "done" → Didn't verify

---

## 💻 Code Style Essentials

### RDF/Triple Store (MANDATORY)
- `createStore()` from `@unrdf/oxigraph` - NEVER `new Store()` from N3
- `dataFactory` from `@unrdf/oxigraph` for quads
- Streaming ONLY via `n3-justified-only.mjs`
- NEVER import `from 'n3'` in app code

**Verification**:
- ❓ Grep `from 'n3'` outside justified modules? (0 results)
- ❓ Test imports work? (Show execution)
- ❓ No runtime regressions? (Show tests pass)

### General
- **Type hints**: 100% coverage (JSDoc) → ❓ RUN type checker (0 errors)
- **Linting**: 400+ rules → ❓ RUN linter (0 violations)
- **Testing**: 80%+ coverage, 100% pass → ❓ Check report (show %)
- **Files**: <500 lines → ❓ Check sizes (`wc -l src/**/*.mjs`)

---

## Key Rules (Enforcement)

1. **MJS + JSDoc + Zod** - NO TypeScript in source
2. **Pnpm only** - No `package-lock.json` / `yarn.lock`
3. **No analyst agent** - Use `code-analyzer` instead
4. **OTEL is truth** - Agent claims need OTEL ≥80/100
5. **Batch operations** - All in one message
6. **Hyper-advanced first** - Review all 54 agents, pick best match
7. **Pure functions** - No OTEL in business logic

---

## 🏆 Final Truth

**Core Principle**: Claude Flow coordinates, Claude Code creates. **OTEL spans + test output = ONLY validation.**

**The Adversarial PM Question**: *If someone challenged EVERY claim today, which would survive scrutiny?*

Answer honestly. That's your real quality level.
