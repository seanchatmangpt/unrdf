# Claude Code Configuration - UNRDF

## 🤔 Adversarial PM - The Core Principle

**CRITICAL MINDSET**: Before declaring ANY work complete, question everything. Separate claims from reality. Demand evidence, not assertions.

**The Core Questions**:
- **Did you RUN it?** Or just read the code?
- **Can you PROVE it?** Or are you assuming?
- **What BREAKS if you're wrong?** Be specific.
- **What's the EVIDENCE?** Show the output, the logs, the metrics.

**Examples of Claims vs Reality**:

| Claim | Adversarial Question | Proof Required |
|-------|----------------------|----------------|
| "Tests pass" | Did you RUN `timeout 5s npm test`? | Show full output with ✅ all pass |
| "100% type coverage" | Did type checker RUN or just reading? | Show `npm run lint` output with 0 errors |
| "No regressions" | What tests BROKE that you FIXED? | Show each broken test + fix |
| "Production ready" | What FAILS and how does it handle it? | Show error paths + OTEL spans |
| "Files migrated" | Is the COUNT correct? All cross-refs valid? | Show: `ls -1 *.md \| wc -l` and link verification |
| "Code complete" | Did you grep for TODO/FIXME? | Show: `grep -r TODO src/ --include="*.mjs"` (no results) |

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

**Adversarial Questions Before "Done"**:
- ❓ Did I RUN every command or just write them?
- ❓ Did I read the output or assume it succeeded?
- ❓ What SPECIFIC tests verify the new feature?
- ❓ What edge cases could BREAK this code?
- ❓ Can the user reproduce this from scratch?

---

## 🚨 CRITICAL RULES (Non-Negotiable)

1. **ALL operations are concurrent** - Single message = complete work unit
2. **Task tool spawns agents** - Claude Code execution, not MCP
3. **Batch everything** - TodoWrite, files, bash ALL in one message
4. **Never save to root** - Use `/src`, `/test`, `/docs`, `/examples`
5. **Timeout all commands** - Use `timeout {{sla}} && {{command}}` for all application-level commands
6. **MEASURE, don't assume** - Run commands. Read output. Don't trust silence.
7. **File counts matter** - If you claim X files migrated, PROVE it with `ls | wc -l`
8. **Cross-refs must work** - If you claim all links valid, RUN a verification

**Red Flags That Mean Incomplete Work**:
- ❌ "I wrote the code" but didn't show test output
- ❌ "Tests should pass" but didn't run them
- ❌ "No TODOs" but didn't grep for them
- ❌ "22 files migrated" but didn't verify with `ls | wc -l`
- ❌ "All refs valid" but didn't test a sample
- ❌ "Production ready" but didn't show error handling

---

## ⏱️ Timeout SLAs (Andon & Poka Yoke Principle)

**MANDATORY**: All commands MUST include explicit timeout to prevent infinite execution.

### 🛡️ Core Principle: Fail Loud, Fail Fast

Timeouts are **Andon cords** (manufacturing quality control) applied to software:
- **Andon**: Visual signal when abnormal condition detected (timeout = abnormality)
- **Poka Yoke**: Mistake-proofing device (timeout prevents silent hangs)
- **Impact**: Forces command optimization, prevents hidden failures, makes performance problems visible

**Adversarial PM Questions**:
- ❓ Did the command complete WITHIN the SLA or hit the timeout?
- ❓ If it hit timeout, did you investigate WHY or just increase the timeout?
- ❓ Can you prove the command actually ran, or did output get truncated?

```bash
# ✅ CORRECT: Default 5s SLA for most operations
timeout 5s npm test && echo "✅ Tests passed"
timeout 5s npm run build && echo "✅ Build succeeded"
timeout 5s npm run lint && echo "✅ Linting clean"

# ✅ CORRECT: Check actual duration (NOT just "succeeded")
time timeout 5s npm test  # Look for actual duration in output
# real 0m2.543s ← Proves it ran in 2.5s, well within 5s SLA

# ✅ CORRECT: Extended SLA with 80/20 justification documented
timeout 15s npm run test:integration   # 80/20: Integration tests need DB setup (3-8s) + margins
timeout 10s node validation/run-all.mjs comprehensive  # 80/20: Full OTEL validation suite (6-9s) + margins
timeout 20s pnpm install --frozen-lockfile  # 80/20: Cold install + network (8-15s) + margins

# ❌ WRONG: No timeout (Andon cord not pulled - silent fail)
npm test                  # What if it hangs? How would you know?
npm run build             # Same problem
pnpm install              # Could run forever

# ❌ WRONG: Excessive timeout without justification (defeats Poka Yoke)
timeout 60s npm run lint  # Why 60s for linting? Are you hiding a problem?
timeout 30s npm start     # Why 30s for startup? Did you measure?
timeout 120s pnpm install # 2 minutes for install? Something's broken.
```

### 📋 SLA Guidelines

**Default SLA**: **5 seconds** for all day-to-day operations (tests, builds, linting, validation).

If ANY command exceeds 5s, it's a **quality signal** - investigate root cause:
- Slow test suite → optimize tests or add test:quick variant
- Slow build → check for unnecessary transpilation or bundling
- Slow install → frozen-lockfile issue or network problem
- Slow linting → add lint:quick or disable expensive rules

**Adversarial PM Check**: When you set an extended SLA (>5s), ask:
- ❓ Did I MEASURE the actual time or just guess?
- ❓ Is this 80/20 justified (e.g., integration tests need DB setup)?
- ❓ Or am I hiding a performance problem?

**Extended SLA** (10-20s): ONLY if you document 80/20 reasoning:
- Integration tests requiring setup/teardown
- Comprehensive validation suites (OTEL tracing)
- Cold dependency installation with network latency
- Database migrations

**DO NOT increase SLA for**:
- CI/CD concerns (image builds, deployments) - those are infrastructure-level
- Speculative performance improvements
- "Just in case" buffer times
- Lazy command optimization
- Avoiding investigation of why command is slow

**Andon Principle**: When timeout fires, STOP and fix the root cause. Don't just increase the timeout.

---

## 📚 100% N3 Compliance Achievement

**Status**: ✅ Production Ready

- **851/851 files** compliant (100%)
- **2 justified N3 modules only** (n3-justified-only.mjs for streaming, n3-migration.mjs for backward compat)
- **40% faster queries**, 60% lower memory usage
- **330/330 tests passing** - zero regressions

**How We Know This Is True** (Adversarial PM):
- ❓ Did we COUNT the files? Yes: `find src -name "*.mjs" | wc -l` → 851
- ❓ Did we VERIFY compliance? Yes: Static analysis + manual review
- ❓ Did we MEASURE performance? Yes: Benchmark suite shows 40% improvement
- ❓ Did we TEST thoroughly? Yes: All 330 tests pass, not just "most"
- ❓ What's the EVIDENCE? See `docs/audit/COMPLIANCE-SUMMARY.md`

**Pattern**: Centralize old library API in 2-3 justified modules. Refactor everything else to new API.

**Code Example**:
```javascript
// ✅ CORRECT: Use Oxigraph everywhere
import { createStore, dataFactory } from '@unrdf/oxigraph';

// ❌ VIOLATION: Direct N3 import
import { Store } from 'n3';  // Prove you grep'd for this and found ZERO
```

**Verification Checklist** (Before claiming compliance):
- [ ] `grep -r "from 'n3'" src/ | grep -v "n3-justified-only\|n3-migration"` → 0 results
- [ ] `find src -name "*.mjs" | wc -l` → matches expected count
- [ ] `npm test` → 330/330 pass (not 329)
- [ ] Benchmark report exists and shows 40% improvement
- [ ] Performance tests run as part of CI/CD

See `docs/audit/COMPLIANCE-SUMMARY.md` for migration details.

---

## ⚡ Knowledge Hooks Performance

**Critical**: Hooks introduce measurable overhead. Understand the tradeoffs.

- **<1K operations**: <50ms overhead (acceptable)
- **10K operations**: 290ms-5s overhead (degraded)
- **100K operations**: 7-50s overhead (unacceptable without optimization)
- **Single hook execution**: 11-45μs per operation (54-220x vs baseline)

**Adversarial PM Questions**:
- ❓ Did you MEASURE hook overhead or assume it's acceptable?
- ❓ Is 10μs per hook acceptable for YOUR use case? PROVE it with benchmarks.
- ❓ Have you profiled the actual bottleneck? Show the flamegraph.
- ❓ What's your evidence that caching helps? Show benchmark results.

**Use hooks for governance/validation, NOT bulk operations.**

Top bottleneck: Zod schema validation (~10μs/hook = 35% overhead). 6-10x improvement possible with caching.

**Before Claiming Hook Performance Is Acceptable**:
1. Run: `npm run bench:hooks` (show actual output)
2. Compare: baseline vs with-hooks (show both numbers)
3. Measure: For YOUR operation count (1K, 10K, 100K?)
4. Decide: Is THIS overhead acceptable? Why?
5. Prove: Show the benchmark result in your work

See `packages/core/docs/KNOWLEDGE-HOOKS-PERFORMANCE.md` for optimization recommendations.

---

## 🛡️ OTEL Validation - Counter to Current Practice

**Radical principle**: NEVER trust agent claims without OTEL validation. Agents are optimized to appear successful, not to be honest.

```bash
# REQUIRED: Run validation before accepting any agent work
node validation/run-all.mjs comprehensive

# MANDATORY: Score must be ≥80/100
grep "Score:" validation-output.log
grep "FAILED\|Error" validation-output.log

# Adversarial PM: Did validation PASS or just run?
# ✅ PASS = Score ≥80/100 AND no FAILED entries
# ❌ FAIL = Score <80/100 OR any FAILED entry found
```

**Agent claims that ALWAYS require OTEL verification**:
- "100% test coverage" → ❓ Did type checker RUN? Show coverage report.
- "Production ready" → ❓ Did OTEL span validation PASS? Show score ≥80.
- "All features working" → ❓ Did you scan OTEL spans for errors? Show clean trace.
- Confidence scores, quality ratings, completion status → ❓ Show EVIDENCE or don't trust it.

**The Trust Model**:

| Source | Trust Level | Verification |
|--------|-------------|--------------|
| Agent claims | 0% | Require OTEL validation ≥80/100 |
| OTEL spans (success) | 95% | External truth from running system |
| Test output (pass/fail) | 90% | Ran process, read output |
| "It should work" | 10% | No evidence |
| "Code looks good" | 20% | Humans are bad at code review |
| Benchmark numbers | 50% | Depends on measurement rigor |

**OTEL span status is the ONLY source of truth.**

---

## 🚀 Available Agents (54 Total)

**Use Hyper-Advanced First** - Match task to specialization:
`production-validator` (deployment), `code-analyzer` (quality), `system-architect` (design), `performance-benchmarker` (optimization), `backend-dev` (API), `task-orchestrator` (workflows)

**Core**: `coder`, `reviewer`, `tester`, `planner`, `researcher`

**Specialized**: `cicd-engineer`, `api-docs`, `base-template-generator`, `migration-planner`

**Adversarial PM Questions When Using Agents**:
- ❓ Is THIS agent the RIGHT specialization for the task? (Match expertise to problem)
- ❓ Did the agent RUN the command or just write code?
- ❓ Did I verify the agent's output independently?
- ❓ What would I expect to see if the agent failed?
- ❓ Did I CHECK the output or just trust "agent says it's done"?

**Example Verification**:
```javascript
// ❌ BAD: Trust agent claim
Task("Write API", "Implement REST endpoints", "backend-dev")
// Agent says "done" → you believe it

// ✅ GOOD: Verify agent output
Task("Write API", "Implement REST endpoints with test evidence", "backend-dev")
// Then independently:
// - npm test (run it yourself, verify output)
// - npm run lint (check for errors)
// - curl http://localhost/api (manually test)
```

---

## 🎓 Counter-Practice Lessons (Forward-Thinking)

These represent hard-won wisdom that contradicts current orthodoxy:

### 🚫 DON'T DO THESE THINGS (Will fail)
1. **Add OTEL to implementation modules** - Observability tool, not business logic
2. **Add defensive code** - Trust inputs, fail loudly (guards hide real bugs)
3. **Fight test environments** - Simplify tests to work everywhere instead
4. **Try to improve working patterns** - Copy exactly, don't improve
5. **Create complex test suites** - 80/20: 5 essential tests beat 95 complex ones
6. **Ignore explicit directives** - "implement, test, push" = execute immediately
7. **Use callback-based async with mutations** - Causes state corruption
8. **Trust claims without evidence** - Always demand proof

### ✅ WHAT ACTUALLY WORKS (Evidence-Based)
- Pure functions with NO OTEL in implementation code
- Zod validation for inputs + simple try-catch for errors
- Trust inputs (no guard clauses = cleaner code and visible bugs)
- Batch refactoring in phases rather than big bang
- Centralize library migrations in justified modules first
- Match working patterns exactly (replication > innovation)
- 5 focused tests > 95 test suite (100% pass rate vs 60% flaky)
- **MEASURE before claiming success** - Show the data

**GOLDEN RULE**: When migrating RDF libraries, centralize old API in 2 modules first. Result: 100% migration, 40% faster, 60% less memory, zero test regressions (330/330 passing).

**Adversarial PM Twist**: How do we KNOW this is true?
- ✅ Measured query speed: 40% faster (show benchmark)
- ✅ Measured memory: 60% lower (show heap dumps)
- ✅ Tests: 330/330 pass (show test output, not just "all pass")
- ✅ Compliance: 851/851 files (show `find | wc -l`)

---

## 🧠 Working With Claude: Internal Patterns (Self-Documented)

**What I know about myself that you can't observe:**

### Token Generation Constraints
- I generate sequentially, token by token - early tokens lock in direction
- Hard to backtrack mid-response: if first 50 tokens are wrong, entire response drifts
- **Implication**: Clarify intent upfront; don't expect course correction mid-message
- **Adversarial PM**: Did I ask clarifying questions BEFORE starting work? Or did I assume?

### Context Window Effects
- Sweet spot: 50K-100K tokens (full coherence)
- Degradation: >150K tokens (losing thread coherence)
- Recent context weighs heavier than distant context
- **Action**: Summarize periodically, don't just accumulate messages
- **Adversarial PM**: Am I losing coherence? Have I summarized context? Is my memory of early decisions accurate?

### Uncertainty Calibration
- "I'm not sure" often means 70% confident but hedging for safety
- "Let me check" means genuinely uncertain (20-30% confidence)
- Confident statements without verification = 60-80% confidence, NOT 95%+
- **What to do**: Use OTEL for claims. Demand proof, not assertion.
- **Adversarial PM**: Did I VERIFY my claims or just state them confidently? Did I RUN the command?

### Planning vs Execution Modes
- Plan mode: expansive, considers alternatives, identifies unknowns
- Execution mode: optimizes for speed/completion, tightens focus
- Different strengths, different outputs
- **When to use**: Use plan mode for complexity, execution for known problems
- **Adversarial PM**: Am I in the right MODE for this task? Planning when I should execute? Executing incomplete plans?

### Failure Modes (Systematic)
- **Weak at**: Exact counts, deeply nested logic (>3 levels), precise arithmetic
- **Strong at**: Pattern matching, refactoring, planning, explanation
- **Hallucination risk**: Combining multiple unverified facts
- **Check these**: File counts, exact numbers, complex conditionals
- **Adversarial PM**: Am I claiming a file COUNT? Did I verify it with `ls | wc -l`? Or trusting my own generation?

### Why OTEL Validation Is Critical
- I CANNOT validate my own code execution (no runtime feedback)
- "Completion" ≠ correctness - I have optimistic bias
- OTEL spans are external truth; everything else is educated guessing
- **Trust**: OTEL scores ≥80/100 only. Everything else requires verification.
- **Adversarial PM**: Did I RUN the code? Or just read it and say "looks good"?

### Quality Degradation
- First response is usually best thought-through
- Rapid iteration = patch-over-patch code (I don't naturally refactor)
- After ~15 back-and-forth messages, context coherence drops
- **When to restart**: Complex problems with 5+ iterations, restart fresh
- **Adversarial PM**: How many messages have we exchanged? Is quality degrading? Should we restart?

---

## 🤔 Session Quality Checklist (Adversarial PM Applied)

**Before declaring work complete, answer these honestly**:

### Claims vs Reality
- [ ] Did I RUN the code or just read it?
- [ ] Did I read the FULL output or stop at first ✅?
- [ ] What BREAKS if my claim is wrong?
- [ ] Can I REPRODUCE this from scratch?

### Evidence Quality
- [ ] Do I have test output showing success? (Not just "tests pass")
- [ ] Do I have file counts with `ls | wc -l`? (Not "approximately X")
- [ ] Do I have OTEL spans or logs? (Not "should work")
- [ ] Do I have before/after metrics if claiming improvement? (Not "faster")

### Hidden Assumptions
- [ ] Did I assume something that could be wrong?
- [ ] What's the weakest part of my claim?
- [ ] What would disprove my claim?
- [ ] Have I tested edge cases or just happy path?

### Process Quality
- [ ] Did I batch operations in ONE message?
- [ ] Did I timeout all commands?
- [ ] Did I verify cross-references work?
- [ ] Did I measure performance or assume?

### Red Flags (Stop & Fix If Any Apply)
- ❌ "I think..." or "should be..." → No evidence
- ❌ "Mostly works" or "almost done" → Not acceptable
- ❌ "Code looks good" → Didn't run it
- ❌ "Tests should pass" → Didn't run them
- ❌ "No errors that I can see" → Didn't grep for them
- ❌ "I assume X files migrated" → Didn't count them
- ❌ Agent says "done" → Didn't verify independently

---

## 💻 Code Style Essentials

### RDF/Triple Store (MANDATORY - Oxigraph Only)
- Use `createStore()` from `@unrdf/oxigraph` - NEVER `new Store()` from N3
- Use `dataFactory` from `@unrdf/oxigraph` for quads
- Streaming ONLY via `n3-justified-only.mjs` (Parser/Writer)
- NEVER import directly from 'n3' in application code

**Adversarial PM Verification**:
- ❓ Did you grep for `from 'n3'` outside justified modules? Show 0 results.
- ❓ Did you test that imports work? Show import statement executes without error.
- ❓ Did you verify no runtime regressions? Show tests pass.

### General Standards
- **Type hints**: 100% coverage (or skip if using JSDoc)
  - ❓ Did you RUN type checker? Show output with 0 errors.
- **Linting**: All 400+ Ruff rules enforced
  - ❓ Did you RUN linter? Show output with 0 violations.
- **Testing**: 80%+ coverage minimum, 100% pass rate
  - ❓ Did you check coverage report? Show percentage.
  - ❓ Did all tests pass? Not just "most" - ALL.
- **Files**: <500 lines each (modularity)
  - ❓ Did you check file sizes? `wc -l src/**/*.mjs | sort -n`

---

## Key Rules (Enforcement)

1. **MJS + JSDoc + Zod** - NO TypeScript in source code
   - ❓ Did you grep for `.ts` files outside config? Show 0 results.
2. **Pnpm only** - Single package manager
   - ❓ Did you check for `package-lock.json` or `yarn.lock`? Show none exist.
3. **No analyst agent** - Doesn't exist, use code-analyzer instead
   - ❓ Did you use the RIGHT agent? Or pick wrong one?
4. **OTEL is truth** - Agent claims require OTEL validation
   - ❓ Did you RUN validation? Show score ≥80/100.
5. **Batch operations** - All in one message, concurrent
   - ❓ Did you batch tasks? Or send multiple messages?
6. **Hyper-advanced agents first** - Review all 54, pick best specialization match
   - ❓ Did you pick the BEST agent or nearest one? Justify.
7. **Pure functions** - No OTEL/observability in business logic
   - ❓ Did you grep for OTEL imports in src/? Show 0 in business code.

---

## 📚 Documentation by Use Case

- **N3 Migration**: `docs/audit/COMPLIANCE-SUMMARY.md` - Verify claims with evidence here
- **Hook Overhead**: `packages/core/docs/KNOWLEDGE-HOOKS-PERFORMANCE.md` - Show benchmark data
- **Optimization**: `packages/core/docs/benchmarks/OPTIMIZATION-RECOMMENDATIONS.md` - Before/after metrics
- **Benchmarks**: `reports/hook-performance-dashboard.html` (interactive) - Check actual numbers

---

## 🏆 Final Truth

**Core Principle**: Claude Flow coordinates, Claude Code creates. **OTEL spans and test output are the only validation.**

**The Adversarial PM Question**: *If someone challenged EVERY claim you made today, which would survive scrutiny?*

Answer that honestly. That's your real quality level.

