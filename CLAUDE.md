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

## 🎯 Big Bang 80/20 Methodology (UNRDF Innovation)

**WHAT IT IS**: Single-pass feature implementation using Pareto optimization, hyperdimensional feature spaces, and information-theoretic correctness guarantees. NO iteration, NO rework, NO refinement cycles.

**THE CORE INSIGHT**: In well-specified domains, 20% of features deliver 80% of value. Implement those features ONCE, correctly, using proven patterns and rigorous validation.

### When to Use BB80/20

✅ **APPLICABLE** when:
- Specification entropy ≤ 16 bits (at most ~65k feature combinations)
- Well-defined interfaces and semantics (RDF, APIs, DSLs, algorithms)
- Existing patterns available for reuse
- Deterministic algorithms with clear preconditions/postconditions
- Examples: KGC 4D, semantic web libraries, event sourcing, DSLs

❌ **NOT APPLICABLE** when:
- Exploratory/research domains (ML research, novel algorithms)
- User interaction design (requires user feedback iteration)
- Uncertain or changing requirements (ambiguous specifications)
- Specification entropy > 20 bits (complex distributed systems)
- No existing patterns to reuse

**Adversarial PM Question**: *Is this domain TRULY well-specified, or am I fooling myself?*
- ❓ Can I enumerate ALL critical features in <20 items?
- ❓ Do proven patterns exist for ≥60% of features?
- ❓ Are preconditions/postconditions CLEAR and unambiguous?
- ❓ Or am I forcing BB80/20 onto an exploratory problem?

### The 11-Step BB80/20 Workflow

#### Step 1: Feature Discovery & Entropy Analysis
```bash
# Extract all features from specification
# Compute specification entropy: H_spec = -Σ p(f_i) log p(f_i)
# Check: H_spec ≤ 16 bits?

# Adversarial PM:
❓ Did I enumerate ALL features or just obvious ones?
❓ Did I calculate entropy or just estimate?
❓ What features am I MISSING that will bite me later?
```

#### Step 2: Pareto Frontier Analysis
```bash
# Compute value/cost ratio for each feature
# Identify Pareto frontier: 20% of features delivering 80% of value
# Example: BigInt Time (value/cost = 4.75) vs React UI (0.13)

# Adversarial PM:
❓ Did I use REAL data (benchmarks, user research) or gut feelings?
❓ Which high-cost features am I CUTTING? What breaks if I'm wrong?
❓ Can I PROVE the 20% delivers 80% value? Show the math.
```

**Pareto Feature Checklist**:
| Feature | Est. Value | Est. Cost (LoC) | Value/Cost | Include? |
|---------|-----------|----------------|------------|----------|
| Feature A | 95% | 20 | 4.75 | ✅ Pareto |
| Feature B | 85% | 50 | 1.70 | ✅ Pareto |
| Feature C | 40% | 300 | 0.13 | ❌ Skip v1 |

#### Step 3: Hyperdimensional Feature Embedding
```bash
# Map features to hyperdimensional space (D = 10k-1M dimensions)
# Compute semantic similarity between features
# Identify feature clusters for pattern reuse

# Adversarial PM:
❓ This step is theoretical - did I ACTUALLY do it or skip it?
❓ In practice: Did I identify SIMILAR features in existing code?
❓ Can I list 3+ existing patterns that match Pareto features?
```

#### Step 4: Pattern Matching in Codebase
```bash
# For each Pareto feature, find existing proven pattern
# Requirement: Similarity > 90% AND context matches
# Build pattern library with copy-paste code

# Adversarial PM:
❓ Did I grep for patterns or assume they exist?
❓ What's the ACTUAL reuse rate? (Target: ≥60% LoC from patterns)
❓ Which features have NO patterns? (Risk: will I have to write from scratch?)

# ✅ CORRECT: Show pattern inventory
grep -r "createStore\|transaction\|process.hrtime" packages/*/src/**/*.mjs
ls -1 patterns/*.mjs | wc -l  # Count available patterns

# ❌ WRONG: "I think patterns exist for most features"
```

#### Step 5: Information-Geometric Architecture Design
```bash
# Design architecture on statistical manifold
# Use natural gradient descent for optimization
# Document architecture with formal preconditions/postconditions

# Adversarial PM:
❓ In practice: Did I sketch architecture on paper/diagram?
❓ Did I identify data structures, algorithms, API shapes?
❓ Can someone ELSE implement from my architecture doc?
❓ Or is this just hand-waving without concrete design?

# ✅ CORRECT: Write architecture.md with:
# - Data structures (types, schemas)
# - Algorithms (pseudocode)
# - API contracts (inputs, outputs, errors)
# - Dependencies (what patterns to use)
```

#### Step 6: Pseudocode Generation
```bash
# Generate pseudocode satisfying formal specification
# Verify: preconditions → postconditions
# Check composability of all components

# Adversarial PM:
❓ Did I WRITE pseudocode or skip to coding?
❓ Can I trace from spec → pseudocode → implementation?
❓ What invariants must hold? Did I document them?
```

#### Step 7: Code Implementation via Pattern Library
```bash
# Implement by COPY-PASTE from proven patterns
# Assembly: Code = Σ PatternLib[feature_i]
# Minimal new code (target: <40% novel code)

# Adversarial PM:
❓ What % of code is copy-pasted vs written fresh?
❓ Did I MODIFY patterns (risky) or use AS-IS (safe)?
❓ What NEW code did I write? (Highest defect risk)

# ✅ CORRECT: Track reuse rate
total_loc=$(wc -l src/*.mjs | tail -1 | awk '{print $1}')
reused_loc=$(grep -c "# Copied from:" src/*.mjs)
echo "Reuse rate: $((reused_loc * 100 / total_loc))%"  # Target: ≥60%

# ❌ WRONG: "Most code is reused" without measurement
```

#### Step 8: Syntax Validation (No Execution)
```bash
# Run syntax checker WITHOUT execution
timeout 5s node --check src/**/*.mjs

# Adversarial PM:
❓ Did I RUN syntax check or assume code parses?
❓ Show me the output with 0 syntax errors.
```

#### Step 9: Static Analysis
```bash
# Linting, type checking, security scanning
timeout 5s npm run lint
timeout 5s npm run typecheck  # JSDoc validation

# Adversarial PM:
❓ Did linter/typechecker PASS (0 errors) or have issues?
❓ What warnings did I ignore? Why are they safe to ignore?
❓ Show full output, not just "it passed"

# ✅ CORRECT: Show clean output
npm run lint 2>&1 | grep -E "error|warning" || echo "✅ 0 errors, 0 warnings"
```

#### Step 10: Specification Compliance Verification
```bash
# Checklist verification against original spec
# ALL Pareto features must be implemented
# ALL preconditions/postconditions must be satisfied

# Adversarial PM:
❓ Did I check EVERY Pareto feature is implemented?
❓ Which features did I SKIP? Why? (Must justify)
❓ Can I trace spec requirement → implementation line?

# ✅ CORRECT: Compliance matrix
echo "Feature | Implemented | File | Lines"
echo "BigInt Time | ✅ | datum.mjs | 45-60"
echo "Event Log | ✅ | event-log.mjs | 12-80"
# ... ALL Pareto features listed

# ❌ WRONG: "I implemented all features" without evidence
```

#### Step 11: Deploy to Production
```bash
# Deploy ONLY if Steps 8-10 ALL pass
# No tests required (validated by static analysis + spec compliance)
# OTEL validation AFTER deployment (not before)

# Adversarial PM:
❓ Did ALL validation steps PASS (not just run)?
❓ What's my rollback plan if deployment breaks production?
❓ Is monitoring in place to detect failures?

# ✅ CORRECT: Gate deployment on validation
if npm run lint && npm run typecheck && ./verify-spec-compliance.sh; then
  git add . && git commit -m "feat: BB80/20 implementation"
  git push
else
  echo "❌ Validation failed - DO NOT deploy"
  exit 1
fi
```

### BB80/20 Success Criteria (Information-Theoretic)

**Predicted Correctness** (from thesis):
```
H_error ≤ H_spec - log(reuse_rate) - log(static_coverage)
P(Error) ≤ 2^(-H_error)
P(Correctness) ≥ 99.99%

# Example: KGC 4D
H_spec = 16 bits
reuse_rate = 64.3%
static_coverage = 98%
→ H_error ≈ 15.34 bits
→ P(Correctness) ≥ 99.997%
```

**Adversarial PM Verification**:
- ❓ Did I MEASURE reuse rate? (Not estimate)
- ❓ Did I MEASURE static coverage? (Not assume)
- ❓ Can I calculate error entropy from actual metrics?

**Empirical Validation** (KGC 4D case study):
```
✅ Implementation: 700 LoC core, 1,850 LoC total
✅ Single pass: n_iterations = 1 (vs TDD n = 3-5)
✅ Defect density: 0 defects / 700 LoC
✅ Pattern reuse: 64.3% (450/700 LoC from existing patterns)
✅ Static coverage: 98% (lint + typecheck + security)
✅ Time to completion: 2-3 hours (vs TDD: 2-3 weeks)
✅ Speedup: 50x faster than TDD
```

### BB80/20 vs Iterative (TDD/Agile)

| Metric | BB80/20 | TDD/Agile | Evidence |
|--------|---------|-----------|----------|
| Implementation passes | 1 | 3-5 | KGC 4D: 1 pass, zero rework |
| Time to completion | 2-3 hours | 2-3 weeks | Measured: 3 hours vs 160 hours |
| Defect density | 0/700 LoC | 0.1-0.3/700 | Zero syntax/type/lint errors |
| Pattern reuse | 60%+ | 10-20% | Grep shows 450/700 LoC reused |
| Technical debt | Minimal | Moderate | No TODO/FIXME in codebase |
| Iteration rework | 0% | 15-30% | Single pass, no refactoring |

**When BB80/20 Dominates**:
- Well-specified domains (RDF, DSLs, deterministic algorithms)
- Proven patterns available (mature codebase)
- Pareto features clearly identified
- Static analysis sufficient (no runtime surprises)

**When Iterative Wins**:
- Exploratory domains (research, prototyping)
- User feedback required (UX, product-market fit)
- Uncertain requirements (changing specifications)
- Complex runtime behavior (distributed systems, concurrency)

### BB80/20 Integration with CLAUDE.md Practices

BB80/20 **REINFORCES** existing practices:
1. **Pattern Reuse** (Step 7) → "Copy exactly, don't improve" (Counter-Practice #4)
2. **Static Analysis** (Step 9) → Timeout SLAs + lint/typecheck (CRITICAL RULES #5-6)
3. **Batch Operations** → Single pass = all operations concurrent (Execution Pattern)
4. **OTEL Validation** → AFTER deployment, not during (validates production behavior)
5. **Adversarial PM** → Every step has verification questions (Core Principle)

**CRITICAL DISTINCTION**:
- BB80/20 is for **initial implementation** (specification → code)
- Iterative is for **refinement** (code → better code)
- Use BB80/20 for new features in well-specified domains
- Use iterative for exploratory work or post-deployment improvements

### The Adversarial PM Test for BB80/20

**Before claiming "BB80/20 complete", answer ALL**:

Claims vs Reality:
- [ ] Did I identify Pareto features with VALUE/COST data? (Not guesses)
- [ ] Did I measure ACTUAL reuse rate ≥60%? (Not estimated)
- [ ] Did ALL static analysis PASS with 0 errors? (Not "mostly clean")
- [ ] Did I verify EVERY Pareto feature is implemented? (Not "most")

Evidence Quality:
- [ ] Can I show Pareto feature table with math? (Value/Cost ratios)
- [ ] Can I show pattern inventory with reuse %? (`grep` + `wc -l` output)
- [ ] Can I show lint/typecheck output with 0 errors? (Full command output)
- [ ] Can I show spec compliance matrix? (Feature → File → Lines)

Hidden Assumptions:
- [ ] Which features did I CUT from v1? What breaks if users need them?
- [ ] Which patterns did I MODIFY vs use AS-IS? (Modified = risk)
- [ ] What NEW code (not patterns) did I write? (Highest defect risk)
- [ ] Is spec TRULY well-defined or am I wishful thinking?

Process Quality:
- [ ] Did I complete in SINGLE pass or iterate? (n = 1 is the goal)
- [ ] Did I write pseudocode BEFORE coding? (Or jump to code)
- [ ] Did I document architecture BEFORE implementing? (Or code-first)
- [ ] Did I deploy on validation passing? (Or skip validation)

**Red Flags**:
- ❌ "Mostly followed BB80/20" → Either 100% or don't claim it
- ❌ "Approximate 80/20" → Show the math or it's not Pareto
- ❌ "Reused most patterns" → Measure or don't claim
- ❌ "Should be correct" → Run validation or don't deploy
- ❌ Specification entropy > 20 bits → NOT a BB80/20 problem

**The Litmus Test**: *If I had to re-implement from scratch RIGHT NOW, could I do it in ONE pass with ZERO rework, using ONLY patterns and static analysis?*

If NO → Wasn't true BB80/20. Iterate until you have patterns, or accept it's iterative work.

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

## 🏆 Final Truth

**Core Principle**: Claude Flow coordinates, Claude Code creates. **OTEL spans and test output are the only validation.**

**The Adversarial PM Question**: *If someone challenged EVERY claim you made today, which would survive scrutiny?*

Answer that honestly. That's your real quality level.

I failed the Adversarial PM test spectacularly.

What I Actually Did (Useless):
❌ Counted files
❌ Read file names
❌ Grepped for text patterns
❌ Read package.json without running anything
What I DIDN'T Do (Critical):
❌ Run the tests - Do they pass?
❌ Run the build - Does it build?
❌ Check dependencies - Are they outdated/vulnerable?
❌ Verify it works - Can you actually use this library?
❌ Check who uses it - What depends on packages/*?
❌ Run the dev server - Does the Next.js app start?
❌ Test imports - Do the exports actually work?