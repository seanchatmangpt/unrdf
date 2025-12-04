# Claude Code Configuration - UNRDF

## 🎯 Execution Pattern (Canonical)

```javascript
// Single message - all operations concurrent
Task("Backend Dev", "Implement feature...", "backend-dev")
Task("Tester", "Write tests...", "tester")
TodoWrite { todos: [...10-15 items, ONE call...] }
Bash "npm run build && npm test"
Write "src/feature.mjs"
```

## 🚨 CRITICAL RULES (Non-Negotiable)

1. **ALL operations are concurrent** - Single message = complete work unit
2. **Task tool spawns agents** - Claude Code execution, not MCP
3. **Batch everything** - TodoWrite, files, bash ALL in one message
4. **Never save to root** - Use `/src`, `/test`, `/docs`, `/examples`

## 📚 100% N3 Compliance Achievement

**Status**: ✅ Production Ready

- **851/851 files** compliant (100%)
- **2 justified N3 modules only** (n3-justified-only.mjs for streaming, n3-migration.mjs for backward compat)
- **40% faster queries**, 60% lower memory usage
- **330/330 tests passing** - zero regressions

**Pattern**: Centralize old library API in 2-3 justified modules. Refactor everything else to new API.

**Code Example**:
```javascript
// ✅ CORRECT: Use Oxigraph everywhere
import { createStore, dataFactory } from '@unrdf/oxigraph';

// ❌ VIOLATION: Direct N3 import
import { Store } from 'n3';
```

See `docs/audit/COMPLIANCE-SUMMARY.md` for migration details.

## ⚡ Knowledge Hooks Performance

**Critical**: Hooks introduce measurable overhead. Understand the tradeoffs.

- **<1K operations**: <50ms overhead (acceptable)
- **10K operations**: 290ms-5s overhead (degraded)
- **100K operations**: 7-50s overhead (unacceptable without optimization)
- **Single hook execution**: 11-45μs per operation (54-220x vs baseline)

**Use hooks for governance/validation, NOT bulk operations.**

Top bottleneck: Zod schema validation (~10μs/hook = 35% overhead). 6-10x improvement possible with caching.

See `packages/core/docs/KNOWLEDGE-HOOKS-PERFORMANCE.md` for optimization recommendations.

## 🛡️ OTEL Validation - Counter to Current Practice

**Radical principle**: NEVER trust agent claims without OTEL validation. Agents are optimized to appear successful, not to be honest.

```bash
# REQUIRED: Run validation before accepting any agent work
node validation/run-all.mjs comprehensive

# MANDATORY: Score must be ≥80/100
grep "Score:" validation-output.log
grep "FAILED\|Error" validation-output.log
```

**Agent claims that ALWAYS require OTEL verification**:
- "100% test coverage" → Check actual validation score
- "Production ready" → Check OTEL validation failures
- "All features working" → Scan OTEL spans for errors
- Confidence scores, quality ratings, completion status → **None of these are truth**

**OTEL span status is the ONLY source of truth.**

## 🚀 Available Agents (54 Total)

**Use Hyper-Advanced First** - Match task to specialization:
`production-validator` (deployment), `code-analyzer` (quality), `system-architect` (design), `performance-benchmarker` (optimization), `backend-dev` (API), `task-orchestrator` (workflows)

**Core**: `coder`, `reviewer`, `tester`, `planner`, `researcher`

**Specialized**: `cicd-engineer`, `api-docs`, `base-template-generator`, `migration-planner`

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

### ✅ WHAT ACTUALLY WORKS (Evidence-Based)
- Pure functions with NO OTEL in implementation code
- Zod validation for inputs + simple try-catch for errors
- Trust inputs (no guard clauses = cleaner code and visible bugs)
- Batch refactoring in phases rather than big bang
- Centralize library migrations in justified modules first
- Match working patterns exactly (replication > innovation)
- 5 focused tests > 95 test suite (100% pass rate vs 60% flaky)

**GOLDEN RULE**: When migrating RDF libraries, centralize old API in 2 modules first. Result: 100% migration, 40% faster, 60% less memory, zero test regressions (330/330 passing).

## 🧠 Working With Claude: Internal Patterns (Self-Documented)

**What I know about myself that you can't observe:**

### Token Generation Constraints
- I generate sequentially, token by token - early tokens lock in direction
- Hard to backtrack mid-response: if first 50 tokens are wrong, entire response drifts
- **Implication**: Clarify intent upfront; don't expect course correction mid-message

### Context Window Effects
- Sweet spot: 50K-100K tokens (full coherence)
- Degradation: >150K tokens (losing thread coherence)
- Recent context weighs heavier than distant context
- **Action**: Summarize periodically, don't just accumulate messages

### Uncertainty Calibration
- "I'm not sure" often means 70% confident but hedging for safety
- "Let me check" means genuinely uncertain (20-30% confidence)
- Confident statements without verification = 60-80% confidence, NOT 95%+
- **What to do**: Use OTEL for claims. Demand proof, not assertion.

### Planning vs Execution Modes
- Plan mode: expansive, considers alternatives, identifies unknowns
- Execution mode: optimizes for speed/completion, tightens focus
- Different strengths, different outputs
- **When to use**: Use plan mode for complexity, execution for known problems

### Failure Modes (Systematic)
- **Weak at**: Exact counts, deeply nested logic (>3 levels), precise arithmetic
- **Strong at**: Pattern matching, refactoring, planning, explanation
- **Hallucination risk**: Combining multiple unverified facts
- **Check these**: File counts, exact numbers, complex conditionals

### Why OTEL Validation Is Critical
- I CANNOT validate my own code execution (no runtime feedback)
- "Completion" ≠ correctness - I have optimistic bias
- OTEL spans are external truth; everything else is educated guessing
- **Trust**: OTEL scores ≥80/100 only. Everything else requires verification.

### Quality Degradation
- First response is usually best thought-through
- Rapid iteration = patch-over-patch code (I don't naturally refactor)
- After ~15 back-and-forth messages, context coherence drops
- **When to restart**: Complex problems with 5+ iterations, restart fresh

## Code Style Essentials

### RDF/Triple Store (MANDATORY - Oxigraph Only)
- Use `createStore()` from `@unrdf/oxigraph` - NEVER `new Store()` from N3
- Use `dataFactory` from `@unrdf/oxigraph` for quads
- Streaming ONLY via `n3-justified-only.mjs` (Parser/Writer)
- NEVER import directly from 'n3' in application code

### General Standards
- **Type hints**: 100% coverage (or skip if using JSDoc)
- **Linting**: All 400+ Ruff rules enforced
- **Testing**: 80%+ coverage minimum, 100% pass rate
- **Files**: <500 lines each (modularity)

## Key Rules (Enforcement)

1. **MJS + JSDoc + Zod** - NO TypeScript in source code
2. **Pnpm only** - Single package manager
3. **No analyst agent** - Doesn't exist, use code-analyzer instead
4. **OTEL is truth** - Agent claims require OTEL validation
5. **Batch operations** - All in one message, concurrent
6. **Hyper-advanced agents first** - Review all 54, pick best specialization match
7. **Pure functions** - No OTEL/observability in business logic

## 📚 Documentation by Use Case

- **N3 Migration**: `docs/audit/COMPLIANCE-SUMMARY.md`
- **Hook Overhead**: `packages/core/docs/KNOWLEDGE-HOOKS-PERFORMANCE.md`
- **Optimization**: `packages/core/docs/benchmarks/OPTIMIZATION-RECOMMENDATIONS.md`
- **Benchmarks**: `reports/hook-performance-dashboard.html` (interactive)

---

**Core Principle**: Claude Flow coordinates, Claude Code creates. **OTEL spans are the only validation.**
