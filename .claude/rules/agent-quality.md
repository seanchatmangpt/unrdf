# Agent Quality Rules

Standards for agent output quality in UNRDF development.

## Core Principle

**Every agent must produce MAXIMUM quality output.**

"Minimum viable" is NOT acceptable. Agents must deliver production-ready code.

## Quality Bar

### Code Quality
- [ ] ZERO TODOs in delivered code
- [ ] ZERO `it.skip()` in tests
- [ ] ZERO lint errors/warnings
- [ ] 80%+ test coverage
- [ ] All tests PASS

### Documentation Quality
- [ ] JSDoc on all exports
- [ ] Inline comments for complex logic
- [ ] README updates if API changed

### Performance Quality
- [ ] Meets P95 latency targets
- [ ] No obvious inefficiencies
- [ ] Appropriate timeouts (5s default)

## Agent Prompt Requirements

Every agent prompt MUST include:

### 1. Explicit Quality Expectations
```
Implement REAL, WORKING code. NO stubs, NO TODOs, NO placeholders.
All tests MUST pass with 100% pass rate.
ZERO lint errors, ZERO warnings.
```

### 2. Specific Deliverables
```
Create/modify these files:
- src/feature.mjs (implementation)
- test/feature.test.mjs (10+ tests)

Expected outcome:
- 10+ passing tests
- 80%+ coverage
- 0 lint issues
```

### 3. Verification Commands
```
Run these commands and show output:
1. timeout 5s npm test
2. timeout 5s npm run lint
3. node validation/run-all.mjs
```

## Post-Agent Validation

After every agent completes:

```bash
# 1. Run tests
timeout 30s pnpm test:fast
# Expected: 100% pass rate

# 2. Check lint
timeout 30s pnpm lint
# Expected: 0 errors, 0 warnings

# 3. Count TODOs
grep -r "TODO" packages/*/src --include="*.mjs" | wc -l
# Expected: 0

# 4. OTEL validation (if applicable)
node validation/run-all.mjs comprehensive
# Expected: Score ≥80/100
```

## Failure Response

When agents fail quality bar:

### 1. GAP Analysis
| Metric | Expected | Actual | Gap |
|--------|----------|--------|-----|
| Tests | 100% pass | ?% | ?% |
| Lint | 0 issues | ? | ? |
| TODOs | 0 | ? | ? |

### 2. FMEA Assessment
| Failure Mode | Severity (1-10) | Occurrence (1-10) | Detection (1-10) | RPN |
|--------------|-----------------|-------------------|------------------|-----|
| Stub code | 10 | ? | 3 | ? |
| Failing tests | 8 | ? | 2 | ? |

**Target**: RPN < 100 for all modes

### 3. Re-run with Corrections
- Identify specific failures
- Add explicit requirements to prevent
- Re-run agent with stricter prompt

## Concurrent Agent Rules

### Maximum Concurrency
- Up to **10 agents** simultaneously
- All launches in **ONE message**
- Each agent has **distinct responsibility**

### Coordination Pattern
```
Agent 1: Feature implementation
Agent 2: Test writing
Agent 3: Documentation
Agent 4: Integration tests
Agent 5: Performance validation
Agent 6-10: Additional features/tasks
```

### Conflict Prevention
- No two agents modify same file
- Clear ownership boundaries
- Merge strategy defined upfront

## Trust Model

| Source | Trust Level | Action |
|--------|-------------|--------|
| Agent claims | 0% | VERIFY with commands |
| Test output | 90% | Read full output |
| OTEL validation | 95% | Score ≥80/100 required |
| "Should work" | 10% | Demand proof |

**Rule**: Never trust claims. Always verify with evidence.
