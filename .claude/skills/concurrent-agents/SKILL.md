# Concurrent Agents Skill

Expert knowledge for running 10 concurrent Claude Code agents with MAXIMUM quality output.

## Activation

Use this skill when:
- Running multiple agents in parallel
- Coordinating complex multi-agent tasks
- Ensuring quality output from agent swarms

## Core Principle

**GOLDEN RULE**: 1 MESSAGE = ALL OPERATIONS

All agent launches MUST be in a single message with multiple Task tool invocations.

## Agent Launch Pattern

```javascript
// Launch 10 concurrent agents in ONE message
Task("Agent 1 - Feature X", "Implement feature X with REAL code...", "backend-dev")
Task("Agent 2 - Tests", "Write comprehensive tests...", "tester")
Task("Agent 3 - Validation", "Validate all implementations...", "production-validator")
// ... up to 10 agents
```

## Quality Requirements

### MAXIMUM Quality (Not Minimum!)

Each agent prompt MUST include:

1. **Explicit Quality Bar**
   - "Implement REAL, WORKING code - NO stubs"
   - "All tests must PASS - no skipped tests"
   - "ZERO lint errors, ZERO warnings"

2. **Specific Deliverables**
   - File paths to create/modify
   - Expected test count and pass rate
   - Performance targets

3. **Verification Steps**
   - "Run `npm test` and show output"
   - "Run `npm run lint` and fix ALL issues"
   - "Verify with OTEL validation ≥80/100"

## Agent Selection

| Task Type | Agent | Quality Metric |
|-----------|-------|----------------|
| Feature Implementation | backend-dev | Working code, tests pass |
| Test Writing | tester | Coverage ≥80%, all pass |
| Validation | production-validator | OTEL ≥80/100 |
| Architecture | system-architect | Design docs, patterns |
| Code Review | code-analyzer | 0 issues found |
| Performance | performance-benchmarker | Meets P95 targets |

## Anti-Patterns (NEVER DO)

1. **Stub Code**: "TODO: implement later" - FORBIDDEN
2. **Skipped Tests**: `it.skip()` - FORBIDDEN
3. **Ignored Lint**: `// eslint-disable` without justification - FORBIDDEN
4. **Vague Prompts**: "Fix the issues" - FORBIDDEN

## Post-Launch Validation

After all agents complete:

```bash
# Verify ALL tests pass
timeout 30s pnpm test:fast

# Verify ZERO lint issues
timeout 30s pnpm lint

# Count TODOs (should be 0)
grep -r "TODO" packages/*/src --include="*.mjs" | wc -l
```

## GAP Analysis Template

When agents underperform:

| Metric | Expected | Actual | Gap |
|--------|----------|--------|-----|
| Test Pass Rate | 100% | ?% | ?% |
| Lint Errors | 0 | ? | ? |
| TODOs Remaining | 0 | ? | ? |
| Coverage | 80%+ | ?% | ?% |

## FMEA Risk Assessment

| Failure Mode | Severity | Occurrence | Detection | RPN |
|--------------|----------|------------|-----------|-----|
| Stub code | 10 | ? | 3 | ? |
| Failing tests | 8 | ? | 2 | ? |
| Lint errors | 5 | ? | 1 | ? |

Target: RPN < 100 for all failure modes
