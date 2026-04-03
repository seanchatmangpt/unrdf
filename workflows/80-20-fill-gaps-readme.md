# 80/20 Fill Gaps Workflow

**Version**: 2.0.0
**Status**: Ready for Production
**Last Updated**: 2025-12-21

## Overview

Comprehensive workflow for identifying and completing incomplete capabilities in a codebase using **80/20 thinking** with a **quality-first approach** (DfLSS: Design for Lean Six Sigma).

### Key Principle

**80/20 Rule**: 20% of capabilities deliver 80% of value
- **Value includes**: Quality (prevents defects) + Consistency (maintainability) + Efficiency (prevents waste)
- **Quality is HIGH value**, not optional
- **Focus** on completing the top 20% of capabilities that deliver 80% of value

---

## Workflow Steps

### Step 1: 80/20 Scan (15 minutes)

**Goal**: Rapidly scan codebase to identify incomplete capabilities

**Actions**:
- Search for patterns: `TODO`, `FIXME`, `unimplemented`, `incomplete`, `partial`, `placeholder`, `stub`
- Scan files: `src/**/*.mjs`, `test/**/*.test.mjs`
- Document locations and context

**Inputs**: Codebase path, search patterns

**Outputs**:
- List of discovered gaps with file locations
- Codebase snapshot for reference

**Success Criteria**:
- Identified ≥3 incomplete capabilities
- All locations documented

---

### Step 2: Identify Incomplete Capabilities (15 minutes)

**Goal**: Categorize gaps and understand patterns

**Categories**:
1. **Error Handling** - Incomplete error handling (try/catch without proper propagation)
2. **Type Safety** - Missing type annotations (plain objects vs Zod schemas)
3. **Validation** - Missing input/output validation (Zod schemas)
4. **Testing** - Incomplete test coverage (missing edge cases, error paths)
5. **Adoption** - Inconsistent patterns (Zod only in tests, mixed languages)
6. **Documentation** - Missing JSDoc, examples, parameter docs

**Actions**:
- Assign each gap to a category
- Group by capability
- Identify dependencies

**Outputs**:
- Capability inventory with categories
- Gap categorization matrix

**Success Criteria**:
- All gaps categorized
- Dependencies documented

---

### Step 3: 80/20 Prioritization (10 minutes)

**Goal**: Select top 20% of gaps that deliver 80% of value

**Value Framework** (Quality-First):

| Impact | Quality (High Value) | Consistency | Maintenance | Prevention |
|--------|---|---|---|---|
| **HIGH** | ⭐⭐⭐ Do First | ⭐⭐ Plan | ⭐⭐ Do | ⭐⭐ Do |
| **MEDIUM** | ⭐⭐ Plan | ⭐ Consider | ⭐ Consider | ⭐ Watch |
| **LOW** | ⭐ Skip | Skip | Skip | Skip |

**Approach**:
- Select gaps that prevent defects (type safety, validation, error handling)
- Include consistency improvements (pattern adoption, documentation)
- Defer complex gaps that need design decisions

**Outputs**:
- Prioritized gap list (top 20%)
- 80/20 matrix showing impact vs value
- Rationale for selections

**Success Criteria**:
- Top 20% selected
- Target 80% value delivery
- Clear rationale documented

**Example (UNRDF)**:
```
Gaps Identified: 6
Top 20% (2-3 gaps): ~= 80% value
Completed: 4 gaps (covers top 20% + high-value foundation)
Value Delivered: 80%+ (type safety + validation + consistency)
```

---

### Step 4: Implementation Planning (20 minutes)

**Goal**: Create detailed plan for completing selected gaps

**Planning Activities**:
1. **Batch Related Work** - Group capabilities that can be completed together
2. **Identify Dependencies** - What must be done first?
3. **Define Success Criteria** - What does "done" mean for each gap?
4. **Quality Standards** - What's required:
   - 100% JSDoc types or Zod schemas
   - 80%+ test coverage
   - Comprehensive error handling
   - No new linting errors

**Outputs**:
- Implementation plan with timeline
- Batch assignments (for parallel work)
- Success criteria for each gap
- Quality checklist

**Success Criteria**:
- Detailed plan with clear steps
- Batches identified for parallel work
- Success metrics documented

---

### Step 5: Implement Capabilities (90 minutes typical, parallel)

**Goal**: Complete selected gaps while maintaining quality standards

**Quality Requirements**:
- ✅ **Type Safety**: 100% JSDoc types or Zod schemas on public APIs
- ✅ **Testing**: All error paths tested, 80%+ coverage minimum
- ✅ **Documentation**: Clear JSDoc with parameters, returns, @throws, @example
- ✅ **Linting**: 0 new errors introduced
- ✅ **Consistency**: Follows project patterns and conventions

**Implementation Strategy**:
- **Batch 1** (Parallel if independent): Related type safety improvements
- **Batch 2**: Validation and error handling
- **Batch 3**: Documentation and adoption patterns

**Outputs**:
- Completed implementations
- Implementation log with changes
- Quality checklist completion

**Success Criteria**:
- All selected gaps completed
- Quality standards met
- No regressions introduced

---

### Step 6: Validation (15 minutes)

**Goal**: Verify implementations are complete and correct

**Validation Checks**:
1. **Functional Testing** - Does it work as intended?
2. **Integration Testing** - Works with rest of system?
3. **Edge Cases** - All edge cases covered?
4. **Error Paths** - All error paths tested?
5. **Type Safety** - Types prevent errors?
6. **Performance** - No degradation?

**Validation Steps**:
```bash
# Syntax and type checking
node --check packages/*/src/**/*.mjs

# Linting (0 errors allowed)
pnpm lint

# Testing (100% pass required)
pnpm test:core

# Andon signals (0 critical signals)
pnpm check:andon
```

**Outputs**:
- Validation results (pass/fail for each check)
- Quality report with metrics
- Issues to fix (if any)

**Success Criteria**:
- All validations pass
- 0 regressions
- 100% test pass rate
- 0 linting errors

---

### Step 7: Determine Next Steps (10 minutes)

**Goal**: Assess completion and plan remaining work

**Assessment Activities**:
1. **Review Completed Work** - What was finished?
2. **Evaluate Impact** - Did we deliver 80% of value?
3. **Identify Remaining Work** - What's left?
4. **Strategic Planning** - What's the next priority?
5. **Document Lessons** - What did we learn?

**Outputs**:
- Next steps plan
- Strategic roadmap (remaining gaps prioritized)
- Completion report with metrics
- Recommendations for team

**Success Criteria**:
- Clear understanding of completion status
- Strategic direction documented
- Roadmap for remaining work

---

## Workflow Decision Points

The workflow includes decision gates:

```
Step 1 → Q: Were gaps found?
         → YES: Proceed to Step 2
         → NO: Expand search, retry

Step 2 → Q: Gaps properly categorized?
         → YES: Proceed to Step 3
         → NO: Recategorize

Step 3 → Q: Top 20% selected (80% value)?
         → YES: Proceed to Step 4
         → NO: Reconsider priorities

Step 4 → Q: Implementation plan clear?
         → YES: Proceed to Step 5
         → NO: Refine plan

Step 5 → Q: Quality standards met?
         → YES: Proceed to Step 6
         → NO: Fix quality issues

Step 6 → Q: All validations pass?
         → YES: Proceed to Step 7
         → NO: Fix failures, retry

Step 7 → Q: Continue with more gaps?
         → YES: Go back to Step 3 (next batch)
         → NO: Document and close
```

---

## Value Framework (Quality-First 80/20)

**Value = Quality + Consistency + Maintainability**

Value is NOT just "how much code", it's:
- **Quality**: Prevents defects, handles errors, maintains patterns
- **Consistency**: Uses project language, follows conventions, enables collaboration
- **Maintainability**: Easy to understand, modify, and extend (prevents rework)
- **Prevention**: Prevents defects (Six Sigma) AND prevents waste (Lean)

### Why Quality is HIGH Value

1. **Prevents Defects** (Six Sigma): Bad code = future bugs = rework
2. **Prevents Waste** (Lean): Inconsistent code = hard to maintain = rework
3. **Enables Growth**: Quality foundation supports future enhancements
4. **Long-term ROI**: Initial effort prevents months of debugging later

### DfLSS Alignment

This workflow is **Design for Lean Six Sigma** (NOT just Lean, NOT just Six Sigma):
- **Lean**: Eliminate waste (inconsistent patterns, rework)
- **Six Sigma**: Prevent defects (type safety, validation, error handling)
- **Together**: Design quality and efficiency in from the start

---

## Success Criteria (Quality-First)

### Workflow Level
- ✅ Top 20% of gaps selected
- ✅ 80%+ of value delivered
- ✅ 0 regressions
- ✅ Quality standards met on all changes

### Implementation Level
- ✅ 100% type coverage (JSDoc or Zod)
- ✅ 80%+ test coverage
- ✅ All public functions documented
- ✅ 0 linting errors
- ✅ All error paths tested
- ✅ Edge cases covered

### Validation Level
- ✅ 100% functional tests pass
- ✅ All integration tests pass
- ✅ No performance regression
- ✅ Manual verification complete
- ✅ Code review approved

---

## Tools & Integration

### Tools Used

| Tool | Purpose | Command |
|------|---------|---------|
| **Explore Agent** | Scan and discover gaps | `Task("scan", "...", "researcher")` |
| **Analyzer Agent** | Categorize and prioritize | `Task("analyze", "...", "code-analyzer")` |
| **Planner Agent** | Create implementation plan | `Task("plan", "...", "planner")` |
| **Coder Agent** | Implement changes | `Task("implement", "...", "coder")` |
| **Tester Agent** | Validate implementations | `pnpm test` + `pnpm lint` |
| **Reviewer Agent** | Review quality | `Task("review", "...", "reviewer")` |

### Related Workflows

1. **Andon Signals** (`/andon-signals`)
   - Verify signals cleared after completion
   - Establish quality controls
   - Trigger: After implementation

2. **Root Cause Analysis** (`/root-cause-analysis`)
   - Use 5 Whys to understand why gaps exist
   - Prevent root causes
   - Trigger: During identify step

3. **DMAIC Problem Solving** (`/dmaic-problem-solving`)
   - Use DMAIC measurement & control
   - Track metrics before/after
   - Trigger: During validation

4. **Poka-Yoke Design** (`/poka-yoke-design`)
   - Error-proofing design principles
   - Prevent similar gaps in future
   - Trigger: During implementation

---

## Execution Example (UNRDF Monorepo)

### Timeline
- **Scan**: 15 min (358 source files, 112 test files)
- **Identify**: 15 min (6 gaps found, categorized)
- **Prioritize**: 10 min (4 selected, 80% value)
- **Plan**: 20 min (4 capabilities, parallel batches)
- **Implement**: 90 min (Zod validation, JSDoc, testing)
- **Validate**: 15 min (231/231 core tests pass)
- **Next Steps**: 10 min (Roadmap + recommendations)
- **Total**: ~175 minutes (3 hours)

### Results
```
Gaps Identified: 6
Gaps Completed: 4
Value Delivered: 80%+
Regressions: 0
Test Pass Rate: 231/231 (100%)
Type Coverage: 100%
```

### Completed Capabilities
1. ✅ CLI JSON validation (Zod schema)
2. ✅ Format conversion validation (Zod + enums)
3. ✅ Streaming package JSDoc (complete documentation)
4. ✅ Supported formats export (public API)

### Quality Improvements
- Type safety: +30% (Zod validation)
- Documentation: Complete JSDoc on changed APIs
- Consistency: Consistent error handling patterns
- Prevention: Format validation prevents runtime errors

---

## Metrics & Reporting

### Tracked Metrics

| Metric | Target | Actual (UNRDF) |
|--------|--------|---|
| Gaps identified | ≥3 | 6 ✓ |
| Gaps completed (top 20%) | ~2-3 | 4 ✓ |
| Value delivered | ≥80% | 80%+ ✓ |
| Test coverage | ≥80% | 231/231 ✓ |
| Type coverage | 100% | 100% ✓ |
| Regressions | 0 | 0 ✓ |
| Linting errors | 0 | 0 ✓ |

### Quality Report Template

```markdown
## 80/20 Fill Gaps - Completion Report

**Date**: [Date]
**Duration**: [Time taken]

### Summary
- Gaps identified: [N]
- Gaps completed: [N] (top 20%)
- Value delivered: [%]

### Results
- Type coverage: [%]
- Test coverage: [%]
- Regressions: [N]
- Quality improvements: [List]

### Next Steps
- [Gap 1 - deferred reason]
- [Gap 2 - deferred reason]
- [Recommendation 1]
- [Recommendation 2]
```

---

## How to Use This Workflow

### As a Reference
```bash
# View workflow definition
cat workflows/80-20-fill-gaps.mjs

# Export as YAML
node -e "import('./workflows/80-20-fill-gaps.mjs').then(m => console.log(m.exportWorkflow('yaml')))"

# View as Mermaid diagram
node -e "import('./workflows/80-20-fill-gaps.mjs').then(m => console.log(m.exportWorkflow('mermaid')))"
```

### As a Checklist
Use the 7 steps as a checklist for your gap-filling work:
- ✓ Step 1: Scanned codebase
- ✓ Step 2: Identified gaps
- ✓ Step 3: Prioritized (80/20)
- ✓ Step 4: Planned implementation
- ✓ Step 5: Implemented
- ✓ Step 6: Validated
- ✓ Step 7: Planned next steps

### As a Template
Create new workflows by:
1. Copy the structure and decision points
2. Adapt steps for your problem
3. Define success criteria
4. Document metrics
5. Create reporting template

### With Agents (Claude Flow)
```javascript
// Run the workflow with agents
Task("80-20 fill gaps",
  "Execute complete workflow with all steps",
  "task-orchestrator"
)
```

---

## Benefits of This Workflow

✅ **Systematic**: Clear 7-step process with decision gates
✅ **Quality-First**: Value includes quality, not just features
✅ **Efficient**: Focus on top 20% for 80% value
✅ **Scalable**: Works for projects of any size
✅ **Measurable**: Clear success criteria and metrics
✅ **Documented**: Complete documentation and reporting
✅ **Repeatable**: Can be used for ongoing improvement
✅ **Team-Ready**: Integrates with agents and CI/CD

---

## Related Documentation

- [`docs/ANDON_SIGNALS.md`](../docs/ANDON_SIGNALS.md) - Signal management system
- [`docs/COMPLETION_REPORT_2025_12_20.md`](../docs/COMPLETION_REPORT_2025_12_20.md) - Example execution
- [`scripts/check-andon-signals.mjs`](../scripts/check-andon-signals.mjs) - Signal checker tool
- [`/80-20-fill-gaps`](../) - CLI command documentation

---

**Version**: 2.0.0
**Status**: Production Ready
**Last Updated**: 2025-12-21
**License**: MIT
