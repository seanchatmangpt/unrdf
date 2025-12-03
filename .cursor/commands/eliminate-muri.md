# Eliminate Muri (Overburden) - Multi-Step Workflow

## Purpose

This command guides agents through identifying and eliminating overburden (Muri) in code. Muri refers to unreasonable strain or overburden on systems, people, or processes. Experts recognize overburden early and reduce it systematically to maintain system health and developer productivity.

## Workflow Overview

```
Step 1: Identify Muri → Step 2: Measure Strain Impact → Step 3: Reduce Overburden → Step 4: Verify System Health → Step 5: Control
```

## Step-by-Step Instructions

### Step 1: Identify Muri (Overburden)

**Action**: Scan code for unreasonable strain and overburden.

**Types of Muri to identify**:

1. **Over-complexity** - Code that's unnecessarily complex
   - Deeply nested conditionals
   - Over-abstraction
   - Complex algorithms for simple problems
   - High cyclomatic complexity

2. **Unreasonable performance requirements** - Requirements that strain the system
   - Unrealistic latency targets
   - Excessive resource consumption
   - Over-optimization for edge cases
   - Premature performance tuning

3. **Over-engineering** - Solutions more complex than needed
   - Enterprise patterns for simple problems
   - Unnecessary design patterns
   - Over-architected solutions
   - YAGNI violations (You Aren't Gonna Need It)

4. **Tight coupling** - Systems too interdependent
   - Circular dependencies
   - Hard dependencies between modules
   - Global state dependencies
   - Monolithic structures

5. **Cognitive overload** - Code too difficult to understand
   - Functions with too many responsibilities
   - Long functions (>50 lines)
   - Unclear naming
   - Missing documentation for complex logic

6. **Resource strain** - Systems consuming too many resources
   - Memory leaks
   - CPU-intensive operations in hot paths
   - Excessive network calls
   - Large bundle sizes

7. **Unnecessary constraints** - Constraints that don't add value
   - Overly strict type constraints
   - Unnecessary validation layers
   - Artificial limitations
   - Over-restrictive policies

**Action**: Create overburden inventory list

```markdown
## Muri Inventory

### Over-complexity

- [ ] `src/parser.mjs:45` - Function with cyclomatic complexity 15 (threshold: 10)
- [ ] `src/validator.mjs:123` - 8 levels of nested conditionals

### Unreasonable Performance Requirements

- [ ] `src/api.mjs:67` - Requires <1ms latency for non-critical path
- [ ] `src/query.mjs:89` - Optimized for 0.01% edge case

### Over-engineering

- [ ] `src/simple.mjs` - Uses enterprise pattern for 10-line function
- [ ] `src/utils.mjs:34` - Factory pattern for single-use case

### Tight Coupling

- [ ] `src/core.mjs` ↔ `src/utils.mjs` - Circular dependency
- [ ] `src/api.mjs` - Hard dependency on 15 modules

### Cognitive Overload

- [ ] `src/processor.mjs:200` - Function is 200 lines long
- [ ] `src/transform.mjs:45` - Function has 12 parameters

### Resource Strain

- [ ] `src/cache.mjs:123` - Memory leak in cache eviction
- [ ] `src/query.mjs:67` - N+1 query problem

### Unnecessary Constraints

- [ ] `src/validator.mjs:45` - Overly strict type validation
- [ ] `src/api.mjs:89` - Unnecessary rate limiting
```

---

### Step 2: Measure Strain Impact

**Action**: Quantify the impact of each overburden item.

**Metrics to measure**:

- **Complexity metrics** - Cyclomatic complexity, nesting depth
- **Performance impact** - Latency, resource consumption
- **Maintenance cost** - Time to understand, modify, test
- **Coupling metrics** - Dependency count, circular dependencies
- **Cognitive load** - Function length, parameter count, readability score
- **Resource usage** - Memory, CPU, network, bundle size

**Action**: Prioritize overburden reduction

**Priority order**:

1. **High impact, low effort** - Quick wins (reduce complexity, fix leaks)
2. **High impact, high effort** - Strategic improvements (decouple, refactor)
3. **Low impact, low effort** - Cleanup tasks (rename, document)
4. **Low impact, high effort** - Defer or eliminate

**Example prioritization**:

```markdown
## Overburden Prioritization

### High Impact, Low Effort (Do First)

- Fix memory leak (`src/cache.mjs`) - Immediate performance impact
- Reduce function length (`src/processor.mjs`) - Improves readability
- Remove circular dependency - Reduces coupling

### High Impact, High Effort (Plan)

- Refactor over-complex parser - Reduces maintenance cost
- Decouple tightly coupled modules - Improves testability

### Low Impact, Low Effort (Cleanup)

- Rename unclear variables - Improves readability
- Add documentation for complex logic - Reduces cognitive load

### Low Impact, High Effort (Defer)

- Refactor enterprise pattern - Works for now, low priority
```

---

### Step 3: Reduce Overburden

**Action**: Systematically reduce overburden, starting with high-priority items.

#### 3.1: Reduce Complexity (Over-complexity Muri)

**Action**: Simplify complex code.

**Steps**:

1. Identify complex code (high cyclomatic complexity, deep nesting)
2. Extract functions to reduce complexity
3. Replace complex conditionals with guard clauses or early returns
4. Use simpler algorithms when possible
5. Verify functionality: `pnpm test`
6. Verify complexity reduced: Measure cyclomatic complexity

**Verification**: Tests pass, complexity reduced (cyclomatic complexity < 10)

#### 3.2: Simplify Architecture (Over-engineering Muri)

**Action**: Remove unnecessary complexity from architecture.

**Steps**:

1. Identify over-engineered solutions
2. Replace with simpler alternatives
3. Remove unnecessary design patterns
4. Apply YAGNI principle
5. Verify functionality: `pnpm test`
6. Verify architecture simpler: Fewer classes/modules

**Verification**: Tests pass, code simpler, fewer abstractions

#### 3.3: Reduce Coupling (Tight Coupling Muri)

**Action**: Loosen dependencies between modules.

**Steps**:

1. Identify tight coupling (circular dependencies, hard dependencies)
2. Introduce interfaces/abstractions
3. Use dependency injection
4. Break circular dependencies
5. Verify functionality: `pnpm test`
6. Verify coupling reduced: No circular dependencies

**Verification**: Tests pass, no circular dependencies, modules more independent

#### 3.4: Reduce Cognitive Load (Cognitive Overload Muri)

**Action**: Make code easier to understand.

**Steps**:

1. Identify cognitive overload (long functions, many parameters, unclear names)
2. Extract functions to reduce length
3. Reduce parameter count (use objects for >3 parameters)
4. Improve naming clarity
5. Add documentation for complex logic
6. Verify functionality: `pnpm test`
7. Verify readability improved: Functions <50 lines, clear names

**Verification**: Tests pass, code more readable, functions shorter

#### 3.5: Optimize Resource Usage (Resource Strain Muri)

**Action**: Reduce resource consumption.

**Steps**:

1. Identify resource strain (memory leaks, excessive CPU, large bundles)
2. Fix memory leaks
3. Optimize hot paths
4. Reduce bundle size (code splitting, tree shaking)
5. Optimize network calls (batching, caching)
6. Verify functionality: `pnpm test`
7. Verify performance improved: Measure resource usage

**Verification**: Tests pass, resource usage reduced, performance improved

#### 3.6: Remove Unnecessary Constraints (Unnecessary Constraints Muri)

**Action**: Remove constraints that don't add value.

**Steps**:

1. Identify unnecessary constraints
2. Evaluate if constraint adds value
3. Remove or relax constraints that don't add value
4. Keep constraints that prevent defects (see [Poka-Yoke Design](./poka-yoke-design.md))
5. Verify functionality: `pnpm test`
6. Verify constraints appropriate: Only valuable constraints remain

**Verification**: Tests pass, unnecessary constraints removed

---

### Step 4: Verify System Health

**Action**: Ensure overburden reduction maintained functionality and improved system health.

**Verification checklist**:

- ✅ All tests pass: `pnpm test`
- ✅ Code compiles: `pnpm lint`
- ✅ Functionality preserved (tests verify behavior)
- ✅ Complexity reduced (lower cyclomatic complexity, less nesting)
- ✅ Coupling reduced (no circular dependencies, fewer hard dependencies)
- ✅ Readability improved (shorter functions, clearer names)
- ✅ Performance maintained or improved (resource usage reduced)
- ✅ Bundle size reduced (if applicable)

**If verification fails**:

- Revert changes that broke functionality
- Re-analyze overburden (may have removed necessary complexity)
- Return to Step 1, refine overburden identification

**If verification succeeds**:

- Proceed to Step 5 (Control)

---

### Step 5: Control (Prevent Overburden from Returning)

**CRITICAL**: Do NOT write documents or reports. Create todos and execute controls.

**Action**: Create 10+ item todo list for implementing controls to prevent overburden.

**Control methods** (implement as todos):

- **Complexity limits** - Enforce maximum complexity thresholds
- **Code review** - Add checklist items to prevent overburden
- **Inline comments** - Add comments explaining complexity decisions (in code, not separate doc)
- **Standards** - Implement overburden prevention patterns
- **Monitoring** - Track complexity, coupling, resource usage

**Example control todo list**:

```markdown
## Overburden Prevention Control Todos (10+ items)

**Complexity Controls**:

- [ ] Add complexity check: Maximum cyclomatic complexity 10
- [ ] Add complexity check: Maximum nesting depth 4
- [ ] Add complexity check to CI pipeline
- [ ] Verify complexity checks catch over-complex code

**Coupling Controls**:

- [ ] Add dependency check: No circular dependencies
- [ ] Add dependency check: Maximum dependency count per module
- [ ] Add dependency check to CI pipeline
- [ ] Verify dependency checks catch tight coupling

**Readability Controls**:

- [ ] Add length check: Maximum function length 50 lines
- [ ] Add parameter check: Maximum 3 parameters (use objects for more)
- [ ] Add naming check: Clear, descriptive names
- [ ] Add readability checks to CI pipeline

**Code Review Controls**:

- [ ] Add checklist item: Complexity within limits
- [ ] Add checklist item: No circular dependencies
- [ ] Add checklist item: Functions <50 lines
- [ ] Add checklist item: No over-engineering (YAGNI)
- [ ] Update code review process to include checklist

**Inline Documentation Controls**:

- [ ] Add inline comment: Document why complexity is necessary (if >10)
- [ ] Add inline comment: Document coupling decisions
- [ ] Add inline comment: Document resource usage considerations
- [ ] Verify comments are clear and helpful

**Standards Controls**:

- [ ] Add standard: Keep functions <50 lines
- [ ] Add standard: Maximum 3 parameters per function
- [ ] Add standard: Maximum cyclomatic complexity 10
- [ ] Add standard: No circular dependencies
- [ ] Add standard: YAGNI - avoid over-engineering

**Monitoring Controls**:

- [ ] Set up complexity monitoring in CI
- [ ] Set up dependency analysis in CI
- [ ] Configure alerts for overburden accumulation
- [ ] Review overburden regularly during code review

**Verification**:

- [ ] Verify controls work: Checks catch overburden patterns
- [ ] Verify no regressions: All tests still pass
- [ ] Verify controls are sustainable: Process is repeatable
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls, don't document them separately. Todos track progress, controls prevent overburden accumulation.

---

## Complete Workflow Example

```bash
# Step 1: Identify Muri
# Found: Function with cyclomatic complexity 15, circular dependency

# Step 2: Measure Impact
# High complexity: 15 (threshold: 10) - High impact, medium effort
# Circular dependency: Blocks testing - High impact, low effort

# Step 3: Reduce Overburden
# Extract functions to reduce complexity
# Break circular dependency
pnpm test   # Verify functionality

# Step 4: Verify System Health
pnpm test   # All tests pass ✅
pnpm lint  # Compiles ✅
# Complexity reduced to 8 ✅
# No circular dependencies ✅

# Step 5: Control
# Add complexity check to CI
# Add dependency check to CI
# Document complexity limits
```

## Integration with Other Commands

- **[Gemba Walk](./gemba-walk.md)** - Go to source to verify overburden identification
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Prevent defects while reducing overburden
- **[Kaizen Improvement](./kaizen-improvement.md)** - Make small improvements to reduce overburden incrementally
- **[Eliminate Muda](./eliminate-muda.md)** - Remove waste while reducing overburden
- **[Eliminate Mura](./eliminate-mura.md)** - Standardize patterns to prevent overburden
- **[Root Cause Analysis](./root-cause-analysis.md)** - Understand why overburden exists

## Expert Insights

**Why this matters**: Overburden accumulates silently and reduces productivity. Experts recognize overburden early and reduce it systematically. Most developers don't see overburden until it's overwhelming.

**Key principle**: "Simplicity is the ultimate sophistication" - The best code is simple, clear, and maintainable. Overburden makes code harder to understand, modify, and test.

**Remember**: Overburden reduction is continuous (Kaizen), not one-time. Regular overburden reduction prevents technical debt accumulation and maintains system health.

**Muri vs Muda vs Mura**:
- **Muda (Waste)**: Things that don't add value - eliminate them
- **Mura (Unevenness)**: Inconsistency - standardize them
- **Muri (Overburden)**: Unreasonable strain - reduce them

All three must be addressed together for optimal system health.

**DfLSS alignment**: Eliminating Muri (overburden) supports DfLSS (Design for Lean Six Sigma) by ensuring both efficiency (reduced complexity improves performance) AND quality (simpler code has fewer defects). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When reducing overburden, ensure both efficiency and quality are maintained. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Eliminate Muri commands must:

1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Reduce overburden, not document it
3. **Verify fixes** - Test that overburden reduction works
4. **Complete todos** - Mark todos as done as overburden is reduced

**Principle**: Reduce overburden, don't document it. Todos track progress, overburden reduction improves system health.

---






