# Kaizen (Continuous Improvement) - Multi-Step Workflow

## Purpose

This command guides agents to make small, incremental improvements rather than big rewrites. Kaizen means "change for better" - continuous small improvements that compound over time. Experts make many small improvements rather than waiting for perfect solutions.

## Workflow Overview

```
Step 1: Identify Opportunity → Step 2: Plan Change (with Success Criteria & Measurement) → Step 3: Do (Implement) → Step 4: Check (Verify with Measurement) → Step 5: Act (Standardize with Control)
```

## Step-by-Step Instructions

### Step 1: Identify Improvement Opportunity

**Action**: Find a small, focused improvement opportunity.

**Opportunity criteria**:

- **Small**: Can be done in minutes, not hours
- **Focused**: Addresses one specific thing
- **Safe**: Low risk of breaking things
- **Value**: Adds value (clarity, performance, maintainability)

**Types of opportunities**:

1. **Code clarity** - Make code more readable
2. **Performance** - Small performance improvement
3. **Maintainability** - Easier to maintain
4. **Error prevention** - Prevent a class of errors
5. **Consistency** - Match existing patterns

**Action**: List improvement opportunities

```markdown
## Kaizen Opportunities

### Code Clarity

- [ ] Extract magic number to named constant
- [ ] Add clarifying comment
- [ ] Rename variable for clarity

### Performance

- [ ] Use reference instead of clone
- [ ] Remove unnecessary allocation
- [ ] Optimize hot path

### Maintainability

- [ ] Extract repeated pattern to function
- [ ] Simplify complex expression
- [ ] Remove dead code

### Error Prevention

- [ ] Add type safety (see [Poka-Yoke Design](./poka-yoke-design.md))
- [ ] Add validation
- [ ] Handle edge case

### Consistency

- [ ] Match naming convention
- [ ] Match code style
- [ ] Match error handling pattern
```

**Principle**: "Small improvements, continuously" - Don't wait for perfect. Make small improvements now.

---

### Step 2: Plan Change

**Action**: Design minimal change that improves code.

#### 2.1: Define Improvement

**Action**: Clearly define what will improve.

**Improvement statement**:

- **What**: What will change?
- **Why**: Why is this improvement valuable?
- **How**: How will it be implemented?
- **Risk**: What could go wrong?

**Example improvement statement**:

```markdown
## Improvement Plan

**What**: Extract magic number `42` to named constant `DEFAULT_TIMEOUT_SECONDS`
**Why**: Makes code more readable, easier to change, self-documenting
**How**:

1. Add constant: `const DEFAULT_TIMEOUT_SECONDS: u64 = 42;`
2. Replace `42` with `DEFAULT_TIMEOUT_SECONDS`
   **Risk**: Low - simple refactoring, no logic change
```

#### 2.2: Define Success Criteria (DMAIC Measurement)

**Action**: Define measurable success criteria for the improvement.

**Success criteria format**:

- **Measurable**: Can be quantified
- **Achievable**: Realistic to achieve
- **Specific**: Clear what success looks like

**Example success criteria**:

```markdown
## Success Criteria

**Primary**:

- Code readability improved: Magic number replaced with named constant
- Maintainability improved: Constant can be changed in one place
- Self-documenting: Constant name explains what the value represents

**Measurable**:

- Magic number count: 1 → 0 (100% reduction)
- Named constants: 0 → 1 (added)
- Code clarity: Improved (subjective, but verifiable through review)

**Verification**:

- Code compiles: `pnpm lint`
- Tests pass: `pnpm test`
- Code review: Improvement approved
```

#### 2.3: Collect Baseline Data (DMAIC Measurement)

**Action**: Measure current state before improvement.

**Data to collect**:

- **Current state**: What is the current state?
- **Metrics**: Quantify current state
- **Patterns**: What patterns exist?

**Action**: Collect baseline data

```bash
# Count magic numbers
grep -r "\b42\b" src/ | wc -l
# Output: 1 magic number found

# Check if constant exists
grep -r "DEFAULT_TIMEOUT" src/
# Output: No constant found

# Measure code clarity (subjective)
# Current: Magic number `42` is unclear
```

**Example baseline data**:

```markdown
## Baseline Data

**Magic Number Count**: 1
**Named Constants**: 0
**Code Clarity**: Low (magic number unclear)
**Maintainability**: Low (value hardcoded in multiple places)
```

#### 2.4: Verify Safety

**Action**: Ensure change is safe.

**Safety checks**:

- ✅ No logic changes (if refactoring)
- ✅ Tests exist for affected code
- ✅ Change is isolated (doesn't affect other code)
- ✅ Can be easily reverted if needed

**Action**: Verify safety

```bash
# Check tests exist
pnpm test

# Verify current behavior
pnpm lint
```

---

### Step 3: Do (Implement)

**Action**: Implement the improvement.

#### 3.1: Make Change

**Action**: Implement the planned change.

**Change principles**:

- **Minimal**: Change only what's necessary
- **Focused**: One improvement at a time
- **Clean**: Follow existing patterns

**Example implementation**:

```javascript
// Before
function connect() => Promise<Connection | Error> {
    tokio.time.timeout(Duration.from_secs(42), async_connect()).await?
});
// After (Kaizen improvement)
const DEFAULT_TIMEOUT_SECONDS: number = 42;

function connect() => Promise<Connection | Error> {
    tokio.time.timeout(
        Duration.from_secs(DEFAULT_TIMEOUT_SECONDS),
        async_connect()
    ).await?
});
```

#### 3.2: Verify Compilation

**Action**: Ensure code compiles.

```bash
pnpm lint
```

**Expected**: Compiles successfully

---

### Step 4: Check (Verify)

**Action**: Verify improvement achieved its goal.

#### 4.1: Verify Functionality

**Action**: Ensure functionality preserved.

```bash
pnpm test
```

**Expected**: All tests pass

#### 4.2: Verify Improvement

**Action**: Check that improvement achieved its goal.

**Verification**:

- **Code clarity**: Is code more readable?
- **Performance**: Is performance improved? (if applicable)
- **Maintainability**: Is code easier to maintain?
- **Error prevention**: Are errors prevented? (if applicable)
- **Consistency**: Does code match patterns? (if applicable)

**Example verification**:

```javascript
// Improvement: Extract magic number to constant
// Verification:
// ✅ Code more readable: `DEFAULT_TIMEOUT_SECONDS` is clearer than `42`
// ✅ Easier to change: Change constant instead of searching for `42`
// ✅ Self-documenting: Name explains what the number means
// ✅ Functionality preserved: Tests pass
```

#### 4.3: Measure Improvement (DMAIC Measurement)

**Action**: Measure improvement against baseline data and success criteria.

**Measurement**:

- Re-measure metrics after improvement
- Compare to baseline
- Calculate improvement percentage
- Verify success criteria met

**Action**: Measure improvement

```bash
# Re-count magic numbers after improvement
grep -r "\b42\b" src/ | wc -l
# Output: 0 magic numbers (down from 1)

# Check if constant exists
grep -r "DEFAULT_TIMEOUT" src/
# Output: const DEFAULT_TIMEOUT_SECONDS: u64 = 42; (constant added)

# Calculate improvement
# Baseline: 1 magic number, 0 constants
# After improvement: 0 magic numbers, 1 constant
# Improvement: 100% (1/1 magic number eliminated)
```

**Example improvement measurement**:

```markdown
## Improvement Measurement

**Baseline**: 1 magic number, 0 named constants
**After Improvement**: 0 magic numbers, 1 named constant
**Improvement**: 100% (1/1 magic number eliminated)

**Success Criteria Met**: ✅

- Magic number count: 1 → 0 (100% reduction) ✅
- Named constants: 0 → 1 (added) ✅
- Code clarity: Improved (subjective, verified through review) ✅
- Maintainability: Improved (constant can be changed in one place) ✅
```

#### 4.4: Check for Regressions

**Action**: Ensure no regressions introduced.

**Checks**:

- ✅ All tests pass
- ✅ No performance degradation (if applicable)
- ✅ No new warnings
- ✅ Code still compiles

**If regressions found**:

- Revert change
- Re-plan improvement
- Return to Step 2

**If no regressions**:

- Proceed to Step 5

---

### Step 5: Act (Standardize)

**Action**: Standardize the improvement if successful.

#### 5.1: Apply Pattern Consistently

**Action**: Apply improvement pattern to similar code.

**Pattern application**:

- Find similar code that could benefit
- Apply same improvement
- Verify each application

**Example**:

```javascript
// Applied improvement pattern to similar code
const DEFAULT_TIMEOUT_SECONDS: number = 42;
const MAX_RETRY_ATTEMPTS: number = 3; // Applied same pattern
const CONNECTION_POOL_SIZE: number = 10; // Applied same pattern
```

#### 5.2: Document Pattern

**Action**: Document improvement pattern for future use.

**Documentation**:

- What pattern was applied
- Why it's beneficial
- When to apply it
- How to apply it

**Example documentation**:

```javascript
/// Connection timeout configuration.
///
/// **Kaizen improvement**: Extracted magic number to named constant.
/// Pattern: Use named constants instead of magic numbers for:
/// - Configuration values
/// - Repeated literals
/// - Values that may change
const DEFAULT_TIMEOUT_SECONDS: number = 42;
```

#### 5.3: Establish Standard

**Action**: Make improvement part of coding standards.

**Standard establishment**:

- Add to code review checklist
- Add to coding standards
- Share with team

**Example standard**:

```markdown
## Coding Standard: Named Constants

**Rule**: Use named constants instead of magic numbers
**Rationale**: Improves readability, maintainability, self-documentation
**Example**: `const DEFAULT_TIMEOUT_SECONDS: u64 = 42;` instead of `42`
**When**: For configuration values, repeated literals, values that may change
```

#### 5.4: Establish Controls (DMAIC Control)

**Action**: Set up controls to ensure improvement is sustained.

**Controls**:

- **Code review**: Check for magic numbers in reviews
- **Automated checks**: Lint rules to flag magic numbers
- **Monitoring**: Track magic number count over time
- **Standards**: Document pattern in coding standards

**Action**: Create todo list for controls (10+ items)

```markdown
## Kaizen Control Todos (10+ items)

**Code Review Controls**:

- [ ] Add checklist item: No magic numbers in new code
- [ ] Add checklist item: Use named constants for configuration values
- [ ] Update code review process to include checklist
- [ ] Verify checklist is used in reviews

**Automated Checks**:

- [ ] Add lint rule: Flag magic numbers
- [ ] Configure CI check: Fail if magic numbers found
- [ ] Verify automated checks work correctly
- [ ] Review lint rules monthly

**Monitoring Controls**:

- [ ] Set up magic number tracking dashboard
- [ ] Configure alerts if magic number count increases
- [ ] Review magic number trends weekly
- [ ] Document magic number patterns

**Standards Controls**:

- [ ] Add standard to coding guidelines: "Use named constants instead of magic numbers"
- [ ] Update team documentation with standard
- [ ] Verify standard is followed in code reviews
- [ ] Review standards quarterly
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to sustain improvement, don't just document them. Todos track progress, controls prevent regression.

#### 5.5: Monitor (DMAIC Control)

**Action**: Monitor to ensure improvement is sustained.

**Monitoring**:

- Track magic number count over time
- Set up alerts for regression
- Review trends periodically
- Adjust controls if needed

**Action**: Set up monitoring

```bash
# Monitor magic number count
# Run weekly: grep -r "\b42\b" src/ | wc -l
# Alert if count > 0

# Track trends
# Week 1: 1 magic number (baseline)
# Week 2: 0 magic numbers (after improvement)
# Week 3: 0 magic numbers (controls working)
# Week 4: 0 magic numbers (sustained)
```

---

## Complete Workflow Example

```javascript
// Step 1: Identify Opportunity
// Opportunity: Extract magic number `42` to named constant

// Step 2: Plan Change
// Plan: Add constant, replace `42` with constant name
// Risk: Low - simple refactoring

// Step 3: Do (Implement)
const DEFAULT_TIMEOUT_SECONDS: number = 42;
// Replace `42` with `DEFAULT_TIMEOUT_SECONDS`

// Step 4: Check (Verify)
pnpm lint  # Compiles ✅
pnpm test   # Tests pass ✅
// Improvement verified: Code more readable ✅

// Step 5: Act (Standardize)
// Apply pattern to other magic numbers
// Document pattern
// Establish standard
```

## Kaizen Mindset

**Principles**:

1. **Small improvements** - Don't wait for perfect, improve now
2. **Continuous** - Make improvements regularly, not just once
3. **Everyone** - Anyone can suggest improvements
4. **No blame** - Focus on improvement, not blame
5. **Data-driven** - Use data to identify opportunities

**Benefits**:

- **Low risk** - Small changes are safer than big rewrites
- **Fast feedback** - See results quickly
- **Compound effect** - Small improvements add up over time
- **Sustainable** - Easier to maintain than big changes

## Integration with Other Commands

- **[Eliminate Muda](./eliminate-muda.md)** - Use Kaizen to eliminate waste incrementally
- **[Mura Elimination](./eliminate-mura.md)** - Use Kaizen to standardize patterns
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use Kaizen to add type safety incrementally
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow

## Expert Insights

**Why this matters**: Big rewrites are risky and slow. Small improvements are safe and fast. Experts make many small improvements rather than waiting for perfect solutions.

**Key principle**: "Better is the enemy of good" - Don't wait for perfect. Make small improvements now.

**Remember**: Kaizen is continuous. Don't stop after one improvement. Keep looking for opportunities. Small improvements compound over time.

**PDCA cycle**: Plan-Do-Check-Act is the Kaizen cycle. Plan small change, do it, check results, act to standardize. Repeat continuously.

**DfLSS alignment**: Kaizen (continuous improvement) aligns with DfLSS (Design for Lean Six Sigma) principles - improvements should address both efficiency (waste elimination) AND quality (defect prevention). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When making improvements, consider both efficiency gains and quality improvements. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.
