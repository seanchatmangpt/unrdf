# Voice of Customer & QFD (Quality Function Deployment) - Multi-Step Workflow

## Purpose

This command guides agents through capturing customer needs and translating them into design requirements using Quality Function Deployment (QFD). Voice of Customer (VOC) identifies what customers actually need, while QFD systematically translates those needs into technical requirements. Experts use VOC + QFD to ensure designs address real customer needs, not assumed ones.

## Workflow Overview

```
Step 1: Capture Voice of Customer → Step 2: Organize Customer Needs → Step 3: Build QFD House of Quality → Step 4: Prioritize Requirements → Step 5: Set Targets and Verify
```

## Step-by-Step Instructions

### Step 1: Capture Voice of Customer

**Action**: Gather customer needs directly from customers.

#### 1.1: Identify Customer Segments

**Action**: Identify who the customers are.

**Customer segments**:

- **End users**: People who use the product
- **Administrators**: People who manage the product
- **Business stakeholders**: People who fund/decide on product
- **Support teams**: People who support customers

**Example customer segments**:

```markdown
## Customer Segments

### End Users
- Individual developers using test framework
- Teams running test suites
- CI/CD pipelines executing tests

### Administrators
- DevOps engineers managing test infrastructure
- Release managers scheduling tests
- Build system maintainers

### Business Stakeholders
- Engineering managers (care about team velocity)
- Product managers (care about features)
- Finance (care about costs)

### Support Teams
- Customer support (care about ease of use)
- Technical support (care about reliability)
```

#### 1.2: Gather Customer Needs

**Action**: Collect needs through multiple methods.

**Gathering methods**:

- **Interviews**: Direct conversations with customers
- **Surveys**: Structured feedback from many customers
- **Observations**: Watch customers actually using product
- **Data analysis**: Look at usage patterns, complaints, feature requests
- **Focus groups**: Discuss needs with customer groups

**Example gathering**:

```markdown
## Customer Needs Gathering

### From Interviews
- "I need tests to run fast - we have 10K tests and pipeline takes too long"
- "Tests are flaky - we need reliable tests"
- "Hard to debug failures - we need clear error messages"

### From Surveys
- 85% want faster execution
- 90% want more reliable tests
- 70% want better error reporting

### From Usage Data
- Average test execution: 15 minutes (too long)
- Flaky test rate: 5% (too high)
- Failed debugging: 30% of support tickets
```

#### 1.3: Translate to Customer Needs

**Action**: Express gathered feedback as customer needs statements.

**Need statement format**:

- **Customer**: Who has the need?
- **Need**: What do they need?
- **Benefit**: Why do they need it?

**Example needs**:

```markdown
## Customer Needs (VOC)

### Speed Needs
- **Need 1**: Developers need fast test execution so they can get rapid feedback
- **Need 2**: CI/CD pipelines need fast tests so deployments complete quickly
- **Need 3**: Teams need consistent speed so they can plan build times

### Reliability Needs
- **Need 4**: Developers need reliable tests so they trust test results
- **Need 5**: CI/CD systems need reliable tests so builds are predictable
- **Need 6**: Managers need reliable tests so they can trust quality metrics

### Debugging Needs
- **Need 7**: Developers need clear errors so they can debug failures quickly
- **Need 8**: Teams need error context so they understand what went wrong
```

---

### Step 2: Organize Customer Needs

**Action**: Organize needs into categories and prioritize.

#### 2.1: Create Affinity Diagram

**Action**: Group related needs together.

**Affinity diagram process**:

1. List all customer needs
2. Group related needs together
3. Create categories (affinities)
4. Identify patterns

**Example affinity diagram**:

```markdown
## Affinity Diagram

### Speed Needs
- Need 1: Fast test execution
- Need 2: Fast feedback
- Need 3: Consistent speed
→ **Category**: Performance

### Reliability Needs
- Need 4: Reliable tests
- Need 5: Trust test results
- Need 6: Predictable builds
→ **Category**: Reliability

### Debugging Needs
- Need 7: Clear error messages
- Need 8: Error context
- Need 9: Stack traces
→ **Category**: Debuggability
```

#### 2.2: Classify Needs (Kano Model)

**Action**: Classify needs by type to prioritize effort.

**Kano classification**:

- **Must-Have**: Basic needs (dissatisfied if missing, neutral if present)
- **Performance**: More is better (satisfied by more, dissatisfied by less)
- **Delighter**: Wow factor (delighted if present, indifferent if absent)

**Example classification**:

```markdown
## Kano Classification

### Must-Have
- Reliable tests (customers dissatisfied if unreliable)
- No false failures (basic expectation)
- Core functionality works (basic requirement)

### Performance
- Test speed (faster is better, slower is worse)
- Clear error messages (clearer is better)
- Resource efficiency (lower resource use is better)

### Delighter
- Automated test optimization suggestions
- Smart test grouping for speed
- AI-powered debugging hints
```

#### 2.3: Prioritize Customer Needs

**Action**: Determine which needs are most important.

**Prioritization methods**:

- **Frequency**: How many customers have this need?
- **Importance**: How important is this need?
- **Urgency**: How urgent is this need?
- **Satisfaction gap**: How unsatisfied are customers currently?

**Example prioritization**:

```markdown
## Customer Need Priorities

| Need Category | Frequency | Importance | Gap | Priority |
|---------------|-----------|------------|-----|----------|
| Fast speed    | 85%       | 10/10      | 9   | **CRITICAL** |
| Reliability   | 90%       | 10/10      | 8   | **CRITICAL** |
| Clear errors  | 70%       | 8/10       | 7   | **HIGH** |
| Debug support | 50%       | 7/10       | 6   | MEDIUM |
| Ease of use   | 60%       | 8/10       | 5   | HIGH |

**Top Priorities**: Speed, Reliability, Ease of Use
```

---

### Step 3: Build QFD House of Quality

**Action**: Create QFD matrix translating customer needs to design requirements.

#### 3.1: Identify Design Requirements

**Action**: Translate customer needs into technical requirements.

**Design requirements** (How):

- **Technical parameters**: Design parameters that affect customer needs
- **Design metrics**: Measurable design characteristics
- **Implementation choices**: Design decisions

**Example design requirements**:

```markdown
## Design Requirements (HOW)

### For Speed Need
- Parallel execution capability
- Efficient resource usage
- Minimal fixture overhead

### For Reliability Need
- Comprehensive error handling
- Transaction support (rollback on failure)
- Isolation between tests
- Deterministic execution

### For Debuggability Need
- Detailed error messages
- Stack traces
- Test context information
- Debug logging
```

#### 3.2: Build QFD House of Quality Matrix

**Action**: Create matrix mapping customer needs to design requirements.

**House of Quality structure**:

- **Rows (What)**: Customer needs (prioritized)
- **Columns (How)**: Design requirements
- **Matrix cells**: Relationship strength (strong/medium/weak)
- **Column weights**: Importance based on customer priorities

**Example House of Quality**:

```markdown
## House of Quality Matrix

| Customer Need (What) | Importance | Parallel Exec | Resource Eff | Error Handling | Isolation | Logging |
|---------------------|-----------|---------------|-------------|--------------|-----------|---------|
| Fast speed (PRIMARY) | 10        | ●●●           | ●●          | ●            | ○         | ○       |
| Reliable tests       | 10        | ●             | ◐           | ●●●          | ●●●       | ○       |
| Clear errors         | 8         | ○             | ○           | ●●●          | ◐         | ●●      |
| Debuggability        | 7         | ◐             | ○           | ●●           | ○         | ●●●     |

Legend: ●●● Strong, ●● Medium, ● Weak, ◐ Moderate, ○ None

**Column Weights** (Importance):
- Parallel Exec: 10 (drives speed, highest impact)
- Error Handling: 9 (drives reliability and errors)
- Logging: 7 (enables debugging)
- Isolation: 8 (drives reliability)
- Resource Eff: 6 (supports speed)
```

#### 3.3: Identify Correlations (Roof Matrix)

**Action**: Identify how design requirements relate to each other.

**Correlations**:

- **Positive**: Requirements that reinforce each other
- **Negative**: Requirements that conflict
- **None**: Requirements that are independent

**Example correlations**:

```markdown
## Correlations Between Design Requirements

### Positive Correlations
- Parallel Execution + Isolation = Requires inter-test communication
- Error Handling + Logging = Comprehensive debugging support
- Resource Efficiency + Isolation = Careful resource management

### Negative Correlations
- Parallel Execution ✗ Simple implementation = Complex code needed
- Comprehensive logging ✗ Performance = Logging overhead

### Independent
- Logging independently addresses debuggability
- Error handling independent of speed optimization
```

#### 3.4: Prioritize Design Requirements

**Action**: Rank design requirements by importance.

**Prioritization** (based on column weights):

1. Calculate importance of each design requirement
2. Identify which requirements drive customer satisfaction
3. Highlight critical design requirements

**Example prioritization**:

```markdown
## Design Requirement Priorities

**CRITICAL** (Must implement):
1. Parallel execution capability (10 importance)
2. Isolation between tests (8 importance)
3. Error handling (9 importance)
4. Logging/debugging (7 importance)

**IMPORTANT** (Should implement):
5. Resource efficiency (6 importance)

**NICE-TO-HAVE** (Could implement):
6. Advanced debugging features (4 importance)
```

---

### Step 4: Prioritize Requirements

**Action**: Set targets and prioritize requirements for implementation.

#### 4.1: Set Measurable Targets

**Action**: Define what "good" looks like for each requirement.

**Target definition**:

- **Current state**: Where we are now
- **Target state**: Where we want to be
- **Measurement**: How to measure progress
- **Timeline**: When to achieve target

**Example targets**:

```markdown
## Design Requirements Targets

### Parallel Execution (CRITICAL)
- **Current**: Sequential only (15 min for 10K tests)
- **Target**: 8-thread parallel execution
- **Measurement**: Execution time
- **Success**: < 3 minutes for 10K tests
- **Timeline**: Phase 1 (Sprint 1)

### Test Isolation (CRITICAL)
- **Current**: Partial isolation (5% flaky tests)
- **Target**: Complete isolation, 0% flaky tests
- **Measurement**: Flakiness rate
- **Success**: 0% flakiness for 3+ weeks
- **Timeline**: Phase 1 (Sprint 2)

### Error Handling (CRITICAL)
- **Current**: Generic errors, no context (30% of support tickets)
- **Target**: Detailed errors with context
- **Measurement**: Support ticket reduction
- **Success**: < 10% of tickets from unclear errors
- **Timeline**: Phase 1 (Sprint 1)

### Logging (IMPORTANT)
- **Current**: Minimal logging
- **Target**: Comprehensive debug logging
- **Measurement**: Debugging time
- **Success**: Debug issues in < 5 min vs current 15+ min
- **Timeline**: Phase 2 (Sprint 3)
```

#### 4.2: Verify Requirements Address Needs

**Action**: Ensure design requirements actually address customer needs.

**Verification**:

- For each customer need, which design requirements address it?
- Is the requirement necessary to satisfy the need?
- Are there alternative ways to satisfy the need?

**Example verification**:

```markdown
## Requirements-to-Needs Mapping Verification

### Speed Need → Parallel Execution Requirement
- ✅ Direct relationship: Parallel execution directly reduces execution time
- ✅ Necessary: Required to meet 3-minute target
- ✅ Impact: Primary driver of speed improvement

### Reliability Need → Isolation + Error Handling Requirements
- ✅ Isolation prevents test interference
- ✅ Error handling enables recovery
- ✅ Both necessary for reliability
- ✅ Combined impact addresses reliability need

### Debugging Need → Logging Requirement
- ✅ Comprehensive logging enables debugging
- ✅ Stack traces + context help understand failures
- ✅ Necessary to reduce debugging time
```

---

### Step 5: Set Targets and Verify

**Action**: Finalize targets and create implementation plan.

#### 5.1: Create Implementation Targets

**Action**: Create specific, measurable targets for each design requirement.

**Target format**:

- **Requirement**: Design requirement
- **Metric**: How to measure
- **Current**: Current state
- **Target**: Goal state
- **Timeline**: When to achieve

**Example implementation targets**:

```markdown
## Implementation Targets

### Parallel Execution
- **Metric**: Test execution time
- **Current**: 15 minutes (sequential)
- **Phase 1 Target**: 3 minutes (8 threads)
- **Phase 2 Target**: 1.5 minutes (16 threads, optimized)

### Test Isolation
- **Metric**: Test flakiness rate
- **Current**: 5% flaky tests
- **Phase 1 Target**: 1% flaky tests
- **Phase 2 Target**: 0% flaky tests

### Error Handling
- **Metric**: Clarity of error messages
- **Current**: Generic errors (30 support tickets/month about unclear errors)
- **Phase 1 Target**: Detailed errors (< 15 tickets/month)
- **Phase 2 Target**: Self-explanatory errors (< 5 tickets/month)

### Logging
- **Metric**: Debug support capability
- **Current**: Minimal logging
- **Phase 1 Target**: Comprehensive logging enabled
- **Phase 2 Target**: Smart debug suggestions
```

#### 5.2: Create 10+ Item Todo List for Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing design requirements.

**Todo list creation**:

1. Create todos for each design requirement
2. Create todos for verification steps
3. Create todos for validation steps
4. Prioritize by criticality
5. Execute todos systematically

**Example todo list**:

```markdown
## QFD Implementation Todos (10+ items)

**CRITICAL - Phase 1 (Sprint 1)**:

- [ ] Design parallel execution architecture
- [ ] Implement thread pool for parallel tests
- [ ] Implement test dependency analysis
- [ ] Group tests by dependency
- [ ] Execute independent test groups in parallel
- [ ] Add error handling for parallel failures
- [ ] Add comprehensive error messages
- [ ] Write tests for parallel execution
- [ ] Write tests for error handling
- [ ] Verify execution time target: < 3 minutes
- [ ] Verify tests pass: `pnpm test`
- [ ] Verify linting passes: `pnpm lint`

**CRITICAL - Phase 1 (Sprint 2)**:

- [ ] Implement test isolation mechanisms
- [ ] Add transaction support for tests
- [ ] Add rollback capability on failure
- [ ] Write tests for isolation
- [ ] Verify flakiness rate < 1%
- [ ] Verify test reliability over 2+ weeks

**IMPORTANT - Phase 2**:

- [ ] Implement comprehensive logging
- [ ] Add debug mode for detailed output
- [ ] Write tests for logging
- [ ] Measure debug time improvement
- [ ] Implement smart debug suggestions (if feasible)
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement requirements)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute implementation, don't document it. Todos track progress, implementation delivers customer value.

---

## Complete Workflow Example

```bash
# Step 1: Capture Voice of Customer
# - Interviewed 15 developers
# - Surveyed 50+ teams
# - Analyzed usage data
# - Identified top needs: Speed, Reliability, Debugging

# Step 2: Organize Customer Needs
# - Grouped needs into 5 categories
# - Classified with Kano Model
# - Prioritized by frequency + importance

# Step 3: Build QFD House of Quality
# - Created 5 customer needs × 6 design requirements matrix
# - Identified relationship strengths
# - Calculated column weights based on priorities

# Step 4: Prioritize Requirements
# - Set CRITICAL requirements: Parallel Exec, Isolation, Error Handling
# - Set IMPORTANT requirement: Logging
# - Set targets: < 3 min exec, 0% flakiness, clear errors

# Step 5: Create Implementation Plan
# - Created 15+ todo items for Phase 1 + Phase 2
# - Prioritized CRITICAL items first
# - Planned incremental delivery
```

## Integration with Other Commands

- **[DMEDI Design Process](./dmedi-design-process.md)** - Use VOC in Define phase, QFD in Explore phase
- **[Concept Selection](./concept-selection.md)** - Use customer needs as selection criteria
- **[Robust Design](./robust-design.md)** - Use customer needs as response variables to optimize
- **[FMEA](./fmea.md)** - Use customer needs to identify critical failure modes
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use VOC to define what "good" looks like
- **[TRIZ Problem Solving](./triz-problem-solving.md)** - Use TRIZ principles to innovate solutions to customer needs

## Expert Insights

**Why this matters**: Designs that don't address real customer needs fail. VOC + QFD ensures designs are built on customer needs, not assumptions.

**Key principle**: "Listen to customers" - Customer needs drive design decisions. Don't assume what customers want - ask them.

**Remember**:

- **Multiple sources**: Gather needs from interviews, surveys, observations, data
- **Organize systematically**: Use affinity diagrams and Kano model
- **Translate precisely**: Design requirements must directly address customer needs
- **Prioritize ruthlessly**: Focus on CRITICAL needs first
- **Verify mapping**: Ensure design requirements actually address customer needs

**VOC + QFD mindset**: Start with customer needs. Understand what customers actually want. Design solutions that address real needs. Verify designs satisfy customers.

**DfLSS alignment**: VOC + QFD supports DfLSS (Design for Lean Six Sigma) by ensuring designs address both efficiency needs (Lean) AND quality needs (Six Sigma) from the start - directly from customer voice. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination and efficiency. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: VOC + QFD commands must:

1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement design requirements, not document them
3. **Verify customer needs** - Test that design addresses real needs
4. **Complete todos** - Mark todos as done as implementation completes

**Principle**: Execute design solutions, don't document them. Todos track progress, implementation delivers customer value.

---

End Command ---
