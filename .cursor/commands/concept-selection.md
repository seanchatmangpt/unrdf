# Concept Selection - Multi-Step Workflow

## Purpose

This command guides agents through systematic concept selection methods (Pugh Matrix and AHP) to evaluate and select the best design concepts. Concept selection ensures objective, data-driven decisions when choosing between multiple design alternatives. Experts use systematic methods to avoid bias and select concepts that best meet requirements.

## Workflow Overview

```
Step 1: Define Selection Criteria → Step 2: Generate Concepts → Step 3: Pugh Matrix Evaluation → Step 4: AHP Evaluation → Step 5: Select and Verify
```

## Step-by-Step Instructions

### Step 1: Define Selection Criteria

**Action**: Define criteria for evaluating concepts.

#### latest: Identify Criteria from Voice of Customer

**Action**: Extract criteria from customer needs and requirements.

**Criteria sources**:

- **Voice of Customer**: What customers value
- **Business Requirements**: Business goals and constraints
- **Technical Requirements**: Technical performance needs
- **Quality Requirements**: Quality and reliability needs

**Example criteria identification**:

```markdown
## Selection Criteria

**From Voice of Customer**:

- C1: Test execution speed (customers want fast tests)
- C2: Test reliability (customers want no flakiness)
- C3: Ease of use (customers want simple API)

**From Business Requirements**:

- C4: Implementation cost (budget constraints)
- C5: Time to market (schedule constraints)

**From Technical Requirements**:

- C6: Scalability (must handle large test suites)
- C7: Maintainability (must be easy to maintain)

**From Quality Requirements**:

- C8: Test coverage (must maintain coverage)
- C9: Error handling (must handle errors gracefully)
```

#### latest: Prioritize Criteria

**Action**: Determine relative importance of criteria.

**Prioritization methods**:

- **Pairwise comparison**: Compare each pair of criteria
- **Direct rating**: Rate each criterion (1-10 scale)
- **Must-have vs Nice-to-have**: Categorize criteria

**Example prioritization**:

```markdown
## Criteria Prioritization

**Must-Have** (Critical):

- C1: Test execution speed (10/10)
- C2: Test reliability (10/10)
- C8: Test coverage (10/10)

**Important** (High priority):

- C3: Ease of use (8/10)
- C6: Scalability (8/10)
- C9: Error handling (8/10)

**Nice-to-Have** (Lower priority):

- C4: Implementation cost (6/10)
- C5: Time to market (6/10)
- C7: Maintainability (7/10)
```

---

### Step 2: Generate Concepts

**Action**: Generate multiple design concept alternatives.

#### latest: Concept Generation Methods

**Action**: Use multiple methods to generate diverse concepts.

**Generation methods**:

- **Brainstorming**: Generate many ideas
- **TRIZ**: Use TRIZ principles for innovative concepts
- **Benchmarking**: Learn from existing solutions
- **Prototyping**: Build quick prototypes to explore

**Example concepts**:

```markdown
## Design Concepts

**Concept A**: Parallel Test Execution

- **Description**: Run independent tests in parallel threads
- **Key Features**: Thread pool, dependency analysis, parallel execution
- **Pros**: Fast execution, simple architecture
- **Cons**: Requires dependency analysis, potential race conditions

**Concept B**: Pre-Compiled Fixtures

- **Description**: Compile test fixtures at build time
- **Key Features**: Build-time compilation, fixture caching, pre-validation
- **Pros**: Reduces runtime overhead, catches errors early
- **Cons**: Requires build-time processing, more complex build

**Concept C**: Smart Test Selection

- **Description**: Run only tests for changed code
- **Key Features**: Change detection, test mapping, selective execution
- **Pros**: Maximum speed improvement, minimal execution
- **Cons**: Requires change tracking, potential coverage gaps

**Concept D**: Hybrid Approach (A + B)

- **Description**: Combine parallel execution with pre-compiled fixtures
- **Key Features**: Parallel execution + fixture caching
- **Pros**: Best of both worlds, high performance
- **Cons**: More complex implementation
```

#### latest: Document Concepts

**Action**: Document each concept clearly for evaluation.

**Concept documentation**:

- **Name**: Clear concept name
- **Description**: What the concept does
- **Key Features**: Main features and capabilities
- **Architecture**: High-level architecture
- **Pros/Cons**: Advantages and disadvantages

---

### Step 3: Pugh Matrix Evaluation

**Action**: Use Pugh Matrix to compare concepts against baseline.

#### latest: Select Baseline

**Action**: Choose baseline concept for comparison.

**Baseline selection**:

- **Current solution**: Existing approach
- **Simple concept**: Simplest alternative
- **Industry standard**: Common industry approach

**Example baseline**:

```markdown
## Baseline Selection

**Baseline**: Current Sequential Execution

- **Description**: Current approach - tests run sequentially
- **Rationale**: Known performance, serves as reference point
- **Performance**: 10 seconds execution time, 100% coverage
```

#### latest: Create Pugh Matrix

**Action**: Create matrix comparing concepts to baseline.

**Pugh Matrix structure**:

- **Rows**: Selection criteria
- **Columns**: Concepts (including baseline)
- **Cells**: Comparison scores (+ better, 0 same, - worse)

**Scoring**:

- **+**: Concept is better than baseline for this criterion
- **0**: Concept is same as baseline for this criterion
- **-**: Concept is worse than baseline for this criterion
- **S**: Concept is significantly better (count as ++)
- **--**: Concept is significantly worse (count as --)

**Example Pugh Matrix**:

```markdown
## Pugh Matrix

| Criterion           | Baseline (Seq) | Concept A (Parallel) | Concept B (Pre-Compile) | Concept C (Smart Select) | Concept D (Hybrid) |
| ------------------- | -------------- | -------------------- | ----------------------- | ------------------------ | ------------------ |
| C1: Speed           | 0              | +                    | +                       | S                        | S                  |
| C2: Reliability     | 0              | -                    | 0                       | -                        | 0                  |
| C3: Ease of Use     | 0              | 0                    | -                       | -                        | -                  |
| C4: Cost            | 0              | 0                    | -                       | -                        | --                 |
| C5: Time to Market  | 0              | 0                    | -                       | -                        | --                 |
| C6: Scalability     | 0              | +                    | +                       | +                        | S                  |
| C7: Maintainability | 0              | 0                    | -                       | -                        | -                  |
| C8: Coverage        | 0              | 0                    | 0                       | -                        | 0                  |
| C9: Error Handling  | 0              | -                    | +                       | 0                        | 0                  |
| **Total +**         | -              | 2                    | 3                       | 2                        | 3                  |
| **Total 0**         | -              | 5                    | 4                       | 3                        | 4                  |
| **Total -**         | -              | 2                    | 2                       | 4                        | 2                  |
| **Net Score**       | 0              | 0                    | +1                      | -2                       | +1                 |
```

#### latest: Analyze Pugh Matrix Results

**Action**: Analyze scores to identify leading concepts.

**Analysis**:

- **Count + scores**: How many criteria improved?
- **Count - scores**: How many criteria worsened?
- **Net score**: Overall improvement (+ - -)
- **Rank concepts**: Order by net score

**Example analysis**:

```markdown
## Pugh Matrix Analysis

**Concept B (Pre-Compile)**: Net +1 (3+, 2-)

- **Strengths**: Speed, scalability, error handling
- **Weaknesses**: Ease of use, cost, time to market
- **Rank**: #1

**Concept D (Hybrid)**: Net +1 (3+, 2-)

- **Strengths**: Speed (S), scalability (S)
- **Weaknesses**: Cost, time to market
- **Rank**: #1 (tie)

**Concept A (Parallel)**: Net 0 (2+, 2-)

- **Strengths**: Speed, scalability
- **Weaknesses**: Reliability, error handling
- **Rank**: #2

**Concept C (Smart Select)**: Net -2 (2+, 4-)

- **Strengths**: Speed (S), scalability
- **Weaknesses**: Reliability, ease of use, coverage
- **Rank**: #3
```

---

### Step 4: AHP Evaluation

**Action**: Use Analytic Hierarchy Process (AHP) for detailed evaluation.

#### latest: Create Pairwise Comparison Matrix

**Action**: Compare criteria pairwise to determine weights.

**AHP process**:

1. Compare each pair of criteria
2. Use 1-9 scale for comparisons
3. Create comparison matrix
4. Calculate weights from matrix

**Comparison scale**:

- **1**: Equal importance
- **3**: Moderate importance
- **5**: Strong importance
- **7**: Very strong importance
- **9**: Extreme importance
- **2, 4, 6, 8**: Intermediate values

**Example pairwise comparison**:

```markdown
## Pairwise Comparison Matrix (Criteria)

| Criterion           | C1  | C2  | C3  | C4  | C5  | C6  | C7  | C8  | C9  |
| ------------------- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| C1: Speed           | 1   | 1   | 3   | 5   | 5   | 1   | 3   | 1   | 3   |
| C2: Reliability     | 1   | 1   | 3   | 5   | 5   | 1   | 3   | 1   | 3   |
| C3: Ease of Use     | 1/3 | 1/3 | 1   | 3   | 3   | 1/3 | 1   | 1/3 | 1   |
| C4: Cost            | 1/5 | 1/5 | 1/3 | 1   | 1   | 1/5 | 1/3 | 1/5 | 1/3 |
| C5: Time to Market  | 1/5 | 1/5 | 1/3 | 1   | 1   | 1/5 | 1/3 | 1/5 | 1/3 |
| C6: Scalability     | 1   | 1   | 3   | 5   | 5   | 1   | 3   | 1   | 3   |
| C7: Maintainability | 1/3 | 1/3 | 1   | 3   | 3   | 1/3 | 1   | 1/3 | 1   |
| C8: Coverage        | 1   | 1   | 3   | 5   | 5   | 1   | 3   | 1   | 3   |
| C9: Error Handling  | 1/3 | 1/3 | 1   | 3   | 3   | 1/3 | 1   | 1/3 | 1   |

**Weights** (normalized):

- C1: Speed: latest
- C2: Reliability: latest
- C3: Ease of Use: latest
- C4: Cost: latest
- C5: Time to Market: latest
- C6: Scalability: latest
- C7: Maintainability: latest
- C8: Coverage: latest
- C9: Error Handling: latest
```

#### latest: Evaluate Concepts Against Criteria

**Action**: Compare concepts pairwise for each criterion.

**Process**:

1. For each criterion, compare concepts pairwise
2. Create comparison matrix for each criterion
3. Calculate concept scores for each criterion
4. Weight scores by criterion weights
5. Sum weighted scores for final ranking

**Example concept evaluation**:

```markdown
## Concept Evaluation (Criterion C1: Speed)

**Pairwise Comparison**:
| Concept | A | B | C | D |
|---------|---|---|---|---|
| A (Parallel) | 1 | 1/3 | 1/5 | 1/3 |
| B (Pre-Compile) | 3 | 1 | 1/3 | 1 |
| C (Smart Select) | 5 | 3 | 1 | 3 |
| D (Hybrid) | 3 | 1 | 1/3 | 1 |

**Scores** (normalized):

- Concept C: latest
- Concept D: latest
- Concept B: latest
- Concept A: latest

**Weighted Score** (× C1 weight latest):

- Concept C: latest
- Concept D: latest
- Concept B: latest
- Concept A: latest
```

#### latest: Calculate Final Scores

**Action**: Sum weighted scores across all criteria.

**Final score calculation**:

- For each concept: Sum (criterion score × criterion weight)
- Rank concepts by final score
- Highest score = best concept

**Example final scores**:

```markdown
## AHP Final Scores

**Concept D (Hybrid)**:

- C1: latest × latest = latest
- C2: latest × latest = latest
- C3: latest × latest = latest
- C4: latest × latest = latest
- C5: latest × latest = latest
- C6: latest × latest = latest
- C7: latest × latest = latest
- C8: latest × latest = latest
- C9: latest × latest = latest
- **Total**: latest

**Concept B (Pre-Compile)**:

- **Total**: latest

**Concept A (Parallel)**:

- **Total**: latest

**Concept C (Smart Select)**:

- **Total**: latest

**Ranking**: D > B > A > C
```

---

### Step 5: Select and Verify

**Action**: Select best concept(s) and verify selection.

#### latest: Compare Methods

**Action**: Compare Pugh Matrix and AHP results.

**Comparison**:

- **Pugh Matrix**: Quick comparison, identifies improvements
- **AHP**: Detailed evaluation, quantifies preferences
- **Consensus**: Do both methods agree?

**Example comparison**:

```markdown
## Method Comparison

**Pugh Matrix Ranking**:

1. Concept B (Pre-Compile): Net +1
2. Concept D (Hybrid): Net +1
3. Concept A (Parallel): Net 0
4. Concept C (Smart Select): Net -2

**AHP Ranking**:

1. Concept D (Hybrid): latest
2. Concept B (Pre-Compile): latest
3. Concept A (Parallel): latest
4. Concept C (Smart Select): latest

**Consensus**: Both methods favor Concept D (Hybrid) or Concept B (Pre-Compile)
**Selected**: Concept D (Hybrid) - highest AHP score, good Pugh score
```

#### latest: Verify Selection Criteria Met

**Action**: Verify selected concept meets all criteria.

**Verification**:

- **Must-have criteria**: All met?
- **Important criteria**: Most met?
- **Nice-to-have criteria**: Some met?

**Example verification**:

```markdown
## Selection Verification

**Selected**: Concept D (Hybrid - Parallel + Pre-Compile)

**Must-Have Criteria**:

- ✅ C1: Speed - Estimated 80% improvement (2s vs 10s)
- ✅ C2: Reliability - Dependency analysis prevents race conditions
- ✅ C8: Coverage - Maintains 100% coverage

**Important Criteria**:

- ✅ C6: Scalability - Handles large test suites
- ✅ C9: Error handling - Comprehensive error handling
- ⚠️ C3: Ease of use - More complex API (acceptable trade-off)

**Nice-to-Have Criteria**:

- ⚠️ C4: Cost - Higher implementation cost (acceptable)
- ⚠️ C5: Time to market - Longer development time (acceptable)
- ✅ C7: Maintainability - Well-structured code

**Conclusion**: Selected concept meets all must-have criteria ✅
```

#### latest: Create Todo List for Concept Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing selected concept.

**Todo list creation**:

1. Create todos for concept implementation steps
2. Create todos for verification steps
3. Create todos for validation steps
4. Prioritize by dependency
5. Execute todos systematically

**Example todo list**:

```markdown
## Concept Implementation Todos (10+ items)

**Concept D (Hybrid) Implementation**:

- [ ] Design parallel execution architecture
- [ ] Implement thread pool for parallel execution
- [ ] Design dependency analysis algorithm
- [ ] Implement test dependency detection
- [ ] Group tests by dependency
- [ ] Implement parallel execution of independent groups
- [ ] Implement sequential execution within groups
- [ ] Design fixture pre-compilation system
- [ ] Implement build-time fixture compilation
- [ ] Implement fixture caching mechanism
- [ ] Integrate parallel execution with fixture caching
- [ ] Add error handling and recovery
- [ ] Add monitoring and logging
- [ ] Write comprehensive tests
- [ ] Verify compilation: `pnpm lint`
- [ ] Run tests: `pnpm test`
- [ ] Measure performance improvement
- [ ] Verify all selection criteria met
- [ ] Document implementation
- [ ] Deploy and monitor
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement concept)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute concept implementation, don't document it. Todos track progress, implementation delivers value.

---

## Complete Workflow Example

```markdown
# Step 1: Define Selection Criteria

Criteria: Speed, Reliability, Ease of Use, Cost, Time to Market, Scalability, Maintainability, Coverage, Error Handling
Priorities: Must-have (Speed, Reliability, Coverage), Important (others)

# Step 2: Generate Concepts

Concept A: Parallel Execution
Concept B: Pre-Compiled Fixtures
Concept C: Smart Test Selection
Concept D: Hybrid (A + B)

# Step 3: Pugh Matrix Evaluation

Baseline: Sequential Execution
Results: Concept B (+1), Concept D (+1), Concept A (0), Concept C (-2)

# Step 4: AHP Evaluation

Pairwise comparisons → Weights → Concept scores
Results: Concept D (latest), Concept B (latest), Concept A (latest), Concept C (latest)

# Step 5: Select and Verify

Selected: Concept D (Hybrid)
Verification: Meets all must-have criteria ✅
Implementation: Create todos and execute
```

## Integration with Other Commands

- **[Voice of Customer (QFD)](./voice-of-customer-qfd.md)** - Use to identify selection criteria from customer needs
- **[TRIZ Problem Solving](./triz-problem-solving.md)** - Use to generate innovative concepts for evaluation
- **[DMEDI Design Process](./dmedi-design-process.md)** - Use concept selection in Explore phase
- **[Robust Design](./robust-design.md)** - Use to evaluate concepts for robustness
- **[FMEA](./fmea.md)** - Use to evaluate concepts for failure modes

## Expert Insights

**Why this matters**: Systematic concept selection ensures objective, data-driven decisions. Avoids bias and selects concepts that best meet requirements.

**Key principle**: "Data over opinion" - Use systematic methods (Pugh Matrix, AHP) to make objective decisions, not subjective preferences.

**Remember**:

- **Multiple concepts**: Generate many concepts before selecting
- **Systematic evaluation**: Use structured methods, not gut feel
- **Multiple methods**: Use both Pugh Matrix and AHP for validation
- **Verify selection**: Ensure selected concept meets all criteria

**Concept selection mindset**: Generate many options. Evaluate systematically. Select objectively. Verify thoroughly.

**DfLSS alignment**: Concept selection supports DfLSS (Design for Lean Six Sigma) by ensuring selected concepts address both efficiency (waste elimination) AND quality (defect prevention) - evaluating concepts against both efficiency and quality criteria. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Concept selection commands must:

1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement selected concept, not document it
3. **Verify selection** - Test that concept meets criteria
4. **Complete todos** - Mark todos as done as implementation completes

**Principle**: Execute concept implementation, don't document it. Todos track progress, implementation delivers value.

---

End Command ---
