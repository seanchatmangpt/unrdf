# KGC-4D README Validation Checklist

**Purpose**: Verify README quality through executable proofs, not assumptions.

**Core Principle**: Every claim in the README must be provable through execution or measurement.

---

## 🎯 Overall Quality Standards

### Correctness Standards

| Standard | Proof Method | Success Criteria |
|----------|--------------|------------------|
| All code examples are syntactically valid | Run ESLint on extracted code | 0 syntax errors |
| Examples match actual API | Execute all code blocks | 100% successful execution |
| Concepts are technically accurate | Cross-reference with source code | All API signatures match implementation |
| No broken links | Run link checker | 0 broken internal/external links |
| Version numbers are current | Compare with package.json | All versions match current release |

### Clarity Standards

| Standard | Verification Method | Success Criteria |
|----------|---------------------|------------------|
| Each section has one clear purpose | Manual review + peer feedback | Each section can be summarized in 1 sentence |
| Terms defined before use | Grep for technical terms | First usage has definition/link |
| Progression is logical | Execute examples in order | Each example builds on previous |
| Jargon minimized | Readability analysis | Gunning Fog Index ≤12 for non-technical sections |
| Code-to-explanation ratio | Count lines | 30-40% code, 60-70% explanation |

### Completeness Standards

| Standard | Verification Method | Success Criteria |
|----------|---------------------|------------------|
| Tutorial teaches productivity | Fresh user test | User can complete first real task in <30 min |
| How-Tos cover primary use cases | Cross-reference with issues/PRs | ≥80% of common questions answered |
| Reference is comprehensive | Compare with API exports | 100% of public API documented |
| Explanations justify design | Architecture Decision Records | All major design choices explained |

---

## 📚 Diataxis Section Validation

### 1. Tutorial Section

**Purpose**: Enable a complete beginner to produce a working KGC-4D document in <30 minutes.

#### Validation Checklist

- [ ] **Setup Instructions Work**
  - **Claim**: User can install and run KGC-4D
  - **Proof**: Fresh Docker container test
  ```bash
  docker run -it --rm node:20 bash -c "
    mkdir test-install && cd test-install &&
    npm init -y &&
    # Run exact installation commands from README &&
    # Run first tutorial example &&
    echo '✅ Installation successful'
  "
  ```
  - **Success**: Exit code 0, output shows expected result

- [ ] **First Example Executes**
  - **Claim**: "Hello World" example works
  - **Proof**: Extract and run code block 1
  ```bash
  # Extract first code example
  grep -A 20 '```javascript' README.md | head -n 21 > /tmp/example1.mjs
  timeout 5s node /tmp/example1.mjs
  ```
  - **Success**: Output matches README expected output

- [ ] **Progressive Complexity Works**
  - **Claim**: Each example builds on previous
  - **Proof**: Run examples 1-N in sequence
  ```bash
  for i in {1..5}; do
    timeout 5s node /tmp/tutorial-example-${i}.mjs || exit 1
  done
  ```
  - **Success**: All 5 examples execute successfully

- [ ] **Concepts Introduced Incrementally**
  - **Claim**: No undefined terms used before explanation
  - **Proof**: Manual review + term tracking
  ```bash
  # Extract all technical terms
  grep -oE '[A-Z][a-z]+([A-Z][a-z]+)+' README.md | sort -u > terms.txt
  # Verify each term is defined before use (manual review)
  ```
  - **Success**: 0 terms used before definition

- [ ] **Tutorial Completion Time**
  - **Claim**: Beginner can complete in <30 minutes
  - **Proof**: Fresh user test (record session)
  - **Success**: 3 of 3 test users complete in <30 min

#### Success Criteria

| Metric | Target | Proof |
|--------|--------|-------|
| Code examples execute | 100% | `npm run test:readme-tutorial` (0 failures) |
| Setup time | <5 min | Fresh install timing |
| First output time | <10 min | User testing (median) |
| Completion rate | >90% | User testing (N≥3) |
| Undefined terms | 0 | Manual review |

---

### 2. How-To Guides Section

**Purpose**: Provide copy-paste solutions for common real-world tasks.

#### Validation Checklist

- [ ] **All How-To Code Runs**
  - **Claim**: Every code example executes successfully
  - **Proof**: Extract and run all how-to examples
  ```bash
  # Extract all how-to code blocks
  node scripts/extract-how-to-examples.mjs README.md
  # Run each example
  for file in how-to-examples/*.mjs; do
    timeout 5s node "$file" || echo "❌ FAILED: $file"
  done
  ```
  - **Success**: 100% execution success rate

- [ ] **Real-World Scenarios Covered**
  - **Claim**: Common use cases are documented
  - **Proof**: Cross-reference with GitHub issues
  ```bash
  # Get top 10 most common issue topics
  gh issue list --limit 100 --json title,labels > issues.json
  # Verify each topic has corresponding how-to (manual)
  ```
  - **Success**: ≥8 of top 10 topics have how-to guides

- [ ] **Examples Are Self-Contained**
  - **Claim**: Each how-to can run independently
  - **Proof**: Run examples in random order
  ```bash
  # Shuffle and run
  find how-to-examples/ -name "*.mjs" | shuf | while read f; do
    timeout 5s node "$f" || exit 1
  done
  ```
  - **Success**: All examples run regardless of order

- [ ] **Copy-Paste Success Rate**
  - **Claim**: Code works without modification
  - **Proof**: User testing (no edits allowed)
  - **Success**: ≥95% of users succeed on first try

- [ ] **Output Examples Match Reality**
  - **Claim**: Shown output matches actual execution
  - **Proof**: Compare README output to actual output
  ```bash
  node how-to-example-1.mjs > actual-output.txt
  # Compare with README expected output
  diff -u readme-expected-output.txt actual-output.txt
  ```
  - **Success**: 0 differences (or only timestamp/dynamic data)

#### Success Criteria

| Metric | Target | Proof |
|--------|--------|-------|
| Code examples execute | 100% | `npm run test:readme-howto` (0 failures) |
| Use case coverage | ≥80% | Issue topic analysis |
| Self-contained examples | 100% | Random order execution |
| Copy-paste success | ≥95% | User testing (N≥10) |
| Output accuracy | 100% | Automated diff check |

---

### 3. Reference Section

**Purpose**: Provide accurate, comprehensive API documentation.

#### Validation Checklist

- [ ] **All Public APIs Documented**
  - **Claim**: 100% API coverage
  - **Proof**: Compare exports to documentation
  ```bash
  # Extract all public exports
  grep -rh '^export ' src/ | sort -u > actual-api.txt
  # Extract all documented APIs
  grep -E '^#{2,4} ' README.md | grep -v 'Example' > documented-api.txt
  # Compare
  comm -23 actual-api.txt documented-api.txt
  ```
  - **Success**: 0 undocumented exports

- [ ] **Function Signatures Match Implementation**
  - **Claim**: All signatures are accurate
  - **Proof**: Extract and compare signatures
  ```bash
  # For each documented function, verify signature matches source
  node scripts/validate-signatures.mjs README.md src/
  ```
  - **Success**: 100% signature accuracy

- [ ] **Type Information Is Correct**
  - **Claim**: Parameter types and return types are accurate
  - **Proof**: Run type checker on documented examples
  ```bash
  # Extract type examples and validate
  node scripts/extract-type-examples.mjs README.md
  timeout 5s npm run type-check -- reference-examples/
  ```
  - **Success**: 0 type errors

- [ ] **Required vs Optional Parameters**
  - **Claim**: Parameter requirements are accurate
  - **Proof**: Test with missing optional params
  ```bash
  # Test each function with minimal params
  node scripts/test-optional-params.mjs
  ```
  - **Success**: Functions work with documented minimal params

- [ ] **Error Cases Documented**
  - **Claim**: Common errors are explained
  - **Proof**: Check error message coverage
  ```bash
  # Extract all throw statements from source
  grep -rh 'throw new' src/ | sort -u > errors-in-code.txt
  # Check if documented in README
  for error in $(cat errors-in-code.txt); do
    grep -q "$error" README.md || echo "Undocumented: $error"
  done
  ```
  - **Success**: ≥80% of error types documented

- [ ] **Default Values Are Correct**
  - **Claim**: Documented defaults match implementation
  - **Proof**: Extract and compare defaults
  ```bash
  node scripts/validate-defaults.mjs README.md src/
  ```
  - **Success**: 100% default value accuracy

#### Success Criteria

| Metric | Target | Proof |
|--------|--------|-------|
| API coverage | 100% | Export comparison (0 missing) |
| Signature accuracy | 100% | Automated signature validation |
| Type correctness | 100% | Type checker (0 errors) |
| Parameter accuracy | 100% | Minimal param tests |
| Error documentation | ≥80% | Error message coverage |
| Default value accuracy | 100% | Default value validation |

---

### 4. Explanation Section

**Purpose**: Help users understand WHY KGC-4D works the way it does.

#### Validation Checklist

- [ ] **Design Decisions Justified**
  - **Claim**: All major design choices explained
  - **Proof**: Cross-reference with ADRs
  ```bash
  # List all architecture decision records
  ls docs/adr/*.md | wc -l
  # Count corresponding explanations in README
  grep -c "## Why" README.md
  ```
  - **Success**: ≥1 explanation per major ADR

- [ ] **Trade-offs Explained**
  - **Claim**: Pros/cons of approach are clear
  - **Proof**: Count trade-off discussions
  ```bash
  grep -i "trade-?off\|advantage\|disadvantage\|pro\|con" README.md
  ```
  - **Success**: ≥3 explicit trade-off discussions

- [ ] **Alternatives Mentioned**
  - **Claim**: Other approaches are acknowledged
  - **Proof**: Check for alternative mentions
  ```bash
  grep -i "alternative\|instead of\|rather than\|could have" README.md
  ```
  - **Success**: ≥2 alternative approaches discussed

- [ ] **Conceptual Model Is Clear**
  - **Claim**: Mental model is understandable
  - **Proof**: User comprehension testing
  - **Success**: ≥80% of users can explain core concept after reading

- [ ] **Links to Deep Dives**
  - **Claim**: Readers can go deeper
  - **Proof**: Count and verify links
  ```bash
  # Extract all documentation links
  grep -oE '\[.*\]\(docs/.*\.md\)' README.md | wc -l
  # Verify all links work
  node scripts/check-links.mjs README.md
  ```
  - **Success**: ≥5 deep-dive links, 0 broken

- [ ] **Diagrams Are Accurate**
  - **Claim**: Visual representations match implementation
  - **Proof**: Manual review + architecture tests
  ```bash
  # Verify architecture matches diagrams
  npm run test:architecture
  ```
  - **Success**: 100% diagram accuracy (manual verification)

#### Success Criteria

| Metric | Target | Proof |
|--------|--------|-------|
| Design decisions explained | ≥1 per major ADR | ADR cross-reference |
| Trade-offs discussed | ≥3 | Keyword count + manual review |
| Alternatives mentioned | ≥2 | Keyword count + manual review |
| User comprehension | ≥80% | User testing (N≥5) |
| Deep-dive links | ≥5 working | Link checker (0 broken) |
| Diagram accuracy | 100% | Architecture tests + manual review |

---

## 🔍 Cross-Cutting Validation

### Code Example Quality

- [ ] **All Examples Are Tested**
  ```bash
  npm run test:readme-examples
  ```
  - **Success**: 0 failures, 100% pass rate

- [ ] **Examples Use Best Practices**
  ```bash
  npm run lint -- examples/
  ```
  - **Success**: 0 linting violations

- [ ] **Examples Handle Errors**
  - **Proof**: Manual review (all examples have try-catch or error handling)
  - **Success**: 100% of examples handle errors appropriately

### Link Validation

- [ ] **Internal Links Work**
  ```bash
  node scripts/check-internal-links.mjs README.md
  ```
  - **Success**: 0 broken internal links

- [ ] **External Links Work**
  ```bash
  node scripts/check-external-links.mjs README.md
  ```
  - **Success**: 0 broken external links (HTTP 200)

- [ ] **Anchor Links Work**
  ```bash
  node scripts/check-anchor-links.mjs README.md
  ```
  - **Success**: 0 broken anchor references

### Version Consistency

- [ ] **Version Numbers Match**
  ```bash
  # Extract version from README
  readme_version=$(grep -oP 'version.*?\K\d+\.\d+\.\d+' README.md | head -1)
  # Compare with package.json
  pkg_version=$(node -p "require('./package.json').version")
  [ "$readme_version" = "$pkg_version" ]
  ```
  - **Success**: Versions match exactly

- [ ] **Dependency Versions Current**
  ```bash
  # Check if documented dependency versions match package.json
  node scripts/check-dependency-versions.mjs README.md
  ```
  - **Success**: 100% version accuracy

### Readability Metrics

- [ ] **Code-to-Text Ratio**
  ```bash
  code_lines=$(grep -c '```' README.md)
  total_lines=$(wc -l < README.md)
  ratio=$((code_lines * 100 / total_lines))
  # Target: 30-40% code blocks
  [ $ratio -ge 30 ] && [ $ratio -le 40 ]
  ```
  - **Success**: 30-40% of README is code

- [ ] **Section Length Balance**
  ```bash
  # No section should be >500 lines (too dense)
  # No section should be <50 lines (too sparse)
  node scripts/check-section-lengths.mjs README.md
  ```
  - **Success**: All sections 50-500 lines

---

## 🚀 Automated Validation Suite

### Create Test Scripts

```bash
# npm run validate:readme - Run full validation suite
# Exit code 0 = all checks pass, >0 = failures

validate:readme:
  - Extract all code examples
  - Execute all examples
  - Check all links
  - Validate signatures
  - Check version consistency
  - Generate validation report
```

### Validation Report Format

```
=== KGC-4D README Validation Report ===
Generated: 2025-12-27 10:30:00

Tutorial Section:
  ✅ Code examples execute: 5/5 (100%)
  ✅ Setup time: 4.2 minutes (target: <5 min)
  ✅ Undefined terms: 0

How-To Guides Section:
  ✅ Code examples execute: 12/12 (100%)
  ✅ Use case coverage: 9/10 topics (90%)
  ✅ Self-contained: 12/12 (100%)

Reference Section:
  ✅ API coverage: 47/47 exports (100%)
  ✅ Signature accuracy: 47/47 (100%)
  ✅ Type correctness: 0 errors
  ❌ Error documentation: 15/20 errors (75%) - BELOW TARGET

Explanation Section:
  ✅ Design decisions: 5/4 ADRs (125%)
  ✅ Trade-offs discussed: 4
  ✅ Deep-dive links: 7 (0 broken)

Cross-Cutting:
  ✅ Link validation: 0 broken (45 total)
  ✅ Version consistency: MATCH (1.0.0)
  ✅ Code-to-text ratio: 35%

OVERALL: 95/100 (1 FAILURE)
  ❌ Reference Section: Error documentation below 80%
```

---

## 📊 Quality Gates

### Pre-Merge Requirements

| Gate | Requirement | Blocker? |
|------|-------------|----------|
| Tutorial examples execute | 100% | YES |
| How-to examples execute | 100% | YES |
| API coverage | 100% | YES |
| Signature accuracy | 100% | YES |
| Internal links work | 100% | YES |
| External links work | ≥95% | NO (soft fail) |
| Use case coverage | ≥80% | NO |
| User comprehension | ≥80% | NO |

### Definition of Done

**README is considered complete when**:
1. `npm run validate:readme` exits with code 0
2. All blocker quality gates pass (100%)
3. Overall validation score ≥90/100
4. At least 3 fresh users complete tutorial successfully
5. OTEL validation ≥80/100 for any automated examples

---

## 🎯 Adversarial PM Questions

**Before declaring README complete, answer these**:

### Correctness
- ❓ Did you RUN every code example or just read them?
- ❓ Did you verify actual output matches documented output?
- ❓ What BREAKS if a signature is wrong?
- ❓ Can you PROVE all examples execute successfully?

### Clarity
- ❓ Can a beginner with NO knowledge complete the tutorial?
- ❓ Did you TEST with actual beginners or assume it's clear?
- ❓ What SPECIFIC metrics prove it's understandable?
- ❓ How do you know progression is logical? (Show user testing)

### Completeness
- ❓ What use cases are MISSING? (Show issue analysis)
- ❓ Which APIs are undocumented? (Show export diff)
- ❓ What design decisions lack explanation? (Show ADR gaps)
- ❓ Can you PROVE completeness with metrics?

### Evidence Quality
- ❓ Do you have execution logs for all examples?
- ❓ Do you have user testing data?
- ❓ Do you have automated validation reports?
- ❓ Can someone else reproduce your validation?

**The Litmus Test**: *Could a skeptical reviewer invalidate ANY claim by running tests?*

If YES → Evidence is insufficient. Get proof.
If NO → Claims are defensible. Ship it.

---

## 🛠️ Implementation Guide

### Phase 1: Setup Validation Infrastructure (Day 1)

```bash
# Create validation scripts
mkdir -p scripts/readme-validation

# Extract code examples
touch scripts/readme-validation/extract-examples.mjs

# Execute examples
touch scripts/readme-validation/run-examples.mjs

# Check links
touch scripts/readme-validation/check-links.mjs

# Validate signatures
touch scripts/readme-validation/validate-signatures.mjs

# Generate report
touch scripts/readme-validation/generate-report.mjs

# Add to package.json
npm pkg set scripts.validate:readme="node scripts/readme-validation/run-all.mjs"
```

### Phase 2: Write README (Days 2-3)

- Write each section with validation in mind
- Include testable examples
- Document actual API (not ideal API)
- Run `npm run validate:readme` after each section

### Phase 3: Validation & Iteration (Day 4)

```bash
# Run full validation
npm run validate:readme > validation-report.txt

# Fix failures
grep "❌" validation-report.txt

# User testing
# Record 3 users attempting tutorial

# Final validation
npm run validate:readme && echo "✅ READY TO MERGE"
```

### Phase 4: Continuous Validation (Ongoing)

```yaml
# .github/workflows/readme-validation.yml
name: README Validation
on: [push, pull_request]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: npm ci
      - run: npm run validate:readme
      - run: test $? -eq 0 || exit 1
```

---

## 📈 Success Metrics Summary

| Category | Metric | Target | Proof Method |
|----------|--------|--------|--------------|
| **Tutorial** | Execution success | 100% | Automated test suite |
| | Setup time | <5 min | Fresh install timing |
| | Completion rate | >90% | User testing (N≥3) |
| **How-To** | Execution success | 100% | Automated test suite |
| | Use case coverage | ≥80% | Issue analysis |
| | Copy-paste success | ≥95% | User testing (N≥10) |
| **Reference** | API coverage | 100% | Export comparison |
| | Signature accuracy | 100% | Automated validation |
| | Type correctness | 0 errors | Type checker |
| **Explanation** | ADR coverage | ≥1 per major | Cross-reference |
| | Trade-off discussions | ≥3 | Keyword analysis |
| | User comprehension | ≥80% | User testing (N≥5) |
| **Cross-Cutting** | Link validation | 0 broken | Link checker |
| | Version consistency | 100% | Automated check |
| | Overall score | ≥90/100 | Validation suite |

---

## 🎓 Lessons from Counter-Practice

### What NOT to Do

1. ❌ "Looks good" without running examples
2. ❌ "Should work" without user testing
3. ❌ "Mostly complete" without coverage metrics
4. ❌ "Clear enough" without comprehension testing
5. ❌ Trust claims without OTEL/test proof

### What WORKS

1. ✅ Extract and execute every code block
2. ✅ Test with actual beginners (record sessions)
3. ✅ Measure coverage with automated tools
4. ✅ Validate comprehension with quizzes/tasks
5. ✅ Require proof for every quality claim

**Golden Rule**: Documentation is code. Test it like code. Prove it like code.

---

## 🔥 Final Checklist

**Before declaring README complete**:

- [ ] `npm run validate:readme` exits 0
- [ ] All code examples execute successfully (show logs)
- [ ] 3+ users completed tutorial (show recordings/notes)
- [ ] API coverage is 100% (show export diff with 0 results)
- [ ] All links work (show link checker report)
- [ ] Version numbers match package.json (show comparison)
- [ ] Overall validation score ≥90/100 (show report)
- [ ] OTEL validation ≥80/100 for automated examples
- [ ] Adversarial PM questions answered with evidence
- [ ] Can reproduce validation on fresh clone

**Evidence Required**:
- Validation report (text file)
- User testing notes/recordings (≥3 users)
- Execution logs for all examples
- Link checker output
- Signature validation output

**Ship Criteria**: All checkboxes ✅ + evidence files committed.

---

**Remember**: The README is the first impression. Make it provably excellent, not assumedly good.
