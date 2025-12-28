# KGC-4D README Quality Standards - Quick Reference

**Purpose**: Quick reference for README quality standards and validation process.

**Full Documentation**: See [kgc-4d-readme-validation-checklist.md](./kgc-4d-readme-validation-checklist.md)

---

## 🎯 Core Principle

**Every claim in the README must be provable through execution or measurement.**

NO assumptions. NO "looks good". ONLY evidence.

---

## 📊 Quality Gates (Blockers)

These MUST pass before merge:

| Gate | Requirement | Validation |
|------|-------------|------------|
| **Code Examples Execute** | 100% success rate | `npm run validate:readme:examples` |
| **Links Valid** | 0 broken links | `npm run validate:readme:links` |
| **API Coverage** | 100% of public exports | `npm run validate:readme:api` |
| **Overall Score** | ≥90/100 | `npm run validate:readme` |

---

## 🚀 Quick Validation

```bash
# Full validation suite (all checks)
npm run validate:readme

# Individual checks
npm run validate:readme:examples  # Extract & validate code examples
npm run validate:readme:links     # Check all links
npm run validate:readme:api       # Verify API documentation coverage

# View report
cat tmp/readme-validation-report.txt
```

---

## 📚 Diataxis Section Standards

### 1. Tutorial Section

**Purpose**: Complete beginner → working output in <30 minutes

**Success Criteria**:
- ✅ All code examples execute successfully (100%)
- ✅ Setup time <5 minutes (fresh install test)
- ✅ First output time <10 minutes
- ✅ Completion rate >90% (user testing, N≥3)
- ✅ No undefined terms (0 terms used before definition)

**Proof Required**:
- Execution logs for all examples
- Fresh install timing data
- User testing recordings/notes (3+ users)

### 2. How-To Guides Section

**Purpose**: Copy-paste solutions for real-world tasks

**Success Criteria**:
- ✅ All code examples execute successfully (100%)
- ✅ Use case coverage ≥80% (cross-reference with issues)
- ✅ Self-contained examples (100% run independently)
- ✅ Copy-paste success rate ≥95% (user testing, N≥10)
- ✅ Output accuracy (100% matches documented output)

**Proof Required**:
- Execution logs for all examples
- Issue topic analysis (coverage metrics)
- Random order execution results
- User testing data (copy-paste success)

### 3. Reference Section

**Purpose**: Accurate, comprehensive API documentation

**Success Criteria**:
- ✅ API coverage 100% (all public exports documented)
- ✅ Signature accuracy 100% (automated validation)
- ✅ Type correctness (0 type errors)
- ✅ Parameter accuracy 100% (minimal param tests pass)
- ✅ Error documentation ≥80% (common errors explained)
- ✅ Default value accuracy 100%

**Proof Required**:
- Export comparison (0 undocumented exports)
- Signature validation output
- Type checker results
- Minimal parameter test results

### 4. Explanation Section

**Purpose**: Help users understand WHY it works this way

**Success Criteria**:
- ✅ Design decisions ≥1 per major ADR
- ✅ Trade-off discussions ≥3
- ✅ Alternatives mentioned ≥2
- ✅ User comprehension ≥80% (user testing, N≥5)
- ✅ Deep-dive links ≥5 (0 broken)
- ✅ Diagram accuracy 100%

**Proof Required**:
- ADR cross-reference
- Keyword analysis (trade-offs, alternatives)
- User comprehension testing results
- Link checker results
- Architecture test validation

---

## 🔍 Adversarial PM Questions

**Before declaring README complete, answer with EVIDENCE**:

### Correctness
- ❓ Did you RUN every code example? (Show logs)
- ❓ Did you verify output matches docs? (Show diff)
- ❓ What BREAKS if a signature is wrong? (Impact analysis)
- ❓ Can you PROVE all examples execute? (Show test results)

### Clarity
- ❓ Can a beginner complete the tutorial? (Show user testing)
- ❓ Did you TEST with actual beginners? (Show recordings)
- ❓ What metrics prove it's clear? (Show comprehension data)
- ❓ How do you know progression is logical? (Show user feedback)

### Completeness
- ❓ What use cases are MISSING? (Show issue analysis)
- ❓ Which APIs are undocumented? (Show export diff)
- ❓ What design decisions lack explanation? (Show ADR gaps)
- ❓ Can you PROVE completeness? (Show coverage metrics)

### Evidence Quality
- ❓ Do you have execution logs? (Show files)
- ❓ Do you have user testing data? (Show recordings/notes)
- ❓ Do you have validation reports? (Show automated output)
- ❓ Can someone else reproduce? (Show reproducibility test)

**The Litmus Test**: *Could a skeptical reviewer invalidate ANY claim by running tests?*

If YES → Get more evidence.
If NO → Ship it.

---

## 📈 Success Metrics Summary

| Category | Metric | Target | Command |
|----------|--------|--------|---------|
| **Tutorial** | Execution success | 100% | `npm run validate:readme:examples` |
| | Setup time | <5 min | Manual timing (fresh install) |
| | Completion rate | >90% | User testing (N≥3) |
| **How-To** | Execution success | 100% | `npm run validate:readme:examples` |
| | Use case coverage | ≥80% | `gh issue list --limit 100` + analysis |
| | Copy-paste success | ≥95% | User testing (N≥10) |
| **Reference** | API coverage | 100% | `npm run validate:readme:api` |
| | Signature accuracy | 100% | Automated validation |
| | Type correctness | 0 errors | Type checker |
| **Explanation** | ADR coverage | ≥1/major | Cross-reference |
| | Trade-off discussions | ≥3 | Keyword count |
| | User comprehension | ≥80% | User testing (N≥5) |
| **Cross-Cutting** | Link validation | 0 broken | `npm run validate:readme:links` |
| | Version consistency | 100% | Automated check |
| | **Overall Score** | **≥90/100** | `npm run validate:readme` |

---

## 🛠️ Validation Workflow

### Phase 1: Write Section

```bash
# Write a section (Tutorial, How-To, Reference, or Explanation)
# Include testable code examples
# Document actual API (not ideal API)
```

### Phase 2: Validate Section

```bash
# Run validation
npm run validate:readme

# Check results
cat tmp/readme-validation-report.txt

# Fix failures
grep "❌" tmp/readme-validation-report.txt
```

### Phase 3: User Testing

```bash
# Tutorial: Test with 3+ beginners
# - Record screen/notes
# - Time to completion
# - Comprehension quiz

# How-To: Test with 10+ users
# - Copy-paste examples (no edits)
# - Success rate measurement
# - Collect feedback

# Explanation: Test with 5+ users
# - Comprehension questions
# - Mental model validation
# - Design rationale understanding
```

### Phase 4: Final Validation

```bash
# Full validation suite
npm run validate:readme > validation-report.txt

# Check exit code (0 = pass, >0 = fail)
echo $?

# Review report
cat validation-report.txt

# If all green → Ready to merge
# If any red → Fix and re-validate
```

---

## 📋 Pre-Merge Checklist

**Required Evidence Files**:

- [ ] `tmp/readme-validation-report.txt` (overall score ≥90/100)
- [ ] `tmp/readme-examples/extraction-report.json` (100% valid syntax)
- [ ] User testing notes (Tutorial: 3+ users, How-To: 10+ users, Explanation: 5+ users)
- [ ] Execution logs (all examples run successfully)
- [ ] Link validation output (0 broken links)
- [ ] API coverage report (100% exports documented)

**Validation Commands Pass**:

```bash
# All must exit with code 0
npm run validate:readme:examples && \
npm run validate:readme:links && \
npm run validate:readme:api && \
npm run validate:readme
```

**Adversarial PM Questions Answered**:

- [ ] All 12 questions answered with evidence (not assumptions)
- [ ] Evidence is reproducible (others can verify)
- [ ] No "should work" or "looks good" - only measurements

**Definition of Done**:

✅ All quality gates pass (100%)
✅ Overall validation score ≥90/100
✅ All code examples execute successfully
✅ All links valid
✅ API coverage 100%
✅ User testing complete (N≥3 for tutorial)
✅ Evidence files committed
✅ Can reproduce on fresh clone

---

## 🎓 What NOT to Do

| ❌ DON'T | ✅ DO INSTEAD |
|----------|---------------|
| "Looks good" without running examples | Extract and execute every code block |
| "Should work" without user testing | Test with actual beginners, record sessions |
| "Mostly complete" without coverage metrics | Measure with automated tools (100% coverage) |
| "Clear enough" without comprehension testing | Validate with quizzes/tasks (≥80% comprehension) |
| Trust claims without proof | Require OTEL/test proof for every claim |
| Assume examples run | Run `npm run validate:readme` |
| Visual inspection for links | Run `npm run validate:readme:links` |
| Guess at API coverage | Run `npm run validate:readme:api` |

---

## 🔥 Common Pitfalls

### 1. Syntax Errors in Examples

**Symptom**: Examples look correct but don't execute
**Detection**: `npm run validate:readme:examples`
**Fix**: Extract, run, fix syntax, update README

### 2. Broken Links

**Symptom**: Links work in editor but not in GitHub
**Detection**: `npm run validate:readme:links`
**Fix**: Use absolute paths, verify anchors exist

### 3. Undocumented APIs

**Symptom**: Users ask about features not in README
**Detection**: `npm run validate:readme:api`
**Fix**: Document all public exports with examples

### 4. Output Mismatch

**Symptom**: Documented output differs from actual
**Detection**: Run examples, compare output
**Fix**: Update README with actual output

### 5. Missing Prerequisites

**Symptom**: Tutorial fails at step 1
**Detection**: Fresh install test (Docker)
**Fix**: Add complete setup instructions

---

## 🚀 Continuous Validation

### Local Development

```bash
# Before committing README changes
npm run validate:readme

# Fix any failures before commit
```

### CI/CD Integration

```yaml
# .github/workflows/readme-validation.yml
name: README Validation
on:
  pull_request:
    paths:
      - 'README.md'
      - 'src/**'
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
      - run: npm ci
      - run: npm run validate:readme
      - run: test $? -eq 0 || exit 1
```

### Pre-commit Hook

```bash
# .husky/pre-commit
#!/bin/sh
. "$(dirname "$0")/_/husky.sh"

# Validate README if changed
if git diff --cached --name-only | grep -q "README.md"; then
  npm run validate:readme || exit 1
fi
```

---

## 📚 Related Documentation

- [Full Validation Checklist](./kgc-4d-readme-validation-checklist.md) - Comprehensive validation guide
- [Diataxis Framework](https://diataxis.fr/) - Documentation framework philosophy
- [Counter-Practice Lessons](../CLAUDE.md#counter-practice-lessons) - What NOT to do

---

## 💡 Key Takeaways

1. **Documentation is Code**: Test it like code, prove it like code
2. **Evidence Over Assumptions**: Run, measure, validate - never assume
3. **Users Are Truth**: If beginners can't complete tutorial, it's wrong
4. **Automation Is Essential**: Manual checks miss things, automate everything
5. **Continuous Validation**: Check on every change, not just before merge

**Golden Rule**: If you can't prove it with automated tests or user data, don't claim it.

---

**Remember**: The README is the first impression. Make it provably excellent, not assumedly good.
