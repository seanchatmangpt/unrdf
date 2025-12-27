---
description: Document a research finding with evidence
arguments:
  - name: finding
    description: Brief title of the finding
    required: true
  - name: category
    description: 'Category: architecture, pattern, limitation, capability, bug'
    required: false
    default: 'capability'
---

# Document Finding: $finding

## Category: $category

Create comprehensive documentation for research finding "$finding".

## Documentation Protocol

### Step 1: Finding Summary

**Title**: $finding

**Category**: $category (architecture | pattern | limitation | capability | bug)

**One-Line Summary**: [What was discovered in 1 sentence]

**Impact**: [Low | Medium | High | Critical]

**Novelty**: [Known | Suspected | Novel]

### Step 2: Context

**Background**:
- What led to this discovery?
- What problem were we trying to solve?
- What assumptions were we testing?

**Related Findings**:
- Previous discoveries that connect to this
- Dependencies or prerequisites
- Conflicting findings (if any)

### Step 3: Evidence Collection

Gather all evidence supporting this finding:

**Code Evidence**:
```javascript
// File: [path]
// Lines: [line numbers]
[relevant code snippet]
```

**Execution Evidence**:
```bash
# Command executed
$ [command]

# Output
[full output]
```

**Documentation Evidence**:
- Source: [file path or URL]
- Quote: "[relevant excerpt]"
- Location: [line numbers or section]

**Test Evidence**:
```javascript
// Test: [test name]
// Result: PASS/FAIL
[test code and output]
```

### Step 4: Analysis

**What This Means**:
- Immediate implications
- Use cases enabled/disabled
- Performance/security/usability impact

**Why This Matters**:
- Value to developers
- Risk if misunderstood
- Productivity impact

**Confidence Level**: [0-100%]

**Confidence Factors**:
- ✅ Multiple independent confirmations
- ✅ Official documentation corroboration
- ✅ Test evidence in multiple scenarios
- ⚠️ Single source of truth
- ❌ Assumptions without validation

### Step 5: Actionable Insights

**Immediate Actions**:
1. [What to do right now based on this finding]
2. [How to apply this knowledge]

**Best Practices**:
1. [Recommended usage pattern]
2. [Common pitfall to avoid]

**Anti-Patterns**:
1. [What NOT to do]
2. [Why it fails]

**Examples**:

**✅ Good Example**:
```javascript
// Recommended approach
[code example]
```

**❌ Bad Example**:
```javascript
// Anti-pattern to avoid
[code example]
```

### Step 6: Integration

**Update Documentation**:
- Which docs need updates? [list files]
- What sections? [section names]

**Update Architecture**:
- Does this change our mental model? [yes/no]
- What diagrams need updates? [list diagrams]

**Update Tests**:
- New test coverage needed? [yes/no]
- Which test suites? [list suites]

### Step 7: Storage

Store structured finding:

```javascript
mcp__claude-flow__memory_usage({
  action: 'store',
  key: 'research/findings/$category/' + Date.now(),
  namespace: 'documentation',
  value: JSON.stringify({
    title: "$finding",
    category: "$category",
    summary: "[one-line summary]",
    impact: "LOW|MEDIUM|HIGH|CRITICAL",
    novelty: "KNOWN|SUSPECTED|NOVEL",
    confidence: 85,
    evidence: {
      code: ["path1", "path2"],
      execution: ["output1", "output2"],
      documentation: ["source1", "source2"],
      tests: ["test1", "test2"]
    },
    implications: ["implication1", "implication2"],
    best_practices: ["practice1", "practice2"],
    anti_patterns: ["antipattern1", "antipattern2"],
    timestamp: new Date().toISOString()
  })
})
```

## Documentation Templates by Category

### Architecture Finding

```markdown
## Architecture Finding: $finding

**Component**: [Which component/system]
**Layer**: [Which architectural layer]

**Discovery**: [What we learned about the architecture]

**Diagram**:
```
[ASCII or mermaid diagram]
```

**Rationale**: [Why it's designed this way]
**Trade-offs**: [What was sacrificed for what]
```

### Pattern Finding

```markdown
## Pattern: $finding

**Problem**: [What problem this pattern solves]
**Solution**: [How the pattern works]
**Implementation**: [Code example]

**When to Use**:
- [Scenario 1]
- [Scenario 2]

**When NOT to Use**:
- [Scenario 1]
- [Scenario 2]

**Variations**: [Alternative implementations]
```

### Limitation Finding

```markdown
## Limitation: $finding

**Constraint**: [What the limitation is]
**Why It Exists**: [Technical or design reason]

**Workarounds**:
1. [Workaround 1 with example]
2. [Workaround 2 with example]

**Future**: [Is this likely to change?]
```

### Capability Finding

```markdown
## Capability: $finding

**Feature**: [What can be done]
**How to Use**: [Usage instructions]

**Parameters**:
- `param1`: [description]
- `param2`: [description]

**Example**:
```javascript
[working example]
```

**Edge Cases**: [Known edge cases]
```

### Bug Finding

```markdown
## Bug: $finding

**Severity**: [Critical | High | Medium | Low]
**Reproducibility**: [Always | Often | Sometimes | Rare]

**Steps to Reproduce**:
1. [Step 1]
2. [Step 2]
3. [Step 3]

**Expected**: [What should happen]
**Actual**: [What actually happens]

**Workaround**: [Temporary fix]
**Root Cause**: [If known]
```

## Success Criteria

- [ ] Finding clearly titled and categorized
- [ ] All evidence collected and verified
- [ ] Confidence level calculated objectively
- [ ] Actionable insights provided
- [ ] Examples (good and bad) included
- [ ] Integration steps identified
- [ ] Finding stored in coordination memory

## Output Format

Provide documentation report:

```markdown
## Finding Documentation: $finding

**Category**: $category
**Impact**: [LOW/MEDIUM/HIGH/CRITICAL]
**Confidence**: [0-100%]

### Summary
[2-3 sentence summary]

### Evidence
- [Evidence item 1]
- [Evidence item 2]

### Implications
- [Implication 1]
- [Implication 2]

### Best Practices
✅ [Do this]
❌ [Don't do this]

### Next Steps
- [ ] Update documentation in [files]
- [ ] Add tests in [suites]
- [ ] Notify team about [impact]
```

---

**Usage Examples**:

```
/research/document "Commands use prompt expansion" architecture
/research/document "Argument substitution via $vars" pattern
/research/document "No built-in type validation" limitation
/research/document "Commands can spawn agents" capability
/research/document "Frontmatter parsing fails on YAML errors" bug
```

---

**Documentation Quality Checklist**:

- ❓ Is finding precisely stated?
- ❓ Is evidence sufficient (3+ sources)?
- ❓ Are implications clearly explained?
- ❓ Are examples runnable/testable?
- ❓ Is confidence level justified?
