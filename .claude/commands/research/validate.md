---
description: Validate a capability claim with evidence
arguments:
  - name: claim
    description: The claim to validate (in quotes)
    required: true
---

# Claim Validation: $claim

## Adversarial Protocol

Execute rigorous validation of claim: "$claim"

Apply the **Adversarial PM** principle: Separate claims from reality. Demand evidence, not assertions.

### Step 1: Claim Decomposition

Break down "$claim" into testable assertions:

**Example**:
- Claim: "Commands can spawn agents"
- Assertions:
  1. Commands can reference the Task tool
  2. Task tool can be invoked from command body
  3. Spawned agents execute successfully

List all assertions for "$claim":

1. [Assertion 1]
2. [Assertion 2]
3. [Assertion 3]

### Step 2: Test Design

For each assertion, design **minimal test** that proves/disproves it:

**Assertion 1 Test**:
```bash
[Command to test assertion 1]
```

**Expected Output**: [What proves assertion true]
**Refutation Output**: [What proves assertion false]

### Step 3: Execution

Run each test and capture FULL output:

```bash
# Test 1
timeout 5s [command] 2>&1 | tee /tmp/test1-output.txt
cat /tmp/test1-output.txt

# Test 2
timeout 5s [command] 2>&1 | tee /tmp/test2-output.txt
cat /tmp/test2-output.txt
```

**CRITICAL**: Capture stderr AND stdout. Read full output, not just exit code.

### Step 4: Analysis

Compare expected vs actual for each assertion:

| Assertion | Expected | Actual | Match? | Confidence |
|-----------|----------|--------|--------|------------|
| 1 | ... | ... | ✅/❌ | 0-100% |
| 2 | ... | ... | ✅/❌ | 0-100% |
| 3 | ... | ... | ✅/❌ | 0-100% |

**Analysis Questions**:
- Did test execute successfully?
- Does output match expected behavior?
- Are there edge cases not covered?
- What assumptions are we making?

### Step 5: Verdict

**Claim**: "$claim"

**Verdict**:
- ✅ **CONFIRMED**: All assertions pass with high confidence (>80%)
- ❌ **REFUTED**: One or more assertions fail with clear evidence
- ⚠️ **INCONCLUSIVE**: Insufficient evidence or conflicting results
- 🔄 **PARTIAL**: Some aspects confirmed, some refuted

**Evidence Summary**:
- Tests run: [count]
- Tests passed: [count]
- Tests failed: [count]
- Confidence: [0-100%] based on test coverage and result clarity

**Detailed Evidence**:
1. [Link to test output 1]
2. [Link to test output 2]
3. [Link to test output 3]

### Step 6: Recommendations

Based on verdict:

**If CONFIRMED**:
- Document capability in architecture docs
- Create usage examples
- Identify edge cases for future testing

**If REFUTED**:
- Document limitation clearly
- Identify workarounds if applicable
- Update assumptions

**If INCONCLUSIVE**:
- Design better tests
- Gather more evidence
- Consult documentation/source code

### Step 7: Storage

Store validation result:

```javascript
mcp__claude-flow__memory_usage({
  action: 'store',
  key: 'research/validation/claim-' + Date.now(),
  namespace: 'evidence',
  value: JSON.stringify({
    claim: "$claim",
    verdict: "CONFIRMED|REFUTED|INCONCLUSIVE|PARTIAL",
    assertions: [
      {
        assertion: "...",
        test: "...",
        result: "PASS|FAIL",
        evidence: "..."
      }
    ],
    confidence: 85,
    timestamp: new Date().toISOString()
  })
})
```

## Success Criteria

- [ ] Claim decomposed into ≥2 testable assertions
- [ ] Tests designed for each assertion
- [ ] All tests executed with full output captured
- [ ] Evidence analyzed objectively
- [ ] Verdict determined with confidence score
- [ ] Results stored in coordination memory

## Output Format

Provide validation report:

```markdown
## Validation Report: $claim

**Verdict**: [CONFIRMED/REFUTED/INCONCLUSIVE/PARTIAL]
**Confidence**: [0-100%]

### Assertions Tested
1. [Assertion] - ✅/❌ [Result]
2. [Assertion] - ✅/❌ [Result]

### Evidence
- [Test output links]
- [Command execution logs]

### Conclusion
[2-3 sentence summary]

### Recommendations
- [Next steps based on verdict]
```

---

**Usage Examples**:

```
/research/validate "Commands can spawn agents"
/research/validate "Arguments are type-validated"
/research/validate "Commands can access conversation history"
```

---

**Adversarial Checklist**:

Before declaring verdict:

- ❓ Did I RUN tests or just read code?
- ❓ Did I read FULL output or assume?
- ❓ What BREAKS if verdict is wrong?
- ❓ Can I REPRODUCE from scratch?
- ❓ What's the evidence quality (0-100%)?
