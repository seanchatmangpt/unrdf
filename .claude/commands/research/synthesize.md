---
description: Synthesize research findings into coherent knowledge
arguments:
  - name: scope
    description: 'Scope: topic-specific, full-session, cross-agent, or all'
    required: false
    default: 'full-session'
---

# Research Synthesis

## Scope: $scope

Synthesize all research findings for scope "$scope" into coherent, actionable knowledge.

## Synthesis Protocol

### Step 1: Finding Retrieval

Retrieve all findings based on scope:

**topic-specific**: Single topic (requires topic name)
**full-session**: All findings from current session
**cross-agent**: Findings from multiple agents
**all**: Complete research corpus

```javascript
// Retrieve findings
mcp__claude-flow__memory_usage({
  action: 'retrieve',
  key: 'research/findings/*',
  namespace: 'documentation'
})

mcp__claude-flow__memory_usage({
  action: 'retrieve',
  key: 'research/exploration/*',
  namespace: 'findings'
})

mcp__claude-flow__memory_usage({
  action: 'retrieve',
  key: 'research/validation/*',
  namespace: 'evidence'
})

mcp__claude-flow__memory_usage({
  action: 'retrieve',
  key: 'research/metrics/*',
  namespace: 'measurements'
})
```

**Findings Count**:
- Explorations: [count]
- Validations: [count]
- Measurements: [count]
- Documented: [count]
- **Total**: [count]

### Step 2: Categorization

Group findings by category:

**By Type**:
- Architecture findings: [count]
- Pattern findings: [count]
- Limitation findings: [count]
- Capability findings: [count]
- Bug findings: [count]

**By Impact**:
- Critical: [count]
- High: [count]
- Medium: [count]
- Low: [count]

**By Confidence**:
- High (>80%): [count]
- Medium (50-80%): [count]
- Low (<50%): [count]

### Step 3: Pattern Recognition

Identify meta-patterns across findings:

**Emerging Themes**:
1. [Theme 1]: [How many findings support this]
2. [Theme 2]: [How many findings support this]
3. [Theme 3]: [How many findings support this]

**Contradictions**:
- Finding A says: [claim]
- Finding B says: [contradictory claim]
- Resolution: [How to reconcile]

**Gaps Identified**:
1. [Unexplored area 1]
2. [Unexplored area 2]
3. [Unexplored area 3]

### Step 4: Knowledge Graph Construction

Build relationships between findings:

```
Finding A (capability: commands spawn agents)
  ├─ enables → Finding B (pattern: agent orchestration)
  ├─ requires → Finding C (architecture: Task tool)
  └─ conflicts with → Finding D (limitation: no direct invocation)
```

**Key Relationships**:
- **Enables**: Finding A makes Finding B possible
- **Requires**: Finding A depends on Finding B
- **Extends**: Finding A builds on Finding B
- **Conflicts**: Finding A contradicts Finding B
- **Validates**: Finding A confirms Finding B

Create adjacency list:

```json
{
  "finding-1": {
    "enables": ["finding-2", "finding-5"],
    "requires": ["finding-3"],
    "validated_by": ["finding-4"]
  }
}
```

### Step 5: Insight Extraction

**High-Level Insights**:

1. **Architectural Principles**:
   - [Principle 1 derived from findings]
   - [Principle 2 derived from findings]

2. **Design Patterns**:
   - [Pattern 1 observed across findings]
   - [Pattern 2 observed across findings]

3. **Limitations & Boundaries**:
   - [What the system CAN'T do]
   - [Why these limitations exist]

4. **Best Practices**:
   - [Practice 1 supported by evidence]
   - [Practice 2 supported by evidence]

5. **Anti-Patterns**:
   - [Anti-pattern 1 to avoid]
   - [Why it fails (evidence)]

### Step 6: Capability Map

Create comprehensive capability map:

```markdown
## Capability Matrix

| Capability | Supported | Confidence | Evidence | Workarounds |
|------------|-----------|------------|----------|-------------|
| [Cap 1] | ✅ | 95% | [findings] | N/A |
| [Cap 2] | ⚠️ | 70% | [findings] | [method] |
| [Cap 3] | ❌ | 90% | [findings] | [alt approach] |
```

### Step 7: Recommendations

**Immediate Actions**:
1. [Action 1] - Based on [finding(s)]
2. [Action 2] - Based on [finding(s)]

**Short-term (1-2 weeks)**:
1. [Action 1] - Based on [finding(s)]
2. [Action 2] - Based on [finding(s)]

**Long-term (1+ months)**:
1. [Action 1] - Based on [finding(s)]
2. [Action 2] - Based on [finding(s)]

**Future Research**:
1. [Research area 1] - Priority: HIGH/MEDIUM/LOW
2. [Research area 2] - Priority: HIGH/MEDIUM/LOW

### Step 8: Deliverable Creation

Generate synthesis deliverable:

**Synthesis Report Structure**:

```markdown
# Research Synthesis Report

**Scope**: $scope
**Date**: [ISO-8601]
**Findings Analyzed**: [count]

## Executive Summary
[2-3 paragraph overview]

## Key Findings
1. [Finding 1] (Impact: HIGH, Confidence: 95%)
2. [Finding 2] (Impact: HIGH, Confidence: 90%)
3. [Finding 3] (Impact: MEDIUM, Confidence: 85%)

## Architectural Insights
[Architecture understanding gained]

## Pattern Library
[Documented patterns]

## Limitation Catalog
[Known limitations with workarounds]

## Capability Map
[What can/can't be done]

## Recommendations
[Actionable next steps]

## Open Questions
[What remains unknown]

## Appendix: Evidence Index
[References to all findings]
```

### Step 9: Quality Validation

**Synthesis Quality Checklist**:

- [ ] All findings retrieved from memory
- [ ] Findings categorized comprehensively
- [ ] Patterns identified across findings
- [ ] Contradictions resolved or noted
- [ ] Knowledge graph constructed
- [ ] High-level insights extracted
- [ ] Capability map complete
- [ ] Recommendations prioritized
- [ ] Open questions documented

**Evidence Quality**:
- Average confidence: [0-100%]
- High-confidence findings: [count/total]
- Validated findings: [count/total]
- Single-source findings: [count/total]

### Step 10: Storage & Distribution

Store synthesis:

```javascript
mcp__claude-flow__memory_usage({
  action: 'store',
  key: 'research/synthesis/' + Date.now(),
  namespace: 'knowledge',
  value: JSON.stringify({
    scope: "$scope",
    findings_count: [count],
    themes: ["theme1", "theme2"],
    insights: ["insight1", "insight2"],
    capabilities: {
      "cap1": { supported: true, confidence: 0.95 }
    },
    recommendations: [
      { action: "...", priority: "HIGH", based_on: ["finding1"] }
    ],
    open_questions: ["question1", "question2"],
    timestamp: new Date().toISOString()
  })
})
```

Write synthesis report:

```javascript
Write({
  file_path: '/home/user/unrdf/research/synthesis-' + Date.now() + '.md',
  content: [synthesis report markdown]
})
```

## Synthesis Output Formats

### Executive Briefing (for stakeholders)

```markdown
# Research Briefing: [Topic]

**Bottom Line Up Front**: [1 sentence key takeaway]

## What We Learned
1. [Key point 1]
2. [Key point 2]
3. [Key point 3]

## What This Means
[Business/technical implications]

## What To Do
1. [Immediate action]
2. [Short-term action]
3. [Long-term action]
```

### Technical Deep Dive (for developers)

```markdown
# Technical Research Report: [Topic]

## Architectural Model
[Detailed architecture understanding]

## Pattern Catalog
### Pattern 1: [Name]
**Problem**: ...
**Solution**: ...
**Implementation**: ...

## Capability Matrix
[Complete capability map]

## Code Examples
[Runnable examples]

## Best Practices
[Detailed recommendations]
```

### Research Continuation (for researchers)

```markdown
# Research Continuation Guide: [Topic]

## What We Know
[High-confidence findings]

## What We Suspect
[Medium-confidence findings]

## What We Don't Know
[Open questions]

## Next Research Directions
1. [Direction 1] - Effort: [H/M/L], Impact: [H/M/L]
2. [Direction 2] - Effort: [H/M/L], Impact: [H/M/L]

## Methodology Recommendations
[What worked well, what didn't]
```

## Success Criteria

- [ ] ≥10 findings retrieved and analyzed
- [ ] Findings categorized by type, impact, confidence
- [ ] ≥3 meta-patterns identified
- [ ] Knowledge graph constructed
- [ ] ≥5 actionable insights extracted
- [ ] Capability map complete
- [ ] Recommendations prioritized
- [ ] Synthesis report written and stored
- [ ] Quality validation passed

## Output Format

Provide synthesis summary:

```markdown
## Synthesis Summary

**Scope**: $scope
**Findings**: [count] analyzed
**Confidence**: [average]%

### Top Insights
1. [Insight 1]
2. [Insight 2]
3. [Insight 3]

### Capability Highlights
✅ [Confirmed capability 1]
✅ [Confirmed capability 2]
❌ [Confirmed limitation 1]
⚠️ [Partial capability 1]

### Recommended Actions
1. [Action 1 - Priority: HIGH]
2. [Action 2 - Priority: MEDIUM]

### Full Report
See: [path to synthesis report]
```

---

**Usage Examples**:

```
/research/synthesize
/research/synthesize topic-specific
/research/synthesize cross-agent
/research/synthesize all
```

---

**Synthesis Quality Principles**:

1. **Evidence-Based**: Every claim backed by findings
2. **Confidence-Weighted**: Prioritize high-confidence findings
3. **Contradiction-Aware**: Explicitly resolve conflicts
4. **Gap-Conscious**: Identify what's unknown
5. **Actionable**: Provide clear next steps
