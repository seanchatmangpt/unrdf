---
description: Systematically explore a capability area
arguments:
  - name: topic
    description: The capability to explore
    required: true
  - name: depth
    description: Exploration depth (shallow, normal, deep)
    required: false
    default: 'normal'
---

# Research Exploration: $topic

## Depth Level: $depth

Execute systematic exploration protocol for "$topic".

## Phase 1: Discovery

Search for all mentions of $topic:

```bash
timeout 5s grep -r "$topic" . --include="*.md" --include="*.js" --include="*.mjs" -i | head -50
```

## Phase 2: Documentation Review

Find official documentation:
- README files
- API documentation
- Architecture docs
- Tutorial content

Use Glob and Grep tools to locate relevant files:

```javascript
Glob({ pattern: "**/*$topic*.md" })
Grep({ pattern: "$topic", glob: "*.md", output_mode: "files_with_matches" })
```

## Phase 3: Code Analysis

Locate implementations:
- Source files
- Test files
- Configuration files
- Examples

```javascript
Grep({ pattern: "$topic", type: "js", output_mode: "files_with_matches" })
```

## Phase 4: Testing

Create minimal viable example based on findings.

Execute and capture output with timeout:

```bash
timeout 5s [test command]
```

## Phase 5: Edge Cases

Based on depth level "$depth":

- **shallow**: Test 2-3 basic scenarios
- **normal**: Test 5-7 common scenarios + 2 edge cases
- **deep**: Test 10+ scenarios including error paths

## Phase 6: Synthesis

Report findings:

```json
{
  "topic": "$topic",
  "depth": "$depth",
  "timestamp": "ISO-8601 timestamp",
  "features_found": ["feature1", "feature2"],
  "configurations": {
    "option1": "value1"
  },
  "edge_cases": ["case1", "case2"],
  "examples": [
    {
      "description": "Example 1",
      "code": "...",
      "output": "..."
    }
  ],
  "open_questions": ["question1"]
}
```

Store in coordination memory:

```javascript
mcp__claude-flow__memory_usage({
  action: 'store',
  key: 'research/exploration/$topic',
  namespace: 'findings',
  value: JSON.stringify(results)
})
```

## Success Criteria

- [ ] All discovery commands executed with output captured
- [ ] Documentation reviewed and summarized
- [ ] Code examples found and tested
- [ ] Edge cases tested based on depth level
- [ ] Findings stored in structured JSON format
- [ ] Open questions identified for future research

## Output Format

Provide comprehensive report with:

1. **Summary**: 2-3 sentences on what was learned
2. **Evidence**: Links to files, command outputs, test results
3. **Confidence**: Low/Medium/High based on evidence quality
4. **Next Steps**: Recommended follow-up research

---

**Usage Examples**:

```
/research/explore "slash commands"
/research/explore "agent spawning" deep
/research/explore "MCP protocol" shallow
```
