# How-to: Execute Tasks in Parallel

Maximize throughput by running multiple agents concurrently.

## Problem

You have multiple independent tasks that could run simultaneously, but you're running them sequentially.

## Solution

Spawn all independent agents in a single message using multiple Task tool calls.

## Steps

### Step 1: Identify Independent Tasks

Tasks are independent when:
- They don't depend on each other's output
- They operate on different files/areas
- Order doesn't matter

```
✅ Independent:
- Analyze frontend code
- Analyze backend code
- Check test coverage

❌ Dependent:
- Generate code → Test code → Review code
```

### Step 2: Select Appropriate Agents

Match agent types to tasks:

| Task | Agent Type |
|------|------------|
| Code analysis | code-analyzer |
| Research | researcher |
| Test writing | tester |
| Implementation | coder |
| Architecture review | system-architect |

### Step 3: Spawn in Single Message

```javascript
// ONE message with multiple Task calls
Task("Analyze frontend",
     "Analyze React components in src/components/...",
     "code-analyzer")

Task("Analyze backend",
     "Analyze Express routes in src/routes/...",
     "code-analyzer")

Task("Check coverage",
     "Review test coverage and identify gaps...",
     "tester")
```

### Step 4: Aggregate Results

All agents return in parallel. Synthesize findings:

```markdown
## Frontend Analysis
[Agent 1 results]

## Backend Analysis
[Agent 2 results]

## Coverage Report
[Agent 3 results]

## Combined Insights
- Cross-cutting concerns
- Shared patterns
- Recommendations
```

## Example: Full-Stack Code Review

```javascript
// Spawn 5 agents for comprehensive review
Task("Security audit",
     "Check for OWASP Top 10 vulnerabilities...",
     "code-analyzer")

Task("Performance review",
     "Identify performance bottlenecks...",
     "code-analyzer")

Task("API consistency",
     "Verify API contracts and documentation...",
     "backend-dev")

Task("Test coverage",
     "Analyze test coverage and quality...",
     "tester")

Task("Architecture review",
     "Evaluate overall architecture...",
     "system-architect")
```

## Measuring Parallel Throughput

### Baseline (Sequential)
```
Task 1: 30s
Task 2: 30s
Task 3: 30s
Total: 90s
```

### With Parallel Execution
```
Task 1, 2, 3 concurrent
Total: ~35s (limited by slowest + overhead)
```

**Throughput increase: 2.5x**

## When NOT to Parallelize

- Tasks with dependencies
- Tasks that modify same files
- Tasks requiring sequential feedback
- Resource-constrained environments

## Troubleshooting

### Problem: One Agent Fails
Solution: Other agents complete normally. Handle failure separately.

### Problem: Results Don't Synthesize
Solution: Use consistent output format instructions in prompts.

### Problem: Context Overflow
Solution: Keep agent prompts focused; use Explore agent for broad searches.

## Related

- [Tutorial: Your First Subagent](../tutorials/01-first-subagent.md)
- [Reference: Agent Types](../reference/subagent-types.md)
- [Explanation: Delegation Patterns](../explanations/delegation-patterns.md)
