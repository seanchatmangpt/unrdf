# Tutorial: Your First Subagent

Learn how to spawn your first specialized subagent in Claude Code.

## Prerequisites

- Claude Code CLI installed
- Basic familiarity with Claude Code interface

## What You'll Learn

- How to use the Task tool
- Available agent types
- Spawning agents for specialized work

## Step 1: Understand Subagent Types

Claude Code provides specialized agents for different tasks:

```
┌─────────────────┬───────────────────────────────────┐
│ Agent Type      │ Purpose                           │
├─────────────────┼───────────────────────────────────┤
│ coder           │ Write implementation code         │
│ tester          │ Write and run tests               │
│ reviewer        │ Code review and analysis          │
│ researcher      │ Information gathering             │
│ planner         │ Task planning and breakdown       │
│ Explore         │ Codebase exploration              │
│ backend-dev     │ Backend API development           │
│ code-analyzer   │ Static analysis                   │
└─────────────────┴───────────────────────────────────┘
```

## Step 2: Spawn a Simple Agent

The Task tool spawns agents. Here's the pattern:

```
Task("description", "detailed prompt", "agent-type")
```

### Example: Research Agent

Ask a researcher agent to find information:

```
Use the Task tool with:
- description: "Find auth implementations"
- prompt: "Search the codebase for authentication implementations.
          Look for login, JWT, session handling, and OAuth patterns.
          Report back with file locations and implementation approach."
- subagent_type: "researcher"
```

## Step 3: Spawn Multiple Agents in Parallel

For independent tasks, spawn multiple agents in ONE message:

```
// All three agents run concurrently
Task("Analyze models", "Review all data models...", "researcher")
Task("Review routes", "Review API routes...", "researcher")
Task("Check tests", "Review test coverage...", "tester")
```

## Step 4: Handle Agent Results

Agents return their findings in a single message. You'll receive:

- Summary of work completed
- Key findings
- File references
- Recommendations

## Try It Yourself

1. Open Claude Code
2. Ask: "Use a researcher agent to find how error handling works in this codebase"
3. Observe the agent spawn and return results

## Next Steps

- [Tutorial: Multiple Agent Coordination](./multi-agent.md)
- [Reference: All Agent Types](../reference/subagent-types.md)
- [How-to: Parallel Task Execution](../how-to/parallel-execution.md)

## Key Takeaways

1. **Task tool** spawns specialized agents
2. **Agent types** match expertise to work
3. **Parallel spawning** in single message maximizes throughput
4. **Results** come back in one message per agent
