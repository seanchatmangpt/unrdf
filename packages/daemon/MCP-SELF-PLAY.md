# MCP Self-Play Loop Engine

Autonomous feedback loops where MCP tools call each other to accomplish goals without explicit human direction.

## Overview

The MCP Self-Play system enables autonomous agents to:

1. **Chain tools together** — Each tool's output feeds into the next tool's input
2. **Learn from feedback** — Rewards/penalties guide future tool selection
3. **Adapt strategies** — Multiple decision policies (explore, greedy, random)
4. **Track episodes** — Full audit trail of autonomous decisions and results

## Architecture

```
┌─────────────────┐
│  Tool Registry  │
└────────┬────────┘
         │
    ┌────▼──────────────────┐
    │  SelfPlayAgent         │
    │  ├─ decisionFn         │  ← Policy (explore/greedy/random)
    │  └─ toolRegistry       │
    └────┬──────────────────┘
         │
    ┌────▼──────────────────┐
    │  SelfPlayEpisode       │
    │  ├─ steps              │  ← Tool executions
    │  ├─ feedback           │  ← Rewards/penalties
    │  └─ metrics            │  ← Performance data
    └────────────────────────┘
```

## Usage

### CLI

Run autonomous exploration on an RDF graph:

```bash
# Default: explore pattern (Query → Hooks → Query)
unrdf mcp self-play --graph data.ttl --hooks-config hooks.json

# With greedy policy (maximize feedback)
unrdf mcp self-play --graph data.ttl --policy greedy --episodes 5

# With random exploration
unrdf mcp self-play --graph data.ttl --policy random --steps-per-episode 20

# Output results to file
unrdf mcp self-play --graph data.ttl --output results.json --verbose
```

### Programmatic

```javascript
import { runSelfPlayLoop, SelfPlayPolicies } from '@unrdf/daemon/mcp-self-play';

const toolRegistry = {
  tool_a: { handler: async (input) => ({ ... }) },
  tool_b: { handler: async (input) => ({ ... }) },
};

const result = await runSelfPlayLoop(toolRegistry, {
  initialContext: { graphFile: 'data.ttl' },
  decisionPolicy: SelfPlayPolicies.explorePatternDecision,
  episodeCount: 3,
  maxStepsPerEpisode: 10,
});

console.log('Best episode:', result.stats.bestEpisodeId);
console.log('Success rate:', result.stats.successRate);
```

## Decision Policies

### Explore Pattern (Default)

Deterministic sequence: Query → Hooks → Query

**Use case:** Understanding graph structure and applying transformations

```
Step 0: unrdf_graph_query    (explore data)
Step 1: unrdf_hooks_execute  (apply rules)
Step 2: unrdf_graph_query    (verify results)
```

### Greedy Feedback

Select tools with highest historical success rate

**Use case:** Optimization and refinement

```
Selects tool with max(success_rate)
Adapts as success rates change
```

### Random Exploration

Sample tools uniformly at random

**Use case:** Baseline exploration, diversity

```
Each step: random { tool_a, tool_b, tool_c, ... }
```

## Feedback System

Automatic signals based on execution:

| Event             | Signal | Reason              |
| ----------------- | ------ | ------------------- |
| Tool succeeds     | +0.1   | Progress            |
| Tool fails        | -0.5   | Blocker             |
| Max steps reached | 0.0    | Neutral termination |
| Unknown tool      | -1.0   | Invalid decision    |

Custom feedback:

```javascript
episode.recordFeedback(0.5, 'query found important data');
episode.recordFeedback(-0.2, 'hook took too long');
```

## Episode Metrics

Each episode tracks:

```javascript
{
  episodeId: 'uuid',
  totalSteps: 5,
  totalFeedback: 3,
  cumulativeFeedback: 0.8,
  avgFeedback: 0.27,
  totalDuration: 1234,  // ms
  terminated: true,
  terminationReason: 'max steps reached'
}
```

## Examples

### Example 1: Graph Analysis Loop

```javascript
const decisionFn = async (episode, previousResult) => {
  const stepCount = episode.steps.length;

  if (stepCount === 0) {
    return { toolName: 'unrdf_graph_stats', input: { file: 'data.ttl' } };
  }

  if (stepCount === 1) {
    return {
      toolName: 'unrdf_graph_query',
      input: { file: 'data.ttl', query: 'SELECT * WHERE { ?s a ?t }' },
    };
  }

  return null; // End episode
};
```

### Example 2: Data Transformation Loop

```javascript
const decisionFn = async (episode, previousResult) => {
  const stepCount = episode.steps.length;

  // Read source
  if (stepCount === 0) {
    return { toolName: 'unrdf_query', input: { file: 'source.ttl' } };
  }

  // Transform via hooks
  if (stepCount === 1) {
    return {
      toolName: 'unrdf_hooks_execute',
      input: { store: 'source.ttl', config: 'transform.json' },
    };
  }

  // Export result
  if (stepCount === 2) {
    return {
      toolName: 'unrdf_convert',
      input: { input: 'source.ttl', output: 'result.jsonld', to: 'jsonld' },
    };
  }

  return null;
};
```

## Testing

```bash
npm test test/mcp-self-play.test.mjs
```

Tests cover:

- Episode lifecycle (record, terminate, serialize)
- Agent execution (single/multiple episodes)
- Policy behaviors (explore, greedy, random)
- Error handling (tool failures, invalid tools)
- Statistics (success rate, feedback aggregation)

## Performance

Typical episode timing:

| Operation             | Time     |
| --------------------- | -------- |
| Single step execution | 5-50ms   |
| Episode (5 steps)     | 25-250ms |
| 3-episode run         | 75-750ms |

Memory:

- Episode overhead: ~1KB per step
- Agent overhead: negligible
- Tool registry: depends on tool count

## Advanced Patterns

### Multi-Goal Episodes

```javascript
const decisionFn = async (episode, previousResult) => {
  // Dynamically decide next goal based on progress
  if (previousResult?.goalAchieved) {
    episode.recordFeedback(1.0, 'goal reached');
    return null; // Next episode
  }

  return { toolName: nextGoalTool, input: { ... } };
};
```

### Learning from Episodes

```javascript
const episodes = await agent.runEpisodes(context, 10);

// Find patterns in successful episodes
episodes
  .filter(ep => ep.getMetrics().cumulativeFeedback > 0.5)
  .forEach(ep => {
    console.log('Winning sequence:');
    ep.steps.forEach(s => console.log(`  → ${s.toolName}`));
  });
```

### Adaptive Policies

```javascript
const decisionFn = async (episode, previousResult) => {
  const feedback = episode.feedback.map(f => f.signal).slice(-3);
  const trend = feedback.reduce((a, b) => a + b, 0) / feedback.length;

  // If trending positive, continue current strategy
  if (trend > 0) {
    return { toolName: lastTool, input: { ... } };
  }

  // Else try different tool
  return { toolName: alternateTool, input: { ... } };
};
```

## See Also

- `packages/daemon/src/mcp.mjs` — MCP server and tool registry
- `packages/cli/src/cli/commands/mcp.mjs` — CLI command structure
- `packages/daemon/test/mcp-self-play.test.mjs` — Full test suite
