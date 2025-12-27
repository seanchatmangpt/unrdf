# How to Implement Multi-Agent Concurrency

Multi-agent concurrency enables multiple Claude instances to operate simultaneously through deterministic shard merge with conflict resolution by law (Λ), not negotiation.

## Problem

You need to:

- Run multiple agents concurrently without conflicts
- Define clear scopes for each agent's operations
- Merge deltas from different agents deterministically
- Resolve conflicts through predefined rules
- Track causality across agents

## Solution

Use agent shards with scope isolation and deterministic merge operations.

## Step-by-Step

### 1. Create Agent Shards

```javascript
import { createShard } from '@unrdf/kgc-claude';

// Agent 1: Works on components
const componentsShard = createShard(
  'agent-components',
  {
    files: ['src/components/**', 'src/ui/**'],
    graphs: ['http://kgc.io/Universe'],
    subjects: ['http://kgc.io/component/*'],
    predicates: [],
  },
  {
    priority: 1, // Used for conflict resolution
  }
);

// Agent 2: Works on services
const servicesShard = createShard(
  'agent-services',
  {
    files: ['src/services/**', 'src/api/**'],
    graphs: ['http://kgc.io/Universe'],
    subjects: ['http://kgc.io/service/*'],
    predicates: [],
  },
  {
    priority: 1,
  }
);

console.log('Shards created:');
console.log('Components shard:', componentsShard.id);
console.log('Services shard:', servicesShard.id);
```

### 2. Create Multi-Agent Session

```javascript
import { createMultiAgentSession } from '@unrdf/kgc-claude';

const session = createMultiAgentSession([
  { agentId: 'agent-components', shard: componentsShard },
  { agentId: 'agent-services', shard: servicesShard },
]);

console.log('Multi-agent session created');
console.log(
  'Shards:',
  session.shards.map(s => s.agentId)
);
```

### 3. Add Deltas to Shards

```javascript
import { addDelta } from '@unrdf/kgc-claude';

// Agent 1 makes changes
addDelta(componentsShard.id, {
  type: 'add',
  target: 'src/components/Button.tsx',
  after: { component: 'Button', exported: true },
});

addDelta(componentsShard.id, {
  type: 'modify',
  target: 'src/components/index.ts',
  before: 'export { Card }',
  after: 'export { Card, Button }',
});

// Agent 2 makes changes
addDelta(servicesShard.id, {
  type: 'add',
  target: 'src/services/auth.ts',
  after: { service: 'AuthService', methods: ['login', 'logout'] },
});

console.log('Deltas added to shards');
```

### 4. Check for Shard Overlap

```javascript
import { checkShardOverlap } from '@unrdf/kgc-claude';

const overlap = checkShardOverlap(componentsShard.id, servicesShard.id);

if (overlap.hasOverlap) {
  console.warn('Shards have overlapping scopes:');
  console.warn('Files:', overlap.files);
  console.warn('Graphs:', overlap.graphs);
  console.warn('Subjects:', overlap.subjects);
} else {
  console.log('No overlap, safe to merge');
}
```

### 5. Merge Deltas

```javascript
import { mergeDeltas } from '@unrdf/kgc-claude';

const mergeResult = await mergeDeltas([componentsShard.id, servicesShard.id]);

console.log('Merge completed:');
console.log('Merged deltas:', mergeResult.merged.length);
console.log('Conflicts:', mergeResult.conflicts.length);

// Review conflicts
mergeResult.conflicts.forEach((conflict, index) => {
  console.log(`\nConflict ${index + 1}:`);
  console.log('  Delta 1:', conflict.delta1.delta.target);
  console.log('  Delta 2:', conflict.delta2.delta.target);
  console.log('  Resolution:', conflict.resolution);
  console.log('  Reason:', conflict.reason);
});
```

### 6. Apply Merged Deltas

```javascript
import { applyMergedDeltas } from '@unrdf/kgc-claude';

const applyResult = await applyMergedDeltas(store, mergeResult.merged);

console.log('Applied deltas:', applyResult.applied);
console.log('Failed:', applyResult.failed);
console.log('Receipt:', applyResult.receiptHash);
```

### 7. Get Pending Deltas

```javascript
import { getPendingDeltas } from '@unrdf/kgc-claude';

const pending = getPendingDeltas(componentsShard.id);

console.log('Pending deltas for components shard:', pending.length);
pending.forEach((delta, index) => {
  console.log(`${index + 1}. ${delta.delta.type} ${delta.delta.target}`);
});
```

## Advanced Patterns

### Priority-Based Conflict Resolution

```javascript
function createPriorityResolver(highPriorityAgents) {
  return conflict => {
    const agent1Priority = highPriorityAgents.includes(conflict.delta1.agentId);
    const agent2Priority = highPriorityAgents.includes(conflict.delta2.agentId);

    if (agent1Priority && !agent2Priority) {
      return 'delta1_wins';
    } else if (agent2Priority && !agent1Priority) {
      return 'delta2_wins';
    } else {
      // Same priority, use timestamp
      return conflict.delta1.t_ns < conflict.delta2.t_ns ? 'delta1_wins' : 'delta2_wins';
    }
  };
}

// Use custom resolver
const customMerge = await mergeDeltas([shard1.id, shard2.id], {
  resolver: createPriorityResolver(['agent-critical']),
});
```

### Scoped Merge (Partial)

```javascript
async function mergeShardsInScope(shardIds, filePattern) {
  const pendingDeltas = shardIds.flatMap(id => getPendingDeltas(id));

  // Filter to scope
  const scopedDeltas = pendingDeltas.filter(delta => {
    if (!delta.delta.target.includes) return false;
    return filePattern.test(delta.delta.target);
  });

  console.log(`Merging ${scopedDeltas.length} deltas in scope`);

  // Merge filtered deltas
  // ... implement custom merge logic ...

  return scopedDeltas;
}

// Merge only TypeScript files
await mergeShardsInScope([shard1.id, shard2.id], /\.tsx?$/);
```

### Incremental Merge

```javascript
async function incrementalMerge(shardIds, batchSize = 10) {
  const allResults = [];

  for (const shardId of shardIds) {
    const pending = getPendingDeltas(shardId);

    // Merge in batches
    for (let i = 0; i < pending.length; i += batchSize) {
      const batch = pending.slice(i, i + batchSize);

      const tempShard = createShard(`temp-${Date.now()}`, {});
      batch.forEach(delta => addDelta(tempShard.id, delta.delta));

      const mergeResult = await mergeDeltas([tempShard.id]);
      await applyMergedDeltas(store, mergeResult.merged);

      allResults.push(mergeResult);

      console.log(`Merged batch ${Math.floor(i / batchSize) + 1}`);
    }
  }

  return allResults;
}
```

### Vector Clock Synchronization

```javascript
import { VectorClock } from '@unrdf/kgc-4d';

class VectorClockSync {
  constructor() {
    this.clocks = new Map();
  }

  tick(agentId) {
    if (!this.clocks.has(agentId)) {
      this.clocks.set(agentId, new VectorClock(agentId));
    }

    const clock = this.clocks.get(agentId);
    clock.tick();
    return clock;
  }

  merge(agentId, otherClock) {
    if (!this.clocks.has(agentId)) {
      this.clocks.set(agentId, new VectorClock(agentId));
    }

    const clock = this.clocks.get(agentId);
    clock.merge(otherClock);
    return clock;
  }

  happensBefore(agentId1, agentId2) {
    const clock1 = this.clocks.get(agentId1);
    const clock2 = this.clocks.get(agentId2);

    if (!clock1 || !clock2) return null;

    return clock1.happensBefore(clock2);
  }
}

const sync = new VectorClockSync();

// Agent 1 makes change
const clock1 = sync.tick('agent-1');
addDelta(
  shard1.id,
  {
    type: 'add',
    target: 'file1.ts',
    after: {},
  },
  { vectorClock: clock1 }
);

// Agent 2 sees agent 1's change
const mergedClock = sync.merge('agent-2', clock1);
const clock2 = sync.tick('agent-2');

// Check causality
console.log('Agent 1 before Agent 2:', sync.happensBefore('agent-1', 'agent-2'));
```

### Hierarchical Sharding

```javascript
class HierarchicalShardManager {
  constructor() {
    this.hierarchy = new Map();
  }

  createParentShard(parentId, scope, childShards = []) {
    const parent = createShard(parentId, scope);

    this.hierarchy.set(parentId, {
      shard: parent,
      children: childShards,
    });

    return parent;
  }

  addChildShard(parentId, childShard) {
    const parent = this.hierarchy.get(parentId);
    if (parent) {
      parent.children.push(childShard);
    }
  }

  async mergeHierarchy(parentId) {
    const parent = this.hierarchy.get(parentId);
    if (!parent) throw new Error('Parent not found');

    // Merge children first
    const childMerges = await Promise.all(parent.children.map(child => mergeDeltas([child.id])));

    // Then merge parent
    const parentMerge = await mergeDeltas([parent.shard.id]);

    // Combine results
    return {
      parent: parentMerge,
      children: childMerges,
    };
  }
}

const manager = new HierarchicalShardManager();

const parentShard = manager.createParentShard('project', {
  files: ['src/**'],
});

const frontendShard = createShard('frontend', { files: ['src/frontend/**'] });
const backendShard = createShard('backend', { files: ['src/backend/**'] });

manager.addChildShard('project', frontendShard);
manager.addChildShard('project', backendShard);

const hierarchyMerge = await manager.mergeHierarchy('project');
```

### Conflict Logging and Replay

```javascript
class ConflictLogger {
  constructor() {
    this.log = [];
  }

  logMerge(mergeResult) {
    this.log.push({
      timestamp: Date.now(),
      merged: mergeResult.merged.length,
      conflicts: mergeResult.conflicts.map(c => ({
        delta1: c.delta1.delta.target,
        delta2: c.delta2.delta.target,
        resolution: c.resolution,
        reason: c.reason,
      })),
    });
  }

  getConflictStats() {
    const total = this.log.length;
    const totalConflicts = this.log.reduce((sum, m) => sum + m.conflicts.length, 0);
    const avgConflicts = total > 0 ? totalConflicts / total : 0;

    return { total, totalConflicts, avgConflicts };
  }

  exportLog() {
    return JSON.stringify(this.log, null, 2);
  }
}

const logger = new ConflictLogger();

// Use with merge
const mergeResult = await mergeDeltas([shard1.id, shard2.id]);
logger.logMerge(mergeResult);

console.log('Conflict stats:', logger.getConflictStats());
```

## Best Practices

1. **Define non-overlapping scopes**: Minimize conflicts through clear boundaries
2. **Use priority wisely**: Higher priority for critical agents
3. **Check overlap before merge**: Detect potential conflicts early
4. **Review merge conflicts**: Understand resolution rationale
5. **Track causality with vector clocks**: Maintain happens-before relations
6. **Batch merges for performance**: Don't merge after every delta
7. **Log conflicts for analysis**: Learn from merge patterns

## Common Issues

**Issue**: High conflict rate

- **Cause**: Overlapping shard scopes
- **Fix**: Refine scopes to be more specific, non-overlapping

**Issue**: Merge produces unexpected results

- **Cause**: Conflict resolution rules not clear
- **Fix**: Define explicit resolution laws (Λ), test thoroughly

**Issue**: Agent changes not visible to others

- **Cause**: Deltas not merged yet
- **Fix**: Trigger periodic merges, or merge before dependent operations

**Issue**: Causality violations

- **Cause**: No vector clock tracking
- **Fix**: Use VectorClock from @unrdf/kgc-4d for all deltas

## See Also

- [API Reference: Shard Merge](../reference.md#shard-merge)
- [Explanation: Why Multi-Agent Sharding](../explanation.md#multi-agent-concurrency)
- [Tutorial: Step 9](../tutorial.md#step-9-multi-agent-concurrency)
