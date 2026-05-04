# Resource Allocator Design & Proofs

**Agent 4 Deliverable**: Deterministic work item allocation with capacity guarantees

## Overview

The Resource Allocator implements deterministic scheduling for multi-agent work distribution with formal capacity proofs.

## Algorithm

### Input Types

```typescript
AllocatableWorkItem = {
  id: string,
  type: string,
  requiredCapabilities: string[],
  estimatedMemoryBytes: number,
  priority: number, // Not used in current implementation
}

AgentCapacity = {
  id: string,
  maxConcurrent: number,
  maxMemoryBytes?: number,
  capabilities: string[],
}
```

### Core Algorithm

```
function allocate(workItems: WorkItem[], agents: AgentCapacity[]) → AllocationResult

1. VALIDATE inputs (Zod schemas)

2. SORT work items lexicographically by ID
   // Ensures determinism: same items → same order

3. INITIALIZE agent state
   for each agent:
     state[agent.id] = {
       capacity: agent.maxConcurrent,
       remaining: agent.maxConcurrent,
       capabilities: Set(agent.capabilities),
       assigned: []
     }

4. ROUND-ROBIN allocation
   agentIndex = 0
   for each item in sortedItems:
     assigned = false
     for attempts in 0..agents.length:
       currentAgent = agents[agentIndex]

       if currentAgent.remaining > 0 AND hasRequiredCapabilities(item, currentAgent):
         ASSIGN item to currentAgent
         currentAgent.remaining--
         agentIndex = (agentIndex + 1) % agents.length  // Move to next agent
         assigned = true
         break

       agentIndex = (agentIndex + 1) % agents.length  // Try next agent

     if NOT assigned:
       ADD item to waitlist

5. CALCULATE metrics
   - Per-agent utilization: (assigned / capacity) * 100
   - System utilization: (totalAssigned / totalCapacity) * 100
   - Waitlist depth: length of waitlist

6. RETURN AllocationResult
```

## Formal Guarantees

### 1. Determinism

**Claim**: For identical inputs, `allocate` produces identical outputs.

**Proof**:

- ✅ Pure function (no side effects, no external state)
- ✅ Lexicographic sort is deterministic (same items → same order)
- ✅ Round-robin iteration is deterministic (same starting index, same sequence)
- ✅ No randomness, no timestamps, no non-deterministic operations

**Test**: `test/Allocator.test.mjs::determinism proofs`

```javascript
const result1 = allocate(items, agents);
const result2 = allocate(items, agents);
assert(result1.assignments === result2.assignments); // ✅
```

### 2. Commutativity (Input Order Independence)

**Claim**: Input order of work items doesn't affect assignment.

**Proof**:

- ✅ Items are sorted lexicographically before allocation
- ✅ `sortedItems = sort(workItems)` normalizes input order
- ✅ `allocate([A, B, C], agents) === allocate([C, A, B], agents)`

**Test**: `test/Allocator.test.mjs::commutativity`

```javascript
allocate([wi - 003, wi - 001, wi - 002], agents) ===
  allocate([wi - 001, wi - 002, wi - 003], agents); // ✅
```

### 3. Capacity Safety (No Over-Allocation)

**Claim**: ∀ agent_i: assigned(agent_i) ≤ capacity(agent_i)

**Proof**:

- ✅ `state.remaining` is initialized to `agent.maxConcurrent`
- ✅ Assignment only occurs if `state.remaining > 0`
- ✅ `state.remaining--` after each assignment
- ✅ Once `remaining === 0`, agent receives no more assignments

**Test**: `test/Allocator.test.mjs::capacity proofs`

```javascript
agents = [{ id: 'a1', maxConcurrent: 2 }];
items = [wi - 1, wi - 2, wi - 3]; // 3 items

result = allocate(items, agents);
assert(countAssigned('a1') <= 2); // ✅
assert(result.waitlist.length === 1); // wi-3 waitlisted ✅
```

### 4. Completeness

**Claim**: ∀ item: item ∈ assignments ∨ item ∈ waitlist

**Proof**:

- ✅ Loop iterates over ALL items in `sortedItems`
- ✅ For each item: either assigned OR added to waitlist
- ✅ No items can be dropped or lost

**Test**: `test/Allocator.test.mjs::validation`

```javascript
result = allocate(items, agents);
assert(result.totalAssigned + result.totalWaitlisted === items.length); // ✅
```

### 5. Capability Enforcement

**Claim**: ∀ assignment(item, agent): item.requiredCapabilities ⊆ agent.capabilities

**Proof**:

- ✅ Assignment condition: `item.requiredCapabilities.every(cap => agent.capabilities.has(cap))`
- ✅ If capabilities don't match, assignment is skipped
- ✅ If no agent has capabilities, item goes to waitlist

**Test**: `test/Allocator.test.mjs::capability matching`

```javascript
item = { id: 'wi-1', requiredCapabilities: ['rust'] };
agents = [{ id: 'a1', capabilities: ['javascript'] }];

result = allocate([item], agents);
assert(result.totalAssigned === 0); // ✅ Not assigned
assert(result.waitlist.includes('wi-1')); // ✅ Waitlisted
```

## Complexity Analysis

### Time Complexity

- **Sort**: O(n log n) where n = number of work items
- **Allocation**: O(n × m) where m = number of agents
  - For each item (n), try up to m agents
- **Total**: O(n log n + n × m) = O(n × m) for typical cases (m ≪ n)

### Space Complexity

- **Agent state**: O(m)
- **Assignments**: O(n)
- **Waitlist**: O(n)
- **Total**: O(n + m)

## Scheduling Properties

### Round-Robin Fairness

- Agents receive items in circular order
- Prevents single agent from monopolizing work
- Load balancing across available capacity

### Lexicographic Priority

- Work items sorted by ID (e.g., `wi-001`, `wi-002`, `wi-003`)
- No explicit priority field (future enhancement)
- Deterministic ordering without bias

### Exhaustion Handling

**Scenario**: 10 work items, 3 agent slots

```javascript
items = [wi-001, ..., wi-010];
agents = [
  { id: 'a1', maxConcurrent: 1 },
  { id: 'a2', maxConcurrent: 1 },
  { id: 'a3', maxConcurrent: 1 },
];

result = allocate(items, agents);

// Assignments (round-robin):
// wi-001 → a1
// wi-002 → a2
// wi-003 → a3
// Capacity exhausted

// Waitlist:
// [wi-004, wi-005, ..., wi-010] (7 items)

assert(result.totalAssigned === 3); // ✅
assert(result.totalWaitlisted === 7); // ✅
assert(systemUtilization(result) === 100); // ✅ Fully utilized
```

## Metrics

### Per-Agent Utilization

```
utilization(agent_i) = (assigned_i / capacity_i) × 100
```

Example:

- Agent has `maxConcurrent: 4`
- Assigned 2 items
- Utilization: (2 / 4) × 100 = 50%

### System Utilization

```
systemUtilization = (totalAssigned / totalCapacity) × 100
```

Example:

- Total capacity: 10 slots
- Total assigned: 7 items
- System utilization: 70%

### Waitlist Depth

```
waitlistDepth = count(waitlist)
```

Indicates pressure on system capacity. High depth → need more agents or capacity.

## Validation

The `validateAllocation` function provides post-hoc verification:

1. **Item Count**: All items accounted for (assigned + waitlisted)
2. **No Duplicates**: Each item assigned at most once
3. **Capacity Bounds**: No agent exceeds `maxConcurrent`
4. **Capability Match**: All assignments satisfy capability requirements

## Future Enhancements

1. **Priority Scheduling**: Use `priority` field for weighted allocation
2. **Memory Budget**: Enforce `maxMemoryBytes` per agent
3. **Preemption**: Allow high-priority items to preempt low-priority
4. **Dynamic Re-allocation**: Adjust assignments based on execution time
5. **Affinity Rules**: Prefer certain agent-item pairings

## Receipt Generation

**Proof Target**: `npm run test:allocator --determinism`

```bash
# Run allocation tests 5 times
for i in {1..5}; do
  echo "=== Run $i ==="
  npm run test:allocator
done

# Expected: All runs produce identical assignments
# Proof: Deterministic algorithm with no non-deterministic inputs
```

## References

- [async-workflow.mjs](../../../kgc-claude/src/async-workflow.mjs) - WorkItem schema
- [Allocator.mjs](../src/Allocator.mjs) - Implementation
- [Allocator.test.mjs](../test/Allocator.test.mjs) - Proofs

---

**Delivered**: 2025-12-27
**Agent**: Agent 4 - Resource Allocator & Capacity Proofs
**Status**: ✅ Complete with formal guarantees
