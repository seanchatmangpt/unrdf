/**
 * Resource Allocator & Capacity Proofs
 *
 * Deterministic work item allocation with capacity tracking and scheduling guarantees.
 *
 * ## Algorithm
 *
 * 1. **Lexicographic Sort**: Work items sorted by ID (deterministic order)
 * 2. **Round-Robin Assignment**: Agents receive items in circular order
 * 3. **Capacity Enforcement**: No agent exceeds declared capacity
 * 4. **Waitlist Management**: Overflow items queued deterministically
 *
 * ## Proofs
 *
 * - **Determinism**: Identical inputs → identical outputs (pure function)
 * - **Commutativity**: Input order doesn't affect assignment (after sort)
 * - **Capacity Guarantee**: ∀ agent_i: assigned(agent_i) ≤ capacity(agent_i)
 * - **Completeness**: ∀ item: item ∈ assignments ∨ item ∈ waitlist
 *
 * @module @unrdf/kgc-substrate/allocator
 */

import { z } from 'zod';

/**
 * Agent capacity schema
 */
export const AgentCapacitySchema = z.object({
  id: z.string(),
  /** Maximum concurrent work items */
  maxConcurrent: z.number().int().positive(),
  /** Maximum memory budget in bytes */
  maxMemoryBytes: z.number().int().positive().optional(),
  /** Agent capabilities */
  capabilities: z.array(z.string()).default([]),
});

/**
 * @typedef {z.infer<typeof AgentCapacitySchema>} AgentCapacity
 */

/**
 * Work item for allocation (simplified from async-workflow)
 */
export const AllocatableWorkItemSchema = z.object({
  id: z.string(),
  type: z.string(),
  requiredCapabilities: z.array(z.string()).default([]),
  estimatedMemoryBytes: z.number().int().nonnegative().default(0),
  priority: z.number().int().default(0), // Not used in lexicographic sort
});

/**
 * @typedef {z.infer<typeof AllocatableWorkItemSchema>} AllocatableWorkItem
 */

/**
 * Allocation result schema
 */
export const AllocationResultSchema = z.object({
  assignments: z.array(
    z.object({
      agentId: z.string(),
      workItemId: z.string(),
    })
  ),
  waitlist: z.array(z.string()), // Work item IDs
  utilization: z.record(z.string(), z.number()), // agentId → utilization %
  totalCapacity: z.number().int().nonnegative(),
  totalAssigned: z.number().int().nonnegative(),
  totalWaitlisted: z.number().int().nonnegative(),
});

/**
 * @typedef {z.infer<typeof AllocationResultSchema>} AllocationResult
 */

/**
 * Allocate work items to agents using deterministic round-robin scheduling
 *
 * **Algorithm**:
 * 1. Filter agents by capability requirements
 * 2. Sort work items lexicographically by ID (deterministic order)
 * 3. Round-robin assignment respecting capacity limits
 * 4. Unassigned items → waitlist (preserving order)
 *
 * **Guarantees**:
 * - Pure function (no side effects)
 * - Deterministic (same input → same output)
 * - Capacity-respecting (no overallocation)
 * - Complete (all items assigned or waitlisted)
 *
 * @param {AllocatableWorkItem[]} workItems - Items to allocate
 * @param {AgentCapacity[]} agents - Available agents
 * @returns {AllocationResult} Allocation result with assignments and waitlist
 *
 * @example
 * const result = allocate(
 *   [
 *     { id: 'wi-003', type: 'review', requiredCapabilities: ['code-review'] },
 *     { id: 'wi-001', type: 'implement', requiredCapabilities: ['backend'] },
 *     { id: 'wi-002', type: 'test', requiredCapabilities: ['testing'] },
 *   ],
 *   [
 *     { id: 'agent-1', maxConcurrent: 2, capabilities: ['backend', 'testing'] },
 *     { id: 'agent-2', maxConcurrent: 1, capabilities: ['code-review'] },
 *   ]
 * );
 * // Deterministic assignment: wi-001→agent-1, wi-002→agent-1, wi-003→agent-2
 */
export function allocate(workItems, agents) {
  // Validate inputs
  const validatedItems = workItems.map((item) => AllocatableWorkItemSchema.parse(item));
  const validatedAgents = agents.map((agent) => AgentCapacitySchema.parse(agent));

  // Sort work items lexicographically by ID (deterministic)
  const sortedItems = [...validatedItems].sort((a, b) => a.id.localeCompare(b.id));

  // Initialize agent state
  const agentState = new Map(
    validatedAgents.map((agent) => [
      agent.id,
      {
        capacity: agent.maxConcurrent,
        remaining: agent.maxConcurrent,
        capabilities: new Set(agent.capabilities),
        assigned: [],
      },
    ])
  );

  const assignments = [];
  const waitlist = [];

  // Round-robin allocation
  let agentIndex = 0;
  const agentIds = validatedAgents.map((a) => a.id);

  for (const item of sortedItems) {
    let assigned = false;

    // Try to assign to agents in round-robin order
    for (let attempts = 0; attempts < agentIds.length; attempts++) {
      const agentId = agentIds[agentIndex];
      const state = agentState.get(agentId);

      // Check if agent has capacity and capabilities
      const hasCapacity = state.remaining > 0;
      const hasCapabilities =
        item.requiredCapabilities.length === 0 ||
        item.requiredCapabilities.every((cap) => state.capabilities.has(cap));

      if (hasCapacity && hasCapabilities) {
        // Assign to this agent
        assignments.push({ agentId, workItemId: item.id });
        state.remaining--;
        state.assigned.push(item.id);
        assigned = true;

        // Move to next agent for next item (round-robin)
        agentIndex = (agentIndex + 1) % agentIds.length;
        break;
      }

      // Try next agent
      agentIndex = (agentIndex + 1) % agentIds.length;
    }

    if (!assigned) {
      // No agent available → waitlist
      waitlist.push(item.id);
    }
  }

  // Calculate utilization metrics
  const utilization = {};
  let totalCapacity = 0;

  for (const agent of validatedAgents) {
    const state = agentState.get(agent.id);
    const used = state.capacity - state.remaining;
    utilization[agent.id] = state.capacity > 0 ? (used / state.capacity) * 100 : 0;
    totalCapacity += state.capacity;
  }

  return AllocationResultSchema.parse({
    assignments,
    waitlist,
    utilization,
    totalCapacity,
    totalAssigned: assignments.length,
    totalWaitlisted: waitlist.length,
  });
}

/**
 * Get remaining capacity for a specific agent
 *
 * @param {string} agentId - Agent identifier
 * @param {AllocationResult} allocation - Current allocation result
 * @param {AgentCapacity[]} agents - Agent capacity definitions
 * @returns {number} Remaining slots available
 *
 * @example
 * const remaining = remainingSlots('agent-1', result, agents);
 */
export function remainingSlots(agentId, allocation, agents) {
  const agent = agents.find((a) => a.id === agentId);
  if (!agent) {
    throw new Error(`Agent ${agentId} not found`);
  }

  const assigned = allocation.assignments.filter((a) => a.agentId === agentId).length;
  return Math.max(0, agent.maxConcurrent - assigned);
}

/**
 * Calculate overall system utilization
 *
 * @param {AllocationResult} allocation - Allocation result
 * @returns {number} System utilization percentage (0-100)
 *
 * @example
 * const systemUtil = systemUtilization(result);
 * // 66.67 (2 of 3 slots used)
 */
export function systemUtilization(allocation) {
  if (allocation.totalCapacity === 0) {
    return 0;
  }
  return (allocation.totalAssigned / allocation.totalCapacity) * 100;
}

/**
 * Get waitlist depth
 *
 * @param {AllocationResult} allocation - Allocation result
 * @returns {number} Number of items in waitlist
 *
 * @example
 * const depth = waitlistDepth(result);
 */
export function waitlistDepth(allocation) {
  return allocation.totalWaitlisted;
}

/**
 * Check allocation validity (all items accounted for, no over-allocation)
 *
 * @param {AllocatableWorkItem[]} workItems - Original work items
 * @param {AgentCapacity[]} agents - Agent capacities
 * @param {AllocationResult} allocation - Allocation result
 * @returns {{ valid: boolean, errors: string[] }}
 *
 * @example
 * const check = validateAllocation(items, agents, result);
 * if (!check.valid) {
 *   console.error('Allocation errors:', check.errors);
 * }
 */
export function validateAllocation(workItems, agents, allocation) {
  const errors = [];

  // Check all items accounted for
  const totalItems = workItems.length;
  const accountedItems = allocation.totalAssigned + allocation.totalWaitlisted;
  if (totalItems !== accountedItems) {
    errors.push(
      `Item count mismatch: ${totalItems} items, ${accountedItems} accounted for`
    );
  }

  // Check no duplicate assignments
  const assignedIds = new Set(allocation.assignments.map((a) => a.workItemId));
  if (assignedIds.size !== allocation.assignments.length) {
    errors.push('Duplicate work item assignments detected');
  }

  // Check no over-allocation
  const agentAssignments = new Map();
  for (const assignment of allocation.assignments) {
    const count = agentAssignments.get(assignment.agentId) || 0;
    agentAssignments.set(assignment.agentId, count + 1);
  }

  for (const agent of agents) {
    const assigned = agentAssignments.get(agent.id) || 0;
    if (assigned > agent.maxConcurrent) {
      errors.push(
        `Agent ${agent.id} over-allocated: ${assigned} > ${agent.maxConcurrent}`
      );
    }
  }

  // Check capability requirements
  const itemMap = new Map(workItems.map((item) => [item.id, item]));
  const agentMap = new Map(agents.map((agent) => [agent.id, agent]));

  for (const assignment of allocation.assignments) {
    const item = itemMap.get(assignment.workItemId);
    const agent = agentMap.get(assignment.agentId);

    if (!item || !agent) {
      errors.push(`Invalid assignment: ${assignment.workItemId} → ${assignment.agentId}`);
      continue;
    }

    for (const requiredCap of item.requiredCapabilities) {
      if (!agent.capabilities.includes(requiredCap)) {
        errors.push(
          `Agent ${agent.id} missing capability ${requiredCap} for item ${item.id}`
        );
      }
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}
