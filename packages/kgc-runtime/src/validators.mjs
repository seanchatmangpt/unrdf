/**
 * @file Custom Validators - Advanced Zod validators for KGC runtime
 * @module @unrdf/kgc-runtime/validators
 *
 * @description
 * Custom validators using Zod .refine() and .superRefine():
 * - Receipt chain integrity (parentHash linkage)
 * - Temporal consistency (timestamps ordered)
 * - Artifact hash validation (content integrity)
 * - Dependency DAG validation (no cycles in WorkItem)
 * - Async validators for external checks
 *
 * All validators return detailed error messages with remediation.
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

// =============================================================================
// Receipt Chain Integrity Validator
// =============================================================================

/**
 * Validates receipt chain integrity - each receipt's parentHash must match previous receipt's hash
 *
 * @param {Array<Object>} receipts - Receipt chain to validate
 * @returns {boolean} True if chain is valid
 *
 * @example
 * const receipts = [
 *   { id: '1', hash: 'abc123', parentHash: null },
 *   { id: '2', hash: 'def456', parentHash: 'abc123' }
 * ];
 * const isValid = validateReceiptChainIntegrity(receipts); // true
 */
export function validateReceiptChainIntegrity(receipts) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    return true; // Empty chain is valid
  }

  // First receipt should have no parent
  if (receipts[0].parentHash !== null && receipts[0].parentHash !== undefined) {
    return false;
  }

  // Each subsequent receipt must reference previous
  for (let i = 1; i < receipts.length; i++) {
    const current = receipts[i];
    const previous = receipts[i - 1];

    if (current.parentHash !== previous.hash) {
      return false;
    }
  }

  return true;
}

/**
 * Receipt chain schema with integrity validation
 */
export const ReceiptChainSchema = z
  .array(
    z.object({
      id: z.string(),
      hash: z.string(),
      parentHash: z.string().nullable(),
      timestamp: z.number().int().positive(),
      operation: z.string(),
      result: z.any().optional(),
    })
  )
  .refine(validateReceiptChainIntegrity, {
    message:
      'Receipt chain integrity violated: parentHash linkage broken. Ensure each receipt references the previous receipt\'s hash.',
  });

// =============================================================================
// Temporal Consistency Validator
// =============================================================================

/**
 * Validates temporal consistency - timestamps must be monotonically increasing
 *
 * @param {Array<Object>} items - Items with timestamps to validate
 * @returns {boolean} True if timestamps are ordered
 *
 * @example
 * const items = [
 *   { timestamp: 1000 },
 *   { timestamp: 2000 },
 *   { timestamp: 3000 }
 * ];
 * const isValid = validateTemporalConsistency(items); // true
 */
export function validateTemporalConsistency(items) {
  if (!Array.isArray(items) || items.length <= 1) {
    return true; // Single item or empty is always consistent
  }

  for (let i = 1; i < items.length; i++) {
    const current = items[i];
    const previous = items[i - 1];

    if (current.timestamp <= previous.timestamp) {
      return false;
    }
  }

  return true;
}

/**
 * Temporally ordered events schema
 */
export const TemporallyOrderedSchema = z
  .array(
    z.object({
      timestamp: z.number().int().positive(),
    }).passthrough()
  )
  .refine(validateTemporalConsistency, {
    message:
      'Temporal consistency violated: timestamps must be monotonically increasing. Check for clock skew or ordering issues.',
  });

// =============================================================================
// Artifact Hash Validator
// =============================================================================

/**
 * Validates artifact content integrity by recomputing hash
 *
 * @param {Object} artifact - Artifact with content and hash
 * @returns {Promise<boolean>} True if hash matches content
 *
 * @example
 * const artifact = {
 *   content: 'Hello World',
 *   hash: await blake3('Hello World')
 * };
 * const isValid = await validateArtifactHash(artifact); // true
 */
export async function validateArtifactHash(artifact) {
  if (!artifact.content || !artifact.hash) {
    return true; // No content or hash means nothing to validate
  }

  const computedHash = await blake3(artifact.content);
  return computedHash === artifact.hash;
}

/**
 * Artifact schema with hash validation
 */
export const ArtifactSchema = z.object({
  type: z.enum(['file', 'directory', 'url', 'inline', 'proof', 'receipt']),
  path: z.string().optional(),
  content: z.string().optional(),
  hash: z.string().optional(),
  size: z.number().nonnegative().optional(),
  metadata: z.record(z.string(), z.any()).optional(),
}).refine(
  async (artifact) => await validateArtifactHash(artifact),
  {
    message:
      'Artifact hash validation failed: computed hash does not match stored hash. Content may be corrupted or tampered.',
  }
);

// =============================================================================
// Dependency DAG Validator (No Cycles)
// =============================================================================

/**
 * Detects cycles in dependency graph using DFS
 *
 * @param {Map<string, string[]>} graph - Adjacency list representation
 * @returns {Object} { hasCycle: boolean, cycle: string[] }
 *
 * @example
 * const graph = new Map([
 *   ['A', ['B', 'C']],
 *   ['B', ['D']],
 *   ['C', ['D']],
 *   ['D', []]
 * ]);
 * const result = detectCycle(graph); // { hasCycle: false, cycle: [] }
 */
export function detectCycle(graph) {
  const visited = new Set();
  const recStack = new Set();
  const path = [];

  function dfs(node) {
    visited.add(node);
    recStack.add(node);
    path.push(node);

    const neighbors = graph.get(node) || [];
    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        const result = dfs(neighbor);
        if (result.hasCycle) {
          return result;
        }
      } else if (recStack.has(neighbor)) {
        // Cycle detected
        const cycleStart = path.indexOf(neighbor);
        const cycle = path.slice(cycleStart);
        cycle.push(neighbor); // Complete the cycle
        return { hasCycle: true, cycle };
      }
    }

    recStack.delete(node);
    path.pop();
    return { hasCycle: false, cycle: [] };
  }

  for (const node of graph.keys()) {
    if (!visited.has(node)) {
      const result = dfs(node);
      if (result.hasCycle) {
        return result;
      }
    }
  }

  return { hasCycle: false, cycle: [] };
}

/**
 * Validates WorkItem dependency graph has no cycles
 *
 * @param {Array<Object>} workItems - Work items with dependencies
 * @returns {Object} { valid: boolean, cycle: string[] }
 *
 * @example
 * const workItems = [
 *   { id: 'A', dependencies: ['B'] },
 *   { id: 'B', dependencies: ['C'] },
 *   { id: 'C', dependencies: [] }
 * ];
 * const result = validateDependencyDAG(workItems); // { valid: true, cycle: [] }
 */
export function validateDependencyDAG(workItems) {
  if (!Array.isArray(workItems) || workItems.length === 0) {
    return { valid: true, cycle: [] };
  }

  // Build adjacency list
  const graph = new Map();
  const itemIds = new Set();

  for (const item of workItems) {
    itemIds.add(item.id);
    graph.set(item.id, item.dependencies || []);
  }

  // Validate all dependencies reference valid items
  for (const [id, deps] of graph.entries()) {
    for (const dep of deps) {
      if (!itemIds.has(dep)) {
        return {
          valid: false,
          cycle: [],
          error: `WorkItem ${id} references non-existent dependency ${dep}`,
        };
      }
    }
  }

  // Detect cycles
  const result = detectCycle(graph);

  return {
    valid: !result.hasCycle,
    cycle: result.cycle,
  };
}

/**
 * WorkItem dependency schema with cycle detection
 */
export const WorkItemDependencySchema = z
  .array(
    z.object({
      id: z.string(),
      dependencies: z.array(z.string()).optional(),
    }).passthrough()
  )
  .superRefine((workItems, ctx) => {
    const result = validateDependencyDAG(workItems);

    if (!result.valid) {
      if (result.error) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: result.error,
        });
      } else if (result.cycle.length > 0) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: `Dependency cycle detected: ${result.cycle.join(' -> ')}. Remove circular dependencies to form a valid DAG.`,
        });
      }
    }
  });

// =============================================================================
// Async Validators
// =============================================================================

/**
 * Async policy validator - checks against external policy service
 *
 * @param {Object} operation - Operation to validate
 * @param {Function} policyCheck - Async policy check function
 * @returns {Promise<boolean>} True if policy allows operation
 *
 * @example
 * const policyCheck = async (op) => {
 *   // Simulate external API call
 *   await new Promise(resolve => setTimeout(resolve, 10));
 *   return op.type === 'safe_operation';
 * };
 *
 * const operation = { type: 'safe_operation', data: 'test' };
 * const isAllowed = await validateAsyncPolicy(operation, policyCheck); // true
 */
export async function validateAsyncPolicy(operation, policyCheck) {
  if (typeof policyCheck !== 'function') {
    throw new Error('policyCheck must be a function');
  }

  try {
    const result = await policyCheck(operation);
    return Boolean(result);
  } catch (error) {
    // Policy check failure = deny
    return false;
  }
}

/**
 * Creates an async policy-validated schema
 *
 * @param {Function} policyCheck - Async policy check function
 * @returns {z.ZodObject} Schema with async validation
 *
 * @example
 * const policyCheck = async (op) => op.type !== 'forbidden';
 * const schema = createAsyncPolicySchema(policyCheck);
 * const result = await schema.parseAsync({ type: 'allowed', data: 'test' });
 */
export function createAsyncPolicySchema(policyCheck) {
  return z
    .object({
      type: z.string(),
    })
    .passthrough()
    .refine(
      async (operation) => await validateAsyncPolicy(operation, policyCheck),
      {
        message:
          'Operation denied by policy: external policy service rejected this operation. Review policy constraints and retry.',
      }
    );
}

// =============================================================================
// Cross-Field Validators
// =============================================================================

/**
 * Validates that endTime > startTime for run capsules
 *
 * @param {Object} capsule - Run capsule with time fields
 * @returns {boolean} True if time ordering is valid
 */
export function validateTimeRange(capsule) {
  if (!capsule.startTime || !capsule.endTime) {
    return true; // If either is missing, let other validators handle it
  }

  return capsule.endTime > capsule.startTime;
}

/**
 * Run capsule schema with time range validation
 */
export const RunCapsuleTimeRangeSchema = z
  .object({
    startTime: z.number().int().positive(),
    endTime: z.number().int().positive().nullable(),
  })
  .passthrough()
  .refine(validateTimeRange, {
    message:
      'Invalid time range: endTime must be greater than startTime. Check for clock issues or logic errors.',
  });

// =============================================================================
// Validator Utilities
// =============================================================================

/**
 * Combines multiple validators with detailed error reporting
 *
 * @param {Array<Function>} validators - Array of validator functions
 * @returns {Function} Combined validator function
 *
 * @example
 * const validator = combineValidators([
 *   (data) => data.value > 0 || 'Value must be positive',
 *   (data) => data.value < 100 || 'Value must be less than 100'
 * ]);
 *
 * const result = validator({ value: 50 });
 * console.log(result); // { valid: true, errors: [] }
 */
export function combineValidators(validators) {
  return (data) => {
    const errors = [];

    for (const validator of validators) {
      const result = validator(data);
      if (typeof result === 'string') {
        errors.push(result);
      } else if (result === false) {
        errors.push('Validation failed');
      }
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  };
}

/**
 * Creates a detailed validation result object
 *
 * @param {boolean} valid - Whether validation passed
 * @param {string} message - Error message if validation failed
 * @param {Object} context - Additional context
 * @returns {Object} Validation result
 */
export function createValidationResult(valid, message = '', context = {}) {
  return {
    valid,
    message,
    context,
    timestamp: Date.now(),
  };
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  // Validators
  validateReceiptChainIntegrity,
  validateTemporalConsistency,
  validateArtifactHash,
  validateDependencyDAG,
  validateAsyncPolicy,
  validateTimeRange,

  // Schemas
  ReceiptChainSchema,
  TemporallyOrderedSchema,
  ArtifactSchema,
  WorkItemDependencySchema,
  RunCapsuleTimeRangeSchema,

  // Utilities
  detectCycle,
  createAsyncPolicySchema,
  combineValidators,
  createValidationResult,
};
