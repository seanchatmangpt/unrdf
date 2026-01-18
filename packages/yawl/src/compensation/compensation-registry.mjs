/**
 * @file Compensation Registry - Undo Handler Management
 * @module yawl/compensation/registry
 *
 * @description
 * Manages compensation handlers for tasks, enabling saga pattern support.
 * Compensation handlers define how to undo completed work.
 *
 * Novel feature - Java YAWL has limited compensation support.
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for compensation specification
 * Same format as regular task spec - compensation is just another workflow
 */
export const CompensationSpecSchema = z.object({
  /** Unique ID for this compensation handler */
  id: z.string().uuid(),
  /** Task ID this compensates */
  taskId: z.string().min(1),
  /** Compensation workflow specification */
  workflowSpec: z.object({
    /** Tasks to execute for compensation */
    tasks: z.array(z.any()),
    /** Flow conditions */
    flow: z.array(z.any()).optional(),
  }),
  /** Inputs to pass to compensation (from original task output) */
  inputMapping: z.record(z.string(), z.string()).optional(),
  /** Timeout for compensation execution (ms) */
  timeoutMs: z.number().int().positive().default(30000),
  /** Retry policy for compensation failures */
  retryPolicy: z.object({
    maxAttempts: z.number().int().positive().default(3),
    backoffMs: z.number().int().positive().default(1000),
  }).optional(),
  /** Metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Schema for compensation mapping entry
 */
export const CompensationMappingSchema = z.object({
  /** Forward task ID */
  forwardTaskId: z.string().min(1),
  /** Compensation task ID */
  compensationTaskId: z.string().min(1),
  /** Compensation spec ID */
  compensationSpecId: z.string().uuid(),
  /** Registered at timestamp */
  registeredAt: z.coerce.date(),
});

/**
 * @typedef {z.infer<typeof CompensationSpecSchema>} CompensationSpec
 * @typedef {z.infer<typeof CompensationMappingSchema>} CompensationMapping
 */

// ============================================================================
// COMPENSATION REGISTRY
// ============================================================================

/**
 * Registry for compensation handlers
 * Bidirectional mapping: forward task ↔ compensation task
 */
export class CompensationRegistry {
  constructor() {
    /** @type {Map<string, CompensationSpec>} */
    this.specs = new Map();

    /** @type {Map<string, CompensationMapping>} */
    this.mappings = new Map();

    /** @type {Map<string, string[]>} Task ID → Compensation Spec IDs */
    this.taskToCompensations = new Map();
  }

  /**
   * Register a compensation handler for a task
   * @param {string} taskId - Task to compensate
   * @param {Object} compensationWorkflow - Compensation workflow spec
   * @param {Object} [options] - Additional options
   * @returns {CompensationSpec} Registered compensation spec
   *
   * @example
   * registry.registerCompensation('book-hotel', {
   *   tasks: [{
   *     id: 'cancel-hotel',
   *     name: 'Cancel Hotel Booking',
   *     execute: async (ctx) => {
   *       await hotelAPI.cancel(ctx.input.bookingId);
   *     }
   *   }]
   * });
   */
  registerCompensation(taskId, compensationWorkflow, options = {}) {
    const spec = {
      id: randomUUID(),
      taskId,
      workflowSpec: compensationWorkflow,
      inputMapping: options.inputMapping || {},
      timeoutMs: options.timeoutMs || 30000,
      retryPolicy: options.retryPolicy,
      metadata: options.metadata,
    };

    const validated = CompensationSpecSchema.parse(spec);

    this.specs.set(validated.id, validated);

    if (!this.taskToCompensations.has(taskId)) {
      this.taskToCompensations.set(taskId, []);
    }
    this.taskToCompensations.get(taskId).push(validated.id);

    // Create bidirectional mapping
    const compensationTaskId = compensationWorkflow.tasks[0]?.id;
    if (compensationTaskId) {
      const mapping = {
        forwardTaskId: taskId,
        compensationTaskId,
        compensationSpecId: validated.id,
        registeredAt: new Date(),
      };
      this.mappings.set(taskId, mapping);
    }

    return validated;
  }

  /**
   * Get compensation spec for a task
   * @param {string} taskId - Task ID
   * @returns {CompensationSpec|null} Compensation spec or null
   */
  getCompensation(taskId) {
    const specIds = this.taskToCompensations.get(taskId);
    if (!specIds || specIds.length === 0) {
      return null;
    }

    // Return most recently registered compensation
    const specId = specIds[specIds.length - 1];
    return this.specs.get(specId) || null;
  }

  /**
   * Get all compensations for a task
   * @param {string} taskId - Task ID
   * @returns {CompensationSpec[]} Array of compensation specs
   */
  getAllCompensations(taskId) {
    const specIds = this.taskToCompensations.get(taskId) || [];
    return specIds.map(id => this.specs.get(id)).filter(Boolean);
  }

  /**
   * Check if task has compensation registered
   * @param {string} taskId - Task ID
   * @returns {boolean} True if compensation exists
   */
  hasCompensation(taskId) {
    return this.taskToCompensations.has(taskId) &&
           this.taskToCompensations.get(taskId).length > 0;
  }

  /**
   * Unregister compensation for a task
   * @param {string} taskId - Task ID
   * @returns {boolean} True if compensation was removed
   */
  unregisterCompensation(taskId) {
    const specIds = this.taskToCompensations.get(taskId);
    if (!specIds || specIds.length === 0) {
      return false;
    }

    for (const specId of specIds) {
      this.specs.delete(specId);
    }

    this.taskToCompensations.delete(taskId);
    this.mappings.delete(taskId);

    return true;
  }

  /**
   * Get mapping between forward and compensation tasks
   * @param {string} taskId - Task ID
   * @returns {CompensationMapping|null} Mapping or null
   */
  getMapping(taskId) {
    return this.mappings.get(taskId) || null;
  }

  /**
   * Get all registered compensations
   * @returns {CompensationSpec[]} All compensation specs
   */
  getAllSpecs() {
    return Array.from(this.specs.values());
  }

  /**
   * Get statistics about registered compensations
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      totalSpecs: this.specs.size,
      totalMappings: this.mappings.size,
      tasksWithCompensation: this.taskToCompensations.size,
    };
  }

  /**
   * Clear all compensations
   */
  clear() {
    this.specs.clear();
    this.mappings.clear();
    this.taskToCompensations.clear();
  }

  /**
   * Export compensations for persistence
   * @returns {Object} Serializable representation
   */
  export() {
    return {
      specs: Array.from(this.specs.entries()),
      mappings: Array.from(this.mappings.entries()).map(([key, val]) => [
        key,
        { ...val, registeredAt: val.registeredAt.toISOString() }
      ]),
      taskToCompensations: Array.from(this.taskToCompensations.entries()),
    };
  }

  /**
   * Import compensations from serialized form
   * @param {Object} data - Exported data
   */
  import(data) {
    this.clear();

    for (const [id, spec] of data.specs) {
      this.specs.set(id, spec);
    }

    for (const [key, val] of data.mappings) {
      this.mappings.set(key, {
        ...val,
        registeredAt: new Date(val.registeredAt)
      });
    }

    for (const [taskId, specIds] of data.taskToCompensations) {
      this.taskToCompensations.set(taskId, specIds);
    }
  }
}

/**
 * Create a new compensation registry
 * @returns {CompensationRegistry} New registry instance
 */
export function createCompensationRegistry() {
  return new CompensationRegistry();
}
