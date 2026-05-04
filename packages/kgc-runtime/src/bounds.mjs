/**
 * @file Bounds checking and enforcement for capacity-limited channels
 * @module kgc-runtime/bounds
 */

import { z } from 'zod';
import { randomUUID } from 'node:crypto';

/**
 * Bounds configuration schema
 */
const BoundsSchema = z.object({
  max_files_touched: z.number().int().positive().optional(),
  max_bytes_changed: z.number().int().positive().optional(),
  max_tool_ops: z.number().int().positive().optional(),
  max_runtime_ms: z.number().int().positive().optional(),
  max_graph_rewrites: z.number().int().positive().optional(),
});

/**
 * Operation schema
 */
const OperationSchema = z.object({
  type: z.string(),
  files: z.number().int().nonnegative().optional(),
  bytes: z.number().int().nonnegative().optional(),
  ops: z.number().int().nonnegative().optional(),
  ms: z.number().int().nonnegative().optional(),
  rewrites: z.number().int().nonnegative().optional(),
});

/**
 * Receipt schema
 */
const ReceiptSchema = z.object({
  admit: z.boolean(),
  receipt_id: z.string().uuid(),
  timestamp: z.number().int().positive(),
  bound_used: z.string().optional(),
  reason: z.string().optional(),
  bound_violated: z.string().optional(),
  parent_receipt_id: z.string().uuid().optional(),
});

/**
 * Bounds checker - enforces capacity limits during execution
 *
 * @class
 */
export class BoundsChecker {
  /**
   * Create a bounds checker
   *
   * @param {object} bounds - Bound limits
   * @param {number} [bounds.max_files_touched] - Maximum files that can be touched
   * @param {number} [bounds.max_bytes_changed] - Maximum bytes that can be changed
   * @param {number} [bounds.max_tool_ops] - Maximum tool operations
   * @param {number} [bounds.max_runtime_ms] - Maximum runtime in milliseconds
   * @param {number} [bounds.max_graph_rewrites] - Maximum graph rewrites
   */
  constructor(bounds) {
    this.bounds = BoundsSchema.parse(bounds);

    /** @type {object} Current usage tracking */
    this.usage = {
      files_touched: 0,
      bytes_changed: 0,
      tool_ops: 0,
      runtime_ms: 0,
      graph_rewrites: 0,
    };
  }

  /**
   * Check if operation can be executed within bounds
   *
   * @param {object} operation - Operation to check
   * @param {string} operation.type - Operation type
   * @param {number} [operation.files] - Files to touch
   * @param {number} [operation.bytes] - Bytes to change
   * @param {number} [operation.ops] - Tool operations
   * @param {number} [operation.ms] - Runtime milliseconds
   * @param {number} [operation.rewrites] - Graph rewrites
   * @returns {boolean} True if operation can be executed
   */
  canExecute(operation) {
    const op = OperationSchema.parse(operation);

    // Check files bound
    if (this.bounds.max_files_touched !== undefined && op.files !== undefined) {
      if (this.usage.files_touched + op.files > this.bounds.max_files_touched) {
        return false;
      }
    }

    // Check bytes bound
    if (this.bounds.max_bytes_changed !== undefined && op.bytes !== undefined) {
      if (this.usage.bytes_changed + op.bytes > this.bounds.max_bytes_changed) {
        return false;
      }
    }

    // Check ops bound
    if (this.bounds.max_tool_ops !== undefined && op.ops !== undefined) {
      if (this.usage.tool_ops + op.ops > this.bounds.max_tool_ops) {
        return false;
      }
    }

    // Check runtime bound
    if (this.bounds.max_runtime_ms !== undefined && op.ms !== undefined) {
      if (this.usage.runtime_ms + op.ms > this.bounds.max_runtime_ms) {
        return false;
      }
    }

    // Check rewrites bound
    if (this.bounds.max_graph_rewrites !== undefined && op.rewrites !== undefined) {
      if (this.usage.graph_rewrites + op.rewrites > this.bounds.max_graph_rewrites) {
        return false;
      }
    }

    return true;
  }

  /**
   * Record operation execution and update usage
   *
   * @param {object} operation - Operation that was executed
   * @param {string} operation.type - Operation type
   * @param {number} [operation.files] - Files touched
   * @param {number} [operation.bytes] - Bytes changed
   * @param {number} [operation.ops] - Tool operations executed
   * @param {number} [operation.ms] - Runtime consumed
   * @param {number} [operation.rewrites] - Graph rewrites performed
   * @returns {void}
   */
  recordOperation(operation) {
    const op = OperationSchema.parse(operation);

    if (op.files !== undefined) {
      this.usage.files_touched += op.files;
    }
    if (op.bytes !== undefined) {
      this.usage.bytes_changed += op.bytes;
    }
    if (op.ops !== undefined) {
      this.usage.tool_ops += op.ops;
    }
    if (op.ms !== undefined) {
      this.usage.runtime_ms += op.ms;
    }
    if (op.rewrites !== undefined) {
      this.usage.graph_rewrites += op.rewrites;
    }
  }

  /**
   * Get current usage statistics
   *
   * @returns {object} Current usage
   * @returns {number} .files_touched - Files touched so far
   * @returns {number} .bytes_changed - Bytes changed so far
   * @returns {number} .tool_ops - Tool operations executed so far
   * @returns {number} .runtime_ms - Runtime consumed so far
   * @returns {number} .graph_rewrites - Graph rewrites performed so far
   */
  getUsage() {
    return { ...this.usage };
  }
}

/**
 * Enforce bounds on an operation and generate receipt
 *
 * @param {object} operation - Operation to check
 * @param {string} operation.type - Operation type
 * @param {number} [operation.files] - Files to touch
 * @param {number} [operation.bytes] - Bytes to change
 * @param {number} [operation.ops] - Tool operations
 * @param {number} [operation.ms] - Runtime milliseconds
 * @param {number} [operation.rewrites] - Graph rewrites
 * @param {object} bounds - Bound limits
 * @param {number} [bounds.max_files_touched] - Maximum files
 * @param {number} [bounds.max_bytes_changed] - Maximum bytes
 * @param {number} [bounds.max_tool_ops] - Maximum operations
 * @param {number} [bounds.max_runtime_ms] - Maximum runtime
 * @param {number} [bounds.max_graph_rewrites] - Maximum rewrites
 * @param {object} [options] - Additional options
 * @param {string} [options.parent_receipt_id] - Parent receipt for chaining
 * @returns {object} Receipt with admission decision
 * @returns {boolean} .admit - Whether operation is admitted
 * @returns {string} .receipt_id - Unique receipt identifier
 * @returns {number} .timestamp - Receipt generation timestamp
 * @returns {string} [.bound_used] - Bound that was checked (on admit)
 * @returns {string} [.reason] - Rejection reason (on deny)
 * @returns {string} [.bound_violated] - Violated bound name (on deny)
 * @returns {string} [.parent_receipt_id] - Parent receipt if chained
 */
export function enforceBounds(operation, bounds, options = {}) {
  const op = OperationSchema.parse(operation);
  const bnds = BoundsSchema.parse(bounds);

  const receipt = {
    receipt_id: randomUUID(),
    timestamp: Date.now(),
  };

  // Include parent receipt if provided
  if (options.parent_receipt_id) {
    receipt.parent_receipt_id = options.parent_receipt_id;
  }

  // Check files bound
  if (bnds.max_files_touched !== undefined && op.files !== undefined) {
    if (op.files > bnds.max_files_touched) {
      return ReceiptSchema.parse({
        ...receipt,
        admit: false,
        reason: `exceeded max_files: requested ${op.files}, limit ${bnds.max_files_touched}`,
        bound_violated: 'files',
      });
    }
  }

  // Check bytes bound
  if (bnds.max_bytes_changed !== undefined && op.bytes !== undefined) {
    if (op.bytes > bnds.max_bytes_changed) {
      return ReceiptSchema.parse({
        ...receipt,
        admit: false,
        reason: `exceeded max_bytes: requested ${op.bytes}, limit ${bnds.max_bytes_changed}`,
        bound_violated: 'bytes',
      });
    }
  }

  // Check ops bound
  if (bnds.max_tool_ops !== undefined && op.ops !== undefined) {
    if (op.ops > bnds.max_tool_ops) {
      return ReceiptSchema.parse({
        ...receipt,
        admit: false,
        reason: `exceeded max_tool_ops: requested ${op.ops}, limit ${bnds.max_tool_ops}`,
        bound_violated: 'ops',
      });
    }
  }

  // Check runtime bound
  if (bnds.max_runtime_ms !== undefined && op.ms !== undefined) {
    if (op.ms > bnds.max_runtime_ms) {
      return ReceiptSchema.parse({
        ...receipt,
        admit: false,
        reason: `exceeded max_runtime: requested ${op.ms}ms, limit ${bnds.max_runtime_ms}ms`,
        bound_violated: 'runtime',
      });
    }
  }

  // Check rewrites bound
  if (bnds.max_graph_rewrites !== undefined && op.rewrites !== undefined) {
    if (op.rewrites > bnds.max_graph_rewrites) {
      return ReceiptSchema.parse({
        ...receipt,
        admit: false,
        reason: `exceeded max_graph_rewrites: requested ${op.rewrites}, limit ${bnds.max_graph_rewrites}`,
        bound_violated: 'rewrites',
      });
    }
  }

  // Determine which bound was used (for receipt tracking)
  let bound_used = 'none';
  if (op.files !== undefined) bound_used = 'files';
  else if (op.bytes !== undefined) bound_used = 'bytes';
  else if (op.ops !== undefined) bound_used = 'ops';
  else if (op.ms !== undefined) bound_used = 'runtime';
  else if (op.rewrites !== undefined) bound_used = 'rewrites';

  // All checks passed - admit operation
  return ReceiptSchema.parse({
    ...receipt,
    admit: true,
    bound_used,
  });
}
