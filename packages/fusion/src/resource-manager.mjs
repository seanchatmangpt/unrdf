/**
 * @file Resource Manager - Unified resource allocation and capacity tracking
 * @module @unrdf/fusion/resource-manager
 *
 * Provides deterministic resource pool management with:
 * - Pool creation with configurable capacity and strategies
 * - Atomic allocate/deallocate operations
 * - Capacity tracking and enforcement
 * - Cryptographic receipts for audit trail
 * - Deterministic allocation receipts (same inputs = same hashes)
 *
 * @example
 * import { createResourceManager } from '@unrdf/fusion/resource-manager';
 * import { generateReceipt } from '@unrdf/yawl/receipt-core';
 *
 * const rm = await createResourceManager();
 * const pool = rm.createPool('compute', 100, 'round-robin');
 * const result = await rm.allocate('compute', 30);
 * console.log(result); // { allocated: 30, remaining: 70 }
 */

import { z } from 'zod';
import { generateReceipt, RECEIPT_EVENT_TYPES } from '@unrdf/yawl/receipt';

// =============================================================================
// SCHEMAS & TYPES
// =============================================================================

/**
 * Allocation strategy types
 * @readonly
 * @enum {string}
 */
export const ALLOCATION_STRATEGIES = Object.freeze({
  ROUND_ROBIN: 'round-robin',
  FIRST_FIT: 'first-fit',
  BEST_FIT: 'best-fit',
});

/**
 * Pool configuration schema
 */
const PoolConfigSchema = z.object({
  name: z.string().min(1),
  capacity: z.number().int().positive(),
  strategy: z.enum(['round-robin', 'first-fit', 'best-fit']).default('round-robin'),
  allocated: z.number().int().min(0).default(0),
});

/**
 * Allocation result schema
 */
const AllocationResultSchema = z.object({
  allocated: z.number().int().min(0),
  remaining: z.number().int().min(0),
  receipt: z.any().optional(), // Receipt object
});

/**
 * Query result schema
 */
const QueryResultSchema = z.object({
  allocated: z.number().int().min(0),
  available: z.number().int().min(0),
  capacity: z.number().int().positive(),
  utilization: z.number().min(0).max(1),
});

/**
 * @typedef {z.infer<typeof PoolConfigSchema>} PoolConfig
 * @typedef {z.infer<typeof AllocationResultSchema>} AllocationResult
 * @typedef {z.infer<typeof QueryResultSchema>} QueryResult
 */

// =============================================================================
// RESOURCE POOL
// =============================================================================

/**
 * Resource pool with capacity tracking
 * @private
 */
class ResourcePool {
  /**
   * Create a new resource pool
   * @param {string} name - Pool name
   * @param {number} capacity - Total capacity
   * @param {string} strategy - Allocation strategy
   */
  constructor(name, capacity, strategy = 'round-robin') {
    const config = PoolConfigSchema.parse({ name, capacity, strategy });

    this.name = config.name;
    this.capacity = config.capacity;
    this.strategy = config.strategy;
    this.allocated = 0;
    this.allocations = new Map(); // Track individual allocations
  }

  /**
   * Get available capacity
   * @returns {number} Available capacity
   */
  getAvailable() {
    return this.capacity - this.allocated;
  }

  /**
   * Check if allocation is possible
   * @param {number} amount - Amount to allocate
   * @returns {boolean} True if allocation is possible
   */
  canAllocate(amount) {
    return this.getAvailable() >= amount;
  }

  /**
   * Allocate resources
   * @param {number} amount - Amount to allocate
   * @param {string} [allocationId] - Optional allocation ID for tracking
   * @returns {number} Amount allocated
   * @throws {Error} If insufficient capacity
   */
  allocate(amount, allocationId = null) {
    if (!this.canAllocate(amount)) {
      throw new Error(
        `Insufficient capacity in pool '${this.name}': requested ${amount}, available ${this.getAvailable()}`
      );
    }

    this.allocated += amount;

    if (allocationId) {
      this.allocations.set(allocationId, amount);
    }

    return amount;
  }

  /**
   * Deallocate resources
   * @param {number} amount - Amount to deallocate
   * @param {string} [allocationId] - Optional allocation ID to remove
   * @returns {number} Amount deallocated
   * @throws {Error} If amount exceeds allocated
   */
  deallocate(amount, allocationId = null) {
    if (amount > this.allocated) {
      throw new Error(
        `Cannot deallocate ${amount} from pool '${this.name}': only ${this.allocated} allocated`
      );
    }

    this.allocated -= amount;

    if (allocationId && this.allocations.has(allocationId)) {
      this.allocations.delete(allocationId);
    }

    return amount;
  }

  /**
   * Get pool query result
   * @returns {QueryResult} Pool status
   */
  query() {
    return QueryResultSchema.parse({
      allocated: this.allocated,
      available: this.getAvailable(),
      capacity: this.capacity,
      utilization: this.allocated / this.capacity,
    });
  }

  /**
   * Reset pool (for testing)
   */
  reset() {
    this.allocated = 0;
    this.allocations.clear();
  }
}

// =============================================================================
// RESOURCE MANAGER
// =============================================================================

/**
 * Resource manager with unified allocation and receipt tracking
 */
export class ResourceManager {
  /**
   * Create a new resource manager
   * @param {Object} [config={}] - Configuration
   * @param {boolean} [config.enableReceipts=true] - Enable receipt generation
   * @param {boolean} [config.deterministic=false] - Use deterministic mode
   */
  constructor(config = {}) {
    this.pools = new Map();
    this.enableReceipts = config.enableReceipts !== false;
    this.deterministic = config.deterministic === true;
    this.receiptChain = []; // Track receipts for chaining
    this.stats = {
      allocations: 0,
      deallocations: 0,
      receiptsGenerated: 0,
    };
  }

  /**
   * Create a new resource pool
   * @param {string} name - Pool name (must be unique)
   * @param {number} capacity - Total capacity
   * @param {string} [strategy='round-robin'] - Allocation strategy
   * @returns {ResourcePool} Created pool
   * @throws {Error} If pool already exists
   */
  createPool(name, capacity, strategy = 'round-robin') {
    if (this.pools.has(name)) {
      throw new Error(`Pool '${name}' already exists`);
    }

    const pool = new ResourcePool(name, capacity, strategy);
    this.pools.set(name, pool);
    return pool;
  }

  /**
   * Get a pool by name
   * @param {string} name - Pool name
   * @returns {ResourcePool} Pool instance
   * @throws {Error} If pool doesn't exist
   * @private
   */
  _getPool(name) {
    const pool = this.pools.get(name);
    if (!pool) {
      throw new Error(`Pool '${name}' does not exist`);
    }
    return pool;
  }

  /**
   * Allocate resources from a pool
   * @param {string} resource - Pool name
   * @param {number} amount - Amount to allocate
   * @param {Object} [options={}] - Allocation options
   * @param {string} [options.allocationId] - Unique allocation ID
   * @param {string} [options.actor] - Actor making allocation
   * @param {string} [options.reason] - Reason for allocation
   * @returns {Promise<AllocationResult>} Allocation result with receipt
   */
  async allocate(resource, amount, options = {}) {
    const pool = this._getPool(resource);

    // Perform allocation
    const allocated = pool.allocate(amount, options.allocationId);
    const remaining = pool.getAvailable();

    this.stats.allocations++;

    // Generate allocation receipt
    let receipt = null;
    if (this.enableReceipts) {
      receipt = await this.emitAllocationReceipt({
        type: 'ALLOCATE',
        resource,
        amount: allocated,
        remaining,
        allocationId: options.allocationId,
        actor: options.actor,
        reason: options.reason,
      });
      this.stats.receiptsGenerated++;
    }

    return AllocationResultSchema.parse({
      allocated,
      remaining,
      receipt,
    });
  }

  /**
   * Deallocate resources from a pool
   * @param {string} resource - Pool name
   * @param {number} amount - Amount to deallocate
   * @param {Object} [options={}] - Deallocation options
   * @param {string} [options.allocationId] - Allocation ID to remove
   * @param {string} [options.actor] - Actor making deallocation
   * @param {string} [options.reason] - Reason for deallocation
   * @returns {Promise<AllocationResult>} Deallocation result with receipt
   */
  async deallocate(resource, amount, options = {}) {
    const pool = this._getPool(resource);

    // Perform deallocation
    const deallocated = pool.deallocate(amount, options.allocationId);
    const remaining = pool.getAvailable();

    this.stats.deallocations++;

    // Generate deallocation receipt
    let receipt = null;
    if (this.enableReceipts) {
      receipt = await this.emitAllocationReceipt({
        type: 'DEALLOCATE',
        resource,
        amount: deallocated,
        remaining,
        allocationId: options.allocationId,
        actor: options.actor,
        reason: options.reason,
      });
      this.stats.receiptsGenerated++;
    }

    return AllocationResultSchema.parse({
      allocated: deallocated,
      remaining,
      receipt,
    });
  }

  /**
   * Query pool status
   * @param {string} resource - Pool name
   * @returns {QueryResult} Pool status
   */
  query(resource) {
    const pool = this._getPool(resource);
    return pool.query();
  }

  /**
   * Enforce resource limit (throws if allocation would exceed)
   * @param {string} resource - Pool name
   * @param {number} limit - Maximum allowed allocation
   * @throws {Error} If current allocation exceeds limit
   */
  enforceLimit(resource, limit) {
    const pool = this._getPool(resource);
    if (pool.allocated > limit) {
      throw new Error(
        `Resource limit exceeded for '${resource}': ${pool.allocated} > ${limit}`
      );
    }
  }

  /**
   * Emit allocation receipt
   * @param {Object} allocation - Allocation data
   * @param {string} allocation.type - 'ALLOCATE' or 'DEALLOCATE'
   * @param {string} allocation.resource - Resource pool name
   * @param {number} allocation.amount - Amount allocated/deallocated
   * @param {number} allocation.remaining - Remaining capacity
   * @param {string} [allocation.allocationId] - Allocation ID
   * @param {string} [allocation.actor] - Actor
   * @param {string} [allocation.reason] - Reason
   * @returns {Promise<Object>} Generated receipt
   */
  async emitAllocationReceipt(allocation) {
    const eventType =
      allocation.type === 'ALLOCATE'
        ? RECEIPT_EVENT_TYPES.RESOURCE_ALLOCATED
        : RECEIPT_EVENT_TYPES.RESOURCE_RELEASED;

    const previousReceipt =
      this.receiptChain.length > 0
        ? this.receiptChain[this.receiptChain.length - 1]
        : null;

    const receipt = await generateReceipt(
      {
        eventType,
        caseId: 'resource-manager',
        taskId: allocation.resource,
        workItemId: allocation.allocationId || undefined,
        payload: {
          decision: allocation.type,
          justification: {
            reasoning: allocation.reason || `${allocation.type} ${allocation.amount} units`,
          },
          actor: allocation.actor || 'system',
          context: {
            amount: allocation.amount,
            remaining: allocation.remaining,
            resource: allocation.resource,
          },
        },
      },
      previousReceipt
    );

    this.receiptChain.push(receipt);
    return receipt;
  }

  /**
   * Get all receipts in chain
   * @returns {Array<Object>} Receipt chain
   */
  getReceiptChain() {
    return [...this.receiptChain];
  }

  /**
   * Get manager statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const poolStats = {};
    for (const [name, pool] of this.pools.entries()) {
      poolStats[name] = pool.query();
    }

    return {
      ...this.stats,
      pools: poolStats,
      chainLength: this.receiptChain.length,
    };
  }

  /**
   * Reset all pools (for testing)
   */
  reset() {
    for (const pool of this.pools.values()) {
      pool.reset();
    }
    this.receiptChain = [];
    this.stats = {
      allocations: 0,
      deallocations: 0,
      receiptsGenerated: 0,
    };
  }

  /**
   * Clear all pools
   */
  clear() {
    this.pools.clear();
    this.reset();
  }
}

/**
 * Create a resource manager instance
 * @param {Object} [config={}] - Configuration
 * @param {boolean} [config.enableReceipts=true] - Enable receipt generation
 * @param {boolean} [config.deterministic=false] - Use deterministic mode
 * @returns {Promise<ResourceManager>} Resource manager instance
 */
export async function createResourceManager(config = {}) {
  return new ResourceManager(config);
}

export default {
  createResourceManager,
  ResourceManager,
  ALLOCATION_STRATEGIES,
};
