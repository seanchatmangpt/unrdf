/**
 * @file Dynamic Synchronization Barrier
 * @module @unrdf/yawl/multiple-instance/dynamic-barrier
 *
 * @description
 * Synchronization primitive for WP15 (Multiple Instances without A Priori Runtime Knowledge).
 * Handles dynamic addition of expected instances with race condition protection.
 *
 * Key Features:
 * - Thread-safe instance counting
 * - Dynamic barrier expansion while waiting
 * - Race condition handling with mutex
 * - Signal-based completion protocol
 */

import { z } from 'zod';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Instance state schema
 */
export const InstanceStateSchema = z.object({
  id: z.string().min(1),
  status: z.enum(['pending', 'running', 'completed', 'failed', 'cancelled']),
  addedAt: z.coerce.date(),
  completedAt: z.coerce.date().optional(),
  error: z.any().optional(),
});

/**
 * @typedef {z.infer<typeof InstanceStateSchema>} InstanceState
 */

// ============================================================================
// DYNAMIC BARRIER
// ============================================================================

/**
 * Dynamic synchronization barrier for WP15 pattern
 *
 * Allows instances to be added dynamically while others are executing.
 * Waits for all instances to complete after signalNoMoreInstances() is called.
 *
 * @example
 * const barrier = new DynamicBarrier();
 *
 * // Add initial instances
 * barrier.addExpected('instance-1');
 * barrier.addExpected('instance-2');
 *
 * // Later, dynamically add more
 * barrier.addExpected('instance-3');
 *
 * // Signal no more instances will be added
 * barrier.signalNoMoreInstances();
 *
 * // Wait for all to complete
 * await barrier.wait();
 */
export class DynamicBarrier {
  /**
   * Create a new dynamic barrier
   */
  constructor() {
    /** @type {Map<string, InstanceState>} */
    this.instances = new Map();

    /** @type {boolean} */
    this.signaled = false;

    /** @type {boolean} */
    this.cancelled = false;

    /** @type {Promise<void>|null} */
    this._waitPromise = null;

    /** @type {Function|null} */
    this._resolveWait = null;

    /** @type {Function|null} */
    this._rejectWait = null;

    /** @type {boolean} */
    this._mutex = false;

    /** @type {Array<Function>} */
    this._mutexQueue = [];
  }

  /**
   * Acquire mutex lock (simple spinlock with queue)
   * @returns {Promise<void>}
   * @private
   */
  async _acquireLock() {
    return new Promise((resolve) => {
      const tryAcquire = () => {
        if (!this._mutex) {
          this._mutex = true;
          resolve();
        } else {
          this._mutexQueue.push(tryAcquire);
        }
      };
      tryAcquire();
    });
  }

  /**
   * Release mutex lock
   * @private
   */
  _releaseLock() {
    this._mutex = false;
    if (this._mutexQueue.length > 0) {
      const next = this._mutexQueue.shift();
      setImmediate(next);
    }
  }

  /**
   * Add an expected instance to the barrier
   *
   * @param {string} instanceId - Unique instance identifier
   * @throws {Error} If barrier already signaled no more instances
   * @throws {Error} If instance ID already exists
   */
  addExpected(instanceId) {
    if (this.signaled) {
      throw new Error('Cannot add instance: barrier has been signaled as complete');
    }

    if (this.instances.has(instanceId)) {
      throw new Error(`Instance ${instanceId} already exists in barrier`);
    }

    const state = {
      id: instanceId,
      status: 'pending',
      addedAt: new Date(),
    };

    this.instances.set(instanceId, state);
  }

  /**
   * Mark instance as running
   *
   * @param {string} instanceId - Instance identifier
   * @throws {Error} If instance not found
   */
  markRunning(instanceId) {
    const state = this.instances.get(instanceId);
    if (!state) {
      throw new Error(`Instance ${instanceId} not found in barrier`);
    }

    if (state.status !== 'pending') {
      throw new Error(`Instance ${instanceId} is already ${state.status}`);
    }

    state.status = 'running';
  }

  /**
   * Mark instance as complete
   *
   * @param {string} instanceId - Instance identifier
   * @param {any} [result] - Optional result data
   * @returns {Promise<void>}
   * @throws {Error} If instance not found
   */
  async markComplete(instanceId, result) {
    await this._acquireLock();

    try {
      const state = this.instances.get(instanceId);
      if (!state) {
        throw new Error(`Instance ${instanceId} not found in barrier`);
      }

      if (state.status === 'completed') {
        return; // Idempotent
      }

      state.status = 'completed';
      state.completedAt = new Date();
      if (result !== undefined) {
        state.result = result;
      }

      // Check if all instances complete after signal
      if (this.signaled && this._isAllComplete()) {
        if (this._resolveWait) {
          this._resolveWait();
        }
      }
    } finally {
      this._releaseLock();
    }
  }

  /**
   * Mark instance as failed
   *
   * @param {string} instanceId - Instance identifier
   * @param {Error} error - Error that caused failure
   * @returns {Promise<void>}
   */
  async markFailed(instanceId, error) {
    await this._acquireLock();

    try {
      const state = this.instances.get(instanceId);
      if (!state) {
        throw new Error(`Instance ${instanceId} not found in barrier`);
      }

      state.status = 'failed';
      state.completedAt = new Date();
      state.error = error;

      // Check if all instances complete (including failures)
      if (this.signaled && this._isAllComplete()) {
        if (this._resolveWait) {
          this._resolveWait();
        }
      }
    } finally {
      this._releaseLock();
    }
  }

  /**
   * Mark instance as cancelled
   *
   * @param {string} instanceId - Instance identifier
   * @returns {Promise<void>}
   */
  async markCancelled(instanceId) {
    await this._acquireLock();

    try {
      const state = this.instances.get(instanceId);
      if (!state) {
        throw new Error(`Instance ${instanceId} not found in barrier`);
      }

      state.status = 'cancelled';
      state.completedAt = new Date();

      // Check if all instances complete
      if (this.signaled && this._isAllComplete()) {
        if (this._resolveWait) {
          this._resolveWait();
        }
      }
    } finally {
      this._releaseLock();
    }
  }

  /**
   * Signal that no more instances will be added
   * Does NOT wait for instances to complete - use wait() for that
   */
  signalNoMoreInstances() {
    this.signaled = true;

    // If already all complete, resolve immediately
    if (this._isAllComplete() && this._resolveWait) {
      this._resolveWait();
    }
  }

  /**
   * Wait for all instances to complete
   * MUST call signalNoMoreInstances() first
   *
   * @returns {Promise<void>}
   * @throws {Error} If not signaled yet
   */
  async wait() {
    if (!this.signaled) {
      throw new Error('Must call signalNoMoreInstances() before wait()');
    }

    if (this._isAllComplete()) {
      return;
    }

    if (!this._waitPromise) {
      this._waitPromise = new Promise((resolve, reject) => {
        this._resolveWait = resolve;
        this._rejectWait = reject;
      });
    }

    return this._waitPromise;
  }

  /**
   * Cancel all pending/running instances
   *
   * @returns {Promise<void>}
   */
  async cancel() {
    await this._acquireLock();

    try {
      this.cancelled = true;

      for (const [id, state] of this.instances.entries()) {
        if (state.status === 'pending' || state.status === 'running') {
          state.status = 'cancelled';
          state.completedAt = new Date();
        }
      }

      if (this._rejectWait) {
        this._rejectWait(new Error('Barrier cancelled'));
      }
    } finally {
      this._releaseLock();
    }
  }

  /**
   * Check if all instances are in terminal state
   * @returns {boolean}
   * @private
   */
  _isAllComplete() {
    if (this.instances.size === 0) {
      return false; // Need at least one instance
    }

    for (const state of this.instances.values()) {
      if (state.status !== 'completed' &&
          state.status !== 'failed' &&
          state.status !== 'cancelled') {
        return false;
      }
    }

    return true;
  }

  /**
   * Get current status
   * @returns {Object}
   */
  getStatus() {
    const total = this.instances.size;
    let completed = 0;
    let running = 0;
    let pending = 0;
    let failed = 0;
    let cancelled = 0;

    for (const state of this.instances.values()) {
      switch (state.status) {
        case 'completed': completed++; break;
        case 'running': running++; break;
        case 'pending': pending++; break;
        case 'failed': failed++; break;
        case 'cancelled': cancelled++; break;
      }
    }

    return {
      total,
      completed,
      running,
      pending,
      failed,
      cancelled,
      signaled: this.signaled,
      isComplete: this._isAllComplete(),
    };
  }

  /**
   * Get all instances
   * @returns {InstanceState[]}
   */
  getInstances() {
    return Array.from(this.instances.values());
  }

  /**
   * Get instance by ID
   * @param {string} instanceId
   * @returns {InstanceState|undefined}
   */
  getInstance(instanceId) {
    return this.instances.get(instanceId);
  }

  /**
   * Get all failed instances
   * @returns {InstanceState[]}
   */
  getFailedInstances() {
    return Array.from(this.instances.values()).filter(s => s.status === 'failed');
  }

  /**
   * Check if barrier has any failures
   * @returns {boolean}
   */
  hasFailed() {
    return this.getFailedInstances().length > 0;
  }

  /**
   * Reset barrier (for testing)
   */
  reset() {
    this.instances.clear();
    this.signaled = false;
    this.cancelled = false;
    this._waitPromise = null;
    this._resolveWait = null;
    this._rejectWait = null;
  }
}

/**
 * Create a dynamic barrier
 * @returns {DynamicBarrier}
 */
export function createDynamicBarrier() {
  return new DynamicBarrier();
}
