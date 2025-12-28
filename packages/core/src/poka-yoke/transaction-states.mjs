/**
 * Poka-Yoke Pattern: Stateful Transaction Manager
 * 
 * Prevents use-after-cleanup by enforcing a state machine:
 * Active → CleaningUp → Disposed
 * 
 * Invalid operations throw at runtime with clear error messages.
 * 
 * @module @unrdf/core/poka-yoke/transaction-states
 */

const STATE = {
  ACTIVE: 'active',
  CLEANING_UP: 'cleaning_up',
  DISPOSED: 'disposed',
};

/**
 * Error thrown when attempting invalid operation based on state
 */
export class InvalidStateError extends Error {
  /**
   *
   */
  constructor(operation, currentState, validStates = []) {
    const msg = `Cannot ${operation}: manager is ${currentState}`;
    super(msg);
    this.name = 'InvalidStateError';
    this.operation = operation;
    this.currentState = currentState;
    this.validStates = validStates;
  }
}

/**
 * Stateful Transaction Manager with Poka-Yoke guards
 * 
 * Enforces lifecycle: Active → CleaningUp → Disposed
 * 
 * @example
 * const txManager = new StatefulTransactionManager();
 * await txManager.apply(store, delta);  // OK (ACTIVE)
 * await txManager.cleanup();            // OK (ACTIVE → DISPOSED)
 * await txManager.apply(store, delta);  // THROWS InvalidStateError
 */
export class StatefulTransactionManager {
  #state = STATE.ACTIVE;
  #hooks = [];
  #lockchainWriter = null;
  
  /**
   *
   */
  constructor(options = {}) {
    this.options = options;
  }
  
  /**
   * Assert manager is in ACTIVE state
   * @private
   * @param {string} operation - Operation being attempted
   * @throws {InvalidStateError} If not active
   */
  #assertActive(operation) {
    if (this.#state === STATE.CLEANING_UP) {
      throw new InvalidStateError(operation, 'cleaning up', [STATE.ACTIVE]);
    }
    if (this.#state === STATE.DISPOSED) {
      throw new InvalidStateError(operation, 'disposed', [STATE.ACTIVE]);
    }
  }
  
  /**
   * Get current state (for testing/observability)
   * @returns {string} Current state
   */
  getState() {
    return this.#state;
  }
  
  /**
   * Apply a transaction
   * @param {Object} store - RDF store
   * @param {Object} delta - Transaction delta
   * @param {Object} options - Transaction options
   * @returns {Promise<Object>} Transaction result
   * @throws {InvalidStateError} If manager not active
   */
  async apply(store, delta, _options = {}) {
    this.#assertActive('apply transaction');
    
    // Simulate transaction application
    // In real implementation, this would delegate to actual transaction logic
    return {
      store,
      receipt: {
        id: `tx-${Date.now()}`,
        committed: true,
        delta,
      },
    };
  }
  
  /**
   * Add a hook to the transaction manager
   * @param {Object} hook - Hook configuration
   * @throws {InvalidStateError} If manager not active
   */
  addHook(hook) {
    this.#assertActive('add hook');
    
    if (this.#hooks.some(h => h.id === hook.id)) {
      throw new Error(`Hook with id "${hook.id}" already exists`);
    }
    
    this.#hooks.push(hook);
  }
  
  /**
   * Remove a hook by ID
   * @param {string} hookId - Hook ID to remove
   * @returns {boolean} True if hook was removed
   * @throws {InvalidStateError} If manager not active
   */
  removeHook(hookId) {
    this.#assertActive('remove hook');
    
    const index = this.#hooks.findIndex(h => h.id === hookId);
    if (index === -1) return false;
    
    this.#hooks.splice(index, 1);
    return true;
  }
  
  /**
   * Get all hooks
   * @returns {Array} Array of hooks
   * @throws {InvalidStateError} If manager not active
   */
  getHooks() {
    this.#assertActive('get hooks');
    return [...this.#hooks];
  }
  
  /**
   * Cleanup transaction manager
   * Transitions: ACTIVE → CLEANING_UP → DISPOSED
   * @throws {InvalidStateError} If already cleaning up or disposed
   */
  async cleanup() {
    // Can only cleanup from ACTIVE state
    if (this.#state === STATE.CLEANING_UP) {
      throw new InvalidStateError('cleanup', 'already cleaning up', [STATE.ACTIVE]);
    }
    if (this.#state === STATE.DISPOSED) {
      throw new InvalidStateError('cleanup', 'already disposed', [STATE.ACTIVE]);
    }
    
    // Transition to CLEANING_UP
    this.#state = STATE.CLEANING_UP;
    
    try {
      // Clear hooks
      this.#hooks.length = 0;
      
      // Cleanup lockchain writer (if exists)
      if (this.#lockchainWriter) {
        await this.#lockchainWriter.cleanup?.();
        this.#lockchainWriter = null;
      }
      
      // Simulate async cleanup
      await new Promise(resolve => setTimeout(resolve, 10));
    } finally {
      // Always transition to DISPOSED, even if cleanup fails
      this.#state = STATE.DISPOSED;
    }
  }
}
