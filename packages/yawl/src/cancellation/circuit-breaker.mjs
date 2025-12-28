/**
 * @file Task Circuit Breaker
 * @module yawl/cancellation/circuit-breaker
 *
 * @description
 * Circuit breaker for task-level failure management.
 * Prevents cascading failures by disabling tasks after consecutive failures.
 */

/**
 * Circuit breaker for task-level failure management
 * Prevents cascading failures by disabling tasks after consecutive failures
 */
export class TaskCircuitBreaker {
  /**
   * @param {Object} config
   * @param {number} [config.failureThreshold=3] - Consecutive failures before opening
   * @param {number} [config.resetTimeout=60000] - Time before half-open state
   * @param {number} [config.halfOpenMaxCalls=1] - Max calls in half-open state
   * @param {Function} [config.onStateChange] - Callback on state transitions
   */
  constructor(config = {}) {
    this.taskId = config.taskId || 'unknown';
    this.failureThreshold = config.failureThreshold ?? 3;
    this.resetTimeout = config.resetTimeout ?? 60000;
    this.halfOpenMaxCalls = config.halfOpenMaxCalls ?? 1;
    this.onStateChange = config.onStateChange ?? null;

    this.state = 'closed';
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.lastFailureTime = null;
    this.lastStateChange = Date.now();
    this.disabledAt = null;
  }

  /**
   * Record a successful task execution
   */
  recordSuccess() {
    if (this.state === 'half_open') {
      this.successCount++;
      if (this.successCount >= 1) {
        this._transition('closed');
        this.failureCount = 0;
        this.successCount = 0;
        this.halfOpenCalls = 0;
      }
    } else if (this.state === 'closed') {
      this.failureCount = 0;
    }
  }

  /**
   * Record a task failure
   * @returns {boolean} True if circuit tripped
   */
  recordFailure() {
    this.failureCount++;
    this.lastFailureTime = Date.now();

    if (this.state === 'half_open') {
      this._transition('open');
      return true;
    }

    if (this.state === 'closed' && this.failureCount >= this.failureThreshold) {
      this._transition('open');
      this.disabledAt = new Date();
      return true;
    }

    return false;
  }

  /**
   * Check if circuit allows execution
   * @returns {boolean} True if execution allowed
   */
  allowExecution() {
    this._checkTransition();

    if (this.state === 'open') {
      return false;
    }

    if (this.state === 'half_open') {
      if (this.halfOpenCalls >= this.halfOpenMaxCalls) {
        return false;
      }
      this.halfOpenCalls++;
    }

    return true;
  }

  /**
   * Manually reset the circuit breaker (enable task)
   */
  reset() {
    this._transition('closed');
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.disabledAt = null;
  }

  /**
   * Get current circuit state
   * @returns {Object} State information
   */
  getState() {
    return {
      taskId: this.taskId,
      state: this.state,
      failureCount: this.failureCount,
      failureThreshold: this.failureThreshold,
      lastFailureTime: this.lastFailureTime,
      lastStateChange: this.lastStateChange,
      disabledAt: this.disabledAt,
      isOpen: this.state === 'open',
    };
  }

  /**
   * Check if state transition should occur
   * @private
   */
  _checkTransition() {
    if (this.state === 'open') {
      const elapsed = Date.now() - this.lastStateChange;
      if (elapsed >= this.resetTimeout) {
        this._transition('half_open');
      }
    }
  }

  /**
   * Transition to a new state
   * @param {string} newState
   * @private
   */
  _transition(newState) {
    const previousState = this.state;
    this.state = newState;
    this.lastStateChange = Date.now();

    if (this.onStateChange) {
      try {
        this.onStateChange({
          taskId: this.taskId,
          from: previousState,
          to: newState,
          timestamp: new Date(),
          failureCount: this.failureCount,
        });
      } catch {
        // Ignore callback errors
      }
    }
  }
}
