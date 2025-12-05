/**
 * @file Context singleton pattern with mutex locking
 * @module cli/utils/context-singleton
 *
 * Prevents race conditions and concurrent context modifications
 * FM-CLI-007: Context state race condition
 */

/**
 * Global context manager with mutex-style locking
 */
class ContextManager {
  constructor() {
    this.currentContext = null;
    this.contextLock = null;
    this.lockTimeout = 30000; // 30 second lock timeout
  }

  /**
   * Acquire lock for context operation
   * @param {string} contextName - Context identifier
   * @returns {Promise<Object>} Lock object with release function
   */
  async acquireLock(contextName) {
    // If lock exists and is held by different context, wait
    const lockAcquireStart = Date.now();
    while (this.contextLock && this.contextLock.context !== contextName) {
      // Check for lock timeout (stale lock)
      if (Date.now() - this.contextLock.acquiredAt > this.lockTimeout) {
        this.contextLock = null;
        break;
      }
      // Wait before retry
      await new Promise(resolve => setTimeout(resolve, 50));

      // Prevent infinite waiting
      if (Date.now() - lockAcquireStart > this.lockTimeout) {
        throw new Error(
          `Failed to acquire context lock for "${contextName}": timeout after ${this.lockTimeout}ms. ` +
          `Another operation may be holding the lock. Try again in a moment.`
        );
      }
    }

    // Acquire lock
    const lock = {
      context: contextName,
      acquiredAt: Date.now(),
      released: false
    };

    this.contextLock = lock;

    // Return release function
    return {
      release: () => {
        if (!lock.released) {
          if (this.contextLock === lock) {
            this.contextLock = null;
          }
          lock.released = true;
        }
      }
    };
  }

  /**
   * Set current context with locking
   */
  async setContext(contextName) {
    const lock = await this.acquireLock(contextName);
    try {
      // TODO: Implement actual context setting
      this.currentContext = contextName;
      return { success: true, context: contextName };
    } finally {
      lock.release();
    }
  }

  /**
   * Get current context
   */
  getCurrentContext() {
    return this.currentContext;
  }

  /**
   * Clear context with locking
   */
  async clearContext() {
    const lockName = this.currentContext || 'default';
    const lock = await this.acquireLock(lockName);
    try {
      // TODO: Implement actual context clearing
      this.currentContext = null;
      return { success: true };
    } finally {
      lock.release();
    }
  }

  /**
   * Check if lock is held
   */
  isLocked() {
    return this.contextLock !== null;
  }

  /**
   * Get lock info for debugging
   */
  getLockInfo() {
    if (!this.contextLock) {
      return { locked: false };
    }

    return {
      locked: true,
      context: this.contextLock.context,
      acquiredAt: new Date(this.contextLock.acquiredAt).toISOString(),
      age_ms: Date.now() - this.contextLock.acquiredAt
    };
  }
}

// Singleton instance
let instance = null;

/**
 * Get or create context manager singleton
 */
export function getContextManager() {
  if (!instance) {
    instance = new ContextManager();
  }
  return instance;
}

/**
 * Reset singleton (for testing)
 */
export function resetContextManager() {
  instance = null;
}

export { ContextManager };

export default {
  getContextManager,
  resetContextManager,
  ContextManager
};
