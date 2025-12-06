/**
 * @file Context singleton pattern with mutex locking
 * @module cli/utils/context-singleton
 *
 * Prevents race conditions and concurrent context modifications
 * FM-CLI-007: Context state race condition
 * FM-002: Context persistence integration (FIXED)
 */

import { ContextManager as CoreContextManager } from '../core/context.mjs';

/**
 * Global context manager with mutex-style locking
 */
class ContextManager {
  constructor() {
    this.currentContext = null;
    this.contextLock = null;
    this.lockTimeout = 30000; // 30 second lock timeout
    this.coreManager = null; // Will hold CoreContextManager instance
    this.initialized = false;
  }

  /**
   * Initialize core context manager (lazy)
   */
  async ensureInitialized() {
    if (!this.initialized) {
      this.coreManager = new CoreContextManager();
      await this.coreManager.init();
      this.initialized = true;

      // Load current context from core manager
      const current = this.coreManager.getCurrentContext();
      this.currentContext = current?.name || null;
    }
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
    await this.ensureInitialized();

    const lock = await this.acquireLock(contextName);
    try {
      // Use core context manager for actual persistence
      await this.coreManager.useContext(contextName);

      // Verify context was set correctly (POKA-YOKE)
      const current = this.coreManager.getCurrentContext();
      if (current?.name !== contextName) {
        throw new Error(
          `Context switch verification failed: expected "${contextName}", got "${current?.name || 'none'}"`
        );
      }

      this.currentContext = contextName;
      return { success: true, context: contextName, verified: true };
    } finally {
      lock.release();
    }
  }

  /**
   * Get current context
   */
  async getCurrentContext() {
    await this.ensureInitialized();

    // Always read from core manager to ensure consistency
    const current = this.coreManager.getCurrentContext();
    this.currentContext = current?.name || null;

    return this.currentContext;
  }

  /**
   * Clear context with locking
   */
  async clearContext() {
    await this.ensureInitialized();

    const lockName = this.currentContext || 'default';
    const lock = await this.acquireLock(lockName);
    try {
      // Clear in core manager (saves empty current context)
      this.coreManager.currentContext = null;
      await this.coreManager.saveCurrentContext();

      this.currentContext = null;
      return { success: true };
    } finally {
      lock.release();
    }
  }

  /**
   * Get core context manager for advanced operations
   */
  async getCoreManager() {
    await this.ensureInitialized();
    return this.coreManager;
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
