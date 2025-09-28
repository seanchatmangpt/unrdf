/**
 * @file Hook Builder Utility for Tests
 * @module test-utils/hook-builder
 * 
 * @description
 * Fluent API for creating test hooks with sensible defaults
 * and easy customization for edge cases.
 */

/**
 * Hook builder class for creating test hooks
 */
export class HookBuilder {
  constructor() {
    this.hook = {
      meta: {
        name: 'test-hook',
        description: 'Test hook created by HookBuilder'
      },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test.sparql',
          sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
          mediaType: 'application/sparql-query'
        }
      },
      run: async () => ({ success: true })
    };
  }

  /**
   * Set hook metadata
   * @param {string} name - Hook name
   * @param {string} [description] - Hook description
   * @returns {HookBuilder} This builder instance
   */
  withMeta(name, description = 'Test hook') {
    this.hook.meta = { name, description };
    return this;
  }

  /**
   * Set hook condition
   * @param {string} kind - Condition kind (sparql-ask, sparql-select, etc.)
   * @param {string} uri - File URI
   * @param {string} [sha256] - File hash
   * @param {string} [mediaType] - Media type
   * @returns {HookBuilder} This builder instance
   */
  withCondition(kind, uri, sha256 = 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3', mediaType = 'application/sparql-query') {
    this.hook.when = {
      kind,
      ref: {
        uri,
        sha256,
        mediaType
      }
    };
    return this;
  }

  /**
   * Set hook execution function
   * @param {Function} runFunction - Hook execution function
   * @returns {HookBuilder} This builder instance
   */
  withRun(runFunction) {
    this.hook.run = runFunction;
    return this;
  }

  /**
   * Set hook to fail with specific error
   * @param {string} errorMessage - Error message to throw
   * @returns {HookBuilder} This builder instance
   */
  withFailure(errorMessage = 'Hook execution failed') {
    this.hook.run = async () => {
      throw new Error(errorMessage);
    };
    return this;
  }

  /**
   * Set hook to return specific result
   * @param {Object} result - Result object to return
   * @returns {HookBuilder} This builder instance
   */
  withResult(result) {
    this.hook.run = async () => result;
    return this;
  }

  /**
   * Set hook to be slow (for performance testing)
   * @param {number} delayMs - Delay in milliseconds
   * @returns {HookBuilder} This builder instance
   */
  withDelay(delayMs = 1000) {
    this.hook.run = async () => {
      await new Promise(resolve => setTimeout(resolve, delayMs));
      return { success: true, delayed: true };
    };
    return this;
  }

  /**
   * Set hook to consume memory (for memory testing)
   * @param {number} sizeMB - Memory size in MB
   * @returns {HookBuilder} This builder instance
   */
  withMemoryConsumption(sizeMB = 10) {
    this.hook.run = async () => {
      const buffer = Buffer.alloc(sizeMB * 1024 * 1024);
      return { success: true, memoryUsed: sizeMB };
    };
    return this;
  }

  /**
   * Set hook to access file system (for security testing)
   * @param {string} filePath - File path to access
   * @returns {HookBuilder} This builder instance
   */
  withFileAccess(filePath) {
    this.hook.run = async () => {
      const fs = await import('fs/promises');
      try {
        await fs.access(filePath);
        return { success: true, fileAccessed: filePath };
      } catch (error) {
        return { success: false, error: error.message };
      }
    };
    return this;
  }

  /**
   * Set hook to make network request (for security testing)
   * @param {string} url - URL to request
   * @returns {HookBuilder} This builder instance
   */
  withNetworkRequest(url) {
    this.hook.run = async () => {
      try {
        const response = await fetch(url);
        return { success: true, status: response.status };
      } catch (error) {
        return { success: false, error: error.message };
      }
    };
    return this;
  }

  /**
   * Build the final hook object
   * @returns {Object} Complete hook definition
   */
  build() {
    return { ...this.hook };
  }
}

/**
 * Create a new hook builder instance
 * @returns {HookBuilder} New hook builder
 */
export function createHookBuilder() {
  return new HookBuilder();
}

/**
 * Create a simple success hook
 * @param {string} name - Hook name
 * @returns {Object} Hook definition
 */
export function createSuccessHook(name = 'success-hook') {
  return new HookBuilder()
    .withMeta(name)
    .build();
}

/**
 * Create a simple failure hook
 * @param {string} name - Hook name
 * @param {string} errorMessage - Error message
 * @returns {Object} Hook definition
 */
export function createFailureHook(name = 'failure-hook', errorMessage = 'Hook failed') {
  return new HookBuilder()
    .withMeta(name)
    .withFailure(errorMessage)
    .build();
}

/**
 * Create a slow hook for performance testing
 * @param {string} name - Hook name
 * @param {number} delayMs - Delay in milliseconds
 * @returns {Object} Hook definition
 */
export function createSlowHook(name = 'slow-hook', delayMs = 1000) {
  return new HookBuilder()
    .withMeta(name)
    .withDelay(delayMs)
    .build();
}
