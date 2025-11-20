/**
 * @file Mock Factory for Knowledge Engine Tests
 * @module mock-factory
 * 
 * @description
 * Factory for creating and managing mocks for knowledge engine components.
 * Provides ESM-compatible mocking strategies.
 */

/**
 * Factory for creating mocks for knowledge engine components
 */
export class MockFactory {
  /**
   *
   */
  constructor(vi) {
    this.vi = vi;
    this.mocks = new Map();
    this.moduleMocks = new Map();
  }

  /**
   * Create a mock for the condition evaluator
   * @param {Object} options - Mock options
   * @param {boolean} [options.shouldSucceed=true] - Whether the mock should succeed
   * @param {Error} [options.error] - Error to throw if shouldSucceed is false
   * @param {number} [options.delay=0] - Delay in milliseconds
   * @returns {Object} Mock object
   */
  createConditionEvaluatorMock(options = {}) {
    const { shouldSucceed = true, error = new Error('Mock error'), delay = 0 } = options;
    
    const mock = {
      evaluate: this.vi.fn().mockImplementation(async (condition, store) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return true;
        }
        throw error;
      }),
      isSatisfied: this.vi.fn().mockImplementation(async (condition, store) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return true;
        }
        throw error;
      }),
      validateCondition: this.vi.fn().mockReturnValue({ valid: true })
    };

    this.mocks.set('conditionEvaluator', mock);
    return mock;
  }

  /**
   * Create a mock for the file resolver
   * @param {Object} options - Mock options
   * @param {boolean} [options.shouldSucceed=true] - Whether the mock should succeed
   * @param {Error} [options.error] - Error to throw if shouldSucceed is false
   * @param {string} [options.content=''] - Content to return
   * @param {string} [options.hash=''] - Hash to return
   * @returns {Object} Mock object
   */
  createFileResolverMock(options = {}) {
    const { shouldSucceed = true, error = new Error('File not found'), content = '', hash = 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3' } = options;
    
    const mock = {
      loadFileWithHash: this.vi.fn().mockImplementation(async (uri, expectedHash, basePath) => {
        if (shouldSucceed) {
          return { content, hash, uri, basePath };
        }
        throw error;
      }),
      calculateFileHash: this.vi.fn().mockImplementation(async (filePath) => {
        if (shouldSucceed) {
          return hash;
        }
        throw error;
      }),
      createFileResolver: this.vi.fn().mockReturnValue({
        loadFileWithHash: mock.loadFileWithHash,
        calculateFileHash: mock.calculateFileHash
      })
    };

    this.mocks.set('fileResolver', mock);
    return mock;
  }

  /**
   * Create a mock for the query engine
   * @param {Object} options - Mock options
   * @param {boolean} [options.shouldSucceed=true] - Whether the mock should succeed
   * @param {Error} [options.error] - Error to throw if shouldSucceed is false
   * @param {any} [options.result] - Result to return
   * @param {number} [options.delay=0] - Delay in milliseconds
   * @returns {Object} Mock object
   */
  createQueryEngineMock(options = {}) {
    const { shouldSucceed = true, error = new Error('Query failed'), result = true, delay = 0 } = options;
    
    const mock = {
      ask: this.vi.fn().mockImplementation(async (query, store) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return result;
        }
        throw error;
      }),
      select: this.vi.fn().mockImplementation(async (query, store) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return result;
        }
        throw error;
      }),
      construct: this.vi.fn().mockImplementation(async (query, store) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return result;
        }
        throw error;
      }),
      describe: this.vi.fn().mockImplementation(async (query, store) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return result;
        }
        throw error;
      })
    };

    this.mocks.set('queryEngine', mock);
    return mock;
  }

  /**
   * Create a mock for the hook executor
   * @param {Object} options - Mock options
   * @param {boolean} [options.shouldSucceed=true] - Whether the mock should succeed
   * @param {Error} [options.error] - Error to throw if shouldSucceed is false
   * @param {Object} [options.result] - Result to return
   * @param {number} [options.delay=0] - Delay in milliseconds
   * @returns {Object} Mock object
   */
  createHookExecutorMock(options = {}) {
    const { shouldSucceed = true, error = new Error('Hook execution failed'), result = { success: true }, delay = 0 } = options;
    
    const mock = {
      execute: this.vi.fn().mockImplementation(async (hook, event, options) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return result;
        }
        throw error;
      }),
      executeAll: this.vi.fn().mockImplementation(async (hooks, event, options) => {
        if (delay > 0) {
          await new Promise(resolve => setTimeout(resolve, delay));
        }
        if (shouldSucceed) {
          return hooks.map(() => result);
        }
        throw error;
      })
    };

    this.mocks.set('hookExecutor', mock);
    return mock;
  }

  /**
   * Setup module-level mocks
   * @param {Object} moduleMocks - Map of module paths to mock implementations
   */
  setupModuleMocks(moduleMocks = {}) {
    for (const [modulePath, mockImplementation] of Object.entries(moduleMocks)) {
      this.vi.mock(modulePath, () => mockImplementation);
      this.moduleMocks.set(modulePath, mockImplementation);
    }
  }

  /**
   * Get a mock by name
   * @param {string} name - Name of the mock
   * @returns {Object|null} Mock object or null if not found
   */
  getMock(name) {
    return this.mocks.get(name) || null;
  }

  /**
   * Reset all mocks
   */
  resetAllMocks() {
    for (const mock of this.mocks.values()) {
      if (mock && typeof mock === 'object') {
        for (const method of Object.values(mock)) {
          if (this.vi.isMockFunction(method)) {
            method.mockReset();
          }
        }
      }
    }
  }

  /**
   * Clear all mocks
   */
  clearAllMocks() {
    this.mocks.clear();
    this.moduleMocks.clear();
  }

  /**
   * Restore all mocks
   */
  restoreAllMocks() {
    this.vi.restoreAllMocks();
    this.clearAllMocks();
  }
}
