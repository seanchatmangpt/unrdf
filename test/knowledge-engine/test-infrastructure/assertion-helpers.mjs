/**
 * @file Assertion Helpers for Knowledge Engine Tests
 * @module assertion-helpers
 * 
 * @description
 * Common assertion helpers for knowledge engine tests providing
 * consistent and readable test assertions.
 */

import { expect } from 'vitest';

/**
 * Helper class for common test assertions
 */
export class AssertionHelpers {
  /**
   * Assert that a hook result indicates success
   * @param {Object} result - Hook execution result
   * @param {string} [message] - Custom assertion message
   */
  static assertHookSuccess(result, message = 'Expected hook to succeed') {
    expect(result, message).toBeDefined();
    expect(result.success, `${message} - success should be true`).toBe(true);
    expect(result.error, `${message} - should not have error`).toBeUndefined();
  }

  /**
   * Assert that a hook result indicates failure
   * @param {Object} result - Hook execution result
   * @param {string} [expectedError] - Expected error message or pattern
   * @param {string} [message] - Custom assertion message
   */
  static assertHookFailure(result, expectedError = null, message = 'Expected hook to fail') {
    expect(result, message).toBeDefined();
    
    if (result.success === false) {
      expect(result.success, `${message} - success should be false`).toBe(false);
      if (expectedError) {
        expect(result.error, `${message} - should have expected error`).toContain(expectedError);
      }
    } else if (result.error) {
      expect(result.error, `${message} - should have error`).toBeDefined();
      if (expectedError) {
        expect(result.error.message || result.error, `${message} - should have expected error`).toContain(expectedError);
      }
    } else {
      throw new Error(`${message} - result should indicate failure`);
    }
  }

  /**
   * Assert that a hook execution completed within the expected time
   * @param {Object} result - Hook execution result
   * @param {number} maxTime - Maximum expected execution time in milliseconds
   * @param {string} [message] - Custom assertion message
   */
  static assertHookExecutionTime(result, maxTime, message = 'Hook execution time') {
    expect(result, message).toBeDefined();
    if (result.duration !== undefined) {
      expect(result.duration, `${message} - should be within ${maxTime}ms`).toBeLessThanOrEqual(maxTime);
    }
  }

  /**
   * Assert that a store contains the expected quads
   * @param {Store} store - N3 Store instance
   * @param {Array} expectedQuads - Array of expected quads
   * @param {string} [message] - Custom assertion message
   */
  static assertStoreState(store, expectedQuads, message = 'Store state') {
    expect(store, message).toBeDefined();
    expect(store.size, `${message} - should have ${expectedQuads.length} quads`).toBe(expectedQuads.length);
    
    for (const expectedQuad of expectedQuads) {
      expect(store.has(expectedQuad), `${message} - should contain quad`).toBe(true);
    }
  }

  /**
   * Assert that a store is empty
   * @param {Store} store - N3 Store instance
   * @param {string} [message] - Custom assertion message
   */
  static assertStoreEmpty(store, message = 'Store should be empty') {
    expect(store, message).toBeDefined();
    expect(store.size, message).toBe(0);
  }

  /**
   * Assert that a file exists and has the expected content
   * @param {string} filePath - Path to the file
   * @param {string} expectedContent - Expected file content
   * @param {string} [message] - Custom assertion message
   */
  static async assertFileContent(filePath, expectedContent, message = 'File content') {
    const fs = await import('fs/promises');
    try {
      const content = await fs.readFile(filePath, 'utf-8');
      expect(content, message).toBe(expectedContent);
    } catch (error) {
      throw new Error(`${message} - file does not exist or cannot be read: ${error.message}`);
    }
  }

  /**
   * Assert that a file exists
   * @param {string} filePath - Path to the file
   * @param {string} [message] - Custom assertion message
   */
  static async assertFileExists(filePath, message = 'File should exist') {
    const fs = await import('fs/promises');
    try {
      await fs.access(filePath);
    } catch (error) {
      throw new Error(`${message} - file does not exist: ${filePath}`);
    }
  }

  /**
   * Assert that a file does not exist
   * @param {string} filePath - Path to the file
   * @param {string} [message] - Custom assertion message
   */
  static async assertFileNotExists(filePath, message = 'File should not exist') {
    const fs = await import('fs/promises');
    try {
      await fs.access(filePath);
      throw new Error(`${message} - file should not exist: ${filePath}`);
    } catch (error) {
      if (error.code === 'ENOENT') {
        // File does not exist, which is what we want
        return;
      }
      throw error;
    }
  }

  /**
   * Assert that an array of hook results all succeeded
   * @param {Array} results - Array of hook execution results
   * @param {string} [message] - Custom assertion message
   */
  static assertAllHooksSucceeded(results, message = 'All hooks should succeed') {
    expect(results, message).toBeDefined();
    expect(Array.isArray(results), `${message} - should be an array`).toBe(true);
    
    for (let i = 0; i < results.length; i++) {
      this.assertHookSuccess(results[i], `${message} - hook ${i} should succeed`);
    }
  }

  /**
   * Assert that an array of hook results all failed
   * @param {Array} results - Array of hook execution results
   * @param {string} [message] - Custom assertion message
   */
  static assertAllHooksFailed(results, message = 'All hooks should fail') {
    expect(results, message).toBeDefined();
    expect(Array.isArray(results), `${message} - should be an array`).toBe(true);
    
    for (let i = 0; i < results.length; i++) {
      this.assertHookFailure(results[i], null, `${message} - hook ${i} should fail`);
    }
  }

  /**
   * Assert that a hook result contains specific data
   * @param {Object} result - Hook execution result
   * @param {Object} expectedData - Expected data to be present
   * @param {string} [message] - Custom assertion message
   */
  static assertHookResultContains(result, expectedData, message = 'Hook result should contain data') {
    expect(result, message).toBeDefined();
    
    for (const [key, value] of Object.entries(expectedData)) {
      expect(result[key], `${message} - should contain ${key}`).toBe(value);
    }
  }

  /**
   * Assert that a condition is valid
   * @param {Object} condition - Condition object
   * @param {string} [message] - Custom assertion message
   */
  static assertConditionValid(condition, message = 'Condition should be valid') {
    expect(condition, message).toBeDefined();
    expect(condition.kind, `${message} - should have kind`).toBeDefined();
    expect(condition.ref, `${message} - should have ref`).toBeDefined();
    expect(condition.ref.uri, `${message} - should have ref.uri`).toBeDefined();
    expect(condition.ref.sha256, `${message} - should have ref.sha256`).toBeDefined();
  }

  /**
   * Assert that a file reference is valid
   * @param {Object} fileRef - File reference object
   * @param {string} [message] - Custom assertion message
   */
  static assertFileRefValid(fileRef, message = 'File reference should be valid') {
    expect(fileRef, message).toBeDefined();
    expect(fileRef.uri, `${message} - should have uri`).toBeDefined();
    expect(fileRef.sha256, `${message} - should have sha256`).toBeDefined();
    expect(fileRef.sha256, `${message} - sha256 should be 64 characters`).toHaveLength(64);
    expect(fileRef.sha256, `${message} - sha256 should be hex`).toMatch(/^[a-f0-9]+$/);
  }

  /**
   * Assert that a manager has the expected number of hooks
   * @param {KnowledgeHookManager} manager - Manager instance
   * @param {number} expectedCount - Expected number of hooks
   * @param {string} [message] - Custom assertion message
   */
  static assertManagerHookCount(manager, expectedCount, message = 'Manager should have expected hook count') {
    expect(manager, message).toBeDefined();
    expect(manager.knowledgeHooks, `${message} - should have knowledgeHooks`).toBeDefined();
    expect(manager.knowledgeHooks.size, `${message} - should have ${expectedCount} hooks`).toBe(expectedCount);
  }

  /**
   * Assert that a promise rejects with a specific error
   * @param {Promise} promise - Promise that should reject
   * @param {string|Error|Function} expectedError - Expected error or error pattern
   * @param {string} [message] - Custom assertion message
   */
  static async assertPromiseRejects(promise, expectedError, message = 'Promise should reject') {
    try {
      await promise;
      throw new Error(`${message} - promise should have rejected`);
    } catch (error) {
      if (typeof expectedError === 'string') {
        expect(error.message, `${message} - should contain expected error`).toContain(expectedError);
      } else if (expectedError instanceof Error) {
        expect(error.message, `${message} - should match expected error`).toBe(expectedError.message);
      } else if (typeof expectedError === 'function') {
        expect(expectedError(error), `${message} - error should match predicate`).toBe(true);
      }
    }
  }

  /**
   * Assert that a promise resolves successfully
   * @param {Promise} promise - Promise that should resolve
   * @param {string} [message] - Custom assertion message
   */
  static async assertPromiseResolves(promise, message = 'Promise should resolve') {
    try {
      const result = await promise;
      expect(result, message).toBeDefined();
      return result;
    } catch (error) {
      throw new Error(`${message} - promise should have resolved: ${error.message}`);
    }
  }
}
