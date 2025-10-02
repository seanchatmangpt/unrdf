/**
 * @fileoverview Mock Knowledge Hook Evaluator (London School TDD)
 * @description Test double for hook evaluation - contract-driven
 */

import { vi } from 'vitest';

/**
 * Creates a mock hook evaluation result
 * @param {object} options - Result configuration
 * @returns {object} Mock evaluation result
 */
export function createMockEvaluationResult(options = {}) {
  const {
    success = true,
    violations = [],
    bindings = [],
    executionTime = 10
  } = options;

  return {
    success,
    violations,
    bindings,
    metadata: {
      executionTime,
      timestamp: new Date().toISOString(),
      hookId: 'mock-hook-id'
    }
  };
}

/**
 * Creates a mock hook definition
 * @param {object} hook - Hook configuration
 * @returns {object} Mock hook
 */
export function createMockHook(hook = {}) {
  return {
    id: hook.id || 'mock-hook-1',
    name: hook.name || 'Mock Hook',
    type: hook.type || 'validation',
    sparql: hook.sparql || 'SELECT * WHERE { ?s ?p ?o }',
    enabled: hook.enabled !== false,
    priority: hook.priority || 100,
    metadata: hook.metadata || {}
  };
}

/**
 * Creates a mock hook evaluator
 * @param {object} options - Evaluator configuration
 * @returns {object} Mock evaluator
 */
export function createMockHookEvaluator(options = {}) {
  const {
    mockResults = [createMockEvaluationResult()],
    shouldError = false,
    errorMessage = 'Mock evaluation error'
  } = options;

  let resultIndex = 0;

  return {
    evaluate: vi.fn(async (hook, context) => {
      if (shouldError) {
        throw new Error(errorMessage);
      }

      const result = mockResults[resultIndex] || mockResults[0];
      resultIndex = (resultIndex + 1) % mockResults.length;

      return result;
    }),

    evaluateBatch: vi.fn(async (hooks, context) => {
      if (shouldError) {
        throw new Error(errorMessage);
      }

      return hooks.map(() => mockResults[0]);
    }),

    registerHook: vi.fn(async (hook) => {
      return { ...hook, id: hook.id || `generated-${Date.now()}` };
    }),

    unregisterHook: vi.fn(async (hookId) => {
      return { success: true };
    }),

    listHooks: vi.fn(async () => {
      return [createMockHook()];
    }),

    // Expose config for verification
    __config: {
      mockResults,
      shouldError,
      errorMessage
    }
  };
}

/**
 * Factory for complete Knowledge Hook mock ecosystem
 * @param {object} options - Configuration
 * @returns {object} Complete hook mocks
 */
export function createKnowledgeHookMocks(options = {}) {
  const evaluator = createMockHookEvaluator(options);

  return {
    evaluator,
    createHook: createMockHook,
    createResult: createMockEvaluationResult
  };
}
