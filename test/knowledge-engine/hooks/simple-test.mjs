/**
 * @file Simple Test
 * @module simple-test
 */

import { describe, it, expect, vi } from 'vitest';
import { TestBase, TestDataBuilder } from '../test-infrastructure/index.mjs';

// Mock condition evaluator at module level
vi.mock('../../../src/knowledge-engine/condition-evaluator.mjs', () => {
  const mockValidateCondition = vi.fn().mockReturnValue({ valid: true });
  return {
    evaluateCondition: vi.fn().mockResolvedValue(true),
    createConditionEvaluator: vi.fn().mockReturnValue({
      evaluate: vi.fn().mockResolvedValue(true),
      isSatisfied: vi.fn().mockResolvedValue(true),
      validateCondition: mockValidateCondition
    }),
    validateCondition: mockValidateCondition
  };
});

// Mock hook executor at module level
vi.mock('../../../src/knowledge-engine/hook-executor.mjs', () => ({
  createHookExecutor: vi.fn().mockReturnValue({
    execute: vi.fn().mockResolvedValue({ success: true }),
    executeAll: vi.fn().mockResolvedValue([{ success: true }])
  })
}));

describe('Simple Test', () => {
  it('should work', async () => {
    const testBase = new TestBase();
    await testBase.setup({ testName: 'simple' });
    
    const manager = testBase.getManager();
    const dataBuilder = new TestDataBuilder();
    
    const hook = dataBuilder.buildHook({
      meta: { name: 'simple-hook' }
    });
    
    // This should not throw
    manager.addKnowledgeHook(hook);
    
    await testBase.teardown();
    
    expect(true).toBe(true);
  });
});
