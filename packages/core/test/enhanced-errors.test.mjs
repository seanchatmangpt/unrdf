/**
 * @file Enhanced Errors Test Suite (80/20 fast suite)
 * @module @unrdf/core/test/enhanced-errors
 */

import { describe, it, expect } from 'vitest';
import { z } from 'zod';
import {
  enhanceZodError,
  WorkflowError,
  getErrorRecoveryGuide,
} from '../src/utils/enhanced-errors.mjs';

describe('Error Enhancement', () => {
  it('should enhance Zod validation errors with context', () => {
    const schema = z.object({
      tasks: z.array(z.string()),
    });

    try {
      schema.parse({ tasks: 'task1,task2' });
    } catch (error) {
      const enhanced = enhanceZodError(error, { operation: 'workflow creation' });
      expect(enhanced.message).toContain('Validation Error');
      expect(enhanced.message).toContain('Field: tasks');
    }
  });

  it('should create workflow errors with context', () => {
    const error = new WorkflowError('Task failed', 'task-1', { timeout: true });
    expect(error.message).toContain('Task failed');
    expect(error.taskId).toBe('task-1');
  });

  it('should provide recovery guides for errors', () => {
    const guide = getErrorRecoveryGuide('type_mismatch');
    expect(guide).toBeDefined();
    expect(typeof guide).toBe('string');
  });
});
