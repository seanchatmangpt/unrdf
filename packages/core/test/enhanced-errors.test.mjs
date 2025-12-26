/**
 * @file Enhanced Errors Test Suite
 * @module @unrdf/core/test/enhanced-errors
 *
 * Tests demonstrating before/after UX improvements:
 * - Zod validation errors
 * - Workflow state errors
 * - Import errors
 * - Debug mode
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { z } from 'zod';
import {
  enhanceZodError,
  WorkflowError,
  ImportError,
  safeImport,
  createDebugger as _createDebugger,
  traceWorkflowStep as _traceWorkflowStep,
  getErrorRecoveryGuide,
  isDebugEnabled as _isDebugEnabled,
} from '../src/utils/enhanced-errors.mjs';

// =============================================================================
// Zod Error Enhancement Tests
// =============================================================================

describe('Zod Error Enhancement', () => {
  it('should enhance type mismatch errors with actionable fixes', () => {
    const schema = z.object({
      tasks: z.array(z.string()),
    });

    try {
      // Common mistake: passing string instead of array
      schema.parse({ tasks: 'task1,task2' });
      expect.fail('Should throw validation error');
    } catch (error) {
      const enhanced = enhanceZodError(error, { operation: 'workflow creation' });

      // Verify enhanced error has context
      expect(enhanced.message).toContain('Validation Error in workflow creation');
      expect(enhanced.message).toContain('Field: tasks');
      expect(enhanced.message).toContain('Expected: array');
      expect(enhanced.message).toContain('Received: string');

      // Verify actionable fix
      expect(enhanced.message).toContain('Suggested Fix');
      expect(enhanced.message).toContain('Change tasks from a string to an array');

      // Verify documentation link
      expect(enhanced.message).toContain('Documentation:');
      expect(enhanced.message).toContain('https://');

      // Verify structured data
      expect(enhanced.issues).toHaveLength(1);
      expect(enhanced.issues[0].field).toBe('tasks');
      expect(enhanced.issues[0].fix).toContain('array');
    }
  });

  it('should handle multiple validation errors', () => {
    const schema = z.object({
      name: z.string().min(3),
      age: z.number().min(0),
      email: z.string().email(),
    });

    try {
      schema.parse({ name: 'ab', age: -5, email: 'invalid' });
      expect.fail('Should throw validation error');
    } catch (error) {
      const enhanced = enhanceZodError(error, { operation: 'user validation' });

      // Should show all issues
      expect(enhanced.message).toContain('Additional validation errors');
      expect(enhanced.issues.length).toBeGreaterThan(1);

      // Each issue should have a fix
      enhanced.issues.forEach(issue => {
        expect(issue.fix).toBeTruthy();
        expect(issue.field).toBeTruthy();
      });
    }
  });

  it('should provide specific fixes for common type errors', () => {
    const testCases = [
      {
        schema: z.number(),
        input: '42',
        expectedFix: 'parseInt',
      },
      {
        schema: z.boolean(),
        input: 'true',
        expectedFix: 'boolean',
      },
      {
        schema: z.object({ value: z.string() }),
        input: 'value',
        expectedFix: 'object',
      },
    ];

    testCases.forEach(({ schema, input, expectedFix }) => {
      try {
        schema.parse(input);
        expect.fail('Should throw');
      } catch (error) {
        const enhanced = enhanceZodError(error);
        expect(enhanced.message.toLowerCase()).toContain(expectedFix.toLowerCase());
      }
    });
  });

  it('should handle enum validation with available options', () => {
    const schema = z.object({
      status: z.enum(['pending', 'running', 'completed']),
    });

    try {
      schema.parse({ status: 'done' });
      expect.fail('Should throw');
    } catch (error) {
      const enhanced = enhanceZodError(error);

      // Should show valid enum values
      expect(enhanced.message).toContain('pending');
      expect(enhanced.message).toContain('running');
      expect(enhanced.message).toContain('completed');
    }
  });
});

// =============================================================================
// Workflow Error Tests
// =============================================================================

describe('WorkflowError', () => {
  it('should provide context for task enablement failures', () => {
    const error = new WorkflowError('Task enablement failed', {
      workflowId: 'wf-123',
      caseId: 'case-456',
      taskId: 'task-789',
      state: 'RUNNING',
      reason: 'Upstream tasks not completed',
      upstreamTasks: ['task-1', 'task-2', 'task-3'],
      completedTasks: ['task-1', 'task-2'],
      debugCommand: 'node debug-workflow.mjs wf-123',
    });

    // Verify structured error message
    expect(error.message).toContain('Workflow Error: Task enablement failed');
    expect(error.message).toContain('Workflow: wf-123');
    expect(error.message).toContain('Case: case-456');
    expect(error.message).toContain('Task: task-789');
    expect(error.message).toContain('Current State: RUNNING');
    expect(error.message).toContain('Reason: Upstream tasks not completed');

    // Verify upstream task status
    expect(error.message).toContain('Required Upstream Tasks');
    expect(error.message).toContain('✅ task-1');
    expect(error.message).toContain('✅ task-2');
    expect(error.message).toContain('❌ task-3');

    // Verify recovery suggestions
    expect(error.recovery).toBeTruthy();
    expect(error.recovery.length).toBeGreaterThan(0);
    expect(error.recovery.some(r => r.includes('task-3'))).toBe(true);
    expect(error.recovery.some(r => r.includes('debug-workflow.mjs'))).toBe(true);
  });

  it('should suggest recovery for INVALID state', () => {
    const error = new WorkflowError('Invalid workflow state', {
      workflowId: 'wf-invalid',
      state: 'INVALID',
    });

    expect(error.recovery.some(r => r.includes('checkpoint'))).toBe(true);
    expect(error.recovery.some(r => r.includes('restart'))).toBe(true);
  });

  it('should suggest recovery for DEADLOCK state', () => {
    const error = new WorkflowError('Workflow deadlocked', {
      workflowId: 'wf-deadlock',
      state: 'DEADLOCK',
    });

    expect(error.recovery.some(r => r.includes('circular'))).toBe(true);
    expect(error.recovery.some(r => r.includes('clearDeadlock'))).toBe(true);
  });

  it('should include documentation links', () => {
    const error = new WorkflowError('Workflow error', {
      workflowId: 'wf-123',
    });

    expect(error.recovery.some(r => r.includes('https://'))).toBe(true);
    expect(error.recovery.some(r => r.includes('workflow'))).toBe(true);
  });
});

// =============================================================================
// Import Error Tests
// =============================================================================

describe('ImportError', () => {
  it('should provide actionable fixes for MODULE_NOT_FOUND', () => {
    const originalError = new Error('Cannot find module @unrdf/missing');
    originalError.code = 'ERR_MODULE_NOT_FOUND';

    const error = new ImportError('@unrdf/missing', originalError);

    // Verify error message has fixes
    expect(error.message).toContain('Cannot import package');
    expect(error.message).toContain('@unrdf/missing');
    expect(error.message).toContain('Possible fixes');

    // Verify specific suggestions
    expect(error.message).toContain('pnpm install');
    expect(error.message).toContain('pnpm add @unrdf/missing');
    expect(error.message).toContain('node_modules/@unrdf/missing');
    expect(error.message).toContain('pnpm-lock.yaml');

    // Verify documentation link
    expect(error.message).toContain('Docs:');
    expect(error.message).toContain('https://');

    // Verify structured data
    expect(error.packageName).toBe('@unrdf/missing');
    expect(error.originalError).toBe(originalError);
  });

  it('should provide fixes for PACKAGE_PATH_NOT_EXPORTED', () => {
    const originalError = new Error('Package path not exported');
    originalError.code = 'ERR_PACKAGE_PATH_NOT_EXPORTED';

    const error = new ImportError('@unrdf/yawl/internal', originalError);

    expect(error.message).toContain('path is not exported');
    expect(error.message).toContain('exports');
    expect(error.message).toContain('subpath');
  });

  it('should provide fixes for syntax errors', () => {
    const originalError = new SyntaxError('Unexpected token');

    const error = new ImportError('legacy-package', originalError);

    expect(error.message).toContain('Syntax error');
    expect(error.message).toContain('Node.js version');
    expect(error.message).toContain('.mjs');
    expect(error.message).toContain('type');
  });

  it('should include custom suggestions', () => {
    const originalError = new Error('Custom error');
    originalError.code = 'ERR_MODULE_NOT_FOUND';

    const error = new ImportError('@custom/package', originalError, {
      suggestion: 'This package is only available in v2.0+',
    });

    expect(error.message).toContain('Suggestion: This package is only available in v2.0+');
  });
});

// =============================================================================
// Safe Import Tests
// =============================================================================

describe('safeImport', () => {
  it('should throw enhanced error for missing packages', async () => {
    try {
      await safeImport('@unrdf/does-not-exist-xyz');
      expect.fail('Should throw ImportError');
    } catch (error) {
      expect(error).toBeInstanceOf(ImportError);
      expect(error.message).toContain('Cannot import package');
      expect(error.message).toContain('pnpm install');
    }
  });

  it('should successfully import existing packages', async () => {
    // Import a known package
    const module = await safeImport('zod');
    expect(module).toBeTruthy();
    expect(module.z).toBeTruthy();
  });
});

// =============================================================================
// Debug Mode Tests
// =============================================================================

describe('Debug Mode', () => {
  const originalDebug = process.env.DEBUG;

  beforeEach(() => {
    // Clear debug settings
    delete process.env.DEBUG;
  });

  afterEach(() => {
    process.env.DEBUG = originalDebug;
  });

  it('should detect DEBUG=* wildcard', async () => {
    process.env.DEBUG = '*';
    // Re-import to trigger initialization
    const { initializeDebugMode, isDebugEnabled } = await import('../src/utils/enhanced-errors.mjs');
    initializeDebugMode();

    expect(isDebugEnabled('unrdf:workflow')).toBe(true);
    expect(isDebugEnabled('unrdf:validation')).toBe(true);
    expect(isDebugEnabled('anything')).toBe(true);
  });

  it('should detect namespace-specific debug', async () => {
    process.env.DEBUG = 'unrdf:workflow';
    const { initializeDebugMode, isDebugEnabled } = await import('../src/utils/enhanced-errors.mjs');
    initializeDebugMode();

    expect(isDebugEnabled('unrdf:workflow')).toBe(true);
    expect(isDebugEnabled('unrdf:validation')).toBe(false);
  });

  it('should detect wildcard namespace debug', async () => {
    process.env.DEBUG = 'unrdf:*';
    const { initializeDebugMode, isDebugEnabled } = await import('../src/utils/enhanced-errors.mjs');
    initializeDebugMode();

    expect(isDebugEnabled('unrdf:workflow')).toBe(true);
    expect(isDebugEnabled('unrdf:validation')).toBe(true);
    expect(isDebugEnabled('other:namespace')).toBe(false);
  });

  it('should create debugger that respects namespace', async () => {
    process.env.DEBUG = 'unrdf:test';
    const { initializeDebugMode, createDebugger } = await import('../src/utils/enhanced-errors.mjs');
    initializeDebugMode();

    const debug = createDebugger('unrdf:test');

    // Should not throw
    expect(() => debug('Test message', { data: 'test' })).not.toThrow();
  });

  it('should trace workflow steps when debug enabled', async () => {
    process.env.DEBUG = 'unrdf:workflow';
    const { initializeDebugMode, traceWorkflowStep } = await import('../src/utils/enhanced-errors.mjs');
    initializeDebugMode();

    // Should not throw
    expect(() => traceWorkflowStep('enableTask', {
      taskId: 'task-1',
      caseId: 'case-1',
      state: 'RUNNING',
    })).not.toThrow();
  });
});

// =============================================================================
// Error Recovery Guide Tests
// =============================================================================

describe('Error Recovery Guide', () => {
  it('should generate recovery guide for WorkflowError', () => {
    const error = new WorkflowError('Test error', {
      workflowId: 'wf-123',
      state: 'INVALID',
    });

    const guide = getErrorRecoveryGuide(error);

    expect(guide).toContain('Workflow Error Recovery Guide');
    expect(guide).toContain('Recovery Steps');
    expect(guide).toContain('Prevention');
    expect(guide).toContain('Resources');
    expect(guide).toContain('checkpoint');
  });

  it('should generate recovery guide for ImportError', () => {
    const originalError = new Error('Not found');
    originalError.code = 'ERR_MODULE_NOT_FOUND';

    const error = new ImportError('@unrdf/test', originalError);
    const guide = getErrorRecoveryGuide(error);

    expect(guide).toContain('Import Error Recovery Guide');
    expect(guide).toContain('Quick Fixes');
    expect(guide).toContain('Verification');
    expect(guide).toContain('pnpm install');
    expect(guide).toContain('@unrdf/test');
  });

  it('should generate recovery guide for ValidationError', () => {
    const schema = z.object({ name: z.string() });

    try {
      schema.parse({ name: 123 });
      expect.fail('Should throw');
    } catch (error) {
      const enhanced = enhanceZodError(error);
      const guide = getErrorRecoveryGuide(enhanced);

      expect(guide).toContain('Validation Error Recovery Guide');
      expect(guide).toContain('Common Causes');
      expect(guide).toContain('Type mismatch');
    }
  });

  it('should generate generic recovery guide for unknown errors', () => {
    const error = new Error('Generic error');
    const guide = getErrorRecoveryGuide(error);

    expect(guide).toContain('Error Recovery Guide');
    expect(guide).toContain('General Troubleshooting');
    expect(guide).toContain('debug mode');
  });
});

// =============================================================================
// Integration Tests - Before/After Comparison
// =============================================================================

describe('Before/After UX Comparison', () => {
  it('BEFORE: Cryptic Zod error', () => {
    const schema = z.object({
      workflow: z.object({
        tasks: z.array(z.string()),
      }),
    });

    try {
      schema.parse({ workflow: { tasks: 'task1,task2,task3' } });
      expect.fail('Should throw');
    } catch (error) {
      // Before: Raw Zod error
      // In Zod 4.x, errors are in issues array
      expect(error.issues).toBeTruthy();
      expect(error.issues[0].expected).toBe('array');
      expect(error.issues[0].message).toContain('expected');

      // Limited context, no actionable fix
      expect(error.message).not.toContain('Suggested Fix');
      expect(error.message).not.toContain('pnpm');
      expect(error.message).not.toContain('Documentation');
      expect(error.message).not.toContain('Docs:');
    }
  });

  it('AFTER: Enhanced Zod error with fixes', () => {
    const schema = z.object({
      workflow: z.object({
        tasks: z.array(z.string()),
      }),
    });

    try {
      schema.parse({ workflow: { tasks: 'task1,task2,task3' } });
      expect.fail('Should throw');
    } catch (error) {
      const enhanced = enhanceZodError(error, { operation: 'workflow creation' });

      // After: Enhanced error with context
      expect(enhanced.message).toContain('Validation Error in workflow creation');
      expect(enhanced.message).toContain('Field: workflow.tasks');
      expect(enhanced.message).toContain('Expected: array');
      expect(enhanced.message).toContain('Received: string');

      // Actionable fix provided
      expect(enhanced.message).toContain('Suggested Fix');
      expect(enhanced.message).toContain('Change workflow.tasks from a string to an array');

      // Documentation link
      expect(enhanced.message).toContain('Documentation:');
      expect(enhanced.message).toContain('https://');
    }
  });

  it('BEFORE: Generic import error', async () => {
    try {
      await import('@unrdf/missing-package-xyz');
      expect.fail('Should throw');
    } catch (error) {
      // Before: Generic Node.js error
      expect(error.message).toContain('Cannot find');

      // No actionable guidance
      expect(error.message).not.toContain('pnpm install');
      expect(error.message).not.toContain('package.json');
      expect(error.message).not.toContain('Possible fixes');
    }
  });

  it('AFTER: Enhanced import error with fixes', async () => {
    try {
      await safeImport('@unrdf/missing-package-xyz');
      expect.fail('Should throw');
    } catch (error) {
      // After: Enhanced with actionable fixes
      expect(error.message).toContain('Cannot import package');
      expect(error.message).toContain('@unrdf/missing-package-xyz');

      // Specific fixes provided
      expect(error.message).toContain('Possible fixes');
      expect(error.message).toContain('pnpm install');
      expect(error.message).toContain('pnpm add @unrdf/missing-package-xyz');
      expect(error.message).toContain('Verification steps');
      expect(error.message).toContain('Docs:');
    }
  });
});
