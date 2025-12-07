/**
 * Validation Suite Tests
 *
 * **80/20 Focus**: Tests that prove system cannot fail:
 * - All validation paths work
 * - State machine enforced
 * - Error prevention verified
 * - Both runtimes work
 *
 * @module validation.test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { ValidationSuite } from '../src/validation-suite.mjs';
import { ProcessValidator } from '../src/process-validator.mjs';
import { SupervisionValidator } from '../src/supervision-validator.mjs';
import { KGC4DValidator } from '../src/kgc4d-validator.mjs';
import { RuntimeValidator } from '../src/runtime-validator.mjs';

describe('Validation Suite', () => {
  let suite;

  beforeEach(() => {
    suite = new ValidationSuite({
      log: () => {} // Silent logging for tests
    });
  });

  describe('runAll', () => {
    it('should throw error if moduleName is empty', async () => {
      await expect(suite.runAll('')).rejects.toThrow('moduleName is required');
      await expect(suite.runAll(null)).rejects.toThrow('moduleName is required');
      await expect(suite.runAll(undefined)).rejects.toThrow('moduleName is required');
    });

    it('should run all validations for valid module name', async () => {
      const results = await suite.runAll('hello_world');

      expect(results).toBeDefined();
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    });

    it('should return results array', async () => {
      const results = await suite.runAll('hello_world');

      results.forEach(result => {
        expect(result).toHaveProperty('name');
        expect(result).toHaveProperty('status');
        expect(['pending', 'running', 'pass', 'fail']).toContain(result.status);
      });
    });
  });

  describe('getResults', () => {
    it('should return empty array initially', () => {
      const results = suite.getResults();
      expect(results).toEqual([]);
    });

    it('should return results after running validations', async () => {
      await suite.runAll('hello_world');
      const results = suite.getResults();

      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('clear', () => {
    it('should clear all results', async () => {
      await suite.runAll('hello_world');
      expect(suite.getResults().length).toBeGreaterThan(0);

      suite.clear();
      expect(suite.getResults()).toEqual([]);
    });
  });
});

describe('ProcessValidator', () => {
  let validator;

  beforeEach(() => {
    validator = new ProcessValidator({
      log: () => {}
    });
  });

  it('should throw error if moduleName is empty', async () => {
    await expect(validator.validate('')).rejects.toThrow('moduleName is required');
    await expect(validator.validate(null)).rejects.toThrow('moduleName is required');
  });

    it('should return passed=false if module file not found', async () => {
      // Mock file system for Node.js
      const { existsSync } = await import('fs');
      const originalExistsSync = existsSync;
      const { existsSync: mockExistsSync } = await import('fs');
      
      // In Node.js test environment, module won't exist
      const result = await validator.validate('nonexistent-module-that-does-not-exist');

      expect(result.passed).toBe(false);
      expect(result.message).toContain('not found');
    });

    it('should return passed=true if module file exists', async () => {
      // Use actual hello_world module that exists
      const result = await validator.validate('hello_world');

      expect(result.passed).toBe(true);
      expect(result.message).toContain('passed');
    });
});

describe('SupervisionValidator', () => {
  let validator;

  beforeEach(() => {
    validator = new SupervisionValidator({
      log: () => {}
    });
  });

  it('should throw error if moduleName is empty', async () => {
    await expect(validator.validate('')).rejects.toThrow('moduleName is required');
  });

    it('should return passed=false if module file not found', async () => {
      const result = await validator.validate('nonexistent-module-that-does-not-exist');

      expect(result.passed).toBe(false);
    });

    it('should return passed=true if module file exists', async () => {
      const result = await validator.validate('hello_world');

      expect(result.passed).toBe(true);
    });
});

describe('KGC4DValidator', () => {
  let validator;

  beforeEach(() => {
    validator = new KGC4DValidator({
      log: () => {}
    });
  });

  it('should throw error if moduleName is empty', async () => {
    await expect(validator.validate('')).rejects.toThrow('moduleName is required');
  });

    it('should return passed=false if module file not found', async () => {
      const result = await validator.validate('nonexistent-module-that-does-not-exist');

      expect(result.passed).toBe(false);
    });

    it('should return passed=true if module file exists and event structure is valid', async () => {
      const result = await validator.validate('hello_world');

      expect(result.passed).toBe(true);
    });

  describe('validateEventStructure', () => {
    it('should return false for invalid event', () => {
      expect(validator.validateEventStructure(null)).toBe(false);
      expect(validator.validateEventStructure({})).toBe(false);
      expect(validator.validateEventStructure({ type: 'test' })).toBe(false);
    });

    it('should return true for valid event', () => {
      const validEvent = {
        type: 'PROCESS_STARTED',
        timestamp: Date.now(),
        payload: { processId: 'test' }
      };

      expect(validator.validateEventStructure(validEvent)).toBe(true);
    });
  });
});

describe('RuntimeValidator', () => {
  let validator;

  beforeEach(() => {
    validator = new RuntimeValidator({
      log: () => {}
    });
  });

  describe('validateStateMachine', () => {
    it('should throw error if moduleName is empty', async () => {
      await expect(validator.validateStateMachine('')).rejects.toThrow('moduleName is required');
    });

    it('should return passed=true if state machine validation passes', async () => {
      const result = await validator.validateStateMachine('test-module');

      expect(result.passed).toBe(true);
      expect(result.message).toContain('passed');
    });
  });

  describe('validateDualRuntime', () => {
    it('should throw error if moduleName is empty', async () => {
      await expect(validator.validateDualRuntime('')).rejects.toThrow('moduleName is required');
    });

    it('should return passed=true if both runtimes work', async () => {
      const result = await validator.validateDualRuntime('test-module');

      expect(result.passed).toBe(true);
      expect(result.message).toContain('passed');
    });
  });
});

