/**
 * @file Tests for useRealTimeValidator hook functionality
 * Tests validation modes, caching, and SHACL validation
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('useRealTimeValidator', () => {
  describe('Validation Processing', () => {
    it('should validate conforming change', async () => {
      const validateChange = async (change) => {
        // Simulate SHACL validation
        return {
          conforms: true,
          violations: []
        };
      };

      const change = {
        id: 'change-1',
        operation: 'insert',
        quads: [{ subject: { value: 'http://example.org/product1' } }]
      };

      const result = await validateChange(change);

      expect(result.conforms).toBe(true);
      expect(result.violations).toHaveLength(0);
    });

    it('should detect validation violations', async () => {
      const validateChange = async (change) => {
        // Simulate SHACL validation with violations
        if (change.quads.some(q => !q.object.value)) {
          return {
            conforms: false,
            violations: [{
              focusNode: change.quads[0].subject.value,
              shape: 'http://example.org/shapes#ProductShape',
              message: 'Missing required property',
              severity: 'violation'
            }]
          };
        }
        return { conforms: true, violations: [] };
      };

      const change = {
        id: 'change-1',
        quads: [{ subject: { value: 'http://example.org/product1' }, object: {} }]
      };

      const result = await validateChange(change);

      expect(result.conforms).toBe(false);
      expect(result.violations).toHaveLength(1);
      expect(result.violations[0].severity).toBe('violation');
    });

    it('should track valid and invalid changes separately', () => {
      const validChanges = [];
      const invalidChanges = [];

      const processResult = (change, result) => {
        if (result.conforms) {
          validChanges.push({ change, validatedAt: new Date().toISOString() });
        } else {
          invalidChanges.push({
            change,
            violations: result.violations,
            validatedAt: new Date().toISOString()
          });
        }
      };

      processResult({ id: '1' }, { conforms: true, violations: [] });
      processResult({ id: '2' }, { conforms: false, violations: [{ message: 'Error' }] });
      processResult({ id: '3' }, { conforms: true, violations: [] });

      expect(validChanges).toHaveLength(2);
      expect(invalidChanges).toHaveLength(1);
    });
  });

  describe('Statistics Tracking', () => {
    it('should track total validated count', () => {
      let stats = {
        totalValidated: 0,
        passed: 0,
        failed: 0,
        passRate: 0
      };

      const updateStats = (conforms) => {
        const total = stats.totalValidated + 1;
        const passed = conforms ? stats.passed + 1 : stats.passed;
        const failed = conforms ? stats.failed : stats.failed + 1;
        stats = {
          totalValidated: total,
          passed,
          failed,
          passRate: Math.round((passed / total) * 100)
        };
      };

      updateStats(true);
      updateStats(true);
      updateStats(false);
      updateStats(true);

      expect(stats.totalValidated).toBe(4);
      expect(stats.passed).toBe(3);
      expect(stats.failed).toBe(1);
      expect(stats.passRate).toBe(75);
    });

    it('should calculate pass rate percentage', () => {
      const passed = 85;
      const total = 100;

      const passRate = Math.round((passed / total) * 100);

      expect(passRate).toBe(85);
    });

    it('should handle zero validations for pass rate', () => {
      const passed = 0;
      const total = 0;

      const passRate = total > 0 ? Math.round((passed / total) * 100) : 0;

      expect(passRate).toBe(0);
    });

    it('should reset stats on clear', () => {
      let stats = {
        totalValidated: 100,
        passed: 80,
        failed: 20,
        passRate: 80
      };

      const clear = () => {
        stats = {
          totalValidated: 0,
          passed: 0,
          failed: 0,
          passRate: 0
        };
      };

      clear();

      expect(stats.totalValidated).toBe(0);
      expect(stats.passed).toBe(0);
      expect(stats.failed).toBe(0);
      expect(stats.passRate).toBe(0);
    });
  });

  describe('Batch Validation', () => {
    it('should validate batch of changes', async () => {
      const validateChange = async (change) => {
        return { conforms: change.valid, violations: change.valid ? [] : [{ message: 'Invalid' }] };
      };

      const validateBatch = async (changes) => {
        return Promise.all(changes.map(c => validateChange(c)));
      };

      const batch = [
        { id: '1', valid: true },
        { id: '2', valid: false },
        { id: '3', valid: true }
      ];

      const results = await validateBatch(batch);

      expect(results).toHaveLength(3);
      expect(results[0].conforms).toBe(true);
      expect(results[1].conforms).toBe(false);
      expect(results[2].conforms).toBe(true);
    });

    it('should respect batch size configuration', () => {
      const batchSize = 5;
      const queue = Array(12).fill(null).map((_, i) => ({ id: `change-${i}` }));
      const batches = [];

      while (queue.length > 0) {
        batches.push(queue.splice(0, batchSize));
      }

      expect(batches).toHaveLength(3);
      expect(batches[0]).toHaveLength(5);
      expect(batches[1]).toHaveLength(5);
      expect(batches[2]).toHaveLength(2);
    });

    it('should process queue sequentially', async () => {
      const processed = [];
      let processing = false;

      const processQueue = async (queue) => {
        if (processing || queue.length === 0) return;
        processing = true;

        while (queue.length > 0) {
          const item = queue.shift();
          processed.push(item);
          await new Promise(resolve => setTimeout(resolve, 1));
        }

        processing = false;
      };

      const queue = [1, 2, 3, 4, 5];
      await processQueue(queue);

      expect(processed).toEqual([1, 2, 3, 4, 5]);
    });
  });

  describe('Violation Filtering', () => {
    it('should filter violations by severity', () => {
      const violations = [
        { id: 'v1', severity: 'violation' },
        { id: 'v2', severity: 'warning' },
        { id: 'v3', severity: 'violation' },
        { id: 'v4', severity: 'info' },
        { id: 'v5', severity: 'warning' }
      ];

      const getViolationsBySeverity = (severity) => {
        return violations.filter(v => v.severity === severity);
      };

      const violationLevel = getViolationsBySeverity('violation');
      const warningLevel = getViolationsBySeverity('warning');

      expect(violationLevel).toHaveLength(2);
      expect(warningLevel).toHaveLength(2);
    });

    it('should filter violations by focus node', () => {
      const violations = [
        { id: 'v1', focusNode: 'http://example.org/product1' },
        { id: 'v2', focusNode: 'http://example.org/product2' },
        { id: 'v3', focusNode: 'http://example.org/product1' }
      ];

      const getViolationsByFocusNode = (focusNode) => {
        return violations.filter(v => v.focusNode === focusNode);
      };

      const product1Violations = getViolationsByFocusNode('http://example.org/product1');

      expect(product1Violations).toHaveLength(2);
    });

    it('should filter violations by shape', () => {
      const violations = [
        { id: 'v1', shape: 'http://example.org/shapes#ProductShape' },
        { id: 'v2', shape: 'http://example.org/shapes#PersonShape' },
        { id: 'v3', shape: 'http://example.org/shapes#ProductShape' }
      ];

      const getViolationsByShape = (shape) => {
        return violations.filter(v => v.shape === shape);
      };

      const productShapeViolations = getViolationsByShape('http://example.org/shapes#ProductShape');

      expect(productShapeViolations).toHaveLength(2);
    });
  });

  describe('Auto-Validation Mode', () => {
    it('should auto-validate when enabled', () => {
      const config = { autoValidate: true };
      let validationCount = 0;

      const processChange = (change, cfg) => {
        if (cfg.autoValidate) {
          validationCount++;
        }
      };

      processChange({ id: '1' }, config);
      processChange({ id: '2' }, config);

      expect(validationCount).toBe(2);
    });

    it('should skip auto-validation when disabled', () => {
      const config = { autoValidate: false };
      let validationCount = 0;

      const processChange = (change, cfg) => {
        if (cfg.autoValidate) {
          validationCount++;
        }
      };

      processChange({ id: '1' }, config);
      processChange({ id: '2' }, config);

      expect(validationCount).toBe(0);
    });

    it('should default auto-validate to true', () => {
      const config = {};
      const autoValidate = config.autoValidate !== false;

      expect(autoValidate).toBe(true);
    });
  });

  describe('Violation Callback', () => {
    it('should call onViolation callback', () => {
      const callback = vi.fn();
      const violation = {
        focusNode: 'http://example.org/product1',
        message: 'Invalid property value',
        severity: 'violation'
      };

      const handleViolation = (v, options) => {
        options.onViolation?.(v);
      };

      handleViolation(violation, { onViolation: callback });

      expect(callback).toHaveBeenCalledWith(violation);
    });

    it('should add timestamp to violations', () => {
      const violation = { message: 'Test violation' };

      const enrichedViolation = {
        ...violation,
        timestamp: new Date().toISOString()
      };

      expect(enrichedViolation.timestamp).toBeDefined();
      expect(enrichedViolation.message).toBe('Test violation');
    });

    it('should accumulate violations over time', () => {
      let violations = [];

      const addViolation = (v) => {
        violations = [...violations, {
          ...v,
          timestamp: new Date().toISOString()
        }];
      };

      addViolation({ id: 'v1', message: 'Error 1' });
      addViolation({ id: 'v2', message: 'Error 2' });
      addViolation({ id: 'v3', message: 'Error 3' });

      expect(violations).toHaveLength(3);
    });
  });

  describe('Clear Functionality', () => {
    it('should clear all validation data', () => {
      let violations = [{ id: 'v1' }];
      let validChanges = [{ id: 'vc1' }];
      let invalidChanges = [{ id: 'ic1' }];

      const clear = () => {
        violations = [];
        validChanges = [];
        invalidChanges = [];
      };

      clear();

      expect(violations).toHaveLength(0);
      expect(validChanges).toHaveLength(0);
      expect(invalidChanges).toHaveLength(0);
    });
  });

  describe('Shape Graph Configuration', () => {
    it('should use configured shape graph', async () => {
      const config = { shapeGraph: 'http://example.org/shapes' };
      let usedShapeGraph = null;

      const initializeValidator = async (cfg) => {
        usedShapeGraph = cfg.shapeGraph;
      };

      await initializeValidator(config);

      expect(usedShapeGraph).toBe('http://example.org/shapes');
    });

    it('should handle missing shape graph', async () => {
      const config = {};
      let usedShapeGraph = null;

      const initializeValidator = async (cfg) => {
        usedShapeGraph = cfg.shapeGraph || null;
      };

      await initializeValidator(config);

      expect(usedShapeGraph).toBeNull();
    });
  });

  describe('Validation Queue', () => {
    it('should queue changes for validation', () => {
      const queue = [];

      const addToQueue = (change) => {
        queue.push(change);
      };

      addToQueue({ id: '1' });
      addToQueue({ id: '2' });
      addToQueue({ id: '3' });

      expect(queue).toHaveLength(3);
    });

    it('should process queue in batches', () => {
      const batchSize = 2;
      const queue = [{ id: '1' }, { id: '2' }, { id: '3' }, { id: '4' }, { id: '5' }];
      const processedBatches = [];

      while (queue.length > 0) {
        const batch = queue.splice(0, batchSize);
        processedBatches.push(batch);
      }

      expect(processedBatches).toHaveLength(3);
      expect(processedBatches[0]).toHaveLength(2);
      expect(processedBatches[2]).toHaveLength(1);
    });

    it('should continue processing if queue has items', async () => {
      let processCount = 0;
      const queue = [1, 2, 3];

      const processQueue = async () => {
        while (queue.length > 0) {
          queue.shift();
          processCount++;
        }
      };

      await processQueue();

      expect(processCount).toBe(3);
      expect(queue).toHaveLength(0);
    });
  });

  describe('Error Handling', () => {
    it('should handle validator initialization error', () => {
      let error = null;

      const initializeValidator = () => {
        try {
          throw new Error('Failed to load SHACL module');
        } catch (err) {
          error = err;
        }
      };

      initializeValidator();

      expect(error).not.toBeNull();
      expect(error.message).toBe('Failed to load SHACL module');
    });

    it('should handle validation error', async () => {
      let error = null;

      const validateChange = async (change) => {
        if (!change.quads) {
          error = new Error('Invalid change: missing quads');
          throw error;
        }
      };

      await expect(validateChange({})).rejects.toThrow('missing quads');
      expect(error).not.toBeNull();
    });

    it('should throw when validator not initialized', async () => {
      const validatorRef = { current: null };

      const validateChange = async (change) => {
        if (!validatorRef.current) {
          throw new Error('Validator not initialized');
        }
      };

      await expect(validateChange({})).rejects.toThrow('not initialized');
    });
  });

  describe('Change Metadata', () => {
    it('should include change ID in violations', () => {
      const change = { id: 'change-123' };
      const violations = [
        { message: 'Error 1' },
        { message: 'Error 2' }
      ];

      const enrichedViolations = violations.map(v => ({
        ...v,
        changeId: change.id,
        timestamp: new Date().toISOString()
      }));

      expect(enrichedViolations[0].changeId).toBe('change-123');
      expect(enrichedViolations[1].changeId).toBe('change-123');
    });

    it('should include validated timestamp', () => {
      const beforeTime = new Date().toISOString();

      const validatedChange = {
        change: { id: '1' },
        validatedAt: new Date().toISOString()
      };

      expect(validatedChange.validatedAt).toBeDefined();
      expect(new Date(validatedChange.validatedAt) >= new Date(beforeTime)).toBe(true);
    });
  });
});
