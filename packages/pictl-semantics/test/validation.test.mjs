/**
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import {
  validatePictlResult,
  validateAgainstShapes,
  validateBatchResults,
  PictlResultSchema,
  PICTL_SHAPES,
} from '../src/result-validator.mjs';

describe('PICTL Result Validation', () => {
  describe('Schema Validation', () => {
    it('should validate complete result', () => {
      const result = {
        modelId: 'model-1',
        fitness: 0.92,
        precision: 0.88,
        generalization: 0.85,
        simplicity: 0.90,
        alignmentCost: 5,
        moveOnLog: 2,
        moveOnModel: 1,
        synchronous: 20,
        model: {
          type: 'petri-net',
          places: 5,
          transitions: 4,
          arcs: 12,
        },
        logSize: 100,
        traceCount: 25,
        eventCount: 500,
        timestamp: Date.now(),
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
      expect(validation.schemaValid).toBe(true);
      expect(validation.shapesValid).toBe(true);
      expect(validation.errors).toHaveLength(0);
    });

    it('should validate minimal result', () => {
      const result = {
        fitness: 0.92,
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
      expect(validation.schemaValid).toBe(true);
    });

    it('should validate with optional fields', () => {
      const result = {
        fitness: 0.92,
        precision: 0.88,
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
    });

    it('should reject invalid fitness value', () => {
      const result = {
        fitness: 1.5, // Out of range [0, 1]
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(false);
      expect(validation.schemaValid).toBe(false);
      expect(validation.errors.length).toBeGreaterThan(0);
    });

    it('should reject invalid precision value', () => {
      const result = {
        precision: -0.5, // Out of range [0, 1]
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(false);
      expect(validation.errors.length).toBeGreaterThan(0);
    });

    it('should reject invalid model type', () => {
      const result = {
        model: {
          type: 'invalid-model-type',
        },
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(false);
    });

    it('should reject negative logSize', () => {
      const result = {
        logSize: -5,
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(false);
    });
  });

  describe('Quality Warnings', () => {
    it('should warn about perfect fitness', () => {
      const result = {
        fitness: 1.0,
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
      expect(validation.warnings).toContain('Fitness score of 1.0 may indicate overfitting');
    });

    it('should warn about zero precision', () => {
      const result = {
        precision: 0.0,
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
      expect(validation.warnings).toContain('Precision score of 0.0 suggests poor model');
    });

    it('should warn about large fitness-precision gap', () => {
      const result = {
        fitness: 0.95,
        precision: 0.60,
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
      expect(validation.warnings.some(w => w.includes('Large gap between fitness and precision'))).toBe(true);
    });

    it('should warn about high normalized alignment cost', () => {
      const result = {
        alignmentCost: 200,
        logSize: 10,
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
      expect(validation.warnings.some(w => w.includes('normalized alignment cost'))).toBe(true);
    });
  });

  describe('SHACL Shape Validation', () => {
    it('should validate fitness against FitnessShape', () => {
      const data = { fitness: 0.92 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.fitness.shapeUri);

      expect(validation.valid).toBe(true);
      expect(validation.shapeFound).toBe(true);
      expect(validation.conforms.length).toBeGreaterThan(0);
    });

    it('should reject fitness out of range', () => {
      const data = { fitness: 1.5 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.fitness.shapeUri);

      expect(validation.valid).toBe(false);
      expect(validation.violations.length).toBeGreaterThan(0);
    });

    it('should validate precision against PrecisionShape', () => {
      const data = { precision: 0.88 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.precision.shapeUri);

      expect(validation.valid).toBe(true);
    });

    it('should validate alignment cost against AlignmentCostShape', () => {
      const data = { alignmentCost: 10 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.alignmentCost.shapeUri);

      expect(validation.valid).toBe(true);
    });

    it('should reject negative alignment cost', () => {
      const data = { alignmentCost: -5 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.alignmentCost.shapeUri);

      expect(validation.valid).toBe(false);
    });

    it('should validate log size against LogSizeShape', () => {
      const data = { logSize: 100 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.logSize.shapeUri);

      expect(validation.valid).toBe(true);
    });

    it('should reject zero log size', () => {
      const data = { logSize: 0 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.logSize.shapeUri);

      expect(validation.valid).toBe(false);
    });

    it('should handle unknown shapes', () => {
      const data = { someValue: 42 };
      const validation = validateAgainstShapes(data, 'urn:unknown:Shape');

      expect(validation.valid).toBe(false);
      expect(validation.shapeFound).toBe(false);
    });

    it('should extract metric name from shape URI', () => {
      const data = { fitness: 0.92 };
      const validation = validateAgainstShapes(data, PICTL_SHAPES.fitness.shapeUri);

      expect(validation.value).toBe(0.92);
    });
  });

  describe('Batch Validation', () => {
    it('should validate multiple results', () => {
      const results = [
        { fitness: 0.92, precision: 0.88 },
        { fitness: 0.85, precision: 0.90 },
        { fitness: 0.95, precision: 0.87 },
      ];

      const validation = validateBatchResults(results);

      expect(validation.totalResults).toBe(3);
      expect(validation.validCount).toBe(3);
      expect(validation.invalidCount).toBe(0);
      expect(validation.validationRate).toBe(1.0);
    });

    it('should identify invalid results in batch', () => {
      const results = [
        { fitness: 0.92 },
        { fitness: 1.5 }, // Invalid
        { fitness: 0.88 },
        { fitness: -0.5 }, // Invalid
      ];

      const validation = validateBatchResults(results);

      expect(validation.totalResults).toBe(4);
      expect(validation.validCount).toBe(2);
      expect(validation.invalidCount).toBe(2);
      expect(validation.validationRate).toBe(0.5);
    });

    it('should collect all validations', () => {
      const results = [
        { fitness: 0.92 },
        { precision: 0.88 },
      ];

      const validation = validateBatchResults(results);

      expect(validation.validations).toHaveLength(2);
      expect(validation.validations[0].validation.valid).toBe(true);
      expect(validation.validations[1].validation.valid).toBe(true);
    });

    it('should handle empty batch', () => {
      const validation = validateBatchResults([]);

      expect(validation.totalResults).toBe(0);
      expect(validation.validCount).toBe(0);
      expect(validation.validationRate).toBe(0);
    });
  });

  describe('Schema Conformance', () => {
    it('should parse schema with safeParse', () => {
      const result = {
        fitness: 0.92,
      };

      const parsed = PictlResultSchema.safeParse(result);

      expect(parsed.success).toBe(true);
      expect(parsed.data.fitness).toBe(0.92);
    });

    it('should return error details for invalid data', () => {
      const result = {
        fitness: 'not-a-number',
      };

      const parsed = PictlResultSchema.safeParse(result);

      expect(parsed.success).toBe(false);
      expect(parsed.error).toBeDefined();
    });
  });

  describe('Model Type Validation', () => {
    it('should accept valid model types', () => {
      const types = ['petri-net', 'bpmn', 'dfg', 'other'];

      for (const type of types) {
        const result = {
          model: { type },
        };

        const validation = validatePictlResult(result);
        expect(validation.valid).toBe(true);
      }
    });

    it('should validate model structure', () => {
      const result = {
        model: {
          type: 'petri-net',
          places: 10,
          transitions: 8,
          arcs: 25,
        },
      };

      const validation = validatePictlResult(result);

      expect(validation.valid).toBe(true);
      expect(validation.data.model.places).toBe(10);
    });
  });

  describe('Quality Metric Ranges', () => {
    it('should accept metrics at boundaries', () => {
      const boundaries = [
        { fitness: 0.0 },
        { fitness: 1.0 },
        { precision: 0.0 },
        { precision: 1.0 },
        { generalization: 0.0 },
        { generalization: 1.0 },
        { simplicity: 0.0 },
        { simplicity: 1.0 },
      ];

      for (const metric of boundaries) {
        const validation = validatePictlResult(metric);
        expect(validation.valid).toBe(true);
      }
    });

    it('should reject metrics outside boundaries', () => {
      const invalid = [
        { fitness: -0.1 },
        { fitness: 1.1 },
        { precision: -0.0001 },
        { precision: 1.0001 },
      ];

      for (const metric of invalid) {
        const validation = validatePictlResult(metric);
        expect(validation.valid).toBe(false);
      }
    });
  });
});
