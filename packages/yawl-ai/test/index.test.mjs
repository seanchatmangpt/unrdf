/**
 * @file YAWL AI Tests
 * @module @unrdf/yawl-ai/test/index
 */

import { describe, it, expect } from 'vitest';
import {
  createPredictor,
  createOptimizer,
  createDetector,
  createAdapter,
} from '../src/index.mjs';

describe('@unrdf/yawl-ai', () => {
  describe('Module Exports', () => {
    it('should export createPredictor function', () => {
      expect(createPredictor).toBeDefined();
      expect(typeof createPredictor).toBe('function');
    });

    it('should export createOptimizer function', () => {
      expect(createOptimizer).toBeDefined();
      expect(typeof createOptimizer).toBe('function');
    });

    it('should export createDetector function', () => {
      expect(createDetector).toBeDefined();
      expect(typeof createDetector).toBe('function');
    });

    it('should export createAdapter function', () => {
      expect(createAdapter).toBeDefined();
      expect(typeof createAdapter).toBe('function');
    });
  });

  describe('WorkflowPathPredictor', () => {
    it('should create a predictor instance', () => {
      const predictor = createPredictor();
      expect(predictor).toBeDefined();
      expect(predictor).toHaveProperty('predict');
      expect(typeof predictor.predict).toBe('function');
    });

    it('should have train method', () => {
      const predictor = createPredictor();
      expect(predictor).toHaveProperty('train');
      expect(typeof predictor.train).toBe('function');
    });
  });

  describe('PerformanceOptimizer', () => {
    it('should create an optimizer instance', () => {
      const optimizer = createOptimizer();
      expect(optimizer).toBeDefined();
      expect(optimizer).toHaveProperty('analyze');
      expect(typeof optimizer.analyze).toBe('function');
    });

    it('should have generateReport method', () => {
      const optimizer = createOptimizer();
      expect(optimizer).toHaveProperty('generateReport');
      expect(typeof optimizer.generateReport).toBe('function');
    });
  });

  describe('AnomalyDetector', () => {
    it('should create a detector instance', () => {
      const detector = createDetector();
      expect(detector).toBeDefined();
      expect(detector).toHaveProperty('detect');
      expect(typeof detector.detect).toBe('function');
    });

    it('should have train method', () => {
      const detector = createDetector();
      expect(detector).toHaveProperty('train');
      expect(typeof detector.train).toBe('function');
    });
  });

  describe('YAWLMLAdapter', () => {
    it('should create an adapter instance with engine', () => {
      const mockEngine = {
        on: () => {},
        emit: () => {},
        getWorkflowInstances: () => [],
      };

      const adapter = createAdapter(mockEngine);
      expect(adapter).toBeDefined();
      expect(adapter).toHaveProperty('trainModels');
      expect(typeof adapter.trainModels).toBe('function');
    });

    it('should accept configuration options', () => {
      const mockEngine = {
        on: () => {},
        emit: () => {},
        getWorkflowInstances: () => [],
      };

      const predictor = createPredictor();
      const optimizer = createOptimizer();
      const detector = createDetector();

      const adapter = createAdapter(mockEngine, {
        predictor,
        optimizer,
        detector,
      });

      expect(adapter).toBeDefined();
    });
  });
});
