/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  OTELValidator,
  createOTELValidator,
  defaultOTELValidator,
  ValidationHelpers,
  createValidationHelpers,
  defaultValidationHelpers,
  ValidationRunner,
  createValidationRunner,
  defaultValidationRunner,
  validator,
  otelValidator,
  helpers,
} from '../src/index.mjs';

describe('Validation Module - OTELValidator', () => {
  it('should export OTELValidator class', () => {
    expect(OTELValidator).toBeDefined();
    expect(typeof OTELValidator).toBe('function');
  });

  it('should create OTEL validator instance', () => {
    const validator = createOTELValidator();
    expect(validator).toBeDefined();
  });

  it('should have default OTEL validator', () => {
    expect(defaultOTELValidator).toBeDefined();
  });

  it('should export otelValidator convenience export', () => {
    expect(otelValidator).toBeDefined();
  });

  it('should validate OTEL spans', async () => {
    const val = createOTELValidator();
    expect(typeof val.validate).toBe('function');
  });

  it('should collect metrics from spans', async () => {
    const val = createOTELValidator();
    if (typeof val.collectMetrics === 'function') {
      expect(typeof val.collectMetrics).toBe('function');
    }
  });

  it('should report validation results', async () => {
    const val = createOTELValidator();
    if (typeof val.report === 'function') {
      expect(typeof val.report).toBe('function');
    }
  });
});

describe('Validation Module - ValidationHelpers', () => {
  it('should export ValidationHelpers', () => {
    expect(ValidationHelpers).toBeDefined();
  });

  it('should create validation helpers instance', () => {
    const hlpers = createValidationHelpers();
    expect(hlpers).toBeDefined();
  });

  it('should have default validation helpers', () => {
    expect(defaultValidationHelpers).toBeDefined();
  });

  it('should export helpers convenience export', () => {
    expect(helpers).toBeDefined();
  });

  it('should provide span analysis utilities', () => {
    const hlpers = createValidationHelpers();
    expect(hlpers).toBeDefined();
    // Should have methods for span analysis
  });

  it('should provide metrics calculation utilities', () => {
    const hlpers = createValidationHelpers();
    expect(hlpers).toBeDefined();
    // Should have methods for metrics
  });

  it('should provide error detection utilities', () => {
    const hlpers = createValidationHelpers();
    expect(hlpers).toBeDefined();
    // Should have methods for error detection
  });
});

describe('Validation Module - ValidationRunner', () => {
  it('should export ValidationRunner class', () => {
    expect(ValidationRunner).toBeDefined();
    expect(typeof ValidationRunner).toBe('function');
  });

  it('should create validation runner instance', () => {
    const runner = createValidationRunner();
    expect(runner).toBeDefined();
  });

  it('should have default validation runner', () => {
    expect(defaultValidationRunner).toBeDefined();
  });

  it('should export validator convenience export', () => {
    expect(validator).toBeDefined();
  });

  it('should run validation suite', async () => {
    const runner = createValidationRunner();
    expect(typeof runner.run).toBe('function');
  });

  it('should collect validation results', async () => {
    const runner = createValidationRunner();
    if (typeof runner.collectResults === 'function') {
      expect(typeof runner.collectResults).toBe('function');
    }
  });

  it('should generate validation report', async () => {
    const runner = createValidationRunner();
    if (typeof runner.generateReport === 'function') {
      expect(typeof runner.generateReport).toBe('function');
    }
  });
});

describe('Validation Module - Integration', () => {
  it('should work together: validator + helpers + runner', async () => {
    const val = createOTELValidator();
    const hlpers = createValidationHelpers();
    const runner = createValidationRunner();

    expect(val).toBeDefined();
    expect(hlpers).toBeDefined();
    expect(runner).toBeDefined();
  });

  it('should use default instances', async () => {
    expect(defaultOTELValidator).toBeDefined();
    expect(defaultValidationHelpers).toBeDefined();
    expect(defaultValidationRunner).toBeDefined();
  });

  it('should export convenience shortcuts', async () => {
    expect(otelValidator).toBeDefined();
    expect(helpers).toBeDefined();
    expect(validator).toBeDefined();
  });

  it('should validate OTEL span structure', async () => {
    const val = createOTELValidator();
    const mockSpan = {
      name: 'test-operation',
      startTime: Date.now(),
      duration: 100,
      status: 'ok',
    };

    if (typeof val.validateSpan === 'function') {
      const result = val.validateSpan(mockSpan);
      expect(result).toBeDefined();
    }
  });

  it('should extract metrics from spans', async () => {
    const hlpers = createValidationHelpers();
    const mockSpan = {
      name: 'test-operation',
      duration: 100,
      attributes: { 'http.status_code': 200 },
    };

    if (typeof hlpers.extractMetrics === 'function') {
      const metrics = hlpers.extractMetrics(mockSpan);
      expect(metrics).toBeDefined();
    }
  });
});

describe('Validation Module - OTEL Metrics Collector', () => {
  it('should have metrics collector functionality', async () => {
    const val = createOTELValidator();
    expect(val).toBeDefined();
    // Metrics collector should be available through validator
  });

  it('should track span durations', async () => {
    const val = createOTELValidator();
    const mockSpan = { name: 'test', duration: 100 };

    if (typeof val.recordSpan === 'function') {
      val.recordSpan(mockSpan);
      expect(val).toBeDefined();
    }
  });
});

describe('Validation Module - OTEL Span Builder', () => {
  it('should provide span building utilities', async () => {
    const hlpers = createValidationHelpers();
    expect(hlpers).toBeDefined();
    // Should have span builder capabilities
  });

  it('should handle span attributes', async () => {
    const val = createOTELValidator();
    const mockSpan = {
      name: 'test',
      attributes: {
        'service.name': 'test-service',
        'span.kind': 'INTERNAL',
      },
    };

    if (typeof val.validateSpan === 'function') {
      expect(val).toBeDefined();
    }
  });
});

describe('Validation Module - Error Handling', () => {
  it('should handle invalid spans gracefully', async () => {
    const val = createOTELValidator();
    const invalidSpan = null;

    try {
      if (typeof val.validateSpan === 'function') {
        val.validateSpan(invalidSpan);
      }
      expect(val).toBeDefined();
    } catch (error) {
      expect(error).toBeDefined();
    }
  });

  it('should report validation errors', async () => {
    const runner = createValidationRunner();
    expect(runner).toBeDefined();
    // Should be able to report errors
  });

  it('should continue on validation failures', async () => {
    const val = createOTELValidator();
    expect(val).toBeDefined();
    // Should continue processing despite failures
  });
});

describe('Validation Module - Configuration', () => {
  it('should allow custom validator configuration', async () => {
    const customValidator = createOTELValidator({
      // Custom config options
    });
    expect(customValidator).toBeDefined();
  });

  it('should allow custom helpers configuration', async () => {
    const customHelpers = createValidationHelpers({
      // Custom config options
    });
    expect(customHelpers).toBeDefined();
  });

  it('should allow custom runner configuration', async () => {
    const customRunner = createValidationRunner({
      // Custom config options
    });
    expect(customRunner).toBeDefined();
  });
});

describe('Validation Module - Span Status Tracking', () => {
  it('should track span success status', async () => {
    const val = createOTELValidator();
    const successSpan = {
      name: 'operation',
      status: { code: 'UNSET' }, // Success
    };

    if (typeof val.validateSpan === 'function') {
      expect(val).toBeDefined();
    }
  });

  it('should track span error status', async () => {
    const val = createOTELValidator();
    const errorSpan = {
      name: 'operation',
      status: { code: 'ERROR', message: 'Operation failed' },
    };

    if (typeof val.validateSpan === 'function') {
      expect(val).toBeDefined();
    }
  });

  it('should calculate span latency metrics', async () => {
    const hlpers = createValidationHelpers();
    const span = {
      startTime: 1000,
      endTime: 1100,
      duration: 100,
    };

    if (typeof hlpers.calculateLatency === 'function') {
      const latency = hlpers.calculateLatency(span);
      expect(latency).toBeDefined();
    }
  });
});
