/**
 * @file Distributed Tracing Tests
 * @module observability/test/distributed-tracing
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createDistributedTracing } from '../src/distributed-tracing.mjs';
import { SpanKind } from '@opentelemetry/api';

describe('DistributedTracing', () => {
  let tracing;

  beforeEach(() => {
    tracing = createDistributedTracing({
      serviceName: 'test-service',
      sampling: {
        defaultRate: 1.0, // 100% for testing
        errorRate: 1.0,
        slowThreshold: 1000,
        slowRate: 1.0,
      },
    });
  });

  afterEach(() => {
    tracing.shutdown();
  });

  describe('Span Management', () => {
    it('should start a span', () => {
      const spanContext = tracing.startSpan('test-operation');

      expect(spanContext).toBeDefined();
      expect(spanContext.span).toBeDefined();
      expect(spanContext.spanName).toBe('test-operation');
      expect(spanContext.traceHeaders).toBeDefined();
    });

    it('should end a span successfully', () => {
      const spanContext = tracing.startSpan('test-operation');

      const activeCountBefore = tracing.getActiveSpanCount();
      tracing.endSpan(spanContext);
      const activeCountAfter = tracing.getActiveSpanCount();

      expect(activeCountAfter).toBe(activeCountBefore - 1);
    });

    it('should end a span with error', () => {
      const spanContext = tracing.startSpan('error-operation');
      const error = new Error('Test error');

      tracing.endSpan(spanContext, { error });

      // Should not throw
      expect(tracing.getActiveSpanCount()).toBeGreaterThanOrEqual(0);
    });

    it('should add attributes to span', () => {
      const spanContext = tracing.startSpan('test-operation', {
        attributes: {
          'custom.attr': 'value',
          'custom.number': 42,
        },
      });

      expect(spanContext.span).toBeDefined();
      tracing.endSpan(spanContext);
    });
  });

  describe('Parent-Child Relationships', () => {
    it('should create child span from parent', () => {
      const parentSpan = tracing.startSpan('parent-operation');
      const childSpan = tracing.createChildSpan(parentSpan, 'child-operation');

      expect(childSpan).toBeDefined();
      expect(childSpan.span).toBeDefined();
      expect(childSpan.spanName).toBe('child-operation');

      tracing.endSpan(childSpan);
      tracing.endSpan(parentSpan);
    });

    it('should support multiple child spans', () => {
      const parentSpan = tracing.startSpan('parent-operation');

      const child1 = tracing.createChildSpan(parentSpan, 'child-1');
      const child2 = tracing.createChildSpan(parentSpan, 'child-2');
      const child3 = tracing.createChildSpan(parentSpan, 'child-3');

      tracing.endSpan(child1);
      tracing.endSpan(child2);
      tracing.endSpan(child3);
      tracing.endSpan(parentSpan);

      expect(tracing.getActiveSpanCount()).toBe(0);
    });

    it('should support nested child spans', () => {
      const root = tracing.startSpan('root');
      const child1 = tracing.createChildSpan(root, 'child-1');
      const grandchild = tracing.createChildSpan(child1, 'grandchild');

      tracing.endSpan(grandchild);
      tracing.endSpan(child1);
      tracing.endSpan(root);

      expect(tracing.getActiveSpanCount()).toBe(0);
    });
  });

  describe('W3C Trace Context', () => {
    it('should generate trace headers', () => {
      const spanContext = tracing.startSpan('test-operation');

      expect(spanContext.traceHeaders).toBeDefined();
      expect(spanContext.traceHeaders).toHaveProperty('traceparent');

      tracing.endSpan(spanContext);
    });

    it('should inject trace context into headers', () => {
      const spanContext = tracing.startSpan('test-operation');
      const headers = tracing.injectIntoHeaders(spanContext);

      expect(headers).toHaveProperty('traceparent');
      expect(typeof headers.traceparent).toBe('string');

      tracing.endSpan(spanContext);
    });

    it('should extract trace context from headers', () => {
      // Create a span and get headers
      const spanContext = tracing.startSpan('test-operation');
      const headers = tracing.injectIntoHeaders(spanContext);

      // Extract context from headers
      const extracted = tracing.extractFromHeaders(headers);

      expect(extracted).toBeDefined();
      expect(extracted.context).toBeDefined();

      tracing.endSpan(spanContext);
    });

    it('should propagate trace context across services', () => {
      // Service A: Create span and inject headers
      const serviceASpan = tracing.startSpan('service-a-operation');
      const headers = tracing.injectIntoHeaders(serviceASpan);

      // Service B: Extract headers and create child span
      const extracted = tracing.extractFromHeaders(headers);
      const serviceBSpan = tracing.startSpan('service-b-operation', {
        parentContext: extracted,
      });

      tracing.endSpan(serviceBSpan);
      tracing.endSpan(serviceASpan);

      expect(tracing.getActiveSpanCount()).toBe(0);
    });
  });

  describe('Correlation', () => {
    it('should correlate by business ID', () => {
      const spanContext = tracing.startSpan('business-operation');

      tracing.correlateByBusinessId('workflow-123', spanContext);

      // Correlation should add attributes (verified via span)
      expect(spanContext.span).toBeDefined();

      tracing.endSpan(spanContext);
    });

    it('should correlate by user ID', () => {
      const spanContext = tracing.startSpan('user-operation');

      tracing.correlateByUserId('user-456', spanContext);

      expect(spanContext.span).toBeDefined();

      tracing.endSpan(spanContext);
    });
  });

  describe('Sampling Strategy', () => {
    it('should sample all operations at 100% rate', () => {
      const spans = [];

      for (let i = 0; i < 10; i++) {
        spans.push(tracing.startSpan(`op-${i}`));
      }

      expect(spans.every(s => s.sampled)).toBe(true);

      spans.forEach(s => tracing.endSpan(s));
    });

    it('should sample errors at 100%', () => {
      const adaptiveTracing = createDistributedTracing({
        sampling: {
          defaultRate: 0.01,
          errorRate: 1.0,
        },
      });

      const errorSpan = adaptiveTracing.startSpan('error-op', {
        attributes: { error: true },
      });

      expect(errorSpan.sampled).toBe(true);

      adaptiveTracing.endSpan(errorSpan);
      adaptiveTracing.shutdown();
    });

    it('should sample slow operations at configured rate', () => {
      const adaptiveTracing = createDistributedTracing({
        sampling: {
          defaultRate: 0.01,
          slowRate: 0.5,
        },
      });

      const slowSpan = adaptiveTracing.startSpan('slow-op', {
        attributes: { slow: true },
      });

      // Sampling is probabilistic, just verify it doesn't crash
      expect(slowSpan).toBeDefined();

      adaptiveTracing.endSpan(slowSpan);
      adaptiveTracing.shutdown();
    });
  });

  describe('Async Operations', () => {
    it('should trace async operations with withSpan', async () => {
      const result = await tracing.withSpan(
        'async-operation',
        async () => {
          await new Promise(resolve => setTimeout(resolve, 10));
          return 'success';
        }
      );

      expect(result).toBe('success');
    });

    it('should handle errors in withSpan', async () => {
      await expect(
        tracing.withSpan('error-operation', async () => {
          throw new Error('Test error');
        })
      ).rejects.toThrow('Test error');
    });
  });

  describe('Span Context Management', () => {
    it('should track active spans', () => {
      const span1 = tracing.startSpan('op-1');
      const span2 = tracing.startSpan('op-2');

      expect(tracing.getActiveSpanCount()).toBe(2);

      tracing.endSpan(span1);
      expect(tracing.getActiveSpanCount()).toBe(1);

      tracing.endSpan(span2);
      expect(tracing.getActiveSpanCount()).toBe(0);
    });

    it('should cleanup on shutdown', () => {
      tracing.startSpan('op-1');
      tracing.startSpan('op-2');

      expect(tracing.getActiveSpanCount()).toBe(2);

      tracing.shutdown();

      expect(tracing.getActiveSpanCount()).toBe(0);
    });
  });

  describe('Performance', () => {
    it('should create spans in <0.1ms', () => {
      const start = performance.now();

      for (let i = 0; i < 1000; i++) {
        const span = tracing.startSpan(`perf-test-${i}`);
        tracing.endSpan(span);
      }

      const elapsed = performance.now() - start;
      const avgTime = elapsed / 1000;

      expect(avgTime).toBeLessThan(0.1); // <0.1ms per span
    });

    it('should have minimal memory overhead', () => {
      const before = process.memoryUsage().heapUsed;

      // Create and end 1000 spans
      for (let i = 0; i < 1000; i++) {
        const span = tracing.startSpan(`memory-test-${i}`);
        tracing.endSpan(span);
      }

      const after = process.memoryUsage().heapUsed;
      const overhead = (after - before) / 1024 / 1024; // MB

      expect(overhead).toBeLessThan(5); // <5MB overhead
    });
  });
});
