/**
 * @vitest-environment node
 * @file Telemetry Tests - Comprehensive coverage for telemetry.mjs
 */
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { BatchedTelemetry } from '../src/hooks/telemetry.mjs';

describe('BatchedTelemetry - Construction', () => {
  it('should create telemetry with default config', () => {
    const mockTracer = {
      startSpan: vi.fn(),
    };

    const telemetry = new BatchedTelemetry(mockTracer);

    expect(telemetry.tracer).toBe(mockTracer);
    expect(telemetry.flushInterval).toBe(10);
  });

  it('should create telemetry with custom config', () => {
    const mockTracer = { startSpan: vi.fn() };
    const telemetry = new BatchedTelemetry(mockTracer, {
      enabled: false,
      flushInterval: 50,
    });

    expect(telemetry.enabled).toBe(false);
    expect(telemetry.flushInterval).toBe(50);
  });

  it('should disable in production by default', () => {
    const originalEnv = process.env.NODE_ENV;
    process.env.NODE_ENV = 'production';

    const telemetry = new BatchedTelemetry({});
    expect(telemetry.enabled).toBe(false);

    process.env.NODE_ENV = originalEnv;
  });

  it('should enable in development by default', () => {
    const originalEnv = process.env.NODE_ENV;
    process.env.NODE_ENV = 'development';

    const telemetry = new BatchedTelemetry({});
    expect(telemetry.enabled).toBe(true);

    process.env.NODE_ENV = originalEnv;
  });

  it('should allow explicit enable in production', () => {
    const originalEnv = process.env.NODE_ENV;
    process.env.NODE_ENV = 'production';

    const telemetry = new BatchedTelemetry({}, { enabled: true });
    expect(telemetry.enabled).toBe(true);

    process.env.NODE_ENV = originalEnv;
  });
});

describe('BatchedTelemetry - Transaction Spans', () => {
  let mockTracer;
  let mockSpan;
  let telemetry;

  beforeEach(() => {
    mockSpan = {
      setAttributes: vi.fn(),
      setAttribute: vi.fn(),
      end: vi.fn(),
      recordException: vi.fn(),
    };

    mockTracer = {
      startSpan: vi.fn(() => mockSpan),
    };

    telemetry = new BatchedTelemetry(mockTracer, { enabled: true });
  });

  it('should start transaction span', () => {
    const span = telemetry.startTransactionSpan('test-transaction', {
      foo: 'bar',
    });

    expect(mockTracer.startSpan).toHaveBeenCalledWith('test-transaction', {
      attributes: { foo: 'bar' },
    });
    expect(span).toBe(mockSpan);
  });

  it('should return null when disabled', () => {
    telemetry.enabled = false;

    const span = telemetry.startTransactionSpan('test');
    expect(span).toBeNull();
    expect(mockTracer.startSpan).not.toHaveBeenCalled();
  });

  it('should handle missing tracer gracefully', () => {
    telemetry.tracer = null;

    const span = telemetry.startTransactionSpan('test');
    expect(span).toBeNull();
  });

  it('should include default attributes', () => {
    telemetry.startTransactionSpan('test', {
      hookCount: 5,
      trigger: 'before-add',
    });

    expect(mockTracer.startSpan).toHaveBeenCalledWith('test', {
      attributes: expect.objectContaining({
        hookCount: 5,
        trigger: 'before-add',
      }),
    });
  });
});

describe('BatchedTelemetry - Batch Attributes', () => {
  let mockSpan;
  let telemetry;

  beforeEach(() => {
    mockSpan = {
      setAttributes: vi.fn(),
      setAttribute: vi.fn(),
      setStatus: vi.fn(),
      end: vi.fn(),
    };

    const mockTracer = {
      startSpan: vi.fn(() => mockSpan),
    };

    telemetry = new BatchedTelemetry(mockTracer, {
      enabled: true,
      flushInterval: 100,
    });
  });

  it('should batch pending attributes', () => {
    const span = telemetry.startTransactionSpan('test');

    telemetry.setAttribute(span, 'attr1', 'value1');
    telemetry.setAttribute(span, 'attr2', 'value2');

    expect(telemetry.pendingAttributes).toHaveLength(2);
  });

  it('should flush pending attributes', () => {
    const span = telemetry.startTransactionSpan('test');

    telemetry.setAttribute(span, 'attr1', 'value1');
    telemetry.flush();

    expect(mockSpan.setAttribute).toHaveBeenCalledWith('attr1', 'value1');
    expect(telemetry.pendingAttributes).toHaveLength(0);
  });

  it('should auto-flush after interval', async () => {
    vi.useFakeTimers();

    const span = telemetry.startTransactionSpan('test');
    telemetry.setAttribute(span, 'delayed', 'value');

    // Fast-forward time
    vi.advanceTimersByTime(150);

    expect(mockSpan.setAttribute).toHaveBeenCalled();

    vi.useRealTimers();
  });

  it('should handle flushing empty batch', () => {
    expect(() => telemetry.flush()).not.toThrow();
  });

  it('should skip batching when disabled', () => {
    telemetry.enabled = false;

    const span = telemetry.startTransactionSpan('test');
    telemetry.setAttribute(span, 'attr', 'value');

    expect(telemetry.pendingAttributes).toHaveLength(0);
  });
});

describe('BatchedTelemetry - Span Lifecycle', () => {
  let mockSpan;
  let telemetry;

  beforeEach(() => {
    mockSpan = {
      setAttributes: vi.fn(),
      setAttribute: vi.fn(),
      setStatus: vi.fn(),
      end: vi.fn(),
      recordEvent: vi.fn(),
    };

    const mockTracer = {
      startSpan: vi.fn(() => mockSpan),
    };

    telemetry = new BatchedTelemetry(mockTracer, { enabled: true });
  });

  it('should end span', () => {
    const span = telemetry.startTransactionSpan('test');
    telemetry.endSpan(span);

    expect(mockSpan.setStatus).toHaveBeenCalled();
    expect(mockSpan.end).toHaveBeenCalled();
  });

  it('should end span with status', () => {
    const span = telemetry.startTransactionSpan('test');
    telemetry.endSpan(span, 'error', 'Test error');

    expect(mockSpan.setStatus).toHaveBeenCalledWith({
      code: 'error',
      message: 'Test error',
    });
  });

  it('should record events', () => {
    const span = telemetry.startTransactionSpan('test');

    telemetry.recordEvent(span, 'test-event', { foo: 'bar' });

    expect(mockSpan.recordEvent).toHaveBeenCalledWith('test-event', { foo: 'bar' });
  });

  it('should handle ending null span', () => {
    expect(() => telemetry.endSpan(null)).not.toThrow();
  });

  it('should handle recording event on null span', () => {
    expect(() => telemetry.recordEvent(null, 'event', {})).not.toThrow();
  });
});

describe('BatchedTelemetry - Enable/Disable', () => {
  let telemetry;

  beforeEach(() => {
    const mockTracer = {
      startSpan: vi.fn(() => ({
        setAttributes: vi.fn(),
        setAttribute: vi.fn(),
        setStatus: vi.fn(),
        end: vi.fn(),
      })),
    };

    telemetry = new BatchedTelemetry(mockTracer, { enabled: true });
  });

  it('should disable telemetry', () => {
    telemetry.disable();
    expect(telemetry.enabled).toBe(false);
  });

  it('should enable telemetry', () => {
    telemetry.disable();
    telemetry.enable();
    expect(telemetry.enabled).toBe(true);
  });

  it('should flush on disable', () => {
    const span = telemetry.startTransactionSpan('test');
    telemetry.setAttribute(span, 'test', 'value');

    telemetry.disable();

    // After disable, pending attributes should be flushed
    expect(telemetry.pendingAttributes).toHaveLength(0);
  });
});

describe('BatchedTelemetry - Edge Cases', () => {
  it('should handle rapid span creation', () => {
    const mockTracer = {
      startSpan: vi.fn(() => ({
        setAttributes: vi.fn(),
        end: vi.fn(),
      })),
    };

    const telemetry = new BatchedTelemetry(mockTracer, { enabled: true });

    // Create 1000 spans rapidly
    for (let i = 0; i < 1000; i++) {
      telemetry.startTransactionSpan(`span-${i}`);
    }

    expect(mockTracer.startSpan).toHaveBeenCalledTimes(1000);
  });

  it('should handle very large attribute batches', () => {
    const mockSpan = {
      setAttribute: vi.fn(),
      setAttributes: vi.fn(),
      end: vi.fn(),
    };

    const mockTracer = {
      startSpan: vi.fn(() => mockSpan),
    };

    const telemetry = new BatchedTelemetry(mockTracer, { enabled: true });
    const span = telemetry.startTransactionSpan('test');

    // Add 100 pending attributes
    for (let i = 0; i < 100; i++) {
      telemetry.addPendingAttribute(span, `attr${i}`, `value${i}`);
    }

    telemetry.flushPendingAttributes();
    expect(mockSpan.setAttribute).toHaveBeenCalledTimes(100);
  });

  it('should handle cleanup on teardown', () => {
    const mockSpan = {
      setAttribute: vi.fn(),
      end: vi.fn(),
    };

    const mockTracer = {
      startSpan: vi.fn(() => mockSpan),
    };

    const telemetry = new BatchedTelemetry(mockTracer, { enabled: true });
    const span = telemetry.startTransactionSpan('test');

    telemetry.addPendingAttribute(span, 'attr', 'value');
    telemetry.cleanup();

    expect(telemetry.pendingAttributes).toHaveLength(0);
  });
});

describe('BatchedTelemetry - Concurrency', () => {
  it('should handle concurrent span operations', async () => {
    const mockSpan = {
      setAttribute: vi.fn(),
      setAttributes: vi.fn(),
      end: vi.fn(),
    };

    const mockTracer = {
      startSpan: vi.fn(() => mockSpan),
    };

    const telemetry = new BatchedTelemetry(mockTracer, { enabled: true });

    const operations = Array(100)
      .fill(null)
      .map((_, i) =>
        Promise.resolve().then(() => {
          const span = telemetry.startTransactionSpan(`concurrent-${i}`);
          telemetry.addPendingAttribute(span, 'index', i);
          return span;
        })
      );

    await Promise.all(operations);
    expect(mockTracer.startSpan).toHaveBeenCalledTimes(100);
  });
});
