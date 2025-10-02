/**
 * @fileoverview Mock OTEL Tracer Provider (London School TDD)
 * @description Test double for OpenTelemetry tracer - focuses on behavior verification
 */

import { vi } from 'vitest';

/**
 * Creates a mock span for testing tracer behavior
 * @returns {object} Mock span with behavior verification methods
 */
export function createMockSpan() {
  return {
    spanContext: vi.fn(() => ({
      traceId: 'mock-trace-id-123',
      spanId: 'mock-span-id-456',
      traceFlags: 1
    })),
    setAttribute: vi.fn().mockReturnThis(),
    setAttributes: vi.fn().mockReturnThis(),
    addEvent: vi.fn().mockReturnThis(),
    setStatus: vi.fn().mockReturnThis(),
    updateName: vi.fn().mockReturnThis(),
    end: vi.fn(),
    isRecording: vi.fn(() => true),
    recordException: vi.fn()
  };
}

/**
 * Creates a mock tracer for testing
 * @returns {object} Mock tracer with startSpan behavior
 */
export function createMockTracer() {
  const mockSpan = createMockSpan();

  return {
    startSpan: vi.fn(() => mockSpan),
    startActiveSpan: vi.fn((name, options, fn) => {
      if (typeof fn === 'function') {
        return fn(mockSpan);
      }
      return mockSpan;
    }),
    // Expose the mock span for verification
    __mockSpan: mockSpan
  };
}

/**
 * Creates a mock tracer provider for testing
 * @returns {object} Mock tracer provider following OTEL API contract
 */
export function createMockTracerProvider() {
  const mockTracer = createMockTracer();

  return {
    getTracer: vi.fn(() => mockTracer),
    register: vi.fn(),
    shutdown: vi.fn().mockResolvedValue(undefined),
    forceFlush: vi.fn().mockResolvedValue(undefined),
    // Expose mock tracer for verification
    __mockTracer: mockTracer
  };
}

/**
 * Creates a mock context manager for OTEL
 * @returns {object} Mock context manager
 */
export function createMockContextManager() {
  return {
    active: vi.fn(() => ({})),
    with: vi.fn((context, fn) => fn()),
    bind: vi.fn((context, target) => target),
    enable: vi.fn().mockReturnThis(),
    disable: vi.fn().mockReturnThis()
  };
}

/**
 * Factory for creating complete OTEL mock ecosystem
 * @returns {object} Complete set of OTEL mocks
 */
export function createOTelMocks() {
  const tracerProvider = createMockTracerProvider();
  const contextManager = createMockContextManager();

  return {
    tracerProvider,
    contextManager,
    tracer: tracerProvider.__mockTracer,
    span: tracerProvider.__mockTracer.__mockSpan
  };
}
