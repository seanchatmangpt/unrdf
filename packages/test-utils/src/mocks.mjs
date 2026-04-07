/**
 * @file Mock factories for common test doubles.
 *
 * Covers the mock span/tracer/fetch patterns found in 55+ test files.
 * Requires vitest to be available in the test context.
 *
 * @module @unrdf/test-utils/mocks
 */

import { vi } from 'vitest';

// ============================================================================
// OTel span / tracer mocks
// ============================================================================

/**
 * Create a mock OpenTelemetry span.
 *
 * @returns {object} Mock span with vitest spy functions
 *
 * @example
 * const span = createMockSpan();
 * // span.setAttribute, span.setAttributes, span.end, span.recordException
 */
export function createMockSpan() {
  return {
    setAttribute: vi.fn().mockReturnThis(),
    setAttributes: vi.fn().mockReturnThis(),
    setStatus: vi.fn().mockReturnThis(),
    addEvent: vi.fn().mockReturnThis(),
    recordException: vi.fn().mockReturnThis(),
    end: vi.fn(),
    spanContext: vi.fn(() => ({ traceId: 'test-trace-id', spanId: 'test-span-id' })),
    isRecording: vi.fn(() => true),
  };
}

/**
 * Create a mock OpenTelemetry tracer.
 *
 * @param {Function} [spanFactory] - Optional factory for the spans it returns (default: createMockSpan)
 * @returns {object} Mock tracer
 *
 * @example
 * const tracer = createMockTracer();
 * tracer.startSpan('my-span'); // returns a mock span
 * expect(tracer.startSpan).toHaveBeenCalledWith('my-span');
 */
export function createMockTracer(spanFactory = createMockSpan) {
  const spans = [];
  const tracer = {
    startSpan: vi.fn(() => {
      const span = spanFactory();
      spans.push(span);
      return span;
    }),
    startActiveSpan: vi.fn((name, fn) => {
      const span = spanFactory();
      spans.push(span);
      return fn(span);
    }),
    _spans: spans,
  };
  return tracer;
}

/**
 * Create a mock OpenTelemetry meter (for metrics).
 * @returns {object} Mock meter with counter, histogram, gauge factories
 */
export function createMockMeter() {
  const makeInstrument = () => ({
    add: vi.fn(),
    record: vi.fn(),
    observe: vi.fn(),
  });
  return {
    createCounter: vi.fn(makeInstrument),
    createHistogram: vi.fn(makeInstrument),
    createObservableGauge: vi.fn(makeInstrument),
  };
}

// ============================================================================
// Fetch mock
// ============================================================================

/**
 * Create a mock `fetch` function that returns configured responses by URL pattern.
 *
 * @param {Record<string, {ok?: boolean, status?: number, data?: any, text?: string}>} responses
 *   Map of URL substrings to response configs
 * @returns {Function} A vi.fn() that resolves to matching mock responses
 *
 * @example
 * global.fetch = createMockFetch({
 *   '/api/query': { ok: true, data: { results: [] } },
 *   '/api/health': { ok: true, data: { status: 'ok' } },
 * });
 */
export function createMockFetch(responses = {}) {
  return vi.fn(async (url) => {
    const urlStr = url.toString();
    const matchKey = Object.keys(responses).find(key => urlStr.includes(key));
    const config = matchKey ? responses[matchKey] : { ok: true, data: null };

    const body = config.data !== undefined ? config.data : config.body ?? null;
    const text = config.text ?? (body !== null ? JSON.stringify(body) : '');

    return {
      ok: config.ok !== false,
      status: config.status ?? (config.ok !== false ? 200 : 500),
      statusText: config.ok !== false ? 'OK' : 'Internal Server Error',
      json: vi.fn(async () => body),
      text: vi.fn(async () => text),
      headers: new Map(Object.entries(config.headers ?? {})),
    };
  });
}

// ============================================================================
// Cleanup manager
// ============================================================================

/**
 * Create a cleanup manager for test teardown.
 * Collects async cleanup functions and runs them in reverse order in afterEach.
 *
 * @returns {{ onCleanup(fn: Function): void, cleanup(): Promise<void> }}
 *
 * @example
 * let cleanup;
 * beforeEach(() => { cleanup = createTestCleanup(); });
 * afterEach(() => cleanup.run());
 *
 * it('test', () => {
 *   const server = startServer();
 *   cleanup.onCleanup(() => server.close());
 * });
 */
export function createTestCleanup() {
  const fns = [];
  return {
    onCleanup(fn) {
      fns.push(fn);
    },
    async run() {
      for (const fn of fns.reverse()) {
        try { await fn(); } catch (e) { /* don't block other cleanups */ }
      }
      fns.length = 0;
      vi.restoreAllMocks();
    },
  };
}

// ============================================================================
// Event emitter mock
// ============================================================================

/**
 * Create a simple mock event emitter for testing change feeds, streams, etc.
 *
 * @returns {object} Mock emitter with on/off/emit and call tracking
 *
 * @example
 * const feed = createMockEmitter();
 * feed.on('change', handler);
 * feed.emit('change', { type: 'add', quad: testQuad });
 * expect(handler).toHaveBeenCalledTimes(1);
 */
export function createMockEmitter() {
  const listeners = new Map();
  return {
    on: vi.fn((event, handler) => {
      if (!listeners.has(event)) listeners.set(event, []);
      listeners.get(event).push(handler);
    }),
    off: vi.fn((event, handler) => {
      if (listeners.has(event)) {
        listeners.set(event, listeners.get(event).filter(h => h !== handler));
      }
    }),
    emit(event, ...args) {
      for (const handler of listeners.get(event) ?? []) {
        handler(...args);
      }
    },
    listenerCount: (event) => (listeners.get(event) ?? []).length,
    removeAllListeners: vi.fn(() => listeners.clear()),
  };
}
