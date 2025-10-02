/**
 * @fileoverview Mock OTEL Meter Provider (London School TDD)
 * @description Test double for OpenTelemetry metrics - behavior-driven
 */

import { vi } from 'vitest';

/**
 * Creates a mock counter instrument
 * @returns {object} Mock counter with add() behavior
 */
export function createMockCounter() {
  return {
    add: vi.fn(),
    bind: vi.fn(() => ({
      add: vi.fn()
    }))
  };
}

/**
 * Creates a mock histogram instrument
 * @returns {object} Mock histogram with record() behavior
 */
export function createMockHistogram() {
  return {
    record: vi.fn(),
    bind: vi.fn(() => ({
      record: vi.fn()
    }))
  };
}

/**
 * Creates a mock gauge instrument
 * @returns {object} Mock gauge with record() behavior
 */
export function createMockGauge() {
  return {
    record: vi.fn(),
    bind: vi.fn(() => ({
      record: vi.fn()
    }))
  };
}

/**
 * Creates a mock observable gauge
 * @returns {object} Mock observable gauge
 */
export function createMockObservableGauge() {
  return {
    addCallback: vi.fn(),
    removeCallback: vi.fn()
  };
}

/**
 * Creates a mock meter for testing
 * @returns {object} Mock meter with instrument creation methods
 */
export function createMockMeter() {
  return {
    createCounter: vi.fn(() => createMockCounter()),
    createHistogram: vi.fn(() => createMockHistogram()),
    createUpDownCounter: vi.fn(() => createMockCounter()),
    createObservableGauge: vi.fn(() => createMockObservableGauge()),
    createObservableCounter: vi.fn(() => createMockObservableGauge()),
    createObservableUpDownCounter: vi.fn(() => createMockObservableGauge()),
    // Store created instruments for verification
    __instruments: {
      counters: [],
      histograms: [],
      gauges: []
    }
  };
}

/**
 * Creates a mock meter provider
 * @returns {object} Mock meter provider following OTEL API contract
 */
export function createMockMeterProvider() {
  const mockMeter = createMockMeter();

  return {
    getMeter: vi.fn(() => mockMeter),
    shutdown: vi.fn().mockResolvedValue(undefined),
    forceFlush: vi.fn().mockResolvedValue(undefined),
    // Expose mock meter for verification
    __mockMeter: mockMeter
  };
}

/**
 * Factory for creating complete metrics mock ecosystem
 * @returns {object} Complete set of metrics mocks
 */
export function createMetricsMocks() {
  const meterProvider = createMockMeterProvider();
  const meter = meterProvider.__mockMeter;

  return {
    meterProvider,
    meter,
    counter: createMockCounter(),
    histogram: createMockHistogram(),
    gauge: createMockGauge()
  };
}
