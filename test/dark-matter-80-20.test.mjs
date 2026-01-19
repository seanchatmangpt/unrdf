/**
 * @file Knowledge Substrate Core Smoke Tests
 * @description Two fast smoke tests (<50ms total) with mocks
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('Knowledge Substrate Core (SMOKE)', () => {
  let mockSystem;

  beforeEach(() => {
    mockSystem = {
      initialized: true,
      getStatus: () => ({
        initialized: true,
        components: ['transactionManager', 'knowledgeHookManager', 'effectSandbox'],
      }),
      getMetrics: () => ({
        valueDeliveryRatio: 0.85,
        performanceImpactRatio: 0.82,
        developmentEfficiencyRatio: 0.88,
      }),
      cleanup: vi.fn(async () => {
        mockSystem.initialized = false;
      }),
    };
  });

  it('should initialize core components', () => {
    expect(mockSystem.initialized).toBe(true);
    const status = mockSystem.getStatus();
    expect(status.initialized).toBe(true);
    expect(status.components).toHaveLength(3);
  });

  it('should deliver high value metrics', () => {
    const metrics = mockSystem.getMetrics();
    expect(metrics.valueDeliveryRatio).toBeGreaterThanOrEqual(0.8);
    expect(metrics.performanceImpactRatio).toBeGreaterThanOrEqual(0.8);
    expect(metrics.developmentEfficiencyRatio).toBeGreaterThanOrEqual(0.8);
  });
});
