/**
 * @file Unit Tests
 * @description Unit tests for graph analytics components
 */

import { describe, it, expect } from 'vitest';
import { AnalyticsEngine } from '../src/analytics-engine.mjs';
import { PatternDetector } from '../src/pattern-detector.mjs';

describe('AnalyticsEngine', () => {
  it('should create analytics engine', () => {
    const analytics = new AnalyticsEngine();
    expect(analytics).toBeDefined();
    expect(analytics.metrics).toBeDefined();
  });

  it('should get metrics', () => {
    const analytics = new AnalyticsEngine();
    const metrics = analytics.getMetrics();

    expect(metrics.triples).toBeDefined();
    expect(metrics.entities).toBeDefined();
    expect(metrics.predicates).toBeDefined();
  });

  it('should subscribe to events', () => {
    const analytics = new AnalyticsEngine();
    let eventReceived = false;

    analytics.subscribe((event) => {
      eventReceived = true;
    });

    analytics.subscribe(() => {});
    expect(analytics.listeners.size).toBe(2);
  });
});

describe('PatternDetector', () => {
  it('should create pattern detector', () => {
    const detector = new PatternDetector();
    expect(detector).toBeDefined();
    expect(detector.patternDefinitions.length).toBeGreaterThan(0);
  });

  it('should have pattern definitions', () => {
    const detector = new PatternDetector();
    const starPattern = detector.patternDefinitions.find((p) => p.name === 'star');

    expect(starPattern).toBeDefined();
    expect(starPattern.sparql).toContain('SELECT');
  });
});
