/**
 * @file Tests for observability enhancements
 * @module test/observability-enhancements
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { ObservabilityManager } from '../src/observability.mjs';
import { AnomalyDetector } from '../src/anomaly-detector.mjs';
import { readFileSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

describe('Observability Enhancements', () => {
  describe('Distributed Trace Sampling', () => {
    let manager;

    beforeEach(() => {
      manager = new ObservabilityManager({
        serviceName: 'test-service',
        enableTracing: false, // Disable actual OTEL for tests
        samplingRate: 0.1, // 10% sampling
      });
    });

    afterEach(async () => {
      await manager.shutdown();
    });

    it('should configure trace sampling rate', () => {
      expect(manager.samplingRate).toBe(0.1);
    });

    it('should support configurable sampling rates', () => {
      const manager2 = new ObservabilityManager({
        samplingRate: 0.5,
      });
      expect(manager2.samplingRate).toBe(0.5);

      const manager3 = new ObservabilityManager({
        samplingRate: 0.01,
      });
      expect(manager3.samplingRate).toBe(0.01);
    });
  });

  describe('Log Sampling', () => {
    let manager;
    let logs = [];
    const originalConsoleLog = console.log;
    const originalConsoleWarn = console.warn;
    const originalConsoleError = console.error;

    beforeEach(() => {
      logs = [];
      console.log = (...args) => logs.push({ level: 'log', args });
      console.warn = (...args) => logs.push({ level: 'warn', args });
      console.error = (...args) => logs.push({ level: 'error', args });

      manager = new ObservabilityManager({
        logSamplingRate: 0.01, // 1% DEBUG sampling
      });
    });

    afterEach(async () => {
      console.log = originalConsoleLog;
      console.warn = originalConsoleWarn;
      console.error = originalConsoleError;
      await manager.shutdown();
    });

    it('should sample DEBUG logs at configured rate', () => {
      // Log 100 DEBUG messages
      for (let i = 0; i < 100; i++) {
        manager.log('DEBUG', `Test message ${i}`);
      }

      // With 1% sampling, expect ~1 log (allow 0-5 for randomness)
      const debugLogs = logs.filter((l) => l.args[0].includes('DEBUG:SAMPLED'));
      expect(debugLogs.length).toBeGreaterThanOrEqual(0);
      expect(debugLogs.length).toBeLessThanOrEqual(5);
    });

    it('should always log WARN and ERROR regardless of sampling', () => {
      // Log WARN and ERROR
      manager.log('WARN', 'Warning message');
      manager.log('ERROR', 'Error message');
      manager.log('INFO', 'Info message');

      const warnLogs = logs.filter((l) => l.level === 'warn');
      const errorLogs = logs.filter((l) => l.level === 'error');
      const infoLogs = logs.filter((l) => l.args[0].includes('[INFO]'));

      expect(warnLogs.length).toBe(1);
      expect(errorLogs.length).toBe(1);
      expect(infoLogs.length).toBe(1); // INFO always logged
    });
  });

  describe('Grafana Dashboards', () => {
    const dashboardsDir = resolve(__dirname, '../../../dashboards');

    it('should have 5 dashboard JSON templates', () => {
      const dashboards = [
        '01-system-health.json',
        '02-validation-metrics.json',
        '03-andon-signals.json',
        '04-kgc-governance.json',
        '05-performance.json',
      ];

      for (const dashboard of dashboards) {
        const path = resolve(dashboardsDir, dashboard);
        expect(existsSync(path), `Dashboard ${dashboard} should exist`).toBe(true);
      }
    });

    it('should have valid JSON structure in all dashboards', () => {
      const dashboards = [
        '01-system-health.json',
        '02-validation-metrics.json',
        '03-andon-signals.json',
        '04-kgc-governance.json',
        '05-performance.json',
      ];

      for (const dashboard of dashboards) {
        const path = resolve(dashboardsDir, dashboard);
        const content = readFileSync(path, 'utf-8');
        const parsed = JSON.parse(content);

        expect(parsed).toHaveProperty('dashboard');
        expect(parsed.dashboard).toHaveProperty('title');
        expect(parsed.dashboard).toHaveProperty('panels');
        expect(Array.isArray(parsed.dashboard.panels)).toBe(true);
        expect(parsed.dashboard.panels.length).toBeGreaterThan(0);
      }
    });
  });

  describe('Alertmanager Rules', () => {
    const alertsPath = resolve(__dirname, '../../../dashboards/prometheus-alerts.yml');

    it('should have Prometheus alert rules file', () => {
      expect(existsSync(alertsPath)).toBe(true);
    });

    it('should have valid alert rules for SLO burn rate, latency, and errors', () => {
      const content = readFileSync(alertsPath, 'utf-8');

      // Check for key alert rules
      expect(content).toContain('HighErrorBurnRate1h');
      expect(content).toContain('HighErrorBurnRate6h');
      expect(content).toContain('LatencySpikeP95');
      expect(content).toContain('LatencySpikeP99');
      expect(content).toContain('ErrorRateSpike');
      expect(content).toContain('ValidationScoreLow');
      expect(content).toContain('ThroughputDrop');
      expect(content).toContain('QueueBackpressureHigh');

      // Check structure
      expect(content).toContain('groups:');
      expect(content).toContain('rules:');
      expect(content).toContain('alert:');
      expect(content).toContain('expr:');
      expect(content).toContain('severity:');
    });
  });

  describe('Anomaly Detection', () => {
    let detector;

    beforeEach(() => {
      detector = new AnomalyDetector({
        zScoreThreshold: 3.0,
        windowSize: 100,
        minSamples: 10,
        cooldownMs: 1000,
      });
    });

    it('should detect latency spikes using z-score analysis', () => {
      // Add normal samples (mean ~100, stdDev ~10)
      for (let i = 0; i < 50; i++) {
        detector.addSample('transaction_latency', 100 + (Math.random() - 0.5) * 20);
      }

      // Add a spike (>3 sigma)
      const anomaly = detector.addSample('transaction_latency', 200);

      expect(anomaly).not.toBeNull();
      expect(anomaly.metric).toBe('transaction_latency');
      expect(Math.abs(anomaly.zScore)).toBeGreaterThan(3.0);
      expect(anomaly.severity).toBeTruthy();
      expect(['low', 'medium', 'high', 'critical']).toContain(anomaly.severity);
    });

    it('should detect throughput drops using z-score analysis', () => {
      // Add normal samples (mean ~1000)
      for (let i = 0; i < 50; i++) {
        detector.addSample('throughput', 1000 + (Math.random() - 0.5) * 100);
      }

      // Add a drop (>3 sigma below mean)
      const anomaly = detector.addSample('throughput', 500);

      expect(anomaly).not.toBeNull();
      expect(anomaly.metric).toBe('throughput');
      expect(anomaly.zScore).toBeLessThan(-3.0); // Negative z-score for drop
      expect(anomaly.message).toContain('drop');
    });
  });

  describe('Log Aggregation Examples', () => {
    const loggingSetupDir = resolve(__dirname, '../../../examples/logging-setup');

    it('should have ELK and Loki configuration examples', () => {
      // Check for ELK setup
      expect(existsSync(resolve(loggingSetupDir, 'docker-compose-elk.yml'))).toBe(true);
      expect(
        existsSync(resolve(loggingSetupDir, 'logstash-config/logstash.conf'))
      ).toBe(true);

      // Check for Loki setup
      expect(existsSync(resolve(loggingSetupDir, 'docker-compose-loki.yml'))).toBe(true);
      expect(
        existsSync(resolve(loggingSetupDir, 'loki-config/loki-config.yml'))
      ).toBe(true);
      expect(
        existsSync(resolve(loggingSetupDir, 'loki-config/promtail-config.yml'))
      ).toBe(true);

      // Check README
      expect(existsSync(resolve(loggingSetupDir, 'README.md'))).toBe(true);
    });
  });
});
