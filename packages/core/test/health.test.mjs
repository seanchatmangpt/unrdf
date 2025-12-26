/**
 * @file Health Check Tests
 * @description Tests for health check system
 */

import { describe, it, expect } from 'vitest';
import { createHealthChecks, createUnrdfHealthChecks, HealthStatus } from '../src/health.mjs';

describe('Health Check System', () => {
  describe('createHealthChecks', () => {
    it('should create health check system', () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0'
      });

      expect(health).toHaveProperty('liveness');
      expect(health).toHaveProperty('readiness');
      expect(health).toHaveProperty('metrics');
      expect(health).toHaveProperty('prometheus');
    });

    it('should perform liveness check', async () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0'
      });

      const result = await health.liveness();

      expect(result).toMatchObject({
        status: HealthStatus.HEALTHY,
        service: 'test-service',
        version: '1.0.0'
      });
      expect(result).toHaveProperty('timestamp');
      expect(result).toHaveProperty('uptime');
      expect(result).toHaveProperty('pid');
    });

    it('should perform readiness check with no dependencies', async () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0'
      });

      const result = await health.readiness();

      expect(result).toMatchObject({
        status: HealthStatus.HEALTHY,
        service: 'test-service',
        version: '1.0.0',
        environment: 'production'
      });
      expect(result).toHaveProperty('dependencies');
      expect(result).toHaveProperty('checkDuration');
    });

    it('should check dependencies in readiness', async () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0',
        dependencies: {
          database: async () => true,
          cache: async () => true
        }
      });

      const result = await health.readiness();

      expect(result.status).toBe(HealthStatus.HEALTHY);
      expect(result.dependencies).toHaveProperty('database');
      expect(result.dependencies).toHaveProperty('cache');
      expect(result.dependencies.database.status).toBe('connected');
      expect(result.dependencies.cache.status).toBe('connected');
    });

    it('should detect unhealthy dependencies', async () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0',
        dependencies: {
          database: async () => false,
          cache: async () => true
        }
      });

      const result = await health.readiness();

      expect(result.status).toBe(HealthStatus.UNHEALTHY);
      expect(result.dependencies.database.status).toBe('disconnected');
    });

    it('should handle dependency errors', async () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0',
        dependencies: {
          database: async () => {
            throw new Error('Connection refused');
          }
        }
      });

      const result = await health.readiness();

      expect(result.status).toBe(HealthStatus.UNHEALTHY);
      expect(result.dependencies.database.status).toBe('disconnected');
      expect(result.dependencies.database.error).toBe('Connection refused');
    });

    it('should return metrics', async () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0'
      });

      const result = await health.metrics();

      expect(result).toHaveProperty('timestamp');
      expect(result).toHaveProperty('service', 'test-service');
      expect(result).toHaveProperty('version', '1.0.0');
      expect(result).toHaveProperty('uptime');
      expect(result).toHaveProperty('requests');
      expect(result).toHaveProperty('process');
      expect(result).toHaveProperty('memory');
      expect(result).toHaveProperty('cpu');
    });

    it('should return Prometheus format metrics', async () => {
      const health = createHealthChecks({
        serviceName: 'test-service',
        version: '1.0.0'
      });

      const result = await health.prometheus();

      expect(result).toContain('# HELP');
      expect(result).toContain('# TYPE');
      expect(result).toContain('unrdf_service_info');
      expect(result).toContain('unrdf_uptime_seconds');
      expect(result).toContain('unrdf_memory_rss_bytes');
    });
  });

  describe('createUnrdfHealthChecks', () => {
    it('should create UNRDF health checks with defaults', async () => {
      const health = createUnrdfHealthChecks();
      const result = await health.liveness();

      expect(result).toMatchObject({
        status: HealthStatus.HEALTHY,
        service: 'unrdf',
        version: '5.0.1'
      });
    });

    it('should accept custom options', async () => {
      const health = createUnrdfHealthChecks({
        serviceName: 'custom-service'
      });
      const result = await health.liveness();

      expect(result.service).toBe('custom-service');
    });
  });
});
