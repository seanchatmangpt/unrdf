/**
 * @file Health Check Zod Schemas
 * @module @unrdf/daemon/monitoring/health-check-schemas
 * @description Zod schema definitions for health check configuration and results.
 */

import { z } from 'zod';

/**
 * Health check status enum values
 */
export const HealthStatus = {
  HEALTHY: 'healthy',
  DEGRADED: 'degraded',
  UNHEALTHY: 'unhealthy',
};

/**
 * Health check result schema
 */
export const HealthCheckResultSchema = z.object({
  name: z.string().min(1),
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  message: z.string().optional(),
  duration: z.number().min(0),
  timestamp: z.date(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Dependency check configuration schema
 */
export const DependencyCheckConfigSchema = z.object({
  name: z.string().min(1),
  type: z.enum(['http', 'tcp', 'custom']),
  endpoint: z.string().optional(),
  timeout: z.number().int().min(100).default(5000),
  critical: z.boolean().default(true),
  retries: z.number().int().min(0).default(0),
});

/**
 * Health check configuration schema
 */
export const HealthCheckConfigSchema = z.object({
  livenessInterval: z.number().int().min(1000).default(10000),
  readinessInterval: z.number().int().min(1000).default(5000),
  historyTTL: z.number().int().min(60000).default(3600000),
  historyMaxSize: z.number().int().min(10).default(1000),
  dependencyTimeout: z.number().int().min(100).default(5000),
  unhealthyThreshold: z.number().int().min(1).default(3),
  healthyThreshold: z.number().int().min(1).default(2),
});

/**
 * Overall health report schema
 */
export const HealthReportSchema = z.object({
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  timestamp: z.date(),
  uptime: z.number().int().min(0),
  liveness: z.object({
    alive: z.boolean(),
    lastCheck: z.date().optional(),
  }),
  readiness: z.object({
    ready: z.boolean(),
    lastCheck: z.date().optional(),
    reason: z.string().optional(),
  }),
  checks: z.array(HealthCheckResultSchema),
  dependencies: z.array(HealthCheckResultSchema),
});
