/**
 * @file Uptime Command Schemas
 * @module cli/commands/daemon/uptime/schemas
 * @description Zod validation schemas for uptime simulation commands
 */

import { z } from 'zod';

/**
 * Valid chaos levels for simulation
 */
export const ChaosLevel = z.enum(['none', 'low', 'medium', 'high', 'extreme']);

/**
 * Output format types
 */
export const OutputFormat = z.enum(['json', 'text', 'csv']);

/**
 * Report period types
 */
export const ReportPeriod = z.enum(['hour', 'day', 'week', 'month', 'custom']);

/**
 * Schema for simulate command arguments
 * @example
 * { duration: 60000, chaosLevel: 'medium', failureRate: 0.05, output: 'report.json' }
 */
export const SimulateArgsSchema = z.object({
  duration: z
    .number()
    .min(1000, 'Duration must be at least 1000ms')
    .max(3600000, 'Duration cannot exceed 1 hour (3600000ms)')
    .optional()
    .default(60000),
  'chaos-level': ChaosLevel.optional().default('none'),
  'failure-rate': z
    .number()
    .min(0, 'Failure rate must be >= 0')
    .max(1, 'Failure rate must be <= 1')
    .optional()
    .default(0.01),
  output: z.string().optional(),
  json: z.boolean().optional().default(false),
  seed: z.number().optional(),
  verbose: z.boolean().optional().default(false),
});

/**
 * Schema for report command arguments
 * @example
 * { period: 'day', format: 'json', slaTarget: 99.9 }
 */
export const ReportArgsSchema = z.object({
  period: ReportPeriod.optional().default('day'),
  format: OutputFormat.optional().default('text'),
  'sla-target': z
    .number()
    .min(0, 'SLA target must be >= 0')
    .max(100, 'SLA target must be <= 100')
    .optional()
    .default(99.9),
  output: z.string().optional(),
  'start-time': z.string().optional(),
  'end-time': z.string().optional(),
  json: z.boolean().optional().default(false),
});

/**
 * Schema for benchmark command arguments
 * @example
 * { iterations: 100, concurrency: 10, output: 'benchmark.json' }
 */
export const BenchmarkArgsSchema = z.object({
  iterations: z
    .number()
    .min(1, 'Iterations must be at least 1')
    .max(10000, 'Iterations cannot exceed 10000')
    .optional()
    .default(100),
  concurrency: z
    .number()
    .min(1, 'Concurrency must be at least 1')
    .max(100, 'Concurrency cannot exceed 100')
    .optional()
    .default(10),
  output: z.string().optional(),
  json: z.boolean().optional().default(false),
  warmup: z.number().min(0).optional().default(5),
  'include-percentiles': z.boolean().optional().default(true),
});

/**
 * Simulation result schema for validation
 */
export const SimulationResultSchema = z.object({
  simulationId: z.string(),
  startTime: z.string(),
  endTime: z.string(),
  duration: z.number(),
  chaosLevel: ChaosLevel,
  failureRate: z.number(),
  events: z.array(
    z.object({
      timestamp: z.string(),
      type: z.enum(['start', 'heartbeat', 'failure', 'recovery', 'chaos', 'end']),
      details: z.record(z.unknown()).optional(),
    })
  ),
  metrics: z.object({
    totalHeartbeats: z.number(),
    successfulHeartbeats: z.number(),
    failedHeartbeats: z.number(),
    uptime: z.number(),
    mtbf: z.number(),
    mttr: z.number(),
  }),
});

/**
 * Report result schema
 */
export const ReportResultSchema = z.object({
  reportId: z.string(),
  period: ReportPeriod,
  generatedAt: z.string(),
  slaTarget: z.number(),
  slaMet: z.boolean(),
  metrics: z.object({
    totalUptime: z.number(),
    totalDowntime: z.number(),
    uptimePercentage: z.number(),
    incidentCount: z.number(),
    mtbf: z.number(),
    mttr: z.number(),
  }),
});

/**
 * Benchmark result schema
 */
export const BenchmarkResultSchema = z.object({
  benchmarkId: z.string(),
  iterations: z.number(),
  concurrency: z.number(),
  completedAt: z.string(),
  metrics: z.object({
    totalDuration: z.number(),
    avgLatency: z.number(),
    minLatency: z.number(),
    maxLatency: z.number(),
    p50: z.number(),
    p90: z.number(),
    p95: z.number(),
    p99: z.number(),
    throughput: z.number(),
    errorRate: z.number(),
  }),
});
