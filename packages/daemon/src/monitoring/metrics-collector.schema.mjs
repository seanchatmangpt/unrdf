/**
 * @file Metrics Collector Zod Schemas
 * @module @unrdf/daemon/monitoring/metrics-collector-schemas
 * @description Zod schema definitions for metrics collection configuration.
 */

import { z } from 'zod';

/**
 * Metric type enum values
 */
export const MetricType = {
  COUNTER: 'counter',
  GAUGE: 'gauge',
  HISTOGRAM: 'histogram',
};

/**
 * Label schema - key-value pairs for metric dimensions
 */
export const LabelsSchema = z.record(z.string(), z.string());

/**
 * Counter metric configuration schema
 */
export const CounterConfigSchema = z.object({
  name: z.string().regex(/^[a-zA-Z_:][a-zA-Z0-9_:]*$/),
  help: z.string().min(1),
  labels: z.array(z.string()).default([]),
});

/**
 * Gauge metric configuration schema
 */
export const GaugeConfigSchema = z.object({
  name: z.string().regex(/^[a-zA-Z_:][a-zA-Z0-9_:]*$/),
  help: z.string().min(1),
  labels: z.array(z.string()).default([]),
});

/**
 * Histogram metric configuration schema
 */
export const HistogramConfigSchema = z.object({
  name: z.string().regex(/^[a-zA-Z_:][a-zA-Z0-9_:]*$/),
  help: z.string().min(1),
  labels: z.array(z.string()).default([]),
  buckets: z.array(z.number()).default([0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]),
});

/**
 * Metrics collector configuration schema
 */
export const MetricsCollectorConfigSchema = z.object({
  prefix: z.string().default('unrdf'),
  defaultLabels: LabelsSchema.default({}),
  aggregationWindow: z.number().int().min(1000).default(60000),
  maxTimeSeries: z.number().int().min(100).default(10000),
  enableAggregation: z.boolean().default(true),
});
