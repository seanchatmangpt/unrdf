/**
 * @file Zod schemas for temporal discovery
 * @module @unrdf/temporal-discovery/schemas
 * @description Runtime validation schemas for all temporal discovery operations
 */

import { z } from 'zod';

/**
 * Temporal data point schema
 */
export const TemporalDataPointSchema = z.object({
  timestamp: z.number().int().positive(),
  value: z.number(),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * Time series schema
 */
export const TimeSeriesSchema = z.object({
  name: z.string().min(1),
  data: z.array(TemporalDataPointSchema).min(1),
  unit: z.string().optional(),
});

/**
 * Pattern schema
 */
export const PatternSchema = z.object({
  items: z.array(z.string()).min(1),
  support: z.number().min(0).max(1),
  confidence: z.number().min(0).max(1).optional(),
  occurrences: z.number().int().nonnegative(),
});

/**
 * Pattern mining options schema
 */
export const PatternMiningOptionsSchema = z.object({
  minSupport: z.number().min(0).max(1).default(0.1),
  minConfidence: z.number().min(0).max(1).default(0.5),
  maxPatternLength: z.number().int().positive().default(5),
});

/**
 * Anomaly schema
 */
export const AnomalySchema = z.object({
  timestamp: z.number().int().positive(),
  value: z.number(),
  zScore: z.number(),
  severity: z.enum(['low', 'medium', 'high', 'critical']),
  expectedValue: z.number().optional(),
});

/**
 * Anomaly detection options schema
 */
export const AnomalyDetectionOptionsSchema = z.object({
  threshold: z.number().positive().default(3.0),
  windowSize: z.number().int().positive().default(100),
  method: z.enum(['zscore', 'mad', 'iqr']).default('zscore'),
});

/**
 * Trend schema
 */
export const TrendSchema = z.object({
  direction: z.enum(['increasing', 'decreasing', 'stable', 'volatile']),
  slope: z.number(),
  strength: z.number().min(0).max(1),
  startTimestamp: z.number().int().positive(),
  endTimestamp: z.number().int().positive(),
  points: z.number().int().positive(),
});

/**
 * Trend analysis options schema
 */
export const TrendAnalysisOptionsSchema = z.object({
  windowSize: z.number().int().positive().default(10),
  smoothingFactor: z.number().min(0).max(1).default(0.3),
  volatilityThreshold: z.number().positive().default(0.5),
});

/**
 * Correlation schema
 */
export const CorrelationSchema = z.object({
  series1: z.string(),
  series2: z.string(),
  coefficient: z.number().min(-1).max(1),
  pValue: z.number().min(0).max(1).optional(),
  strength: z.enum(['very_weak', 'weak', 'moderate', 'strong', 'very_strong']),
  direction: z.enum(['positive', 'negative', 'none']),
});

/**
 * Correlation options schema
 */
export const CorrelationOptionsSchema = z.object({
  method: z.enum(['pearson', 'spearman', 'kendall']).default('pearson'),
  minOverlap: z.number().int().positive().default(10),
  lagMax: z.number().int().nonnegative().default(0),
});

/**
 * Changepoint schema
 */
export const ChangepointSchema = z.object({
  timestamp: z.number().int().positive(),
  index: z.number().int().nonnegative(),
  cost: z.number(),
  beforeMean: z.number(),
  afterMean: z.number(),
  magnitude: z.number(),
});

/**
 * Changepoint detection options schema
 */
export const ChangepointDetectionOptionsSchema = z.object({
  penalty: z.number().positive().default(3.0),
  minSegmentLength: z.number().int().positive().default(5),
  maxChangepoints: z.number().int().positive().default(10),
  method: z.enum(['pelt', 'binary_segmentation', 'bottom_up']).default('pelt'),
});

/**
 * Discovery results schema
 */
export const DiscoveryResultsSchema = z.object({
  patterns: z.array(PatternSchema).optional(),
  anomalies: z.array(AnomalySchema).optional(),
  trends: z.array(TrendSchema).optional(),
  correlations: z.array(CorrelationSchema).optional(),
  changepoints: z.array(ChangepointSchema).optional(),
  metadata: z.object({
    executionTimeMs: z.number().nonnegative(),
    dataPointsProcessed: z.number().int().nonnegative(),
    algorithmsRun: z.array(z.string()),
  }),
});

/**
 * Discovery options schema
 */
export const DiscoveryOptionsSchema = z.object({
  enablePatternMining: z.boolean().default(true),
  enableAnomalyDetection: z.boolean().default(true),
  enableTrendAnalysis: z.boolean().default(true),
  enableCorrelationAnalysis: z.boolean().default(false),
  enableChangepointDetection: z.boolean().default(false),
  patternMining: PatternMiningOptionsSchema.optional(),
  anomalyDetection: AnomalyDetectionOptionsSchema.optional(),
  trendAnalysis: TrendAnalysisOptionsSchema.optional(),
  correlation: CorrelationOptionsSchema.optional(),
  changepoint: ChangepointDetectionOptionsSchema.optional(),
});
