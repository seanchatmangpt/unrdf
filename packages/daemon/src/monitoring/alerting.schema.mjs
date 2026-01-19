/**
 * @file Alerting Zod Schemas
 * @module @unrdf/daemon/monitoring/alerting-schemas
 * @description Zod schema definitions for alert rules, notifications, and configuration.
 */

import { z } from 'zod';

/**
 * Alert severity levels
 */
export const AlertSeverity = {
  INFO: 'info',
  WARNING: 'warning',
  ERROR: 'error',
  CRITICAL: 'critical',
};

/**
 * Alert state values
 */
export const AlertState = {
  PENDING: 'pending',
  FIRING: 'firing',
  RESOLVED: 'resolved',
};

/**
 * Threshold comparison operators
 */
export const ComparisonOperator = {
  GREATER_THAN: 'gt',
  GREATER_THAN_OR_EQUAL: 'gte',
  LESS_THAN: 'lt',
  LESS_THAN_OR_EQUAL: 'lte',
  EQUAL: 'eq',
  NOT_EQUAL: 'neq',
};

/**
 * Threshold alert rule schema
 */
export const ThresholdAlertRuleSchema = z.object({
  type: z.literal('threshold'),
  name: z.string().min(1),
  metric: z.string().min(1),
  operator: z.enum(['gt', 'gte', 'lt', 'lte', 'eq', 'neq']),
  threshold: z.number(),
  duration: z.number().int().min(0).default(0),
  severity: z.enum(['info', 'warning', 'error', 'critical']).default('warning'),
  labels: z.record(z.string(), z.string()).optional(),
  annotations: z.record(z.string(), z.string()).optional(),
});

/**
 * Rate of change alert rule schema
 */
export const RateOfChangeAlertRuleSchema = z.object({
  type: z.literal('rate_of_change'),
  name: z.string().min(1),
  metric: z.string().min(1),
  window: z.number().int().min(1000).default(60000),
  threshold: z.number(),
  operator: z.enum(['gt', 'gte', 'lt', 'lte']).default('gt'),
  severity: z.enum(['info', 'warning', 'error', 'critical']).default('warning'),
  labels: z.record(z.string(), z.string()).optional(),
  annotations: z.record(z.string(), z.string()).optional(),
});

/**
 * Combined alert rule schema
 */
export const AlertRuleSchema = z.discriminatedUnion('type', [
  ThresholdAlertRuleSchema,
  RateOfChangeAlertRuleSchema,
]);

/**
 * Alert instance schema
 */
export const AlertSchema = z.object({
  id: z.string().min(1),
  ruleName: z.string().min(1),
  state: z.enum(['pending', 'firing', 'resolved']),
  severity: z.enum(['info', 'warning', 'error', 'critical']),
  value: z.number(),
  threshold: z.number(),
  metric: z.string().min(1),
  message: z.string(),
  labels: z.record(z.string(), z.string()).default({}),
  annotations: z.record(z.string(), z.string()).default({}),
  startsAt: z.date(),
  endsAt: z.date().optional(),
  updatedAt: z.date(),
  fingerprint: z.string().min(1),
});

/**
 * Webhook notification channel configuration schema
 */
export const WebhookChannelSchema = z.object({
  type: z.literal('webhook'),
  name: z.string().min(1),
  url: z.string().url(),
  method: z.enum(['GET', 'POST', 'PUT']).default('POST'),
  headers: z.record(z.string(), z.string()).optional(),
  timeout: z.number().int().min(1000).default(10000),
  retries: z.number().int().min(0).default(3),
});

/**
 * Console notification channel configuration schema
 */
export const ConsoleChannelSchema = z.object({
  type: z.literal('console'),
  name: z.string().min(1),
  format: z.enum(['json', 'text']).default('text'),
  level: z.enum(['debug', 'info', 'warn', 'error']).default('warn'),
});

/**
 * Combined notification channel schema
 */
export const NotificationChannelSchema = z.discriminatedUnion('type', [
  WebhookChannelSchema,
  ConsoleChannelSchema,
]);

/**
 * Alerting configuration schema
 */
export const AlertingConfigSchema = z.object({
  evaluationInterval: z.number().int().min(1000).default(15000),
  suppressionDuration: z.number().int().min(0).default(300000),
  deduplicationWindow: z.number().int().min(0).default(60000),
  maxAlertsPerRule: z.number().int().min(1).default(100),
  historyRetention: z.number().int().min(60000).default(86400000),
});
