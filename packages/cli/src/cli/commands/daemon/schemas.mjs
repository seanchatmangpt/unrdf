/**
 * @file Daemon Command Schemas
 * @module cli/commands/daemon/schemas
 * @description Zod validation schemas for daemon commands
 */

import { z } from 'zod';

export const ListArgsSchema = z.object({
  json: z.boolean().optional().default(false),
  'include-metadata': z.boolean().optional().default(false),
});

export const RunArgsSchema = z.object({
  operation: z.string().min(1, 'Operation ID required'),
  payload: z.string().optional(),
  json: z.boolean().optional().default(false),
  timeout: z.number().optional().default(30000),
});

export const ScheduleArgsSchema = z.object({
  operation: z.string().min(1, 'Operation ID required'),
  trigger: z.string().min(1, 'Trigger type required'),
  payload: z.string().optional(),
  json: z.boolean().optional().default(false),
});

export const StatusArgsSchema = z.object({
  json: z.boolean().optional().default(false),
  'include-metrics': z.boolean().optional().default(false),
});

export const LogsArgsSchema = z.object({
  follow: z.boolean().optional().default(false),
  filter: z.string().optional(),
  'max-lines': z.number().optional().default(100),
  json: z.boolean().optional().default(false),
});

export const ConfigArgsSchema = z.object({
  json: z.boolean().optional().default(false),
});

export const ClusterArgsSchema = z.object({
  json: z.boolean().optional().default(false),
  'include-metrics': z.boolean().optional().default(false),
});
