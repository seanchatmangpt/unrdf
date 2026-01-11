/**
 * @file Daemon Observability Schemas
 * @module @unrdf/daemon/integrations/observability-schemas
 * @description Zod schema definitions for observability configuration
 */

import { z } from 'zod';

export const ConfigSchema = z.object({
  serviceName: z.string().default('unrdf-daemon'),
  queueBacklogThreshold: z.number().default(100),
  healthCheckInterval: z.number().default(10000),
  prometheusPort: z.number().optional(),
  alertManagerUrl: z.string().optional(),
  enableYawlMetrics: z.boolean().default(true),
  enableSpanTracing: z.boolean().default(true),
  metricsHistorySize: z.number().default(10000),
});
