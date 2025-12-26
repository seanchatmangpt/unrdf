/**
 * @file Configuration
 * @description Configuration management for distributed orchestration
 * @module distributed-orchestration/config
 */

import { z } from 'zod';

/**
 * Configuration schema
 */
const ConfigSchema = z.object({
  orchestrator: z.object({
    port: z.number().default(8080),
    peers: z.array(z.string()).default([]),
    maxWorkers: z.number().default(10),
    taskTimeout: z.number().default(30000),
  }),
  worker: z.object({
    nodeId: z.string().optional(),
    orchestratorUrl: z.string().default('http://localhost:8080'),
    capacity: z.number().default(5),
    heartbeatInterval: z.number().default(5000),
  }),
  dashboard: z.object({
    port: z.number().default(3000),
    updateInterval: z.number().default(1000),
  }),
  telemetry: z.object({
    enabled: z.boolean().default(true),
    endpoint: z.string().optional(),
  }),
});

/**
 * Load configuration from environment
 *
 * @returns {object} Validated configuration
 */
export function loadConfig() {
  const config = {
    orchestrator: {
      port: parseInt(process.env.ORCHESTRATOR_PORT || '8080', 10),
      peers: process.env.PEERS ? process.env.PEERS.split(',') : [],
      maxWorkers: parseInt(process.env.MAX_WORKERS || '10', 10),
      taskTimeout: parseInt(process.env.TASK_TIMEOUT || '30000', 10),
    },
    worker: {
      nodeId: process.env.NODE_ID,
      orchestratorUrl: process.env.ORCHESTRATOR_URL || 'http://localhost:8080',
      capacity: parseInt(process.env.WORKER_CAPACITY || '5', 10),
      heartbeatInterval: parseInt(process.env.HEARTBEAT_INTERVAL || '5000', 10),
    },
    dashboard: {
      port: parseInt(process.env.DASHBOARD_PORT || '3000', 10),
      updateInterval: parseInt(process.env.UPDATE_INTERVAL || '1000', 10),
    },
    telemetry: {
      enabled: process.env.TELEMETRY_ENABLED !== 'false',
      endpoint: process.env.OTEL_ENDPOINT,
    },
  };

  return ConfigSchema.parse(config);
}

export default loadConfig();
