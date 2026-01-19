/**
 * @file Daemon Command - Background Operation Management
 * @module cli/commands/daemon
 * @description CLI commands for managing UNRDF daemon operations, scheduling, and monitoring
 *
 * Provides comprehensive daemon management:
 * - list: Show all configured operations
 * - run: Execute operation immediately with optional payload
 * - schedule: Add scheduled trigger to operation
 * - status: Show daemon health and metrics
 * - logs: View operation logs with filtering and following
 * - config: Display current daemon configuration
 * - cluster: Show Raft cluster status and members
 * - uptime: Uptime simulation, reporting, and benchmarking
 */

import { defineCommand } from 'citty';
import { listCommand } from './daemon/list.mjs';
import { runCommand } from './daemon/run.mjs';
import { scheduleCommand } from './daemon/schedule.mjs';
import { statusCommand } from './daemon/status.mjs';
import { logsCommand } from './daemon/logs.mjs';
import { configCommand } from './daemon/config.mjs';
import { clusterCommand } from './daemon/cluster.mjs';
import { uptimeCommand } from './daemon/uptime.mjs';

/**
 * Main daemon command
 */
export const daemonCommand = defineCommand({
  meta: {
    name: 'daemon',
    description: 'Manage background operations and daemon lifecycle',
  },
  subCommands: {
    list: listCommand,
    run: runCommand,
    schedule: scheduleCommand,
    status: statusCommand,
    logs: logsCommand,
    config: configCommand,
    cluster: clusterCommand,
    uptime: uptimeCommand,
  },
});
