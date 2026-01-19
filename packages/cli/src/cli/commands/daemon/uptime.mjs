/**
 * @file Uptime Command - Uptime Simulation and Monitoring
 * @module cli/commands/daemon/uptime
 * @description CLI commands for uptime simulation, reporting, and benchmarking
 *
 * Provides comprehensive uptime management:
 * - simulate: Run uptime simulations with chaos injection
 * - report: Generate uptime reports with SLA analysis
 * - benchmark: Run performance benchmarks
 *
 * @example
 * # Run a simulation
 * unrdf daemon uptime simulate --duration 30000 --chaos-level medium
 *
 * # Generate a report
 * unrdf daemon uptime report --period day --sla-target 99.9
 *
 * # Run benchmarks
 * unrdf daemon uptime benchmark --iterations 100 --concurrency 10
 */

import { defineCommand } from 'citty';
import { simulateCommand } from './uptime/simulate.mjs';
import { reportCommand } from './uptime/report.mjs';
import { benchmarkCommand } from './uptime/benchmark.mjs';

/**
 * Uptime command with subcommands
 */
export const uptimeCommand = defineCommand({
  meta: {
    name: 'uptime',
    description: 'Uptime simulation, reporting, and benchmarking',
  },
  subCommands: {
    simulate: simulateCommand,
    report: reportCommand,
    benchmark: benchmarkCommand,
  },
});
