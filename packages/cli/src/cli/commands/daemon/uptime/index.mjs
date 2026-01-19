/**
 * @file Uptime Commands Index
 * @module cli/commands/daemon/uptime
 * @description Export all uptime simulation commands and utilities
 *
 * This module provides CLI commands for:
 * - Running uptime simulations with chaos injection
 * - Generating uptime reports with SLA analysis
 * - Running performance benchmarks
 *
 * @example
 * // Import specific commands
 * import { simulateCommand, reportCommand, benchmarkCommand } from './uptime/index.mjs';
 *
 * // Import schemas for validation
 * import { SimulateArgsSchema, ReportArgsSchema } from './uptime/index.mjs';
 *
 * // Import helpers for custom implementations
 * import { calculateStats, formatDuration } from './uptime/index.mjs';
 */

// Commands
export { simulateCommand } from './simulate.mjs';
export { reportCommand } from './report.mjs';
export { benchmarkCommand } from './benchmark.mjs';

// Schemas
export {
  ChaosLevel,
  OutputFormat,
  ReportPeriod,
  SimulateArgsSchema,
  ReportArgsSchema,
  BenchmarkArgsSchema,
  SimulationResultSchema,
  ReportResultSchema,
  BenchmarkResultSchema,
} from './schemas.mjs';

// Helpers
export {
  generateId,
  createSeededRandom,
  formatDuration,
  formatPercentage,
  CHAOS_PARAMS,
  simulateHeartbeat,
  calculatePercentile,
  calculateStats,
  calculateMTBF,
  calculateMTTR,
  formatAsCSV,
  formatBenchmarkAsCSV,
  writeOutput,
  printSeparator,
  printHeader,
} from './helpers.mjs';
