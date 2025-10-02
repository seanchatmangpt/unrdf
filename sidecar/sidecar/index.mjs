/**
 * @file Sidecar client module exports
 * @module sidecar
 */

export { SidecarClient, createSidecarClient } from './client.mjs';
export { SidecarConfig, createSidecarConfig } from './config.mjs';
export { CircuitBreaker, createCircuitBreaker, CircuitState } from './circuit-breaker.mjs';
export { RetryStrategy, createRetryStrategy, retryWithBackoff } from './retry-strategy.mjs';
export { ConnectionPool, createConnectionPool } from './connection-pool.mjs';
export { HealthMonitor, createHealthMonitor, HealthStatus, createHealthCheckResult } from './health-check.mjs';
export { SidecarTelemetry, createSidecarTelemetry } from './telemetry.mjs';
export {
  createTelemetryInterceptor,
  createRetryInterceptor,
  createTimeoutInterceptor,
  createLoggingInterceptor,
  composeInterceptors
} from './interceptors.mjs';

export default { createSidecarClient };
