/**
 * @unrdf/otel — OpenTelemetry integration for UNRDF using pm4py-rust telemetry infrastructure.
 *
 * This package integrates UNRDF's semantic conventions with pm4py-rust's
 * comprehensive observability stack including metrics collection, alerting,
 * and monitoring dashboards.
 *
 * PM4Py Integration:
 * - Uses ~/chatmangpt/pm4py monitoring infrastructure
 * - Leverages pm4py's MetricCollector for process mining metrics
 * - Integrates with pm4py's AlertManager for incident management
 * - Shares OTLP collector configuration
 *
 * Generated semantic conventions from OTel Weaver:
 * Re-generate: cd otel && weaver registry generate --registry registry/ js ../packages/otel/src/generated/
 */

// Export generated semantic conventions
export * from './generated/attributes.mjs';
export * from './generated/metrics.mjs';

// Export pm4py integration
export * from './pm4py.mjs';

// Export monitoring utilities
export * from './monitoring.mjs';

// Export semantic invariants validation layer
export * from './validation/index.mjs';
