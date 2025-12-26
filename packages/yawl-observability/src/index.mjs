/**
 * @file YAWL Observability - Main entry point
 * @module @unrdf/yawl-observability
 *
 * @description
 * Comprehensive observability framework for YAWL workflows featuring:
 * - Prometheus metrics collection
 * - OpenTelemetry distributed tracing
 * - Custom Service Level Indicators (SLIs)
 * - SLO compliance monitoring
 * - Receipt-based proof correlation
 *
 * @example
 * ```javascript
 * import { createWorkflowEngine } from '@unrdf/yawl';
 * import {
 *   YAWLMetricsCollector,
 *   YAWLTracer,
 *   YAWLSLICalculator
 * } from '@unrdf/yawl-observability';
 *
 * // Create engine
 * const engine = createWorkflowEngine();
 *
 * // Setup observability
 * const metrics = new YAWLMetricsCollector(engine);
 * const tracer = new YAWLTracer(engine);
 * const sli = new YAWLSLICalculator(engine);
 *
 * // Expose metrics endpoint
 * app.get('/metrics', async (req, res) => {
 *   res.set('Content-Type', metrics.contentType);
 *   res.end(await metrics.getMetrics());
 * });
 *
 * // Get SLI snapshot
 * const snapshot = sli.getSnapshot();
 * console.log(`SLO Compliance: ${snapshot.sloCompliance.score * 100}%`);
 * ```
 */

// Metrics
export {
  YAWLMetricsCollector,
  createMetricsCollector,
  MetricsConfigSchema,
} from './metrics.mjs';

// Tracing
export {
  YAWLTracer,
  createTracer,
  TracingConfigSchema,
} from './tracing.mjs';

// SLI/SLO
export {
  YAWLSLICalculator,
  createSLICalculator,
  SLIConfigSchema,
} from './sli.mjs';

// Re-export for convenience
export { trace, context, SpanStatusCode, SpanKind } from '@opentelemetry/api';
