/**
 * @file OTel Integration Module
 * @module manufacturing/otel
 *
 * @description
 * OpenTelemetry integration for manufacturing pipeline.
 * Converts OTel traces to OCEL 2.0 event logs for process mining verification.
 *
 * @example
 * ```javascript
 * import { convertOtelToOCEL, validateOCELLog } from '@unrdf/manufacturing/otel';
 *
 * const ocelLog = convertOtelToOCEL(otelTraces);
 * const { valid, errors } = validateOCELLog(ocelLog);
 * ```
 */

export { convertOtelToOCEL, validateOCELLog, generateSampleOtelTraces } from './otel-to-ocel.mjs';
