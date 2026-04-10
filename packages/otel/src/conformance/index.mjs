/**
 * @file Process Conformance Module — Van der Aalst Pipeline Entry Point
 * @module otel/conformance
 * @description
 * Full Van der Aalst pipeline: OTel spans → OCEL log → conformance report.
 *
 * This is the upgrade to Chicago TDD that moves from "did the function return
 * success?" to "did a lawful process actually happen, provable from event evidence?"
 *
 * Usage:
 *   import { checkProcessConformance, DEVIATION_TYPES } from '@unrdf/otel/conformance';
 *   const report = await checkProcessConformance(spans);
 *   if (!report.conformant) throw new Error('Process violated manufacturing model');
 */

import { otelSpansToOcel } from '../ocel/index.mjs';
import { verifyProcessConformance, DEVIATION_TYPES } from '@unrdf/manufacturing/causality';

export { DEVIATION_TYPES, verifyProcessConformance };

/**
 * Run the full Van der Aalst conformance pipeline on OTel spans.
 *
 * 1. Converts OTel spans to an OCEL 2.0 object-centric event log
 * 2. Verifies the log against the intended manufacturing process model
 * 3. Returns fitness, precision, deviations, and diagnostics
 *
 * @param {Array<{spanId, traceId, name, startTimeUnixNano, attributes, status}>} otelSpans
 * @param {{ stages?: string[], expectedVariants?: object }} [intendedModel={}]
 * @returns {Promise<{
 *   conformant: boolean,
 *   fitness: number,
 *   precision: number,
 *   deviations: Array,
 *   diagnostics: object
 * }>}
 */
export async function checkProcessConformance(otelSpans, intendedModel = {}) {
  const ocelLog = otelSpansToOcel(otelSpans);
  return verifyProcessConformance(ocelLog, intendedModel);
}
