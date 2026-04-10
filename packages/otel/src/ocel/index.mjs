/**
 * @file OCEL Module — OTel Span to Object-Centric Event Log
 * @module otel/ocel
 * @description
 * Converts OpenTelemetry spans into OCEL 2.0 event logs for Van der Aalst-style
 * process conformance checking. This is the bridge that makes "did a lawful process
 * happen?" a testable question, not just "did the function return success?".
 *
 * Port of ostar's chicago_tdd.py otel_spans_to_ocel() and generate_impossible_log().
 */

/**
 * Declared manufacturing pipeline stages in lawful order.
 * Any observed execution must follow this sequence.
 */
export const MANUFACTURING_STAGES = [
  'seeded',
  'bred',
  'validated',
  'projected',
  'compiled',
  'benchmarked',
  'released',
  'observed',
  'receipted',
];

/**
 * Named scenarios for negative testing — impossible event sequences
 * that a conformance checker must reject.
 */
export const IMPOSSIBLE_LOG_SCENARIOS = [
  'release_before_validate',
  'validate_before_breed',
  'concurrent_terminal_states',
  'receipt_without_artifact',
  'benchmark_for_uncompiled',
  'proof_gate_pass_missing_causal',
  'orphan_mcp_invocation',
];

/**
 * Maps OTel span names (lowercased) to OCEL activity names and object types.
 * Partial string matching: first match wins.
 */
const SPAN_ACTIVITY_MAP = [
  { pattern: /seed/,      activity: 'seeded',      objectType: 'artifact' },
  { pattern: /breed|bred/, activity: 'bred',       objectType: 'artifact' },
  { pattern: /valida/,    activity: 'validated',   objectType: 'artifact' },
  { pattern: /project/,   activity: 'projected',   objectType: 'artifact' },
  { pattern: /compil/,    activity: 'compiled',    objectType: 'artifact' },
  { pattern: /benchmark/, activity: 'benchmarked', objectType: 'benchmark' },
  { pattern: /release|releas/, activity: 'released', objectType: 'release' },
  { pattern: /observ/,    activity: 'observed',    objectType: 'artifact' },
  { pattern: /receipt|receip/, activity: 'receipted', objectType: 'receipt' },
  { pattern: /proof.gate|gate_eval/, activity: 'gate_evaluated', objectType: 'proof_gate' },
  { pattern: /mcp|tool.call/,        activity: 'mcp_invocation',  objectType: 'mcp_tool' },
];

/**
 * Infer OCEL activity and object type from a span name.
 * @param {string} name - OTel span name
 * @returns {{ activity: string, objectType: string }}
 */
function inferOcelMetadata(name) {
  const lower = name.toLowerCase();
  for (const entry of SPAN_ACTIVITY_MAP) {
    if (entry.pattern.test(lower)) {
      return { activity: entry.activity, objectType: entry.objectType };
    }
  }
  return { activity: lower, objectType: 'artifact' };
}

/**
 * Extract object ID from span attributes.
 * Priority: artifact.id → artifact_id → case_id → spanId
 * @param {object} attributes - OTel span attributes
 * @param {string} fallback - spanId to use if no attribute found
 * @returns {string}
 */
function extractObjectId(attributes, fallback) {
  return attributes?.['artifact.id']
    ?? attributes?.['artifact_id']
    ?? attributes?.['case_id']
    ?? fallback;
}

/**
 * Convert nanosecond Unix timestamp string to ISO 8601 string.
 * @param {string} nanoStr
 * @returns {string}
 */
function nanoToIso(nanoStr) {
  const ms = Number(BigInt(nanoStr) / 1_000_000n);
  return new Date(ms).toISOString();
}

/**
 * Convert an array of OTel spans into an OCEL 2.0 event log.
 *
 * The OCEL log format matches what verifyProcessConformance() expects:
 * {
 *   objects: { [id]: { id, type, kind? } },
 *   events:  [{ object, type, activity, timestamp, traceId, attributes }]
 * }
 *
 * @param {Array<{spanId, traceId, name, startTimeUnixNano, attributes, status}>} spans
 * @returns {{ objects: object, events: Array }}
 */
export function otelSpansToOcel(spans) {
  const objects = {};
  const events = [];

  for (const span of spans) {
    const { activity, objectType } = inferOcelMetadata(span.name || '');
    const objectId = extractObjectId(span.attributes, span.spanId);
    const timestamp = nanoToIso(span.startTimeUnixNano || '0');

    // Register object if not seen
    if (!objects[objectId]) {
      objects[objectId] = {
        id: objectId,
        type: objectType,
        kind: span.attributes?.['artifact.kind'] ?? undefined,
      };
    }

    // Emit creation event on first encounter of an artifact
    if (objectType === 'artifact' && activity === 'seeded') {
      events.push({
        object: objectId,
        type: 'ArtifactCreated',
        activity: 'seeded',
        timestamp,
        traceId: span.traceId,
        attributes: span.attributes ?? {},
      });
    } else if (activity === 'released') {
      events.push({
        object: objectId,
        type: 'ArtifactReleased',
        activity: 'released',
        timestamp,
        traceId: span.traceId,
        attributes: span.attributes ?? {},
      });
    } else if (activity === 'receipted') {
      events.push({
        object: objectId,
        type: 'ReceiptGenerated',
        activity: 'receipted',
        timestamp,
        traceId: span.traceId,
        attributes: span.attributes ?? {},
      });
    } else {
      events.push({
        object: objectId,
        type: `Stage_${activity}`,
        activity,
        timestamp,
        traceId: span.traceId,
        attributes: span.attributes ?? {},
      });
    }
  }

  return { objects, events };
}

/**
 * Generate an impossible OCEL log for a named scenario.
 * Used in negative tests — the conformance checker must reject these logs.
 *
 * @param {string} scenario - One of IMPOSSIBLE_LOG_SCENARIOS
 * @returns {{ objects: object, events: Array }}
 * @throws {Error} for unknown scenario names
 */
export function generateImpossibleLog(scenario) {
  const generators = {
    release_before_validate: () => ({
      objects: {
        'art-1': { id: 'art-1', type: 'artifact', kind: 'sparql' },
      },
      events: [
        { object: 'art-1', type: 'ArtifactCreated',  activity: 'seeded',   timestamp: iso(1) },
        { object: 'art-1', type: 'Stage_bred',        activity: 'bred',     timestamp: iso(2) },
        // Released without validated — impossible
        { object: 'art-1', type: 'ArtifactReleased', activity: 'released', timestamp: iso(3) },
      ],
    }),

    validate_before_breed: () => ({
      objects: {
        'art-2': { id: 'art-2', type: 'artifact', kind: 'powl' },
      },
      events: [
        { object: 'art-2', type: 'ArtifactCreated', activity: 'seeded',    timestamp: iso(1) },
        // Validated before bred — backward transition
        { object: 'art-2', type: 'Stage_validated', activity: 'validated', timestamp: iso(2) },
        { object: 'art-2', type: 'Stage_bred',      activity: 'bred',      timestamp: iso(3) },
        { object: 'art-2', type: 'ArtifactReleased', activity: 'released', timestamp: iso(4) },
      ],
    }),

    concurrent_terminal_states: () => ({
      objects: {
        'art-3': { id: 'art-3', type: 'artifact', kind: 'shacl' },
      },
      events: [
        { object: 'art-3', type: 'ArtifactCreated',  activity: 'seeded',   timestamp: iso(1) },
        { object: 'art-3', type: 'Stage_validated',  activity: 'validated', timestamp: iso(2) },
        // Two release events for the same artifact — impossible
        { object: 'art-3', type: 'ArtifactReleased', activity: 'released', timestamp: iso(3) },
        { object: 'art-3', type: 'ArtifactReleased', activity: 'released', timestamp: iso(4) },
      ],
    }),

    receipt_without_artifact: () => ({
      objects: {
        'receipt-x': { id: 'receipt-x', type: 'receipt' },
      },
      events: [
        // Receipt references an artifact that doesn't exist
        {
          object: 'receipt-x', type: 'ReceiptGenerated', activity: 'receipted',
          timestamp: iso(1), attributes: { artifactId: 'phantom-artifact' },
        },
      ],
    }),

    benchmark_for_uncompiled: () => ({
      objects: {
        'art-4': { id: 'art-4', type: 'artifact', kind: 'python' },
      },
      events: [
        { object: 'art-4', type: 'ArtifactCreated',  activity: 'seeded',      timestamp: iso(1) },
        { object: 'art-4', type: 'Stage_validated',  activity: 'validated',    timestamp: iso(2) },
        // Benchmarked without compiled — skipped critical stage
        { object: 'art-4', type: 'Stage_benchmarked', activity: 'benchmarked', timestamp: iso(3) },
      ],
    }),

    proof_gate_pass_missing_causal: () => ({
      objects: {
        'gate-1': { id: 'gate-1', type: 'proof_gate' },
        'art-5':  { id: 'art-5',  type: 'artifact', kind: 'typescript' },
      },
      events: [
        // Gate evaluated before artifact reached validated state
        {
          object: 'gate-1', type: 'GateEvaluated', activity: 'gate_evaluated',
          timestamp: iso(1), attributes: { artifactId: 'art-5', result: 'pass' },
        },
        { object: 'art-5', type: 'ArtifactCreated', activity: 'seeded', timestamp: iso(2) },
      ],
    }),

    orphan_mcp_invocation: () => ({
      objects: {
        'mcp-1': { id: 'mcp-1', type: 'mcp_tool' },
      },
      events: [
        // MCP tool call with no associated manufacturing case
        {
          object: 'mcp-1', type: 'McpInvocation', activity: 'mcp_invocation',
          timestamp: iso(1), attributes: { tool: 'onto_query' },
        },
      ],
    }),
  };

  if (!generators[scenario]) {
    throw new Error(
      `Unknown impossible log scenario: "${scenario}". Known: ${IMPOSSIBLE_LOG_SCENARIOS.join(', ')}`,
    );
  }

  return generators[scenario]();
}

/** Helper: produce a deterministic ISO timestamp at offset T seconds */
function iso(offsetSeconds) {
  return new Date(1_700_000_000_000 + offsetSeconds * 1000).toISOString();
}
