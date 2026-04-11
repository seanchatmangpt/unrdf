/**
 * @file OTel to OCEL Converter
 * @module manufacturing/otel/otel-to-ocel
 *
 * @description
 * Convert OpenTelemetry traces to OCEL 2.0 object-centric event logs
 * for process mining verification.
 *
 * Bridges the gap between runtime execution evidence (OTel spans)
 * and process truth verification (OCEL logs + pm4py discovery).
 *
 * Usage:
 * ```javascript
 * import { convertOtelToOCEL } from '@unrdf/manufacturing/otel/otel-to-ocel.mjs';
 *
 * // From Jaeger trace export
 * const jaegerTraces = await fetch('http://localhost:16686/api/traces?service=manufacturing');
 * const ocelLog = convertOtelToOCEL(await jaegerTraces.json());
 *
 * // Verify OCEL structure
 * console.log(ocelLog.objects);      // artifact, receipt, proof_gate, release
 * console.log(ocelLog.events);       // OCEL 2.0 events
 * ```
 */

/**
 * OTel span event to OCEL event type mapping
 * @constant {Object<string, string>}
 */
const SPAN_TO_OCEL_EVENT = {
  // Artifact lifecycle events
  'ArtifactCreated': 'ArtifactCreated',
  'ArtifactTransformed': 'ArtifactTransformed',
  'ArtifactValidated': 'ArtifactValidated',
  'ArtifactProjected': 'ArtifactProjected',
  'ArtifactCompiled': 'ArtifactCompiled',
  'ArtifactBenchmarked': 'ArtifactBenchmarked',
  'ArtifactReleased': 'ArtifactReleased',
  'ArtifactObserved': 'ArtifactObserved',
  'ArtifactReceipted': 'ArtifactReceipted',

  // Receipt events
  'ReceiptGenerated': 'ReceiptGenerated',
  'ReceiptLinked': 'ReceiptGenerated',

  // Gate events
  'ProofGateEvaluated': 'ProofGateEvaluated',
  'GatePassed': 'ProofGateEvaluated',
  'GateFailed': 'ProofGateEvaluated',

  // Operator events
  'OperatorExecuted': 'OperatorExecuted',
  'OperatorError': 'OperatorExecuted',
};

/**
 * OTel span name to OCEL activity mapping
 * Extracts activity from span names like "operator.validate", "gate.schema-valid"
 * @constant {RegExp}
 */
const SPAN_NAME_ACTIVITY_RE = /^(?:operator|gate|stage)\.([^.]+)$/;

/**
 * Manufacturing stage ordering (for lifecycle validation)
 * @constant {string[]}
 */
const STAGE_ORDER = [
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
 * Convert OTel traces to OCEL 2.0 object-centric event log
 *
 * @param {object} otelTraces - OTel trace data from Jaeger/OTEL collector
 * @param {object} config - Conversion configuration
 * @param {string} [config.serviceName='manufacturing'] - Filter by service name
 * @param {object} [config.eventTypes=SPAN_TO_OCEL_EVENT] - Custom event type mapping
 * @param {number} [config.maxEvents=10000] - Maximum events to process (prevent memory overflow)
 * @returns {object} OCEL 2.0 JSON log
 *
 * @example
 * ```javascript
 * const ocelLog = convertOtelToOCEL({
 *   data: [
 *     {
 *       traceID: 'abc123',
 *       spans: [
 *         {
 *           traceID: 'abc123',
 *           spanID: 'def456',
 *           operationName: 'operator.validate',
 *           startTime: 1645123456000000, // microseconds
 *           duration: 5000,               // microseconds
 *           process: {
 *             serviceName: 'manufacturing'
 *           },
 *           tags: [
 *             { key: 'artifact.id', value: 'artifact-1' },
 *             { key: 'artifact.kind', value: 'source-code' },
 *             { key: 'stage', value: 'validated' },
 *             { key: 'status', value: 'success' }
 *           ]
 *         }
 *       ]
 *     }
 *   ]
 * });
 * ```
 */
export function convertOtelToOCEL(otelTraces, config = {}) {
  const {
    serviceName = 'manufacturing',
    eventTypes = SPAN_TO_OCEL_EVENT,
    maxEvents = 10000,
  } = config;

  // OCEL 2.0 structure
  const ocelLog = {
    '@context': 'https://ocelstandard.org/ocel2.0/context.json',
    'objects': {},
    'events': [],
  };

  let eventCount = 0;

  // Flatten all spans from all traces
  const allSpans = [];
  if (otelTraces.data && Array.isArray(otelTraces.data)) {
    for (const trace of otelTraces.data) {
      if (trace.spans && Array.isArray(trace.spans)) {
        for (const span of trace.spans) {
          // Filter by service name
          if (span.process?.serviceName === serviceName) {
            allSpans.push(span);
          }
        }
      }
    }
  }

  // Sort spans by start time (temporal ordering)
  allSpans.sort((a, b) => {
    const timeA = a.startTime || 0;
    const timeB = b.startTime || 0;
    return timeA - timeB;
  });

  // Process each span
  for (const span of allSpans) {
    if (eventCount >= maxEvents) {
      console.warn(`OTel→OCEL: Reached maxEvents limit (${maxEvents}), stopping conversion`);
      break;
    }

    // Extract span attributes from tags
    const attributes = extractAttributes(span.tags || []);

    // Determine OCEL event type
    let eventType = determineEventType(span, attributes, eventTypes);

    // Skip unknown event types
    if (!eventType) {
      continue;
    }

    // Determine activity from span name or attributes
    const activity = determineActivity(span, attributes, eventType);

    // Determine object ID and type
    const { objectId, objectType } = determineObjectInfo(span, attributes, eventType);

    // Create or update object
    ensureObject(ocelLog.objects, objectId, objectType, span, attributes, eventType);

    // Create OCEL event
    const ocelEvent = createOCelEvent(span, eventType, activity, objectId, attributes);

    ocelLog.events.push(ocelEvent);
    eventCount++;
  }

  // Post-process: build lifecycle arrays for artifacts
  postProcessObjects(ocelLog.objects);

  return ocelLog;
}

/**
 * Extract attributes from OTel span tags
 * @param {Array<{key: string, value: string, type?: string}>} tags
 * @returns {Object<string, any>}
 */
function extractAttributes(tags) {
  const attributes = {};

  for (const tag of tags) {
    // Parse different value types
    let value = tag.value;
    if (tag.type === 'bool') {
      value = value === 'true' || value === true;
    } else if (tag.type === 'number' || tag.vType === 'int64' || tag.vType === 'float64') {
      value = Number(value);
    } else if (tag.type === 'string') {
      // Keep as string
    }

    attributes[tag.key] = value;
  }

  return attributes;
}

/**
 * Determine OCEL event type from span
 * @param {object} span - OTel span
 * @param {object} attributes - Extracted attributes
 * @param {object} eventTypes - Event type mapping
 * @returns {string|null} OCEL event type or null
 */
function determineEventType(span, attributes, eventTypes) {
  // Check explicit event type attribute
  if (attributes['ocel.event.type']) {
    return attributes['ocel.event.type'];
  }

  // Check span name for event type patterns
  const spanName = span.operationName || '';
  for (const [pattern, ocelType] of Object.entries(eventTypes)) {
    if (spanName.includes(pattern) || spanName.startsWith(pattern)) {
      return ocelType;
    }
  }

  // Infer from span name prefix (operator.*, gate.*, stage.*)
  if (spanName.startsWith('operator.') || spanName.startsWith('OperatorExecuted')) {
    return 'OperatorExecuted';
  }
  if (spanName.startsWith('gate.') || spanName.startsWith('ProofGateEvaluated')) {
    return 'ProofGateEvaluated';
  }
  if (spanName.startsWith('stage.') || spanName.startsWith('Artifact')) {
    // Extract stage name from span name
    const match = spanName.match(/^stage\.([^.]+)|Artifact([A-Z][a-z]+)/);
    if (match) {
      const stage = (match[1] || match[2]).toLowerCase();
      const stageToEvent = {
        seeded: 'ArtifactCreated',
        bred: 'ArtifactTransformed',
        validated: 'ArtifactValidated',
        projected: 'ArtifactProjected',
        compiled: 'ArtifactCompiled',
        benchmarked: 'ArtifactBenchmarked',
        released: 'ArtifactReleased',
        observed: 'ArtifactObserved',
        receipted: 'ArtifactReceipted',
        created: 'ArtifactCreated',
        transformed: 'ArtifactTransformed',
      };
      return stageToEvent[stage] || 'OperatorExecuted';
    }
    return 'OperatorExecuted';
  }

  // Infer from attributes
  if (attributes['gate'] || attributes['gate.name']) {
    return 'ProofGateEvaluated';
  }
  if (attributes['receipt'] || attributes['receipt.hash']) {
    return 'ReceiptGenerated';
  }
  if (attributes['operator'] || attributes['operator.name']) {
    return 'OperatorExecuted';
  }
  if (attributes['stage'] || attributes['artifact.stage']) {
    const stage = attributes['stage'] || attributes['artifact.stage'];
    // Map stage to event type
    const stageToEvent = {
      seeded: 'ArtifactCreated',
      bred: 'ArtifactTransformed',
      validated: 'ArtifactValidated',
      projected: 'ArtifactProjected',
      compiled: 'ArtifactCompiled',
      benchmarked: 'ArtifactBenchmarked',
      released: 'ArtifactReleased',
      observed: 'ArtifactObserved',
      receipted: 'ArtifactReceipted',
    };
    return stageToEvent[stage] || null;
  }

  // Default: if span has any artifact-related attributes, treat as operator execution
  if (attributes['artifact.id'] || attributes['artifact_id'] || attributes['artifact']) {
    return 'OperatorExecuted';
  }

  return null;
}

/**
 * Determine activity from span
 * @param {object} span - OTel span
 * @param {object} attributes - Extracted attributes
 * @param {string} eventType - OCEL event type
 * @returns {string} Activity name
 */
function determineActivity(span, attributes, eventType) {
  // Check explicit activity attribute
  if (attributes['ocel.activity']) {
    return attributes['ocel.activity'];
  }

  // Extract from span name using regex
  const spanName = span.operationName || '';
  const match = spanName.match(SPAN_NAME_ACTIVITY_RE);
  if (match && match[2]) {
    return match[2];
  }

  // Infer from event type and attributes
  if (eventType === 'OperatorExecuted') {
    return attributes['operator'] || attributes['operator.name'] || 'execute';
  }
  if (eventType === 'ProofGateEvaluated') {
    return attributes['gate'] || attributes['gate.name'] || 'evaluate-gate';
  }
  if (eventType === 'ReceiptGenerated') {
    return 'emit-receipt';
  }

  // Default: use event type without "Artifact" prefix
  return eventType.replace(/^Artifact/, '').toLowerCase() || 'process';
}

/**
 * Determine object ID and type from span
 * @param {object} span - OTel span
 * @param {object} attributes - Extracted attributes
 * @param {string} eventType - OCEL event type
 * @returns {{objectId: string, objectType: string}}
 */
function determineObjectInfo(span, attributes, eventType) {
  // Default object ID from span
  let objectId = span.spanID;
  let objectType = 'unknown';

  // Check explicit object attributes
  if (attributes['artifact.id'] || attributes['artifact_id']) {
    objectId = attributes['artifact.id'] || attributes['artifact_id'];
    objectType = 'artifact';
  } else if (attributes['receipt.id'] || attributes['receipt_id']) {
    objectId = attributes['receipt.id'] || attributes['receipt_id'];
    objectType = 'receipt';
  } else if (attributes['gate.id'] || attributes['gate_id']) {
    objectId = attributes['gate.id'] || attributes['gate_id'];
    objectType = 'proof_gate';
  } else if (attributes['release.id'] || attributes['release_id']) {
    objectId = attributes['release.id'] || attributes['release_id'];
    objectType = 'release';
  } else {
    // Infer from event type
    if (eventType.startsWith('Artifact') || eventType === 'OperatorExecuted') {
      objectId = attributes['artifact'] || `artifact-${span.spanID}`;
      objectType = 'artifact';
    } else if (eventType === 'ReceiptGenerated') {
      objectId = attributes['receipt'] || `receipt-${span.spanID}`;
      objectType = 'receipt';
    } else if (eventType === 'ProofGateEvaluated') {
      objectId = attributes['gate'] || `gate-${span.spanID}`;
      objectType = 'proof_gate';
    }
  }

  return { objectId, objectType };
}

/**
 * Ensure object exists in OCEL objects map
 * @param {Object} objects - OCEL objects map
 * @param {string} objectId - Object ID
 * @param {string} objectType - Object type
 * @param {object} span - OTel span
 * @param {object} attributes - Extracted attributes
 * @param {string} eventType - OCEL event type
 */
function ensureObject(objects, objectId, objectType, span, attributes, eventType) {
  if (!objects[objectId]) {
    objects[objectId] = {
      id: objectId,
      type: objectType,
      lifecycle: [],
    };
  }

  const obj = objects[objectId];

  // Add type-specific attributes
  if (objectType === 'artifact') {
    if (attributes['artifact.kind'] || attributes['kind']) {
      obj.kind = attributes['artifact.kind'] || attributes['kind'];
    }
    if (attributes['stage']) {
      obj.stage = attributes['stage'];
    }
  } else if (objectType === 'receipt') {
    if (attributes['receipt.hash'] || attributes['hash']) {
      obj.hash = attributes['receipt.hash'] || attributes['hash'];
    }
    if (attributes['receipt.previousHash'] || attributes['previousHash']) {
      obj.previousHash = attributes['receipt.previousHash'] || attributes['previousHash'];
    }
    if (attributes['artifact'] || attributes['artifact.id']) {
      obj.artifact = attributes['artifact'] || attributes['artifact.id'];
    }
    if (attributes['stage']) {
      obj.stage = attributes['stage'];
    }
  } else if (objectType === 'proof_gate') {
    if (attributes['gate'] || attributes['gate.name']) {
      obj.gate = attributes['gate'] || attributes['gate.name'];
    }
    if (attributes['result'] || attributes['gate.result']) {
      obj.result = attributes['result'] || attributes['gate.result'];
    }
    if (attributes['severity']) {
      obj.severity = attributes['severity'];
    }
  }

  // Track lifecycle stages for artifacts
  if (objectType === 'artifact' && attributes['stage']) {
    const stage = attributes['stage'];
    if (!obj.lifecycle.includes(stage)) {
      obj.lifecycle.push(stage);
    }
  }
}

/**
 * Create OCEL event from OTel span
 * @param {object} span - OTel span
 * @param {string} eventType - OCEL event type
 * @param {string} activity - Activity name
 * @param {string} objectId - Object ID
 * @param {object} attributes - Extracted attributes
 * @returns {object} OCEL event
 */
function createOCelEvent(span, eventType, activity, objectId, attributes) {
  // Convert microseconds to ISO 8601 timestamp
  const timestampUs = span.startTime || 0;
  const timestamp = new Date(timestampUs / 1000).toISOString();

  // Build event attributes (excluding OCEL-reserved keys)
  const eventAttrs = {};
  const reservedKeys = ['artifact.id', 'artifact_id', 'receipt.id', 'receipt_id',
                        'gate.id', 'gate_id', 'release.id', 'release_id',
                        'ocel.event.type', 'ocel.activity', 'stage'];
  for (const [key, value] of Object.entries(attributes)) {
    if (!reservedKeys.includes(key)) {
      eventAttrs[key] = value;
    }
  }

  // Add OTel provenance
  eventAttrs['trace_id'] = span.traceID;
  eventAttrs['span_id'] = span.spanID;
  if (span.duration !== undefined) {
    eventAttrs['duration_us'] = span.duration;
  }

  return {
    id: `event-${span.spanID}`,
    type: eventType,
    timestamp,
    object: objectId,
    activity,
    attributes: eventAttrs,
  };
}

/**
 * Post-process objects: build lifecycle arrays, validate completeness
 * @param {Object} objects - OCEL objects map
 */
function postProcessObjects(objects) {
  for (const objectId of Object.keys(objects)) {
    const obj = objects[objectId];

    // Sort lifecycle stages by defined order
    if (obj.type === 'artifact' && obj.lifecycle && obj.lifecycle.length > 0) {
      obj.lifecycle.sort((a, b) => {
        const indexA = STAGE_ORDER.indexOf(a);
        const indexB = STAGE_ORDER.indexOf(b);
        // Preserve stages not in defined order
        const orderA = indexA >= 0 ? indexA : 999;
        const orderB = indexB >= 0 ? indexB : 999;
        return orderA - orderB;
      });
    }

    // Validate required fields
    if (obj.type === 'artifact' && !obj.kind) {
      obj.kind = 'unknown'; // Default kind if not specified
    }
  }
}

/**
 * Validate OCEL log structure
 * @param {object} ocelLog - OCEL log to validate
 * @returns {{valid: boolean, errors: string[]}}
 */
export function validateOCELLog(ocelLog) {
  const errors = [];

  // Check required top-level fields
  if (!ocelLog['@context']) {
    errors.push('Missing @context');
  }
  if (!ocelLog.objects || typeof ocelLog.objects !== 'object') {
    errors.push('Missing or invalid objects field');
  }
  if (!Array.isArray(ocelLog.events)) {
    errors.push('Missing or invalid events array');
  }

  // Check each object has required fields
  for (const [id, obj] of Object.entries(ocelLog.objects || {})) {
    if (!obj.id) {
      errors.push(`Object ${id} missing id field`);
    }
    if (!obj.type) {
      errors.push(`Object ${id} missing type field`);
    }
    if (obj.type === 'artifact' && !Array.isArray(obj.lifecycle)) {
      errors.push(`Artifact ${id} missing lifecycle array`);
    }
  }

  // Check each event has required fields
  for (const [i, event] of (ocelLog.events || []).entries()) {
    if (!event.id) {
      errors.push(`Event at index ${i} missing id field`);
    }
    if (!event.type) {
      errors.push(`Event ${event.id} missing type field`);
    }
    if (!event.timestamp) {
      errors.push(`Event ${event.id} missing timestamp field`);
    }
    if (!event.object) {
      errors.push(`Event ${event.id} missing object field`);
    }
    if (!event.activity) {
      errors.push(`Event ${event.id} missing activity field`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Generate sample OTel trace data for testing
 * @returns {object} Sample OTel traces
 */
export function generateSampleOtelTraces() {
  const traceId = '4bf92f3577b34da6a3ce929d0e0e4736';
  const baseTime = Date.now() * 1000; // microseconds

  return {
    data: [
      {
        traceID: traceId,
        spans: [
          {
            traceID: traceId,
            spanID: 'span-1',
            operationName: 'operator.breed',
            startTime: baseTime,
            duration: 5000,
            process: {
              serviceName: 'manufacturing',
              tags: [],
            },
            tags: [
              { key: 'artifact.id', value: 'artifact-1', type: 'string' },
              { key: 'artifact.kind', value: 'source-code', type: 'string' },
              { key: 'stage', value: 'bred', type: 'string' },
              { key: 'operator', value: 'breed-ontology', type: 'string' },
              { key: 'status', value: 'success', type: 'string' },
            ],
          },
          {
            traceID: traceId,
            spanID: 'span-2',
            operationName: 'gate.schema-valid',
            startTime: baseTime + 10000,
            duration: 2000,
            process: {
              serviceName: 'manufacturing',
              tags: [],
            },
            tags: [
              { key: 'artifact.id', value: 'artifact-1', type: 'string' },
              { key: 'gate', value: 'schema-valid', type: 'string' },
              { key: 'result', value: 'pass', type: 'string' },
              { key: 'severity', value: 'critical', type: 'string' },
            ],
          },
          {
            traceID: traceId,
            spanID: 'span-3',
            operationName: 'stage.receipted',
            startTime: baseTime + 20000,
            duration: 1000,
            process: {
              serviceName: 'manufacturing',
              tags: [],
            },
            tags: [
              { key: 'receipt.id', value: 'receipt-1', type: 'string' },
              { key: 'receipt.hash', value: 'abc123def456', type: 'string' },
              { key: 'artifact', value: 'artifact-1', type: 'string' },
              { key: 'stage', value: 'validated', type: 'string' },
            ],
          },
        ],
      },
    ],
  };
}
