/**
 * @file Avro Schema Generation from YAWL Event Schemas
 * @module @unrdf/yawl-kafka/schemas
 *
 * @description
 * Converts YAWL Zod schemas to Avro schemas for Kafka serialization.
 * Provides schema registry integration and versioning support.
 */

import avro from 'avsc';

// =============================================================================
// Avro Schema Definitions (Derived from YAWL Event Schemas)
// =============================================================================

/**
 * Receipt Avro schema - cryptographic proof of state transition
 * Matches YAWL ReceiptSchema from yawl-events.mjs
 */
export const ReceiptAvroSchema = {
  type: 'record',
  name: 'YawlReceipt',
  namespace: 'io.yawl.events',
  doc: 'Cryptographic receipt with BLAKE3 hashes for workflow state transitions',
  fields: [
    { name: 'beforeHash', type: 'string', doc: 'BLAKE3 hash (64 hex chars) of state before operation' },
    { name: 'afterHash', type: 'string', doc: 'BLAKE3 hash (64 hex chars) of state after operation' },
    { name: 'hash', type: 'string', doc: 'BLAKE3 hash (64 hex chars) of decision/event data' },
    {
      name: 'justification',
      type: {
        type: 'record',
        name: 'Justification',
        fields: [
          { name: 'hookValidated', type: ['null', 'string'], default: null },
          { name: 'conditionChecked', type: ['null', 'string'], default: null },
          { name: 'sparqlQuery', type: ['null', 'string'], default: null },
          { name: 'reasoning', type: ['null', 'string'], default: null },
        ]
      }
    },
    { name: 'gitRef', type: ['null', 'string'], default: null },
    { name: 't_ns', type: 'string', doc: 'Nanosecond timestamp' },
    { name: 'timestamp_iso', type: 'string', doc: 'ISO 8601 timestamp' },
  ]
};

/**
 * CASE_CREATED event Avro schema
 */
export const CaseCreatedAvroSchema = {
  type: 'record',
  name: 'YawlCaseCreated',
  namespace: 'io.yawl.events',
  doc: 'Event emitted when a new workflow case is created',
  fields: [
    { name: 'caseId', type: 'string', doc: 'UUID of the workflow case' },
    { name: 'specId', type: 'string', doc: 'Workflow specification identifier' },
    { name: 'timestamp', type: 'string', doc: 'ISO 8601 creation timestamp' },
    { name: 'receipt', type: 'io.yawl.events.YawlReceipt' },
  ]
};

/**
 * TASK_ENABLED event Avro schema
 */
export const TaskEnabledAvroSchema = {
  type: 'record',
  name: 'YawlTaskEnabled',
  namespace: 'io.yawl.events',
  doc: 'Event emitted when a task becomes enabled (ready for execution)',
  fields: [
    { name: 'taskId', type: 'string', doc: 'Task definition identifier' },
    { name: 'caseId', type: 'string', doc: 'UUID of the workflow case' },
    { name: 'workItemId', type: 'string', doc: 'UUID of the work item instance' },
    { name: 'enabledAt', type: 'string', doc: 'ISO 8601 enablement timestamp' },
    { name: 'receipt', type: 'io.yawl.events.YawlReceipt' },
  ]
};

/**
 * TASK_STARTED event Avro schema
 */
export const TaskStartedAvroSchema = {
  type: 'record',
  name: 'YawlTaskStarted',
  namespace: 'io.yawl.events',
  doc: 'Event emitted when a work item execution starts',
  fields: [
    { name: 'workItemId', type: 'string', doc: 'UUID of the work item' },
    { name: 'startedAt', type: 'string', doc: 'ISO 8601 start timestamp' },
    { name: 'receipt', type: 'io.yawl.events.YawlReceipt' },
  ]
};

/**
 * TASK_COMPLETED event Avro schema
 */
export const TaskCompletedAvroSchema = {
  type: 'record',
  name: 'YawlTaskCompleted',
  namespace: 'io.yawl.events',
  doc: 'Event emitted when a work item execution completes',
  fields: [
    { name: 'workItemId', type: 'string', doc: 'UUID of the work item' },
    { name: 'completedAt', type: 'string', doc: 'ISO 8601 completion timestamp' },
    { name: 'result', type: ['null', 'string'], default: null, doc: 'Task result (JSON string)' },
    { name: 'receipt', type: 'io.yawl.events.YawlReceipt' },
  ]
};

/**
 * TASK_CANCELLED event Avro schema
 */
export const TaskCancelledAvroSchema = {
  type: 'record',
  name: 'YawlTaskCancelled',
  namespace: 'io.yawl.events',
  doc: 'Event emitted when a work item is cancelled',
  fields: [
    { name: 'workItemId', type: 'string', doc: 'UUID of the work item' },
    { name: 'cancelledAt', type: 'string', doc: 'ISO 8601 cancellation timestamp' },
    { name: 'reason', type: 'string', doc: 'Cancellation reason' },
    { name: 'receipt', type: 'io.yawl.events.YawlReceipt' },
  ]
};

/**
 * CONTROL_FLOW_EVALUATED event Avro schema
 */
export const ControlFlowEvaluatedAvroSchema = {
  type: 'record',
  name: 'YawlControlFlowEvaluated',
  namespace: 'io.yawl.events',
  doc: 'Event emitted when control flow routing is evaluated',
  fields: [
    { name: 'caseId', type: 'string', doc: 'UUID of the workflow case' },
    { name: 'taskId', type: 'string', doc: 'Task being evaluated' },
    { name: 'result', type: 'boolean', doc: 'Evaluation result (enabled or blocked)' },
    { name: 'timestamp', type: 'string', doc: 'ISO 8601 evaluation timestamp' },
    { name: 'sparqlQuery', type: 'string', doc: 'SPARQL query used for evaluation' },
    { name: 'receipt', type: 'io.yawl.events.YawlReceipt' },
  ]
};

// =============================================================================
// Avro Type Registry
// =============================================================================

/**
 * Map event types to Avro schemas
 * @type {Map<string, Object>}
 */
export const EVENT_SCHEMA_MAP = new Map([
  ['YAWL_CASE_CREATED', CaseCreatedAvroSchema],
  ['YAWL_TASK_ENABLED', TaskEnabledAvroSchema],
  ['YAWL_TASK_STARTED', TaskStartedAvroSchema],
  ['YAWL_TASK_COMPLETED', TaskCompletedAvroSchema],
  ['YAWL_TASK_CANCELLED', TaskCancelledAvroSchema],
  ['YAWL_CONTROL_FLOW_EVALUATED', ControlFlowEvaluatedAvroSchema],
]);

/**
 * Get compiled Avro type for an event type
 *
 * @param {string} eventType - YAWL event type
 * @returns {avro.Type} Compiled Avro type
 * @throws {Error} If event type has no schema
 *
 * @example
 * const type = getAvroType('YAWL_CASE_CREATED');
 * const buffer = type.toBuffer(eventData);
 */
export function getAvroType(eventType) {
  const schema = EVENT_SCHEMA_MAP.get(eventType);
  if (!schema) {
    throw new Error(`No Avro schema found for event type: ${eventType}`);
  }
  return avro.Type.forSchema(schema);
}

/**
 * Get Avro schema JSON for an event type
 *
 * @param {string} eventType - YAWL event type
 * @returns {Object} Avro schema object
 * @throws {Error} If event type has no schema
 */
export function getAvroSchema(eventType) {
  const schema = EVENT_SCHEMA_MAP.get(eventType);
  if (!schema) {
    throw new Error(`No Avro schema found for event type: ${eventType}`);
  }
  return schema;
}

/**
 * Serialize event data to Avro buffer
 *
 * @param {string} eventType - YAWL event type
 * @param {Object} eventData - Event payload
 * @returns {Buffer} Avro-encoded buffer
 *
 * @example
 * const buffer = serializeEvent('YAWL_CASE_CREATED', {
 *   caseId: 'uuid-here',
 *   specId: 'approval-workflow',
 *   timestamp: '2024-01-01T00:00:00Z',
 *   receipt: { ... }
 * });
 */
export function serializeEvent(eventType, eventData) {
  const type = getAvroType(eventType);

  // Normalize result field for TASK_COMPLETED (must be string or null)
  if (eventType === 'YAWL_TASK_COMPLETED' && eventData.result !== undefined && eventData.result !== null) {
    eventData = {
      ...eventData,
      result: typeof eventData.result === 'string' ? eventData.result : JSON.stringify(eventData.result)
    };
  }

  return type.toBuffer(eventData);
}

/**
 * Deserialize Avro buffer to event data
 *
 * @param {string} eventType - YAWL event type
 * @param {Buffer} buffer - Avro-encoded buffer
 * @returns {Object} Deserialized event data
 *
 * @example
 * const eventData = deserializeEvent('YAWL_CASE_CREATED', buffer);
 * console.log(eventData.caseId);
 */
export function deserializeEvent(eventType, buffer) {
  const type = getAvroType(eventType);
  return type.fromBuffer(buffer);
}

// =============================================================================
// Schema Evolution Support
// =============================================================================

/**
 * Schema version metadata for evolution tracking
 *
 * @typedef {Object} SchemaVersion
 * @property {string} version - Semantic version (e.g., '1.0.0')
 * @property {string} eventType - YAWL event type
 * @property {Object} schema - Avro schema object
 * @property {string[]} compatibleWith - List of compatible versions
 */

/**
 * Schema registry for version management
 * Supports forward and backward compatibility
 */
export class SchemaRegistry {
  /**
   * Create a new Schema Registry
   */
  constructor() {
    /** @type {Map<string, Map<string, SchemaVersion>>} */
    this.schemas = new Map();
    this._initializeDefaultSchemas();
  }

  /**
   * Initialize default schemas (v1.0.0)
   * @private
   */
  _initializeDefaultSchemas() {
    for (const [eventType, schema] of EVENT_SCHEMA_MAP.entries()) {
      this.registerSchema(eventType, '1.0.0', schema, []);
    }
  }

  /**
   * Register a schema version
   *
   * @param {string} eventType - YAWL event type
   * @param {string} version - Semantic version
   * @param {Object} schema - Avro schema object
   * @param {string[]} compatibleWith - Compatible versions
   */
  registerSchema(eventType, version, schema, compatibleWith = []) {
    if (!this.schemas.has(eventType)) {
      this.schemas.set(eventType, new Map());
    }

    this.schemas.get(eventType).set(version, {
      version,
      eventType,
      schema,
      compatibleWith,
    });
  }

  /**
   * Get schema for specific version
   *
   * @param {string} eventType - YAWL event type
   * @param {string} version - Semantic version
   * @returns {Object} Avro schema
   * @throws {Error} If schema version not found
   */
  getSchema(eventType, version) {
    const versions = this.schemas.get(eventType);
    if (!versions) {
      throw new Error(`No schemas registered for event type: ${eventType}`);
    }

    const schemaVersion = versions.get(version);
    if (!schemaVersion) {
      throw new Error(`Schema version ${version} not found for ${eventType}`);
    }

    return schemaVersion.schema;
  }

  /**
   * Get latest schema version
   *
   * @param {string} eventType - YAWL event type
   * @returns {SchemaVersion} Latest schema version
   */
  getLatestVersion(eventType) {
    const versions = this.schemas.get(eventType);
    if (!versions || versions.size === 0) {
      throw new Error(`No schemas registered for event type: ${eventType}`);
    }

    // Get latest by semantic version sorting
    const sortedVersions = [...versions.values()].sort((a, b) => {
      return b.version.localeCompare(a.version, undefined, { numeric: true });
    });

    return sortedVersions[0];
  }

  /**
   * Check compatibility between versions
   *
   * @param {string} eventType - YAWL event type
   * @param {string} fromVersion - Source version
   * @param {string} toVersion - Target version
   * @returns {boolean} True if compatible
   */
  isCompatible(eventType, fromVersion, toVersion) {
    if (fromVersion === toVersion) return true;

    const fromSchema = this.schemas.get(eventType)?.get(fromVersion);
    if (!fromSchema) return false;

    return fromSchema.compatibleWith.includes(toVersion);
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  // Schemas
  ReceiptAvroSchema,
  CaseCreatedAvroSchema,
  TaskEnabledAvroSchema,
  TaskStartedAvroSchema,
  TaskCompletedAvroSchema,
  TaskCancelledAvroSchema,
  ControlFlowEvaluatedAvroSchema,

  // Registry
  EVENT_SCHEMA_MAP,
  SchemaRegistry,

  // Functions
  getAvroType,
  getAvroSchema,
  serializeEvent,
  deserializeEvent,
};
