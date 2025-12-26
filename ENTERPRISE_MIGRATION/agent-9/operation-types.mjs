/**
 * @file Operation type definitions for substrate operations
 * @module agent-9/operation-types
 */

/**
 * @typedef {'INSERT' | 'UPDATE' | 'DELETE' | 'QUERY'} OperationType
 */

/**
 * Operation types
 * @enum {string}
 */
export const OperationType = {
  INSERT: 'INSERT',
  UPDATE: 'UPDATE',
  DELETE: 'DELETE',
  QUERY: 'QUERY'
};

/**
 * @typedef {Object} OperationPayload
 * @property {string} [subject] - Subject IRI
 * @property {string} [predicate] - Predicate IRI
 * @property {string} [object] - Object literal or IRI
 * @property {string} [graph] - Named graph IRI
 * @property {string} [query] - SPARQL query string
 * @property {*} [data] - Additional operation data
 */

/**
 * @typedef {Object} Operation
 * @property {string} type - Operation type
 * @property {OperationPayload} payload - Operation payload
 * @property {string} id - Unique operation ID
 * @property {number} timestamp - Operation timestamp
 */

/**
 * Create a typed operation
 * @param {OperationType} type - Operation type
 * @param {OperationPayload} payload - Operation payload
 * @returns {Operation} Created operation
 */
export function createOperation(type, payload) {
  if (!Object.values(OperationType).includes(type)) {
    throw new Error(`Invalid operation type: ${type}`);
  }

  return {
    type,
    payload: { ...payload },
    id: generateOperationId(),
    timestamp: Date.now()
  };
}

/**
 * Validate operation structure
 * @param {Operation} op - Operation to validate
 * @returns {boolean} True if valid
 * @throws {Error} If operation is invalid
 */
export function validateOperation(op) {
  if (!op || typeof op !== 'object') {
    throw new Error('Operation must be an object');
  }

  if (!op.type || !Object.values(OperationType).includes(op.type)) {
    throw new Error(`Invalid operation type: ${op.type}`);
  }

  if (!op.payload || typeof op.payload !== 'object') {
    throw new Error('Operation must have a payload object');
  }

  if (!op.id || typeof op.id !== 'string') {
    throw new Error('Operation must have an id');
  }

  if (!op.timestamp || typeof op.timestamp !== 'number') {
    throw new Error('Operation must have a timestamp');
  }

  // Type-specific validation
  switch (op.type) {
    case OperationType.INSERT:
    case OperationType.DELETE:
      if (!op.payload.subject || !op.payload.predicate || !op.payload.object) {
        throw new Error(`${op.type} operation requires subject, predicate, and object`);
      }
      break;

    case OperationType.UPDATE:
      if (!op.payload.subject || !op.payload.predicate) {
        throw new Error('UPDATE operation requires subject and predicate');
      }
      break;

    case OperationType.QUERY:
      if (!op.payload.query || typeof op.payload.query !== 'string') {
        throw new Error('QUERY operation requires a query string');
      }
      break;

    default:
      throw new Error(`Unknown operation type: ${op.type}`);
  }

  return true;
}

/**
 * Generate unique operation ID
 * @returns {string} Unique ID
 */
function generateOperationId() {
  return `op-${Date.now()}-${Math.random().toString(36).substring(2, 11)}`;
}

/**
 * Batch validate operations
 * @param {Operation[]} operations - Operations to validate
 * @returns {boolean} True if all valid
 * @throws {Error} If any operation is invalid
 */
export function validateOperations(operations) {
  if (!Array.isArray(operations)) {
    throw new Error('Operations must be an array');
  }

  operations.forEach((op, index) => {
    try {
      validateOperation(op);
    } catch (error) {
      throw new Error(`Invalid operation at index ${index}: ${error.message}`);
    }
  });

  return true;
}

/**
 * Check if value is a valid operation
 * @param {*} value - Value to check
 * @returns {boolean} True if valid operation
 */
export function isOperation(value) {
  try {
    validateOperation(value);
    return true;
  } catch {
    return false;
  }
}
