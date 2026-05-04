/**
 * KGC Multiverse - Worker Task Module
 * Defines tasks that run in piscina worker threads
 *
 * @module @unrdf/kgc-multiverse/worker-task
 */

import { blake3 } from 'hash-wasm';

/**
 * Task types for worker execution
 * @readonly
 * @enum {string}
 */
export const TaskType = {
  CREATE_UNIVERSE: 'CREATE_UNIVERSE',
  APPLY_MORPHISM: 'APPLY_MORPHISM',
  GENERATE_RECEIPT: 'GENERATE_RECEIPT',
  FREEZE_UNIVERSE: 'FREEZE_UNIVERSE',
  COMPUTE_HASH: 'COMPUTE_HASH',
};

/**
 * Generate Q* Identifier (worker-safe version)
 * Creates a unique Q* identifier without shared state
 *
 * @param {Object} [options={}] - Configuration options
 * @returns {Promise<Object>} Q* identifier object
 */
async function generateQStarIDWorker(options = {}) {
  const timestamp = BigInt(Date.now()) * 1_000_000n;
  const randomBytes = crypto.getRandomValues(new Uint8Array(8));
  const randomHex = Array.from(randomBytes, b => b.toString(16).padStart(2, '0')).join('');
  const timestampHex = timestamp.toString(16).padStart(16, '0').slice(-16);

  const combined = timestampHex + randomHex;
  const hash = await blake3(combined);
  const qidSuffix = hash.slice(0, 16);

  const Q_ID = `Q*_${qidSuffix}`;
  const Q_RDF = `http://kgc.io/multiverse/${qidSuffix}`;
  const Q_PROV = {
    createdAt: timestamp,
    ...(options.createdBy && { createdBy: options.createdBy }),
    ...(options.parentID && { parentID: options.parentID }),
    ...(options.batchIndex !== undefined && { batchIndex: options.batchIndex }),
  };

  return { Q_ID, Q_RDF, Q_PROV };
}

/**
 * Create universe task
 * Creates a new universe with Q* identifier
 *
 * @param {Object} params - Task parameters
 * @param {number} params.batchIndex - Index in batch
 * @param {string} [params.createdBy] - Creator identifier
 * @returns {Promise<Object>} Created universe
 */
async function createUniverseTask(params) {
  const { batchIndex, createdBy } = params;

  const qid = await generateQStarIDWorker({
    createdBy,
    batchIndex,
  });

  return {
    id: qid,
    state: 'GENESIS',
    eventCount: 0,
    batchIndex,
    metadata: {},
  };
}

/**
 * Apply morphism task
 * Applies a morphism transformation to universe data
 *
 * @param {Object} params - Task parameters
 * @param {Object} params.universe - Universe to transform
 * @param {Object} params.morphismConfig - Morphism configuration
 * @returns {Promise<Object>} Transformation result with deltas
 */
async function applyMorphismTask(params) {
  const { universe, morphismConfig } = params;

  // Simulate morphism application
  const deltas = [];

  // Create sample deltas based on morphism type
  if (morphismConfig.type === 'SCHEMA') {
    deltas.push({
      type: 'add',
      subject: `http://kgc.io/entity/${universe.id.Q_ID.slice(3)}`,
      predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      object: {
        type: 'NamedNode',
        value: 'http://kgc.io/types/TransformedEntity',
      },
    });
  }

  // Handle BigInt serialization
  const serialized = JSON.stringify({ universe, deltas }, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
  const hash = await blake3(serialized);

  return {
    universeID: universe.id.Q_ID,
    morphismID: morphismConfig.id,
    deltas,
    deltaCount: deltas.length,
    resultHash: hash,
    timestamp: BigInt(Date.now()) * 1_000_000n,
  };
}

/**
 * Generate receipt task
 * Creates a cryptographic receipt for an operation
 *
 * @param {Object} params - Task parameters
 * @param {string} params.universeID - Universe Q* ID
 * @param {Array<Object>} params.operations - Operations to include
 * @param {string} params.operationType - Type of operation
 * @returns {Promise<Object>} Generated receipt
 */
async function generateReceiptTask(params) {
  const { universeID, operations, operationType, batchIndex } = params;

  const timestamp = BigInt(Date.now()) * 1_000_000n;

  // Compute content hash
  const serialized = JSON.stringify(operations, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
  const contentHash = await blake3(serialized);

  // Generate receipt ID
  const receiptIDInput = `receipt-${universeID}-${timestamp}`;
  const receiptHash = await blake3(receiptIDInput);
  const Q_ID = `Q*_${receiptHash.slice(0, 16)}`;
  const Q_RDF = `http://kgc.io/receipts/${receiptHash.slice(0, 16)}`;

  return {
    Q_ID,
    Q_RDF,
    Q_PROV: {
      timestamp,
      batchSize: operations.length,
      operationType,
      universeID,
      contentHash,
      batchIndex,
    },
  };
}

/**
 * Freeze universe task
 * Creates an immutable snapshot of universe state
 *
 * @param {Object} params - Task parameters
 * @param {Object} params.universe - Universe to freeze
 * @param {Array<Object>} [params.quads] - Optional quads to include in hash
 * @returns {Promise<Object>} Frozen universe with hash
 */
async function freezeUniverseTask(params) {
  const { universe, quads = [] } = params;

  // Compute universe hash from state + quads
  const stateData = {
    id: universe.id,
    eventCount: universe.eventCount,
    quads: quads.length,
    timestamp: BigInt(Date.now()) * 1_000_000n,
  };

  const serialized = JSON.stringify(stateData, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
  const universeHash = await blake3(serialized);

  return {
    ...universe,
    state: 'FROZEN',
    universeHash,
    frozenAt: BigInt(Date.now()) * 1_000_000n,
  };
}

/**
 * Compute hash task
 * Generic hash computation for arbitrary data
 *
 * @param {Object} params - Task parameters
 * @param {*} params.data - Data to hash
 * @returns {Promise<Object>} Hash result
 */
async function computeHashTask(params) {
  const { data, index } = params;

  const serialized = typeof data === 'string'
    ? data
    : JSON.stringify(data, (key, value) =>
        typeof value === 'bigint' ? value.toString() : value
      );

  const hash = await blake3(serialized);

  return {
    hash,
    index,
    dataSize: serialized.length,
  };
}

/**
 * Main worker function - dispatches to appropriate task handler
 * This is the entry point called by piscina
 *
 * @param {Object} task - Task configuration
 * @param {string} task.type - Task type (from TaskType enum)
 * @param {Object} task.params - Task-specific parameters
 * @returns {Promise<Object>} Task result
 */
export default async function workerTask(task) {
  const { type, params } = task;

  switch (type) {
    case TaskType.CREATE_UNIVERSE:
      return createUniverseTask(params);

    case TaskType.APPLY_MORPHISM:
      return applyMorphismTask(params);

    case TaskType.GENERATE_RECEIPT:
      return generateReceiptTask(params);

    case TaskType.FREEZE_UNIVERSE:
      return freezeUniverseTask(params);

    case TaskType.COMPUTE_HASH:
      return computeHashTask(params);

    default:
      throw new Error(`Unknown task type: ${type}`);
  }
}

// Export task type and individual task functions for testing
export {
  generateQStarIDWorker,
  createUniverseTask,
  applyMorphismTask,
  generateReceiptTask,
  freezeUniverseTask,
  computeHashTask,
};
