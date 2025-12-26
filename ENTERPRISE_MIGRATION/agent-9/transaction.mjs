/**
 * @file Transaction support for substrate operations
 * @module agent-9/transaction
 */

/**
 * @typedef {Object} Transaction
 * @property {string} id - Transaction ID
 * @property {Map<string, *>} originalTriples - Original triples before transaction
 * @property {Map<string, *>} originalIndexes - Original indexes before transaction
 * @property {Array<import('./operation-types.mjs').Operation>} operations - Operations in transaction
 * @property {number} startTime - Transaction start timestamp
 * @property {'active' | 'committed' | 'rolled_back'} status - Transaction status
 * @property {import('./substrate-store.mjs').Substrate} substrate - Associated substrate
 */

/**
 * Begin a transaction on substrate
 * @param {import('./substrate-store.mjs').Substrate} substrate - Target substrate
 * @returns {Transaction} New transaction
 * @throws {Error} If substrate already has active transaction or is frozen
 */
export function beginTransaction(substrate) {
  if (substrate.frozen) {
    throw new Error('Cannot begin transaction on frozen substrate');
  }

  if (substrate.transaction) {
    throw new Error('Substrate already has active transaction');
  }

  const transaction = {
    id: generateTransactionId(),
    originalTriples: new Map(substrate.triples),
    originalIndexes: cloneIndexes(substrate.indexes),
    operations: [],
    startTime: Date.now(),
    status: 'active',
    substrate
  };

  substrate.transaction = transaction;

  return transaction;
}

/**
 * Commit transaction
 * @param {Transaction} tx - Transaction to commit
 * @returns {Object} Commit result
 * @throws {Error} If transaction is not active
 */
export function commit(tx) {
  if (tx.status !== 'active') {
    throw new Error(`Cannot commit transaction with status: ${tx.status}`);
  }

  if (!tx.substrate.transaction || tx.substrate.transaction.id !== tx.id) {
    throw new Error('Transaction is not associated with substrate');
  }

  // Mark as committed
  tx.status = 'committed';
  tx.substrate.transaction = null;

  return {
    success: true,
    transactionId: tx.id,
    operationCount: tx.operations.length,
    duration: Date.now() - tx.startTime
  };
}

/**
 * Rollback transaction
 * @param {Transaction} tx - Transaction to rollback
 * @returns {Object} Rollback result
 * @throws {Error} If transaction is not active
 */
export function rollback(tx) {
  if (tx.status !== 'active') {
    throw new Error(`Cannot rollback transaction with status: ${tx.status}`);
  }

  if (!tx.substrate.transaction || tx.substrate.transaction.id !== tx.id) {
    throw new Error('Transaction is not associated with substrate');
  }

  // Restore original state
  tx.substrate.triples = tx.originalTriples;
  tx.substrate.indexes = tx.originalIndexes;

  // Mark as rolled back
  tx.status = 'rolled_back';
  tx.substrate.transaction = null;

  return {
    success: true,
    transactionId: tx.id,
    operationCount: tx.operations.length,
    duration: Date.now() - tx.startTime
  };
}

/**
 * Check if substrate is in transaction
 * @param {import('./substrate-store.mjs').Substrate} substrate - Substrate to check
 * @returns {boolean} True if in transaction
 */
export function isInTransaction(substrate) {
  return substrate.transaction !== null && substrate.transaction.status === 'active';
}

/**
 * Get current transaction
 * @param {import('./substrate-store.mjs').Substrate} substrate - Substrate to check
 * @returns {Transaction|null} Current transaction or null
 */
export function getCurrentTransaction(substrate) {
  return substrate.transaction;
}

/**
 * Add operation to transaction
 * @param {Transaction} tx - Transaction
 * @param {import('./operation-types.mjs').Operation} operation - Operation to add
 * @throws {Error} If transaction is not active
 */
export function addOperation(tx, operation) {
  if (tx.status !== 'active') {
    throw new Error(`Cannot add operation to transaction with status: ${tx.status}`);
  }

  tx.operations.push(operation);
}

/**
 * Get transaction statistics
 * @param {Transaction} tx - Transaction
 * @returns {Object} Statistics
 */
export function getTransactionStats(tx) {
  return {
    id: tx.id,
    status: tx.status,
    operationCount: tx.operations.length,
    duration: tx.status === 'active' ? Date.now() - tx.startTime : null,
    startTime: tx.startTime
  };
}

/**
 * Execute function within transaction
 * @param {import('./substrate-store.mjs').Substrate} substrate - Target substrate
 * @param {Function} fn - Function to execute
 * @returns {*} Result of function
 * @throws {Error} If function throws, transaction is rolled back
 */
export async function withTransaction(substrate, fn) {
  const tx = beginTransaction(substrate);

  try {
    const result = await fn(tx);
    commit(tx);
    return result;
  } catch (error) {
    rollback(tx);
    throw error;
  }
}

// --- Internal helpers ---

/**
 * Generate unique transaction ID
 * @returns {string} Unique ID
 */
function generateTransactionId() {
  return `tx-${Date.now()}-${Math.random().toString(36).substring(2, 11)}`;
}

/**
 * Clone indexes
 * @param {Map} indexes - Indexes to clone
 * @returns {Map} Cloned indexes
 */
function cloneIndexes(indexes) {
  const cloned = new Map();
  for (const [key, index] of indexes) {
    const clonedIndex = new Map();
    for (const [k, set] of index) {
      clonedIndex.set(k, new Set(set));
    }
    cloned.set(key, clonedIndex);
  }
  return cloned;
}
