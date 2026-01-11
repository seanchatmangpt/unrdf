/**
 * @file v6 DeltaGate State Management
 * @module @unrdf/daemon/integrations/v6-deltagate-state
 * @description State capture and history management
 */

/**
 * Capture current state from store
 * @param {Map} store - State store
 * @returns {Object} Captured state
 */
export function captureState(store) {
  const state = {};
  for (const [key, value] of store.entries()) {
    state[key] = value;
  }
  return state;
}

/**
 * Store delta and receipt in history
 * @param {Array} deltaHistory - Delta history array
 * @param {Array} receiptHistory - Receipt history array
 * @param {Map} stateHistory - State history map
 * @param {Object} delta - Delta to store
 * @param {Object} receipt - Receipt to store
 * @param {Object} oldState - State before delta
 * @param {Object} newState - State after delta
 * @param {number} maxHistorySize - Maximum history size
 * @returns {void}
 */
export function storeHistory(
  deltaHistory,
  receiptHistory,
  stateHistory,
  delta,
  receipt,
  oldState,
  newState,
  maxHistorySize
) {
  deltaHistory.push(delta);
  receiptHistory.push(receipt);
  stateHistory.set(receipt.id, { oldState, newState });

  // Trim history if needed
  if (deltaHistory.length > maxHistorySize) {
    deltaHistory.shift();
    receiptHistory.shift();
  }
}
