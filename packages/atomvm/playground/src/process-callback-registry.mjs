/**
 * Process Callback Registry
 * 
 * Manages JavaScript callbacks that Erlang processes can invoke.
 * Erlang processes reference callbacks by ID, and this registry
 * maps IDs to actual JavaScript functions.
 * 
 * **Poka-Yoke**: Validates callbacks before registration
 * 
 * @module process-callback-registry
 */

// Callback registry: Map<callbackId, { initFn, handleFn }>
const callbacks = new Map();

/**
 * Generate unique callback ID
 * @returns {string} Unique callback ID
 */
function generateCallbackId() {
  return `cb-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`;
}

/**
 * Validate function (poka-yoke)
 * @param {Function} value - Value to validate
 * @param {string} name - Parameter name for error message
 * @throws {Error} If value is not a function
 */
function validateFunction(value, name) {
  if (typeof value !== 'function') {
    throw new Error(`${name} must be a function, got: ${typeof value}`);
  }
}

/**
 * Register callbacks for a process
 * 
 * **Poka-Yoke**: Validates callbacks are functions
 * 
 * @param {Function} initFn - Initialization function (async)
 * @param {Function} handleFn - Message handler function (async)
 * @returns {string} Callback ID
 * @throws {Error} If callbacks are invalid
 */
export function registerCallback(initFn, handleFn) {
  // Poka-yoke: Validate callbacks
  validateFunction(initFn, 'initFn');
  validateFunction(handleFn, 'handleFn');
  
  const callbackId = generateCallbackId();
  callbacks.set(callbackId, { initFn, handleFn });
  
  return callbackId;
}

/**
 * Invoke init callback
 * 
 * **Poka-Yoke**: Validates callback ID exists
 * 
 * @param {string} callbackId - Callback ID
 * @returns {Promise<any>} Init result
 * @throws {Error} If callback ID not found
 */
export async function invokeInit(callbackId) {
  if (typeof callbackId !== 'string' || callbackId.length === 0) {
    throw new Error(`callbackId must be a non-empty string, got: ${typeof callbackId}`);
  }
  
  const callback = callbacks.get(callbackId);
  if (!callback) {
    throw new Error(`Callback not found: ${callbackId}`);
  }
  
  const { initFn } = callback;
  
  try {
    const result = await initFn();
    return result;
  } catch (error) {
    throw new Error(`Init callback failed: ${error.message}`);
  }
}

/**
 * Invoke message handler callback
 * 
 * **Poka-Yoke**: Validates callback ID exists and message is valid
 * 
 * @param {string} callbackId - Callback ID
 * @param {any} message - Message to handle
 * @returns {Promise<any>} Handle result
 * @throws {Error} If callback ID not found
 */
export async function invokeHandle(callbackId, message) {
  if (typeof callbackId !== 'string' || callbackId.length === 0) {
    throw new Error(`callbackId must be a non-empty string, got: ${typeof callbackId}`);
  }
  
  const callback = callbacks.get(callbackId);
  if (!callback) {
    throw new Error(`Callback not found: ${callbackId}`);
  }
  
  const { handleFn } = callback;
  
  try {
    const result = await handleFn(message);
    return result;
  } catch (error) {
    throw new Error(`Handle callback failed: ${error.message}`);
  }
}

/**
 * Unregister callback
 * 
 * @param {string} callbackId - Callback ID
 */
export function unregisterCallback(callbackId) {
  callbacks.delete(callbackId);
}

/**
 * Get callback info (for debugging)
 * 
 * @param {string} callbackId - Callback ID
 * @returns {Object|null} Callback info or null if not found
 */
export function getCallbackInfo(callbackId) {
  const callback = callbacks.get(callbackId);
  if (!callback) {
    return null;
  }
  
  return {
    callbackId,
    hasInitFn: typeof callback.initFn === 'function',
    hasHandleFn: typeof callback.handleFn === 'function',
  };
}

/**
 * List all registered callbacks (for debugging)
 * 
 * @returns {Array<string>} Array of callback IDs
 */
export function listCallbacks() {
  return Array.from(callbacks.keys());
}

/**
 * Clear all callbacks (for testing)
 */
export function clearCallbacks() {
  callbacks.clear();
}

