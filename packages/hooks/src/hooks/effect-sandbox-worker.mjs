/**
 * @file Worker thread implementation for effect sandbox
 * @module effect-sandbox-worker
 *
 * @description
 * Worker thread implementation for secure hook effect execution.
 * This file runs in a separate worker thread to isolate hook execution.
 */

import { parentPort, workerData, isMainThread } from 'worker_threads';

/**
 * Worker thread entry point
 */
if (!isMainThread) {
  const { effect, context, executionId, config, _options } = workerData;

  try {
    // Create safe execution environment
    const safeGlobals = createSafeGlobals(context, config);

    // Create safe effect function
    const safeEffect = createSafeEffect(effect, safeGlobals);

    // Execute effect with timeout
    const result = await executeWithTimeout(safeEffect, context, config.timeout);

    // Send result back to main thread
    parentPort.postMessage({
      success: true,
      result: result.result,
      assertions: result.assertions || [],
      events: result.events || [],
      executionId,
    });
  } catch (error) {
    // Send error back to main thread
    parentPort.postMessage({
      success: false,
      error: error.message,
      executionId,
    });
  }
}

/**
 * Create safe globals for worker execution
 * @param {Object} context - Execution context
 * @param {Object} config - Sandbox configuration
 * @returns {Object} Safe globals
 */
function createSafeGlobals(context, config) {
  const globals = {};

  // Add context data
  globals.event = context.event;
  globals.store = context.store;
  globals.delta = context.delta;
  globals.metadata = context.metadata || {};

  // Add safe console
  globals.console = {
    log: message => console.log(`[Worker] ${message}`),
    warn: message => console.warn(`[Worker] ${message}`),
    error: message => console.error(`[Worker] ${message}`),
    info: message => console.info(`[Worker] ${message}`),
  };

  // Add safe functions
  globals.emitEvent = eventData => {
    if (!context.events) context.events = [];
    context.events.push(eventData);
  };

  globals.log = (message, level = 'info') => {
    console.log(`[Sandbox ${level.toUpperCase()}] ${message}`);
  };

  globals.assert = (subject, predicate, object, graph) => {
    if (!context.assertions) context.assertions = [];
    context.assertions.push({ subject, predicate, object, graph });
  };

  // Add allowed globals
  for (const globalName of config.allowedGlobals || []) {
    if (globalName === 'Date') globals.Date = Date;
    if (globalName === 'Math') globals.Math = Math;
    if (globalName === 'JSON') globals.JSON = JSON;
  }

  return globals;
}

/**
 * Create safe effect function
 * @param {string} effectCode - Effect function code
 * @param {Object} safeGlobals - Safe globals
 * @returns {Function} Safe effect function
 */
function createSafeEffect(effectCode, safeGlobals) {
  // Create function with safe globals
  const safeFunction = new Function(
    ...Object.keys(safeGlobals),
    `
    "use strict";
    
    // Override dangerous globals
    const process = undefined;
    const require = undefined;
    const module = undefined;
    const exports = undefined;
    const _dirname = undefined;
    const __filename = undefined;
    
    // Create effect function
    const effect = ${effectCode};
    
    // Return wrapper that captures assertions and events
    return function(context) {
      const result = effect(context);
      
      return {
        result,
        assertions: context.assertions || [],
        events: context.events || []
      };
    };
    `
  );

  return safeFunction(...Object.values(safeGlobals));
}

/**
 * Execute function with timeout
 * @param {Function} fn - Function to execute
 * @param {Object} context - Execution context
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object>} Execution result
 */
async function executeWithTimeout(fn, context, timeout) {
  return new Promise((resolve, reject) => {
    const timeoutId = setTimeout(() => {
      reject(new Error(`Execution timeout after ${timeout}ms`));
    }, timeout);

    try {
      const result = fn(context);

      // Handle both sync and async results
      if (result && typeof result.then === 'function') {
        result
          .then(res => {
            clearTimeout(timeoutId);
            resolve(res);
          })
          .catch(err => {
            clearTimeout(timeoutId);
            reject(err);
          });
      } else {
        clearTimeout(timeoutId);
        resolve(result);
      }
    } catch (error) {
      clearTimeout(timeoutId);
      reject(error);
    }
  });
}
