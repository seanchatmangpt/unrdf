/**
 * Process Framework Bridge Handler
 *
 * Handles PROCESS_FRAMEWORK bridge commands from JavaScript to Erlang.
 * These commands are sent via io:format and intercepted by bridge-interceptor.
 *
 * **Poka-Yoke**: Validates commands before sending to Erlang
 *
 * @module process-bridge-handler
 */

/* eslint-env browser, node */

/**
 * Validate non-empty string (poka-yoke)
 */
function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.length === 0) {
    throw new Error(`${name} must be a non-empty string`);
  }
}

/**
 * Send bridge command to Erlang
 *
 * @param {string} command - Command string
 */
function sendBridgeCommand(command) {
  // Bridge commands are sent via Module.print (intercepted by bridge-interceptor)
  // eslint-disable-next-line no-undef
  if (typeof window !== 'undefined' && window.Module && window.Module.print) {
    // eslint-disable-next-line no-undef
    window.Module.print(`PROCESS_FRAMEWORK:${command}\n`);
  } else if (typeof process !== 'undefined' && process.stdout) {
    process.stdout.write(`PROCESS_FRAMEWORK:${command}\n`);
  } else {
    console.log(`PROCESS_FRAMEWORK:${command}`);
  }
}

/**
 * Handle spawn process command
 *
 * @param {string} name - Process name
 * @param {string} callbackId - JavaScript callback ID
 * @param {Object} options - Process options
 */
export function handleSpawn(name, callbackId, options = {}) {
  validateNonEmptyString(name, 'name');
  validateNonEmptyString(callbackId, 'callbackId');

  const optionsStr = JSON.stringify(options);
  sendBridgeCommand(`spawn:${name}:${callbackId}:${optionsStr}`);
}

/**
 * Handle send message command
 *
 * @param {string} name - Process name
 * @param {any} message - Message to send
 */
export function handleSend(name, message) {
  validateNonEmptyString(name, 'name');

  const messageStr = JSON.stringify(message);
  sendBridgeCommand(`send:${name}:${messageStr}`);
}

/**
 * Handle link processes command
 *
 * @param {string} pid1 - First process name or PID
 * @param {string} pid2 - Second process name or PID
 */
export function handleLink(pid1, pid2) {
  validateNonEmptyString(pid1, 'pid1');
  validateNonEmptyString(pid2, 'pid2');

  sendBridgeCommand(`link:${pid1}:${pid2}`);
}

/**
 * Handle monitor process command
 *
 * @param {string} pid - Process name or PID to monitor
 * @param {string} monitorPid - Monitoring process name or PID
 */
export function handleMonitor(pid, monitorPid) {
  validateNonEmptyString(pid, 'pid');
  validateNonEmptyString(monitorPid, 'monitorPid');

  sendBridgeCommand(`monitor:${pid}:${monitorPid}`);
}

/**
 * Handle exit process command
 *
 * @param {string} pid - Process name or PID
 * @param {string} reason - Exit reason
 */
export function handleExit(pid, reason = 'normal') {
  validateNonEmptyString(pid, 'pid');

  sendBridgeCommand(`exit:${pid}:${reason}`);
}

/**
 * Handle callback result (from Erlang to JavaScript)
 * This is called when Erlang sends callback result back
 *
 * @param {string} _callbackId - Callback ID (unused, for future use)
 * @param {any} _result - Callback result (unused, for future use)
 */
export function handleCallbackResult(_callbackId, _result) {
  // This is handled by the Erlang process waiting for the result
  // The result is sent as a message to the Erlang process
  // This function is for future use if we need to handle results in JS
}
