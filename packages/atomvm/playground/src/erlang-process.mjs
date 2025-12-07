/**
 * Erlang-Like Process Framework - Thin JavaScript Wrapper
 * 
 * **Architecture**: Erlang manages all state, JavaScript provides callbacks only.
 * This is a thin wrapper that communicates with Erlang processes via bridge commands.
 * 
 * **Poka-Yoke Design**:
 * - Input validation prevents invalid operations
 * - State validation delegated to Erlang
 * - Type guards ensure API consistency
 * 
 * @module erlang-process
 */

import { registerCallback, unregisterCallback } from './process-callback-registry.mjs';
import { handleSpawn, handleSend, handleLink, handleMonitor, handleExit } from './process-bridge-handler.mjs';

/**
 * Validate non-empty string (poka-yoke)
 */
function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.length === 0) {
    throw new Error(`${name} must be a non-empty string`);
  }
}

/**
 * Validate function (poka-yoke)
 */
function validateFunction(value, name) {
  if (typeof value !== 'function') {
    throw new Error(`${name} must be a function, got: ${typeof value}`);
  }
}

/**
 * Process class - thin wrapper (no state storage)
 * 
 * **Poka-Yoke**: Input validation only, state managed in Erlang
 */
export class Process {
  /**
   * @param {string} name - Process name
   * @param {string} pid - Erlang process PID (from Erlang)
   * @param {string} callbackId - JavaScript callback ID
   */
  constructor(name, pid, callbackId) {
    // Poka-yoke: Validate inputs
    validateNonEmptyString(name, 'name');
    validateNonEmptyString(pid, 'pid');
    validateNonEmptyString(callbackId, 'callbackId');
    
    this.name = name;
    this.pid = pid;
    this.callbackId = callbackId;
  }
  
  /**
   * Send message to process
   * 
   * **Poka-Yoke**: Validates message is valid object
   * 
   * @param {any} message - Message to send
   */
  send(message) {
    if (message === null || message === undefined) {
      throw new Error('Message cannot be null or undefined');
    }
    
    handleSend(this.name, message);
  }
  
  /**
   * Link to another process
   * 
   * @param {Process} targetProcess - Target process
   */
  link(targetProcess) {
    if (!(targetProcess instanceof Process)) {
      throw new Error('link target must be a Process instance');
    }
    
    handleLink(this.pid, targetProcess.pid);
  }
  
  /**
   * Monitor another process
   * 
   * @param {Process} targetProcess - Target process
   * @returns {Object} Monitor reference
   */
  monitor(targetProcess) {
    if (!(targetProcess instanceof Process)) {
      throw new Error('monitor target must be a Process instance');
    }
    
    handleMonitor(targetProcess.pid, this.pid);
    
    return { ref: `monitor_${Date.now()}`, process: targetProcess };
  }
  
  /**
   * Exit process
   * 
   * @param {string} reason - Exit reason
   */
  exit(reason = 'normal') {
    handleExit(this.pid, reason);
  }
  
  /**
   * Kill process (forceful termination)
   */
  kill() {
    this.exit('kill');
  }
  
  /**
   * Get process info (minimal - state in Erlang)
   * 
   * @returns {Object} Process info
   */
  getInfo() {
    return {
      name: this.name,
      pid: this.pid,
      callbackId: this.callbackId,
    };
  }
}

/**
 * Process Registry - tracks processes by name (minimal, state in Erlang)
 */
class ProcessRegistry {
  constructor() {
    this.processes = new Map(); // Map<name, Process>
  }
  
  /**
   * Register process
   * 
   * **Poka-Yoke**: Prevents duplicate names
   */
  register(process) {
    if (this.processes.has(process.name)) {
      throw new Error(`Process name ${process.name} already registered`);
    }
    
    this.processes.set(process.name, process);
  }
  
  /**
   * Unregister process
   */
  unregister(process) {
    this.processes.delete(process.name);
  }
  
  /**
   * Get process by name
   */
  getByName(name) {
    return this.processes.get(name);
  }
  
  /**
   * List all processes
   */
  list() {
    return Array.from(this.processes.values());
  }
}

// Global registry
const registry = new ProcessRegistry();

/**
 * Spawn a new process
 * 
 * **Poka-Yoke**: Validates inputs, registers callbacks
 * 
 * @param {string} name - Process name (must be unique)
 * @param {Function} initFn - Initialization function (async)
 * @param {Function} handleFn - Message handler function (async)
 * @param {Object} options - Process options
 * @returns {Process} Process instance
 */
export function spawn(name, initFn, handleFn, options = {}) {
  // Poka-yoke: Validate inputs
  validateNonEmptyString(name, 'name');
  validateFunction(initFn, 'initFn');
  validateFunction(handleFn, 'handleFn');
  
  // Register callbacks
  const callbackId = registerCallback(initFn, handleFn);
  
  // Send spawn command to Erlang
  handleSpawn(name, callbackId, options);
  
  // Create process wrapper (PID will be set by Erlang response)
  // For now, use callbackId as temporary PID
  const process = new Process(name, `pid_${callbackId}`, callbackId);
  registry.register(process);
  
  // TODO: Wait for Erlang to respond with actual PID
  // For now, return process immediately
  
  return process;
}

/**
 * Send message to process by name
 * 
 * @param {string} name - Process name
 * @param {any} message - Message to send
 */
export function send(name, message) {
  validateNonEmptyString(name, 'name');
  
  handleSend(name, message);
}

/**
 * Get process by name
 * 
 * @param {string} name - Process name
 * @returns {Process|null} Process instance or null
 */
export function whereis(name) {
  return registry.getByName(name);
}

/**
 * List all processes
 * 
 * @returns {Array<Process>} Array of processes
 */
export function listProcesses() {
  return registry.list();
}

/**
 * Supervisor - thin wrapper (delegates to Erlang supervisor)
 * 
 * **Poka-Yoke**: Input validation only, state managed in Erlang
 */
export class Supervisor {
  constructor(name, strategy = 'one_for_one') {
    validateNonEmptyString(name, 'name');
    
    this.name = name;
    this.strategy = strategy;
    this.children = new Map(); // Map<childName, Process> (minimal tracking)
  }
  
  /**
   * Start child process
   * 
   * **Poka-Yoke**: Validates child spec
   */
  startChild(childSpec) {
    const { name, initFn, handleFn, options = {} } = childSpec;
    
    validateNonEmptyString(name, 'childSpec.name');
    validateFunction(initFn, 'childSpec.initFn');
    validateFunction(handleFn, 'childSpec.handleFn');
    
    // Spawn child process (Erlang supervisor will manage it)
    const child = spawn(name, initFn, handleFn, options);
    
    // Track child (minimal - state in Erlang)
    this.children.set(name, child);
    
    return child;
  }
  
  /**
   * Terminate supervisor and all children
   */
  terminate() {
    // Send terminate command to Erlang supervisor
    // TODO: Implement Erlang supervisor terminate command
    
    // Unregister children
    for (const child of this.children.values()) {
      registry.unregister(child);
    }
    
    this.children.clear();
  }
}

/**
 * Export registry for testing
 */
export { registry };
