/**
 * Hook Primitive Bridge - JavaScript Execution Engine
 *
 * **Kernel Architecture**: This is the pluggable execution engine.
 * The kernel (`hook_primitives.erl`) sends commands here.
 * We execute hooks and send results back.
 *
 * **Big Bang 80/20**: Start with trivial passthrough, progressively enhance.
 *
 * @module hook-primitive-bridge
 */

import { KnowledgeHookManager } from '@unrdf/hooks';

/**
 * Hook Primitive Bridge
 *
 * Handles HOOK_PRIMITIVE: commands from Erlang kernel.
 * Executes hooks and sends results back via sendToErlang callback.
 */
export class HookPrimitiveBridge {
  /**
   * @param {Object} [options] - Bridge options
   * @param {Function} [options.log] - Logging function
   * @param {Function} [options.sendToErlang] - Function to send messages to Erlang
   * @param {boolean} [options.includeBuiltins=false] - Include built-in hooks
   */
  constructor(options = {}) {
    this.log = options.log || console.log;
    this.sendToErlang = options.sendToErlang || (() => {
      this.log('[Bridge] Warning: sendToErlang not configured');
    });
    
    // Initialize hook manager
    this.manager = new KnowledgeHookManager({
      includeBuiltins: options.includeBuiltins || false
    });
    
    // Hook metadata registry (from Erlang)
    // Maps: hook name -> {name, trigger, hasValidate, hasTransform}
    this.hookMetadata = new Map();
    
    // Compiled chains cache
    this.compiledChains = new Map();
    
    this.log('Hook Primitive Bridge initialized');
  }
  
  /**
   * Handle a line from Erlang (via io:format)
   *
   * @param {string} line - Line from Erlang (may contain HOOK_PRIMITIVE: command)
   */
  onLine(line) {
    if (!line || typeof line !== 'string') {
      return;
    }
    
    // Check if this is a HOOK_PRIMITIVE command
    if (!line.startsWith('HOOK_PRIMITIVE:')) {
      return;
    }
    
    // Remove prefix and split
    const commandStr = line.substring('HOOK_PRIMITIVE:'.length).trim();
    const parts = commandStr.split(':');
    
    if (parts.length === 0) {
      this.log(`[Bridge] Invalid HOOK_PRIMITIVE command: ${line}`);
      return;
    }
    
    const command = parts[0];
    const args = parts.slice(1);
    
    try {
      switch (command) {
        case 'register':
          this.handleRegister(args.join(':'));
          break;
        case 'execute':
          this.handleExecute(args);
          break;
        case 'chain':
          this.handleChain(args);
          break;
        case 'execute_chain':
          this.handleExecuteChain(args);
          break;
        default:
          this.log(`[Bridge] Unknown HOOK_PRIMITIVE command: ${command}`);
      }
    } catch (error) {
      this.log(`[Bridge] Error processing command ${command}: ${error.message}`);
    }
  }
  
  /**
   * Handle register command
   * Format: HOOK_PRIMITIVE:register:<HookDefJSON>
   *
   * @param {string} hookDefJSON - JSON string with hook definition
   */
  handleRegister(hookDefJSON) {
    try {
      const hookDef = JSON.parse(hookDefJSON);
      
      // Poka-yoke: Validate hook definition
      if (!hookDef.name || !hookDef.trigger) {
        this.log(`[Bridge] Invalid hook definition: missing name or trigger`);
        return;
      }
      
      // Store metadata (we don't have the actual functions from Erlang)
      this.hookMetadata.set(hookDef.name, hookDef);
      
      this.log(`[Bridge] Hook metadata registered: ${hookDef.name} (trigger: ${hookDef.trigger})`);
      
      // **Big Bang 80/20**: For now, we just store metadata
      // Hook execution is passthrough (returns {valid: true, data} unchanged)
      // This is expected for the initial implementation - real hook execution
      // will be added in the next iteration when we wire KnowledgeHookManager
    } catch (parseError) {
      this.log(`[Bridge] Failed to parse hook definition JSON: ${parseError.message}`);
    }
  }
  
  /**
   * Handle execute command
   * Format: HOOK_PRIMITIVE:execute:<Trigger>:<DataJSON>:<ChainKey>:<RequestId>
   *
   * @param {Array<string>} args - Command arguments
   */
  handleExecute(args) {
    if (args.length < 4) {
      this.log(`[Bridge] Invalid execute command: requires trigger, data, chainKey, requestId`);
      return;
    }
    
    const trigger = args[0];
    const dataJSON = args.slice(1, -2).join(':'); // Data may contain colons
    const chainKey = args[args.length - 2];
    const requestId = args[args.length - 1]; // Request ID for matching
    
    try {
      const data = JSON.parse(dataJSON);
      
      // **Big Bang 80/20**: Start with trivial passthrough
      // Later: actually execute hooks using manager.executeHooksByTrigger()
      const result = {
        valid: true,
        data: data
      };
      
      // Send result back to Erlang with request ID
      this.sendToErlang({
        type: 'hook_primitive_result',
        requestId, // Use request ID for matching
        result
      });
      
      this.log(`[Bridge] Hook execution completed: requestId=${requestId} (passthrough)`);
    } catch (parseError) {
      this.log(`[Bridge] Failed to parse data JSON: ${parseError.message}`);
      
      // Send error result
      this.sendToErlang({
        type: 'hook_primitive_result',
        chainKey,
        result: {
          valid: false,
          data: {},
          errors: `JSON parse error: ${parseError.message}`
        }
      });
    }
  }
  
  /**
   * Handle chain command
   * Format: HOOK_PRIMITIVE:chain:<ChainKey>:<HookNamesJSON>
   *
   * @param {Array<string>} args - Command arguments
   */
  handleChain(args) {
    if (args.length < 2) {
      this.log(`[Bridge] Invalid chain command: requires chainKey and hookNames`);
      return;
    }
    
    const chainKey = args[0];
    const hookNamesJSON = args.slice(1).join(':'); // May contain colons
    
    try {
      const hookNames = JSON.parse(hookNamesJSON);
      
      // Store chain metadata
      this.compiledChains.set(chainKey, {
        hookNames,
        compiled: false // Will be compiled on first execution
      });
      
      this.log(`[Bridge] Chain registered: ${chainKey} with ${hookNames.length} hooks`);
      
      // For now, we just store metadata
      // Later: pre-compile chain using compileHookChain()
    } catch (parseError) {
      this.log(`[Bridge] Failed to parse hook names JSON: ${parseError.message}`);
    }
  }
  
  /**
   * Handle execute_chain command
   * Format: HOOK_PRIMITIVE:execute_chain:<ChainKey>:<DataJSON>
   *
   * @param {Array<string>} args - Command arguments
   */
  handleExecuteChain(args) {
    if (args.length < 2) {
      this.log(`[Bridge] Invalid execute_chain command: requires chainKey and data`);
      return;
    }
    
    const chainKey = args[0];
    const dataJSON = args.slice(1).join(':'); // Data may contain colons
    
    try {
      const data = JSON.parse(dataJSON);
      
      // Check if chain exists
      if (!this.compiledChains.has(chainKey)) {
        this.log(`[Bridge] Chain not found: ${chainKey}`);
        this.sendToErlang({
          type: 'hook_primitive_result',
          requestId: chainKey, // Temporary: use chainKey as requestId
          result: {
            valid: false,
            data: {},
            errors: `Chain not found: ${chainKey}`
          }
        });
        return;
      }
      
      // **Big Bang 80/20**: Start with trivial passthrough
      // Later: actually execute compiled chain
      const result = {
        valid: true,
        data: data
      };
      
      // Send result back to Erlang
      this.sendToErlang({
        type: 'hook_primitive_result',
        chainKey,
        result
      });
      
      this.log(`[Bridge] Chain execution completed: ${chainKey} (passthrough)`);
    } catch (parseError) {
      this.log(`[Bridge] Failed to parse data JSON: ${parseError.message}`);
      
      // Send error result
      this.sendToErlang({
        type: 'hook_primitive_result',
        chainKey,
        result: {
          valid: false,
          data: {},
          errors: `JSON parse error: ${parseError.message}`
        }
      });
    }
  }
}

/**
 * Create a global bridge instance
 */
let globalBridge = null;

/**
 * Get or create global bridge instance
 *
 * @param {Object} [options] - Bridge options
 * @returns {HookPrimitiveBridge} Bridge instance
 */
export function getHookPrimitiveBridge(options = {}) {
  if (!globalBridge) {
    globalBridge = new HookPrimitiveBridge(options);
  }
  return globalBridge;
}

/**
 * Set global bridge instance (for testing)
 *
 * @param {HookPrimitiveBridge} bridge - Bridge instance
 */
export function setHookPrimitiveBridge(bridge) {
  globalBridge = bridge;
}

