/**
 * Bridge Interceptor for Erlang → JavaScript Communication
 *
 * Intercepts AtomVM Module.print/printErr output to parse special markers
 * and call KGC-4D bridge methods. This enables Erlang processes to communicate
 * with JavaScript KGC-4D and hooks APIs.
 *
 * @module bridge-interceptor
 */

import { getBridge } from './kgc4d-bridge.mjs';
import { getHookPrimitiveBridge } from './hook-primitive-bridge.mjs';
import { getGenStatemBridge } from './gen-statem-bridge.mjs';
import { trace } from '@opentelemetry/api';
import { startRoundtrip, endRoundtrip, getSLAStats, OPERATION_TYPES } from '../../src/roundtrip-sla.mjs';
import { invokeInit, invokeHandle } from './process-callback-registry.mjs';

// Get tracer lazily to ensure provider is registered first
function getTracer() {
  return trace.getTracer('atomvm-interceptor');
}

/**
 * Intercept AtomVM Module output and route to bridge
 *
 * @param {Object} Module - AtomVM Module object
 * @param {Function} log - Logging function
 */
export function interceptAtomVMOutput(Module, log = console.log) {
  const bridge = getBridge({ log });
  
  // Pending hook results: chainKey -> {result, helperPid}
  // **Big Bang 80/20**: Store results here, helper process retrieves them
  const pendingHookResults = new Map();
  
  // Active helper processes: chainKey -> helperPid
  // Used to send results to the correct helper process
  const activeHelpers = new Map();
  
  /**
   * Send result to Erlang
   * For now, we store results and Erlang can check for them
   * Later: implement proper message passing mechanism
   *
   * @param {Object} message - Message to send
   * @param {string} message.type - Message type (must be 'hook_primitive_result')
   * @param {string} message.chainKey - Chain key
   * @param {Object} message.result - Result map {valid, data, errors}
   */
  function sendToErlang(message) {
    if (message.type === 'hook_primitive_result') {
      // Store result for Erlang to retrieve
      const requestId = message.requestId || message.chainKey; // Use requestId if available
      pendingHookResults.set(requestId, message.result);
      
      // **Big Bang 80/20**: Send result via command that Erlang can parse
      // Format: HOOK_PRIMITIVE_RESULT:<RequestId>:<ResultJSON>
      // Erlang will parse this and store in ETS table
      const resultJSON = JSON.stringify(message.result);
      const command = `HOOK_PRIMITIVE_RESULT:${requestId}:${resultJSON}`;
      
      // Send via Module.print (will be intercepted)
      if (typeof window !== 'undefined' && window.Module && window.Module.print) {
        window.Module.print(`${command}\n`);
      } else if (typeof process !== 'undefined' && process.stdout) {
        process.stdout.write(`${command}\n`);
      } else {
        console.log(command);
      }
      
      log(`[Bridge] Hook result sent to Erlang: requestId=${requestId}`);
    }
  }
  
  // Initialize hook primitive bridge
  const hookBridge = getHookPrimitiveBridge({
    log,
    sendToErlang
  });
  
  // Initialize gen_statem bridge
  const genStatemBridge = getGenStatemBridge({
    log,
    sendCommand: (command) => {
      // Send command to Erlang via Module.print
      if (typeof window !== 'undefined' && window.Module && window.Module.print) {
        window.Module.print(`${command}\n`);
      } else if (typeof process !== 'undefined' && process.stdout) {
        process.stdout.write(`${command}\n`);
      }
    }
  });
  
  // Store original print functions (may have been set by AtomVMRuntime)
  const originalPrint = Module.print || ((text) => {
    if (typeof console !== 'undefined' && console.log) {
      console.log(text);
    }
  });
  const originalPrintErr = Module.printErr || ((text) => {
    if (typeof console !== 'undefined' && console.error) {
      console.error(text);
    }
  });
  
  /**
   * **Poka-Yoke**: Validate bridge command format
   * @param {string} text - Command text to validate
   * @returns {boolean} True if text is a valid string
   */
  function validateCommandText(text) {
    if (typeof text !== 'string' || text.length === 0) {
      return false;
    }
    return true;
  }
  
  /**
   * **Poka-Yoke**: Validate command arguments
   * @param {string} command - Command name
   * @param {string} argsStr - Arguments string
   * @returns {boolean} True if arguments are valid for the command
   */
  function validateCommandArgs(command, argsStr) {
    switch (command) {
      case 'emit_event':
        // Must have at least type (argsStr should have at least one colon-separated part)
        if (!argsStr || argsStr.trim().length === 0) {
          log(`[Bridge] Poka-Yoke: emit_event requires type argument`);
          return false;
        }
        return true;
      case 'register_hook':
        // Must have at least name (argsStr should have at least one colon-separated part)
        if (!argsStr || argsStr.trim().length === 0) {
          log(`[Bridge] Poka-Yoke: register_hook requires name argument`);
          return false;
        }
        return true;
      case 'process_intent':
        // Must have at least intentId (argsStr should have at least one colon-separated part)
        if (!argsStr || argsStr.trim().length === 0) {
          log(`[Bridge] Poka-Yoke: process_intent requires intentId argument`);
          return false;
        }
        return true;
      case 'get_outcome':
        // Must have intentId
        if (!argsStr || argsStr.trim().length === 0) {
          log(`[Bridge] Poka-Yoke: get_outcome requires intentId argument`);
          return false;
        }
        return true;
      default:
        // Unknown commands are allowed (may be handled elsewhere)
        return true;
    }
  }
  
  /**
   * Parse and handle bridge commands from Erlang output
   * **Poka-Yoke**: Validates command format and arguments before processing
   */
  function handleBridgeCommand(text) {
    // Poka-Yoke: Validate command text
    if (!validateCommandText(text)) {
      return false;
    }
    
    // Parse JS_CALLBACK commands (Erlang → JavaScript)
    // Format: JS_CALLBACK:type:callbackId:pid:args
    // pid is the Erlang process PID waiting for the result
    const jsCallbackMatch = text.match(/^JS_CALLBACK:(\w+):([^:]+):([^:]+):(.*)$/);
    if (jsCallbackMatch) {
      const [, type, callbackId, erlangPid, argsStr] = jsCallbackMatch;
      // Poka-Yoke: Validate callback parameters
      if (!type || !callbackId || !erlangPid) {
        log(`[Bridge] Poka-Yoke: Invalid JS_CALLBACK format - missing required parameters`);
        return false;
      }
      handleJSCallback(type, callbackId, erlangPid, argsStr, log);
      return true;
    }
    
    // Also support old format without PID (for backward compatibility)
    const jsCallbackMatchOld = text.match(/^JS_CALLBACK:(\w+):([^:]+):(.*)$/);
    if (jsCallbackMatchOld) {
      const [, type, callbackId, argsStr] = jsCallbackMatchOld;
      // Poka-Yoke: Validate callback parameters
      if (!type || !callbackId) {
        log(`[Bridge] Poka-Yoke: Invalid JS_CALLBACK format (old) - missing required parameters`);
        return false;
      }
      // Use callbackId as PID fallback (not ideal, but works)
      handleJSCallback(type, callbackId, callbackId, argsStr, log);
      return true;
    }
    
    // Parse PROCESS_FRAMEWORK commands (JavaScript → Erlang)
    // Format: PROCESS_FRAMEWORK:command:args
    const processFrameworkMatch = text.match(/^PROCESS_FRAMEWORK:(\w+):(.*)$/);
    if (processFrameworkMatch) {
      const [, command, argsStr] = processFrameworkMatch;
      
      // Handle callback results/errors (JS → Erlang)
      // These are sent from JavaScript after callback completion
      // The Erlang process will receive them via its message loop
      if (command === 'callback_result' || command === 'callback_error') {
        // Format: PROCESS_FRAMEWORK:callback_result:callbackId:erlangPid:result
        // Format: PROCESS_FRAMEWORK:callback_error:callbackId:erlangPid:error
        // The erlangPid is used by Erlang to route the message
        log(`[Bridge] Callback ${command} routed to Erlang`);
      } else {
        // Other PROCESS_FRAMEWORK commands (spawn, send, link, etc.)
        // These are sent from JS to Erlang, just log for now
        log(`[Bridge] Process framework command: ${command}`);
      }
      
      return true;
    }
    
    // Parse HOOK_PRIMITIVE commands (Erlang hook primitives)
    // Format: HOOK_PRIMITIVE:command:arg1:arg2:...
    const hookPrimitiveMatch = text.match(/^HOOK_PRIMITIVE:(\w+):(.*)$/);
    if (hookPrimitiveMatch) {
      // Use hook primitive bridge to handle command
      hookBridge.onLine(text);
      return true;
    }
    
    // Parse HOOK_PRIMITIVE_RESULT (from JS to Erlang - workaround for message passing)
    // Format: HOOK_PRIMITIVE_RESULT:<ChainKey>:<ResultJSON>
    // **Big Bang 80/20**: Store result and notify Erlang via a special command
    // Erlang will need to check for results (this is a limitation of AtomVM message passing)
    const hookResultMatch = text.match(/^HOOK_PRIMITIVE_RESULT:(.+?):(.*)$/);
    if (hookResultMatch) {
      const [, chainKey, resultJSON] = hookResultMatch;
      
      try {
        const result = JSON.parse(resultJSON);
        
        // Store result for Erlang to retrieve
        pendingHookResults.set(chainKey, result);
        
        // **Big Bang 80/20 Workaround**: AtomVM doesn't support direct JS→Erlang messages
        // We use a command that Erlang can receive and convert to a message
        // Format: HOOK_RESULT_READY:<ChainKey>
        // Erlang will poll for this and retrieve the result
        const notifyCommand = `HOOK_RESULT_READY:${chainKey}`;
        if (typeof window !== 'undefined' && window.Module && window.Module.print) {
          window.Module.print(`${notifyCommand}\n`);
        } else if (typeof process !== 'undefined' && process.stdout) {
          process.stdout.write(`${notifyCommand}\n`);
        }
        
        log(`[Bridge] Hook result stored and Erlang notified: ${chainKey}`);
      } catch (parseError) {
        log(`[Bridge] Failed to parse hook result JSON: ${parseError.message}`);
      }
      
      return true;
    }
    
    // Parse GEN_STATEM commands (JavaScript → Erlang state machine control)
    // Format: GEN_STATEM:command:arg1:arg2:...
    const genStatemMatch = text.match(/^GEN_STATEM:(\w+):(.*)$/);
    if (genStatemMatch) {
      const [, command, argsStr] = genStatemMatch;
      handleGenStatemCommand(command, argsStr, log);
      return true;
    }
    
    // Parse GEN_STATEM bridge commands (JS → Erlang, sent via Module.print)
    // Format: GEN_STATEM_BUTTON:<Digit>
    // Format: GEN_STATEM_GET_STATE
    // Format: GEN_STATEM_STOP
    // Note: These commands are sent to Erlang via Module.print, but Erlang can't intercept
    // its own io:format output. For now, we just log them - the state machine would need
    // to poll for these commands or use a different mechanism.
    const genStatemButtonMatch = text.match(/^GEN_STATEM_BUTTON:(.+)$/);
    if (genStatemButtonMatch) {
      const [, digitStr] = genStatemButtonMatch;
      log(`[Bridge] State machine button command: ${digitStr} (Erlang will handle)`);
      return true;
    }
    
    const genStatemGetStateMatch = text.match(/^GEN_STATEM_GET_STATE$/);
    if (genStatemGetStateMatch) {
      log(`[Bridge] State machine get_state command (Erlang will handle)`);
      return true;
    }
    
    const genStatemStopMatch = text.match(/^GEN_STATEM_STOP$/);
    if (genStatemStopMatch) {
      log(`[Bridge] State machine stop command (Erlang will handle)`);
      return true;
    }
    
    // Parse GEN_STATEM response commands (Erlang → JavaScript)
    // Format: GEN_STATEM_STATE:<State>:<DataJSON>
    // DataJSON is a JSON object with buttons, attempts, lockout_until
    const genStatemStateMatch = text.match(/^GEN_STATEM_STATE:(.+?):(.*)$/);
    if (genStatemStateMatch) {
      const [, state, dataStr] = genStatemStateMatch;
      
      // Parse data (JSON object or legacy list format)
      let data = null;
      try {
        // Try parsing as JSON first
        data = JSON.parse(dataStr);
      } catch (e) {
        // Fallback: legacy list format [1,2,3]
        try {
          const cleaned = dataStr.trim().replace(/^\[|\]$/g, '');
          if (cleaned.length > 0) {
            const buttons = cleaned.split(',').map(b => parseInt(b.trim(), 10)).filter(n => !isNaN(n));
            data = { buttons };
          } else {
            data = { buttons: [] };
          }
        } catch (e2) {
          log(`[Bridge] Failed to parse state data: ${dataStr}`);
          data = { buttons: [] };
        }
      }
      
      // Update bridge cache
      genStatemBridge.updateState(state, data);
      
      log(`[Bridge] State machine state: ${state}, data: ${JSON.stringify(data)}`);
      return true;
    }
    
    // Parse KGC4D_BRIDGE commands
    // Format: KGC4D_BRIDGE:command:arg1:arg2:...
    const bridgeMatch = text.match(/^KGC4D_BRIDGE:(\w+):(.*)$/);
    if (!bridgeMatch) {
      return false;
    }
    
    const [, command, argsStr] = bridgeMatch;
    
    // Poka-Yoke: Validate command arguments
    if (!validateCommandArgs(command, argsStr)) {
      log(`[Bridge] Poka-Yoke: Invalid arguments for command ${command}`);
      return false;
    }
    
    try {
      switch (command) {
        case 'emit_event': {
          // Format: KGC4D_BRIDGE:emit_event:Type:Payload
          const [type, payloadStr] = argsStr.split(':', 2);
          
          // Poka-Yoke: Validate event type
          if (!type || type.trim().length === 0) {
            log(`[Bridge] Poka-Yoke: emit_event requires non-empty type`);
            return false;
          }
          
          let payload = {};
          if (payloadStr) {
            try {
              // Try to parse as JSON first
              payload = JSON.parse(payloadStr);
              // Poka-Yoke: Validate payload is an object
              if (typeof payload !== 'object' || payload === null || Array.isArray(payload)) {
                log(`[Bridge] Poka-Yoke: emit_event payload must be an object, got: ${typeof payload}`);
                payload = { raw: payloadStr };
              }
            } catch (parseError) {
              // If not JSON, treat as string
              log(`[Bridge] Poka-Yoke: emit_event payload JSON parse failed: ${parseError.message}, treating as string`);
              payload = { raw: payloadStr };
            }
          }
          
          // Start roundtrip tracking for Erlang→JS call
          const roundtripId = startRoundtrip(OPERATION_TYPES.EMIT_EVENT);
          
          // Create real OTEL span for event emission
          trace.getTracer('atomvm-interceptor').startActiveSpan('erlang.process.emit_event', {
            attributes: {
              'event.type': type,
              'event.source': 'erlang-process',
              'service.name': 'atomvm-playground',
              'operation.type': 'erlang.process',
              'roundtrip.operation_id': roundtripId,
              'roundtrip.operation_type': OPERATION_TYPES.EMIT_EVENT,
            },
          }, async (span) => {
            try {
              const result = await bridge.emitEvent(type, payload);
              
              // End roundtrip tracking (bridge method already tracks its portion)
              // This tracks the Erlang→JS portion
              const roundtripResult = endRoundtrip(roundtripId, result.success, result.error);
              span.setAttribute('roundtrip.latency', roundtripResult.latency);
              span.setAttribute('roundtrip.sla_met', roundtripResult.slaMet);
              
              // Add current error rate to span
              const slaStats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);
              span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
              if (result.success) {
                span.setAttribute('event.id', result.receipt?.id || '');
                span.setAttribute('event.success', true);
                span.setStatus({ code: 1 }); // OK
                log(`[Bridge] Event emitted: ${type} (id: ${result.receipt.id})`);
              } else {
                span.setAttribute('event.success', false);
                span.setAttribute('event.error', result.error || '');
                span.setStatus({ code: 2, message: result.error }); // ERROR
                log(`[Bridge] Event emission failed: ${result.error}`);
              }
            } catch (error) {
              span.recordException(error);
              span.setStatus({ code: 2, message: error.message }); // ERROR
              log(`[Bridge] Event emission error: ${error.message}`);
            } finally {
              span.end();
            }
          });
          return true;
        }
        
        case 'register_hook': {
          // Format: KGC4D_BRIDGE:register_hook:name:trigger
          const [name, trigger] = argsStr.split(':');
          
          // Poka-Yoke: Validate hook name
          if (!name || name.trim().length === 0) {
            log(`[Bridge] Poka-Yoke: register_hook requires non-empty name`);
            return false;
          }
          
          // Start roundtrip tracking for Erlang→JS call
          const roundtripId = startRoundtrip(OPERATION_TYPES.REGISTER_HOOK);
          
          // Create real OTEL span for hook registration
          trace.getTracer('atomvm-interceptor').startActiveSpan('erlang.process.register_hook', {
            attributes: {
              'hook.name': name,
              'hook.trigger': trigger || 'before-add',
              'service.name': 'atomvm-playground',
              'operation.type': 'erlang.process',
              'roundtrip.operation_id': roundtripId,
              'roundtrip.operation_type': OPERATION_TYPES.REGISTER_HOOK,
            },
          }, (span) => {
            try {
              const result = bridge.registerHook({
                name,
                trigger: trigger || 'before-add',
                validate: () => true, // Default: always valid (80/20)
                transform: undefined
              });
              
              // End roundtrip tracking
              const roundtripResult = endRoundtrip(roundtripId, result.success, result.error);
              span.setAttribute('roundtrip.latency', roundtripResult.latency);
              span.setAttribute('roundtrip.sla_met', roundtripResult.slaMet);
              
              // Add current error rate to span
              const slaStats = getSLAStats(OPERATION_TYPES.REGISTER_HOOK);
              span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
              
              if (result.success) {
                span.setAttribute('hook.success', true);
                span.setStatus({ code: 1 }); // OK
                log(`[Bridge] Hook registered: ${name}`);
              } else {
                span.setAttribute('hook.success', false);
                span.setAttribute('hook.error', result.error || '');
                span.setStatus({ code: 2, message: result.error }); // ERROR
                log(`[Bridge] Hook registration failed: ${result.error}`);
              }
            } catch (error) {
              span.recordException(error);
              span.setStatus({ code: 2, message: error.message }); // ERROR
              log(`[Bridge] Hook registration error: ${error.message}`);
            } finally {
              span.end();
            }
          });
          return true;
        }
        
        case 'process_intent': {
          // Format: KGC4D_BRIDGE:process_intent:intentId:IntentJSON
          const [intentId, intentStr] = argsStr.split(':', 2);
          
          // Poka-Yoke: Validate intent ID
          if (!intentId || intentId.trim().length === 0) {
            log(`[Bridge] Poka-Yoke: process_intent requires non-empty intentId`);
            return false;
          }
          
          let intent = {};
          if (intentStr) {
            try {
              intent = JSON.parse(intentStr);
              // Poka-Yoke: Validate intent is an object
              if (typeof intent !== 'object' || intent === null || Array.isArray(intent)) {
                log(`[Bridge] Poka-Yoke: process_intent intent must be an object, got: ${typeof intent}`);
                intent = { description: intentStr };
              }
            } catch (parseError) {
              // If not JSON, treat as description string
              log(`[Bridge] Poka-Yoke: process_intent intent JSON parse failed: ${parseError.message}, treating as description`);
              intent = { description: intentStr };
            }
          }
          
          // Start roundtrip tracking for Erlang→JS call
          const roundtripId = startRoundtrip(OPERATION_TYPES.PROCESS_INTENT);
          
          // Create real OTEL span for intent processing
          trace.getTracer('atomvm-interceptor').startActiveSpan('erlang.process.intent', {
            attributes: {
              'intent.id': intentId,
              'service.name': 'atomvm-playground',
              'operation.type': 'erlang.process',
              'roundtrip.operation_id': roundtripId,
              'roundtrip.operation_type': OPERATION_TYPES.PROCESS_INTENT,
            },
          }, async (span) => {
            try {
              span.setAttribute('intent.description', intent.description || '');
              
              const result = await bridge.processIntent(intentId, intent);
              
              // End roundtrip tracking
              const roundtripResult = endRoundtrip(roundtripId, result.success, result.error);
              span.setAttribute('roundtrip.latency', roundtripResult.latency);
              span.setAttribute('roundtrip.sla_met', roundtripResult.slaMet);
              
              // Add current error rate to span
              const slaStats = getSLAStats(OPERATION_TYPES.PROCESS_INTENT);
              span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
              if (result.success) {
                span.setAttribute('intent.success', true);
                span.setAttribute('intent.accepted', result.outcome?.accepted || false);
                span.setStatus({ code: 1 }); // OK
                log(`[Bridge] Intent processed: ${intentId} → ${result.outcome?.accepted ? 'accepted' : 'rejected'}`);
              } else {
                span.setAttribute('intent.success', false);
                span.setAttribute('intent.error', result.error || '');
                span.setStatus({ code: 2, message: result.error }); // ERROR
                log(`[Bridge] Intent processing failed: ${result.error}`);
              }
            } catch (error) {
              span.recordException(error);
              span.setStatus({ code: 2, message: error.message }); // ERROR
              log(`[Bridge] Intent processing error: ${error.message}`);
            } finally {
              span.end();
            }
          });
          return true;
        }
        
        case 'get_outcome': {
          // Format: KGC4D_BRIDGE:get_outcome:intentId
          const intentId = argsStr;
          const outcome = bridge.getIntentOutcome(intentId);
          
          if (outcome) {
            log(`[Bridge] Outcome for ${intentId}: ${JSON.stringify(outcome.outcome)}`);
          } else {
            log(`[Bridge] No outcome found for ${intentId}`);
          }
          return true;
        }
        
        default:
          log(`[Bridge] Unknown command: ${command}`);
          return false;
      }
    } catch (error) {
      const errorMessage = error?.message || String(error);
      log(`[Bridge] Error processing command ${command}: ${errorMessage}`);
      return false;
    }
  }
  
  // Intercept print (wrap existing function to preserve AtomVMRuntime's terminal logging)
  const existingPrint = Module.print;
  Module.print = (text) => {
    // Try to handle as bridge command first
    const handled = handleBridgeCommand(text);
    
    // Always call original print to preserve AtomVMRuntime's terminal logging
    // This ensures both bridge commands AND normal output work
    if (existingPrint && existingPrint !== Module.print) {
      existingPrint(text);
    } else {
      originalPrint(text);
    }
  };
  
  // Intercept printErr (wrap existing function to preserve AtomVMRuntime's terminal logging)
  const existingPrintErr = Module.printErr;
  Module.printErr = (text) => {
    // Try to handle as bridge command first
    const handled = handleBridgeCommand(text);
    
    // Always call original printErr to preserve AtomVMRuntime's terminal logging
    if (existingPrintErr && existingPrintErr !== Module.printErr) {
      existingPrintErr(text);
    } else {
      originalPrintErr(text);
    }
  };
  
  // Callback-to-process mapping: callbackId → erlangPid
  const callbackToProcessMap = new Map();
  
  /**
   * Handle JavaScript callback invocation from Erlang
   * @param {string} type - Callback type (init or handle)
   * @param {string} callbackId - Callback ID
   * @param {string} erlangPid - Erlang process PID waiting for result
   * @param {string} argsStr - Arguments string
   * @param {Function} log - Logging function
   */
  function handleJSCallback(type, callbackId, erlangPid, argsStr, log) {
    // Store mapping for result routing
    callbackToProcessMap.set(callbackId, erlangPid);
    if (type === 'init') {
      // Invoke init callback
      invokeInit(callbackId)
        .then(result => {
          // Send result back to Erlang process via bridge command
          log(`[Bridge] Init callback ${callbackId} completed, routing to ${erlangPid}`);
          // Send result via Module.print (will be intercepted and sent to Erlang)
          const resultStr = JSON.stringify(result);
          const command = `PROCESS_FRAMEWORK:callback_result:${callbackId}:${erlangPid}:${resultStr}`;
          if (typeof window !== 'undefined' && window.Module && window.Module.print) {
            window.Module.print(`${command}\n`);
          } else if (typeof process !== 'undefined' && process.stdout) {
            process.stdout.write(`${command}\n`);
          } else {
            console.log(command);
          }
          // Clean up mapping
          callbackToProcessMap.delete(callbackId);
        })
        .catch(error => {
          log(`[Bridge] Init callback ${callbackId} failed: ${error.message}`);
          const errorStr = JSON.stringify(error.message);
          const command = `PROCESS_FRAMEWORK:callback_error:${callbackId}:${erlangPid}:${errorStr}`;
          if (typeof window !== 'undefined' && window.Module && window.Module.print) {
            window.Module.print(`${command}\n`);
          } else if (typeof process !== 'undefined' && process.stdout) {
            process.stdout.write(`${command}\n`);
          } else {
            console.log(command);
          }
          // Clean up mapping
          callbackToProcessMap.delete(callbackId);
        });
    } else if (type === 'handle') {
      // Invoke handle callback
      // Args: message (encoded)
      let message;
      try {
        // Try to parse as JSON first, then as Erlang term
        message = JSON.parse(argsStr);
      } catch {
        // Not JSON, treat as string
        message = argsStr;
      }
      
      invokeHandle(callbackId, message)
        .then(result => {
          log(`[Bridge] Handle callback ${callbackId} completed, routing to ${erlangPid}`);
          // Send result back to Erlang process
          const resultStr = JSON.stringify(result);
          const command = `PROCESS_FRAMEWORK:callback_result:${callbackId}:${erlangPid}:${resultStr}`;
          if (typeof window !== 'undefined' && window.Module && window.Module.print) {
            window.Module.print(`${command}\n`);
          } else if (typeof process !== 'undefined' && process.stdout) {
            process.stdout.write(`${command}\n`);
          } else {
            console.log(command);
          }
          // Clean up mapping
          callbackToProcessMap.delete(callbackId);
        })
        .catch(error => {
          log(`[Bridge] Handle callback ${callbackId} failed: ${error.message}`);
          const errorStr = JSON.stringify(error.message);
          const command = `PROCESS_FRAMEWORK:callback_error:${callbackId}:${erlangPid}:${errorStr}`;
          if (typeof window !== 'undefined' && window.Module && window.Module.print) {
            window.Module.print(`${command}\n`);
          } else if (typeof process !== 'undefined' && process.stdout) {
            process.stdout.write(`${command}\n`);
          } else {
            console.log(command);
          }
          // Clean up mapping
          callbackToProcessMap.delete(callbackId);
        });
    }
  }
  
  // Note: handleHookPrimitiveCommand is now handled by hookBridge.onLine()
  // This function is kept for backward compatibility but is no longer used
  
  /**
   * Handle GEN_STATEM commands (JavaScript → Erlang state machine)
   * @param {string} command - Command name
   * @param {string} argsStr - Arguments string
   * @param {Function} log - Logging function
   */
  function handleGenStatemCommand(command, argsStr, log) {
    try {
      switch (command) {
        case 'button': {
          // Format: GEN_STATEM:button:<Digit>
          const digit = parseInt(argsStr, 10);
          if (isNaN(digit)) {
            log(`[Bridge] Invalid button digit: ${argsStr}`);
            return;
          }
          
          // Send button event to Erlang state machine
          // Format: GEN_STATEM_BUTTON:<Digit>
          const buttonCommand = `GEN_STATEM_BUTTON:${digit}`;
          if (typeof window !== 'undefined' && window.Module && window.Module.print) {
            window.Module.print(`${buttonCommand}\n`);
          } else if (typeof process !== 'undefined' && process.stdout) {
            process.stdout.write(`${buttonCommand}\n`);
          }
          
          log(`[Bridge] State machine button pressed: ${digit}`);
          break;
        }
        
        case 'get_state': {
          // Format: GEN_STATEM:get_state
          // Request state from Erlang state machine
          const stateCommand = `GEN_STATEM_GET_STATE`;
          if (typeof window !== 'undefined' && window.Module && window.Module.print) {
            window.Module.print(`${stateCommand}\n`);
          } else if (typeof process !== 'undefined' && process.stdout) {
            process.stdout.write(`${stateCommand}\n`);
          }
          
          log(`[Bridge] State machine state requested`);
          break;
        }
        
        case 'stop': {
          // Format: GEN_STATEM:stop
          const stopCommand = `GEN_STATEM_STOP`;
          if (typeof window !== 'undefined' && window.Module && window.Module.print) {
            window.Module.print(`${stopCommand}\n`);
          } else if (typeof process !== 'undefined' && process.stdout) {
            process.stdout.write(`${stopCommand}\n`);
          }
          
          log(`[Bridge] State machine stop requested`);
          break;
        }
        
        default:
          log(`[Bridge] Unknown GEN_STATEM command: ${command}`);
      }
    } catch (error) {
      log(`[Bridge] Error processing GEN_STATEM command ${command}: ${error.message}`);
    }
  }
  
  // Parse GEN_STATEM response commands (Erlang → JavaScript)
  // Format: GEN_STATEM_STATE:<State>:<Buttons>
  // This is handled in the main handleBridgeCommand function above
  
  log('[Bridge] Interceptor installed - Erlang processes can now communicate with KGC-4D bridge, use hook primitives, and control state machines');
}

