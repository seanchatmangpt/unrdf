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
   * Parse and handle bridge commands from Erlang output
   */
  function handleBridgeCommand(text) {
    if (typeof text !== 'string') {
      return false;
    }
    
    // Parse KGC4D_BRIDGE commands
    // Format: KGC4D_BRIDGE:command:arg1:arg2:...
    const bridgeMatch = text.match(/^KGC4D_BRIDGE:(\w+):(.*)$/);
    if (!bridgeMatch) {
      return false;
    }
    
    const [, command, argsStr] = bridgeMatch;
    
    try {
      switch (command) {
        case 'emit_event': {
          // Format: KGC4D_BRIDGE:emit_event:Type:Payload
          const [type, payloadStr] = argsStr.split(':', 2);
          let payload = {};
          if (payloadStr) {
            try {
              // Try to parse as JSON first
              payload = JSON.parse(payloadStr);
            } catch {
              // If not JSON, treat as string
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
          let intent = {};
          
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
              if (intentStr) {
                try {
                  intent = JSON.parse(intentStr);
                } catch {
                  intent = { description: intentStr };
                }
              }
              
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
  
  log('[Bridge] Interceptor installed - Erlang processes can now communicate with KGC-4D bridge');
}

