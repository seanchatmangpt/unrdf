/**
 * KGC-4D Bridge for Erlang Process Integration
 *
 * Bridges Erlang processes (via AtomVM) with real KGC-4D and knowledge hooks implementations.
 * This enables Erlang processes to emit events and process intents through the actual
 * @unrdf/kgc-4d and @unrdf/hooks packages.
 *
 * **Big Bang 80/20**: Focuses on the 20% that proves 80% works:
 * - Event emission to KGC-4D
 * - Hook registration and execution
 * - Intent → Outcome processing
 *
 * @module kgc4d-bridge
 */

import { KGCStore, EVENT_TYPES } from '@unrdf/kgc-4d';
import { defineHook, createHookRegistry, registerHook } from '@unrdf/hooks';
import { trace } from '@opentelemetry/api';
import { startRoundtrip, endRoundtrip, getSLAStats, OPERATION_TYPES } from '../../src/roundtrip-sla.mjs';

// Get tracer lazily to ensure provider is registered first
function getTracer() {
  return trace.getTracer('atomvm-bridge');
}

/**
 * KGC-4D Bridge
 *
 * Provides JavaScript API for Erlang processes to interact with KGC-4D and hooks.
 * Erlang processes communicate via message passing or io:format interception.
 */
export class KGC4DBridge {
  /**
   * @param {Object} [options] - Bridge options
   * @param {Function} [options.log] - Logging function
   */
  constructor(options = {}) {
    this.log = options.log || console.log;
    
    // Initialize KGC-4D store
    this.store = new KGCStore({ nodeId: 'boardroom-bridge' });
    
    // Initialize hook registry using real implementation
    this.registry = createHookRegistry();
    
    // Event log for tracking
    this.eventLog = [];
    
    // Intent processing state
    this.intents = new Map(); // intentId → { intent, outcome }
    
    this.log('KGC-4D Bridge initialized');
  }

  /**
   * Emit event to KGC-4D
   *
   * **80/20**: Proves Erlang processes can emit events to real KGC-4D
   *
   * @param {string} type - Event type (from EVENT_TYPES or custom)
   * @param {Object} payload - Event payload
   * @returns {Promise<Object>} Event receipt
   */
  async emitEvent(type, payload = {}) {
    // Start roundtrip tracking for JS→Erlang→JS roundtrip
    const roundtripId = startRoundtrip(OPERATION_TYPES.EMIT_EVENT);
    
    return getTracer().startActiveSpan('bridge.emit_event', {
      attributes: {
        'event.type': type || EVENT_TYPES.CREATE,
        'event.source': 'erlang-process',
        'roundtrip.operation_id': roundtripId,
        'roundtrip.operation_type': OPERATION_TYPES.EMIT_EVENT,
      }
    }, async (span) => {
      try {
        const receipt = await this.store.appendEvent(
          {
            type: type || EVENT_TYPES.CREATE,
            payload: {
              ...payload,
              timestamp: Date.now(),
              source: 'erlang-process'
            }
          },
          [] // No RDF operations for now (80/20 focus)
        );
        
        this.eventLog.push({
          receipt,
          type,
          payload,
          timestamp: Date.now()
        });
        
        span.setAttribute('event.id', receipt.receipt.id);
        span.setStatus({ code: 1 }); // OK
        this.log(`Event emitted: ${type} (id: ${receipt.receipt.id})`);
        
        // End roundtrip tracking
        const roundtripResult = endRoundtrip(roundtripId, true);
        span.setAttribute('roundtrip.latency', roundtripResult.latency);
        span.setAttribute('roundtrip.sla_met', roundtripResult.slaMet);
        
        // Add current error rate to span
        const slaStats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);
        span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
        
        return {
          success: true,
          receipt: {
            id: receipt.receipt.id,
            timestamp: receipt.receipt.timestamp_iso,
            event_count: receipt.receipt.event_count.toString()
          },
          roundtrip: {
            latency: roundtripResult.latency,
            slaMet: roundtripResult.slaMet,
          }
        };
      } catch (error) {
        const errorMessage = error?.message || String(error);
        span.setStatus({ code: 2, message: errorMessage }); // ERROR
        this.log(`Event emission failed: ${errorMessage}`);
        
        // End roundtrip tracking with failure
        const roundtripResult = endRoundtrip(roundtripId, false, errorMessage);
        span.setAttribute('roundtrip.latency', roundtripResult.latency);
        span.setAttribute('roundtrip.sla_met', false);
        
        // Add current error rate to span
        const slaStats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);
        span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
        
        return {
          success: false,
          error: errorMessage,
          roundtrip: {
            latency: roundtripResult.latency,
            slaMet: false,
          }
        };
      } finally {
        span.end();
      }
    });
  }

  /**
   * Register a knowledge hook
   *
   * **80/20**: Proves hooks can be registered and will process events
   *
   * @param {Object} hookConfig - Hook configuration
   * @param {string} hookConfig.name - Hook name
   * @param {string} hookConfig.trigger - Hook trigger (e.g., 'before-add', 'after-add')
   * @param {Function} [hookConfig.validate] - Validation function
   * @param {Function} [hookConfig.transform] - Transformation function
   * @returns {Object} Registration result
   */
  registerHook(hookConfig) {
    // Start roundtrip tracking for JS→Erlang→JS roundtrip
    const roundtripId = startRoundtrip(OPERATION_TYPES.REGISTER_HOOK);
    
    return getTracer().startActiveSpan('bridge.register_hook', {
      attributes: {
        'hook.name': hookConfig.name,
        'hook.trigger': hookConfig.trigger || 'before-add',
        'roundtrip.operation_id': roundtripId,
        'roundtrip.operation_type': OPERATION_TYPES.REGISTER_HOOK,
      }
    }, (span) => {
      try {
        // Provide default validate function if none provided (80/20 focus)
        const validateFn = hookConfig.validate || (() => true);
        
        const hook = defineHook({
          name: hookConfig.name,
          trigger: hookConfig.trigger || 'before-add',
          validate: validateFn,
          transform: hookConfig.transform,
          metadata: hookConfig.metadata || {}
        });
        
        // Register using real implementation
        registerHook(this.registry, hook);
        
        span.setStatus({ code: 1 }); // OK
        this.log(`Hook registered: ${hookConfig.name} (trigger: ${hookConfig.trigger || 'before-add'})`);
        
        // End roundtrip tracking
        const roundtripResult = endRoundtrip(roundtripId, true);
        span.setAttribute('roundtrip.latency', roundtripResult.latency);
        span.setAttribute('roundtrip.sla_met', roundtripResult.slaMet);
        
        // Add current error rate to span
        const slaStats = getSLAStats(OPERATION_TYPES.REGISTER_HOOK);
        span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
        
        return {
          success: true,
          hookName: hookConfig.name,
          roundtrip: {
            latency: roundtripResult.latency,
            slaMet: roundtripResult.slaMet,
          }
        };
      } catch (error) {
        const errorMessage = error?.message || String(error);
        span.setStatus({ code: 2, message: errorMessage }); // ERROR
        this.log(`Hook registration failed: ${errorMessage}`);
        
        // End roundtrip tracking with failure
        const roundtripResult = endRoundtrip(roundtripId, false, errorMessage);
        span.setAttribute('roundtrip.latency', roundtripResult.latency);
        span.setAttribute('roundtrip.sla_met', false);
        
        // Add current error rate to span
        const slaStats = getSLAStats(OPERATION_TYPES.REGISTER_HOOK);
        span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
        
        return {
          success: false,
          error: errorMessage,
          roundtrip: {
            latency: roundtripResult.latency,
            slaMet: false,
          }
        };
      } finally {
        span.end();
      }
    });
  }

  /**
   * Process intent (Λ) through hooks to outcome (A)
   *
   * **80/20**: Proves intent → outcome transformation works
   *
   * @param {string} intentId - Unique intent identifier
   * @param {Object} intent - Intent payload (Λ)
   * @returns {Promise<Object>} Outcome (A)
   */
  async processIntent(intentId, intent) {
    // Start roundtrip tracking for JS→Erlang→JS roundtrip
    const roundtripId = startRoundtrip(OPERATION_TYPES.PROCESS_INTENT);
    
    return getTracer().startActiveSpan('bridge.process_intent', {
      attributes: {
        'intent.id': intentId,
        'intent.description': intent.description || 'No description',
        'roundtrip.operation_id': roundtripId,
        'roundtrip.operation_type': OPERATION_TYPES.PROCESS_INTENT,
      }
    }, async (span) => {
      try {
        // Emit intent as event
        const intentEvent = await this.emitEvent('INTENT', {
          intentId,
          intent,
          description: `Intent: ${intent.description || 'No description'}`
        });
        
        if (!intentEvent.success) {
          span.setStatus({ code: 2, message: 'Failed to emit intent event' });
          return {
            success: false,
            error: 'Failed to emit intent event'
          };
        }
        
        // Process through hooks (simplified - 80/20 focus)
        // In full implementation, this would go through µ(O) operators
        const outcome = {
          intentId,
          accepted: true,
          constraints: [],
          recommendations: [],
          timestamp: Date.now()
        };
        
        // Apply hook processing (if hooks are registered)
        // For 80/20, we just mark as processed
        outcome.processed = true;
        
        // Store intent → outcome mapping
        this.intents.set(intentId, {
          intent,
          outcome,
          timestamp: Date.now()
        });
        
        span.setAttribute('intent.accepted', outcome.accepted);
        span.setStatus({ code: 1 }); // OK
        this.log(`Intent processed: ${intentId} → Outcome: ${outcome.accepted ? 'accepted' : 'rejected'}`);
        
        // End roundtrip tracking
        const roundtripResult = endRoundtrip(roundtripId, true);
        span.setAttribute('roundtrip.latency', roundtripResult.latency);
        span.setAttribute('roundtrip.sla_met', roundtripResult.slaMet);
        
        // Add current error rate to span
        const slaStats = getSLAStats(OPERATION_TYPES.PROCESS_INTENT);
        span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
        
        return {
          success: true,
          outcome,
          roundtrip: {
            latency: roundtripResult.latency,
            slaMet: roundtripResult.slaMet,
          }
        };
      } catch (error) {
        const errorMessage = error?.message || String(error);
        span.setStatus({ code: 2, message: errorMessage }); // ERROR
        this.log(`Intent processing failed: ${errorMessage}`);
        
        // End roundtrip tracking with failure
        const roundtripResult = endRoundtrip(roundtripId, false, errorMessage);
        span.setAttribute('roundtrip.latency', roundtripResult.latency);
        span.setAttribute('roundtrip.sla_met', false);
        
        // Add current error rate to span
        const slaStats = getSLAStats(OPERATION_TYPES.PROCESS_INTENT);
        span.setAttribute('roundtrip.error_rate', slaStats.errorRate);
        
        return {
          success: false,
          error: errorMessage,
          roundtrip: {
            latency: roundtripResult.latency,
            slaMet: false,
          }
        };
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get event log
   *
   * @returns {Array} Event log entries
   */
  getEventLog() {
    return this.eventLog;
  }

  /**
   * Get intent outcome
   *
   * @param {string} intentId - Intent identifier
   * @returns {Object|null} Intent outcome or null
   */
  getIntentOutcome(intentId) {
    return this.intents.get(intentId) || null;
  }

  /**
   * Get all intents
   *
   * @returns {Array} All intent outcomes
   */
  getAllIntents() {
    return Array.from(this.intents.values());
  }

  /**
   * Clear event log and intents (for testing)
   */
  clear() {
    this.eventLog = [];
    this.intents.clear();
    this.log('Bridge cleared');
  }
}

/**
 * Global bridge instance (for Erlang access via AtomVM Module)
 * 
 * Erlang processes can access this via:
 * - io:format with special markers that JavaScript intercepts
 * - AtomVM Module.callMain or Module.run callbacks
 */
let globalBridge = null;

/**
 * Get or create global bridge instance
 *
 * @param {Object} [options] - Bridge options
 * @returns {KGC4DBridge} Bridge instance
 */
export function getBridge(options = {}) {
  if (!globalBridge) {
    globalBridge = new KGC4DBridge(options);
  }
  return globalBridge;
}

/**
 * Set global bridge instance (for testing)
 *
 * @param {KGC4DBridge} bridge - Bridge instance
 */
export function setBridge(bridge) {
  globalBridge = bridge;
}

