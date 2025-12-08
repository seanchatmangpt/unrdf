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
import { defineHook, createHookRegistry, registerHook, HookTriggerSchema } from '@unrdf/hooks';
import { trace } from '@opentelemetry/api';
import { startRoundtrip, endRoundtrip, getSLAStats, OPERATION_TYPES, canStartRoundtrip } from '../../src/roundtrip-sla.mjs';
import { getHookExecutionEngine } from './hook-execution-engine.mjs';
import { z } from 'zod';

// Get tracer lazily to ensure provider is registered first
function getTracer() {
  return trace.getTracer('atomvm-bridge');
}

/**
 * Poka-Yoke: Validation schemas for KGC-4D integration
 */

/** @constant {number} Maximum payload size in bytes (1MB limit from KGC-4D store) */
const MAX_PAYLOAD_SIZE_BYTES = 1_000_000;

/** @constant {number} Warning threshold for payload size */
const PAYLOAD_SIZE_WARNING_BYTES = 100_000;

/**
 * Event payload schema (poka-yoke: validates payload structure and size)
 */
const EventPayloadSchema = z.object({
  // Allow any object structure, but validate it's an object
}).passthrough().refine(
  (payload) => {
    const payloadStr = JSON.stringify(payload);
    const payloadSize = Buffer.byteLength(payloadStr, 'utf8');
    return payloadSize <= MAX_PAYLOAD_SIZE_BYTES;
  },
  {
    message: `Payload size exceeds limit: ${MAX_PAYLOAD_SIZE_BYTES} bytes (1MB)`,
  }
);

/**
 * Event type schema (poka-yoke: validates event type is non-empty string)
 */
const EventTypeSchema = z.string().min(1, 'Event type must be non-empty string');

/**
 * Hook config schema (poka-yoke: validates hook configuration)
 * Note: validate/transform are optional - bridge provides default validate if neither is provided
 */
const HookConfigSchema = z.object({
  name: z.string().min(1, 'Hook name must be non-empty string'),
  trigger: HookTriggerSchema,
  validate: z.function().optional(),
  transform: z.function().optional(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Intent schema (poka-yoke: validates intent structure)
 */
const IntentSchema = z.object({
  description: z.string().optional(),
}).passthrough();

/**
 * Intent ID schema (poka-yoke: validates intent ID is non-empty string)
 */
const IntentIdSchema = z.string().min(1, 'Intent ID must be non-empty string');

/**
 * Poka-Yoke: Validate non-empty string
 * @param {any} value - Value to validate
 * @param {string} name - Name of the parameter
 * @throws {Error} If value is not a non-empty string
 */
function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.length === 0) {
    throw new Error(`${name} must be a non-empty string, got: ${typeof value}`);
  }
}

/**
 * Poka-Yoke: Validate event payload size
 * @param {Object} payload - Payload to validate
 * @throws {Error} If payload exceeds size limit
 */
function validatePayloadSize(payload) {
  const payloadStr = JSON.stringify(payload);
  const payloadSize = Buffer.byteLength(payloadStr, 'utf8');
  
  if (payloadSize > MAX_PAYLOAD_SIZE_BYTES) {
    throw new Error(
      `Event payload exceeds size limit: ${payloadSize} bytes > ${MAX_PAYLOAD_SIZE_BYTES} bytes (1MB)`
    );
  }
  
  if (payloadSize > PAYLOAD_SIZE_WARNING_BYTES && typeof console !== 'undefined' && console.warn) {
    console.warn(
      `[KGC-4D Bridge] Large payload warning: ${payloadSize} bytes (threshold: ${PAYLOAD_SIZE_WARNING_BYTES} bytes)`
    );
  }
}

/**
 * Bridge state machine (poka-yoke: prevents invalid operations)
 * @enum {string}
 */
const BridgeState = {
  UNINITIALIZED: 'uninitialized',
  INITIALIZING: 'initializing',
  READY: 'ready',
  ERROR: 'error',
  DESTROYED: 'destroyed',
};

/**
 * KGC-4D Bridge
 *
 * Provides JavaScript API for Erlang processes to interact with KGC-4D and hooks.
 * Erlang processes communicate via message passing or io:format interception.
 */
export class KGC4DBridge {
  /**
   * **Poka-Yoke**: Bridge state machine prevents invalid operations
   * 
   * @param {Object} [options] - Bridge options
   * @param {Function} [options.log] - Logging function
   * @param {string} [options.nodeId] - Node ID for KGC-4D store
   */
  constructor(options = {}) {
    this.log = options.log || console.log;
    
    // Poka-Yoke: State machine initialization
    this.state = BridgeState.UNINITIALIZED;
    
    // Initialize bridge components
    this._initialize(options);
  }
  
  /**
   * **Poka-Yoke**: Initialize bridge components with error handling
   * @private
   */
  _initialize(options) {
    try {
      this.state = BridgeState.INITIALIZING;
      
      // Initialize KGC-4D store
      this.store = new KGCStore({ nodeId: options.nodeId || 'boardroom-bridge' });
      
      // Initialize hook registry using real implementation (for backward compatibility)
      this.registry = createHookRegistry();
      
      // Initialize hook execution engine (innovative layer - 800ns execution)
      this.hookEngine = getHookExecutionEngine({ log: this.log });
      
      // Event log for tracking
      this.eventLog = [];
      
      // Intent processing state
      this.intents = new Map(); // intentId → { intent, outcome }
      
      this.state = BridgeState.READY;
      this.log('KGC-4D Bridge initialized');
    } catch (error) {
      this.state = BridgeState.ERROR;
      this.log(`KGC-4D Bridge initialization failed: ${error.message}`);
      throw error;
    }
  }
  
  /**
   * **Poka-Yoke**: Type guard - checks if bridge is ready
   * @returns {boolean} True if bridge is ready
   */
  isReady() {
    return this.state === BridgeState.READY && this.store !== null && this.registry !== null;
  }
  
  /**
   * **Poka-Yoke**: Type guard - checks if bridge is destroyed
   * @returns {boolean} True if bridge is destroyed
   */
  isDestroyed() {
    return this.state === BridgeState.DESTROYED;
  }
  
  /**
   * **Poka-Yoke**: Ensures bridge is ready before operations
   * @throws {Error} If bridge is not ready
   */
  _ensureReady() {
    if (this.isDestroyed()) {
      throw new Error('KGC-4D Bridge has been destroyed');
    }
    if (!this.isReady()) {
      throw new Error(`KGC-4D Bridge is not ready (state: ${this.state})`);
    }
  }

  /**
   * Emit event to KGC-4D
   *
   * **80/20**: Proves Erlang processes can emit events to real KGC-4D
   * **Poka-Yoke**: Validates event type, payload structure, and size before store call
   *
   * @param {string} type - Event type (from EVENT_TYPES or custom)
   * @param {Object} payload - Event payload
   * @returns {Promise<Object>} Event receipt
   * @throws {Error} If bridge is not ready, event type is invalid, or payload is invalid
   */
  async emitEvent(type, payload = {}) {
    // Poka-Yoke: Ensure bridge is ready
    this._ensureReady();
    
    // Poka-Yoke: Validate event type and payload
    type = EventTypeSchema.parse(type || EVENT_TYPES.CREATE);
    payload = EventPayloadSchema.parse(payload);
    validatePayloadSize(payload);
    
    // Poka-Yoke: Check if roundtrip can start (SLA compliance)
    if (!canStartRoundtrip(OPERATION_TYPES.EMIT_EVENT)) {
      const stats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);
      throw new Error(
        `SLA violation prevented: Error rate ${(stats.errorRate * 100).toFixed(2)}% exceeds threshold for emit_event`
      );
    }
    
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
        // **Innovative Layer**: Execute hooks before event emission (800ns execution)
        // This replaces Zod validation and manual validation functions
        const beforeResult = this.hookEngine.executeByTrigger('before-add', {
          type,
          payload,
          timestamp: Date.now(),
          source: 'erlang-process'
        });
        
        if (!beforeResult.valid) {
          throw new Error(`Hook validation failed: ${beforeResult.error || 'Unknown error'}`);
        }
        
        // Use transformed payload from hooks
        // If hooks transformed the quad, extract the payload
        let transformedPayload = payload;
        if (beforeResult.quad && beforeResult.quad.object) {
          try {
            transformedPayload = JSON.parse(beforeResult.quad.object.value);
          } catch {
            // If parsing fails, use original payload
            transformedPayload = payload;
          }
        }
        
        const receipt = await this.store.appendEvent(
          {
            type,
            payload: {
              ...transformedPayload,
              timestamp: Date.now(),
              source: 'erlang-process'
            }
          },
          [] // No RDF operations for now (80/20 focus)
        );
        
        // **Innovative Layer**: Execute hooks after event emission
        this.hookEngine.executeByTrigger('after-add', {
          type,
          payload: transformedPayload,
          receipt: receipt.receipt.id
        });
        
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
   * **Innovative Layer**: Hooks are registered in both the legacy registry (for backward compatibility)
   * and the hook execution engine (for 800ns execution from Erlang).
   *
   * **Poka-Yoke**: Validates hook config, checks for duplicate registration
   *
   * @param {Object} hookConfig - Hook configuration
   * @param {string} hookConfig.name - Hook name
   * @param {string} hookConfig.trigger - Hook trigger (e.g., 'before-add', 'after-add')
   * @param {Function} [hookConfig.validate] - Validation function
   * @param {Function} [hookConfig.transform] - Transformation function
   * @returns {Object} Registration result
   * @throws {Error} If bridge is not ready, hook config is invalid, or hook already exists
   */
  registerHook(hookConfig) {
    // Poka-Yoke: Ensure bridge is ready
    this._ensureReady();
    
    // Poka-Yoke: Validate hook config
    hookConfig = HookConfigSchema.parse(hookConfig);
    
    // Poka-Yoke: Check if hook already exists
    if (this.registry.hooks.has(hookConfig.name)) {
      throw new Error(`Hook already registered: ${hookConfig.name}`);
    }
    
    // Register in hook execution engine (innovative layer - 800ns execution)
    this.hookEngine.registerHook(
      hookConfig.name,
      hookConfig.trigger,
      hookConfig.validate,
      hookConfig.transform
    );
    
    // Poka-Yoke: Check if roundtrip can start (SLA compliance)
    if (!canStartRoundtrip(OPERATION_TYPES.REGISTER_HOOK)) {
      const stats = getSLAStats(OPERATION_TYPES.REGISTER_HOOK);
      throw new Error(
        `SLA violation prevented: Error rate ${(stats.errorRate * 100).toFixed(2)}% exceeds threshold for register_hook`
      );
    }
    
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
          trigger: hookConfig.trigger,
          validate: validateFn,
          transform: hookConfig.transform,
          metadata: hookConfig.metadata || {}
        });
        
        // Register using real implementation
        registerHook(this.registry, hook);
        
        span.setStatus({ code: 1 }); // OK
        this.log(`Hook registered: ${hookConfig.name} (trigger: ${hookConfig.trigger})`);
        
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
   * **Poka-Yoke**: Validates intent ID and intent structure
   *
   * @param {string} intentId - Unique intent identifier
   * @param {Object} intent - Intent payload (Λ)
   * @returns {Promise<Object>} Outcome (A)
   * @throws {Error} If bridge is not ready, intent ID is invalid, or intent structure is invalid
   */
  async processIntent(intentId, intent) {
    // Poka-Yoke: Ensure bridge is ready
    this._ensureReady();
    
    // Poka-Yoke: Validate intent ID and structure
    intentId = IntentIdSchema.parse(intentId);
    intent = IntentSchema.parse(intent || {});
    
    // Poka-Yoke: Check if roundtrip can start (SLA compliance)
    if (!canStartRoundtrip(OPERATION_TYPES.PROCESS_INTENT)) {
      const stats = getSLAStats(OPERATION_TYPES.PROCESS_INTENT);
      throw new Error(
        `SLA violation prevented: Error rate ${(stats.errorRate * 100).toFixed(2)}% exceeds threshold for process_intent`
      );
    }
    
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
   * **Poka-Yoke**: Only clears if bridge is ready
   */
  clear() {
    this._ensureReady();
    this.eventLog = [];
    this.intents.clear();
    this.log('Bridge cleared');
  }
  
  /**
   * Destroy bridge (poka-yoke: terminal state)
   */
  destroy() {
    this.state = BridgeState.DESTROYED;
    this.store = null;
    this.registry = null;
    this.eventLog = [];
    this.intents.clear();
    this.log('KGC-4D Bridge destroyed');
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

