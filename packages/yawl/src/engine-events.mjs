/**
 * @file YAWL Engine Events - Event subscription, emission, and logging
 * @module @unrdf/yawl/engine-events
 */

import { now, toISO } from '@unrdf/kgc-4d';
import {
  createWorkflowReceipt,
  appendWorkflowEvent,
  YAWL_EVENT_TYPES,
} from './events/yawl-events.mjs';
import { ENGINE_EVENTS } from './engine-core.mjs';

/**
 * Mixin that adds event subscription and emission capabilities
 *
 * Provides:
 * - Event subscription (on/off)
 * - Event emission
 * - In-memory event log
 * - KGC-4D event logging integration
 *
 * @param {class} Base - Base class to extend
 * @returns {class} Extended class with event capabilities
 */
export function withEvents(Base) {
  return class EngineEvents extends Base {
    // =========================================================================
    // Event Subscription
    // =========================================================================

    /**
     * Subscribe to engine events
     * @param {string} eventType - Event type from ENGINE_EVENTS
     * @param {Function} handler - Event handler function
     * @returns {Function} Unsubscribe function
     *
     * @example
     * ```javascript
     * const unsubscribe = engine.on('task:completed', (event) => {
     *   console.log('Task completed:', event.taskId, event.output);
     * });
     *
     * // Later...
     * unsubscribe();
     * ```
     */
    on(eventType, handler) {
      if (typeof handler !== 'function') {
        throw new TypeError('Handler must be a function');
      }

      if (!this._eventHandlers.has(eventType)) {
        this._eventHandlers.set(eventType, new Set());
      }

      this._eventHandlers.get(eventType).add(handler);

      // Return unsubscribe function
      return () => {
        const handlers = this._eventHandlers.get(eventType);
        if (handlers) {
          handlers.delete(handler);
        }
      };
    }

    /**
     * Emit an event to all subscribers
     * @param {string} eventType - Event type
     * @param {Object} data - Event data
     */
    emit(eventType, data) {
      const handlers = this._eventHandlers.get(eventType);
      if (!handlers) return;

      const event = {
        type: eventType,
        timestamp: now().toString(),
        timestampISO: toISO(now()),
        ...data,
      };

      for (const handler of handlers) {
        try {
          handler(event);
        } catch (error) {
          // Log error but don't throw to avoid disrupting other handlers
          console.error(`Error in event handler for ${eventType}:`, error);
        }
      }
    }

    /**
     * Remove all handlers for an event type
     * @param {string} eventType - Event type
     */
    off(eventType) {
      this._eventHandlers.delete(eventType);
    }

    // =========================================================================
    // Event Log (In-Memory)
    // =========================================================================

    /**
     * Append an event to the in-memory log
     * @param {Object} eventData - Event data
     * @protected
     */
    _appendEvent(eventData) {
      const timestamp = now();
      this.events.push({
        ...eventData,
        timestamp: timestamp.toString(),
        timestampISO: toISO(timestamp),
      });
      this._stats.eventsLogged++;
    }

    /**
     * Get events for a case
     * @param {string} caseId - Case ID
     * @returns {Array} Events for the case
     */
    getEventsForCase(caseId) {
      return this.events.filter(e => e.caseId === caseId);
    }

    /**
     * Get all events
     * @returns {Array} All events
     */
    getAllEvents() {
      return [...this.events];
    }

    // =========================================================================
    // KGC-4D Integration
    // =========================================================================

    /**
     * Log a case event to KGC-4D
     * @param {string} eventType - Event type
     * @param {Object} payload - Event payload
     * @protected
     */
    async _logCaseEvent(eventType, payload) {
      try {
        const receipt = await createWorkflowReceipt({
          beforeState: { empty: true },
          afterState: payload,
          decision: { action: eventType, ...payload },
          justification: { reasoning: `Case event: ${eventType}` },
        });

        await appendWorkflowEvent(this.store, eventType, {
          ...payload,
          timestamp: toISO(now()),
          receipt,
        });
      } catch (error) {
        console.error(`Failed to log case event ${eventType}:`, error);
      }
    }

    /**
     * Log a task event to KGC-4D
     * @param {string} eventType - Event type
     * @param {Object} payload - Event payload
     * @param {string} [caseId] - Case ID for context
     * @protected
     */
    async _logTaskEvent(eventType, payload, caseId) {
      try {
        const receipt = await createWorkflowReceipt({
          beforeState: payload.beforeState || { workItemId: payload.workItemId },
          afterState: payload,
          decision: { action: eventType, ...payload },
          justification: { reasoning: `Task event: ${eventType}` },
        });

        await appendWorkflowEvent(
          this.store,
          eventType,
          {
            ...payload,
            receipt,
          },
          { caseId }
        );
      } catch (error) {
        console.error(`Failed to log task event ${eventType}:`, error);
      }
    }

    // =========================================================================
    // Workflow Registration Hook Override
    // =========================================================================

    /**
     * Called when a workflow is registered
     * @param {YawlWorkflow} workflow - Registered workflow
     * @protected
     */
    _onWorkflowRegistered(workflow) {
      this._appendEvent({
        type: 'WORKFLOW_REGISTERED',
        workflowId: workflow.id,
        version: workflow.version,
      });

      this.emit(ENGINE_EVENTS.WORKFLOW_REGISTERED, {
        workflowId: workflow.id,
        version: workflow.version,
        name: workflow.name,
      });

      // Call parent implementation if exists
      if (super._onWorkflowRegistered) {
        super._onWorkflowRegistered(workflow);
      }
    }
  };
}
