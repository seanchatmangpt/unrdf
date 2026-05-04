/**
 * @file Custom Event System
 * @module observability/custom-events
 *
 * @description
 * Security, performance, and business event tracking with structured
 * event correlation and alerting integration.
 */

import { trace, SpanKind, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Event severity levels
 */
export const EventSeverity = {
  DEBUG: 'debug',
  INFO: 'info',
  WARNING: 'warning',
  ERROR: 'error',
  CRITICAL: 'critical',
};

/**
 * Event types
 */
export const EventType = {
  // Security events
  SECURITY_AUTH_FAILURE: 'security.auth.failure',
  SECURITY_INJECTION_ATTEMPT: 'security.injection.attempt',
  SECURITY_RATE_LIMIT_EXCEEDED: 'security.rate_limit.exceeded',
  SECURITY_UNAUTHORIZED_ACCESS: 'security.unauthorized_access',

  // Performance events
  PERFORMANCE_SLOW_QUERY: 'performance.slow_query',
  PERFORMANCE_TIMEOUT_WARNING: 'performance.timeout.warning',
  PERFORMANCE_MEMORY_HIGH: 'performance.memory.high',
  PERFORMANCE_CPU_HIGH: 'performance.cpu.high',

  // Business events
  BUSINESS_WORKFLOW_COMPLETE: 'business.workflow.complete',
  BUSINESS_STATE_CHANGE: 'business.state.change',
  BUSINESS_VALIDATION_FAILURE: 'business.validation.failure',
  BUSINESS_TRANSACTION_COMPLETE: 'business.transaction.complete',
};

/**
 * Custom event schema
 */
export const CustomEventSchema = z.object({
  type: z.string(),
  severity: z.enum([
    EventSeverity.DEBUG,
    EventSeverity.INFO,
    EventSeverity.WARNING,
    EventSeverity.ERROR,
    EventSeverity.CRITICAL,
  ]),
  message: z.string(),
  timestamp: z.number(),
  attributes: z.record(z.string(), z.any()),
  correlationId: z.string().optional(),
  userId: z.string().optional(),
  spanId: z.string().optional(),
});

/**
 * Custom events manager
 *
 * Provides structured event tracking for:
 * - Security incidents (auth failures, injection attempts)
 * - Performance anomalies (slow queries, timeouts)
 * - Business events (workflow completion, state changes)
 */
export class CustomEvents {
  /**
   * Create custom events manager
   *
   * @param {Object} [config] - Configuration
   * @param {string} [config.serviceName='unrdf'] - Service name
   * @param {boolean} [config.enabled=true] - Enable events
   * @param {Function} [config.eventHandler] - Custom event handler
   */
  constructor(config = {}) {
    this.serviceName = config.serviceName || 'unrdf';
    this.enabled = config.enabled !== false;
    this.eventHandler = config.eventHandler;
    this.tracer = trace.getTracer(this.serviceName);

    // Event storage (last 1000 events)
    this.events = [];
    this.maxEvents = 1000;
  }

  /**
   * Emit a security event
   *
   * @param {Object} options - Event options
   * @param {string} options.type - Event type
   * @param {string} options.message - Event message
   * @param {Object} [options.attributes] - Additional attributes
   * @param {string} [options.userId] - User ID if applicable
   * @param {string} [options.correlationId] - Correlation ID
   */
  emitSecurityEvent({ type, message, attributes = {}, userId, correlationId }) {
    return this._emitEvent({
      type,
      severity: this._getSeverityForSecurityEvent(type),
      message,
      attributes: {
        ...attributes,
        category: 'security',
      },
      userId,
      correlationId,
    });
  }

  /**
   * Emit authentication failure event
   *
   * @param {Object} options - Event options
   * @param {string} options.userId - User ID that failed auth
   * @param {string} options.reason - Failure reason
   * @param {string} [options.ip] - IP address
   * @param {Object} [options.metadata] - Additional metadata
   */
  emitAuthFailure({ userId, reason, ip, metadata = {} }) {
    return this.emitSecurityEvent({
      type: EventType.SECURITY_AUTH_FAILURE,
      message: `Authentication failure for user: ${userId}`,
      attributes: {
        'auth.user_id': userId,
        'auth.failure_reason': reason,
        'auth.ip_address': ip,
        ...metadata,
      },
      userId,
    });
  }

  /**
   * Emit injection attempt event
   *
   * @param {Object} options - Event options
   * @param {string} options.attackType - Type of injection (SQL, SPARQL, command)
   * @param {string} options.payload - Attack payload (sanitized)
   * @param {string} [options.userId] - User ID if authenticated
   * @param {string} [options.ip] - IP address
   */
  emitInjectionAttempt({ attackType, payload, userId, ip }) {
    return this.emitSecurityEvent({
      type: EventType.SECURITY_INJECTION_ATTEMPT,
      message: `${attackType} injection attempt detected`,
      attributes: {
        'injection.type': attackType,
        'injection.payload_hash': this._hashPayload(payload),
        'injection.ip_address': ip,
      },
      userId,
    });
  }

  /**
   * Emit performance event
   *
   * @param {Object} options - Event options
   * @param {string} options.type - Event type
   * @param {string} options.message - Event message
   * @param {Object} [options.attributes] - Additional attributes
   * @param {string} [options.correlationId] - Correlation ID
   */
  emitPerformanceEvent({ type, message, attributes = {}, correlationId }) {
    return this._emitEvent({
      type,
      severity: this._getSeverityForPerformanceEvent(type, attributes),
      message,
      attributes: {
        ...attributes,
        category: 'performance',
      },
      correlationId,
    });
  }

  /**
   * Emit slow query event
   *
   * @param {Object} options - Event options
   * @param {string} options.query - Query (sanitized)
   * @param {number} options.duration - Query duration in ms
   * @param {number} options.threshold - Slow query threshold in ms
   * @param {Object} [options.metadata] - Additional metadata
   */
  emitSlowQuery({ query, duration, threshold, metadata = {} }) {
    return this.emitPerformanceEvent({
      type: EventType.PERFORMANCE_SLOW_QUERY,
      message: `Slow query detected: ${duration}ms (threshold: ${threshold}ms)`,
      attributes: {
        'query.duration_ms': duration,
        'query.threshold_ms': threshold,
        'query.hash': this._hashPayload(query),
        ...metadata,
      },
    });
  }

  /**
   * Emit timeout warning event
   *
   * @param {Object} options - Event options
   * @param {string} options.operation - Operation name
   * @param {number} options.elapsed - Elapsed time in ms
   * @param {number} options.timeout - Timeout threshold in ms
   */
  emitTimeoutWarning({ operation, elapsed, timeout }) {
    return this.emitPerformanceEvent({
      type: EventType.PERFORMANCE_TIMEOUT_WARNING,
      message: `Operation approaching timeout: ${elapsed}ms / ${timeout}ms`,
      attributes: {
        'operation.name': operation,
        'operation.elapsed_ms': elapsed,
        'operation.timeout_ms': timeout,
        'operation.remaining_ms': timeout - elapsed,
      },
    });
  }

  /**
   * Emit high memory usage event
   *
   * @param {Object} options - Event options
   * @param {number} options.heapUsed - Heap used in bytes
   * @param {number} options.heapTotal - Heap total in bytes
   * @param {number} options.threshold - Threshold as fraction (0-1)
   */
  emitHighMemory({ heapUsed, heapTotal, threshold }) {
    const usageRatio = heapUsed / heapTotal;

    return this.emitPerformanceEvent({
      type: EventType.PERFORMANCE_MEMORY_HIGH,
      message: `High memory usage: ${Math.round(usageRatio * 100)}%`,
      attributes: {
        'memory.heap_used': heapUsed,
        'memory.heap_total': heapTotal,
        'memory.usage_ratio': usageRatio,
        'memory.threshold': threshold,
      },
    });
  }

  /**
   * Emit business event
   *
   * @param {Object} options - Event options
   * @param {string} options.type - Event type
   * @param {string} options.message - Event message
   * @param {Object} [options.attributes] - Additional attributes
   * @param {string} [options.userId] - User ID
   * @param {string} [options.correlationId] - Correlation ID
   */
  emitBusinessEvent({ type, message, attributes = {}, userId, correlationId }) {
    return this._emitEvent({
      type,
      severity: EventSeverity.INFO,
      message,
      attributes: {
        ...attributes,
        category: 'business',
      },
      userId,
      correlationId,
    });
  }

  /**
   * Emit workflow completion event
   *
   * @param {Object} options - Event options
   * @param {string} options.workflowId - Workflow ID
   * @param {string} options.workflowType - Workflow type
   * @param {number} options.duration - Workflow duration in ms
   * @param {boolean} options.success - Whether workflow succeeded
   * @param {Object} [options.metadata] - Additional metadata
   */
  emitWorkflowComplete({ workflowId, workflowType, duration, success, metadata = {} }) {
    return this.emitBusinessEvent({
      type: EventType.BUSINESS_WORKFLOW_COMPLETE,
      message: `Workflow ${workflowId} completed: ${success ? 'success' : 'failure'}`,
      attributes: {
        'workflow.id': workflowId,
        'workflow.type': workflowType,
        'workflow.duration_ms': duration,
        'workflow.success': success,
        ...metadata,
      },
      correlationId: workflowId,
    });
  }

  /**
   * Emit state change event
   *
   * @param {Object} options - Event options
   * @param {string} options.entity - Entity type
   * @param {string} options.entityId - Entity ID
   * @param {string} options.fromState - Previous state
   * @param {string} options.toState - New state
   * @param {string} [options.userId] - User who triggered change
   */
  emitStateChange({ entity, entityId, fromState, toState, userId }) {
    return this.emitBusinessEvent({
      type: EventType.BUSINESS_STATE_CHANGE,
      message: `${entity} ${entityId} state changed: ${fromState} → ${toState}`,
      attributes: {
        'state.entity': entity,
        'state.entity_id': entityId,
        'state.from': fromState,
        'state.to': toState,
      },
      userId,
      correlationId: entityId,
    });
  }

  /**
   * Emit event (internal)
   *
   * @param {Object} eventData - Event data
   * @returns {Object} Created event
   * @private
   */
  _emitEvent(eventData) {
    if (!this.enabled) return null;

    // Create span for event
    const span = this.tracer.startSpan(`event.${eventData.type}`, {
      kind: SpanKind.INTERNAL,
      attributes: {
        'event.type': eventData.type,
        'event.severity': eventData.severity,
        'event.category': eventData.attributes.category,
        ...eventData.attributes,
      },
    });

    // Create event object
    const event = CustomEventSchema.parse({
      ...eventData,
      timestamp: Date.now(),
      spanId: span.spanContext().spanId,
    });

    // Store event
    this.events.push(event);
    if (this.events.length > this.maxEvents) {
      this.events = this.events.slice(-this.maxEvents);
    }

    // Call custom handler if provided
    if (this.eventHandler) {
      try {
        this.eventHandler(event);
      } catch (error) {
        console.error('[CustomEvents] Handler error:', error);
      }
    }

    // Log event
    const logLevel = this._getLogLevel(event.severity);
    console[logLevel](`[CustomEvents] ${event.type}: ${event.message}`, event.attributes);

    // End span
    span.setStatus({ code: SpanStatusCode.OK });
    span.end();

    return event;
  }

  /**
   * Get severity for security event
   *
   * @param {string} type - Event type
   * @returns {string} Severity level
   * @private
   */
  _getSeverityForSecurityEvent(type) {
    const severityMap = {
      [EventType.SECURITY_AUTH_FAILURE]: EventSeverity.WARNING,
      [EventType.SECURITY_INJECTION_ATTEMPT]: EventSeverity.CRITICAL,
      [EventType.SECURITY_RATE_LIMIT_EXCEEDED]: EventSeverity.WARNING,
      [EventType.SECURITY_UNAUTHORIZED_ACCESS]: EventSeverity.ERROR,
    };

    return severityMap[type] || EventSeverity.WARNING;
  }

  /**
   * Get severity for performance event
   *
   * @param {string} type - Event type
   * @param {Object} attributes - Event attributes
   * @returns {string} Severity level
   * @private
   */
  _getSeverityForPerformanceEvent(type, attributes) {
    // Timeout warnings are critical if very close to timeout
    if (type === EventType.PERFORMANCE_TIMEOUT_WARNING) {
      const remaining = attributes['operation.remaining_ms'] || 0;
      return remaining < 1000 ? EventSeverity.CRITICAL : EventSeverity.WARNING;
    }

    // High memory is critical if > 90%
    if (type === EventType.PERFORMANCE_MEMORY_HIGH) {
      const ratio = attributes['memory.usage_ratio'] || 0;
      return ratio > 0.9 ? EventSeverity.CRITICAL : EventSeverity.WARNING;
    }

    return EventSeverity.WARNING;
  }

  /**
   * Get log level for severity
   *
   * @param {string} severity - Event severity
   * @returns {string} Console log level
   * @private
   */
  _getLogLevel(severity) {
    const levelMap = {
      [EventSeverity.DEBUG]: 'debug',
      [EventSeverity.INFO]: 'info',
      [EventSeverity.WARNING]: 'warn',
      [EventSeverity.ERROR]: 'error',
      [EventSeverity.CRITICAL]: 'error',
    };

    return levelMap[severity] || 'info';
  }

  /**
   * Hash payload for storage (to avoid storing sensitive data)
   *
   * @param {string} payload - Payload to hash
   * @returns {string} Hash
   * @private
   */
  _hashPayload(payload) {
    // Simple hash for demonstration (use crypto in production)
    let hash = 0;
    for (let i = 0; i < payload.length; i++) {
      const char = payload.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash;
    }
    return hash.toString(16);
  }

  /**
   * Get events by type
   *
   * @param {string} type - Event type
   * @param {Object} [options] - Filter options
   * @param {number} [options.limit=100] - Max events to return
   * @param {number} [options.since] - Timestamp to filter from
   * @returns {Array} Filtered events
   */
  getEventsByType(type, options = {}) {
    const { limit = 100, since } = options;

    let filtered = this.events.filter(e => e.type === type);

    if (since) {
      filtered = filtered.filter(e => e.timestamp >= since);
    }

    return filtered.slice(-limit);
  }

  /**
   * Get events by severity
   *
   * @param {string} severity - Severity level
   * @param {Object} [options] - Filter options
   * @returns {Array} Filtered events
   */
  getEventsBySeverity(severity, options = {}) {
    const { limit = 100, since } = options;

    let filtered = this.events.filter(e => e.severity === severity);

    if (since) {
      filtered = filtered.filter(e => e.timestamp >= since);
    }

    return filtered.slice(-limit);
  }

  /**
   * Get events by correlation ID
   *
   * @param {string} correlationId - Correlation ID
   * @returns {Array} Correlated events
   */
  getEventsByCorrelationId(correlationId) {
    return this.events.filter(e => e.correlationId === correlationId);
  }

  /**
   * Clear stored events
   */
  clearEvents() {
    this.events = [];
  }

  /**
   * Get event statistics
   *
   * @returns {Object} Event stats
   */
  getStats() {
    const stats = {
      total: this.events.length,
      bySeverity: {},
      byType: {},
      byCategory: {},
    };

    for (const event of this.events) {
      // Count by severity
      stats.bySeverity[event.severity] = (stats.bySeverity[event.severity] || 0) + 1;

      // Count by type
      stats.byType[event.type] = (stats.byType[event.type] || 0) + 1;

      // Count by category
      const category = event.attributes.category || 'unknown';
      stats.byCategory[category] = (stats.byCategory[category] || 0) + 1;
    }

    return stats;
  }
}

/**
 * Create custom events instance
 *
 * @param {Object} [config] - Configuration
 * @returns {CustomEvents} Events instance
 */
export function createCustomEvents(config = {}) {
  return new CustomEvents(config);
}

/**
 * Default custom events instance
 */
export const defaultCustomEvents = createCustomEvents();
