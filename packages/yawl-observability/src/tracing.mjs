/**
 * @file YAWL Tracing - OpenTelemetry distributed tracing for workflow execution
 * @module @unrdf/yawl-observability/tracing
 *
 * @description
 * Creates OTEL spans for workflow execution with:
 * - Case-level spans (entire workflow execution)
 * - Task-level spans (individual task execution)
 * - Receipt correlation (cryptographic proof linking)
 * - Workflow context propagation (baggage, attributes)
 */

import { trace, context, SpanStatusCode, SpanKind } from '@opentelemetry/api';
import { z } from 'zod';

// =============================================================================
// Configuration Schema
// =============================================================================

/**
 * Tracing configuration schema
 */
export const TracingConfigSchema = z.object({
  /** Name of the tracer */
  tracerName: z.string().default('@unrdf/yawl'),
  /** Version of the tracer */
  tracerVersion: z.string().optional(),
  /** Enable automatic span context propagation */
  enableContextPropagation: z.boolean().default(true),
  /** Include receipt hashes in span attributes */
  includeReceiptHashes: z.boolean().default(true),
  /** Include task input/output data in spans (may be large) */
  includeTaskData: z.boolean().default(false),
  /** Maximum size of task data to include (characters) */
  maxTaskDataSize: z.number().positive().default(1000),
});

// =============================================================================
// YAWLTracer Class
// =============================================================================

/**
 * OpenTelemetry tracer for YAWL workflow engine
 *
 * Creates distributed tracing spans for workflow execution with:
 * - Parent case span encompassing entire workflow
 * - Child task spans for each task execution
 * - Receipt hash correlation for cryptographic proof linking
 * - Workflow context attributes (workflow ID, case ID, task ID)
 *
 * @example
 * ```javascript
 * import { createWorkflowEngine } from '@unrdf/yawl';
 * import { YAWLTracer } from '@unrdf/yawl-observability';
 *
 * const engine = createWorkflowEngine();
 * const tracer = new YAWLTracer(engine, {
 *   includeReceiptHashes: true,
 *   includeTaskData: true
 * });
 *
 * // Traces are automatically created for all workflow execution
 * // Export to Jaeger, Zipkin, or other OTEL-compatible backends
 * ```
 */
export class YAWLTracer {
  /**
   * @param {Object} engine - YAWL WorkflowEngine instance
   * @param {Object} [config={}] - Tracing configuration
   */
  constructor(engine, config = {}) {
    const validated = TracingConfigSchema.parse(config);

    /** @type {Object} YAWL engine reference */
    this.engine = engine;
    /** @type {Object} Configuration */
    this.config = validated;
    /** @type {Object} OpenTelemetry tracer */
    this.tracer = trace.getTracer(
      validated.tracerName,
      validated.tracerVersion || '1.0.0'
    );

    /** @type {Map<string, Object>} Active case spans */
    this._caseSpans = new Map();
    /** @type {Map<string, Object>} Active task spans */
    this._taskSpans = new Map();
    /** @type {Map<string, Object>} Span contexts for propagation */
    this._spanContexts = new Map();

    // Attach event listeners
    this._attachEventListeners();
  }

  // ===========================================================================
  // Event Listeners
  // ===========================================================================

  /**
   * Attach event listeners to YAWL engine
   * @private
   */
  _attachEventListeners() {
    const engine = this.engine;

    // Case lifecycle
    engine.on('CASE_CREATED', (event) => {
      this._startCaseSpan(event);
    });

    engine.on('CASE_COMPLETED', (event) => {
      this._endCaseSpan(event, SpanStatusCode.OK);
    });

    // Task lifecycle
    engine.on('TASK_ENABLED', (event) => {
      this._recordTaskEvent(event, 'task.enabled');
    });

    engine.on('TASK_STARTED', (event) => {
      this._startTaskSpan(event);
    });

    engine.on('TASK_COMPLETED', (event) => {
      this._endTaskSpan(event, SpanStatusCode.OK);
    });

    engine.on('TASK_CANCELLED', (event) => {
      this._endTaskSpan(event, SpanStatusCode.ERROR, 'Task cancelled');
    });

    engine.on('TASK_TIMEOUT', (event) => {
      this._endTaskSpan(event, SpanStatusCode.ERROR, 'Task timeout');
    });

    // Resource allocation
    engine.on('RESOURCE_ALLOCATED', (event) => {
      this._recordResourceEvent(event, 'resource.allocated');
    });

    engine.on('RESOURCE_RELEASED', (event) => {
      this._recordResourceEvent(event, 'resource.released');
    });
  }

  // ===========================================================================
  // Case Span Management
  // ===========================================================================

  /**
   * Start a span for case execution
   * @private
   * @param {Object} event - CASE_CREATED event
   */
  _startCaseSpan(event) {
    const span = this.tracer.startSpan('workflow.case', {
      kind: SpanKind.INTERNAL,
      attributes: {
        'workflow.id': event.workflowId,
        'workflow.case.id': event.caseId,
        'workflow.case.status': 'created',
      },
    });

    // Store span and create context
    this._caseSpans.set(event.caseId, span);
    const ctx = trace.setSpan(context.active(), span);
    this._spanContexts.set(event.caseId, ctx);

    span.addEvent('case.created', {
      'case.data': JSON.stringify(event.data || {}).substring(0, this.config.maxTaskDataSize),
    });
  }

  /**
   * End a case span
   * @private
   * @param {Object} event - CASE_COMPLETED event
   * @param {SpanStatusCode} statusCode - Span status
   */
  _endCaseSpan(event, statusCode) {
    const span = this._caseSpans.get(event.caseId);
    if (!span) return;

    span.setAttributes({
      'workflow.case.status': 'completed',
    });

    span.addEvent('case.completed');

    span.setStatus({ code: statusCode });
    span.end();

    // Cleanup
    this._caseSpans.delete(event.caseId);
    this._spanContexts.delete(event.caseId);
  }

  // ===========================================================================
  // Task Span Management
  // ===========================================================================

  /**
   * Start a span for task execution
   * @private
   * @param {Object} event - TASK_STARTED event
   */
  _startTaskSpan(event) {
    const caseId = event.caseId;
    const workItemId = event.workItemId;
    const taskId = event.taskId || this._getTaskIdFromWorkItem(caseId, workItemId);
    const workflowId = event.workflowId || this._getWorkflowId(caseId);

    // Get parent context from case span
    const parentContext = this._spanContexts.get(caseId) || context.active();

    // Start task span as child of case span
    const span = this.tracer.startSpan(
      `task.${taskId}`,
      {
        kind: SpanKind.INTERNAL,
        attributes: {
          'workflow.id': workflowId,
          'workflow.case.id': caseId,
          'workflow.task.id': taskId,
          'workflow.task.work_item_id': workItemId,
          'workflow.task.status': 'started',
          'workflow.task.resource_id': event.resourceId || 'none',
          'workflow.task.actor': event.actor || 'system',
        },
      },
      parentContext
    );

    // Store span
    this._taskSpans.set(workItemId, span);

    span.addEvent('task.started', {
      'task.resource_id': event.resourceId || 'none',
      'task.actor': event.actor || 'system',
    });
  }

  /**
   * End a task span
   * @private
   * @param {Object} event - TASK_COMPLETED/CANCELLED/TIMEOUT event
   * @param {SpanStatusCode} statusCode - Span status
   * @param {string} [statusMessage] - Optional status message
   */
  _endTaskSpan(event, statusCode, statusMessage) {
    const span = this._taskSpans.get(event.workItemId);
    if (!span) return;

    const taskId = event.taskId || this._getTaskIdFromWorkItem(event.caseId, event.workItemId);

    // Update attributes
    span.setAttributes({
      'workflow.task.status': statusCode === SpanStatusCode.OK ? 'completed' : 'failed',
    });

    // Add receipt hash if available and configured
    if (this.config.includeReceiptHashes && event.hookReceipt) {
      span.setAttributes({
        'workflow.receipt.hash': event.hookReceipt.receiptHash,
        'workflow.receipt.previous_hash': event.hookReceipt.previousReceiptHash || 'genesis',
        'workflow.receipt.event_type': event.hookReceipt.eventType,
      });
    }

    // Add task output if configured
    if (this.config.includeTaskData && event.output) {
      const outputStr = JSON.stringify(event.output).substring(0, this.config.maxTaskDataSize);
      span.setAttributes({
        'workflow.task.output': outputStr,
      });
    }

    // Add event
    const eventName = statusCode === SpanStatusCode.OK ? 'task.completed' : 'task.failed';
    span.addEvent(eventName, {
      'task.status_message': statusMessage || 'success',
    });

    // Set span status
    span.setStatus({
      code: statusCode,
      message: statusMessage,
    });

    span.end();

    // Cleanup
    this._taskSpans.delete(event.workItemId);
  }

  /**
   * Record a task event (without creating a span)
   * @private
   * @param {Object} event - Task event
   * @param {string} eventName - Event name
   */
  _recordTaskEvent(event, eventName) {
    const caseId = event.caseId;
    const span = this._caseSpans.get(caseId);

    if (span) {
      span.addEvent(eventName, {
        'workflow.task.id': event.taskId,
        'workflow.task.work_item_id': event.workItemId,
        'workflow.task.actor': event.actor || 'system',
      });
    }
  }

  /**
   * Record a resource event
   * @private
   * @param {Object} event - Resource event
   * @param {string} eventName - Event name
   */
  _recordResourceEvent(event, eventName) {
    const caseId = event.caseId;
    const span = this._caseSpans.get(caseId);

    if (span) {
      span.addEvent(eventName, {
        'resource.id': event.resourceId,
        'resource.role': event.role || 'default',
        'workflow.task.work_item_id': event.workItemId || 'none',
      });
    }
  }

  // ===========================================================================
  // Receipt Correlation
  // ===========================================================================

  /**
   * Create a span linked to a receipt hash
   *
   * This creates a span that is cryptographically linked to a YAWL receipt,
   * enabling distributed tracing across systems with proof of execution.
   *
   * @param {string} spanName - Span name
   * @param {Object} receipt - YAWL receipt object
   * @param {Function} fn - Function to execute within span
   * @returns {Promise<*>} Result of fn
   *
   * @example
   * ```javascript
   * const result = await tracer.spanWithReceipt(
   *   'external.approval',
   *   receipt,
   *   async () => {
   *     return await externalApprovalService.approve(caseId);
   *   }
   * );
   * ```
   */
  async spanWithReceipt(spanName, receipt, fn) {
    const span = this.tracer.startSpan(spanName, {
      kind: SpanKind.INTERNAL,
      attributes: {
        'workflow.receipt.hash': receipt.receiptHash,
        'workflow.receipt.previous_hash': receipt.previousReceiptHash || 'genesis',
        'workflow.receipt.event_type': receipt.eventType,
        'workflow.receipt.case_id': receipt.caseId,
        'workflow.receipt.task_id': receipt.taskId,
        'workflow.receipt.timestamp': receipt.timestamp_iso,
      },
    });

    try {
      const result = await context.with(trace.setSpan(context.active(), span), fn);
      span.setStatus({ code: SpanStatusCode.OK });
      return result;
    } catch (error) {
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      span.recordException(error);
      throw error;
    } finally {
      span.end();
    }
  }

  // ===========================================================================
  // Helper Methods
  // ===========================================================================

  /**
   * Get workflow ID from case ID
   * @private
   * @param {string} caseId - Case ID
   * @returns {string} Workflow ID
   */
  _getWorkflowId(caseId) {
    const yawlCase = this.engine.cases.get(caseId);
    return yawlCase ? yawlCase.workflowId : 'unknown';
  }

  /**
   * Get task ID from work item ID
   * @private
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @returns {string} Task definition ID
   */
  _getTaskIdFromWorkItem(caseId, workItemId) {
    const yawlCase = this.engine.cases.get(caseId);
    if (!yawlCase) return 'unknown';

    const workItem = yawlCase.workItems.get(workItemId);
    return workItem ? workItem.taskDefId : 'unknown';
  }

  // ===========================================================================
  // Public API
  // ===========================================================================

  /**
   * Get active span for a case
   * @param {string} caseId - Case ID
   * @returns {Object|undefined} Active span or undefined
   */
  getCaseSpan(caseId) {
    return this._caseSpans.get(caseId);
  }

  /**
   * Get active span for a task
   * @param {string} workItemId - Work item ID
   * @returns {Object|undefined} Active span or undefined
   */
  getTaskSpan(workItemId) {
    return this._taskSpans.get(workItemId);
  }

  /**
   * Create a custom span within a case context
   * @param {string} caseId - Case ID
   * @param {string} spanName - Span name
   * @param {Function} fn - Function to execute
   * @returns {Promise<*>} Result of fn
   */
  async spanInCase(caseId, spanName, fn) {
    const parentContext = this._spanContexts.get(caseId) || context.active();
    const span = this.tracer.startSpan(spanName, {}, parentContext);

    try {
      const result = await context.with(trace.setSpan(parentContext, span), fn);
      span.setStatus({ code: SpanStatusCode.OK });
      return result;
    } catch (error) {
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      span.recordException(error);
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Remove all event listeners (cleanup)
   */
  destroy() {
    // End all active spans
    for (const span of this._caseSpans.values()) {
      span.end();
    }
    for (const span of this._taskSpans.values()) {
      span.end();
    }

    // Remove listeners
    this.engine.removeAllListeners('CASE_CREATED');
    this.engine.removeAllListeners('CASE_COMPLETED');
    this.engine.removeAllListeners('TASK_ENABLED');
    this.engine.removeAllListeners('TASK_STARTED');
    this.engine.removeAllListeners('TASK_COMPLETED');
    this.engine.removeAllListeners('TASK_CANCELLED');
    this.engine.removeAllListeners('TASK_TIMEOUT');
    this.engine.removeAllListeners('RESOURCE_ALLOCATED');
    this.engine.removeAllListeners('RESOURCE_RELEASED');

    // Clear maps
    this._caseSpans.clear();
    this._taskSpans.clear();
    this._spanContexts.clear();
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a YAWLTracer instance
 *
 * @param {Object} engine - YAWL WorkflowEngine instance
 * @param {Object} [config={}] - Tracing configuration
 * @returns {YAWLTracer} Tracer instance
 *
 * @example
 * ```javascript
 * const tracer = createTracer(engine, {
 *   includeReceiptHashes: true,
 *   includeTaskData: false
 * });
 * ```
 */
export function createTracer(engine, config = {}) {
  return new YAWLTracer(engine, config);
}
