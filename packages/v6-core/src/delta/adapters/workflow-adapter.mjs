/**
 * V6 Delta Adapter - YAWL Workflow
 *
 * Adapts YAWL workflow state transitions to V6 Delta operations.
 * Converts workflow mutations into structured deltas.
 *
 * @module @unrdf/v6-core/delta/adapters/workflow-adapter
 *
 * ADAPTER PATTERN:
 * - workflowMutation → createDelta → ΔGate.propose → receipt
 * - NO direct state mutation
 * - ALL workflow changes flow through Δ
 */

import { validateDelta } from '../schema.mjs';
import { createWorkflowAdapterParamsSchema } from './workflow-adapter.schema.mjs';

/**
 * Workflow Delta Adapter
 *
 * Converts YAWL workflow operations to deltas:
 * - Task state transitions (enabled → executing → completed)
 * - Workflow instance creation/completion
 * - Resource assignments
 * - Cancellation region activations
 *
 * @class
 *
 * @example
 * import { WorkflowAdapter } from '@unrdf/v6-core/delta/adapters';
 * const adapter = new WorkflowAdapter();
 * const delta = adapter.taskTransition('task-1', 'enabled', 'executing');
 */
export class WorkflowAdapter {
  /**
   * @param {Object} [options] - Configuration options
   * @param {string} [options.namespace] - RDF namespace for workflow URIs
   * @param {string} [options.graphUri] - Graph URI for workflow state
   */
  constructor(options = {}) {
    this.namespace = options.namespace || 'http://unrdf.io/workflow/';
    this.graphUri = options.graphUri || 'http://unrdf.io/graph/workflow';
  }

  /**
   * Create delta for task state transition
   *
   * Transitions: enabled → executing → completed
   * Also: enabled → cancelled, executing → cancelled
   *
   * @param {string} taskId - Task identifier
   * @param {string} fromState - Current state
   * @param {string} toState - Target state
   * @param {Object} [context] - Additional context metadata
   * @returns {Object} Delta for transition
   *
   * @example
   * const delta = adapter.taskTransition('task-1', 'enabled', 'executing', {
   *   actor: 'workflow-engine',
   *   workflowId: 'wf-123'
   * });
   */
  taskTransition(taskId, fromState, toState, context = {}) {
    // Extract temporal and random values from context (deterministic when provided)
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n,
      timestamp_iso = new Date().toISOString(),
      uuid
    } = context;

    const taskUri = `${this.namespace}task/${taskId}`;
    const stateProperty = `${this.namespace}state`;
    const timestampProperty = `${this.namespace}stateChangedAt`;

    const operations = [
      {
        op: 'update',
        subject: taskUri,
        predicate: stateProperty,
        oldObject: fromState,
        newObject: toState,
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: taskUri,
        predicate: timestampProperty,
        object: timestamp_iso,
        graph: this.graphUri,
      },
    ];

    const delta = {
      id: uuid || this._generateUUID({}),
      timestamp_iso,
      t_ns,
      operations,
      source: {
        package: '@unrdf/yawl',
        actor: context.actor || 'workflow-engine',
        context: {
          taskId,
          workflowId: context.workflowId,
          transition: `${fromState} → ${toState}`,
        },
      },
    };

    return validateDelta(delta);
  }

  /**
   * Create delta for workflow instance creation
   *
   * Initializes workflow instance state in ontology.
   *
   * @param {string} workflowId - Workflow instance identifier
   * @param {string} specId - Workflow specification identifier
   * @param {Object} [context] - Additional context metadata
   * @returns {Object} Delta for creation
   *
   * @example
   * const delta = adapter.workflowCreation('wf-123', 'order-processing-v1');
   */
  workflowCreation(workflowId, specId, context = {}) {
    // Extract temporal and random values from context (deterministic when provided)
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n,
      timestamp_iso = new Date().toISOString(),
      uuid
    } = context;

    const workflowUri = `${this.namespace}workflow/${workflowId}`;
    const typeProperty = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
    const specProperty = `${this.namespace}specification`;
    const stateProperty = `${this.namespace}state`;
    const createdAtProperty = `${this.namespace}createdAt`;

    const operations = [
      {
        op: 'add',
        subject: workflowUri,
        predicate: typeProperty,
        object: `${this.namespace}WorkflowInstance`,
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: workflowUri,
        predicate: specProperty,
        object: specId,
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: workflowUri,
        predicate: stateProperty,
        object: 'running',
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: workflowUri,
        predicate: createdAtProperty,
        object: timestamp_iso,
        graph: this.graphUri,
      },
    ];

    const delta = {
      id: uuid || this._generateUUID({}),
      timestamp_iso,
      t_ns,
      operations,
      source: {
        package: '@unrdf/yawl',
        actor: context.actor || 'workflow-engine',
        context: { workflowId, specId },
      },
    };

    return validateDelta(delta);
  }

  /**
   * Create delta for resource assignment
   *
   * Assigns resource to task instance.
   *
   * @param {string} taskId - Task identifier
   * @param {string} resourceId - Resource identifier
   * @param {Object} [context] - Additional context metadata
   * @returns {Object} Delta for assignment
   *
   * @example
   * const delta = adapter.resourceAssignment('task-1', 'agent-42');
   */
  resourceAssignment(taskId, resourceId, context = {}) {
    // Extract temporal and random values from context (deterministic when provided)
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n,
      timestamp_iso = new Date().toISOString(),
      uuid
    } = context;

    const taskUri = `${this.namespace}task/${taskId}`;
    const resourceProperty = `${this.namespace}assignedTo`;
    const assignedAtProperty = `${this.namespace}assignedAt`;

    const operations = [
      {
        op: 'add',
        subject: taskUri,
        predicate: resourceProperty,
        object: resourceId,
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: taskUri,
        predicate: assignedAtProperty,
        object: timestamp_iso,
        graph: this.graphUri,
      },
    ];

    const delta = {
      id: uuid || this._generateUUID({}),
      timestamp_iso,
      t_ns,
      operations,
      source: {
        package: '@unrdf/yawl',
        actor: context.actor || 'resource-allocator',
        context: { taskId, resourceId },
      },
    };

    return validateDelta(delta);
  }

  /**
   * Create delta for cancellation region activation
   *
   * Cancels all tasks in a cancellation region.
   *
   * @param {string} regionId - Cancellation region identifier
   * @param {string[]} taskIds - Task IDs in region
   * @param {Object} [context] - Additional context metadata
   * @returns {Object} Delta for cancellation
   *
   * @example
   * const delta = adapter.cancellationRegion('region-1', ['task-2', 'task-3']);
   */
  cancellationRegion(regionId, taskIds, context = {}) {
    // Extract temporal and random values from context (deterministic when provided)
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n,
      timestamp_iso = new Date().toISOString(),
      uuid
    } = context;

    const stateProperty = `${this.namespace}state`;
    const cancelledAtProperty = `${this.namespace}cancelledAt`;
    const cancelledByProperty = `${this.namespace}cancelledBy`;

    const operations = [];

    for (const taskId of taskIds) {
      const taskUri = `${this.namespace}task/${taskId}`;
      operations.push(
        {
          op: 'add',
          subject: taskUri,
          predicate: stateProperty,
          object: 'cancelled',
          graph: this.graphUri,
        },
        {
          op: 'add',
          subject: taskUri,
          predicate: cancelledAtProperty,
          object: timestamp_iso,
          graph: this.graphUri,
        },
        {
          op: 'add',
          subject: taskUri,
          predicate: cancelledByProperty,
          object: regionId,
          graph: this.graphUri,
        }
      );
    }

    const delta = {
      id: uuid || this._generateUUID({}),
      timestamp_iso,
      t_ns,
      operations,
      source: {
        package: '@unrdf/yawl',
        actor: context.actor || 'cancellation-handler',
        context: { regionId, taskIds },
      },
    };

    return validateDelta(delta);
  }

  /**
   * Generate UUID (browser/Node.js compatible)
   *
   * @param {Object} [context={}] - Execution context with uuid/deltaId/random for determinism
   * @returns {string} UUID v4
   * @private
   */
  _generateUUID(context = {}) {
    const { uuid, deltaId, random = Math.random } = context;

    // Use context-provided UUID for determinism if available
    if (uuid) return uuid;
    if (deltaId) return deltaId;

    // Single crypto check and usage
    const cryptoAPI = typeof crypto !== 'undefined' ? crypto : (() => {
      try { return require('crypto'); } catch { return null; }
    })();

    if (cryptoAPI && cryptoAPI.randomUUID) {
      return cryptoAPI.randomUUID();
    }

    // Fallback with context-injectable random
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = (random() * 16) | 0;
      const v = c === 'x' ? r : (r & 0x3) | 0x8;
      return v.toString(16);
    });
  }
}

/**
 * Create workflow adapter instance
 *
 * Factory function for creating WorkflowAdapter.
 *
 * @param {Object} [options] - Configuration options
 * @returns {WorkflowAdapter} Adapter instance
 *
 * @example
 * const adapter = createWorkflowAdapter({ namespace: 'http://my.org/wf/' });
 */
export function createWorkflowAdapter(options = {}) {
  const [validOptions] = createWorkflowAdapterParamsSchema.parse([options]);
  return new WorkflowAdapter(validOptions);
}
