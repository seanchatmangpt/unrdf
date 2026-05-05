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
export function createWorkflowAdapter(options?: any): WorkflowAdapter;
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
    constructor(options?: {
        namespace?: string;
        graphUri?: string;
    });
    namespace: string;
    graphUri: string;
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
    taskTransition(taskId: string, fromState: string, toState: string, context?: any): any;
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
    workflowCreation(workflowId: string, specId: string, context?: any): any;
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
    resourceAssignment(taskId: string, resourceId: string, context?: any): any;
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
    cancellationRegion(regionId: string, taskIds: string[], context?: any): any;
    /**
     * Generate UUID (browser/Node.js compatible)
     *
     * @param {Object} [context={}] - Execution context with uuid/deltaId/random for determinism
     * @returns {string} UUID v4
     * @private
     */
    private _generateUUID;
}
