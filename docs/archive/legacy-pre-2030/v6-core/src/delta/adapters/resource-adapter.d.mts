/**
 * Create resource adapter instance
 *
 * @param {Object} [options] - Configuration options
 * @returns {ResourceAdapter} Adapter instance
 *
 * @example
 * const adapter = createResourceAdapter({ namespace: 'http://my.org/res/' });
 */
export function createResourceAdapter(options?: any): ResourceAdapter;
/**
 * Resource Delta Adapter
 *
 * Converts resource operations to deltas:
 * - Resource allocation/deallocation
 * - Capability registration
 * - Availability updates
 * - Resource pool management
 *
 * @class
 *
 * @example
 * import { ResourceAdapter } from '@unrdf/v6-core/delta/adapters';
 * const adapter = new ResourceAdapter();
 * const delta = adapter.allocate('agent-42', 'task-1');
 */
export class ResourceAdapter {
    /**
     * @param {Object} [options] - Configuration options
     * @param {string} [options.namespace] - RDF namespace for resource URIs
     * @param {string} [options.graphUri] - Graph URI for resource state
     */
    constructor(options?: {
        namespace?: string;
        graphUri?: string;
    });
    namespace: string;
    graphUri: string;
    /**
     * Create delta for resource allocation
     *
     * @param {string} resourceId - Resource identifier
     * @param {string} taskId - Task identifier
     * @param {Object} [context] - Additional context metadata
     * @returns {Object} Delta for allocation
     *
     * @example
     * const delta = adapter.allocate('agent-42', 'task-1', { priority: 'high' });
     */
    allocate(resourceId: string, taskId: string, context?: any): any;
    /**
     * Create delta for resource deallocation
     *
     * @param {string} resourceId - Resource identifier
     * @param {string} taskId - Task identifier
     * @param {Object} [context] - Additional context metadata
     * @returns {Object} Delta for deallocation
     *
     * @example
     * const delta = adapter.deallocate('agent-42', 'task-1');
     */
    deallocate(resourceId: string, taskId: string, context?: any): any;
    /**
     * Create delta for capability registration
     *
     * @param {string} resourceId - Resource identifier
     * @param {string} capability - Capability identifier
     * @param {Object} [metadata] - Capability metadata
     * @returns {Object} Delta for registration
     *
     * @example
     * const delta = adapter.registerCapability('agent-42', 'code-generation', {
     *   level: 'expert',
     *   languages: ['javascript', 'python']
     * });
     */
    registerCapability(resourceId: string, capability: string, metadata?: any): any;
    /**
     * Create delta for availability update
     *
     * @param {string} resourceId - Resource identifier
     * @param {boolean} available - Availability status
     * @param {Object} [context] - Additional context metadata
     * @returns {Object} Delta for update
     *
     * @example
     * const delta = adapter.updateAvailability('agent-42', false, { reason: 'maintenance' });
     */
    updateAvailability(resourceId: string, available: boolean, context?: any): any;
    /**
     * Generate UUID (browser/Node.js compatible)
     *
     * @param {Object} [context={}] - Execution context with uuid/deltaId/random for determinism
     * @returns {string} UUID v4
     * @private
     */
    private _generateUUID;
}
