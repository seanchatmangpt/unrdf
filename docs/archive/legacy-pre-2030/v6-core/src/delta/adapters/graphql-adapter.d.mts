/**
 * Create GraphQL adapter instance
 *
 * @param {Object} [options] - Configuration options
 * @returns {GraphQLAdapter} Adapter instance
 *
 * @example
 * const adapter = createGraphQLAdapter({
 *   namespace: 'http://my.org/entities/',
 *   typeMapping: {
 *     userName: 'http://schema.org/name',
 *     userEmail: 'http://schema.org/email'
 *   }
 * });
 */
export function createGraphQLAdapter(options?: any): GraphQLAdapter;
/**
 * GraphQL Delta Adapter
 *
 * Converts GraphQL mutations to deltas:
 * - createEntity → add operations
 * - updateEntity → update operations
 * - deleteEntity → delete operations
 * - Nested mutations → batch operations
 *
 * @class
 *
 * @example
 * import { GraphQLAdapter } from '@unrdf/v6-core/delta/adapters';
 * const adapter = new GraphQLAdapter();
 * const delta = adapter.mutationToDelta('createUser', { name: 'Alice', email: 'alice@example.com' });
 */
export class GraphQLAdapter {
    /**
     * @param {Object} [options] - Configuration options
     * @param {string} [options.namespace] - RDF namespace for entity URIs
     * @param {string} [options.graphUri] - Graph URI for entity state
     * @param {Object} [options.typeMapping] - GraphQL type to RDF predicate mapping
     */
    constructor(options?: {
        namespace?: string;
        graphUri?: string;
        typeMapping?: any;
    });
    namespace: string;
    graphUri: string;
    typeMapping: any;
    /**
     * Convert GraphQL mutation to delta
     *
     * @param {string} mutationName - Mutation name (e.g., 'createUser', 'updateTask')
     * @param {Object} input - Mutation input object
     * @param {Object} [context] - Additional context metadata
     * @returns {Object} Delta for mutation
     *
     * @example
     * const delta = adapter.mutationToDelta('createUser', {
     *   id: 'user-1',
     *   name: 'Alice',
     *   email: 'alice@example.com'
     * });
     */
    mutationToDelta(mutationName: string, input: any, context?: any): any;
    /**
     * Create delta for entity creation
     *
     * @param {string} entityType - Entity type (e.g., 'User', 'Task')
     * @param {Object} attributes - Entity attributes
     * @param {Object} [context] - Additional context metadata
     * @returns {Object} Delta for creation
     *
     * @example
     * const delta = adapter.createEntity('User', {
     *   id: 'user-1',
     *   name: 'Alice',
     *   email: 'alice@example.com'
     * });
     */
    createEntity(entityType: string, attributes: any, context?: any): any;
    /**
     * Create delta for entity update
     *
     * @param {string} entityType - Entity type (e.g., 'User', 'Task')
     * @param {Object} updates - Updated attributes (must include id)
     * @param {Object} [context] - Additional context metadata
     * @returns {Object} Delta for update
     *
     * @example
     * const delta = adapter.updateEntity('User', {
     *   id: 'user-1',
     *   name: 'Alice Smith',
     *   email: 'alice.smith@example.com'
     * });
     */
    updateEntity(entityType: string, updates: any, context?: any): any;
    /**
     * Create delta for entity deletion
     *
     * @param {string} entityType - Entity type (e.g., 'User', 'Task')
     * @param {string} entityId - Entity identifier
     * @param {Object} [context] - Additional context metadata
     * @returns {Object} Delta for deletion
     *
     * @example
     * const delta = adapter.deleteEntity('User', 'user-1');
     */
    deleteEntity(entityType: string, entityId: string, context?: any): any;
    /**
     * Parse mutation name to extract operation and entity type
     *
     * Examples:
     * - createUser → {type: 'create', entityType: 'User'}
     * - updateTask → {type: 'update', entityType: 'Task'}
     * - deleteProject → {type: 'delete', entityType: 'Project'}
     *
     * @param {string} mutationName - Mutation name
     * @returns {Object} Parsed operation
     * @private
     */
    private _parseMutationName;
    /**
     * Generate UUID (browser/Node.js compatible)
     *
     * @param {Object} [context={}] - Execution context with uuid/deltaId/random for determinism
     * @returns {string} UUID v4
     * @private
     */
    private _generateUUID;
}
