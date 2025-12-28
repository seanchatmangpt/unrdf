/**
 * V6 Delta Adapter - GraphQL Mutations
 *
 * Adapts GraphQL mutation operations to V6 Delta operations.
 * Provides seamless integration with GraphQL APIs.
 *
 * @module @unrdf/v6-core/delta/adapters/graphql-adapter
 */

import { validateDelta } from '../schema.mjs';
import { createGraphQLAdapterParamsSchema } from './graphql-adapter.schema.mjs';

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
  constructor(options = {}) {
    this.namespace = options.namespace || 'http://unrdf.io/entity/';
    this.graphUri = options.graphUri || 'http://unrdf.io/graph/graphql';
    this.typeMapping = options.typeMapping || {};
  }

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
  mutationToDelta(mutationName, input, context = {}) {
    // Parse mutation name to determine operation type
    const operation = this._parseMutationName(mutationName);

    if (operation.type === 'create') {
      return this.createEntity(operation.entityType, input, context);
    } else if (operation.type === 'update') {
      return this.updateEntity(operation.entityType, input, context);
    } else if (operation.type === 'delete') {
      return this.deleteEntity(operation.entityType, input.id, context);
    }

    throw new Error(`Unknown mutation type: ${mutationName}`);
  }

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
  createEntity(entityType, attributes, context = {}) {
    // Extract temporal and random values from context (deterministic when provided)
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n,
      timestamp_iso = new Date().toISOString(),
      uuid,
      entityUuid
    } = context;

    const entityId = attributes.id || entityUuid || this._generateUUID({});
    const entityUri = `${this.namespace}${entityType}/${entityId}`;
    const typeProperty = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

    const operations = [
      {
        op: 'add',
        subject: entityUri,
        predicate: typeProperty,
        object: `${this.namespace}${entityType}`,
        graph: this.graphUri,
      },
    ];

    // Convert attributes to RDF triples
    for (const [key, value] of Object.entries(attributes)) {
      if (key === 'id') continue; // Already used in URI

      const predicate = this.typeMapping[key] || `${this.namespace}${key}`;
      operations.push({
        op: 'add',
        subject: entityUri,
        predicate,
        object: String(value),
        graph: this.graphUri,
      });
    }

    const delta = {
      id: uuid || this._generateUUID({}),
      timestamp_iso,
      t_ns,
      operations,
      source: {
        package: '@unrdf/graphql',
        actor: context.actor || 'graphql-api',
        context: { entityType, entityId, mutation: 'create' },
      },
    };

    return validateDelta(delta);
  }

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
  updateEntity(entityType, updates, context = {}) {
    if (!updates.id) {
      throw new Error('Update mutation must include entity id');
    }

    // Extract temporal and random values from context (deterministic when provided)
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n,
      timestamp_iso = new Date().toISOString(),
      uuid
    } = context;

    const entityId = updates.id;
    const entityUri = `${this.namespace}${entityType}/${entityId}`;
    const operations = [];

    // Convert updates to RDF update operations
    for (const [key, value] of Object.entries(updates)) {
      if (key === 'id') continue;

      const predicate = this.typeMapping[key] || `${this.namespace}${key}`;

      // For updates, we add the new value
      // (actual update logic requires knowing old value, handled by reconciliation)
      operations.push({
        op: 'add',
        subject: entityUri,
        predicate,
        object: String(value),
        graph: this.graphUri,
      });
    }

    const delta = {
      id: uuid || this._generateUUID({}),
      timestamp_iso,
      t_ns,
      operations,
      source: {
        package: '@unrdf/graphql',
        actor: context.actor || 'graphql-api',
        context: { entityType, entityId, mutation: 'update' },
      },
    };

    return validateDelta(delta);
  }

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
  deleteEntity(entityType, entityId, context = {}) {
    // Extract temporal and random values from context (deterministic when provided)
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n,
      timestamp_iso = new Date().toISOString(),
      uuid
    } = context;

    const entityUri = `${this.namespace}${entityType}/${entityId}`;
    const typeProperty = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

    // Delete type triple (marks entity as deleted)
    const operations = [
      {
        op: 'delete',
        subject: entityUri,
        predicate: typeProperty,
        object: `${this.namespace}${entityType}`,
        graph: this.graphUri,
      },
    ];

    const delta = {
      id: uuid || this._generateUUID({}),
      timestamp_iso,
      t_ns,
      operations,
      source: {
        package: '@unrdf/graphql',
        actor: context.actor || 'graphql-api',
        context: { entityType, entityId, mutation: 'delete' },
      },
    };

    return validateDelta(delta);
  }

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
  _parseMutationName(mutationName) {
    const match = mutationName.match(/^(create|update|delete)(.+)$/);

    if (!match) {
      throw new Error(`Invalid mutation name format: ${mutationName}`);
    }

    return {
      type: match[1],
      entityType: match[2],
    };
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
export function createGraphQLAdapter(options = {}) {
  const [validOptions] = createGraphQLAdapterParamsSchema.parse([options]);
  return new GraphQLAdapter(validOptions);
}
