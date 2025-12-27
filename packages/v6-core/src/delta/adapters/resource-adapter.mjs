/**
 * V6 Delta Adapter - Resource Management
 *
 * Adapts resource allocation/deallocation to V6 Delta operations.
 * Handles agent resources, computational resources, and data resources.
 *
 * @module @unrdf/v6-core/delta/adapters/resource-adapter
 */

import { validateDelta } from '../schema.mjs';

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
  constructor(options = {}) {
    this.namespace = options.namespace || 'http://unrdf.io/resource/';
    this.graphUri = options.graphUri || 'http://unrdf.io/graph/resource';
  }

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
  allocate(resourceId, taskId, context = {}) {
    const resourceUri = `${this.namespace}${resourceId}`;
    const statusProperty = `${this.namespace}status`;
    const allocatedToProperty = `${this.namespace}allocatedTo`;
    const allocatedAtProperty = `${this.namespace}allocatedAt`;

    const operations = [
      {
        op: 'update',
        subject: resourceUri,
        predicate: statusProperty,
        oldObject: 'available',
        newObject: 'allocated',
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: resourceUri,
        predicate: allocatedToProperty,
        object: taskId,
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: resourceUri,
        predicate: allocatedAtProperty,
        object: new Date().toISOString(),
        graph: this.graphUri,
      },
    ];

    const delta = {
      id: this._generateUUID(),
      timestamp_iso: new Date().toISOString(),
      t_ns: BigInt(Date.now()) * 1_000_000n,
      operations,
      source: {
        package: '@unrdf/resource',
        actor: context.actor || 'resource-allocator',
        context: { resourceId, taskId, priority: context.priority },
      },
    };

    return validateDelta(delta);
  }

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
  deallocate(resourceId, taskId, context = {}) {
    const resourceUri = `${this.namespace}${resourceId}`;
    const statusProperty = `${this.namespace}status`;
    const allocatedToProperty = `${this.namespace}allocatedTo`;
    const deallocatedAtProperty = `${this.namespace}deallocatedAt`;

    const operations = [
      {
        op: 'update',
        subject: resourceUri,
        predicate: statusProperty,
        oldObject: 'allocated',
        newObject: 'available',
        graph: this.graphUri,
      },
      {
        op: 'delete',
        subject: resourceUri,
        predicate: allocatedToProperty,
        object: taskId,
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: resourceUri,
        predicate: deallocatedAtProperty,
        object: new Date().toISOString(),
        graph: this.graphUri,
      },
    ];

    const delta = {
      id: this._generateUUID(),
      timestamp_iso: new Date().toISOString(),
      t_ns: BigInt(Date.now()) * 1_000_000n,
      operations,
      source: {
        package: '@unrdf/resource',
        actor: context.actor || 'resource-allocator',
        context: { resourceId, taskId },
      },
    };

    return validateDelta(delta);
  }

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
  registerCapability(resourceId, capability, metadata = {}) {
    const resourceUri = `${this.namespace}${resourceId}`;
    const capabilityProperty = `${this.namespace}capability`;
    const capabilityUri = `${this.namespace}capability/${capability}`;
    const registeredAtProperty = `${this.namespace}registeredAt`;

    const operations = [
      {
        op: 'add',
        subject: resourceUri,
        predicate: capabilityProperty,
        object: capabilityUri,
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: capabilityUri,
        predicate: registeredAtProperty,
        object: new Date().toISOString(),
        graph: this.graphUri,
      },
    ];

    // Add metadata properties
    for (const [key, value] of Object.entries(metadata)) {
      operations.push({
        op: 'add',
        subject: capabilityUri,
        predicate: `${this.namespace}${key}`,
        object: String(value),
        graph: this.graphUri,
      });
    }

    const delta = {
      id: this._generateUUID(),
      timestamp_iso: new Date().toISOString(),
      t_ns: BigInt(Date.now()) * 1_000_000n,
      operations,
      source: {
        package: '@unrdf/resource',
        actor: 'capability-registry',
        context: { resourceId, capability, metadata },
      },
    };

    return validateDelta(delta);
  }

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
  updateAvailability(resourceId, available, context = {}) {
    const resourceUri = `${this.namespace}${resourceId}`;
    const statusProperty = `${this.namespace}status`;
    const updatedAtProperty = `${this.namespace}statusUpdatedAt`;

    const operations = [
      {
        op: 'add',
        subject: resourceUri,
        predicate: statusProperty,
        object: available ? 'available' : 'unavailable',
        graph: this.graphUri,
      },
      {
        op: 'add',
        subject: resourceUri,
        predicate: updatedAtProperty,
        object: new Date().toISOString(),
        graph: this.graphUri,
      },
    ];

    const delta = {
      id: this._generateUUID(),
      timestamp_iso: new Date().toISOString(),
      t_ns: BigInt(Date.now()) * 1_000_000n,
      operations,
      source: {
        package: '@unrdf/resource',
        actor: context.actor || 'availability-manager',
        context: { resourceId, available, reason: context.reason },
      },
    };

    return validateDelta(delta);
  }

  /**
   * Generate UUID (browser/Node.js compatible)
   *
   * @returns {string} UUID v4
   * @private
   */
  _generateUUID() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return crypto.randomUUID();
    }
    try {
      const crypto = require('crypto');
      return crypto.randomUUID();
    } catch {
      return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = (Math.random() * 16) | 0;
        const v = c === 'x' ? r : (r & 0x3) | 0x8;
        return v.toString(16);
      });
    }
  }
}

/**
 * Create resource adapter instance
 *
 * @param {Object} [options] - Configuration options
 * @returns {ResourceAdapter} Adapter instance
 *
 * @example
 * const adapter = createResourceAdapter({ namespace: 'http://my.org/res/' });
 */
export function createResourceAdapter(options = {}) {
  return new ResourceAdapter(options);
}
