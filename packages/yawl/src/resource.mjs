/**
 * YAWL Resource Pool - Resource allocation and role-based assignment
 */

import { z } from 'zod';

// =============================================================================
// Resource Schemas
// =============================================================================

const ResourceSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  roles: z.array(z.string()).default([]),
  priority: z.number().default(0),
  available: z.boolean().default(true),
  currentTaskId: z.string().optional(),
  completedTasks: z.number().default(0),
});

// =============================================================================
// YawlResourcePool Class
// =============================================================================

/**
 * Manages resources and their allocation to tasks
 */
export class YawlResourcePool {
  /**
   *
   */
  constructor() {
    /** @type {Map<string, Object>} */
    this.resources = new Map();
    /** @type {Array<{taskId: string, role?: string, resourceId?: string, queuedAt: bigint}>} */
    this.queue = [];
    /** @type {Map<string, Set<string>>} Role -> Resource IDs */
    this.roleIndex = new Map();
  }

  /**
   * Add a resource to the pool
   * @param {Object} resourceData - Resource definition
   * @returns {Object} Added resource
   */
  addResource(resourceData) {
    const resource = ResourceSchema.parse(resourceData);
    this.resources.set(resource.id, resource);

    // Update role index
    for (const role of resource.roles) {
      if (!this.roleIndex.has(role)) {
        this.roleIndex.set(role, new Set());
      }
      this.roleIndex.get(role).add(resource.id);
    }

    return resource;
  }

  /**
   * Remove a resource from the pool
   * @param {string} resourceId - Resource ID
   * @returns {boolean} True if removed
   */
  removeResource(resourceId) {
    const resource = this.resources.get(resourceId);
    if (!resource) return false;

    // Update role index
    for (const role of resource.roles) {
      const roleResources = this.roleIndex.get(role);
      if (roleResources) {
        roleResources.delete(resourceId);
        if (roleResources.size === 0) {
          this.roleIndex.delete(role);
        }
      }
    }

    return this.resources.delete(resourceId);
  }

  /**
   * Get a resource by ID
   * @param {string} resourceId - Resource ID
   * @returns {Object|undefined}
   */
  getResource(resourceId) {
    return this.resources.get(resourceId);
  }

  /**
   * Get all available resources
   * @returns {Object[]}
   */
  getAvailableResources() {
    return [...this.resources.values()].filter(r => r.available);
  }

  /**
   * Get available resources for a role
   * @param {string} role - Role name
   * @returns {Object[]}
   */
  getAvailableResourcesForRole(role) {
    const roleResourceIds = this.roleIndex.get(role);
    if (!roleResourceIds) return [];

    return [...roleResourceIds]
      .map(id => this.resources.get(id))
      .filter(r => r && r.available);
  }

  /**
   * Allocate best available resource for a task
   * @param {Object} params - Allocation parameters
   * @param {string} params.taskId - Task ID
   * @param {string} [params.role] - Required role
   * @param {string} [params.preferredResourceId] - Preferred resource
   * @returns {{resource: Object, queued: boolean}|null}
   */
  allocate({ taskId, role, preferredResourceId }) {
    // Try preferred resource first
    if (preferredResourceId) {
      const preferred = this.resources.get(preferredResourceId);
      if (preferred && preferred.available) {
        if (!role || preferred.roles.includes(role)) {
          preferred.available = false;
          preferred.currentTaskId = taskId;
          return { resource: preferred, queued: false };
        }
      }
    }

    // Get candidates
    let candidates = role
      ? this.getAvailableResourcesForRole(role)
      : this.getAvailableResources();

    if (candidates.length === 0) {
      // Queue the request
      this.queue.push({
        taskId,
        role,
        resourceId: preferredResourceId,
        queuedAt: BigInt(Date.now()),
      });
      return { resource: null, queued: true };
    }

    // Sort by priority (higher is better) then by completed tasks (load balancing)
    candidates.sort((a, b) => {
      if (b.priority !== a.priority) return b.priority - a.priority;
      return a.completedTasks - b.completedTasks;
    });

    const selected = candidates[0];
    selected.available = false;
    selected.currentTaskId = taskId;
    return { resource: selected, queued: false };
  }

  /**
   * Release a resource back to the pool
   * @param {string} resourceId - Resource ID
   * @returns {Object|null} Next queued task or null
   */
  release(resourceId) {
    const resource = this.resources.get(resourceId);
    if (!resource) return null;

    resource.available = true;
    resource.currentTaskId = undefined;
    resource.completedTasks++;

    // Check queue for waiting tasks
    for (let i = 0; i < this.queue.length; i++) {
      const queued = this.queue[i];

      // Check if this resource can fulfill the queued request
      if (queued.resourceId && queued.resourceId !== resourceId) continue;
      if (queued.role && !resource.roles.includes(queued.role)) continue;

      // Allocate to queued task
      this.queue.splice(i, 1);
      resource.available = false;
      resource.currentTaskId = queued.taskId;
      return { taskId: queued.taskId, resource };
    }

    return null;
  }

  /**
   * Get queue length
   * @returns {number}
   */
  getQueueLength() {
    return this.queue.length;
  }

  /**
   * Get queued tasks for a role
   * @param {string} role - Role name
   * @returns {Array}
   */
  getQueuedForRole(role) {
    return this.queue.filter(q => q.role === role);
  }

  /**
   * Check if pool is exhausted for a role
   * @param {string} role - Role name
   * @returns {boolean}
   */
  isExhaustedForRole(role) {
    const roleResourceIds = this.roleIndex.get(role);
    if (!roleResourceIds || roleResourceIds.size === 0) return true;

    return [...roleResourceIds].every(id => {
      const r = this.resources.get(id);
      return !r || !r.available;
    });
  }

  /**
   * Get pool statistics
   * @returns {Object}
   */
  getStats() {
    const resources = [...this.resources.values()];
    return {
      total: resources.length,
      available: resources.filter(r => r.available).length,
      busy: resources.filter(r => !r.available).length,
      queueLength: this.queue.length,
      roles: [...this.roleIndex.keys()],
    };
  }

  /**
   * Serialize to JSON
   * @returns {Object}
   */
  toJSON() {
    return {
      resources: [...this.resources.values()],
      queue: this.queue.map(q => ({
        ...q,
        queuedAt: q.queuedAt.toString(),
      })),
    };
  }

  /**
   * Create from JSON
   * @param {Object} json
   * @returns {YawlResourcePool}
   */
  static fromJSON(json) {
    const pool = new YawlResourcePool();
    for (const resource of json.resources) {
      pool.addResource(resource);
    }
    pool.queue = json.queue.map(q => ({
      ...q,
      queuedAt: BigInt(q.queuedAt),
    }));
    return pool;
  }
}
