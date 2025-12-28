/**
 * @file YAWL Cancellation Regions
 * @module yawl/cancellation/regions
 *
 * @description
 * Manages cancellation regions - groups of tasks that cancel together.
 * Provides region hierarchy, task membership, and region-based cancellation.
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';
import { CancellationError } from '../../errors.mjs';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for cancellation region
 */
export const CancellationRegionSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  taskIds: z.array(z.string().min(1)),
  parentRegionId: z.string().uuid().optional(),
  childRegionIds: z.array(z.string().uuid()).default([]),
  createdAt: z.coerce.date(),
  active: z.boolean().default(true),
});

/**
 * @typedef {z.infer<typeof CancellationRegionSchema>} CancellationRegion
 */

// ============================================================================
// CANCELLATION REGION MANAGER
// ============================================================================

/**
 * Manages cancellation regions - groups of tasks that cancel together
 */
export class CancellationRegionManager {
  constructor() {
    /** @type {Map<string, CancellationRegion>} */
    this.regions = new Map();
    /** @type {Map<string, Set<string>>} Task ID -> Region IDs */
    this.taskToRegions = new Map();
  }

  /**
   * Create a new cancellation region
   * @param {Object} options
   * @param {string} options.name - Region name
   * @param {string[]} options.taskIds - Task IDs in region
   * @param {string} [options.parentRegionId] - Parent region for nesting
   * @returns {CancellationRegion}
   */
  createRegion(options) {
    // Validate required fields
    if (!options.name || typeof options.name !== 'string') {
      throw new Error('Region name is required and must be a string');
    }
    if (!Array.isArray(options.taskIds) || options.taskIds.length === 0) {
      throw new Error('taskIds must be a non-empty array');
    }

    const region = {
      id: randomUUID(),
      name: options.name,
      taskIds: options.taskIds,
      parentRegionId: options.parentRegionId,
      childRegionIds: [],
      createdAt: new Date(),
      active: true,
    };

    // Register region
    this.regions.set(region.id, region);

    // Map tasks to region
    for (const taskId of region.taskIds) {
      if (!this.taskToRegions.has(taskId)) {
        this.taskToRegions.set(taskId, new Set());
      }
      this.taskToRegions.get(taskId).add(region.id);
    }

    // Link to parent region
    if (options.parentRegionId) {
      const parent = this.regions.get(options.parentRegionId);
      if (parent) {
        parent.childRegionIds.push(region.id);
      }
    }

    return region;
  }

  /**
   * Get region by ID
   * @param {string} regionId
   * @returns {CancellationRegion|undefined}
   */
  getRegion(regionId) {
    return this.regions.get(regionId);
  }

  /**
   * Get all regions containing a task
   * @param {string} taskId
   * @returns {CancellationRegion[]}
   */
  getRegionsForTask(taskId) {
    const regionIds = this.taskToRegions.get(taskId);
    if (!regionIds) return [];

    return Array.from(regionIds)
      .map(id => this.regions.get(id))
      .filter(r => r !== undefined);
  }

  /**
   * Get all sibling tasks in regions containing a task
   * @param {string} taskId
   * @returns {string[]} Array of sibling task IDs (excluding input task)
   */
  getSiblingTasks(taskId) {
    const siblings = new Set();
    const regions = this.getRegionsForTask(taskId);

    for (const region of regions) {
      for (const tid of region.taskIds) {
        if (tid !== taskId) {
          siblings.add(tid);
        }
      }
    }

    return Array.from(siblings);
  }

  /**
   * Get all descendant regions (nested)
   * @param {string} regionId
   * @returns {CancellationRegion[]}
   */
  getDescendantRegions(regionId) {
    const descendants = [];
    const region = this.regions.get(regionId);
    if (!region) return descendants;

    const queue = [...region.childRegionIds];
    while (queue.length > 0) {
      const childId = queue.shift();
      const child = this.regions.get(childId);
      if (child) {
        descendants.push(child);
        queue.push(...child.childRegionIds);
      }
    }

    return descendants;
  }

  /**
   * Deactivate a region and all descendants
   * @param {string} regionId
   * @returns {string[]} IDs of all deactivated regions
   */
  deactivateRegion(regionId) {
    const deactivated = [];
    const region = this.regions.get(regionId);
    if (!region) return deactivated;

    region.active = false;
    deactivated.push(regionId);

    const descendants = this.getDescendantRegions(regionId);
    for (const desc of descendants) {
      desc.active = false;
      deactivated.push(desc.id);
    }

    return deactivated;
  }

  /**
   * Get all tasks in a region and its descendants
   * @param {string} regionId
   * @returns {string[]} All task IDs
   */
  getAllTasksInRegion(regionId) {
    const tasks = new Set();
    const region = this.regions.get(regionId);
    if (!region) return [];

    for (const taskId of region.taskIds) {
      tasks.add(taskId);
    }

    const descendants = this.getDescendantRegions(regionId);
    for (const desc of descendants) {
      for (const taskId of desc.taskIds) {
        tasks.add(taskId);
      }
    }

    return Array.from(tasks);
  }

  /**
   * Export regions for serialization
   * @returns {Object}
   */
  export() {
    return {
      regions: Array.from(this.regions.values()),
      taskMappings: Object.fromEntries(
        Array.from(this.taskToRegions.entries()).map(([k, v]) => [k, Array.from(v)])
      ),
    };
  }

  /**
   * Import regions from serialized data
   * @param {Object} data
   */
  import(data) {
    this.regions.clear();
    this.taskToRegions.clear();

    for (const region of data.regions || []) {
      // Reconstruct region with proper Date object
      const parsed = {
        id: region.id,
        name: region.name,
        taskIds: region.taskIds,
        parentRegionId: region.parentRegionId,
        childRegionIds: region.childRegionIds || [],
        createdAt: new Date(region.createdAt),
        active: region.active !== false,
      };
      this.regions.set(parsed.id, parsed);
    }

    for (const [taskId, regionIds] of Object.entries(data.taskMappings || {})) {
      this.taskToRegions.set(taskId, new Set(regionIds));
    }
  }

  /**
   * Clear all regions
   */
  clear() {
    this.regions.clear();
    this.taskToRegions.clear();
  }
}

/**
 * Create a cancellation region manager
 * @returns {CancellationRegionManager}
 */
export function createRegionManager() {
  return new CancellationRegionManager();
}
