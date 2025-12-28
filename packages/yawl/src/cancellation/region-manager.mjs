/**
 * @file Cancellation Region Manager
 * @module yawl/cancellation/region-manager
 *
 * @description
 * Manages cancellation regions - groups of tasks that cancel together.
 */

import { randomUUID } from 'crypto';

/**
 * Manages cancellation regions - groups of tasks that cancel together
 */
export class CancellationRegionManager {
  constructor() {
    /** @type {Map<string, import('./schemas.mjs').CancellationRegion>} */
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
   * @returns {import('./schemas.mjs').CancellationRegion}
   */
  createRegion(options) {
    this._validateRegionOptions(options);

    const region = {
      id: randomUUID(),
      name: options.name,
      taskIds: options.taskIds,
      parentRegionId: options.parentRegionId,
      childRegionIds: [],
      createdAt: new Date(),
      active: true,
    };

    this._registerRegion(region);
    this._linkToParent(region.id, options.parentRegionId);

    return region;
  }

  /**
   * Validate region creation options
   * @param {Object} options
   * @private
   */
  _validateRegionOptions(options) {
    if (!options.name || typeof options.name !== 'string') {
      throw new Error('Region name is required and must be a string');
    }
    if (!Array.isArray(options.taskIds) || options.taskIds.length === 0) {
      throw new Error('taskIds must be a non-empty array');
    }
  }

  /**
   * Register region and map tasks
   * @param {import('./schemas.mjs').CancellationRegion} region
   * @private
   */
  _registerRegion(region) {
    this.regions.set(region.id, region);

    for (const taskId of region.taskIds) {
      if (!this.taskToRegions.has(taskId)) {
        this.taskToRegions.set(taskId, new Set());
      }
      this.taskToRegions.get(taskId).add(region.id);
    }
  }

  /**
   * Link region to parent
   * @param {string} regionId
   * @param {string} [parentRegionId]
   * @private
   */
  _linkToParent(regionId, parentRegionId) {
    if (parentRegionId) {
      const parent = this.regions.get(parentRegionId);
      if (parent) {
        parent.childRegionIds.push(regionId);
      }
    }
  }

  /**
   * Get region by ID
   * @param {string} regionId
   * @returns {import('./schemas.mjs').CancellationRegion|undefined}
   */
  getRegion(regionId) {
    return this.regions.get(regionId);
  }

  /**
   * Get all regions containing a task
   * @param {string} taskId
   * @returns {import('./schemas.mjs').CancellationRegion[]}
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
   * @returns {import('./schemas.mjs').CancellationRegion[]}
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

    this._importRegions(data.regions || []);
    this._importTaskMappings(data.taskMappings || {});
  }

  /**
   * Import regions array
   * @param {Array} regions
   * @private
   */
  _importRegions(regions) {
    for (const region of regions) {
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
  }

  /**
   * Import task mappings
   * @param {Object} taskMappings
   * @private
   */
  _importTaskMappings(taskMappings) {
    for (const [taskId, regionIds] of Object.entries(taskMappings)) {
      this.taskToRegions.set(taskId, new Set(regionIds));
    }
  }
}
