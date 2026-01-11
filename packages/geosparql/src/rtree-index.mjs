/**
 * @file R-tree spatial index for efficient querying
 * @module @unrdf/geosparql/rtree-index
 * @description High-performance spatial indexing using rbush
 */

import RBush from 'rbush';
import { getBBox } from './geometry.mjs';
import { BBoxSchema } from './schemas.mjs';

/**
 * Creates a spatial index using R-tree
 * @param {number} [maxEntries=9] - Maximum entries per node
 * @returns {Object} Spatial index
 * @example
 * const index = createSpatialIndex();
 * index.insert(point, { id: 'sf' });
 * const results = index.search(bbox);
 */
export function createSpatialIndex(maxEntries = 9) {
  const tree = new RBush(maxEntries);
  const items = new Map();

  return {
    /**
     * Inserts geometry into index
     * @param {Object} geometry - GeoJSON geometry
     * @param {*} [data] - Associated data
     * @returns {string} Item ID
     */
    insert(geometry, data = null) {
      const bbox = getBBox(geometry);
      const [minX, minY, maxX, maxY] = bbox;

      const id = crypto.randomUUID();
      const item = {
        minX,
        minY,
        maxX,
        maxY,
        id,
        geometry,
        data
      };

      tree.insert(item);
      items.set(id, item);

      return id;
    },

    /**
     * Removes geometry from index
     * @param {string} id - Item ID
     * @returns {boolean} True if removed
     */
    remove(id) {
      const item = items.get(id);
      if (!item) return false;

      tree.remove(item);
      items.delete(id);
      return true;
    },

    /**
     * Searches index with bounding box
     * @param {[number, number, number, number]} bbox - Bounding box
     * @returns {Array<Object>} Matching items
     */
    search(bbox) {
      BBoxSchema.parse(bbox);
      const [minX, minY, maxX, maxY] = bbox;

      return tree.search({ minX, minY, maxX, maxY });
    },

    /**
     * Searches index with geometry
     * @param {Object} geometry - Query geometry
     * @returns {Array<Object>} Matching items
     */
    searchGeometry(geometry) {
      const bbox = getBBox(geometry);
      return this.search(bbox);
    },

    /**
     * Clears all items from index
     */
    clear() {
      tree.clear();
      items.clear();
    },

    /**
     * Gets all items in index
     * @returns {Array<Object>} All items
     */
    all() {
      return tree.all();
    },

    /**
     * Gets number of items in index
     * @returns {number} Item count
     */
    size() {
      return items.size;
    },

    /**
     * Checks if index contains item
     * @param {string} id - Item ID
     * @returns {boolean} True if exists
     */
    has(id) {
      return items.has(id);
    },

    /**
     * Gets item by ID
     * @param {string} id - Item ID
     * @returns {Object|undefined} Item or undefined
     */
    get(id) {
      return items.get(id);
    },

    /**
     * Bulk loads geometries (more efficient than individual inserts)
     * @param {Array<{geometry: Object, data?: *}>} entries - Geometries to load
     * @returns {Array<string>} Item IDs
     */
    bulkLoad(entries) {
      const treeItems = entries.map(({ geometry, data }) => {
        const bbox = getBBox(geometry);
        const [minX, minY, maxX, maxY] = bbox;
        const id = crypto.randomUUID();

        const item = {
          minX,
          minY,
          maxX,
          maxY,
          id,
          geometry,
          data
        };

        items.set(id, item);
        return item;
      });

      tree.load(treeItems);
      return treeItems.map(item => item.id);
    },

    /**
     * Exports index state
     * @returns {Object} Serializable index state
     */
    toJSON() {
      return {
        tree: tree.toJSON(),
        items: Array.from(items.entries())
      };
    },

    /**
     * Imports index state
     * @param {Object} state - Serialized state
     */
    fromJSON(state) {
      tree.fromJSON(state.tree);
      items.clear();
      for (const [id, item] of state.items) {
        items.set(id, item);
      }
    }
  };
}

/**
 * Creates nearest neighbor searcher
 * @param {Object} index - Spatial index
 * @param {Object} target - Target geometry
 * @param {number} [k=1] - Number of neighbors
 * @returns {Array<Object>} K nearest neighbors
 */
export function kNearest(index, target, k = 1) {
  const targetBBox = getBBox(target);
  const [targetX, targetY] = [
    (targetBBox[0] + targetBBox[2]) / 2,
    (targetBBox[1] + targetBBox[3]) / 2
  ];

  const all = index.all();

  // Calculate distances
  const withDistances = all.map(item => {
    const itemX = (item.minX + item.maxX) / 2;
    const itemY = (item.minY + item.maxY) / 2;
    const dist = Math.sqrt(
      Math.pow(itemX - targetX, 2) + Math.pow(itemY - targetY, 2)
    );

    return { ...item, distance: dist };
  });

  // Sort by distance and take k
  return withDistances
    .sort((a, b) => a.distance - b.distance)
    .slice(0, k);
}
