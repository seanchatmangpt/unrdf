/**
 * @file Spatial Query Engine
 * @module @unrdf/spatial-kg/spatial-query
 * @description Spatial queries for 3D knowledge graphs (proximity, ray casting, k-NN)
 */

import { SpatialQueryOptionsSchema } from './schemas.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/spatial-kg/spatial-query');

/**
 * Octree node for spatial indexing
 * @private
 */
class OctreeNode {
  constructor(bounds, capacity = 8) {
    this.bounds = bounds;
    this.capacity = capacity;
    this.nodes = [];
    this.divided = false;
    this.children = null;
  }

  /**
   * Check if point is in bounds
   */
  contains(point) {
    return point.x >= this.bounds.min.x && point.x <= this.bounds.max.x &&
           point.y >= this.bounds.min.y && point.y <= this.bounds.max.y &&
           point.z >= this.bounds.min.z && point.z <= this.bounds.max.z;
  }

  /**
   * Insert node into octree
   */
  insert(node) {
    if (!this.contains(node.position)) return false;

    if (this.nodes.length < this.capacity) {
      this.nodes.push(node);
      return true;
    }

    if (!this.divided) {
      this._subdivide();
    }

    for (const child of this.children) {
      if (child.insert(node)) return true;
    }

    return false;
  }

  /**
   * Subdivide into 8 octants
   * @private
   */
  _subdivide() {
    const { min, max } = this.bounds;
    const midX = (min.x + max.x) / 2;
    const midY = (min.y + max.y) / 2;
    const midZ = (min.z + max.z) / 2;

    this.children = [
      // Front quadrants
      new OctreeNode({ min: { x: min.x, y: min.y, z: min.z }, max: { x: midX, y: midY, z: midZ } }, this.capacity),
      new OctreeNode({ min: { x: midX, y: min.y, z: min.z }, max: { x: max.x, y: midY, z: midZ } }, this.capacity),
      new OctreeNode({ min: { x: min.x, y: midY, z: min.z }, max: { x: midX, y: max.y, z: midZ } }, this.capacity),
      new OctreeNode({ min: { x: midX, y: midY, z: min.z }, max: { x: max.x, y: max.y, z: midZ } }, this.capacity),
      // Back quadrants
      new OctreeNode({ min: { x: min.x, y: min.y, z: midZ }, max: { x: midX, y: midY, z: max.z } }, this.capacity),
      new OctreeNode({ min: { x: midX, y: min.y, z: midZ }, max: { x: max.x, y: midY, z: max.z } }, this.capacity),
      new OctreeNode({ min: { x: min.x, y: midY, z: midZ }, max: { x: midX, y: max.y, z: max.z } }, this.capacity),
      new OctreeNode({ min: { x: midX, y: midY, z: midZ }, max: { x: max.x, y: max.y, z: max.z } }, this.capacity),
    ];

    this.divided = true;

    // Redistribute existing nodes
    for (const existingNode of this.nodes) {
      for (const child of this.children) {
        if (child.insert(existingNode)) break;
      }
    }
    this.nodes = [];
  }

  /**
   * Query nodes in range
   */
  query(bounds, found = []) {
    if (!this._intersects(bounds)) return found;

    for (const node of this.nodes) {
      if (this._pointInBounds(node.position, bounds)) {
        found.push(node);
      }
    }

    if (this.divided) {
      for (const child of this.children) {
        child.query(bounds, found);
      }
    }

    return found;
  }

  /**
   * Check if bounds intersect
   * @private
   */
  _intersects(bounds) {
    return !(bounds.max.x < this.bounds.min.x || bounds.min.x > this.bounds.max.x ||
             bounds.max.y < this.bounds.min.y || bounds.min.y > this.bounds.max.y ||
             bounds.max.z < this.bounds.min.z || bounds.min.z > this.bounds.max.z);
  }

  /**
   * Check if point in bounds
   * @private
   */
  _pointInBounds(point, bounds) {
    return point.x >= bounds.min.x && point.x <= bounds.max.x &&
           point.y >= bounds.min.y && point.y <= bounds.max.y &&
           point.z >= bounds.min.z && point.z <= bounds.max.z;
  }
}

/**
 * Spatial Query Engine
 */
export class SpatialQueryEngine {
  /**
   * @param {Map<string, Object>} nodes - Graph nodes with positions
   */
  constructor(nodes = new Map()) {
    this.nodes = nodes;
    this.octree = null;
    this._buildOctree();
  }

  /**
   * Build octree spatial index
   * @private
   */
  _buildOctree() {
    if (this.nodes.size === 0) {
      this.octree = null;
      return;
    }

    // Calculate bounds
    let minX = Infinity, minY = Infinity, minZ = Infinity;
    let maxX = -Infinity, maxY = -Infinity, maxZ = -Infinity;

    for (const node of this.nodes.values()) {
      minX = Math.min(minX, node.position.x);
      minY = Math.min(minY, node.position.y);
      minZ = Math.min(minZ, node.position.z);
      maxX = Math.max(maxX, node.position.x);
      maxY = Math.max(maxY, node.position.y);
      maxZ = Math.max(maxZ, node.position.z);
    }

    // Add padding
    const padding = 10;
    const bounds = {
      min: { x: minX - padding, y: minY - padding, z: minZ - padding },
      max: { x: maxX + padding, y: maxY + padding, z: maxZ + padding },
    };

    this.octree = new OctreeNode(bounds);

    // Insert all nodes
    for (const node of this.nodes.values()) {
      this.octree.insert(node);
    }
  }

  /**
   * Proximity query - find nodes within radius
   * @param {Object} options - Query options
   * @param {Object} options.origin - Center point
   * @param {number} options.radius - Search radius
   * @returns {Array<Object>} Nodes within radius
   */
  proximity(options) {
    return tracer.startActiveSpan('spatial-query.proximity', (span) => {
      try {
        const { origin, radius } = SpatialQueryOptionsSchema.parse({
          type: 'proximity',
          ...options,
        });

        if (!this.octree) return [];

        // Query octree with bounding box
        const bounds = {
          min: {
            x: origin.x - radius,
            y: origin.y - radius,
            z: origin.z - radius,
          },
          max: {
            x: origin.x + radius,
            y: origin.y + radius,
            z: origin.z + radius,
          },
        };

        const candidates = this.octree.query(bounds);

        // Filter by actual distance
        const results = candidates.filter((node) => {
          const dx = node.position.x - origin.x;
          const dy = node.position.y - origin.y;
          const dz = node.position.z - origin.z;
          const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
          return dist <= radius;
        });

        span.setAttributes({
          'query.type': 'proximity',
          'query.candidates': candidates.length,
          'query.results': results.length,
          'query.radius': radius,
        });

        return results;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Ray casting query - find nodes along ray
   * @param {Object} options - Query options
   * @param {Object} options.origin - Ray origin
   * @param {Object} options.direction - Ray direction (normalized)
   * @param {number} [options.radius=1] - Ray thickness
   * @returns {Array<Object>} Nodes intersecting ray
   */
  rayCast(options) {
    return tracer.startActiveSpan('spatial-query.raycast', (span) => {
      try {
        const { origin, direction, radius = 1 } = SpatialQueryOptionsSchema.parse({
          type: 'ray',
          ...options,
        });

        const results = [];

        for (const node of this.nodes.values()) {
          // Vector from origin to node
          const toNode = {
            x: node.position.x - origin.x,
            y: node.position.y - origin.y,
            z: node.position.z - origin.z,
          };

          // Project onto ray direction
          const projection = toNode.x * direction.x +
                           toNode.y * direction.y +
                           toNode.z * direction.z;

          if (projection < 0) continue; // Behind ray

          // Closest point on ray
          const closest = {
            x: origin.x + direction.x * projection,
            y: origin.y + direction.y * projection,
            z: origin.z + direction.z * projection,
          };

          // Distance from node to ray
          const dx = node.position.x - closest.x;
          const dy = node.position.y - closest.y;
          const dz = node.position.z - closest.z;
          const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);

          if (dist <= radius) {
            results.push({ node, distance: projection });
          }
        }

        // Sort by distance along ray
        results.sort((a, b) => a.distance - b.distance);

        span.setAttributes({
          'query.type': 'raycast',
          'query.results': results.length,
        });

        return results.map(r => r.node);
      } finally {
        span.end();
      }
    });
  }

  /**
   * K-nearest neighbors query
   * @param {Object} options - Query options
   * @param {Object} options.origin - Query point
   * @param {number} options.k - Number of neighbors
   * @returns {Array<Object>} K nearest nodes
   */
  kNearestNeighbors(options) {
    return tracer.startActiveSpan('spatial-query.knn', (span) => {
      try {
        const { origin, k } = SpatialQueryOptionsSchema.parse({
          type: 'knn',
          ...options,
        });

        const distances = [];

        for (const node of this.nodes.values()) {
          const dx = node.position.x - origin.x;
          const dy = node.position.y - origin.y;
          const dz = node.position.z - origin.z;
          const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
          distances.push({ node, distance: dist });
        }

        distances.sort((a, b) => a.distance - b.distance);
        const results = distances.slice(0, k).map(d => d.node);

        span.setAttributes({
          'query.type': 'knn',
          'query.k': k,
          'query.results': results.length,
        });

        return results;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Box query - find nodes in bounding box
   * @param {Object} options - Query options
   * @param {Object} options.bounds - Bounding box {min, max}
   * @returns {Array<Object>} Nodes in box
   */
  box(options) {
    return tracer.startActiveSpan('spatial-query.box', (span) => {
      try {
        const { bounds } = SpatialQueryOptionsSchema.parse({
          type: 'box',
          ...options,
        });

        if (!this.octree) return [];

        const results = this.octree.query(bounds);

        span.setAttributes({
          'query.type': 'box',
          'query.results': results.length,
        });

        return results;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Rebuild spatial index
   * @returns {void}
   */
  rebuild() {
    this._buildOctree();
  }
}
