/**
 * @file Level-of-Detail Manager
 * @module @unrdf/spatial-kg/lod-manager
 * @description Optimize rendering with distance-based LOD
 */

import { LODLevelSchema, SpatialGraphConfigSchema } from './schemas.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/spatial-kg/lod-manager');

/**
 * LOD Manager
 */
export class LODManager {
  /**
   * @param {Object} config - LOD configuration
   */
  constructor(config = {}) {
    const validated = SpatialGraphConfigSchema.parse({ lod: config });
    this.config = validated.lod;
    this.nodeLevels = new Map();
    this.cameraPosition = { x: 0, y: 0, z: 0 };
  }

  /**
   * Update camera position
   * @param {Object} position - Camera position {x, y, z}
   * @returns {void}
   */
  updateCamera(position) {
    this.cameraPosition = { ...position };
  }

  /**
   * Calculate LOD level for node
   * @param {Object} node - Node with position
   * @returns {number} LOD level (0=high, 1=medium, 2=low)
   */
  calculateLevel(node) {
    if (!this.config.enabled) return 0;

    const dx = node.position.x - this.cameraPosition.x;
    const dy = node.position.y - this.cameraPosition.y;
    const dz = node.position.z - this.cameraPosition.z;
    const distance = Math.sqrt(dx * dx + dy * dy + dz * dz);

    // Find appropriate LOD level
    for (let i = 0; i < this.config.levels.length; i++) {
      if (distance < this.config.levels[i].distance) {
        return i;
      }
    }

    return this.config.levels.length - 1;
  }

  /**
   * Update all node LOD levels
   * @param {Map<string, Object>} nodes - All graph nodes
   * @returns {Array<Object>} Nodes that changed LOD level
   */
  updateLevels(nodes) {
    return tracer.startActiveSpan('lod.update-levels', (span) => {
      try {
        const changes = [];

        for (const [id, node] of nodes) {
          const newLevel = this.calculateLevel(node);
          const oldLevel = this.nodeLevels.get(id);

          if (oldLevel !== newLevel) {
            const dx = node.position.x - this.cameraPosition.x;
            const dy = node.position.y - this.cameraPosition.y;
            const dz = node.position.z - this.cameraPosition.z;
            const distance = Math.sqrt(dx * dx + dy * dy + dz * dz);

            const change = LODLevelSchema.parse({
              nodeId: id,
              currentLevel: newLevel,
              distance,
              shouldUpdate: true,
            });

            changes.push(change);
            this.nodeLevels.set(id, newLevel);
          }
        }

        span.setAttributes({
          'lod.changes': changes.length,
          'lod.total_nodes': nodes.size,
        });

        return changes;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get LOD level for node
   * @param {string} nodeId - Node ID
   * @returns {number} LOD level
   */
  getLevel(nodeId) {
    return this.nodeLevels.get(nodeId) || 0;
  }

  /**
   * Get complexity for LOD level
   * @param {number} level - LOD level
   * @returns {string} Complexity ('high', 'medium', 'low')
   */
  getComplexity(level) {
    if (level >= this.config.levels.length) {
      return this.config.levels[this.config.levels.length - 1].complexity;
    }
    return this.config.levels[level].complexity;
  }

  /**
   * Get all nodes at LOD level
   * @param {number} level - LOD level
   * @returns {Array<string>} Node IDs
   */
  getNodesAtLevel(level) {
    const nodes = [];
    for (const [id, nodeLevel] of this.nodeLevels) {
      if (nodeLevel === level) {
        nodes.push(id);
      }
    }
    return nodes;
  }

  /**
   * Get LOD statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const stats = {
      enabled: this.config.enabled,
      levels: {},
      total: this.nodeLevels.size,
    };

    for (let i = 0; i < this.config.levels.length; i++) {
      const count = this.getNodesAtLevel(i).length;
      stats.levels[i] = {
        count,
        complexity: this.getComplexity(i),
        distance: this.config.levels[i].distance,
      };
    }

    return stats;
  }

  /**
   * Reset LOD state
   * @returns {void}
   */
  reset() {
    this.nodeLevels.clear();
    this.cameraPosition = { x: 0, y: 0, z: 0 };
  }

  /**
   * Determine if node should be rendered
   * @param {string} nodeId - Node ID
   * @param {Object} _frustum - Camera frustum (optional)
   * @returns {boolean} Should render
   */
  shouldRender(nodeId, _frustum = null) {
    const level = this.getLevel(nodeId);

    // Always render high detail
    if (level === 0) return true;

    // Medium detail - render 50%
    if (level === 1) {
      const hash = this._hashString(nodeId);
      return (hash % 2) === 0;
    }

    // Low detail - render 25%
    if (level === 2) {
      const hash = this._hashString(nodeId);
      return (hash % 4) === 0;
    }

    return false;
  }

  /**
   * Simple string hash for deterministic culling
   * @private
   */
  _hashString(str) {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      hash = ((hash << 5) - hash) + str.charCodeAt(i);
      hash = hash & hash;
    }
    return Math.abs(hash);
  }
}
