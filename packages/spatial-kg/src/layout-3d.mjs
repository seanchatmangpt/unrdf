/**
 * @file 3D Force-Directed Graph Layout
 * @module @unrdf/spatial-kg/layout-3d
 * @description Implements 3D Fruchterman-Reingold force-directed layout algorithm
 */

import { Node3DSchema, Edge3DSchema, SpatialGraphConfigSchema } from './schemas.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/spatial-kg/layout-3d');

/**
 * Calculate distance between two 3D points
 * @param {Object} a - First point with x, y, z
 * @param {Object} b - Second point with x, y, z
 * @returns {number} Euclidean distance
 */
function distance3D(a, b) {
  const dx = a.x - b.x;
  const dy = a.y - b.y;
  const dz = a.z - b.z;
  return Math.sqrt(dx * dx + dy * dy + dz * dz);
}

/**
 * 3D Force-Directed Layout Engine
 */
export class Layout3D {
  /**
   * @param {Object} config - Layout configuration
   */
  constructor(config = {}) {
    const validated = SpatialGraphConfigSchema.parse({ layout: config });
    this.config = validated.layout;
    this.nodes = new Map();
    this.edges = [];
    this.iteration = 0;
    this.temperature = 1.0;
  }

  /**
   * Add node to layout
   * @param {Object} node - Node data
   * @returns {void}
   */
  addNode(node) {
    const validated = Node3DSchema.parse(node);

    // Initialize position if not set
    if (!validated.position) {
      validated.position = {
        x: (Math.random() - 0.5) * 100,
        y: (Math.random() - 0.5) * 100,
        z: (Math.random() - 0.5) * 100,
      };
    }

    // Initialize velocity
    validated.velocity = validated.velocity || { x: 0, y: 0, z: 0 };

    this.nodes.set(validated.id, validated);
  }

  /**
   * Add edge to layout
   * @param {Object} edge - Edge data
   * @returns {void}
   */
  addEdge(edge) {
    const validated = Edge3DSchema.parse(edge);
    this.edges.push(validated);
  }

  /**
   * Calculate repulsion force between nodes
   * @param {Object} nodeA - First node
   * @param {Object} nodeB - Second node
   * @returns {Object} Force vector
   * @private
   */
  _calculateRepulsion(nodeA, nodeB) {
    const dist = distance3D(nodeA.position, nodeB.position);
    if (dist < 0.01) return { x: 0, y: 0, z: 0 };

    const force = (this.config.repulsionStrength * this.temperature) / (dist * dist);
    const dx = nodeA.position.x - nodeB.position.x;
    const dy = nodeA.position.y - nodeB.position.y;
    const dz = nodeA.position.z - nodeB.position.z;

    return {
      x: (dx / dist) * force,
      y: (dy / dist) * force,
      z: (dz / dist) * force,
    };
  }

  /**
   * Calculate attraction force along edge
   * @param {Object} edge - Edge connecting nodes
   * @returns {Object} Force vectors for source and target
   * @private
   */
  _calculateAttraction(edge) {
    const source = this.nodes.get(edge.source);
    const target = this.nodes.get(edge.target);

    if (!source || !target) {
      return { source: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 0 } };
    }

    const dist = distance3D(source.position, target.position);
    if (dist < 0.01) return { source: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 0 } };

    const force = this.config.attractionStrength * dist * edge.strength;
    const dx = target.position.x - source.position.x;
    const dy = target.position.y - source.position.y;
    const dz = target.position.z - source.position.z;

    const forceVec = {
      x: (dx / dist) * force,
      y: (dy / dist) * force,
      z: (dz / dist) * force,
    };

    return {
      source: forceVec,
      target: { x: -forceVec.x, y: -forceVec.y, z: -forceVec.z },
    };
  }

  /**
   * Calculate gravity force toward center
   * @param {Object} node - Node to apply gravity to
   * @returns {Object} Force vector
   * @private
   */
  _calculateGravity(node) {
    if (!this.config.centerForce) return { x: 0, y: 0, z: 0 };

    const dist = Math.sqrt(
      node.position.x ** 2 + node.position.y ** 2 + node.position.z ** 2
    );

    if (dist < 0.01) return { x: 0, y: 0, z: 0 };

    const force = this.config.gravityStrength * dist;

    return {
      x: -(node.position.x / dist) * force,
      y: -(node.position.y / dist) * force,
      z: -(node.position.z / dist) * force,
    };
  }

  /**
   * Execute one iteration of layout algorithm
   * @returns {number} Total displacement (convergence metric)
   */
  step() {
    return tracer.startActiveSpan('layout3d.step', (span) => {
      try {
        const forces = new Map();

        // Initialize forces
        for (const [id] of this.nodes) {
          forces.set(id, { x: 0, y: 0, z: 0 });
        }

        // Calculate repulsion forces (all pairs)
        const nodeArray = Array.from(this.nodes.values());
        for (let i = 0; i < nodeArray.length; i++) {
          for (let j = i + 1; j < nodeArray.length; j++) {
            const repulsion = this._calculateRepulsion(nodeArray[i], nodeArray[j]);

            const forceA = forces.get(nodeArray[i].id);
            forceA.x += repulsion.x;
            forceA.y += repulsion.y;
            forceA.z += repulsion.z;

            const forceB = forces.get(nodeArray[j].id);
            forceB.x -= repulsion.x;
            forceB.y -= repulsion.y;
            forceB.z -= repulsion.z;
          }
        }

        // Calculate attraction forces (edges)
        for (const edge of this.edges) {
          const attraction = this._calculateAttraction(edge);

          const sourceForce = forces.get(edge.source);
          if (sourceForce) {
            sourceForce.x += attraction.source.x;
            sourceForce.y += attraction.source.y;
            sourceForce.z += attraction.source.z;
          }

          const targetForce = forces.get(edge.target);
          if (targetForce) {
            targetForce.x += attraction.target.x;
            targetForce.y += attraction.target.y;
            targetForce.z += attraction.target.z;
          }
        }

        // Calculate gravity forces
        for (const node of this.nodes.values()) {
          const gravity = this._calculateGravity(node);
          const force = forces.get(node.id);
          force.x += gravity.x;
          force.y += gravity.y;
          force.z += gravity.z;
        }

        // Apply forces and update positions
        let totalDisplacement = 0;

        for (const [id, node] of this.nodes) {
          if (node.fixed) continue;

          const force = forces.get(id);

          // Update velocity with damping
          node.velocity.x = node.velocity.x * 0.8 + force.x;
          node.velocity.y = node.velocity.y * 0.8 + force.y;
          node.velocity.z = node.velocity.z * 0.8 + force.z;

          // Limit velocity
          const speed = Math.sqrt(
            node.velocity.x ** 2 + node.velocity.y ** 2 + node.velocity.z ** 2
          );
          const maxSpeed = 10 * this.temperature;
          if (speed > maxSpeed) {
            node.velocity.x = (node.velocity.x / speed) * maxSpeed;
            node.velocity.y = (node.velocity.y / speed) * maxSpeed;
            node.velocity.z = (node.velocity.z / speed) * maxSpeed;
          }

          // Update position
          node.position.x += node.velocity.x * this.temperature;
          node.position.y += node.velocity.y * this.temperature;
          node.position.z += node.velocity.z * this.temperature;

          totalDisplacement += Math.abs(node.velocity.x) +
                              Math.abs(node.velocity.y) +
                              Math.abs(node.velocity.z);
        }

        // Cool down
        this.temperature *= this.config.cooldown;
        this.iteration++;

        span.setAttributes({
          'layout.iteration': this.iteration,
          'layout.displacement': totalDisplacement,
          'layout.temperature': this.temperature,
          'layout.nodes': this.nodes.size,
        });

        return totalDisplacement;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Run layout until convergence or max iterations
   * @param {number} [maxIterations] - Maximum iterations (default from config)
   * @returns {Promise<number>} Final displacement
   */
  async run(maxIterations) {
    return tracer.startActiveSpan('layout3d.run', async (span) => {
      try {
        const max = maxIterations || this.config.iterations;
        let displacement = Infinity;

        for (let i = 0; i < max && displacement > 0.1; i++) {
          displacement = this.step();
        }

        span.setAttributes({
          'layout.iterations': this.iteration,
          'layout.final_displacement': displacement,
          'layout.converged': displacement <= 0.1,
        });

        return displacement;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get all node positions
   * @returns {Map<string, Object>} Map of node ID to position
   */
  getPositions() {
    const positions = new Map();
    for (const [id, node] of this.nodes) {
      positions.set(id, { ...node.position });
    }
    return positions;
  }

  /**
   * Reset layout to initial state
   * @returns {void}
   */
  reset() {
    this.iteration = 0;
    this.temperature = 1.0;

    for (const node of this.nodes.values()) {
      if (!node.fixed) {
        node.position.x = (Math.random() - 0.5) * 100;
        node.position.y = (Math.random() - 0.5) * 100;
        node.position.z = (Math.random() - 0.5) * 100;
        node.velocity.x = 0;
        node.velocity.y = 0;
        node.velocity.z = 0;
      }
    }
  }
}
