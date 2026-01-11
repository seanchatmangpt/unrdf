/**
 * @file Spatial Knowledge Graph Engine
 * @module @unrdf/spatial-kg
 * @description Main orchestration engine for spatial knowledge graphs
 */

import { Layout3D } from './layout-3d.mjs';
import { WebXRRenderer } from './webxr-renderer.mjs';
import { SpatialQueryEngine } from './spatial-query.mjs';
import { GestureController } from './gesture-controller.mjs';
import { LODManager } from './lod-manager.mjs';
import { CollaborationManager } from './collaboration.mjs';
import { SpatialGraphConfigSchema } from './schemas.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/spatial-kg');

/**
 * Spatial Knowledge Graph Engine
 */
export class SpatialKGEngine {
  /**
   * @param {Object} config - Engine configuration
   */
  constructor(config = {}) {
    this.config = SpatialGraphConfigSchema.parse(config);

    // Core components
    this.layout = new Layout3D(this.config.layout);
    this.renderer = new WebXRRenderer(this.config.rendering);
    this.queryEngine = null; // Created after layout
    this.gestureController = new GestureController();
    this.lodManager = new LODManager(this.config.lod);
    this.collaboration = null;

    // State
    this.running = false;
    this.animationFrame = null;
    this.initialized = false;
  }

  /**
   * Initialize engine
   * @returns {Promise<void>}
   */
  async initialize() {
    return tracer.startActiveSpan('engine.initialize', async (span) => {
      try {
        await this.renderer.initialize();

        // Setup collaboration if enabled
        if (this.config.collaboration.enabled) {
          this.collaboration = new CollaborationManager({
            userId: this.config.collaboration.username || 'user-' + Date.now(),
            serverUrl: this.config.collaboration.serverUrl,
          });
          await this.collaboration.connect();
        }

        this.initialized = true;

        span.setAttributes({
          'engine.initialized': true,
          'engine.collaboration': this.config.collaboration.enabled,
        });
      } finally {
        span.end();
      }
    });
  }

  /**
   * Load graph data
   * @param {Object} data - Graph data
   * @param {Array<Object>} data.nodes - Node list
   * @param {Array<Object>} data.edges - Edge list
   * @returns {Promise<void>}
   */
  async loadGraph(data) {
    return tracer.startActiveSpan('engine.load-graph', async (span) => {
      try {
        // Add nodes to layout
        for (const node of data.nodes) {
          this.layout.addNode(node);
        }

        // Add edges to layout
        for (const edge of data.edges) {
          this.layout.addEdge(edge);
        }

        // Run initial layout
        await this.layout.run();

        // Initialize query engine with node positions
        this.queryEngine = new SpatialQueryEngine(this.layout.nodes);

        // Update renderer
        this.renderer.updateNodes(this.layout.nodes);
        this.renderer.updateEdges(this.layout.edges, this.layout.nodes);

        span.setAttributes({
          'graph.nodes': data.nodes.length,
          'graph.edges': data.edges.length,
        });
      } finally {
        span.end();
      }
    });
  }

  /**
   * Start rendering loop
   * @returns {void}
   */
  start() {
    if (!this.initialized) {
      throw new Error('Engine not initialized. Call initialize() first.');
    }

    this.running = true;
    this._renderLoop();
  }

  /**
   * Stop rendering loop
   * @returns {void}
   */
  stop() {
    this.running = false;
    if (this.animationFrame && typeof globalThis.cancelAnimationFrame !== 'undefined') {
      globalThis.cancelAnimationFrame(this.animationFrame);
      this.animationFrame = null;
    }
  }

  /**
   * Main render loop
   * @private
   */
  _renderLoop() {
    if (!this.running) return;

    tracer.startActiveSpan('engine.render-loop', (span) => {
      try {
        // Update LOD based on camera
        const cameraPos = this.renderer.getCameraPosition();
        this.lodManager.updateCamera(cameraPos);
        this.lodManager.updateLevels(this.layout.nodes);

        // Update renderer
        this.renderer.updateNodes(this.layout.nodes);

        // Render frame
        this.renderer.render();

        // Update collaboration state
        if (this.collaboration?.isConnected()) {
          this.collaboration.updateLocalState({
            position: cameraPos,
            rotation: { x: 0, y: 0, z: 0, w: 1 }, // Placeholder
          });
        }

        const metrics = this.renderer.getMetrics();
        span.setAttributes({
          'engine.fps': metrics.fps,
          'engine.frameTime': metrics.frameTime,
        });
      } finally {
        span.end();
      }
    });

    if (typeof globalThis.requestAnimationFrame !== 'undefined') {
      this.animationFrame = globalThis.requestAnimationFrame(() => this._renderLoop());
    }
  }

  /**
   * Query nodes spatially
   * @param {Object} options - Query options
   * @returns {Array<Object>} Query results
   */
  query(options) {
    if (!this.queryEngine) {
      throw new Error('No graph loaded. Call loadGraph() first.');
    }

    const { type } = options;

    switch (type) {
      case 'proximity':
        return this.queryEngine.proximity(options);
      case 'ray':
        return this.queryEngine.rayCast(options);
      case 'knn':
        return this.queryEngine.kNearestNeighbors(options);
      case 'box':
        return this.queryEngine.box(options);
      default:
        throw new Error(`Unknown query type: ${type}`);
    }
  }

  /**
   * Start WebXR session
   * @param {string} mode - 'immersive-vr' or 'immersive-ar'
   * @returns {Promise<void>}
   */
  async startXR(mode) {
    await this.renderer.startXRSession(mode);
  }

  /**
   * End WebXR session
   * @returns {Promise<void>}
   */
  async endXR() {
    await this.renderer.endXRSession();
  }

  /**
   * Register gesture listener
   * @param {string} gestureType - Gesture type
   * @param {Function} callback - Event handler
   * @returns {Function} Unsubscribe function
   */
  onGesture(gestureType, callback) {
    return this.gestureController.on(gestureType, callback);
  }

  /**
   * Get performance metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      rendering: this.renderer.getMetrics(),
      lod: this.lodManager.getStats(),
      layout: {
        iteration: this.layout.iteration,
        temperature: this.layout.temperature,
        nodes: this.layout.nodes.size,
        edges: this.layout.edges.length,
      },
      collaboration: this.collaboration?.getStats() || null,
    };
  }

  /**
   * Dispose engine and cleanup resources
   * @returns {void}
   */
  dispose() {
    this.stop();
    this.renderer.dispose();
    this.gestureController.clear();
    this.lodManager.reset();
    if (this.collaboration) {
      this.collaboration.disconnect();
    }
  }
}
