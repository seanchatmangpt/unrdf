/**
 * @fileoverview Store & Persistence - RDF-backed storage for Universes and Scenes
 *
 * **Purpose**: Persistent storage layer using @unrdf/oxigraph
 * - UniverseStore: Create and retrieve Universe definitions
 * - SceneStore: Add scenes, verify receipts, replay history
 * - TTL serialization for persistence
 *
 * **Design**:
 * - Universes stored as RDF metadata + JSON-LD functions
 * - Scenes stored as RDF quads (observations) + JSON delta
 * - Transactional: all-or-nothing scene additions
 *
 * @module narrative-state-chain/store
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { randomUUID } from 'crypto';
import { UniverseSchema, SceneSchema, validateUniverse, validateScene } from './types.mjs';
import { generateReceipt } from './receipts.mjs';
import { evaluateAllGuards } from './guards.mjs';
import { reconcile as executeReconciliation } from './reconcile.mjs';

const { namedNode, literal, quad } = dataFactory;

/**
 * Universe Store - manages Universe definitions
 *
 * @class UniverseStore
 *
 * @example
 * const store = new UniverseStore();
 * const universe = await store.create({
 *   schema: 'http://example.org/schema#',
 *   reconcile: async (state, obs) => ({ consequences: [], artifacts: {}, errors: [] }),
 *   invariants: [],
 *   guards: [],
 *   metadata: { name: 'MyUniverse' }
 * });
 */
export class UniverseStore {
  /**
   * Create a new UniverseStore
   *
   * @param {Object} [options] - Store options
   * @param {import('@unrdf/oxigraph').OxigraphStore} [options.store] - Existing store
   */
  constructor(options = {}) {
    /**
     * RDF store for metadata
     * @type {import('@unrdf/oxigraph').OxigraphStore}
     * @private
     */
    this._store = options.store || createStore();

    /**
     * In-memory cache of universe definitions
     * @type {Map<string, import('./types.mjs').Universe>}
     * @private
     */
    this._universes = new Map();
  }

  /**
   * Create a new Universe
   *
   * @param {Object} config - Universe configuration
   * @param {string} config.schema - RDF schema IRI
   * @param {Function} config.reconcile - Reconciliation function μ
   * @param {import('./types.mjs').Invariant[]} [config.invariants] - State invariants
   * @param {import('./types.mjs').Guard[]} [config.guards] - Authorization guards
   * @param {Object} config.metadata - Universe metadata
   * @returns {Promise<import('./types.mjs').Universe>} Created universe
   *
   * @example
   * const universe = await store.create({
   *   schema: 'http://example.org/schema#',
   *   reconcile: async (state, obs) => ({ consequences: [], artifacts: {}, errors: [] }),
   *   metadata: { name: 'MyUniverse', description: 'Example universe' }
   * });
   */
  async create(config) {
    const id = randomUUID();

    const universe = {
      id,
      schema: config.schema,
      reconcile: config.reconcile,
      invariants: config.invariants || [],
      guards: config.guards || [],
      metadata: {
        name: config.metadata.name,
        description: config.metadata.description,
        version: config.metadata.version || '1.0.0',
        created: new Date(),
        updated: new Date(),
      },
    };

    // Validate
    const validation = validateUniverse(universe);
    if (!validation.success) {
      throw new Error(`Invalid universe: ${validation.error.message}`);
    }

    // Store metadata as RDF
    const universeNode = namedNode(`urn:universe:${id}`);
    this._store.add(
      quad(
        universeNode,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/narrative-state-chain#Universe')
      )
    );
    this._store.add(
      quad(
        universeNode,
        namedNode('http://example.org/narrative-state-chain#schema'),
        namedNode(config.schema)
      )
    );
    this._store.add(
      quad(
        universeNode,
        namedNode('http://purl.org/dc/terms/title'),
        literal(config.metadata.name)
      )
    );

    // Cache in memory
    this._universes.set(id, validation.data);

    return validation.data;
  }

  /**
   * Get Universe by ID
   *
   * @param {string} id - Universe UUID
   * @returns {import('./types.mjs').Universe|null} Universe or null if not found
   *
   * @example
   * const universe = store.get('550e8400-e29b-41d4-a716-446655440000');
   */
  get(id) {
    return this._universes.get(id) || null;
  }

  /**
   * List all Universe IDs
   *
   * @returns {string[]} Array of Universe UUIDs
   */
  list() {
    return Array.from(this._universes.keys());
  }

  /**
   * Delete Universe (CAUTION: destructive)
   *
   * @param {string} id - Universe UUID
   * @returns {boolean} True if deleted, false if not found
   */
  delete(id) {
    const existed = this._universes.has(id);
    this._universes.delete(id);

    // Remove from RDF store
    const universeNode = namedNode(`urn:universe:${id}`);
    const quadsToRemove = this._store.match(universeNode, null, null);
    for (const q of quadsToRemove) {
      this._store.delete(q);
    }

    return existed;
  }

  /**
   * Get RDF store
   *
   * @returns {import('@unrdf/oxigraph').OxigraphStore}
   */
  getStore() {
    return this._store;
  }
}

/**
 * Scene Store - manages Scene history and receipts
 *
 * @class SceneStore
 *
 * @example
 * const sceneStore = new SceneStore(universeStore);
 * const scene = await sceneStore.add(
 *   universeId,
 *   [quad1, quad2],
 *   { key: 'value' }
 * );
 */
export class SceneStore {
  /**
   * Create a new SceneStore
   *
   * @param {UniverseStore} universeStore - Universe store
   * @param {Object} [options] - Store options
   * @param {import('@unrdf/oxigraph').OxigraphStore} [options.store] - Existing store
   */
  constructor(universeStore, options = {}) {
    /**
     * Universe store
     * @type {UniverseStore}
     * @private
     */
    this._universeStore = universeStore;

    /**
     * RDF store for scene observations
     * @type {import('@unrdf/oxigraph').OxigraphStore}
     * @private
     */
    this._store = options.store || createStore();

    /**
     * Scene metadata cache
     * @type {Map<string, import('./types.mjs').Scene>}
     * @private
     */
    this._scenes = new Map();

    /**
     * Scene history per universe (ordered)
     * @type {Map<string, string[]>}
     * @private
     */
    this._history = new Map();
  }

  /**
   * Add a new Scene to the universe
   *
   * **Process**:
   * 1. Validate universe exists
   * 2. Evaluate guards (admissibility)
   * 3. Execute reconciliation
   * 4. Generate receipt
   * 5. Store scene (transactional)
   *
   * @param {string} universeId - Universe UUID
   * @param {any[]} observations - Input observations (RDF quads or objects)
   * @param {Object} delta - State change
   * @param {Object} [options] - Additional options
   * @param {string} [options.agent] - Agent performing action
   * @returns {Promise<import('./types.mjs').Scene>} Created scene with receipt
   *
   * @example
   * const scene = await sceneStore.add(
   *   universeId,
   *   [quad(s, p, o)],
   *   { property: 'newValue' },
   *   { agent: 'user@example.com' }
   * );
   */
  async add(universeId, observations, delta, options = {}) {
    // 1. Validate universe exists
    const universe = this._universeStore.get(universeId);
    if (!universe) {
      throw new Error(`Universe not found: ${universeId}`);
    }

    const sceneId = randomUUID();
    const previousSceneId = this._getLastSceneId(universeId);
    const previousScene = previousSceneId ? this._scenes.get(previousSceneId) : null;
    const agent = options.agent || 'system';

    // 2. Evaluate guards
    const guardResults = await evaluateAllGuards(universe, agent, { observations, delta });

    const admissible = guardResults.every(r => r.passed);
    if (!admissible) {
      const failures = guardResults.filter(r => !r.passed);
      throw new Error(`Scene not admissible: ${failures.map(f => f.reason).join(', ')}`);
    }

    // 3. Execute reconciliation
    const currentState = previousScene ? { ...previousScene.delta, ...previousScene.artifacts } : {};
    const reconciliationResult = await executeReconciliation(
      universe,
      currentState,
      observations
    );

    if (reconciliationResult.errors.length > 0) {
      throw new Error(`Reconciliation errors: ${reconciliationResult.errors.join(', ')}`);
    }

    // 4. Generate receipt
    const receipt = await generateReceipt({
      sceneId,
      universeId,
      admissibilityChecks: guardResults,
      delta,
      previousReceipt: previousScene?.receipts[previousScene.receipts.length - 1],
    });

    // 5. Create scene
    const scene = {
      id: sceneId,
      universeId,
      observations,
      delta,
      consequences: reconciliationResult.consequences,
      artifacts: reconciliationResult.artifacts,
      receipts: [receipt],
      timestamp: new Date(),
      previousSceneId,
    };

    // Validate scene
    const validation = validateScene(scene);
    if (!validation.success) {
      throw new Error(`Invalid scene: ${validation.error.message}`);
    }

    // Store observations as RDF quads
    const sceneNode = namedNode(`urn:scene:${sceneId}`);
    for (const obs of observations) {
      // If observation is a quad, store directly
      if (obs.subject && obs.predicate && obs.object) {
        this._store.add(quad(obs.subject, obs.predicate, obs.object, sceneNode));
      }
    }

    // Store scene metadata
    this._store.add(
      quad(
        sceneNode,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/narrative-state-chain#Scene')
      )
    );
    this._store.add(
      quad(
        sceneNode,
        namedNode('http://example.org/narrative-state-chain#universe'),
        namedNode(`urn:universe:${universeId}`)
      )
    );

    // Cache scene
    this._scenes.set(sceneId, validation.data);

    // Update history
    if (!this._history.has(universeId)) {
      this._history.set(universeId, []);
    }
    this._history.get(universeId).push(sceneId);

    return validation.data;
  }

  /**
   * Get Scene by ID
   *
   * @param {string} sceneId - Scene UUID
   * @returns {import('./types.mjs').Scene|null} Scene or null
   */
  get(sceneId) {
    return this._scenes.get(sceneId) || null;
  }

  /**
   * Get last Scene ID for a universe
   *
   * @param {string} universeId - Universe UUID
   * @returns {string|null} Last scene ID or null
   * @private
   */
  _getLastSceneId(universeId) {
    const history = this._history.get(universeId);
    return history && history.length > 0 ? history[history.length - 1] : null;
  }

  /**
   * Verify a receipt's integrity
   *
   * @param {import('./types.mjs').Receipt} receipt - Receipt to verify
   * @returns {Promise<{admissible: boolean, violations: string[]}>} Verification result
   *
   * @example
   * const result = await sceneStore.verify(scene.receipts[0]);
   * if (!result.admissible) {
   *   console.error('Violations:', result.violations);
   * }
   */
  async verify(receipt) {
    const violations = [];

    // Check all admissibility checks passed
    const failedGuards = receipt.admissibilityChecks.filter(c => !c.passed);
    if (failedGuards.length > 0) {
      violations.push(`Failed guards: ${failedGuards.map(g => g.guardId).join(', ')}`);
    }

    // Check receipt hash integrity (would use crypto verification here)
    if (!receipt.receiptHash) {
      violations.push('Missing receipt hash');
    }

    return {
      admissible: violations.length === 0,
      violations,
    };
  }

  /**
   * Replay scene history from one commit to another
   *
   * @param {string} universeId - Universe UUID
   * @param {string} [fromCommit] - Starting scene ID (inclusive)
   * @param {string} [toCommit] - Ending scene ID (inclusive)
   * @returns {Promise<Object>} Final state after replay
   *
   * @example
   * const finalState = await sceneStore.replay(universeId, scene1.id, scene5.id);
   */
  async replay(universeId, fromCommit, toCommit) {
    const history = this._history.get(universeId);
    if (!history || history.length === 0) {
      return {};
    }

    const fromIndex = fromCommit ? history.indexOf(fromCommit) : 0;
    const toIndex = toCommit ? history.indexOf(toCommit) : history.length - 1;

    if (fromIndex === -1 || toIndex === -1) {
      throw new Error('Invalid commit range');
    }

    let state = {};

    for (let i = fromIndex; i <= toIndex; i++) {
      const sceneId = history[i];
      const scene = this._scenes.get(sceneId);

      if (!scene) {
        throw new Error(`Scene not found in cache: ${sceneId}`);
      }

      // Apply delta
      state = { ...state, ...scene.delta };

      // Apply artifacts
      state = { ...state, ...scene.artifacts };
    }

    return state;
  }

  /**
   * Get scene history for a universe
   *
   * @param {string} universeId - Universe UUID
   * @returns {string[]} Array of scene IDs in chronological order
   */
  getHistory(universeId) {
    return this._history.get(universeId) || [];
  }

  /**
   * Get RDF store
   *
   * @returns {import('@unrdf/oxigraph').OxigraphStore}
   */
  getStore() {
    return this._store;
  }
}
