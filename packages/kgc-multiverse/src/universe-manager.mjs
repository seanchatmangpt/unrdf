/**
 * KGC Multiverse - Universe Manager
 * Implements Q* identifiers, state machine, and CRUD operations for universes
 *
 * @module @unrdf/kgc-multiverse/universe-manager
 */

import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';
import { guardStateTransition, guardUniverseState } from './guards.mjs';

/**
 * Universe State Enum
 * @readonly
 * @enum {string}
 */
export const UniverseState = {
  GENESIS: 'GENESIS',     // Initial empty universe (0 events, 0 quads)
  ACTIVE: 'ACTIVE',       // Mutable universe accepting mutations
  FORKED: 'FORKED',       // Branched universe with parent reference
  MERGED: 'MERGED',       // Merged universe post-conflict resolution (transient)
  FROZEN: 'FROZEN',       // Immutable snapshot (read-only)
  DISCARDED: 'DISCARDED', // Terminal state (cleanup eligible)
};

/**
 * Q* Identifier Schema (Zod validation)
 * Enforces Q*_ID, Q*_RDF, Q*_PROV structure
 */
const QStarIDSchema = z.object({
  Q_ID: z.string().regex(/^Q\*_[a-f0-9]{16}$/), // Q* identifier (16 hex chars)
  Q_RDF: z.string().url(),                      // RDF URI representation
  Q_PROV: z.object({                            // Provenance metadata
    createdAt: z.bigint(),                      // Nanosecond timestamp
    createdBy: z.string().optional(),           // Creator identifier
    parentID: z.string().optional(),            // Parent universe Q*_ID (if forked)
    forkPoint: z.bigint().optional(),           // Fork timestamp (if forked)
  }),
});

/**
 * Universe Schema (Zod validation)
 */
const UniverseSchema = z.object({
  id: QStarIDSchema,                            // Q* identifier object
  state: z.nativeEnum(UniverseState),           // Current state
  eventCount: z.number().int().nonnegative(),   // Number of events
  universeHash: z.string().optional(),          // BLAKE3 hash (when frozen)
  parent: z.string().optional(),                // Parent universe Q*_ID
  metadata: z.record(z.any()).optional(),       // Arbitrary metadata
});

/**
 * Generate Q* Identifier
 * Creates a unique Q* identifier with RDF URI and provenance
 *
 * @param {Object} [options={}] - Configuration options
 * @param {string} [options.createdBy] - Creator identifier
 * @param {string} [options.parentID] - Parent universe Q*_ID (if forked)
 * @param {bigint} [options.forkPoint] - Fork timestamp (if forked)
 * @returns {Promise<Object>} Q* identifier object with Q_ID, Q_RDF, Q_PROV
 * @throws {TypeError} If options validation fails
 *
 * @example
 * import { generateQStarID } from './universe-manager.mjs';
 * const qid = await generateQStarID({ createdBy: 'alice@example.com' });
 * console.assert(qid.Q_ID.startsWith('Q*_'), 'Q* ID has correct prefix');
 * console.assert(qid.Q_RDF.startsWith('http://'), 'Q* RDF is valid URI');
 */
export async function generateQStarID(options = {}) {
  // Validate options
  if (typeof options !== 'object') {
    throw new TypeError('generateQStarID: options must be object');
  }

  // Generate timestamp
  const createdAt = typeof process !== 'undefined' && process.hrtime
    ? process.hrtime.bigint()
    : BigInt(Date.now()) * 1_000_000n;

  // Generate unique 16-hex-char ID using timestamp + random
  const randomBytes = crypto.getRandomValues(new Uint8Array(8));
  const randomHex = Array.from(randomBytes, b => b.toString(16).padStart(2, '0')).join('');
  const timestampHex = createdAt.toString(16).padStart(16, '0').slice(-16);

  // Combine and hash to get deterministic 16-char ID
  const combined = timestampHex + randomHex;
  const hash = await blake3(combined);
  const qidSuffix = hash.slice(0, 16);

  const Q_ID = `Q*_${qidSuffix}`;
  const Q_RDF = `http://kgc.io/multiverse/${qidSuffix}`;
  const Q_PROV = {
    createdAt,
    ...(options.createdBy && { createdBy: options.createdBy }),
    ...(options.parentID && { parentID: options.parentID }),
    ...(options.forkPoint && { forkPoint: options.forkPoint }),
  };

  // Validate before returning
  const result = { Q_ID, Q_RDF, Q_PROV };
  QStarIDSchema.parse(result); // Throws if invalid

  return result;
}

/**
 * Universe Manager Class
 * Manages universe lifecycle: creation, forking, merging, freezing
 */
export class UniverseManager {
  /**
   * Create a new UniverseManager
   *
   * @param {Object} [options={}] - Configuration options
   * @param {Object} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    /** @private */
    this._universes = new Map(); // Q*_ID → Universe
    /** @private */
    this._logger = options.logger || console;
  }

  /**
   * Create a new universe (GENESIS state)
   *
   * @param {Object} [options={}] - Creation options
   * @param {string} [options.createdBy] - Creator identifier
   * @param {Object} [options.metadata] - Arbitrary metadata
   * @returns {Promise<Object>} Created universe object
   * @throws {Error} If creation fails
   *
   * @example
   * import { UniverseManager } from './universe-manager.mjs';
   * const manager = new UniverseManager();
   * const universe = await manager.createUniverse({ createdBy: 'alice' });
   * console.assert(universe.state === 'GENESIS', 'New universe is GENESIS');
   */
  async createUniverse(options = {}) {
    const qid = await generateQStarID({ createdBy: options.createdBy });

    const universe = {
      id: qid,
      state: UniverseState.GENESIS,
      eventCount: 0,
      metadata: options.metadata || {},
    };

    // Validate
    UniverseSchema.parse(universe);

    // Store
    this._universes.set(qid.Q_ID, universe);

    this._logger.debug?.(`Created universe ${qid.Q_ID} in GENESIS state`);

    return Object.freeze({ ...universe }); // Return immutable copy
  }

  /**
   * Get universe by Q*_ID
   *
   * @param {string} qid - Q* identifier (Q*_xxxxxxxxxxxxxxxx)
   * @returns {Object|null} Universe object or null if not found
   * @throws {TypeError} If qid is invalid
   *
   * @example
   * const universe = manager.getUniverse('Q*_0123456789abcdef');
   * if (universe) {
   *   console.log('State:', universe.state);
   * }
   */
  getUniverse(qid) {
    if (typeof qid !== 'string' || !qid.startsWith('Q*_')) {
      throw new TypeError('getUniverse: qid must be Q* identifier string');
    }

    const universe = this._universes.get(qid);
    return universe ? Object.freeze({ ...universe }) : null; // Immutable copy
  }

  /**
   * Transition universe to new state
   * Enforces state machine rules via guards
   *
   * @param {string} qid - Q* identifier
   * @param {string} newState - Target state (from UniverseState enum)
   * @returns {Object} Updated universe
   * @throws {Error} If transition is invalid (via guards)
   *
   * @example
   * // Transition from GENESIS to ACTIVE (after first event)
   * const updated = manager.transitionState('Q*_0123...', 'ACTIVE');
   * console.assert(updated.state === 'ACTIVE', 'State transitioned');
   */
  transitionState(qid, newState) {
    const universe = this._universes.get(qid);
    if (!universe) {
      throw new Error(`transitionState: Universe ${qid} not found`);
    }

    // Guard: Validate state transition (throws if invalid)
    guardStateTransition(universe.state, newState);

    // Update state
    universe.state = newState;

    this._logger.debug?.(`Universe ${qid} transitioned to ${newState}`);

    return Object.freeze({ ...universe });
  }

  /**
   * Fork a universe (create child from parent)
   * Creates a new universe in FORKED state with parent reference
   *
   * @param {string} parentQID - Parent universe Q*_ID
   * @param {Object} [options={}] - Fork options
   * @param {string} [options.createdBy] - Creator identifier
   * @param {Object} [options.metadata] - Fork metadata
   * @returns {Promise<Object>} Forked universe
   * @throws {Error} If parent not found or not forkable
   *
   * @example
   * const parent = await manager.createUniverse();
   * const fork = await manager.forkUniverse(parent.id.Q_ID);
   * console.assert(fork.state === 'FORKED', 'Fork is FORKED state');
   * console.assert(fork.parent === parent.id.Q_ID, 'Fork has parent');
   */
  async forkUniverse(parentQID, options = {}) {
    const parent = this._universes.get(parentQID);
    if (!parent) {
      throw new Error(`forkUniverse: Parent universe ${parentQID} not found`);
    }

    // Guard: Parent must be ACTIVE or GENESIS (auto-transition GENESIS → ACTIVE)
    guardUniverseState(parent.state, ['ACTIVE', 'GENESIS']);

    if (parent.state === UniverseState.GENESIS) {
      // Auto-transition parent to ACTIVE before forking
      this.transitionState(parentQID, UniverseState.ACTIVE);
    }

    const forkPoint = typeof process !== 'undefined' && process.hrtime
      ? process.hrtime.bigint()
      : BigInt(Date.now()) * 1_000_000n;

    const qid = await generateQStarID({
      createdBy: options.createdBy,
      parentID: parentQID,
      forkPoint,
    });

    const fork = {
      id: qid,
      state: UniverseState.FORKED,
      eventCount: 0,
      parent: parentQID,
      metadata: options.metadata || {},
    };

    // Validate
    UniverseSchema.parse(fork);

    // Store
    this._universes.set(qid.Q_ID, fork);

    this._logger.debug?.(`Forked universe ${qid.Q_ID} from ${parentQID}`);

    return Object.freeze({ ...fork });
  }

  /**
   * Freeze a universe (create immutable snapshot)
   * Transitions ACTIVE → FROZEN
   *
   * @param {string} qid - Universe Q*_ID
   * @param {string} universeHash - BLAKE3 hash of universe state
   * @returns {Object} Frozen universe
   * @throws {Error} If universe not found or not freezable
   *
   * @example
   * const frozen = manager.freezeUniverse('Q*_0123...', 'abc123...');
   * console.assert(frozen.state === 'FROZEN', 'Universe frozen');
   * console.assert(frozen.universeHash, 'Has universe hash');
   */
  freezeUniverse(qid, universeHash) {
    const universe = this._universes.get(qid);
    if (!universe) {
      throw new Error(`freezeUniverse: Universe ${qid} not found`);
    }

    // Guard: Must be ACTIVE to freeze
    guardStateTransition(universe.state, UniverseState.FROZEN);

    // Validate hash
    if (typeof universeHash !== 'string' || !/^[a-f0-9]{64}$/.test(universeHash)) {
      throw new TypeError('freezeUniverse: universeHash must be 64-char hex string (BLAKE3)');
    }

    // Update state and hash
    universe.state = UniverseState.FROZEN;
    universe.universeHash = universeHash;

    // Note: We don't freeze the internal object to allow state transitions to DISCARDED
    // The returned copy is frozen for immutability

    this._logger.debug?.(`Froze universe ${qid} with hash ${universeHash}`);

    return Object.freeze({ ...universe });
  }

  /**
   * Delete universe (transition to DISCARDED)
   *
   * @param {string} qid - Universe Q*_ID
   * @param {Object} [options={}] - Delete options
   * @param {boolean} [options.force=false] - Force delete ACTIVE universe
   * @returns {boolean} True if deleted
   * @throws {Error} If universe not found or delete not allowed
   *
   * @example
   * manager.deleteUniverse('Q*_0123...');
   * const deleted = manager.getUniverse('Q*_0123...');
   * console.assert(deleted.state === 'DISCARDED', 'Universe discarded');
   */
  deleteUniverse(qid, options = {}) {
    const universe = this._universes.get(qid);
    if (!universe) {
      throw new Error(`deleteUniverse: Universe ${qid} not found`);
    }

    // Guard: Cannot delete DISCARDED (idempotency)
    if (universe.state === UniverseState.DISCARDED) {
      throw new Error('deleteUniverse: Universe already DISCARDED');
    }

    // Guard: Cannot delete ACTIVE without force
    if (universe.state === UniverseState.ACTIVE && !options.force) {
      throw new Error('deleteUniverse: Cannot delete ACTIVE universe without force=true');
    }

    // Transition to DISCARDED
    universe.state = UniverseState.DISCARDED;

    this._logger.debug?.(`Deleted universe ${qid}`);

    return true;
  }

  /**
   * List all universes
   *
   * @param {Object} [filter={}] - Filter criteria
   * @param {string} [filter.state] - Filter by state
   * @param {string} [filter.parent] - Filter by parent Q*_ID
   * @returns {Array<Object>} Array of universe objects
   *
   * @example
   * const active = manager.listUniverses({ state: 'ACTIVE' });
   * const forked = manager.listUniverses({ parent: 'Q*_0123...' });
   */
  listUniverses(filter = {}) {
    const universes = Array.from(this._universes.values());

    let filtered = universes;

    if (filter.state) {
      filtered = filtered.filter(u => u.state === filter.state);
    }

    if (filter.parent) {
      filtered = filtered.filter(u => u.parent === filter.parent);
    }

    // Return immutable copies
    return filtered.map(u => Object.freeze({ ...u }));
  }

  /**
   * Get universe count
   *
   * @returns {number} Total number of universes
   *
   * @example
   * const count = manager.count();
   * console.log('Total universes:', count);
   */
  count() {
    return this._universes.size;
  }
}
