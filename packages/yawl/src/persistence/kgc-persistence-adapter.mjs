/**
 * @file KGC-4D Persistence Adapter - Storage layer for YAWL entities
 * @module @unrdf/yawl/persistence/kgc-persistence-adapter
 *
 * @description
 * Provides CRUD operations for YAWL workflow entities using KGC-4D as the persistence layer.
 * Novel improvement over Java YAWL's Hibernate: temporal queries, time-travel, and cryptographic receipts.
 *
 * Features:
 * - RDF-based storage (vs SQL tables)
 * - Time-travel queries (reconstruct state at any time)
 * - Cryptographic receipts for all mutations
 * - SPARQL queries (vs SQL)
 * - Automatic event sourcing
 */

import { z } from 'zod';
import { KGCStore, freezeUniverse, reconstructState, TemporalSPARQL, now, toISO } from '@unrdf/kgc-4d';
import { randomUUID } from 'crypto';
import { EntitySerializer } from './entity-serializer.mjs';

// =============================================================================
// Schemas
// =============================================================================

const PersistenceOptionsSchema = z.object({
  storeDir: z.string().optional(),
  enableTimeTravel: z.boolean().default(true),
  enableReceipts: z.boolean().default(true),
  namespace: z.string().default('http://yawl.sourceforge.net/'),
});

const StoreCaseOptionsSchema = z.object({
  generateReceipt: z.boolean().default(true),
});

const QueryOptionsSchema = z.object({
  asOfTime: z.string().optional(), // ISO timestamp for time-travel
  includeHistory: z.boolean().default(false),
});

// =============================================================================
// KGC Persistence Adapter
// =============================================================================

/**
 * Persistence adapter for YAWL entities using KGC-4D storage.
 *
 * @class
 * @example
 * import { KGCPersistenceAdapter } from '@unrdf/yawl/persistence';
 *
 * const adapter = new KGCPersistenceAdapter({
 *   storeDir: './data/yawl',
 *   enableTimeTravel: true,
 * });
 *
 * await adapter.initialize();
 *
 * // Store a case
 * const receipt = await adapter.storeCase(caseInstance);
 * console.log('Stored with receipt:', receipt.id);
 *
 * // Load case
 * const loaded = await adapter.loadCase('case-123');
 *
 * // Time-travel query
 * const historical = await adapter.loadCase('case-123', {
 *   asOfTime: '2024-01-15T10:30:00Z'
 * });
 */
export class KGCPersistenceAdapter {
  /**
   * Create persistence adapter
   * @param {Object} options - Configuration options
   * @param {string} [options.storeDir] - Storage directory path
   * @param {boolean} [options.enableTimeTravel=true] - Enable time-travel queries
   * @param {boolean} [options.enableReceipts=true] - Generate cryptographic receipts
   * @param {string} [options.namespace] - RDF namespace for YAWL entities
   */
  constructor(options = {}) {
    const validated = PersistenceOptionsSchema.parse(options);

    this.options = validated;
    this.store = null;
    this.temporalQuery = null;
    this.serializer = new EntitySerializer({ namespace: validated.namespace });
    this.initialized = false;

    // Statistics
    this.stats = {
      casesStored: 0,
      casesLoaded: 0,
      casesUpdated: 0,
      casesDeleted: 0,
      queries: 0,
    };
  }

  /**
   * Initialize the persistence adapter
   * @returns {Promise<void>}
   */
  async initialize() {
    if (this.initialized) {
      return;
    }

    this.store = new KGCStore({
      dir: this.options.storeDir,
    });

    if (this.options.enableTimeTravel) {
      this.temporalQuery = new TemporalSPARQL(this.store);
    }

    this.initialized = true;
  }

  /**
   * Store a case to KGC-4D
   * @param {Object} caseInstance - Case instance to store
   * @param {Object} [options] - Storage options
   * @returns {Promise<Object>} Receipt with metadata
   * @throws {Error} If not initialized or serialization fails
   */
  async storeCase(caseInstance, options = {}) {
    this._ensureInitialized();
    const opts = StoreCaseOptionsSchema.parse(options);

    // Serialize case to RDF triples
    const triples = this.serializer.serializeCase(caseInstance);

    // Insert triples into KGC-4D
    const timestamp = now();
    const eventId = randomUUID();

    for (const triple of triples) {
      await this.store.insert(
        triple.subject,
        triple.predicate,
        triple.object,
        {
          timestamp,
          eventId,
          metadata: {
            operation: 'case.stored',
            caseId: caseInstance.id,
            workflowId: caseInstance.workflowId,
          },
        }
      );
    }

    this.stats.casesStored++;

    // Generate receipt if requested
    let receipt = null;
    if (opts.generateReceipt && this.options.enableReceipts) {
      receipt = await freezeUniverse(this.store, {
        operation: 'storeCase',
        entityType: 'Case',
        entityId: caseInstance.id,
        timestamp: toISO(timestamp),
        triples: triples.length,
      });
    }

    return {
      caseId: caseInstance.id,
      eventId,
      timestamp: toISO(timestamp),
      triplesStored: triples.length,
      receipt,
    };
  }

  /**
   * Update an existing case in KGC-4D
   * @param {Object} caseInstance - Updated case instance
   * @param {Object} [options] - Update options
   * @returns {Promise<Object>} Receipt with metadata
   */
  async updateCase(caseInstance, options = {}) {
    this._ensureInitialized();
    const opts = StoreCaseOptionsSchema.parse(options);

    // Delete old triples for this case
    await this._deleteCaseTriples(caseInstance.id);

    // Insert updated triples
    const triples = this.serializer.serializeCase(caseInstance);
    const timestamp = now();
    const eventId = randomUUID();

    for (const triple of triples) {
      await this.store.insert(
        triple.subject,
        triple.predicate,
        triple.object,
        {
          timestamp,
          eventId,
          metadata: {
            operation: 'case.updated',
            caseId: caseInstance.id,
          },
        }
      );
    }

    this.stats.casesUpdated++;

    let receipt = null;
    if (opts.generateReceipt && this.options.enableReceipts) {
      receipt = await freezeUniverse(this.store, {
        operation: 'updateCase',
        entityType: 'Case',
        entityId: caseInstance.id,
        timestamp: toISO(timestamp),
        triples: triples.length,
      });
    }

    return {
      caseId: caseInstance.id,
      eventId,
      timestamp: toISO(timestamp),
      triplesUpdated: triples.length,
      receipt,
    };
  }

  /**
   * Delete a case from KGC-4D (soft delete - maintains audit trail)
   * @param {string} caseId - Case ID to delete
   * @returns {Promise<Object>} Deletion receipt
   */
  async deleteCase(caseId) {
    this._ensureInitialized();

    const timestamp = now();
    const eventId = randomUUID();

    // Soft delete: mark as deleted instead of removing triples
    const deleteTriple = {
      subject: `${this.options.namespace}case/${caseId}`,
      predicate: `${this.options.namespace}deleted`,
      object: 'true',
    };

    await this.store.insert(
      deleteTriple.subject,
      deleteTriple.predicate,
      deleteTriple.object,
      {
        timestamp,
        eventId,
        metadata: {
          operation: 'case.deleted',
          caseId,
        },
      }
    );

    this.stats.casesDeleted++;

    return {
      caseId,
      eventId,
      timestamp: toISO(timestamp),
      deleted: true,
    };
  }

  /**
   * Load a case from KGC-4D
   * @param {string} caseId - Case ID to load
   * @param {Object} [options] - Query options
   * @param {string} [options.asOfTime] - ISO timestamp for time-travel
   * @param {boolean} [options.includeHistory] - Include event history
   * @returns {Promise<Object|null>} Reconstructed case instance or null if not found
   */
  async loadCase(caseId, options = {}) {
    this._ensureInitialized();
    const opts = QueryOptionsSchema.parse(options);

    this.stats.casesLoaded++;

    // SPARQL query to fetch case triples
    const query = `
      PREFIX yawl: <${this.options.namespace}>

      SELECT ?predicate ?object
      WHERE {
        yawl:case/${caseId} ?predicate ?object .
        FILTER(?predicate != yawl:deleted)
      }
    `;

    let results;
    if (opts.asOfTime && this.temporalQuery) {
      // Time-travel query
      const state = await reconstructState(this.store, opts.asOfTime);
      results = await this.temporalQuery.query(query, {
        asOf: opts.asOfTime,
      });
    } else {
      // Current state query
      results = await this.store.query(query);
    }

    if (!results || results.length === 0) {
      return null;
    }

    // Deserialize RDF triples back to case object
    const caseData = this.serializer.deserializeCase(caseId, results);

    if (opts.includeHistory) {
      caseData.history = await this._getCaseHistory(caseId);
    }

    return caseData;
  }

  /**
   * Query cases using SPARQL
   * @param {string} sparqlQuery - SPARQL query string
   * @param {Object} [options] - Query options
   * @returns {Promise<Array>} Query results
   */
  async query(sparqlQuery, options = {}) {
    this._ensureInitialized();
    const opts = QueryOptionsSchema.parse(options);

    this.stats.queries++;

    if (opts.asOfTime && this.temporalQuery) {
      return await this.temporalQuery.query(sparqlQuery, {
        asOf: opts.asOfTime,
      });
    }

    return await this.store.query(sparqlQuery);
  }

  /**
   * Get all active cases
   * @param {Object} [options] - Query options
   * @returns {Promise<Array<string>>} Array of case IDs
   */
  async getActiveCases(options = {}) {
    const query = `
      PREFIX yawl: <${this.options.namespace}>

      SELECT DISTINCT ?caseId
      WHERE {
        ?case a yawl:Case ;
              yawl:id ?caseId ;
              yawl:status "active" .
        FILTER NOT EXISTS { ?case yawl:deleted "true" }
      }
    `;

    const results = await this.query(query, options);
    return results.map(r => r.caseId);
  }

  /**
   * Get statistics
   * @returns {Object} Adapter statistics
   */
  getStats() {
    return { ...this.stats };
  }

  /**
   * Close the adapter and cleanup resources
   * @returns {Promise<void>}
   */
  async close() {
    if (this.store) {
      await this.store.close();
      this.store = null;
    }
    this.initialized = false;
  }

  // ===========================================================================
  // Private Methods
  // ===========================================================================

  /**
   * Ensure adapter is initialized
   * @private
   */
  _ensureInitialized() {
    if (!this.initialized) {
      throw new Error('KGCPersistenceAdapter not initialized. Call initialize() first.');
    }
  }

  /**
   * Delete all triples for a case
   * @private
   * @param {string} caseId - Case ID
   */
  async _deleteCaseTriples(caseId) {
    const query = `
      PREFIX yawl: <${this.options.namespace}>

      DELETE {
        yawl:case/${caseId} ?p ?o .
      }
      WHERE {
        yawl:case/${caseId} ?p ?o .
      }
    `;

    await this.store.update(query);
  }

  /**
   * Get event history for a case
   * @private
   * @param {string} caseId - Case ID
   * @returns {Promise<Array>} Event history
   */
  async _getCaseHistory(caseId) {
    const query = `
      PREFIX yawl: <${this.options.namespace}>

      SELECT ?timestamp ?eventType ?data
      WHERE {
        ?event yawl:caseId "${caseId}" ;
               yawl:timestamp ?timestamp ;
               yawl:eventType ?eventType ;
               yawl:data ?data .
      }
      ORDER BY ?timestamp
    `;

    return await this.query(query);
  }
}

// =============================================================================
// Exports
// =============================================================================

export default KGCPersistenceAdapter;
