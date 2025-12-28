/**
 * @file universe.mjs
 * @description Universe class for managing all RDF partitions
 */

import { createStore } from '@unrdf/oxigraph';
import {
  Partition,
  IndustrialSubstrate,
  CorporateCanon,
  BusinessUnitOverlay,
  RegionalOverlay,
  ExecutionLedger,
  SystemPolicyPartition,
} from './partition.mjs';
import { OntologyRegistry } from './registry.mjs';
import { mergeStores } from './rdf-utils.mjs';

/**
 * Universe class - manages all RDF partitions
 *
 * The Universe maintains a strict partition order and provides
 * unified query and merge capabilities across all partitions.
 *
 * Partition order (lower index = higher priority in merges):
 * 1. IndustrialSubstrate (foundational ontologies)
 * 2. CorporateCanon (enterprise models)
 * 3. BusinessUnitOverlay (business unit customizations)
 * 4. RegionalOverlay (regional data)
 * 5. ExecutionLedger (execution events)
 * 6. SystemPolicyPartition (policies and governance)
 *
 * @class Universe
 *
 * @example
 * ```javascript
 * const universe = new Universe();
 *
 * // Access partitions
 * const industrial = universe.getPartition('IndustrialSubstrate');
 *
 * // Merge all partitions
 * const merged = universe.merge();
 *
 * // Query across all partitions
 * const results = universe.query(sparql`'SELECT * WHERE { ?s ?p ?o }'`);
 * ```
 */
export class Universe {
  /**
   * Create a new Universe
   *
   * @param {Object} [config] - Universe configuration
   * @param {OntologyRegistry} [config.registry] - Ontology registry
   */
  constructor(config = {}) {
    /**
     * Ontology registry for allow-listing
     * @type {OntologyRegistry}
     */
    this.registry = config.registry || OntologyRegistry.createStandardRegistry();

    /**
     * Ordered array of partitions
     * @type {Array<Partition>}
     * @private
     */
    this._partitions = [
      new IndustrialSubstrate(),
      new CorporateCanon(),
      new BusinessUnitOverlay(),
      new RegionalOverlay(),
      new ExecutionLedger(),
      new SystemPolicyPartition(),
    ];

    /**
     * Map of partition name to partition instance
     * @type {Map<string, Partition>}
     * @private
     */
    this._partitionMap = new Map();

    // Build partition map
    for (const partition of this._partitions) {
      this._partitionMap.set(partition.name, partition);
    }
  }

  /**
   * Get a partition by name
   *
   * @param {string} name - Partition name
   * @returns {Partition|undefined} Partition if found
   *
   * @example
   * ```javascript
   * const industrial = universe.getPartition('IndustrialSubstrate');
   * ```
   */
  getPartition(name) {
    return this._partitionMap.get(name);
  }

  /**
   * Get all partitions in order
   *
   * @returns {Array<Partition>} Array of partitions
   *
   * @example
   * ```javascript
   * const partitions = universe.getAllPartitions();
   * console.log('Total partitions:', partitions.length);
   * ```
   */
  getAllPartitions() {
    return [...this._partitions];
  }

  /**
   * Get partition names in order
   *
   * @returns {Array<string>} Array of partition names
   */
  getPartitionNames() {
    return this._partitions.map((p) => p.name);
  }

  /**
   * Merge all partitions into a single store
   *
   * Partitions are merged in order, with later partitions
   * potentially overriding earlier ones (based on quad matching).
   *
   * @returns {import('@unrdf/oxigraph').OxigraphStore} Merged store
   *
   * @example
   * ```javascript
   * const merged = universe.merge();
   * console.log('Total quads after merge:', merged.size);
   * ```
   */
  merge() {
    const stores = this._partitions.map((p) => p.store);
    return mergeStores(stores);
  }

  /**
   * Query across all partitions (merged view)
   *
   * @param {string} query - SPARQL query
   * @returns {*} Query results
   *
   * @example
   * ```javascript
   * const results = universe.query(`
   *   PREFIX prov: <http://www.w3.org/ns/prov#>
   *   SELECT ?activity WHERE {
   *     ?activity a prov:Activity .
   *   }
   * `);
   * ```
   */
  query(query) {
    const merged = this.merge();
    return merged.query(query);
  }

  /**
   * Match quads across all partitions (merged view)
   *
   * @param {*} subject - Subject pattern
   * @param {*} predicate - Predicate pattern
   * @param {*} object - Object pattern
   * @param {*} graph - Graph pattern
   * @returns {Array<import('@unrdf/oxigraph').Quad>} Matching quads
   *
   * @example
   * ```javascript
   * const quads = universe.match(null, rdfType, null);
   * ```
   */
  match(subject, predicate, object, graph) {
    const merged = this.merge();
    return merged.match(subject, predicate, object, graph);
  }

  /**
   * Get total size (sum of all partition sizes)
   *
   * @returns {number} Total number of quads
   */
  get totalSize() {
    return this._partitions.reduce((sum, p) => sum + p.size, 0);
  }

  /**
   * Get partition sizes
   *
   * @returns {Object} Map of partition name to size
   *
   * @example
   * ```javascript
   * const sizes = universe.getPartitionSizes();
   * console.log('IndustrialSubstrate size:', sizes.IndustrialSubstrate);
   * ```
   */
  getPartitionSizes() {
    const sizes = {};
    for (const partition of this._partitions) {
      sizes[partition.name] = partition.size;
    }
    return sizes;
  }

  /**
   * Load foundational ontologies into IndustrialSubstrate
   *
   * This method is designed to be called once during initialization
   * to populate the IndustrialSubstrate with W3C ontologies.
   *
   * @param {Object} ontologyContent - Map of namespace IRI to Turtle content
   * @returns {Promise<void>}
   *
   * @example
   * ```javascript
   * await universe.loadIndustrialSubstrate({
   *   'http://www.w3.org/ns/prov#': provTurtleContent,
   *   'http://www.w3.org/ns/odrl/2/': odrlTurtleContent,
   * });
   * ```
   */
  async loadIndustrialSubstrate(ontologyContent) {
    const industrial = this.getPartition('IndustrialSubstrate');
    if (!industrial) {
      throw new Error('IndustrialSubstrate partition not found');
    }

    // Temporarily make it writable for loading
    const wasReadOnly = industrial.readOnly;
    industrial.readOnly = false;

    try {
      for (const [namespaceIri, content] of Object.entries(ontologyContent)) {
        // Verify namespace is allowed
        if (!this.registry.isNamespaceAllowed(namespaceIri)) {
          throw new Error(`Namespace not allowed: ${namespaceIri}`);
        }

        // Load content
        industrial.loadTurtle(content, namespaceIri);
      }
    } finally {
      // Restore read-only status
      industrial.readOnly = wasReadOnly;
    }
  }

  /**
   * Validate that IndustrialSubstrate contains exactly 7 allowed ontologies
   *
   * @returns {Promise<Object>} Validation result
   *
   * @example
   * ```javascript
   * const validation = await universe.validateIndustrialSubstrate();
   * if (validation.valid) {
   *   console.log('IndustrialSubstrate is valid');
   * }
   * ```
   */
  async validateIndustrialSubstrate() {
    const industrial = this.getPartition('IndustrialSubstrate');
    if (!industrial) {
      return {
        valid: false,
        error: 'IndustrialSubstrate partition not found',
      };
    }

    // Expected 7 ontologies
    const expectedNamespaces = [
      'http://www.w3.org/ns/prov#',
      'http://www.w3.org/ns/odrl/2/',
      'http://www.w3.org/2004/02/skos/core#',
      'http://www.w3.org/2006/time#',
      'http://www.w3.org/ns/dcat#',
      'http://www.w3.org/ns/org#',
      'http://www.w3.org/ns/oa#',
    ];

    // Check if partition is read-only
    if (!industrial.readOnly) {
      return {
        valid: false,
        error: 'IndustrialSubstrate must be read-only',
      };
    }

    // Check namespace IRIs
    const actualNamespaces = new Set(industrial.namespaceIris);
    if (actualNamespaces.size !== 7) {
      return {
        valid: false,
        error: `Expected 7 namespaces, found ${actualNamespaces.size}`,
        expected: expectedNamespaces,
        actual: Array.from(actualNamespaces),
      };
    }

    for (const ns of expectedNamespaces) {
      if (!actualNamespaces.has(ns)) {
        return {
          valid: false,
          error: `Missing expected namespace: ${ns}`,
          expected: expectedNamespaces,
          actual: Array.from(actualNamespaces),
        };
      }
    }

    return {
      valid: true,
      namespaceCount: actualNamespaces.size,
      namespaces: Array.from(actualNamespaces),
      size: industrial.size,
      contentHash: await industrial.getContentHash(),
    };
  }

  /**
   * Register protected namespaces in SystemPolicyPartition
   *
   * @param {Array<string>} namespaces - Namespace IRIs to protect
   * @returns {void}
   *
   * @example
   * ```javascript
   * universe.registerProtectedNamespaces([
   *   'http://www.w3.org/ns/prov#',
   *   'http://www.w3.org/ns/odrl/2/',
   * ]);
   * ```
   */
  registerProtectedNamespaces(namespaces) {
    const policy = this.getPartition('SystemPolicyPartition');
    if (!policy) {
      throw new Error('SystemPolicyPartition not found');
    }

    for (const ns of namespaces) {
      if (!policy.protectedNamespaces.includes(ns)) {
        policy.protectedNamespaces.push(ns);
      }
    }
  }

  /**
   * Convert universe to JSON representation
   *
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      registry: this.registry.toJSON(),
      partitions: this._partitions.map((p) => p.toJSON()),
      totalSize: this.totalSize,
    };
  }

  /**
   * Create a Universe with standard configuration
   *
   * @param {Object} [hashes] - Optional pre-computed ontology hashes
   * @returns {Universe} Universe instance
   *
   * @example
   * ```javascript
   * const universe = Universe.createStandard({
   *   'http://www.w3.org/ns/prov#': 'abc123...',
   * });
   * ```
   */
  static createStandard(hashes) {
    const registry = OntologyRegistry.createStandardRegistry(hashes);
    return new Universe({ registry });
  }
}
