/**
 * @file registry.mjs
 * @description OntologyRegistry for allow-listing and managing ontologies
 */

import { AllowedOntology, OntologyRelease } from './ontology-release.mjs';
import { validateIri } from './rdf-utils.mjs';

/**
 * Registry for managing allowed ontologies
 *
 * Provides allow-listing, lookups by namespace IRI, and hash verification.
 *
 * @class OntologyRegistry
 *
 * @example
 * ```javascript
 * const registry = new OntologyRegistry();
 *
 * // Register allowed ontology
 * registry.register(provOntology);
 *
 * // Lookup by namespace
 * const prov = registry.getByNamespace('http://www.w3.org/ns/prov#');
 *
 * // Verify content hash
 * const isAllowed = registry.isHashAllowed(hash);
 * ```
 */
export class OntologyRegistry {
  /**
   * Create an ontology registry
   */
  constructor() {
    /**
     * Map of namespace IRI to AllowedOntology
     * @type {Map<string, AllowedOntology>}
     * @private
     */
    this._ontologies = new Map();

    /**
     * Map of content hash to OntologyRelease
     * @type {Map<string, OntologyRelease>}
     * @private
     */
    this._hashIndex = new Map();
  }

  /**
   * Register an allowed ontology
   *
   * @param {AllowedOntology} ontology - Ontology to register
   * @throws {Error} If ontology is invalid or namespace already registered
   *
   * @example
   * ```javascript
   * registry.register(new AllowedOntology({
   *   namespaceIri: 'http://www.w3.org/ns/prov#',
   *   name: 'PROV-O',
   *   releases: [...]
   * }));
   * ```
   */
  register(ontology) {
    if (!(ontology instanceof AllowedOntology)) {
      throw new Error('Must provide an AllowedOntology instance');
    }

    if (this._ontologies.has(ontology.namespaceIri)) {
      throw new Error(
        `Ontology namespace already registered: ${ontology.namespaceIri}`
      );
    }

    this._ontologies.set(ontology.namespaceIri, ontology);

    // Index all releases by content hash
    for (const release of ontology.releases) {
      if (this._hashIndex.has(release.contentHash)) {
        throw new Error(
          `Duplicate content hash detected: ${release.contentHash} (namespace: ${ontology.namespaceIri})`
        );
      }
      this._hashIndex.set(release.contentHash, release);
    }
  }

  /**
   * Get ontology by namespace IRI
   *
   * @param {string} namespaceIri - Namespace IRI to lookup
   * @returns {AllowedOntology|undefined} Ontology if found
   *
   * @example
   * ```javascript
   * const prov = registry.getByNamespace('http://www.w3.org/ns/prov#');
   * ```
   */
  getByNamespace(namespaceIri) {
    if (!validateIri(namespaceIri)) {
      throw new Error(`Invalid namespace IRI: ${namespaceIri}`);
    }

    return this._ontologies.get(namespaceIri);
  }

  /**
   * Get ontology release by content hash
   *
   * @param {string} hash - SHA256 content hash
   * @returns {OntologyRelease|undefined} Release if found
   *
   * @example
   * ```javascript
   * const release = registry.getByHash('abc123...');
   * ```
   */
  getByHash(hash) {
    return this._hashIndex.get(hash);
  }

  /**
   * Check if a namespace IRI is allowed
   *
   * @param {string} namespaceIri - Namespace IRI to check
   * @returns {boolean} True if allowed
   *
   * @example
   * ```javascript
   * if (registry.isNamespaceAllowed('http://www.w3.org/ns/prov#')) {
   *   console.log('PROV-O is allowed');
   * }
   * ```
   */
  isNamespaceAllowed(namespaceIri) {
    return this._ontologies.has(namespaceIri);
  }

  /**
   * Check if a content hash is allowed
   *
   * @param {string} hash - SHA256 content hash
   * @returns {boolean} True if hash matches any allowed release
   *
   * @example
   * ```javascript
   * if (registry.isHashAllowed(hash)) {
   *   console.log('Content is from an allowed ontology');
   * }
   * ```
   */
  isHashAllowed(hash) {
    return this._hashIndex.has(hash);
  }

  /**
   * Get all registered namespace IRIs
   *
   * @returns {Array<string>} Array of namespace IRIs
   *
   * @example
   * ```javascript
   * const namespaces = registry.getAllNamespaces();
   * console.log('Registered namespaces:', namespaces);
   * ```
   */
  getAllNamespaces() {
    return Array.from(this._ontologies.keys());
  }

  /**
   * Get all registered ontologies
   *
   * @returns {Array<AllowedOntology>} Array of ontologies
   *
   * @example
   * ```javascript
   * const ontologies = registry.getAllOntologies();
   * ```
   */
  getAllOntologies() {
    return Array.from(this._ontologies.values());
  }

  /**
   * Get total number of registered ontologies
   *
   * @returns {number} Number of ontologies
   */
  get size() {
    return this._ontologies.size;
  }

  /**
   * Clear all registered ontologies
   *
   * @example
   * ```javascript
   * registry.clear();
   * ```
   */
  clear() {
    this._ontologies.clear();
    this._hashIndex.clear();
  }

  /**
   * Convert registry to JSON representation
   *
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      ontologies: this.getAllOntologies().map((o) => o.toJSON()),
    };
  }

  /**
   * Create OntologyRegistry from JSON
   *
   * @param {Object} json - JSON representation
   * @returns {OntologyRegistry} Registry instance
   */
  static fromJSON(json) {
    const registry = new OntologyRegistry();

    for (const ontologyJson of json.ontologies) {
      const ontology = AllowedOntology.fromJSON(ontologyJson);
      registry.register(ontology);
    }

    return registry;
  }

  /**
   * Create a registry with standard W3C ontologies
   *
   * Includes: PROV, ODRL, SKOS, OWL-Time, DCAT, ORG, OA
   *
   * @param {Object} [hashes] - Optional pre-computed content hashes
   * @returns {OntologyRegistry} Registry with standard ontologies
   *
   * @example
   * ```javascript
   * const registry = OntologyRegistry.createStandardRegistry({
   *   'http://www.w3.org/ns/prov#': 'abc123...',
   *   'http://www.w3.org/ns/odrl/2/': 'def456...',
   * });
   * ```
   */
  static createStandardRegistry(hashes = {}) {
    const registry = new OntologyRegistry();

    // Define standard ontologies
    const standardOntologies = [
      {
        namespaceIri: 'http://www.w3.org/ns/prov#',
        name: 'PROV-O',
        description: 'W3C Provenance Ontology',
        hash:
          hashes['http://www.w3.org/ns/prov#'] ||
          '0000000000000000000000000000000000000000000000000000000000000001',
      },
      {
        namespaceIri: 'http://www.w3.org/ns/odrl/2/',
        name: 'ODRL',
        description: 'Open Digital Rights Language',
        hash:
          hashes['http://www.w3.org/ns/odrl/2/'] ||
          '0000000000000000000000000000000000000000000000000000000000000002',
      },
      {
        namespaceIri: 'http://www.w3.org/2004/02/skos/core#',
        name: 'SKOS',
        description: 'Simple Knowledge Organization System',
        hash:
          hashes['http://www.w3.org/2004/02/skos/core#'] ||
          '0000000000000000000000000000000000000000000000000000000000000003',
      },
      {
        namespaceIri: 'http://www.w3.org/2006/time#',
        name: 'OWL-Time',
        description: 'Time Ontology in OWL',
        hash:
          hashes['http://www.w3.org/2006/time#'] ||
          '0000000000000000000000000000000000000000000000000000000000000004',
      },
      {
        namespaceIri: 'http://www.w3.org/ns/dcat#',
        name: 'DCAT',
        description: 'Data Catalog Vocabulary',
        hash:
          hashes['http://www.w3.org/ns/dcat#'] ||
          '0000000000000000000000000000000000000000000000000000000000000005',
      },
      {
        namespaceIri: 'http://www.w3.org/ns/org#',
        name: 'ORG',
        description: 'Organization Ontology',
        hash:
          hashes['http://www.w3.org/ns/org#'] ||
          '0000000000000000000000000000000000000000000000000000000000000006',
      },
      {
        namespaceIri: 'http://www.w3.org/ns/oa#',
        name: 'OA',
        description: 'Web Annotation Ontology',
        hash:
          hashes['http://www.w3.org/ns/oa#'] ||
          '0000000000000000000000000000000000000000000000000000000000000007',
      },
    ];

    for (const ontDef of standardOntologies) {
      const ontology = new AllowedOntology({
        namespaceIri: ontDef.namespaceIri,
        name: ontDef.name,
        description: ontDef.description,
        releases: [
          new OntologyRelease({
            namespaceIri: ontDef.namespaceIri,
            version: '1.0',
            contentHash: ontDef.hash,
          }),
        ],
      });

      registry.register(ontology);
    }

    return registry;
  }
}
