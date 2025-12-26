/**
 * @file partition.mjs
 * @description Partition classes for the RDF Universe
 */

import { createStore } from '@unrdf/oxigraph';
import { z } from 'zod';
import { IriSchema, extractNamespaces, computeContentHash } from './rdf-utils.mjs';

/**
 * Zod schema for partition configuration
 */
const PartitionConfigSchema = z.object({
  name: z.string().min(1),
  description: z.string().optional(),
  readOnly: z.boolean().default(false),
  namespaceIris: z.array(IriSchema).default([]),
  protectedNamespaces: z.array(IriSchema).default([]),
});

/**
 * Base Partition class
 *
 * A partition is a logical division of the RDF universe with specific properties:
 * - Read-only status
 * - Namespace IRIs
 * - Protected namespaces (cannot be modified)
 *
 * @class Partition
 * @abstract
 */
export class Partition {
  /**
   * Create a partition
   *
   * @param {Object} config - Partition configuration
   * @param {string} config.name - Partition name
   * @param {string} [config.description] - Partition description
   * @param {boolean} [config.readOnly=false] - Whether partition is read-only
   * @param {Array<string>} [config.namespaceIris=[]] - Allowed namespace IRIs
   * @param {Array<string>} [config.protectedNamespaces=[]] - Protected namespace IRIs
   */
  constructor(config) {
    if (new.target === Partition) {
      throw new Error('Partition is an abstract class and cannot be instantiated directly');
    }

    const validated = PartitionConfigSchema.parse(config);

    /** @type {string} Partition name */
    this.name = validated.name;

    /** @type {string|undefined} Partition description */
    this.description = validated.description;

    /** @type {boolean} Whether partition is read-only */
    this.readOnly = validated.readOnly;

    /** @type {Array<string>} Allowed namespace IRIs */
    this.namespaceIris = validated.namespaceIris;

    /** @type {Array<string>} Protected namespace IRIs */
    this.protectedNamespaces = validated.protectedNamespaces;

    /** @type {import('@unrdf/oxigraph').OxigraphStore} RDF store */
    this.store = createStore();

    /** @type {string|null} Content hash (computed lazily) */
    this._contentHash = null;
  }

  /**
   * Add a quad to the partition
   *
   * @param {import('@unrdf/oxigraph').Quad} quad - Quad to add
   * @throws {Error} If partition is read-only or namespace is protected
   */
  add(quad) {
    if (this.readOnly) {
      throw new Error(`Cannot add quad to read-only partition: ${this.name}`);
    }

    // Check if quad namespace is protected
    const namespace = this._extractQuadNamespace(quad);
    if (this.protectedNamespaces.includes(namespace)) {
      throw new Error(
        `Cannot add quad to protected namespace: ${namespace} (partition: ${this.name})`
      );
    }

    this.store.add(quad);
    this._contentHash = null; // Invalidate cache
  }

  /**
   * Delete a quad from the partition
   *
   * @param {import('@unrdf/oxigraph').Quad} quad - Quad to delete
   * @throws {Error} If partition is read-only or namespace is protected
   */
  delete(quad) {
    if (this.readOnly) {
      throw new Error(`Cannot delete quad from read-only partition: ${this.name}`);
    }

    // Check if quad namespace is protected
    const namespace = this._extractQuadNamespace(quad);
    if (this.protectedNamespaces.includes(namespace)) {
      throw new Error(
        `Cannot delete quad from protected namespace: ${namespace} (partition: ${this.name})`
      );
    }

    this.store.delete(quad);
    this._contentHash = null; // Invalidate cache
  }

  /**
   * Match quads in the partition
   *
   * @param {*} subject - Subject pattern
   * @param {*} predicate - Predicate pattern
   * @param {*} object - Object pattern
   * @param {*} graph - Graph pattern
   * @returns {Array<import('@unrdf/oxigraph').Quad>} Matching quads
   */
  match(subject, predicate, object, graph) {
    return this.store.match(subject, predicate, object, graph);
  }

  /**
   * Execute a SPARQL query on the partition
   *
   * @param {string} query - SPARQL query
   * @returns {*} Query results
   */
  query(query) {
    return this.store.query(query);
  }

  /**
   * Get partition size (number of quads)
   *
   * @returns {number} Number of quads
   */
  get size() {
    return this.store.size;
  }

  /**
   * Compute content hash for the partition
   *
   * @returns {Promise<string>} SHA256 content hash
   */
  async getContentHash() {
    if (this._contentHash) {
      return this._contentHash;
    }

    const quads = this.store.match();
    this._contentHash = await computeContentHash(quads);
    return this._contentHash;
  }

  /**
   * Load Turtle content into the partition
   *
   * @param {string} turtleContent - Turtle RDF content
   * @param {string} [baseIri] - Base IRI
   * @throws {Error} If partition is read-only
   */
  loadTurtle(turtleContent, baseIri) {
    if (this.readOnly) {
      throw new Error(`Cannot load content into read-only partition: ${this.name}`);
    }

    this.store.load(turtleContent, {
      format: 'text/turtle',
      baseIRI: baseIri || 'http://example.org/',
    });

    this._contentHash = null; // Invalidate cache
  }

  /**
   * Extract namespace from a quad (using subject)
   *
   * @param {import('@unrdf/oxigraph').Quad} quad - Quad
   * @returns {string} Namespace IRI
   * @private
   */
  _extractQuadNamespace(quad) {
    if (quad.subject.termType === 'NamedNode') {
      const iri = quad.subject.value;
      const hashIndex = iri.lastIndexOf('#');
      if (hashIndex !== -1) return iri.substring(0, hashIndex + 1);

      const slashIndex = iri.lastIndexOf('/');
      if (slashIndex !== -1) return iri.substring(0, slashIndex + 1);

      return iri;
    }

    return '';
  }

  /**
   * Convert partition to JSON
   *
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      type: this.constructor.name,
      name: this.name,
      description: this.description,
      readOnly: this.readOnly,
      namespaceIris: this.namespaceIris,
      protectedNamespaces: this.protectedNamespaces,
      size: this.size,
    };
  }
}

/**
 * Industrial Substrate Partition
 *
 * Contains foundational ontologies (PROV, ODRL, SKOS, OWL-Time, DCAT, ORG, OA).
 * Always read-only.
 *
 * @class IndustrialSubstrate
 * @extends Partition
 */
export class IndustrialSubstrate extends Partition {
  constructor() {
    super({
      name: 'IndustrialSubstrate',
      description: 'Foundational W3C ontologies (PROV, ODRL, SKOS, OWL-Time, DCAT, ORG, OA)',
      readOnly: true,
      namespaceIris: [
        'http://www.w3.org/ns/prov#',
        'http://www.w3.org/ns/odrl/2/',
        'http://www.w3.org/2004/02/skos/core#',
        'http://www.w3.org/2006/time#',
        'http://www.w3.org/ns/dcat#',
        'http://www.w3.org/ns/org#',
        'http://www.w3.org/ns/oa#',
      ],
      protectedNamespaces: [
        'http://www.w3.org/ns/prov#',
        'http://www.w3.org/ns/odrl/2/',
        'http://www.w3.org/2004/02/skos/core#',
        'http://www.w3.org/2006/time#',
        'http://www.w3.org/ns/dcat#',
        'http://www.w3.org/ns/org#',
        'http://www.w3.org/ns/oa#',
      ],
    });
  }
}

/**
 * Corporate Canon Partition
 *
 * Contains enterprise-wide canonical data models and vocabularies.
 * Read-only by default.
 *
 * @class CorporateCanon
 * @extends Partition
 */
export class CorporateCanon extends Partition {
  constructor() {
    super({
      name: 'CorporateCanon',
      description: 'Enterprise-wide canonical data models and vocabularies',
      readOnly: true,
      namespaceIris: [],
      protectedNamespaces: [],
    });
  }
}

/**
 * Business Unit Overlay Partition
 *
 * Contains business unit-specific extensions and customizations.
 * Read-write by default.
 *
 * @class BusinessUnitOverlay
 * @extends Partition
 */
export class BusinessUnitOverlay extends Partition {
  constructor() {
    super({
      name: 'BusinessUnitOverlay',
      description: 'Business unit-specific extensions and customizations',
      readOnly: false,
      namespaceIris: [],
      protectedNamespaces: [],
    });
  }
}

/**
 * Regional Overlay Partition
 *
 * Contains region-specific data and localization.
 * Read-write by default.
 *
 * @class RegionalOverlay
 * @extends Partition
 */
export class RegionalOverlay extends Partition {
  constructor() {
    super({
      name: 'RegionalOverlay',
      description: 'Region-specific data and localization',
      readOnly: false,
      namespaceIris: [],
      protectedNamespaces: [],
    });
  }
}

/**
 * Execution Ledger Partition
 *
 * Contains execution events, transactions, and audit trails.
 * Read-write (append-only in practice).
 *
 * @class ExecutionLedger
 * @extends Partition
 */
export class ExecutionLedger extends Partition {
  constructor() {
    super({
      name: 'ExecutionLedger',
      description: 'Execution events, transactions, and audit trails',
      readOnly: false,
      namespaceIris: [],
      protectedNamespaces: [],
    });
  }
}

/**
 * System Policy Partition
 *
 * Contains system policies, access controls, and governance rules.
 * Read-only by default.
 *
 * @class SystemPolicyPartition
 * @extends Partition
 */
export class SystemPolicyPartition extends Partition {
  constructor() {
    super({
      name: 'SystemPolicyPartition',
      description: 'System policies, access controls, and governance rules',
      readOnly: true,
      namespaceIris: [],
      protectedNamespaces: [],
    });
  }
}
