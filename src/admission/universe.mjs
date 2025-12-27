/**
 * @file Universe - RDF Knowledge Graph Universe with Partitioned Ontologies
 * @module admission/universe
 */

import { createHash } from 'crypto';

// Simple validation without external dependencies
const validateString = (val, name) => {
  if (typeof val !== 'string') throw new Error(`${name} must be a string`);
  return val;
};

const validateBoolean = (val, name, defaultValue = false) => {
  if (val === undefined) return defaultValue;
  if (typeof val !== 'boolean') throw new Error(`${name} must be a boolean`);
  return val;
};

/**
 * Partition types in the Universe
 * @typedef {'IndustrialSubstrate' | 'SystemPolicyPartition' | 'StudiosOverlay' | 'ApplicationOverlay' | 'TemporalOverlay' | 'ProjectionOverlay'} PartitionType
 */

/**
 * Validate Ontology Release config
 * @param {object} config
 * @returns {object}
 */
function validateOntologyRelease(config) {
  return {
    iri: validateString(config.iri, 'iri'),
    version: validateString(config.version, 'version'),
    namespace: validateString(config.namespace, 'namespace'),
    isProtected: validateBoolean(config.isProtected, 'isProtected', false),
    isReadOnly: validateBoolean(config.isReadOnly, 'isReadOnly', false),
  };
}

/**
 * @class OntologyRelease
 * @description Represents a versioned ontology release
 */
export class OntologyRelease {
  /**
   * @param {object} config
   * @param {string} config.iri - Ontology IRI
   * @param {string} config.version - Ontology version
   * @param {string} config.namespace - Ontology namespace
   * @param {boolean} [config.isProtected=false] - Whether namespace is protected
   * @param {boolean} [config.isReadOnly=false] - Whether ontology is read-only
   */
  constructor(config) {
    const validated = validateOntologyRelease(config);
    Object.assign(this, validated);
  }

  /**
   * Get ontology IRI
   * @returns {string}
   */
  getIRI() {
    return this.iri;
  }

  /**
   * Get namespace
   * @returns {string}
   */
  getNamespace() {
    return this.namespace;
  }

  /**
   * Check if protected
   * @returns {boolean}
   */
  isProtectedNamespace() {
    return this.isProtected;
  }
}

/**
 * @class Partition
 * @description Represents a partition in the RDF Universe
 */
export class Partition {
  /**
   * @param {object} config
   * @param {string} config.name - Partition name
   * @param {PartitionType} config.type - Partition type
   * @param {string} config.iri - Partition IRI
   * @param {boolean} [config.isReadOnly=false] - Whether partition is read-only
   */
  constructor(config) {
    this.name = config.name;
    this.type = config.type;
    this.iri = config.iri;
    this.isReadOnly = config.isReadOnly || false;
    this.ontologies = [];
    this.protectedNamespaces = new Set();
  }

  /**
   * Add ontology to partition
   * @param {OntologyRelease} ontology
   */
  addOntology(ontology) {
    this.ontologies.push(ontology);
    if (ontology.isProtectedNamespace()) {
      this.protectedNamespaces.add(ontology.getNamespace());
    }
  }

  /**
   * Get partition IRI
   * @returns {string}
   */
  getIRI() {
    return this.iri;
  }

  /**
   * Check if partition is read-only
   * @returns {boolean}
   */
  isReadOnlyPartition() {
    return this.isReadOnly;
  }

  /**
   * Get protected namespaces
   * @returns {Set<string>}
   */
  getProtectedNamespaces() {
    return this.protectedNamespaces;
  }

  /**
   * Get ontologies
   * @returns {OntologyRelease[]}
   */
  getOntologies() {
    return this.ontologies;
  }
}

/**
 * @class Universe
 * @description RDF Knowledge Graph Universe with partitioned ontologies
 */
export class Universe {
  /**
   * @param {object} [config={}]
   */
  constructor(config = {}) {
    this.partitions = new Map();
    this.store = new Map(); // Simple in-memory store
    this.contentHash = null;
  }

  /**
   * Load universe from TTL ontology
   * @param {string} ttlContent - Turtle format ontology
   * @returns {Promise<void>}
   */
  async loadFromTTL(ttlContent) {
    // Parse TTL and create partitions
    // For now, create default 6 partitions based on spec
    this._createDefaultPartitions();

    // Compute content hash
    this.contentHash = this._computeContentHash(ttlContent);
  }

  /**
   * Create default 6 partitions
   * @private
   */
  _createDefaultPartitions() {
    const partitionConfigs = [
      {
        name: 'IndustrialSubstrate',
        type: 'IndustrialSubstrate',
        iri: 'https://unrdf.org/partition/substrate',
        isReadOnly: true,
      },
      {
        name: 'SystemPolicyPartition',
        type: 'SystemPolicyPartition',
        iri: 'https://unrdf.org/partition/policy',
        isReadOnly: false,
      },
      {
        name: 'StudiosOverlay',
        type: 'StudiosOverlay',
        iri: 'https://unrdf.org/partition/studios',
        isReadOnly: false,
      },
      {
        name: 'ApplicationOverlay',
        type: 'ApplicationOverlay',
        iri: 'https://unrdf.org/partition/application',
        isReadOnly: false,
      },
      {
        name: 'TemporalOverlay',
        type: 'TemporalOverlay',
        iri: 'https://unrdf.org/partition/temporal',
        isReadOnly: false,
      },
      {
        name: 'ProjectionOverlay',
        type: 'ProjectionOverlay',
        iri: 'https://unrdf.org/partition/projection',
        isReadOnly: false,
      },
    ];

    for (const config of partitionConfigs) {
      const partition = new Partition(config);
      this.partitions.set(config.name, partition);
    }

    // Add protected namespaces to SystemPolicyPartition
    const policyPartition = this.partitions.get('SystemPolicyPartition');
    const protectedNamespaces = [
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
      'http://www.w3.org/2000/01/rdf-schema#',
      'http://www.w3.org/2002/07/owl#',
      'http://www.w3.org/2001/XMLSchema#',
      'http://www.w3.org/ns/shacl#',
      'http://purl.org/dc/terms/',
      'http://xmlns.com/foaf/0.1/',
    ];

    for (const ns of protectedNamespaces) {
      policyPartition.protectedNamespaces.add(ns);
    }

    // Add 7 allowed ontologies
    const allowedOntologies = [
      new OntologyRelease({
        iri: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        version: '1.1',
        namespace: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        isProtected: true,
      }),
      new OntologyRelease({
        iri: 'http://www.w3.org/2000/01/rdf-schema#',
        version: '1.1',
        namespace: 'http://www.w3.org/2000/01/rdf-schema#',
        isProtected: true,
      }),
      new OntologyRelease({
        iri: 'http://www.w3.org/2002/07/owl#',
        version: '2',
        namespace: 'http://www.w3.org/2002/07/owl#',
        isProtected: true,
      }),
      new OntologyRelease({
        iri: 'http://www.w3.org/2001/XMLSchema#',
        version: '1.1',
        namespace: 'http://www.w3.org/2001/XMLSchema#',
        isProtected: true,
      }),
      new OntologyRelease({
        iri: 'http://www.w3.org/ns/shacl#',
        version: '1',
        namespace: 'http://www.w3.org/ns/shacl#',
        isProtected: true,
      }),
      new OntologyRelease({
        iri: 'http://purl.org/dc/terms/',
        version: '2020-01-20',
        namespace: 'http://purl.org/dc/terms/',
        isProtected: true,
      }),
      new OntologyRelease({
        iri: 'http://xmlns.com/foaf/0.1/',
        version: '0.99',
        namespace: 'http://xmlns.com/foaf/0.1/',
        isProtected: true,
      }),
    ];

    for (const ontology of allowedOntologies) {
      policyPartition.addOntology(ontology);
    }
  }

  /**
   * Compute deterministic content hash
   * @param {string} content - Content to hash
   * @returns {string} - SHA-256 hash
   * @private
   */
  _computeContentHash(content) {
    return createHash('sha256').update(content).digest('hex');
  }

  /**
   * Get partition by name
   * @param {string} name - Partition name
   * @returns {Partition | undefined}
   */
  getPartition(name) {
    return this.partitions.get(name);
  }

  /**
   * Get all partitions
   * @returns {Partition[]}
   */
  getPartitions() {
    return Array.from(this.partitions.values());
  }

  /**
   * Get content hash
   * @returns {string | null}
   */
  getContentHash() {
    return this.contentHash;
  }

  /**
   * Verify partition IRIs are distinct
   * @returns {boolean}
   */
  verifyDistinctPartitionIRIs() {
    const iris = new Set();
    for (const partition of this.partitions.values()) {
      const iri = partition.getIRI();
      if (iris.has(iri)) {
        return false;
      }
      iris.add(iri);
    }
    return iris.size === this.partitions.size;
  }
}
