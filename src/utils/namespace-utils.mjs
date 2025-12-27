/**
 * @fileoverview Namespace utilities - RDF vocabulary and namespace management
 *
 * These utilities provide comprehensive namespace management, vocabulary handling,
 * and prefix management for RDF operations.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { DataFactory } from 'n3';
import { _asNamedNode } from './term-utils.mjs';
import { createNamespaceId } from './id-utils.mjs';

const { namedNode } = DataFactory;

/**
 * Common RDF vocabularies and their namespaces
 */
export const COMMON_VOCABULARIES = {
  // Core RDF vocabularies
  RDF: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  RDFS: 'http://www.w3.org/2000/01/rdf-schema#',
  OWL: 'http://www.w3.org/2002/07/owl#',
  XSD: 'http://www.w3.org/2001/XMLSchema#',

  // Dublin Core
  DC: 'http://purl.org/dc/elements/1.1/',
  DCTERMS: 'http://purl.org/dc/terms/',

  // FOAF
  FOAF: 'http://xmlns.com/foaf/0.1/',

  // SKOS
  SKOS: 'http://www.w3.org/2004/02/skos/core#',

  // Schema.org
  SCHEMA: 'https://schema.org/',

  // PROV
  PROV: 'http://www.w3.org/ns/prov#',

  // SHACL
  SHACL: 'http://www.w3.org/ns/shacl#',

  // Time
  TIME: 'http://www.w3.org/2006/time#',

  // Geo
  GEO: 'http://www.opengis.net/ont/geosparql#',
  WGS84: 'http://www.w3.org/2003/01/geo/wgs84_pos#',

  // Creative Commons
  CC: 'http://creativecommons.org/ns#',

  // DOAP
  DOAP: 'http://usefulinc.com/ns/doap#',

  // VCard
  VCARD: 'http://www.w3.org/2006/vcard/ns#',
};

/**
 * Common prefixes for serialization
 */
export const COMMON_PREFIXES = {
  rdf: COMMON_VOCABULARIES.RDF,
  rdfs: COMMON_VOCABULARIES.RDFS,
  owl: COMMON_VOCABULARIES.OWL,
  xsd: COMMON_VOCABULARIES.XSD,
  dc: COMMON_VOCABULARIES.DC,
  dcterms: COMMON_VOCABULARIES.DCTERMS,
  foaf: COMMON_VOCABULARIES.FOAF,
  skos: COMMON_VOCABULARIES.SKOS,
  schema: COMMON_VOCABULARIES.SCHEMA,
  prov: COMMON_VOCABULARIES.PROV,
  sh: COMMON_VOCABULARIES.SHACL,
  time: COMMON_VOCABULARIES.TIME,
  geo: COMMON_VOCABULARIES.GEO,
  wgs84: COMMON_VOCABULARIES.WGS84,
  cc: COMMON_VOCABULARIES.CC,
  doap: COMMON_VOCABULARIES.DOAP,
  vcard: COMMON_VOCABULARIES.VCARD,
};

/**
 * Namespace manager class
 */
export class NamespaceManager {
  /**
   *
   */
  constructor() {
    this.namespaces = new Map();
    this.prefixes = new Map();
    this.reverseLookup = new Map();

    // Initialize with common vocabularies
    this.addCommonVocabularies();
  }

  /**
   * Add common vocabularies to the manager
   */
  addCommonVocabularies() {
    for (const [prefix, namespace] of Object.entries(COMMON_PREFIXES)) {
      this.addNamespace(prefix, namespace);
    }
  }

  /**
   * Add a namespace with a prefix
   * @param {string} prefix - The prefix (e.g., 'foaf')
   * @param {string} namespace - The namespace URI (e.g., 'http://xmlns.com/foaf/0.1/')
   */
  addNamespace(prefix, namespace) {
    // Ensure namespace ends with # or /
    const normalizedNamespace =
      namespace.endsWith('#') || namespace.endsWith('/') ? namespace : `${namespace}#`;

    this.namespaces.set(prefix, normalizedNamespace);
    this.prefixes.set(normalizedNamespace, prefix);
    this.reverseLookup.set(normalizedNamespace, prefix);
  }

  /**
   * Get namespace for a prefix
   * @param {string} prefix - The prefix
   * @returns {string|null} The namespace URI or null if not found
   */
  getNamespace(prefix) {
    return this.namespaces.get(prefix) || null;
  }

  /**
   * Get prefix for a namespace
   * @param {string} namespace - The namespace URI
   * @returns {string|null} The prefix or null if not found
   */
  getPrefix(namespace) {
    return this.prefixes.get(namespace) || null;
  }

  /**
   * Create a named node using a prefix and local name
   * @param {string} prefix - The prefix
   * @param {string} localName - The local name
   * @returns {import('n3').NamedNode} The named node
   */
  createNamedNode(prefix, localName) {
    const namespace = this.getNamespace(prefix);
    if (!namespace) {
      throw new Error(`Unknown prefix: ${prefix}`);
    }
    return namedNode(createNamespaceId(namespace, localName));
  }

  /**
   * Expand a prefixed IRI to full IRI
   * @param {string} prefixedIRI - The prefixed IRI (e.g., 'foaf:name')
   * @returns {string} The full IRI
   */
  expandIRI(prefixedIRI) {
    const [prefix, localName] = prefixedIRI.split(':');
    if (!localName) {
      return prefixedIRI; // Not a prefixed IRI
    }

    const namespace = this.getNamespace(prefix);
    if (!namespace) {
      throw new Error(`Unknown prefix: ${prefix}`);
    }

    return createNamespaceId(namespace, localName);
  }

  /**
   * Contract a full IRI to prefixed form
   * @param {string} fullIRI - The full IRI
   * @returns {string} The prefixed IRI or original if no prefix found
   */
  contractIRI(fullIRI) {
    for (const [namespace, prefix] of this.prefixes.entries()) {
      if (fullIRI.startsWith(namespace)) {
        const localName = fullIRI.slice(namespace.length);
        return `${prefix}:${localName}`;
      }
    }
    return fullIRI;
  }

  /**
   * Get all registered prefixes
   * @returns {string[]} Array of prefixes
   */
  getPrefixes() {
    return [...this.namespaces.keys()];
  }

  /**
   * Get all registered namespaces
   * @returns {string[]} Array of namespace URIs
   */
  getNamespaces() {
    return [...this.namespaces.values()];
  }

  /**
   * Export prefixes for serialization
   * @returns {Object} Object with prefix mappings
   */
  exportPrefixes() {
    const result = {};
    for (const [prefix, namespace] of this.namespaces.entries()) {
      result[prefix] = namespace;
    }
    return result;
  }

  /**
   * Import prefixes from an object
   * @param {Object} prefixes - Object with prefix mappings
   */
  importPrefixes(prefixes) {
    for (const [prefix, namespace] of Object.entries(prefixes)) {
      this.addNamespace(prefix, namespace);
    }
  }

  /**
   * Clear all namespaces
   */
  clear() {
    this.namespaces.clear();
    this.prefixes.clear();
    this.reverseLookup.clear();
  }

  /**
   * Remove a namespace
   * @param {string} prefix - The prefix to remove
   */
  removeNamespace(prefix) {
    const namespace = this.namespaces.get(prefix);
    if (namespace) {
      this.namespaces.delete(prefix);
      this.prefixes.delete(namespace);
      this.reverseLookup.delete(namespace);
    }
  }
}

/**
 * Create a new namespace manager
 * @returns {NamespaceManager} New namespace manager instance
 */
export const createNamespaceManager = () => new NamespaceManager();

/**
 * Get a named node using common vocabulary
 * @param {string} vocabulary - The vocabulary key (e.g., 'FOAF')
 * @param {string} localName - The local name
 * @returns {import('n3').NamedNode} The named node
 */
export const getVocabularyTerm = (vocabulary, localName) => {
  const namespace = COMMON_VOCABULARIES[vocabulary.toUpperCase()];
  if (!namespace) {
    throw new Error(`Unknown vocabulary: ${vocabulary}`);
  }
  return namedNode(createNamespaceId(namespace, localName));
};

/**
 * Check if an IRI belongs to a known vocabulary
 * @param {string} iri - The IRI to check
 * @returns {string|null} The vocabulary name or null if not found
 */
export const getVocabularyForIRI = iri => {
  for (const [vocab, namespace] of Object.entries(COMMON_VOCABULARIES)) {
    if (iri.startsWith(namespace)) {
      return vocab;
    }
  }
  return null;
};

/**
 * Get vocabulary statistics from a store
 * @param {import('n3').Store} store - The RDF store
 * @returns {Object} Statistics about vocabulary usage
 */
export const getVocabularyStats = store => {
  const stats = {};
  const vocabularies = new Set();

  for (const quad of store) {
    // Check subject
    const subjectVocab = getVocabularyForIRI(quad.subject.value);
    if (subjectVocab) {
      vocabularies.add(subjectVocab);
      stats[subjectVocab] = (stats[subjectVocab] || 0) + 1;
    }

    // Check predicate
    const predicateVocab = getVocabularyForIRI(quad.predicate.value);
    if (predicateVocab) {
      vocabularies.add(predicateVocab);
      stats[predicateVocab] = (stats[predicateVocab] || 0) + 1;
    }

    // Check object if it's a named node
    if (quad.object.termType === 'NamedNode') {
      const objectVocab = getVocabularyForIRI(quad.object.value);
      if (objectVocab) {
        vocabularies.add(objectVocab);
        stats[objectVocab] = (stats[objectVocab] || 0) + 1;
      }
    }
  }

  return {
    vocabularies: [...vocabularies],
    usage: stats,
    totalVocabularies: vocabularies.size,
  };
};

/**
 * Validate namespace consistency in a store
 * @param {import('n3').Store} store - The RDF store
 * @returns {Object} Validation result
 */
export const validateNamespaces = store => {
  const _issues = [];
  const usedNamespaces = new Set();
  const unknownNamespaces = new Set();

  for (const quad of store) {
    const terms = [quad.subject, quad.predicate, quad.object];
    if (quad.graph) terms.push(quad.graph);

    for (const term of terms) {
      if (term.termType === 'NamedNode') {
        const iri = term.value;
        const vocab = getVocabularyForIRI(iri);

        if (vocab) {
          usedNamespaces.add(vocab);
        } else {
          // Extract namespace from IRI
          const hashIndex = iri.lastIndexOf('#');
          const slashIndex = iri.lastIndexOf('/');
          const index = Math.max(hashIndex, slashIndex);

          if (index > 0) {
            const namespace = iri.slice(0, Math.max(0, index + 1));
            unknownNamespaces.add(namespace);
          }
        }
      }
    }
  }

  return {
    valid: unknownNamespaces.size === 0,
    usedVocabularies: [...usedNamespaces],
    unknownNamespaces: [...unknownNamespaces],
    issueCount: unknownNamespaces.size,
  };
};

/**
 * Generate Turtle prefix declarations
 * @param {Object} prefixes - Prefix mappings
 * @returns {string} Turtle prefix declarations
 */
export const generateTurtlePrefixes = prefixes => {
  const lines = [];
  for (const [prefix, namespace] of Object.entries(prefixes)) {
    lines.push(`@prefix ${prefix}: <${namespace}> .`);
  }
  return lines.join('\n') + '\n';
};

/**
 * Generate SPARQL prefix declarations
 * @param {Object} prefixes - Prefix mappings
 * @returns {string} SPARQL prefix declarations
 */
export const generateSPARQLPrefixes = prefixes => {
  const lines = [];
  for (const [prefix, namespace] of Object.entries(prefixes)) {
    lines.push(`PREFIX ${prefix}: <${namespace}>`);
  }
  return lines.join('\n') + '\n';
};

/**
 * Create a namespace function
 * @param {string} namespace - Base namespace IRI
 * @returns {Function} Function that creates IRIs in the namespace
 */
export const createNamespace = namespace => {
  return localName => `${namespace}${localName}`;
};

/**
 * Expand a CURIE to full IRI
 * @param {string} curie - CURIE to expand (e.g., "foaf:Person")
 * @param {Object} prefixes - Prefix mappings
 * @returns {string} Full IRI
 */
export const expandCurie = (curie, prefixes) => {
  const colonIndex = curie.indexOf(':');
  if (colonIndex === -1) {
    return curie; // Not a CURIE
  }

  const prefix = curie.slice(0, Math.max(0, colonIndex));
  const localName = curie.slice(Math.max(0, colonIndex + 1));

  if (prefixes[prefix]) {
    return `${prefixes[prefix]}${localName}`;
  }

  return curie; // Unknown prefix, return as-is
};

/**
 * Shrink a full IRI to CURIE if possible
 * @param {string} iri - Full IRI to shrink
 * @param {Object} prefixes - Prefix mappings
 * @returns {string} CURIE or original IRI if no match
 */
export const shrinkIri = (iri, prefixes) => {
  for (const [prefix, namespace] of Object.entries(prefixes)) {
    if (iri.startsWith(namespace)) {
      const localName = iri.slice(namespace.length);
      return `${prefix}:${localName}`;
    }
  }

  return iri; // No matching prefix found
};
