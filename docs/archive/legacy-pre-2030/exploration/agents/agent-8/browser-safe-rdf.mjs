/**
 * Browser-Safe RDF Module
 *
 * A dual-runtime module that provides core RDF operations in both Node.js and browser
 * without any Node-specific dependencies (no fs, path, os, process, etc.)
 *
 * Usage:
 *   // Node.js
 *   import { RDFStore, createQuad } from './browser-safe-rdf.mjs';
 *
 *   // Browser (works the same!)
 *   import { RDFStore, createQuad } from './browser-safe-rdf.mjs';
 *
 * This module demonstrates the "dual-runtime safe path" by:
 * 1. Using ONLY in-memory data structures (no I/O)
 * 2. Using ONLY standard JavaScript APIs (no Node modules)
 * 3. Using ONLY dual-runtime dependencies (@rdfjs packages)
 * 4. Abstracting file operations via provider pattern
 */

/**
 * Represents an RDF Term (Named Node, Literal, or Blank Node)
 */
class RDFTerm {
  constructor(termType, value, language = null, datatype = null) {
    this.termType = termType; // 'NamedNode' | 'Literal' | 'BlankNode'
    this.value = value;
    this.language = language; // For Literals
    this.datatype = datatype; // For Literals (e.g., xsd:string)
  }

  equals(other) {
    if (!other) return false;
    if (this.termType !== other.termType) return false;
    if (this.value !== other.value) return false;
    if (this.termType === 'Literal') {
      return this.language === other.language && this.datatype === other.datatype;
    }
    return true;
  }

  toString() {
    if (this.termType === 'NamedNode') {
      return `<${this.value}>`;
    } else if (this.termType === 'Literal') {
      const langTag = this.language ? `@${this.language}` : '';
      const typeTag = this.datatype ? `^^${this.datatype}` : '';
      return `"${this.value}"${langTag}${typeTag}`;
    } else if (this.termType === 'BlankNode') {
      return `_:${this.value}`;
    }
    return String(this.value);
  }
}

/**
 * Create a Named Node (IRI)
 */
export function namedNode(iri) {
  return new RDFTerm('NamedNode', iri);
}

/**
 * Create a Literal
 */
export function literal(value, languageOrDatatype = null) {
  if (typeof languageOrDatatype === 'string') {
    if (languageOrDatatype.startsWith('http://') || languageOrDatatype.startsWith('https://')) {
      // It's a datatype IRI
      return new RDFTerm('Literal', String(value), null, languageOrDatatype);
    } else {
      // It's a language tag
      return new RDFTerm('Literal', String(value), languageOrDatatype, null);
    }
  }
  return new RDFTerm('Literal', String(value));
}

/**
 * Create a Blank Node
 */
export function blankNode(name = null) {
  const id = name || `b${Math.random().toString(36).substr(2, 9)}`;
  return new RDFTerm('BlankNode', id);
}

/**
 * Create a Quad (RDF triple in a named graph)
 */
export function quad(subject, predicate, object, graph = null) {
  // Validate terms
  if (!subject || !predicate || !object) {
    throw new Error('Subject, predicate, and object are required');
  }

  if (!(subject instanceof RDFTerm)) {
    throw new TypeError('Subject must be an RDFTerm');
  }
  if (!(predicate instanceof RDFTerm)) {
    throw new TypeError('Predicate must be an RDFTerm');
  }
  if (!(object instanceof RDFTerm)) {
    throw new TypeError('Object must be an RDFTerm');
  }

  return {
    subject,
    predicate,
    object,
    graph: graph || new RDFTerm('DefaultGraph', ''),
  };
}

/**
 * In-memory RDF Store (triple store)
 *
 * This is a pure, browser-safe implementation with no Node.js dependencies
 */
export class RDFStore {
  /**
   *
   */
  constructor(initialQuads = []) {
    this.quads = [];
    this.indexes = {
      // Index by subject
      bySubject: new Map(),
      // Index by predicate
      byPredicate: new Map(),
      // Index by object
      byObject: new Map(),
    };

    // Add initial quads
    if (Array.isArray(initialQuads)) {
      initialQuads.forEach(q => this.add(q));
    }
  }

  /**
   * Add a quad to the store
   */
  add(quad) {
    if (!quad) throw new Error('Quad is required');
    if (!quad.subject || !quad.predicate || !quad.object) {
      throw new Error('Quad must have subject, predicate, and object');
    }

    // Check if quad already exists
    const exists = this.quads.some(q => this._quadsEqual(q, quad));
    if (exists) return; // Already in store

    // Add quad
    this.quads.push(quad);

    // Update indexes
    this._updateIndexes(quad);
  }

  /**
   * Remove a quad from the store
   */
  delete(quad) {
    if (!quad) throw new Error('Quad is required');

    const index = this.quads.findIndex(q => this._quadsEqual(q, quad));
    if (index !== -1) {
      this.quads.splice(index, 1);
      this._rebuildIndexes();
    }
  }

  /**
   * Query quads by pattern
   * Pass null to match any term
   */
  match(subject = null, predicate = null, object = null, graph = null) {
    return this.quads.filter(q => {
      if (subject && !this._termsEqual(q.subject, subject)) return false;
      if (predicate && !this._termsEqual(q.predicate, predicate)) return false;
      if (object && !this._termsEqual(q.object, object)) return false;
      if (graph && !this._termsEqual(q.graph, graph)) return false;
      return true;
    });
  }

  /**
   * Get all quads with a given subject
   */
  getQuadsForSubject(subject) {
    return this.match(subject);
  }

  /**
   * Get all quads with a given predicate
   */
  getQuadsForPredicate(predicate) {
    return this.match(null, predicate);
  }

  /**
   * Get all quads with a given object
   */
  getQuadsForObject(object) {
    return this.match(null, null, object);
  }

  /**
   * Get all quads
   */
  getAll() {
    return [...this.quads];
  }

  /**
   * Get number of quads in store
   */
  size() {
    return this.quads.length;
  }

  /**
   * Clear the store
   */
  clear() {
    this.quads = [];
    this._rebuildIndexes();
  }

  /**
   * Export store as Turtle (simplified)
   */
  toTurtle() {
    const lines = ['@prefix : <http://example.org/>.', ''];

    for (const quad of this.quads) {
      const subStr = quad.subject.toString();
      const predStr = quad.predicate.toString();
      const objStr = quad.object.toString();

      lines.push(`${subStr} ${predStr} ${objStr} .`);
    }

    return lines.join('\n');
  }

  /**
   * Export store as Ntriples
   */
  toNTriples() {
    return this.quads.map(quad => {
      const subject = quad.subject.toString();
      const predicate = quad.predicate.toString();
      const object = quad.object.toString();
      return `${subject} ${predicate} ${object} .`;
    }).join('\n');
  }

  /**
   * Export store as JSON-LD
   */
  toJSON() {
    const context = {
      '@context': {
        '@vocab': 'http://example.org/',
      }
    };

    const bySubject = new Map();
    for (const quad of this.quads) {
      const subjId = quad.subject.value;
      if (!bySubject.has(subjId)) {
        bySubject.set(subjId, {
          '@id': subjId,
        });
      }

      const obj = bySubject.get(subjId);
      const pred = quad.predicate.value;
      const value = quad.object.termType === 'NamedNode'
        ? { '@id': quad.object.value }
        : quad.object.value;

      if (pred in obj) {
        if (!Array.isArray(obj[pred])) {
          obj[pred] = [obj[pred]];
        }
        obj[pred].push(value);
      } else {
        obj[pred] = value;
      }
    }

    return {
      ...context,
      '@graph': Array.from(bySubject.values())
    };
  }

  // Private helper methods

  /**
   *
   */
  _quadsEqual(q1, q2) {
    return this._termsEqual(q1.subject, q2.subject) &&
           this._termsEqual(q1.predicate, q2.predicate) &&
           this._termsEqual(q1.object, q2.object) &&
           this._termsEqual(q1.graph, q2.graph);
  }

  /**
   *
   */
  _termsEqual(t1, t2) {
    if (!t1 || !t2) return false;
    if (t1 === t2) return true;
    if (t1 instanceof RDFTerm && t2 instanceof RDFTerm) {
      return t1.equals(t2);
    }
    return false;
  }

  /**
   *
   */
  _updateIndexes(quad) {
    // Update subject index
    if (!this.indexes.bySubject.has(quad.subject.value)) {
      this.indexes.bySubject.set(quad.subject.value, []);
    }
    this.indexes.bySubject.get(quad.subject.value).push(quad);

    // Update predicate index
    if (!this.indexes.byPredicate.has(quad.predicate.value)) {
      this.indexes.byPredicate.set(quad.predicate.value, []);
    }
    this.indexes.byPredicate.get(quad.predicate.value).push(quad);

    // Update object index
    if (!this.indexes.byObject.has(quad.object.value)) {
      this.indexes.byObject.set(quad.object.value, []);
    }
    this.indexes.byObject.get(quad.object.value).push(quad);
  }

  /**
   *
   */
  _rebuildIndexes() {
    this.indexes.bySubject.clear();
    this.indexes.byPredicate.clear();
    this.indexes.byObject.clear();

    for (const quad of this.quads) {
      this._updateIndexes(quad);
    }
  }
}

/**
 * Dual-runtime File Provider Interface
 *
 * Implement this interface to abstract file I/O for both Node and browser
 */
export class FileProvider {
  /**
   * Read data from a path
   * @param {string} path - File path or URL
   * @returns {Promise<string>} File contents
   */
  async read(path) {
    throw new Error('FileProvider.read() must be implemented');
  }

  /**
   * Write data to a path
   * @param {string} path - File path
   * @param {string} data - Data to write
   * @returns {Promise<void>}
   */
  async write(path, data) {
    throw new Error('FileProvider.write() must be implemented');
  }
}

/**
 * Browser-compatible File Provider (uses fetch)
 *
 * For use in browser environments
 */
export class FetchFileProvider extends FileProvider {
  /**
   *
   */
  async read(path) {
    const response = await fetch(path);
    if (!response.ok) {
      throw new Error(`Failed to fetch ${path}: ${response.statusText}`);
    }
    return response.text();
  }

  /**
   *
   */
  async write(path, data) {
    const response = await fetch(path, {
      method: 'PUT',
      headers: { 'Content-Type': 'text/plain' },
      body: data
    });
    if (!response.ok) {
      throw new Error(`Failed to write to ${path}: ${response.statusText}`);
    }
  }
}

/**
 * RDF Data Factory - Factory methods for creating RDF terms and quads
 *
 * Dual-runtime implementation compatible with RDFjs spec
 */
export const dataFactory = {
  namedNode,
  literal,
  blankNode,
  quad,
  defaultGraph: () => new RDFTerm('DefaultGraph', ''),
};

/**
 * Common Namespaces - For convenience
 */
export const namespaces = {
  rdf: (localName) => namedNode(`http://www.w3.org/1999/02/22-rdf-syntax-ns#${localName}`),
  rdfs: (localName) => namedNode(`http://www.w3.org/2000/01/rdf-schema#${localName}`),
  xsd: (localName) => namedNode(`http://www.w3.org/2001/XMLSchema#${localName}`),
  foaf: (localName) => namedNode(`http://xmlns.com/foaf/0.1/${localName}`),
  dcterms: (localName) => namedNode(`http://purl.org/dc/terms/${localName}`),
  owl: (localName) => namedNode(`http://www.w3.org/2002/07/owl#${localName}`),
};

/**
 * Validator - Check if object is valid RDF term/quad
 */
export function isValidRDFTerm(obj) {
  return obj instanceof RDFTerm;
}

/**
 *
 */
export function isValidQuad(obj) {
  return obj && obj.subject && obj.predicate && obj.object &&
         isValidRDFTerm(obj.subject) &&
         isValidRDFTerm(obj.predicate) &&
         isValidRDFTerm(obj.object);
}

export default {
  RDFStore,
  RDFTerm,
  FileProvider,
  FetchFileProvider,
  dataFactory,
  namespaces,
  namedNode,
  literal,
  blankNode,
  quad,
  isValidRDFTerm,
  isValidQuad,
};
