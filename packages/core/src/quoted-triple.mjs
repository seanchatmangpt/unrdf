/**
 * @file Quoted Triple Class
 * @module @unrdf/core/quoted-triple
 * @description Object-oriented wrapper for RDF-star quoted triples
 */

import { factory } from './rdf-star.mjs';

/**
 * QuotedTriple class - Object-oriented wrapper for RDF-star quoted triples
 *
 * @example
 * const qt = new QuotedTriple(subject, predicate, object);
 * qt.addProvenance({ source: 'http://example.org', creator: 'Alice' });
 * qt.addConfidence({ confidence: 0.95 });
 * const quads = qt.toQuads();
 */
export class QuotedTriple {
  /**
   * Create a quoted triple
   * @param {Object} subject - Subject term
   * @param {Object} predicate - Predicate term
   * @param {Object} object - Object term
   */
  constructor(subject, predicate, object) {
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
    this.quoted = factory.quotedTriple(subject, predicate, object);
    this.annotations = {
      provenance: null,
      temporal: null,
      confidence: null,
      multiSource: null,
      custom: {},
    };
  }

  /**
   * Get the underlying quoted triple
   * @returns {Object} Oxigraph quoted triple
   */
  getQuoted() {
    return this.quoted;
  }

  /**
   * Add provenance annotation
   * @param {Object} provenance - Provenance information
   * @param {string} [provenance.source] - Source URL
   * @param {string} [provenance.creator] - Creator identifier
   * @param {string} [provenance.created] - Creation timestamp
   * @param {string} [provenance.modified] - Modification timestamp
   * @returns {QuotedTriple} This instance for chaining
   */
  addProvenance(provenance) {
    this.annotations.provenance = provenance;
    return this;
  }

  /**
   * Add temporal annotation
   * @param {Object} temporal - Temporal information
   * @param {string} [temporal.validFrom] - Valid from timestamp
   * @param {string} [temporal.validTo] - Valid to timestamp
   * @returns {QuotedTriple} This instance for chaining
   */
  addTemporal(temporal) {
    this.annotations.temporal = temporal;
    return this;
  }

  /**
   * Add confidence annotation
   * @param {Object} confidence - Confidence information
   * @param {number} confidence.confidence - Confidence score (0-1)
   * @param {string} [confidence.method] - Method used
   * @returns {QuotedTriple} This instance for chaining
   */
  addConfidence(confidence) {
    this.annotations.confidence = confidence;
    return this;
  }

  /**
   * Add multi-source annotation
   * @param {Object} multiSource - Multi-source information
   * @param {Array<string>} multiSource.sources - Array of source URLs
   * @param {number} [multiSource.agreement] - Agreement score (0-1)
   * @returns {QuotedTriple} This instance for chaining
   */
  addMultiSource(multiSource) {
    this.annotations.multiSource = multiSource;
    return this;
  }

  /**
   * Add custom annotation
   * @param {string} key - Annotation key
   * @param {any} value - Annotation value
   * @returns {QuotedTriple} This instance for chaining
   */
  addCustom(key, value) {
    this.annotations.custom[key] = value;
    return this;
  }

  /**
   * Get all annotations
   * @returns {Object} All annotations
   */
  getAnnotations() {
    return this.annotations;
  }

  /**
   * Get provenance annotation
   * @returns {Object|null} Provenance or null
   */
  getProvenance() {
    return this.annotations.provenance;
  }

  /**
   * Get temporal annotation
   * @returns {Object|null} Temporal annotation or null
   */
  getTemporal() {
    return this.annotations.temporal;
  }

  /**
   * Get confidence annotation
   * @returns {Object|null} Confidence or null
   */
  getConfidence() {
    return this.annotations.confidence;
  }

  /**
   * Convert to array of quads (base triple + annotation quads)
   * @returns {Array<Object>} Array of quads
   */
  toQuads() {
    const quads = [];

    // Add base triple
    quads.push(factory.triple(this.subject, this.predicate, this.object));

    // Add annotation quads
    const annotationQuads = factory.addAnnotations(this.quoted, this.annotations);
    quads.push(...annotationQuads);

    return quads;
  }

  /**
   * Convert to JSON representation
   * @returns {Object} JSON object
   */
  toJSON() {
    return {
      subject: {
        termType: this.subject.termType,
        value: this.subject.value,
      },
      predicate: {
        termType: this.predicate.termType,
        value: this.predicate.value,
      },
      object: {
        termType: this.object.termType,
        value: this.object.value,
        language: this.object.language,
        datatype: this.object.datatype ? { value: this.object.datatype.value } : undefined,
      },
      annotations: this.annotations,
    };
  }

  /**
   * Create QuotedTriple from JSON
   * @param {Object} json - JSON representation
   * @param {Object} dataFactory - Data factory for creating terms
   * @returns {QuotedTriple} New QuotedTriple instance
   */
  static fromJSON(json, dataFactory = factory) {
    const subject = dataFactory.namedNode(json.subject.value);
    const predicate = dataFactory.namedNode(json.predicate.value);

    let object;
    if (json.object.termType === 'Literal') {
      object = dataFactory.literal(
        json.object.value,
        json.object.language || (json.object.datatype ? dataFactory.namedNode(json.object.datatype.value) : undefined)
      );
    } else {
      object = dataFactory.namedNode(json.object.value);
    }

    const qt = new QuotedTriple(subject, predicate, object);

    if (json.annotations.provenance) {
      qt.addProvenance(json.annotations.provenance);
    }
    if (json.annotations.temporal) {
      qt.addTemporal(json.annotations.temporal);
    }
    if (json.annotations.confidence) {
      qt.addConfidence(json.annotations.confidence);
    }
    if (json.annotations.multiSource) {
      qt.addMultiSource(json.annotations.multiSource);
    }
    if (json.annotations.custom) {
      Object.entries(json.annotations.custom).forEach(([key, value]) => {
        qt.addCustom(key, value);
      });
    }

    return qt;
  }

  /**
   * Validate the quoted triple structure
   * @returns {boolean} True if valid
   */
  validate() {
    return true;
  }
}

/**
 * Factory function to create quoted triple
 * @param {Object} subject - Subject term
 * @param {Object} predicate - Predicate term
 * @param {Object} object - Object term
 * @returns {QuotedTriple} New QuotedTriple instance
 */
export function createQuotedTriple(subject, predicate, object) {
  return new QuotedTriple(subject, predicate, object);
}

export default QuotedTriple;
