/**
 * @file RDF-star Data Factory and Utilities
 * @module @unrdf/core/rdf-star
 * @description Native RDF-star (W3C RDF 1.2) support for quoted triples and annotations
 *
 * RDF-star extends RDF with the ability to make statements about statements.
 * This module provides a data factory for creating quoted triples and utilities
 * for working with RDF-star features including provenance, temporal annotations,
 * and confidence scores.
 */

import oxigraph from 'oxigraph';
import {
  validateProvenance,
  validateTemporal,
  validateConfidence,
  validateAnnotation,
} from './rdf-star.schema.mjs';

/**
 * RDF-star namespace constants
 */
export const RDFSTAR = {
  confidence: 'http://www.w3.org/ns/rdf-star#confidence',
  source: 'http://www.w3.org/ns/rdf-star#source',
  validFrom: 'http://www.w3.org/ns/rdf-star#validFrom',
  validTo: 'http://www.w3.org/ns/rdf-star#validTo',
  creator: 'http://purl.org/dc/terms/creator',
  created: 'http://purl.org/dc/terms/created',
  modified: 'http://purl.org/dc/terms/modified',
};

/**
 * RDF-star Data Factory
 * Creates quoted triples and annotations compatible with Oxigraph
 */
export class RDFStarFactory {
  /**
   * Create a new RDF-star factory
   */
  constructor() {
    this.dataFactory = {
      namedNode: oxigraph.namedNode,
      blankNode: oxigraph.blankNode,
      literal: oxigraph.literal,
      defaultGraph: oxigraph.defaultGraph,
      quad: oxigraph.quad,
      triple: oxigraph.triple,
    };
  }

  /**
   * Create a quoted triple (RDF-star)
   * Note: Current Oxigraph JS bindings don't support quoted triples as subjects.
   * This returns a wrapper object with the triple and a blank node identifier.
   * @param {Object} subject - Subject term
   * @param {Object} predicate - Predicate term
   * @param {Object} object - Object term
   * @returns {Object} Quoted triple wrapper with triple and identifier
   * @example
   * const factory = new RDFStarFactory();
   * const quoted = factory.quotedTriple(
   *   factory.namedNode('http://example.org/Alice'),
   *   factory.namedNode('http://xmlns.com/foaf/0.1/knows'),
   *   factory.namedNode('http://example.org/Bob')
   * );
   */
  quotedTriple(subject, predicate, object) {
    const triple = this.dataFactory.triple(subject, predicate, object);
    const identifier = this.dataFactory.blankNode();

    return {
      triple,
      subject,
      predicate,
      object,
      identifier,
      termType: 'QuotedTriple',
      value: `<<${subject.value} ${predicate.value} ${object.value}>>`,
    };
  }

  /**
   * Create a named node
   * @param {string} iri - IRI of the named node
   * @returns {Object} Named node term
   */
  namedNode(iri) {
    return this.dataFactory.namedNode(iri);
  }

  /**
   * Create a blank node
   * @param {string} [label] - Optional blank node label
   * @returns {Object} Blank node term
   */
  blankNode(label) {
    return this.dataFactory.blankNode(label);
  }

  /**
   * Create a literal
   * @param {string} value - Literal value
   * @param {Object|string} [languageOrDatatype] - Language tag or datatype
   * @returns {Object} Literal term
   */
  literal(value, languageOrDatatype) {
    return this.dataFactory.literal(value, languageOrDatatype);
  }

  /**
   * Create a quad (triple with optional graph)
   * @param {Object} subject - Subject term (can be quoted triple)
   * @param {Object} predicate - Predicate term
   * @param {Object} object - Object term (can be quoted triple)
   * @param {Object} [graph] - Optional graph term
   * @returns {Object} Quad
   */
  quad(subject, predicate, object, graph) {
    return this.dataFactory.quad(subject, predicate, object, graph);
  }

  /**
   * Create a triple (quad with default graph)
   * @param {Object} subject - Subject term
   * @param {Object} predicate - Predicate term
   * @param {Object} object - Object term
   * @returns {Object} Triple (quad with default graph)
   */
  triple(subject, predicate, object) {
    return this.dataFactory.triple(subject, predicate, object);
  }

  /**
   * Add provenance annotation to a quoted triple
   * Uses blank node as proxy for quoted triple (Oxigraph compatibility)
   * @param {Object} quotedTriple - The quoted triple to annotate
   * @param {Object} provenance - Provenance information
   * @param {string} [provenance.source] - Source URL
   * @param {string} [provenance.creator] - Creator identifier
   * @param {string} [provenance.created] - Creation timestamp
   * @param {string} [provenance.modified] - Modification timestamp
   * @returns {Array<Object>} Array of annotation quads
   * @example
   * const quads = factory.addProvenance(quoted, {
   *   source: 'http://example.org/dataset',
   *   creator: 'Alice',
   *   created: '2026-01-11T10:00:00Z'
   * });
   */
  addProvenance(quotedTriple, provenance) {
    const validated = validateProvenance(provenance);
    const quads = [];
    const subject = quotedTriple.identifier || quotedTriple;

    if (validated.source) {
      quads.push(
        this.dataFactory.quad(
          subject,
          this.namedNode(RDFSTAR.source),
          this.namedNode(validated.source),
          this.dataFactory.defaultGraph()
        )
      );
    }

    if (validated.creator) {
      quads.push(
        this.dataFactory.quad(
          subject,
          this.namedNode(RDFSTAR.creator),
          this.literal(validated.creator),
          this.dataFactory.defaultGraph()
        )
      );
    }

    if (validated.created) {
      quads.push(
        this.dataFactory.quad(
          subject,
          this.namedNode(RDFSTAR.created),
          this.literal(validated.created, this.namedNode('http://www.w3.org/2001/XMLSchema#dateTime')),
          this.dataFactory.defaultGraph()
        )
      );
    }

    if (validated.modified) {
      quads.push(
        this.dataFactory.quad(
          subject,
          this.namedNode(RDFSTAR.modified),
          this.literal(validated.modified, this.namedNode('http://www.w3.org/2001/XMLSchema#dateTime')),
          this.dataFactory.defaultGraph()
        )
      );
    }

    return quads;
  }

  /**
   * Add temporal annotation to a quoted triple
   * Uses blank node as proxy for quoted triple (Oxigraph compatibility)
   * @param {Object} quotedTriple - The quoted triple to annotate
   * @param {Object} temporal - Temporal information
   * @param {string} [temporal.validFrom] - Valid from timestamp
   * @param {string} [temporal.validTo] - Valid to timestamp
   * @returns {Array<Object>} Array of annotation quads
   * @example
   * const quads = factory.addTemporal(quoted, {
   *   validFrom: '2026-01-01T00:00:00Z',
   *   validTo: '2026-12-31T23:59:59Z'
   * });
   */
  addTemporal(quotedTriple, temporal) {
    const validated = validateTemporal(temporal);
    const quads = [];
    const subject = quotedTriple.identifier || quotedTriple;

    if (validated.validFrom) {
      quads.push(
        this.dataFactory.quad(
          subject,
          this.namedNode(RDFSTAR.validFrom),
          this.literal(validated.validFrom, this.namedNode('http://www.w3.org/2001/XMLSchema#dateTime')),
          this.dataFactory.defaultGraph()
        )
      );
    }

    if (validated.validTo) {
      quads.push(
        this.dataFactory.quad(
          subject,
          this.namedNode(RDFSTAR.validTo),
          this.literal(validated.validTo, this.namedNode('http://www.w3.org/2001/XMLSchema#dateTime')),
          this.dataFactory.defaultGraph()
        )
      );
    }

    return quads;
  }

  /**
   * Add confidence annotation to a quoted triple
   * Uses blank node as proxy for quoted triple (Oxigraph compatibility)
   * @param {Object} quotedTriple - The quoted triple to annotate
   * @param {Object} confidence - Confidence information
   * @param {number} confidence.confidence - Confidence score (0-1)
   * @param {string} [confidence.method] - Method used to determine confidence
   * @returns {Array<Object>} Array of annotation quads
   * @example
   * const quads = factory.addConfidence(quoted, {
   *   confidence: 0.95,
   *   method: 'machine-learning'
   * });
   */
  addConfidence(quotedTriple, confidence) {
    const validated = validateConfidence(confidence);
    const quads = [];
    const subject = quotedTriple.identifier || quotedTriple;

    quads.push(
      this.dataFactory.quad(
        subject,
        this.namedNode(RDFSTAR.confidence),
        this.literal(validated.confidence.toString(), this.namedNode('http://www.w3.org/2001/XMLSchema#decimal')),
        this.dataFactory.defaultGraph()
      )
    );

    return quads;
  }

  /**
   * Add all annotations to a quoted triple
   * @param {Object} quotedTriple - The quoted triple to annotate
   * @param {Object} annotations - All annotation types
   * @param {Object} [annotations.provenance] - Provenance annotation
   * @param {Object} [annotations.temporal] - Temporal annotation
   * @param {Object} [annotations.confidence] - Confidence annotation
   * @returns {Array<Object>} Array of all annotation quads
   */
  addAnnotations(quotedTriple, annotations) {
    const validated = validateAnnotation(annotations);
    const quads = [];

    if (validated.provenance) {
      quads.push(...this.addProvenance(quotedTriple, validated.provenance));
    }

    if (validated.temporal) {
      quads.push(...this.addTemporal(quotedTriple, validated.temporal));
    }

    if (validated.confidence) {
      quads.push(...this.addConfidence(quotedTriple, validated.confidence));
    }

    return quads;
  }

  /**
   * Create an annotated triple (quoted triple + annotations)
   * @param {Object} subject - Subject term
   * @param {Object} predicate - Predicate term
   * @param {Object} object - Object term
   * @param {Object} annotations - Annotation object
   * @returns {Object} Object with quotedTriple and annotation quads
   * @example
   * const result = factory.createAnnotatedTriple(
   *   factory.namedNode('http://example.org/Alice'),
   *   factory.namedNode('http://xmlns.com/foaf/0.1/age'),
   *   factory.literal('30'),
   *   {
   *     confidence: { confidence: 0.95 },
   *     provenance: { source: 'http://example.org/dataset' }
   *   }
   * );
   */
  createAnnotatedTriple(subject, predicate, object, annotations) {
    const quotedTriple = this.quotedTriple(subject, predicate, object);
    const annotationQuads = this.addAnnotations(quotedTriple, annotations);

    const linkQuad = this.dataFactory.quad(
      quotedTriple.identifier,
      this.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      this.namedNode('http://www.w3.org/ns/rdf-star#QuotedTriple'),
      this.dataFactory.defaultGraph()
    );

    const subjectQuad = this.dataFactory.quad(
      quotedTriple.identifier,
      this.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#subject'),
      subject,
      this.dataFactory.defaultGraph()
    );

    const predicateQuad = this.dataFactory.quad(
      quotedTriple.identifier,
      this.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate'),
      predicate,
      this.dataFactory.defaultGraph()
    );

    const objectQuad = this.dataFactory.quad(
      quotedTriple.identifier,
      this.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#object'),
      object,
      this.dataFactory.defaultGraph()
    );

    return {
      quotedTriple,
      baseQuad: quotedTriple.triple,
      annotationQuads,
      reificationQuads: [linkQuad, subjectQuad, predicateQuad, objectQuad],
      allQuads: [quotedTriple.triple, linkQuad, subjectQuad, predicateQuad, objectQuad, ...annotationQuads],
    };
  }
}

/**
 * Default RDF-star factory instance
 */
export const factory = new RDFStarFactory();

/**
 * Utility: Check if a term is a quoted triple
 * @param {Object} term - Term to check
 * @returns {boolean} True if term is a quoted triple
 */
export function isQuotedTriple(term) {
  return term && (term.termType === 'Quad' || term.termType === 'QuotedTriple');
}

/**
 * Utility: Extract base triple from quoted triple
 * @param {Object} quotedTriple - Quoted triple
 * @returns {Object} Base triple (subject, predicate, object)
 */
export function extractBaseTriple(quotedTriple) {
  if (!isQuotedTriple(quotedTriple)) {
    throw new Error('Term is not a quoted triple');
  }

  if (quotedTriple.termType === 'QuotedTriple') {
    return {
      subject: quotedTriple.subject,
      predicate: quotedTriple.predicate,
      object: quotedTriple.object,
    };
  }

  return {
    subject: quotedTriple.subject,
    predicate: quotedTriple.predicate,
    object: quotedTriple.object,
  };
}

/**
 * Export singleton factory
 */
export default factory;
