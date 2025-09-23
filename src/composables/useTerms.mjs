/**
 * @fileoverview useTerms composable - RDF term creation and manipulation
 * 
 * This composable provides the foundation for creating RDF terms.
 * It enforces the "One Terms Rule" - N3 DataFactory is the only term creation method.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { DataFactory } from "n3";

const { namedNode, literal, blankNode, quad, defaultGraph } = DataFactory;

/**
 * Create a terms composable for RDF term creation
 * 
 * @param {Object} [options] - Terms options
 * @param {string} [options.baseIRI] - Base IRI for relative IRIs
 * @param {string} [options.defaultDatatype] - Default datatype for literals
 * @returns {Object} Terms creation interface
 * 
 * @example
 * const terms = useTerms();
 * 
 * // Create named nodes
 * const subject = terms.iri("http://example.org/person");
 * 
 * // Create literals
 * const name = terms.lit("John Doe");
 * const age = terms.lit(30, "http://www.w3.org/2001/XMLSchema#integer");
 * 
 * // Create blank nodes
 * const bnode = terms.bnode("person1");
 * 
 * // Create quads
 * const statement = terms.quad(subject, terms.iri("http://example.org/name"), name);
 */
export function useTerms(options = {}) {
  const {
    baseIRI = "http://example.org/",
    defaultDatatype = "http://www.w3.org/2001/XMLSchema#string"
  } = options;

  return {
    /**
     * Create a named node (IRI)
     * @param {string} iri - The IRI string
     * @returns {NamedNode} Named node term
     */
    iri(iri) {
      if (typeof iri !== "string") {
        throw new Error("[useTerms] IRI must be a string");
      }
      
      // Handle relative IRIs
      if (iri.startsWith("#") || iri.startsWith("/")) {
        iri = baseIRI + iri;
      }
      
      return namedNode(iri);
    },

    /**
     * Create a literal
     * @param {string|number|boolean} value - The literal value
     * @param {string} [datatype] - The datatype IRI
     * @param {string} [language] - The language tag
     * @returns {Literal} Literal term
     */
    lit(value, datatype, language) {
      if (value === null || value === undefined) {
        throw new Error("[useTerms] Literal value cannot be null or undefined");
      }
      
      // Convert to string if needed
      const stringValue = String(value);
      
      // Handle language tags
      if (language) {
        return literal(stringValue, language);
      }
      
      // Handle datatypes
      if (datatype) {
        return literal(stringValue, datatype);
      }
      
      // Use default datatype
      return literal(stringValue, defaultDatatype);
    },

    /**
     * Create a blank node
     * @param {string} [id] - Optional blank node ID
     * @returns {BlankNode} Blank node term
     */
    bnode(id) {
      if (id !== undefined && typeof id !== "string") {
        throw new Error("[useTerms] Blank node ID must be a string");
      }
      
      return blankNode(id);
    },

    /**
     * Create a quad (statement)
     * @param {Term} subject - The subject term
     * @param {Term} predicate - The predicate term
     * @param {Term} object - The object term
     * @param {Term} [graph] - The graph term (optional)
     * @returns {Quad} Quad term
     */
    quad(subject, predicate, object, graph) {
      if (!subject || !predicate || !object) {
        throw new Error("[useTerms] Subject, predicate, and object are required");
      }
      
      return quad(subject, predicate, object, graph || defaultGraph());
    },

    /**
     * Create a default graph term
     * @returns {DefaultGraph} Default graph term
     */
    defaultGraph() {
      return defaultGraph();
    },

    /**
     * Get the base IRI
     * @returns {string} Base IRI
     */
    getBaseIRI() {
      return baseIRI;
    },

    /**
     * Get the default datatype
     * @returns {string} Default datatype
     */
    getDefaultDatatype() {
      return defaultDatatype;
    }
  };
}