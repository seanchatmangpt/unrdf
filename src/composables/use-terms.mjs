/**
 * @fileoverview useTerms composable - RDF term creation with context and configuration
 * 
 * This composable provides convenient RDF term creation with base IRI resolution,
 * default datatypes, and type conversion. It wraps N3 DataFactory with additional
 * functionality while maintaining compatibility with the core RDF data model.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { DataFactory } from "n3";
import { useStoreContext } from "../context/index.mjs";

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
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const terms = useTerms({ baseIRI: "https://example.org/" });
 * 
 *   // Create named nodes with base IRI resolution
 *   const subject = terms.iri("person"); // → "https://example.org/person"
 * 
 *   // Create literals with type conversion
 *   const name = terms.lit("John Doe");
 *   const age = terms.lit(30, "http://www.w3.org/2001/XMLSchema#integer");
 * 
 *   // Create blank nodes
 *   const bnode = terms.bnode("person1");
 * 
 *   // Create quads
 *   const statement = terms.quad(subject, terms.iri("name"), name);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useTerms(options = {}) {
  // Get the store context
  const storeContext = useStoreContext();

  const {
    baseIRI = "http://example.org/",
    defaultDatatype = "http://www.w3.org/2001/XMLSchema#string"
  } = options || {};

  return {
    /**
     * Create a named node (IRI)
     * @param {string} iri - The IRI string
     * @returns {NamedNode} Named node term
     * 
     * @throws {TypeError} If iri is not a string
     */
    iri(iri) {
      if (typeof iri !== "string") {
        throw new TypeError("[useTerms] IRI must be a string");
      }
      
      // Handle relative IRIs
      if (!iri.startsWith("http://") && !iri.startsWith("https://") && !iri.startsWith("urn:")) {
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
     * 
     * @throws {TypeError} If value is null or undefined
     */
    lit(value, datatype, language) {
      if (value === null || value === undefined) {
        throw new TypeError("[useTerms] Literal value cannot be null or undefined");
      }
      
      // Convert to string if needed
      const stringValue = String(value);
      
      // Handle language tags
      if (language) {
        return literal(stringValue, language);
      }
      
      // Handle datatypes - check for null/undefined explicitly
      if (datatype !== null && datatype !== undefined) {
        return literal(stringValue, namedNode(datatype));
      }
      
      // Use default datatype
      return literal(stringValue, namedNode(defaultDatatype));
    },

    /**
     * Create a blank node
     * @param {string} [id] - Optional blank node ID
     * @returns {BlankNode} Blank node term
     * 
     * @throws {TypeError} If id is provided but not a string
     */
    bnode(id) {
      if (id !== undefined && id !== null && typeof id !== "string") {
        throw new TypeError("[useTerms] Blank node ID must be a string");
      }
      
      // Allow null/undefined to generate automatic ID
      return blankNode(id || undefined);
    },

    /**
     * Create a quad (statement)
     * @param {Term} subject - The subject term
     * @param {Term} predicate - The predicate term
     * @param {Term} object - The object term
     * @param {Term} [graph] - The graph term (optional)
     * @returns {Quad} Quad term
     * 
     * @throws {TypeError} If subject, predicate, or object are missing
     */
    quad(subject, predicate, object, graph) {
      if (!subject || !predicate || !object) {
        throw new TypeError("[useTerms] Subject, predicate, and object are required");
      }
      
      // Handle null/undefined graph as null (not defaultGraph)
      return quad(subject, predicate, object, graph === null || graph === undefined ? null : graph);
    },

    /**
     * Create a named node for graph (alias for iri)
     * @param {string} iri - The IRI string
     * @returns {NamedNode} Named node term
     */
    graph(iri) {
      return this.iri(iri);
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
    },

    /**
     * Check if a term is a NamedNode
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a NamedNode
     */
    isNamedNode(term) {
      return term && typeof term === 'object' && term.termType === 'NamedNode';
    },

    /**
     * Check if a term is a Literal
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a Literal
     */
    isLiteral(term) {
      return term && typeof term === 'object' && term.termType === 'Literal';
    },

    /**
     * Check if a term is a BlankNode
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a BlankNode
     */
    isBlankNode(term) {
      return term && typeof term === 'object' && term.termType === 'BlankNode';
    },

    /**
     * Check if a term is a DefaultGraph
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a DefaultGraph
     */
    isDefaultGraph(term) {
      return term && typeof term === 'object' && term.termType === 'DefaultGraph';
    },

    /**
     * Check if a term is a valid RDF term
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a valid RDF term
     */
    isTerm(term) {
      if (!term || typeof term !== 'object') {
        return false;
      }
      return this.isNamedNode(term) || 
             this.isLiteral(term) || 
             this.isBlankNode(term) || 
             this.isDefaultGraph(term);
    },

    /**
     * Get the term type of a term
     * @param {any} term - The term to check
     * @returns {string|null} The term type or null if not a term
     */
    getTermType(term) {
      if (!term || typeof term !== 'object') {
        return null;
      }
      return term.termType || null;
    },

  };
}