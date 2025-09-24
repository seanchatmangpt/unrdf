/**
 * @fileoverview useTerms composable - RDF term creation and manipulation with context
 * 
 * This composable provides the foundation for creating RDF terms.
 * It enforces the "One Terms Rule" - N3 DataFactory is the only term creation method.
 * Now uses unctx for global term management.
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
 *   const terms = useTerms();
 * 
 *   // Create named nodes
 *   const subject = terms.iri("http://example.org/person");
 * 
 *   // Create literals
 *   const name = terms.lit("John Doe");
 *   const age = terms.lit(30, "http://www.w3.org/2001/XMLSchema#integer");
 * 
 *   // Create blank nodes
 *   const bnode = terms.bnode("person1");
 * 
 *   // Create quads
 *   const statement = terms.quad(subject, terms.iri("http://example.org/name"), name);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useTerms(options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  const {
    baseIRI = engine.baseIRI || "http://example.org/",
    defaultDatatype = "http://www.w3.org/2001/XMLSchema#string"
  } = options || {};

  return {
    /**
     * Create a named node (IRI)
     * @param {string} iri - The IRI string
     * @returns {NamedNode} Named node term
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
     */
    bnode(id) {
      if (id !== undefined && id !== null && typeof id !== "string") {
        throw new Error("[useTerms] Blank node ID must be a string");
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
     */
    quad(subject, predicate, object, graph) {
      if (!subject || !predicate || !object) {
        throw new Error("[useTerms] Subject, predicate, and object are required");
      }
      
      // Handle null/undefined graph as null (not defaultGraph)
      return quad(subject, predicate, object, graph === null || graph === undefined ? null : graph);
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
     * 
     * @example
     * const isNamed = terms.isNamedNode(term);
     */
    isNamedNode(term) {
      return term && typeof term === 'object' && term.termType === 'NamedNode';
    },

    /**
     * Check if a term is a Literal
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a Literal
     * 
     * @example
     * const isLiteral = terms.isLiteral(term);
     */
    isLiteral(term) {
      return term && typeof term === 'object' && term.termType === 'Literal';
    },

    /**
     * Check if a term is a BlankNode
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a BlankNode
     * 
     * @example
     * const isBlank = terms.isBlankNode(term);
     */
    isBlankNode(term) {
      return term && typeof term === 'object' && term.termType === 'BlankNode';
    },

    /**
     * Check if a term is a DefaultGraph
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a DefaultGraph
     * 
     * @example
     * const isDefault = terms.isDefaultGraph(term);
     */
    isDefaultGraph(term) {
      return term && typeof term === 'object' && term.termType === 'DefaultGraph';
    },

    /**
     * Check if a term is a Variable
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a Variable
     * 
     * @example
     * const isVariable = terms.isVariable(term);
     */
    isVariable(term) {
      return term && typeof term === 'object' && term.termType === 'Variable';
    },

    /**
     * Check if a term is a Quad
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a Quad
     * 
     * @example
     * const isQuad = terms.isQuad(term);
     */
    isQuad(term) {
      return term && typeof term === 'object' && term.termType === 'Quad';
    },

    /**
     * Check if a term is a valid RDF term
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a valid RDF term
     * 
     * @example
     * const isTerm = terms.isTerm(term);
     */
    isTerm(term) {
      return this.isNamedNode(term) || 
             this.isLiteral(term) || 
             this.isBlankNode(term) || 
             this.isDefaultGraph(term) || 
             this.isVariable(term) ||
             this.isQuad(term);
    },

    /**
     * Get the term type of a term
     * @param {any} term - The term to check
     * @returns {string|null} The term type or null if not a term
     * 
     * @example
     * const type = terms.getTermType(term);
     * console.log(type); // "NamedNode"
     */
    getTermType(term) {
      if (!term || typeof term !== 'object') {
        return null;
      }
      return term.termType || null;
    },

    /**
     * Validate a term for correctness
     * @param {any} term - The term to validate
     * @returns {Object} Validation result
     * 
     * @example
     * const validation = terms.validateTerm(term);
     * if (!validation.valid) {
     *   console.log("Errors:", validation.errors);
     * }
     */
    validateTerm(term) {
      const errors = [];

      if (!term) {
        errors.push("Term is null or undefined");
        return { valid: false, errors };
      }

      if (typeof term !== 'object') {
        errors.push("Term must be an object");
        return { valid: false, errors };
      }

      if (!term.termType) {
        errors.push("Term must have a termType property");
        return { valid: false, errors };
      }

      if (!this.isTerm(term)) {
        errors.push(`Invalid termType: ${term.termType}`);
        return { valid: false, errors };
      }

      if (!term.value && term.termType !== 'DefaultGraph') {
        errors.push("Term must have a value property");
        return { valid: false, errors };
      }

      // Type-specific validation
      if (this.isNamedNode(term)) {
        if (typeof term.value !== 'string') {
          errors.push("NamedNode value must be a string");
        } else if (!term.value.includes(':')) {
          errors.push("NamedNode value should be a valid IRI");
        }
      }

      if (this.isLiteral(term)) {
        if (typeof term.value !== 'string') {
          errors.push("Literal value must be a string");
        }
        if (term.datatype && typeof term.datatype !== 'object') {
          errors.push("Literal datatype must be an object");
        }
        if (term.language && typeof term.language !== 'string') {
          errors.push("Literal language must be a string");
        }
      }

      if (this.isBlankNode(term)) {
        if (typeof term.value !== 'string') {
          errors.push("BlankNode value must be a string");
        }
      }

      return {
        valid: errors.length === 0,
        errors,
        termType: term.termType
      };
    },

    /**
     * Get detailed type information about a term
     * @param {any} term - The term to analyze
     * @returns {Object} Type information
     * 
     * @example
     * const info = terms.getTypeInfo(term);
     * console.log(`Type: ${info.type}, Value: ${info.value}`);
     */
    getTypeInfo(term) {
      const validation = this.validateTerm(term);
      
      if (!validation.valid) {
        return {
          type: 'Invalid',
          value: null,
          valid: false,
          errors: validation.errors
        };
      }

      const info = {
        type: term.termType,
        value: term.value,
        valid: true,
        errors: []
      };

      // Add type-specific information
      if (this.isNamedNode(term)) {
        info.isIRI = true;
        info.scheme = term.value.split(':')[0];
        info.localName = term.value.split('/').pop();
      }

      if (this.isLiteral(term)) {
        info.hasDatatype = !!term.datatype;
        info.hasLanguage = !!term.language;
        info.datatype = term.datatype?.value;
        info.language = term.language;
      }

      if (this.isBlankNode(term)) {
        info.isBlank = true;
        info.identifier = term.value;
      }

      return info;
    },

    /**
     * Check if two terms are equal
     * @param {any} term1 - First term
     * @param {any} term2 - Second term
     * @returns {boolean} True if terms are equal
     * 
     * @example
     * const equal = terms.equals(term1, term2);
     */
    equals(term1, term2) {
      if (!this.isTerm(term1) || !this.isTerm(term2)) {
        return false;
      }

      if (term1.termType !== term2.termType) {
        return false;
      }

      if (term1.termType === 'DefaultGraph') {
        return true;
      }

      if (term1.value !== term2.value) {
        return false;
      }

      // Additional checks for literals
      if (this.isLiteral(term1)) {
        const dt1 = term1.datatype?.value;
        const dt2 = term2.datatype?.value;
        if (dt1 !== dt2) {
          return false;
        }

        const lang1 = term1.language;
        const lang2 = term2.language;
        if (lang1 !== lang2) {
          return false;
        }
      }

      return true;
    },

    /**
     * Get all valid term types
     * @returns {Array<string>} Array of valid term types
     * 
     * @example
     * const types = terms.getValidTermTypes();
     * console.log(types); // ["NamedNode", "Literal", "BlankNode", ...]
     */
    getValidTermTypes() {
      return [
        'NamedNode',
        'Literal', 
        'BlankNode',
        'DefaultGraph',
        'Variable',
        'Quad'
      ];
    }
  };
}