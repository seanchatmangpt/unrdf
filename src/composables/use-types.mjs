/**
 * @fileoverview useTypes composable - RDF type checking and validation with @rdfjs/types
 * 
 * This composable provides comprehensive RDF type checking and validation
 * using the official @rdfjs/types definitions for better type safety.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { useStoreContext } from "../context/index.mjs";

/**
 * Create a types composable for RDF type checking and validation
 * 
 * @param {Object} [options] - Types options
 * @param {boolean} [options.strict=true] - Enable strict type checking
 * @returns {Object} Types interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const types = useTypes();
 * 
 *   // Check term types
 *   const isNamedNode = types.isNamedNode(term);
 *   const isLiteral = types.isLiteral(term);
 *   const isBlankNode = types.isBlankNode(term);
 * 
 *   // Validate terms
 *   const validation = types.validateTerm(term);
 *   if (!validation.valid) {
 *     console.log("Invalid term:", validation.errors);
 *   }
 * 
 *   // Get term type information
 *   const typeInfo = types.getTypeInfo(term);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useTypes(options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  const { strict = true } = options;

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * Check if a term is a NamedNode
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a NamedNode
     * 
     * @example
     * const isNamed = types.isNamedNode(namedNode('http://example.org/'));
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
     * const isLiteral = types.isLiteral(literal('Hello'));
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
     * const isBlank = types.isBlankNode(blankNode());
     */
    isBlankNode(term) {
      return term && typeof term === 'object' && term.termType === 'BlankNode';
    },

    /**
     * Check if a term is a Quad
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a Quad
     * 
     * @example
     * const isQuad = types.isQuad(quad(subject, predicate, object));
     */
    isQuad(term) {
      return term && typeof term === 'object' && term.termType === 'Quad';
    },

    /**
     * Check if a term is a DefaultGraph
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a DefaultGraph
     * 
     * @example
     * const isDefault = types.isDefaultGraph(defaultGraph());
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
     * const isVariable = types.isVariable(variable('x'));
     */
    isVariable(term) {
      return term && typeof term === 'object' && term.termType === 'Variable';
    },

    /**
     * Check if a term is a Term (any RDF term)
     * @param {any} term - The term to check
     * @returns {boolean} True if the term is a valid RDF term
     * 
     * @example
     * const isTerm = types.isTerm(someTerm);
     */
    isTerm(term) {
      return this.isNamedNode(term) || 
             this.isLiteral(term) || 
             this.isBlankNode(term) || 
             this.isDefaultGraph(term) || 
             this.isVariable(term);
    },

    /**
     * Get the term type of a term
     * @param {any} term - The term to check
     * @returns {string|null} The term type or null if not a term
     * 
     * @example
     * const type = types.getTermType(namedNode('http://example.org/'));
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
     * const validation = types.validateTerm(term);
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
     * const info = types.getTypeInfo(term);
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
     * const equal = types.equals(term1, term2);
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
     * const types = types.getValidTermTypes();
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
    },

    /**
     * Create a type-safe term factory
     * @returns {Object} Type-safe term factory
     * 
     * @example
     * const factory = types.createFactory();
     * const node = factory.namedNode('http://example.org/');
     */
    createFactory() {
      const { DataFactory } = await import('n3');
      
      return {
        namedNode: (iri) => {
          const term = DataFactory.namedNode(iri);
          const validation = this.validateTerm(term);
          if (!validation.valid && strict) {
            throw new Error(`Invalid NamedNode: ${validation.errors.join(', ')}`);
          }
          return term;
        },

        literal: (value, datatype, language) => {
          const term = DataFactory.literal(value, datatype, language);
          const validation = this.validateTerm(term);
          if (!validation.valid && strict) {
            throw new Error(`Invalid Literal: ${validation.errors.join(', ')}`);
          }
          return term;
        },

        blankNode: (id) => {
          const term = DataFactory.blankNode(id);
          const validation = this.validateTerm(term);
          if (!validation.valid && strict) {
            throw new Error(`Invalid BlankNode: ${validation.errors.join(', ')}`);
          }
          return term;
        },

        defaultGraph: () => {
          const term = DataFactory.defaultGraph();
          const validation = this.validateTerm(term);
          if (!validation.valid && strict) {
            throw new Error(`Invalid DefaultGraph: ${validation.errors.join(', ')}`);
          }
          return term;
        },

        variable: (name) => {
          const term = DataFactory.variable(name);
          const validation = this.validateTerm(term);
          if (!validation.valid && strict) {
            throw new Error(`Invalid Variable: ${validation.errors.join(', ')}`);
          }
          return term;
        },

        quad: (subject, predicate, object, graph) => {
          const term = DataFactory.quad(subject, predicate, object, graph);
          const validation = this.validateTerm(term);
          if (!validation.valid && strict) {
            throw new Error(`Invalid Quad: ${validation.errors.join(', ')}`);
          }
          return term;
        }
      };
    },

    /**
     * Get statistics about term types in a store
     * @param {Store} [store] - Store to analyze (uses context store if not provided)
     * @returns {Object} Term type statistics
     * 
     * @example
     * const stats = types.getStoreStats();
     * console.log(`NamedNodes: ${stats.NamedNode}, Literals: ${stats.Literal}`);
     */
    getStoreStats(store) {
      const targetStore = store || storeContext.store;
      const stats = {
        NamedNode: 0,
        Literal: 0,
        BlankNode: 0,
        DefaultGraph: 0,
        Variable: 0,
        Quad: 0,
        total: 0
      };

      for (const quad of targetStore) {
        stats.total++;
        
        const subjType = this.getTermType(quad.subject);
        const predType = this.getTermType(quad.predicate);
        const objType = this.getTermType(quad.object);
        const graphType = this.getTermType(quad.graph);

        if (subjType) stats[subjType]++;
        if (predType) stats[predType]++;
        if (objType) stats[objType]++;
        if (graphType) stats[graphType]++;
      }

      return stats;
    }
  };
}
