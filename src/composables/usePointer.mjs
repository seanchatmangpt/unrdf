/**
 * @fileoverview usePointer composable - Clownface graph traversal
 * 
 * This composable provides Clownface-based graph traversal capabilities.
 * It enforces the "One Pointer Rule" - Clownface is the only traversal method.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from "../engines/RdfEngine.mjs";

/**
 * Create a pointer composable for graph traversal
 * 
 * @param {Store} store - N3.Store to traverse
 * @param {Object} [options] - Pointer options
 * @param {string} [options.baseIRI] - Base IRI for operations
 * @returns {Object} Pointer composable interface
 * 
 * @example
 * const pointer = usePointer(store);
 * 
 * // Traverse the graph
 * const name = pointer.node("ex:person").out("foaf:name").value;
 * const friends = pointer.node("ex:person").out("foaf:knows").toArray();
 */
export function usePointer(store, options = {}) {
  if (!store || typeof store.getQuads !== "function") {
    throw new Error("[usePointer] Store is required");
  }

  const { baseIRI = "http://example.org/" } = options || {};
  const engine = new RdfEngine({ baseIRI });

  // Get the Clownface instance
  const clownface = engine.getClownface(store);

  return {
    /**
     * Get a pointer to a specific node
     * @param {string|Term} node - Node identifier or term
     * @returns {Clownface} Clownface pointer to the node
     * 
     * @example
     * const personPointer = pointer.node("ex:person");
     * const bnodePointer = pointer.node(blankNode("person1"));
     */
    node(node) {
      if (typeof node === "string") {
        // Handle CURIEs and IRIs
        if (node.includes(":")) {
          // Assume it's a CURIE, expand it
          const expanded = node.startsWith("http") ? node : `${baseIRI}${node}`;
          return clownface.namedNode(expanded);
        } else {
          // Assume it's a local identifier
          return clownface.namedNode(`${baseIRI}${node}`);
        }
      }
      
      // Handle RDF terms
      if (node && node.termType) {
        switch (node.termType) {
          case "NamedNode":
            return clownface.namedNode(node.value);
          case "BlankNode":
            return clownface.blankNode(node.value);
          case "Literal":
            return clownface.literal(node.value, node.datatype, node.language);
          default:
            throw new Error(`[usePointer] Unsupported term type: ${node.termType}`);
        }
      }
      
      throw new Error("[usePointer] Invalid node identifier");
    },

    /**
     * Get a pointer to a blank node
     * @param {string} [id] - Blank node identifier
     * @returns {Clownface} Clownface pointer to the blank node
     * 
     * @example
     * const bnodePointer = pointer.blankNode("person1");
     */
    blankNode(id) {
      return clownface.blankNode(id);
    },

    /**
     * Get a pointer to a named node
     * @param {string} iri - IRI string
     * @returns {Clownface} Clownface pointer to the named node
     * 
     * @example
     * const namedPointer = pointer.namedNode("http://example.org/person");
     */
    namedNode(iri) {
      return clownface.namedNode(iri);
    },

    /**
     * Get a pointer to a literal
     * @param {string} value - Literal value
     * @param {string} [datatype] - Datatype IRI
     * @param {string} [language] - Language tag
     * @returns {Clownface} Clownface pointer to the literal
     * 
     * @example
     * const literalPointer = pointer.literal("John Doe");
     * const typedLiteral = pointer.literal("30", "http://www.w3.org/2001/XMLSchema#integer");
     */
    literal(value, datatype, language) {
      return clownface.literal(value, datatype, language);
    },

    /**
     * Get all nodes in the graph
     * @returns {Clownface} Clownface pointer to all nodes
     * 
     * @example
     * const allNodes = pointer.all();
     */
    all() {
      return clownface;
    },

    /**
     * Get all subjects in the graph
     * @returns {Clownface} Clownface pointer to all subjects
     * 
     * @example
     * const subjects = pointer.subjects();
     */
    subjects() {
      return clownface.subjects();
    },

    /**
     * Get all predicates in the graph
     * @returns {Clownface} Clownface pointer to all predicates
     * 
     * @example
     * const predicates = pointer.predicates();
     */
    predicates() {
      return clownface.predicates();
    },

    /**
     * Get all objects in the graph
     * @returns {Clownface} Clownface pointer to all objects
     * 
     * @example
     * const objects = pointer.objects();
     */
    objects() {
      return clownface.objects();
    },

    /**
     * Get all nodes of a specific type
     * @param {string|Term} type - RDF type to filter by
     * @returns {Clownface} Clownface pointer to nodes of the type
     * 
     * @example
     * const persons = pointer.ofType("foaf:Person");
     * const documents = pointer.ofType("http://example.org/Document");
     */
    ofType(type) {
      const typeTerm = typeof type === "string" ? this.namedNode(type) : type;
      return clownface.has(clownface.namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"), typeTerm);
    },

    /**
     * Get all nodes with a specific property
     * @param {string|Term} property - Property to filter by
     * @returns {Clownface} Clownface pointer to nodes with the property
     * 
     * @example
     * const namedNodes = pointer.withProperty("foaf:name");
     */
    withProperty(property) {
      const propertyTerm = typeof property === "string" ? this.namedNode(property) : property;
      return clownface.has(propertyTerm);
    },

    /**
     * Get all nodes with a specific property value
     * @param {string|Term} property - Property to filter by
     * @param {string|Term} value - Value to filter by
     * @returns {Clownface} Clownface pointer to nodes with the property value
     * 
     * @example
     * const johnNodes = pointer.withValue("foaf:name", "John Doe");
     */
    withValue(property, value) {
      const propertyTerm = typeof property === "string" ? this.namedNode(property) : property;
      const valueTerm = typeof value === "string" ? this.literal(value) : value;
      return clownface.has(propertyTerm, valueTerm);
    },

    /**
     * Get the underlying Clownface instance
     * @returns {Clownface} Raw Clownface instance
     * 
     * @example
     * const cf = pointer.getClownface();
     * const result = cf.namedNode("ex:person").out("foaf:name").value;
     */
    getClownface() {
      return clownface;
    },

    /**
     * Get the underlying store
     * @returns {Store} N3.Store instance
     * 
     * @example
     * const store = pointer.getStore();
     */
    getStore() {
      return store;
    },

    /**
     * Get the underlying engine
     * @returns {RdfEngine} RdfEngine instance
     * 
     * @example
     * const engine = pointer.getEngine();
     */
    getEngine() {
      return engine;
    },

    /**
     * Get the base IRI
     * @returns {string} Base IRI
     * 
     * @example
     * const baseIRI = pointer.getBaseIRI();
     */
    getBaseIRI() {
      return baseIRI;
    },

    /**
     * Create a new pointer with a different base IRI
     * @param {string} newBaseIRI - New base IRI
     * @returns {Object} New pointer instance
     * 
     * @example
     * const newPointer = pointer.withBaseIRI("http://other.org/");
     */
    withBaseIRI(newBaseIRI) {
      return usePointer(store, { ...options, baseIRI: newBaseIRI });
    },

    /**
     * Get statistics about the graph
     * @returns {Object} Graph statistics
     * 
     * @example
     * const stats = pointer.getStats();
     * console.log(`Quads: ${stats.quads}, Subjects: ${stats.subjects}`);
     */
    getStats() {
      return engine.getStats(store);
    },

    /**
     * Check if the graph is empty
     * @returns {boolean} True if graph is empty
     * 
     * @example
     * const isEmpty = pointer.isEmpty();
     */
    isEmpty() {
      return store.size === 0;
    },

    /**
     * Get the size of the graph
     * @returns {number} Number of quads
     * 
     * @example
     * const size = pointer.size();
     */
    size() {
      return store.size;
    }
  };
}