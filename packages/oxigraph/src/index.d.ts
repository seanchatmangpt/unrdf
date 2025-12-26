/**
 * @unrdf/oxigraph - RDF Store Type Definitions
 *
 * Oxigraph-backed RDF triple store with SPARQL query support.
 * Provides high-performance in-memory graph database for semantic web applications.
 *
 * @packageDocumentation
 */

// ============================================================================
// RDF Term Types
// ============================================================================

/**
 * RDF Named Node (IRI/URI)
 */
export interface NamedNode {
  /** Term type identifier */
  readonly termType: 'NamedNode';
  /** IRI value */
  readonly value: string;
  /** Equals method for term comparison */
  equals(other: Term): boolean;
}

/**
 * RDF Blank Node (anonymous resource)
 */
export interface BlankNode {
  /** Term type identifier */
  readonly termType: 'BlankNode';
  /** Blank node identifier */
  readonly value: string;
  /** Equals method for term comparison */
  equals(other: Term): boolean;
}

/**
 * RDF Literal (data value with optional language/datatype)
 */
export interface Literal {
  /** Term type identifier */
  readonly termType: 'Literal';
  /** Literal value as string */
  readonly value: string;
  /** Language tag (e.g., 'en', 'fr') */
  readonly language: string;
  /** Datatype IRI (e.g., xsd:string, xsd:integer) */
  readonly datatype: NamedNode;
  /** Equals method for term comparison */
  equals(other: Term): boolean;
}

/**
 * RDF Default Graph
 */
export interface DefaultGraph {
  /** Term type identifier */
  readonly termType: 'DefaultGraph';
  /** Value is empty string for default graph */
  readonly value: '';
  /** Equals method for term comparison */
  equals(other: Term): boolean;
}

/**
 * RDF Term (union type)
 */
export type Term = NamedNode | BlankNode | Literal | DefaultGraph;

/**
 * RDF Quad/Triple Subject
 */
export type Subject = NamedNode | BlankNode;

/**
 * RDF Quad/Triple Predicate
 */
export type Predicate = NamedNode;

/**
 * RDF Quad/Triple Object
 */
export type RDFObject = NamedNode | BlankNode | Literal;

/**
 * RDF Graph identifier
 */
export type Graph = NamedNode | BlankNode | DefaultGraph;

/**
 * RDF Quad (subject-predicate-object-graph)
 */
export interface Quad {
  /** Quad subject */
  readonly subject: Subject;
  /** Quad predicate */
  readonly predicate: Predicate;
  /** Quad object */
  readonly object: RDFObject;
  /** Quad graph (named graph or default) */
  readonly graph: Graph;
  /** Equals method for quad comparison */
  equals(other: Quad): boolean;
}

// ============================================================================
// Data Factory
// ============================================================================

/**
 * RDF Data Factory for creating RDF terms
 *
 * Provides factory methods compliant with RDF/JS specification for
 * creating named nodes, blank nodes, literals, and quads.
 */
export interface DataFactory {
  /**
   * Create a named node (IRI/URI)
   *
   * @param value - IRI as string (e.g., 'http://example.org/resource')
   * @returns Named node term
   *
   * @example
   * ```javascript
   * import { dataFactory } from '@unrdf/oxigraph';
   *
   * const person = dataFactory.namedNode('http://example.org/person/alice');
   * const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
   * ```
   */
  namedNode(value: string): NamedNode;

  /**
   * Create a blank node (anonymous resource)
   *
   * @param value - Optional blank node identifier
   * @returns Blank node term
   *
   * @example
   * ```javascript
   * const anonymous = dataFactory.blankNode(); // Auto-generated ID
   * const named = dataFactory.blankNode('b1'); // Specific ID
   * ```
   */
  blankNode(value?: string): BlankNode;

  /**
   * Create a literal value
   *
   * @param value - String value
   * @param languageOrDatatype - Optional language tag or datatype IRI
   * @returns Literal term
   *
   * @example
   * ```javascript
   * // Plain string
   * const name = dataFactory.literal('Alice');
   *
   * // With language tag
   * const greeting = dataFactory.literal('Hello', 'en');
   *
   * // With datatype
   * const age = dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'));
   * ```
   */
  literal(value: string, languageOrDatatype?: string | NamedNode): Literal;

  /**
   * Get the default graph identifier
   *
   * @returns Default graph term
   *
   * @example
   * ```javascript
   * const defaultGraph = dataFactory.defaultGraph();
   * ```
   */
  defaultGraph(): DefaultGraph;

  /**
   * Create a quad (RDF statement)
   *
   * @param subject - Quad subject (named node or blank node)
   * @param predicate - Quad predicate (named node)
   * @param object - Quad object (any RDF term)
   * @param graph - Optional graph identifier (defaults to default graph)
   * @returns Quad object
   *
   * @example
   * ```javascript
   * const quad = dataFactory.quad(
   *   dataFactory.namedNode('http://example.org/alice'),
   *   dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
   *   dataFactory.literal('Alice')
   * );
   * ```
   */
  quad(
    subject: Subject,
    predicate: Predicate,
    object: RDFObject,
    graph?: Graph
  ): Quad;

  /**
   * Create a triple (quad in default graph)
   *
   * @param subject - Triple subject
   * @param predicate - Triple predicate
   * @param object - Triple object
   * @returns Quad in default graph
   *
   * @example
   * ```javascript
   * const triple = dataFactory.triple(
   *   dataFactory.namedNode('http://example.org/alice'),
   *   dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
   *   dataFactory.literal('Alice')
   * );
   * ```
   */
  triple(subject: Subject, predicate: Predicate, object: RDFObject): Quad;
}

/**
 * RDF data factory instance
 *
 * Use this to create RDF terms (named nodes, literals, etc.)
 *
 * @example
 * ```javascript
 * import { dataFactory } from '@unrdf/oxigraph';
 *
 * const { namedNode, literal, quad } = dataFactory;
 *
 * const alice = namedNode('http://example.org/alice');
 * const name = namedNode('http://xmlns.com/foaf/0.1/name');
 * const aliceName = literal('Alice');
 *
 * const statement = quad(alice, name, aliceName);
 * ```
 */
export const dataFactory: DataFactory;

// ============================================================================
// Store Interface
// ============================================================================

/**
 * SPARQL query result binding
 */
export type Binding = Record<string, Term>;

/**
 * SPARQL SELECT query results
 */
export interface QueryResults {
  /** Result bindings array */
  bindings: Binding[];
  /** Variable names */
  variables: string[];
}

/**
 * Oxigraph RDF Store
 *
 * High-performance in-memory RDF triple store with SPARQL 1.1 support.
 * Implements RDF/JS Dataset interface.
 */
export class OxigraphStore {
  /**
   * Create a new Oxigraph store
   *
   * @param quads - Optional initial quads to load
   *
   * @example
   * ```javascript
   * import { OxigraphStore, dataFactory } from '@unrdf/oxigraph';
   *
   * const store = new OxigraphStore([
   *   dataFactory.quad(
   *     dataFactory.namedNode('http://example.org/alice'),
   *     dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
   *     dataFactory.literal('Alice')
   *   )
   * ]);
   * ```
   */
  constructor(quads?: Quad[]);

  /**
   * Add a quad to the store
   *
   * @param quad - Quad to add
   * @returns This store (for chaining)
   *
   * @example
   * ```javascript
   * store.add(dataFactory.quad(
   *   dataFactory.namedNode('http://example.org/bob'),
   *   dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
   *   dataFactory.literal('Bob')
   * ));
   * ```
   */
  add(quad: Quad): this;

  /**
   * Delete a quad from the store
   *
   * @param quad - Quad to remove
   * @returns This store (for chaining)
   *
   * @example
   * ```javascript
   * store.delete(quadToRemove);
   * ```
   */
  delete(quad: Quad): this;

  /**
   * Check if store contains a quad
   *
   * @param quad - Quad to check
   * @returns True if quad exists in store
   *
   * @example
   * ```javascript
   * if (store.has(quad)) {
   *   console.log('Quad exists');
   * }
   * ```
   */
  has(quad: Quad): boolean;

  /**
   * Match quads by pattern (quad template matching)
   *
   * Use null/undefined to match any value for a component.
   *
   * @param subject - Subject to match (null = any)
   * @param predicate - Predicate to match (null = any)
   * @param object - Object to match (null = any)
   * @param graph - Graph to match (null = any)
   * @returns Array of matching quads
   *
   * @example
   * ```javascript
   * // Find all quads about Alice
   * const aliceQuads = store.match(
   *   dataFactory.namedNode('http://example.org/alice'),
   *   null,
   *   null
   * );
   *
   * // Find all names
   * const names = store.match(
   *   null,
   *   dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
   *   null
   * );
   * ```
   */
  match(
    subject?: Subject | null,
    predicate?: Predicate | null,
    object?: RDFObject | null,
    graph?: Graph | null
  ): Quad[];

  /**
   * Execute a SPARQL query
   *
   * @param query - SPARQL query string
   * @returns Query results (bindings for SELECT, boolean for ASK)
   *
   * @example
   * ```javascript
   * // SELECT query
   * const results = store.query(`
   *   PREFIX foaf: <http://xmlns.com/foaf/0.1/>
   *   SELECT ?name WHERE {
   *     ?person foaf:name ?name .
   *   }
   * `);
   * console.log('Names:', results.bindings.map(b => b.name.value));
   *
   * // ASK query
   * const exists = store.query(`
   *   PREFIX foaf: <http://xmlns.com/foaf/0.1/>
   *   ASK {
   *     ?person foaf:name "Alice" .
   *   }
   * `);
   * console.log('Alice exists:', exists);
   * ```
   */
  query(query: string): QueryResults | boolean;

  /**
   * Get store size (number of quads)
   *
   * @returns Number of quads in store
   *
   * @example
   * ```javascript
   * console.log('Store contains', store.size, 'quads');
   * ```
   */
  get size(): number;

  /**
   * Load quads from Turtle string
   *
   * @param turtle - Turtle format string
   * @param baseIRI - Base IRI for resolving relative IRIs
   * @returns This store (for chaining)
   *
   * @example
   * ```javascript
   * store.loadTurtle(`
   *   @prefix foaf: <http://xmlns.com/foaf/0.1/> .
   *   <http://example.org/alice> foaf:name "Alice" .
   * `, 'http://example.org/');
   * ```
   */
  loadTurtle(turtle: string, baseIRI?: string): this;

  /**
   * Serialize store to Turtle string
   *
   * @returns Turtle format string
   *
   * @example
   * ```javascript
   * const turtle = store.toTurtle();
   * console.log(turtle);
   * ```
   */
  toTurtle(): string;
}

// ============================================================================
// Store Factory Function
// ============================================================================

/**
 * Create a new Oxigraph RDF store
 *
 * This is the primary entry point for creating an RDF store.
 * Use this instead of calling `new OxigraphStore()` directly.
 *
 * @param quads - Optional initial quads to load into the store
 * @returns New Oxigraph store instance
 *
 * @example
 * ```javascript
 * import { createStore, dataFactory } from '@unrdf/oxigraph';
 *
 * // Empty store
 * const store = createStore();
 *
 * // Store with initial data
 * const store = createStore([
 *   dataFactory.quad(
 *     dataFactory.namedNode('http://example.org/alice'),
 *     dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
 *     dataFactory.namedNode('http://xmlns.com/foaf/0.1/Person')
 *   )
 * ]);
 *
 * // Add more quads
 * store.add(dataFactory.quad(
 *   dataFactory.namedNode('http://example.org/alice'),
 *   dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
 *   dataFactory.literal('Alice')
 * ));
 *
 * // Query
 * const results = store.query(`
 *   PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *   SELECT ?name WHERE {
 *     ?person a foaf:Person ;
 *             foaf:name ?name .
 *   }
 * `);
 * ```
 */
export function createStore(quads?: Quad[]): OxigraphStore;

// ============================================================================
// Default Export
// ============================================================================

export default {
  createStore,
  dataFactory,
  OxigraphStore,
};
