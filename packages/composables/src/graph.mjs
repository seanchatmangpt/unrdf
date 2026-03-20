/**
 * @file Vue 3 composables for RDF graph operations
 * @module @unrdf/composables/graph
 *
 * @description
 * Provides reactive composables for working with RDF graphs in Vue 3 applications.
 * Each composable uses the store context from the context module and exposes
 * reactive state via Vue's ref/computed/watch APIs.
 */

import { ref, computed, watch, shallowRef, triggerRef } from 'vue';
import { z } from 'zod';
import { useStoreContext } from './context/index.mjs';

/**
 * Schema for graph composable options
 */
const GraphOptionsSchema = z
  .object({
    autoSync: z.boolean().optional().default(true),
    defaultGraph: z.string().optional(),
  })
  .optional()
  .default({});

/**
 * Schema for SPARQL query input
 */
const SparqlQuerySchema = z.string().min(1, 'SPARQL query must not be empty');

/**
 * Schema for IRI input
 */
const IriSchema = z.string().min(1, 'IRI must not be empty');

/**
 * Composable that provides reactive graph operations.
 *
 * Wraps the store context with reactive Vue state so that components
 * can add, remove, and query triples while staying in sync with the graph.
 *
 * @param {Object} [options] - Configuration options
 * @param {boolean} [options.autoSync=true] - Automatically sync reactive state on mutations
 * @param {string} [options.defaultGraph] - Default named graph IRI
 * @returns {Object} Reactive graph operations
 * @returns {import('vue').Ref<number>} returns.tripleCount - Reactive triple count
 * @returns {Function} returns.addTriple - Add a triple to the graph
 * @returns {Function} returns.removeTriple - Remove a triple from the graph
 * @returns {Function} returns.clear - Clear all triples
 * @returns {Function} returns.getTriples - Get all triples as an array
 * @returns {Function} returns.query - Execute a SPARQL query
 * @returns {import('vue').Ref<boolean>} returns.loading - Whether a query is in progress
 * @returns {import('vue').Ref<Error|null>} returns.error - Last error, if any
 *
 * @example
 * const { addTriple, tripleCount, query } = useGraph();
 * addTriple('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o');
 * console.log(tripleCount.value); // 1
 */
export function useGraph(options) {
  const opts = GraphOptionsSchema.parse(options);
  const ctx = useStoreContext();

  const version = ref(0);
  const loading = ref(false);
  const error = ref(null);

  const tripleCount = computed(() => {
    // Access version to create reactive dependency
    // eslint-disable-next-line no-unused-expressions
    version.value;
    const stats = ctx.stats();
    return stats.quads;
  });

  /**
   * Increment the reactive version to trigger recomputation
   * @private
   */
  function _touch() {
    if (opts.autoSync) {
      version.value++;
    }
  }

  /**
   * Add a triple to the graph
   * @param {string} subject - Subject IRI
   * @param {string} predicate - Predicate IRI
   * @param {string} object - Object value (IRI or literal)
   * @param {string} [graph] - Optional named graph IRI
   * @returns {void}
   * @throws {TypeError} If subject, predicate, or object are not strings
   */
  function addTriple(subject, predicate, object, graph) {
    if (typeof subject !== 'string') {
      throw new TypeError('[useGraph] subject must be a string');
    }
    if (typeof predicate !== 'string') {
      throw new TypeError('[useGraph] predicate must be a string');
    }
    if (typeof object !== 'string') {
      throw new TypeError('[useGraph] object must be a string');
    }

    const s = ctx.namedNode(subject);
    const p = ctx.namedNode(predicate);
    const o = ctx.namedNode(object);
    const g = graph ? ctx.namedNode(graph) : opts.defaultGraph ? ctx.namedNode(opts.defaultGraph) : undefined;

    const q = ctx.quad(s, p, o, g);
    ctx.add(q);
    error.value = null;
    _touch();
  }

  /**
   * Remove a triple from the graph
   * @param {string} subject - Subject IRI
   * @param {string} predicate - Predicate IRI
   * @param {string} object - Object value
   * @param {string} [graph] - Optional named graph IRI
   * @returns {void}
   * @throws {TypeError} If subject, predicate, or object are not strings
   */
  function removeTriple(subject, predicate, object, graph) {
    if (typeof subject !== 'string') {
      throw new TypeError('[useGraph] subject must be a string');
    }
    if (typeof predicate !== 'string') {
      throw new TypeError('[useGraph] predicate must be a string');
    }
    if (typeof object !== 'string') {
      throw new TypeError('[useGraph] object must be a string');
    }

    const s = ctx.namedNode(subject);
    const p = ctx.namedNode(predicate);
    const o = ctx.namedNode(object);
    const g = graph ? ctx.namedNode(graph) : opts.defaultGraph ? ctx.namedNode(opts.defaultGraph) : undefined;

    const q = ctx.quad(s, p, o, g);
    ctx.remove(q);
    error.value = null;
    _touch();
  }

  /**
   * Clear all triples from the graph
   * @returns {void}
   */
  function clear() {
    ctx.clear();
    error.value = null;
    _touch();
  }

  /**
   * Get all triples from the store as an array
   * @returns {Array<Object>} Array of quads
   */
  function getTriples() {
    return Array.from(ctx.store.match());
  }

  /**
   * Execute a SPARQL query against the store
   * @param {string} sparql - SPARQL query string
   * @param {Object} [queryOptions] - Query options
   * @returns {Promise<Object>} Query result
   * @throws {Error} If query is invalid or execution fails
   */
  async function query(sparql, queryOptions = {}) {
    SparqlQuerySchema.parse(sparql);
    loading.value = true;
    error.value = null;

    try {
      const result = await ctx.query(sparql, queryOptions);
      return result;
    } catch (err) {
      error.value = err;
      throw err;
    } finally {
      loading.value = false;
    }
  }

  return {
    tripleCount,
    loading,
    error,
    addTriple,
    removeTriple,
    clear,
    getTriples,
    query,
  };
}

/**
 * Reactive triple that auto-syncs with the store.
 *
 * Creates a reactive representation of a single triple. When the triple's
 * properties change (via returned refs), it automatically updates in the store.
 *
 * @param {string} subject - Subject IRI
 * @param {string} predicate - Predicate IRI
 * @param {string} object - Object value
 * @returns {Object} Reactive triple state
 * @returns {import('vue').Ref<string>} returns.subject - Reactive subject IRI
 * @returns {import('vue').Ref<string>} returns.predicate - Reactive predicate IRI
 * @returns {import('vue').Ref<string>} returns.object - Reactive object value
 * @returns {import('vue').Ref<boolean>} returns.exists - Whether triple exists in store
 * @returns {Function} returns.save - Persist current state to store
 * @returns {Function} returns.remove - Remove triple from store
 *
 * @example
 * const triple = useTriple('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o');
 * triple.save(); // adds to store
 * console.log(triple.exists.value); // true
 */
export function useTriple(subject, predicate, object) {
  IriSchema.parse(subject);
  IriSchema.parse(predicate);
  IriSchema.parse(object);

  const ctx = useStoreContext();

  const subjectRef = ref(subject);
  const predicateRef = ref(predicate);
  const objectRef = ref(object);
  const exists = ref(false);

  /**
   * Build a quad from current ref values
   * @returns {Object} Quad
   * @private
   */
  function _buildQuad() {
    const s = ctx.namedNode(subjectRef.value);
    const p = ctx.namedNode(predicateRef.value);
    const o = ctx.namedNode(objectRef.value);
    return ctx.quad(s, p, o);
  }

  /**
   * Persist the triple to the store
   * @returns {void}
   */
  function save() {
    const q = _buildQuad();
    ctx.add(q);
    exists.value = true;
  }

  /**
   * Remove the triple from the store
   * @returns {void}
   */
  function remove() {
    const q = _buildQuad();
    ctx.remove(q);
    exists.value = false;
  }

  return {
    subject: subjectRef,
    predicate: predicateRef,
    object: objectRef,
    exists,
    save,
    remove,
  };
}

/**
 * Reactive named node with utility methods.
 *
 * Wraps an IRI as a reactive named node, providing convenience methods
 * for common RDF operations on that node.
 *
 * @param {string} iri - The IRI for the named node
 * @returns {Object} Reactive named node state
 * @returns {import('vue').Ref<string>} returns.iri - Reactive IRI value
 * @returns {import('vue').ComputedRef<string>} returns.localName - Computed local name (fragment or last path segment)
 * @returns {import('vue').ComputedRef<string>} returns.namespace - Computed namespace
 * @returns {Function} returns.toTerm - Convert to RDF term
 * @returns {Function} returns.equals - Check equality with another IRI
 *
 * @example
 * const node = useNamedNode('http://xmlns.com/foaf/0.1/name');
 * console.log(node.localName.value); // "name"
 * console.log(node.namespace.value); // "http://xmlns.com/foaf/0.1/"
 */
export function useNamedNode(iri) {
  IriSchema.parse(iri);

  const ctx = useStoreContext();

  const iriRef = ref(iri);

  const localName = computed(() => {
    const val = iriRef.value;
    const hashIdx = val.lastIndexOf('#');
    if (hashIdx !== -1) {
      return val.substring(hashIdx + 1);
    }
    const slashIdx = val.lastIndexOf('/');
    if (slashIdx !== -1) {
      return val.substring(slashIdx + 1);
    }
    return val;
  });

  const namespace = computed(() => {
    const val = iriRef.value;
    const hashIdx = val.lastIndexOf('#');
    if (hashIdx !== -1) {
      return val.substring(0, hashIdx + 1);
    }
    const slashIdx = val.lastIndexOf('/');
    if (slashIdx !== -1) {
      return val.substring(0, slashIdx + 1);
    }
    return '';
  });

  /**
   * Convert to an RDF named node term
   * @returns {Object} Named node term
   */
  function toTerm() {
    return ctx.namedNode(iriRef.value);
  }

  /**
   * Check equality with another IRI string
   * @param {string} otherIri - IRI to compare with
   * @returns {boolean} True if IRIs are equal
   */
  function equals(otherIri) {
    return iriRef.value === otherIri;
  }

  return {
    iri: iriRef,
    localName,
    namespace,
    toTerm,
    equals,
  };
}

/**
 * Reactive SPARQL query that re-executes when dependencies change.
 *
 * Wraps a SPARQL query string in a reactive shell. When the query string
 * changes (via the returned ref), the query is automatically re-executed
 * and results are updated reactively.
 *
 * @param {string} sparqlQuery - Initial SPARQL query string
 * @returns {Object} Reactive query state
 * @returns {import('vue').Ref<string>} returns.queryString - Reactive query string
 * @returns {import('vue').Ref<Object|null>} returns.result - Latest query result
 * @returns {import('vue').Ref<boolean>} returns.loading - Whether query is executing
 * @returns {import('vue').Ref<Error|null>} returns.error - Last error, if any
 * @returns {Function} returns.execute - Manually trigger query execution
 * @returns {Function} returns.reset - Clear results and error
 *
 * @example
 * const { result, loading, execute } = useQuery('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
 * await execute();
 * console.log(result.value);
 */
export function useQuery(sparqlQuery) {
  SparqlQuerySchema.parse(sparqlQuery);

  const ctx = useStoreContext();

  const queryString = ref(sparqlQuery);
  const result = shallowRef(null);
  const loading = ref(false);
  const error = ref(null);

  /**
   * Execute the current query against the store
   * @param {Object} [options] - Query options
   * @returns {Promise<Object>} Query result
   */
  async function execute(options = {}) {
    loading.value = true;
    error.value = null;

    try {
      const res = await ctx.query(queryString.value, options);
      result.value = res;
      triggerRef(result);
      return res;
    } catch (err) {
      error.value = err;
      result.value = null;
      throw err;
    } finally {
      loading.value = false;
    }
  }

  /**
   * Reset the query state
   * @returns {void}
   */
  function reset() {
    result.value = null;
    error.value = null;
    loading.value = false;
  }

  // Auto-execute when query string changes
  watch(queryString, async (newQuery) => {
    if (newQuery && newQuery.trim()) {
      try {
        await execute();
      } catch (_err) {
        // Error already captured in error ref
      }
    }
  });

  return {
    queryString,
    result,
    loading,
    error,
    execute,
    reset,
  };
}
