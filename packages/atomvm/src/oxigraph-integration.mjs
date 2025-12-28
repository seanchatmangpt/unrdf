/**
 * Oxigraph Integration - Real Oxigraph RDF Store Integration
 *
 * Connects OxigraphBridge to actual @unrdf/oxigraph implementation.
 * Provides factory functions for creating store instances and bridges.
 *
 * **Poka-Yoke Design**: Ensures store is properly initialized before use.
 *
 * @module oxigraph-integration
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { OxigraphBridge } from './oxigraph-bridge.mjs';
import { trace, SpanStatusCode } from '@opentelemetry/api';

/**
 * Get tracer lazily to ensure provider is registered first
 * @returns {import('@opentelemetry/api').Tracer}
 */
function getTracer() {
  return trace.getTracer('oxigraph-integration');
}

/**
 * Create a new Oxigraph store instance
 *
 * **Poka-Yoke**: Factory ensures store is properly initialized
 *
 * @param {Array<object>} [initialQuads] - Optional initial quads to populate
 * @returns {import('@unrdf/oxigraph').OxigraphStore} Oxigraph store instance
 *
 * @example
 * ```javascript
 * import { createOxigraphStore } from '@unrdf/atomvm';
 *
 * const store = createOxigraphStore();
 * console.log(`Store created with ${store.size} quads`);
 * ```
 */
export function createOxigraphStore(initialQuads) {
  const tracer = getTracer();
  return tracer.startActiveSpan('integration.create_store', {
    attributes: {
      'store.has_initial_quads': !!initialQuads,
      'store.initial_count': initialQuads?.length || 0,
    },
  }, (span) => {
    try {
      const store = createStore(initialQuads);

      span.setAttribute('store.created', true);
      span.setAttribute('store.size', store.size);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      return store;
    } catch (error) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      span.end();
      throw error;
    }
  });
}

/**
 * Create a new OxigraphBridge connected to a real Oxigraph store
 *
 * **Poka-Yoke**: Factory ensures bridge is properly initialized with valid store
 *
 * @param {Array<object>} [initialQuads] - Optional initial quads to populate
 * @returns {OxigraphBridge} Bridge instance ready for use
 *
 * @example
 * ```javascript
 * import { createOxigraphBridge, dataFactory } from '@unrdf/atomvm';
 *
 * const bridge = createOxigraphBridge();
 * const { namedNode, literal } = dataFactory;
 *
 * await bridge.addTriples([{
 *   subject: namedNode('http://example.org/s'),
 *   predicate: namedNode('http://example.org/p'),
 *   object: literal('value')
 * }]);
 * ```
 */
export function createOxigraphBridge(initialQuads) {
  const tracer = getTracer();
  return tracer.startActiveSpan('integration.create_bridge', {
    attributes: {
      'bridge.has_initial_quads': !!initialQuads,
      'bridge.initial_count': initialQuads?.length || 0,
    },
  }, (span) => {
    try {
      const store = createOxigraphStore(initialQuads);
      const bridge = new OxigraphBridge(store);

      span.setAttribute('bridge.created', true);
      span.setAttribute('bridge.state', bridge.state);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      return bridge;
    } catch (error) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      span.end();
      throw error;
    }
  });
}

/**
 * Create a fully integrated Oxigraph store + bridge combination
 *
 * Returns both the store and bridge for advanced use cases.
 *
 * @param {Array<object>} [initialQuads] - Optional initial quads to populate
 * @returns {{store: import('@unrdf/oxigraph').OxigraphStore, bridge: OxigraphBridge}} Store and bridge
 *
 * @example
 * ```javascript
 * import { createIntegratedStore } from '@unrdf/atomvm';
 *
 * const { store, bridge } = createIntegratedStore();
 *
 * // Use store directly for SPARQL
 * const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10');
 *
 * // Use bridge for RDF operations
 * await bridge.addTriples([...]);
 * ```
 */
export function createIntegratedStore(initialQuads) {
  const tracer = getTracer();
  return tracer.startActiveSpan('integration.create_integrated', {
    attributes: {
      'integrated.has_initial_quads': !!initialQuads,
      'integrated.initial_count': initialQuads?.length || 0,
    },
  }, (span) => {
    try {
      const store = createOxigraphStore(initialQuads);
      const bridge = new OxigraphBridge(store);

      span.setAttribute('integrated.created', true);
      span.setAttribute('integrated.store_size', store.size);
      span.setAttribute('integrated.bridge_state', bridge.state);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      return { store, bridge };
    } catch (error) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      span.end();
      throw error;
    }
  });
}

/**
 * Re-export dataFactory from @unrdf/oxigraph for convenience
 *
 * Provides factory methods for creating RDF terms:
 * - namedNode(iri) - Create IRI node
 * - literal(value, languageOrDatatype?) - Create literal
 * - blankNode(id?) - Create blank node
 * - quad(subject, predicate, object, graph?) - Create quad
 * - defaultGraph() - Get default graph
 */
export { dataFactory };

/**
 * Export bridge operations for OTEL tracking
 */
export { BRIDGE_OPERATIONS } from './oxigraph-bridge.mjs';
