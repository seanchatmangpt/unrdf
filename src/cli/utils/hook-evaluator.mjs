/**
 * @file Hook Evaluation Engine
 * @module cli/utils/hook-evaluator
 *
 * @description
 * Core engine for evaluating knowledge hooks with SPARQL ASK queries,
 * SHACL validation, and threshold checks with full OTEL instrumentation.
 */

import { Store } from 'n3';
import { SparqlEngine } from '../../knowledge-engine/query.mjs';
import { trace, context as otelContext, SpanStatusCode } from '@opentelemetry/api';
import { readFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';

const tracer = trace.getTracer('unrdf-hook-evaluator');

/**
 * Evaluate a knowledge hook against RDF data
 * @param {Object} hook - Hook definition
 * @param {Store} store - N3 store with RDF data
 * @param {Object} [options] - Evaluation options
 * @returns {Promise<Object>} Evaluation result
 */
export async function evaluateHook(hook, store, options = {}) {
  return await tracer.startActiveSpan('hook.evaluate', async (span) => {
    try {
      // Add span attributes
      span.setAttribute('hook.name', hook.meta?.name || 'unnamed');
      span.setAttribute('hook.kind', hook.when?.kind || 'unknown');
      span.setAttribute('store.size', store.size);

      const startTime = Date.now();
      let result = { fired: false, executionTime: 0 };

      // Determine hook type and evaluate
      if (hook.when?.kind === 'sparql-ask') {
        result = await evaluateSparqlAsk(hook, store, span);
      } else if (hook.when?.kind === 'shacl') {
        result = await evaluateShaclHook(hook, store, span);
      } else if (hook.when?.kind === 'threshold') {
        result = await evaluateThresholdHook(hook, store, span);
      } else {
        throw new Error(`Unknown hook kind: ${hook.when?.kind}`);
      }

      result.executionTime = Date.now() - startTime;

      // Add result attributes
      span.setAttribute('hook.fired', result.fired);
      span.setAttribute('hook.executionTime', result.executionTime);
      span.setStatus({ code: SpanStatusCode.OK });

      return result;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Evaluate SPARQL ASK hook
 * @param {Object} hook - Hook definition
 * @param {Store} store - N3 store
 * @param {Span} parentSpan - Parent OTEL span
 * @returns {Promise<Object>} Evaluation result
 */
async function evaluateSparqlAsk(hook, store, parentSpan) {
  return await tracer.startActiveSpan('hook.evaluate.sparql-ask', { parent: parentSpan }, async (span) => {
    try {
      // Load SPARQL query from ref
      let query;
      if (hook.when.ref?.uri) {
        query = await loadQueryFromRef(hook.when.ref, span);
      } else if (hook.when.query) {
        query = hook.when.query;
      } else {
        throw new Error('SPARQL ASK hook requires ref.uri or query');
      }

      span.setAttribute('query.length', query.length);

      // Execute SPARQL ASK query
      const engine = new SparqlEngine(store);
      const bindings = await engine.query(query);

      // ASK queries return single boolean binding
      const fired = bindings?._root?.entries?.[0]?.[1] === 'true' || false;

      span.setAttribute('query.result', fired);
      span.setStatus({ code: SpanStatusCode.OK });

      return { fired, query, type: 'sparql-ask' };
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw new Error(`SPARQL ASK evaluation failed: ${error.message}`);
    } finally {
      span.end();
    }
  });
}

/**
 * Evaluate SHACL validation hook
 * @param {Object} hook - Hook definition
 * @param {Store} store - N3 store
 * @param {Span} parentSpan - Parent OTEL span
 * @returns {Promise<Object>} Evaluation result
 */
async function evaluateShaclHook(hook, store, parentSpan) {
  return await tracer.startActiveSpan('hook.evaluate.shacl', { parent: parentSpan }, async (span) => {
    try {
      // Load SHACL shapes from ref
      const shapesGraph = await loadShapesFromRef(hook.when.ref, span);

      span.setAttribute('shapes.size', shapesGraph.size);

      // Run SHACL validation
      const { validate } = await import('../../knowledge-engine/validate.mjs');
      const report = await validate(store, shapesGraph);

      // Hook fires if validation PASSES (conforms = true)
      const fired = report.conforms;

      span.setAttribute('validation.conforms', fired);
      span.setAttribute('validation.violations', report.results?.length || 0);
      span.setStatus({ code: SpanStatusCode.OK });

      return {
        fired,
        type: 'shacl',
        conforms: report.conforms,
        violations: report.results || []
      };
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw new Error(`SHACL evaluation failed: ${error.message}`);
    } finally {
      span.end();
    }
  });
}

/**
 * Evaluate threshold hook
 * @param {Object} hook - Hook definition
 * @param {Store} store - N3 store
 * @param {Span} parentSpan - Parent OTEL span
 * @returns {Promise<Object>} Evaluation result
 */
async function evaluateThresholdHook(hook, store, parentSpan) {
  return await tracer.startActiveSpan('hook.evaluate.threshold', { parent: parentSpan }, async (span) => {
    try {
      // Load SPARQL query for count/metric
      let query;
      if (hook.when.ref?.uri) {
        query = await loadQueryFromRef(hook.when.ref, span);
      } else if (hook.when.query) {
        query = hook.when.query;
      } else {
        throw new Error('Threshold hook requires ref.uri or query');
      }

      // Execute query to get metric value
      const engine = new SparqlEngine(store);
      const bindings = await engine.query(query);

      // Extract first binding value (assumed to be count/metric)
      const value = parseInt(bindings?._root?.entries?.[0]?.[1] || '0', 10);
      const threshold = hook.when.threshold || 0;
      const operator = hook.when.operator || 'gt';

      span.setAttribute('query.value', value);
      span.setAttribute('threshold.value', threshold);
      span.setAttribute('threshold.operator', operator);

      // Compare based on operator
      let fired = false;
      switch (operator) {
        case 'gt':
          fired = value > threshold;
          break;
        case 'gte':
          fired = value >= threshold;
          break;
        case 'lt':
          fired = value < threshold;
          break;
        case 'lte':
          fired = value <= threshold;
          break;
        case 'eq':
          fired = value === threshold;
          break;
        case 'neq':
          fired = value !== threshold;
          break;
        default:
          throw new Error(`Unknown threshold operator: ${operator}`);
      }

      span.setAttribute('threshold.fired', fired);
      span.setStatus({ code: SpanStatusCode.OK });

      return {
        fired,
        type: 'threshold',
        value,
        threshold,
        operator
      };
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw new Error(`Threshold evaluation failed: ${error.message}`);
    } finally {
      span.end();
    }
  });
}

/**
 * Load SPARQL query from content-addressed reference
 * @param {Object} ref - Content-addressed reference
 * @param {Span} parentSpan - Parent OTEL span
 * @returns {Promise<string>} Query text
 */
async function loadQueryFromRef(ref, parentSpan) {
  return await tracer.startActiveSpan('hook.loadQuery', { parent: parentSpan }, async (span) => {
    try {
      // Parse file:// URI
      const uri = ref.uri;
      if (!uri.startsWith('file://')) {
        throw new Error(`Only file:// URIs supported, got: ${uri}`);
      }

      const filePath = uri.substring(7); // Remove 'file://'
      const content = await readFile(filePath, 'utf-8');

      // Verify content hash if provided
      if (ref.sha256) {
        const hash = createHash('sha256').update(content).digest('hex');
        if (hash !== ref.sha256) {
          throw new Error(`Content hash mismatch for ${filePath}: expected ${ref.sha256}, got ${hash}`);
        }
        span.setAttribute('query.verified', true);
      }

      span.setAttribute('query.path', filePath);
      span.setAttribute('query.size', content.length);
      span.setStatus({ code: SpanStatusCode.OK });

      return content;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Load SHACL shapes from content-addressed reference
 * @param {Object} ref - Content-addressed reference
 * @param {Span} parentSpan - Parent OTEL span
 * @returns {Promise<Store>} Shapes graph
 */
async function loadShapesFromRef(ref, parentSpan) {
  return await tracer.startActiveSpan('hook.loadShapes', { parent: parentSpan }, async (span) => {
    try {
      // Parse file:// URI
      const uri = ref.uri;
      if (!uri.startsWith('file://')) {
        throw new Error(`Only file:// URIs supported, got: ${uri}`);
      }

      const filePath = uri.substring(7); // Remove 'file://'
      const content = await readFile(filePath, 'utf-8');

      // Verify content hash if provided
      if (ref.sha256) {
        const hash = createHash('sha256').update(content).digest('hex');
        if (hash !== ref.sha256) {
          throw new Error(`Content hash mismatch for ${filePath}: expected ${ref.sha256}, got ${hash}`);
        }
        span.setAttribute('shapes.verified', true);
      }

      // Parse Turtle into store
      const { useTurtle } = await import('../../composables/index.mjs');
      const turtle = await useTurtle();
      const quads = await turtle.parse(content);

      const shapesStore = new Store();
      shapesStore.addQuads(quads);

      span.setAttribute('shapes.path', filePath);
      span.setAttribute('shapes.quads', quads.length);
      span.setStatus({ code: SpanStatusCode.OK });

      return shapesStore;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

export default {
  evaluateHook,
  evaluateSparqlAsk,
  evaluateShaclHook,
  evaluateThresholdHook
};
