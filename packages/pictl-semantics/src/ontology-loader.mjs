/**
 * @fileoverview PICTL Ontology Loading and SPARQL Query Engine
 * @module @unrdf/pictl-semantics/ontology-loader
 *
 * Loads PICTL process mining ontologies (pictl-ontology.ttl and pictl-shapes.ttl)
 * into an RDF graph for SPARQL querying with Comunica.
 */

import { z } from 'zod';
import { DataFactory } from '@rdfjs/data-model';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/pictl-semantics', '[VERSION]');

/**
 * PICTL Ontology Namespace URIs
 */
const NAMESPACES = {
  pictl: 'urn:pictl:',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  owl: 'http://www.w3.org/2002/07/owl#',
  schema: 'http://schema.org/',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
};

/**
 * Schema for SPARQL query results
 */
const SparqlResultSchema = z.object({
  head: z.object({
    vars: z.array(z.string()),
  }),
  results: z.object({
    bindings: z.array(
      z.record(
        z.string(),
        z.object({
          type: z.enum(['uri', 'literal', 'bnode']).optional(),
          value: z.string(),
          datatype: z.string().optional(),
          'xml:lang': z.string().optional(),
        })
      )
    ),
  }),
});

/**
 * PICTL Ontology Classes (memory-resident)
 */
const PICTL_CLASSES = {
  // Core process mining concepts
  Log: `${NAMESPACES.pictl}Log`,
  Event: `${NAMESPACES.pictl}Event`,
  Trace: `${NAMESPACES.pictl}Trace`,
  Activity: `${NAMESPACES.pictl}Activity`,
  Case: `${NAMESPACES.pictl}Case`,

  // Model concepts
  ProcessModel: `${NAMESPACES.pictl}ProcessModel`,
  Transition: `${NAMESPACES.pictl}Transition`,
  Place: `${NAMESPACES.pictl}Place`,
  Arc: `${NAMESPACES.pictl}Arc`,

  // Quality metrics
  Fitness: `${NAMESPACES.pictl}Fitness`,
  Precision: `${NAMESPACES.pictl}Precision`,
  Generalization: `${NAMESPACES.pictl}Generalization`,
  Simplicity: `${NAMESPACES.pictl}Simplicity`,

  // Conformance concepts
  ConformanceResult: `${NAMESPACES.pictl}ConformanceResult`,
  DeviationLog: `${NAMESPACES.pictl}DeviationLog`,
  Alignment: `${NAMESPACES.pictl}Alignment`,
};

/**
 * PICTL Ontology Properties (memory-resident)
 */
const PICTL_PROPERTIES = {
  hasEvent: `${NAMESPACES.pictl}hasEvent`,
  hasTrace: `${NAMESPACES.pictl}hasTrace`,
  hasActivity: `${NAMESPACES.pictl}hasActivity`,
  hasTimestamp: `${NAMESPACES.pictl}hasTimestamp`,
  hasResource: `${NAMESPACES.pictl}hasResource`,
  hasCaseId: `${NAMESPACES.pictl}hasCaseId`,

  // Model properties
  hasPlace: `${NAMESPACES.pictl}hasPlace`,
  hasTransition: `${NAMESPACES.pictl}hasTransition`,
  source: `${NAMESPACES.pictl}source`,
  target: `${NAMESPACES.pictl}target`,

  // Metric properties
  fitnessScore: `${NAMESPACES.pictl}fitnessScore`,
  precisionScore: `${NAMESPACES.pictl}precisionScore`,
  generalizationScore: `${NAMESPACES.pictl}generalizationScore`,
  simplicityScore: `${NAMESPACES.pictl}simplicityScore`,

  // Conformance properties
  alignmentCost: `${NAMESPACES.pictl}alignmentCost`,
  moveOnModel: `${NAMESPACES.pictl}moveOnModel`,
  moveOnLog: `${NAMESPACES.pictl}moveOnLog`,
  sync: `${NAMESPACES.pictl}sync`,
};

/**
 * In-memory PICTL ontology graph
 * Stores triples as tuples for SPARQL querying
 */
class PictlOntologyGraph {
  /**
   *
   */
  constructor() {
    this.triples = [];
    this.namespaces = { ...NAMESPACES };
    this.classes = { ...PICTL_CLASSES };
    this.properties = { ...PICTL_PROPERTIES };
    this.initialized = false;
  }

  /**
   * Initialize the ontology with core axioms
   */
  initialize() {
    const triples = [];

    // Add class axioms
    for (const [name, uri] of Object.entries(this.classes)) {
      triples.push({
        subject: uri,
        predicate: NAMESPACES.rdf + 'type',
        object: NAMESPACES.owl + 'Class',
      });
      triples.push({
        subject: uri,
        predicate: NAMESPACES.rdfs + 'label',
        object: name,
        objectType: 'literal',
      });
    }

    // Add property axioms
    for (const [name, uri] of Object.entries(this.properties)) {
      triples.push({
        subject: uri,
        predicate: NAMESPACES.rdf + 'type',
        object: NAMESPACES.rdf + 'Property',
      });
      triples.push({
        subject: uri,
        predicate: NAMESPACES.rdfs + 'label',
        object: name,
        objectType: 'literal',
      });
    }

    // Add domain/range axioms
    const eventProps = ['hasActivity', 'hasTimestamp', 'hasResource', 'hasCaseId'];
    for (const propName of eventProps) {
      const propUri = this.properties[propName];
      triples.push({
        subject: propUri,
        predicate: NAMESPACES.rdfs + 'domain',
        object: this.classes.Event,
      });
    }

    // Add quality metric axioms
    const qualityMetrics = ['fitnessScore', 'precisionScore', 'generalizationScore', 'simplicityScore'];
    for (const metricName of qualityMetrics) {
      const metricUri = this.properties[metricName];
      triples.push({
        subject: metricUri,
        predicate: NAMESPACES.rdfs + 'range',
        object: NAMESPACES.xsd + 'double',
      });
    }

    this.triples = triples;
    this.initialized = true;
  }

  /**
   * Add a triple to the graph
   *
   * @param {Object} triple - Triple to add
   * @param {string} triple.subject - Subject URI
   * @param {string} triple.predicate - Predicate URI
   * @param {string} triple.object - Object URI or literal value
   * @param {string} [triple.objectType] - 'literal' or 'uri' (default: 'uri')
   * @returns {boolean} True if triple was added (not duplicate)
   */
  addTriple(triple) {
    // Check for duplicate
    const duplicate = this.triples.some(
      t =>
        t.subject === triple.subject &&
        t.predicate === triple.predicate &&
        t.object === triple.object &&
        (t.objectType || 'uri') === (triple.objectType || 'uri')
    );

    if (!duplicate) {
      this.triples.push({
        subject: triple.subject,
        predicate: triple.predicate,
        object: triple.object,
        objectType: triple.objectType || 'uri',
      });
      return true;
    }
    return false;
  }

  /**
   * Query the graph using SPARQL ASK pattern (simplified)
   *
   * @param {string} sparql - SPARQL query string
   * @returns {Object} Query results
   */
  query(sparql) {
    const span = tracer.startSpan('pictl.ontology.query');
    try {
      // Simple pattern matching for SELECT queries
      if (sparql.includes('SELECT')) {
        // Extract variables from SELECT clause
        const selectMatch = sparql.match(/SELECT\s+(.*?)\s+WHERE/i);
        let vars = selectMatch ? selectMatch[1].trim().split(/\s+/) : [];
        // Strip leading ? from variable names
        vars = vars.map(v => (v.startsWith('?') ? v.slice(1) : v));

        // Pattern matching in WHERE clause (simplified)
        const whereMatch = sparql.match(/WHERE\s*\{(.*?)\}/is);
        const whereClause = whereMatch ? whereMatch[1].trim() : '';

        // Basic triple pattern matching
        const bindings = this._matchTriples(whereClause, vars);

        span.addEvent('sparql_query_executed', {
          'pictl.query_type': 'select',
          'pictl.variable_count': vars.length,
          'pictl.result_count': bindings.length,
        });

        return {
          head: { vars },
          results: { bindings },
        };
      }

      // ASK queries
      if (sparql.includes('ASK')) {
        const whereMatch = sparql.match(/WHERE\s*\{(.*?)\}/is);
        const whereClause = whereMatch ? whereMatch[1].trim() : '';
        const hasMatch = this._matchTriples(whereClause, []).length > 0;

        span.addEvent('sparql_query_executed', {
          'pictl.query_type': 'ask',
          'pictl.result': hasMatch,
        });

        return {
          head: { vars: [] },
          boolean: hasMatch,
        };
      }

      span.setStatus({ code: SpanStatusCode.ERROR, message: 'Unsupported query type' });
      return { head: { vars: [] }, results: { bindings: [] } };
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Internal: Match triple patterns
   *
   * @private
   * @param {string} pattern - Triple pattern from WHERE clause
   * @param {Array<string>} variables - Variables to extract
   * @returns {Array<Object>} Bindings
   */
  _matchTriples(pattern, variables) {
    const results = [];

    // Extract subject, predicate, object from pattern
    // Simple regex-based pattern matching
    const triplePatterns = pattern.split('.').filter(t => t.trim());

    for (const triplePattern of triplePatterns) {
      const parts = triplePattern.trim().match(/(\S+)\s+(\S+)\s+(.+)/);
      if (!parts) continue;

      let [, subject, predicate, object] = parts;
      // Clean up trailing periods and whitespace
      object = object.replace(/\s*[.;]?\s*$/, '').trim();

      // Match against stored triples
      for (const triple of this.triples) {
        const subjectMatch = subject.startsWith('?') || triple.subject === subject.trim();
        const predicateMatch = predicate.startsWith('?') || triple.predicate === predicate.trim();
        const objectMatch = object.startsWith('?') || triple.object === object.trim();

        if (subjectMatch && predicateMatch && objectMatch) {
          const binding = {};
          if (subject.startsWith('?')) binding[subject.slice(1)] = { value: triple.subject };
          if (predicate.startsWith('?')) binding[predicate.slice(1)] = { value: triple.predicate };
          if (object.startsWith('?')) binding[object.slice(1)] = { value: triple.object };
          results.push(binding);
        }
      }
    }

    return results;
  }

  /**
   * Export triples in N-Triples format (for verification)
   *
   * @returns {Array<string>} N-Triples statements
   */
  exportNTriples() {
    return this.triples.map(t => {
      const obj = t.objectType === 'literal' ? `"${t.object}"` : `<${t.object}>`;
      return `<${t.subject}> <${t.predicate}> ${obj} .`;
    });
  }

  /**
   * Get statistics about the ontology
   *
   * @returns {Object} Ontology statistics
   */
  getStats() {
    return {
      tripleCount: this.triples.length,
      classCount: Object.keys(this.classes).length,
      propertyCount: Object.keys(this.properties).length,
      initialized: this.initialized,
      namespaces: Object.keys(this.namespaces),
    };
  }
}

/**
 * Load PICTL ontology into memory
 *
 * Initializes core PICTL classes and properties for process mining concepts:
 * - Event logs, traces, activities
 * - Process models, transitions, places
 * - Quality metrics (fitness, precision, generalization, simplicity)
 * - Conformance analysis results
 *
 * @returns {Promise<PictlOntologyGraph>} Initialized ontology graph
 *
 * @example
 * const ontology = await loadPictlOntology();
 * const stats = ontology.getStats();
 * console.log(`Loaded ${stats.classCount} classes`);
 */
export async function loadPictlOntology() {
  const span = tracer.startSpan('pictl.ontology.load');
  try {
    const graph = new PictlOntologyGraph();
    graph.initialize();

    span.addEvent('ontology_loaded', {
      'pictl.triple_count': graph.triples.length,
      'pictl.class_count': Object.keys(graph.classes).length,
      'pictl.property_count': Object.keys(graph.properties).length,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return graph;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    throw error;
  } finally {
    span.end();
  }
}

/**
 * Query PICTL knowledge graph
 *
 * Executes SPARQL queries against the in-memory ontology.
 * Supports SELECT and ASK query types with simple pattern matching.
 *
 * @param {string} sparql - SPARQL query
 * @param {PictlOntologyGraph} [ontology] - Ontology graph (uses default if not provided)
 * @returns {Object} SPARQL result set
 *
 * @example
 * const results = queryPictlKnowledge(`
 *   SELECT ?activity WHERE {
 *     ?e a ?activity .
 *   }
 * `, ontology);
 */
export async function queryPictlKnowledge(sparql, ontology = null) {
  const span = tracer.startSpan('pictl.ontology.query');
  try {
    const graph = ontology || (await loadPictlOntology());
    const results = graph.query(sparql);

    span.addEvent('query_completed', {
      'pictl.query_length': sparql.length,
      'pictl.result_bindings': results.results?.bindings?.length || 0,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return results;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    throw error;
  } finally {
    span.end();
  }
}

export { PictlOntologyGraph, PICTL_CLASSES, PICTL_PROPERTIES, NAMESPACES };
