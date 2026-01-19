/**
 * @file AI Anomaly Detector for RDF Data Quality Analysis
 * @module ai-semantic/anomaly-detector
 *
 * @description
 * Implements AI-powered anomaly detection for RDF knowledge graphs.
 * Detects inconsistencies, missing data, outliers, and quality issues
 * using statistical analysis and pattern recognition techniques.
 *
 * Integrates with UNRDF's Knowledge Hook system and provides OTEL observability.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import LRUCache from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-anomaly');

/**
 * Anomaly schema
 */
const AnomalySchema = z.object({
  type: z.enum([
    'missing_property',
    'outlier_value',
    'type_inconsistency',
    'domain_violation',
    'range_violation',
    'cardinality_violation',
    'orphan_entity',
    'duplicate_entity',
  ]),
  entity: z.string(),
  property: z.string().optional(),
  severity: z.enum(['high', 'medium', 'low']),
  confidence: z.number().min(0).max(1),
  description: z.string(),
  suggestion: z.string().optional(),
});

/**
 * Anomaly detection result schema
 */
const AnomalyDetectionResultSchema = z.object({
  anomalies: z.array(AnomalySchema),
  statistics: z.object({
    totalEntities: z.number(),
    totalTriples: z.number(),
    anomaliesFound: z.number(),
    severityBreakdown: z.object({
      high: z.number(),
      medium: z.number(),
      low: z.number(),
    }),
  }),
  duration: z.number(),
});

/**
 * Anomaly Detector Configuration
 */
const AnomalyDetectorConfigSchema = z.object({
  cacheSize: z.number().default(1000),
  enableCache: z.boolean().default(true),
  outlierThreshold: z.number().default(3.0), // Standard deviations
  minOccurrences: z.number().default(5), // Minimum for pattern detection
  confidenceThreshold: z.number().min(0).max(1).default(0.7),
});

/**
 * AI Anomaly Detector for RDF graphs
 */
export class AnomalyDetector {
  /**
   * Create a new anomaly detector
   * @param {Object} [config] - Detector configuration
   * @param {number} [config.cacheSize=1000] - Cache size
   * @param {boolean} [config.enableCache=true] - Enable caching
   * @param {number} [config.outlierThreshold=3.0] - Outlier detection threshold (std devs)
   * @param {number} [config.minOccurrences=5] - Minimum occurrences for pattern detection
   * @param {number} [config.confidenceThreshold=0.7] - Minimum confidence threshold
   */
  constructor(config = {}) {
    this.config = AnomalyDetectorConfigSchema.parse(config);

    // Initialize cache if enabled
    if (this.config.enableCache) {
      this.cache = new LRUCache({
        max: this.config.cacheSize,
        ttl: 1000 * 60 * 15, // 15 minutes
      });
    }

    // Statistics
    this.stats = {
      detections: 0,
      anomaliesFound: 0,
      cacheHits: 0,
    };
  }

  /**
   * Detect anomalies in RDF store
   * @param {Store} store - RDF store to analyze
   * @param {Object} [options] - Detection options
   * @param {Array<string>} [options.targetEntities] - Specific entities to check
   * @param {Array<string>} [options.excludeTypes] - Entity types to exclude
   * @returns {Promise<Object>} Detection result
   */
  async detect(store, options = {}) {
    return tracer.startActiveSpan('anomaly.detect', async (span) => {
      try {
        const startTime = Date.now();

        span.setAttributes({
          'anomaly.store_size': store.size,
        });

        // Check cache
        const cacheKey = this._getCacheKey(store);
        if (this.config.enableCache && this.cache.has(cacheKey)) {
          this.stats.cacheHits++;
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();
          return this.cache.get(cacheKey);
        }

        const anomalies = [];

        // Detect missing properties
        anomalies.push(...this._detectMissingProperties(store, options));

        // Detect type inconsistencies
        anomalies.push(...this._detectTypeInconsistencies(store, options));

        // Detect orphan entities
        anomalies.push(...this._detectOrphanEntities(store, options));

        // Detect outlier values
        anomalies.push(...this._detectOutlierValues(store, options));

        // Detect domain/range violations
        anomalies.push(...this._detectDomainRangeViolations(store, options));

        // Detect cardinality violations
        anomalies.push(...this._detectCardinalityViolations(store, options));

        // Calculate statistics
        const entities = new Set();
        for (const quad of store) {
          entities.add(quad.subject.value);
          if (quad.object.termType === 'NamedNode') {
            entities.add(quad.object.value);
          }
        }

        const severityBreakdown = {
          high: anomalies.filter((a) => a.severity === 'high').length,
          medium: anomalies.filter((a) => a.severity === 'medium').length,
          low: anomalies.filter((a) => a.severity === 'low').length,
        };

        const duration = Date.now() - startTime;

        const result = AnomalyDetectionResultSchema.parse({
          anomalies,
          statistics: {
            totalEntities: entities.size,
            totalTriples: store.size,
            anomaliesFound: anomalies.length,
            severityBreakdown,
          },
          duration,
        });

        // Cache result
        if (this.config.enableCache) {
          this.cache.set(cacheKey, result);
        }

        this.stats.detections++;
        this.stats.anomaliesFound += anomalies.length;

        span.setStatus({ code: SpanStatusCode.OK });
        span.setAttributes({
          'anomaly.found': anomalies.length,
          'anomaly.high_severity': severityBreakdown.high,
        });
        span.end();

        return result;
      } catch (error) {
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.recordException(error);
        span.end();
        throw error;
      }
    });
  }

  /**
   * Detect missing properties
   * @param {Store} store - RDF store
   * @param {Object} options - Options
   * @returns {Array<Object>} Anomalies
   * @private
   */
  _detectMissingProperties(store, _options) {
    const anomalies = [];
    const propertyFrequency = new Map();

    // Count property usage per type
    const typeProperties = new Map();

    for (const quad of store) {
      if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
        const type = quad.object.value;
        if (!typeProperties.has(type)) {
          typeProperties.set(type, new Map());
        }
      }
    }

    for (const quad of store) {
      const subject = quad.subject.value;
      const predicate = quad.predicate.value;

      // Find type of subject
      let entityType = null;
      for (const typeQuad of store) {
        if (
          typeQuad.subject.value === subject &&
          typeQuad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
        ) {
          entityType = typeQuad.object.value;
          break;
        }
      }

      if (entityType && typeProperties.has(entityType)) {
        const props = typeProperties.get(entityType);
        props.set(predicate, (props.get(predicate) || 0) + 1);
      }
    }

    // Detect entities missing common properties
    for (const [type, props] of typeProperties) {
      const totalEntities = Array.from(store).filter(
        (q) =>
          q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
          q.object.value === type
      ).length;

      for (const [prop, count] of props) {
        const frequency = count / totalEntities;
        propertyFrequency.set(`${type}:${prop}`, frequency);

        // If property is very common (>80%) but missing from some entities
        if (frequency > 0.8 && frequency < 1.0) {
          // Find entities missing this property
          const entitiesWithType = Array.from(store)
            .filter(
              (q) =>
                q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
                q.object.value === type
            )
            .map((q) => q.subject.value);

          const entitiesWithProp = Array.from(store)
            .filter((q) => q.predicate.value === prop)
            .map((q) => q.subject.value);

          const missingEntities = entitiesWithType.filter((e) => !entitiesWithProp.includes(e));

          for (const entity of missingEntities.slice(0, 10)) {
            anomalies.push({
              type: 'missing_property',
              entity,
              property: prop,
              severity: 'medium',
              confidence: frequency,
              description: `Entity is missing common property ${prop} (${Math.round(frequency * 100)}% of ${type} entities have it)`,
              suggestion: `Add ${prop} property to ${entity}`,
            });
          }
        }
      }
    }

    return anomalies;
  }

  /**
   * Detect type inconsistencies
   * @param {Store} store - RDF store
   * @param {Object} options - Options
   * @returns {Array<Object>} Anomalies
   * @private
   */
  _detectTypeInconsistencies(store, _options) {
    const anomalies = [];
    const entityTypes = new Map();

    // Collect types for each entity
    for (const quad of store) {
      if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
        const entity = quad.subject.value;
        if (!entityTypes.has(entity)) {
          entityTypes.set(entity, []);
        }
        entityTypes.get(entity).push(quad.object.value);
      }
    }

    // Check for entities with multiple types (potential inconsistency)
    for (const [entity, types] of entityTypes) {
      if (types.length > 3) {
        // More than 3 types might be suspicious
        anomalies.push({
          type: 'type_inconsistency',
          entity,
          severity: 'low',
          confidence: 0.6,
          description: `Entity has ${types.length} different types, which may indicate over-classification`,
          suggestion: 'Review entity types and remove unnecessary ones',
        });
      }
    }

    return anomalies;
  }

  /**
   * Detect orphan entities
   * @param {Store} store - RDF store
   * @param {Object} options - Options
   * @returns {Array<Object>} Anomalies
   * @private
   */
  _detectOrphanEntities(store, _options) {
    const anomalies = [];
    const entityConnections = new Map();

    // Count connections for each entity
    for (const quad of store) {
      const subject = quad.subject.value;
      entityConnections.set(subject, (entityConnections.get(subject) || 0) + 1);

      if (quad.object.termType === 'NamedNode') {
        const object = quad.object.value;
        entityConnections.set(object, (entityConnections.get(object) || 0) + 1);
      }
    }

    // Find entities with very few connections
    for (const [entity, connections] of entityConnections) {
      if (connections <= 2) {
        // Only has rdf:type or one property
        anomalies.push({
          type: 'orphan_entity',
          entity,
          severity: 'low',
          confidence: 0.75,
          description: `Entity has only ${connections} connection(s), may be orphaned or incomplete`,
          suggestion: 'Add more properties or relationships to this entity',
        });
      }
    }

    return anomalies.slice(0, 20); // Limit to top 20
  }

  /**
   * Detect outlier values
   * @param {Store} store - RDF store
   * @param {Object} options - Options
   * @returns {Array<Object>} Anomalies
   * @private
   */
  _detectOutlierValues(store, _options) {
    const anomalies = [];
    const numericProperties = new Map();

    // Collect numeric values for each property
    for (const quad of store) {
      if (
        quad.object.termType === 'Literal' &&
        quad.object.datatype &&
        (quad.object.datatype.value === 'http://www.w3.org/2001/XMLSchema#integer' ||
          quad.object.datatype.value === 'http://www.w3.org/2001/XMLSchema#decimal' ||
          quad.object.datatype.value === 'http://www.w3.org/2001/XMLSchema#double')
      ) {
        const predicate = quad.predicate.value;
        if (!numericProperties.has(predicate)) {
          numericProperties.set(predicate, []);
        }
        numericProperties.get(predicate).push({
          entity: quad.subject.value,
          value: parseFloat(quad.object.value),
        });
      }
    }

    // Detect outliers using standard deviation
    for (const [predicate, values] of numericProperties) {
      if (values.length < this.config.minOccurrences) continue;

      const numValues = values.map((v) => v.value);
      const mean = numValues.reduce((sum, v) => sum + v, 0) / numValues.length;
      const variance =
        numValues.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / numValues.length;
      const stdDev = Math.sqrt(variance);

      for (const { entity, value } of values) {
        const zScore = Math.abs((value - mean) / stdDev);
        if (zScore > this.config.outlierThreshold) {
          anomalies.push({
            type: 'outlier_value',
            entity,
            property: predicate,
            severity: zScore > 4 ? 'high' : 'medium',
            confidence: Math.min(0.95, zScore / 5),
            description: `Value ${value} is ${zScore.toFixed(2)} standard deviations from mean (${mean.toFixed(2)})`,
            suggestion: 'Verify this value is correct',
          });
        }
      }
    }

    return anomalies;
  }

  /**
   * Detect domain/range violations (simplified)
   * @param {Store} store - RDF store
   * @param {Object} options - Options
   * @returns {Array<Object>} Anomalies
   * @private
   */
  _detectDomainRangeViolations(_store, _options) {
    // This is a simplified placeholder
    // Real implementation would check against RDFS/OWL ontology
    return [];
  }

  /**
   * Detect cardinality violations (simplified)
   * @param {Store} store - RDF store
   * @param {Object} options - Options
   * @returns {Array<Object>} Anomalies
   * @private
   */
  _detectCardinalityViolations(_store, _options) {
    // This is a simplified placeholder
    // Real implementation would check against OWL cardinality restrictions
    return [];
  }

  /**
   * Generate cache key
   * @param {Store} store - RDF store
   * @returns {string} Cache key
   * @private
   */
  _getCacheKey(store) {
    const sample = Array.from(store)
      .slice(0, 10)
      .map((q) => `${q.subject.value}|${q.predicate.value}|${q.object.value}`)
      .join('::');
    return `anomaly:${store.size}:${sample}`;
  }

  /**
   * Clear the detection cache
   */
  clearCache() {
    if (this.cache) {
      this.cache.clear();
    }
  }

  /**
   * Get detector statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      cacheSize: this.cache ? this.cache.size : 0,
      cacheHitRate: this.stats.detections > 0 ? this.stats.cacheHits / this.stats.detections : 0,
      avgAnomaliesPerDetection:
        this.stats.detections > 0 ? this.stats.anomaliesFound / this.stats.detections : 0,
    };
  }
}

/**
 * Create an anomaly detector instance
 * @param {Object} [config] - Configuration
 * @returns {AnomalyDetector} Anomaly detector
 */
export function createAnomalyDetector(config = {}) {
  return new AnomalyDetector(config);
}

/**
 * Default anomaly detector instance
 */
export const defaultAnomalyDetector = createAnomalyDetector();
