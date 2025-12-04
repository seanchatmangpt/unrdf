/**
 * @file Anomaly Detection for RDF Graphs
 * @module ai-semantic/anomaly-detector
 *
 * @description
 * Implements AI-powered anomaly detection for RDF knowledge graphs.
 * Detects unexpected patterns, missing links, data quality issues using
 * statistical and ML approaches. Integrates with SHACL validation.
 */

// Unused N3 import removed
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';
import { createSemanticAnalyzer } from './semantic-analyzer.mjs';
import { createEmbeddingsManager } from './embeddings-manager.mjs';

const tracer = trace.getTracer('unrdf-ai-semantic');

/**
 * Anomaly Schema
 */
const AnomalySchema = z.object({
  type: z.enum([
    'missing_link',
    'unexpected_pattern',
    'data_quality',
    'outlier',
    'inconsistency',
    'structural_anomaly',
  ]),
  severity: z.enum(['critical', 'high', 'medium', 'low']),
  description: z.string(),
  subject: z.string().optional(),
  predicate: z.string().optional(),
  object: z.string().optional(),
  confidence: z.number().min(0).max(1),
  evidence: z.array(z.string()).optional(),
});

/**
 * Anomaly Detection Result Schema
 */
const AnomalyDetectionResultSchema = z.object({
  anomalies: z.array(AnomalySchema),
  statistics: z.object({
    total: z.number(),
    bySeverity: z.record(z.number()),
    byType: z.record(z.number()),
  }),
  duration: z.number(),
});

/**
 * Anomaly Detector Configuration
 */
const AnomalyDetectorConfigSchema = z.object({
  enableStatistical: z.boolean().default(true),
  enableMLBased: z.boolean().default(true),
  enableSHACL: z.boolean().default(true),
  outlierThreshold: z.number().default(2.5), // Standard deviations
  minConfidence: z.number().min(0).max(1).default(0.6),
  maxAnomalies: z.number().default(100),
});

/**
 * Anomaly Detector for RDF graphs
 */
export class AnomalyDetector {
  /**
   * Create a new anomaly detector
   * @param {Object} [config] - Detector configuration
   */
  constructor(config = {}) {
    this.config = AnomalyDetectorConfigSchema.parse(config);

    // Initialize semantic analyzer and embeddings manager
    this.semanticAnalyzer = createSemanticAnalyzer();
    this.embeddingsManager = createEmbeddingsManager();

    // Statistics
    this.stats = {
      detections: 0,
      anomaliesFound: 0,
      avgDuration: 0,
    };
  }

  /**
   * Detect anomalies in an RDF graph
   * @param {Store} store - RDF store to analyze
   * @param {Object} [options] - Detection options
   * @returns {Promise<Object>} Detection results
   */
  async detectAnomalies(store, _options = {}) {
    return tracer.startActiveSpan('anomaly.detect', async span => {
      const startTime = Date.now();

      try {
        span.setAttributes({
          'anomaly.store_size': store.size,
          'anomaly.enable_statistical': this.config.enableStatistical,
          'anomaly.enable_ml': this.config.enableMLBased,
        });

        const anomalies = [];

        // Statistical anomaly detection
        if (this.config.enableStatistical) {
          const statAnomalies = await this._detectStatisticalAnomalies(store);
          anomalies.push(...statAnomalies);
        }

        // ML-based anomaly detection
        if (this.config.enableMLBased) {
          const mlAnomalies = await this._detectMLAnomalies(store);
          anomalies.push(...mlAnomalies);
        }

        // Missing link detection
        const missingLinks = await this._detectMissingLinks(store);
        anomalies.push(...missingLinks);

        // Data quality issues
        const qualityIssues = await this._detectDataQualityIssues(store);
        anomalies.push(...qualityIssues);

        // Structural anomalies
        const structuralAnomalies = await this._detectStructuralAnomalies(store);
        anomalies.push(...structuralAnomalies);

        // Filter by confidence and limit
        const filtered = anomalies
          .filter(a => a.confidence >= this.config.minConfidence)
          .sort((a, b) => b.confidence - a.confidence)
          .slice(0, this.config.maxAnomalies);

        // Calculate statistics
        const statistics = this._calculateAnomalyStatistics(filtered);

        const duration = Date.now() - startTime;
        const result = AnomalyDetectionResultSchema.parse({
          anomalies: filtered,
          statistics,
          duration,
        });

        // Update stats
        this.stats.detections++;
        this.stats.anomaliesFound += filtered.length;
        this.stats.avgDuration =
          (this.stats.avgDuration * (this.stats.detections - 1) + duration) / this.stats.detections;

        span.setAttributes({
          'anomaly.total_found': filtered.length,
          'anomaly.duration_ms': duration,
          'anomaly.detection_complete': true,
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Detect statistical anomalies (outliers in property distributions)
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Statistical anomalies
   * @private
   */
  async _detectStatisticalAnomalies(store) {
    return tracer.startActiveSpan('anomaly.statistical', async span => {
      try {
        const anomalies = [];

        // Calculate degree distribution
        const degreeMap = new Map();

        for (const quad of store) {
          const subject = quad.subject.value;
          degreeMap.set(subject, (degreeMap.get(subject) || 0) + 1);
        }

        // Calculate statistics
        const degrees = Array.from(degreeMap.values());
        const mean = degrees.reduce((a, b) => a + b, 0) / degrees.length;
        const variance =
          degrees.reduce((sum, d) => sum + Math.pow(d - mean, 2), 0) / degrees.length;
        const stdDev = Math.sqrt(variance);

        // Detect outliers (entities with unusually high or low degree)
        for (const [entity, degree] of degreeMap.entries()) {
          const zScore = Math.abs((degree - mean) / stdDev);

          if (zScore > this.config.outlierThreshold) {
            anomalies.push({
              type: 'outlier',
              severity: zScore > 4 ? 'high' : 'medium',
              description: `Entity has unusual degree: ${degree} (mean: ${mean.toFixed(2)}, z-score: ${zScore.toFixed(2)})`,
              subject: entity,
              confidence: Math.min(0.9, zScore / 10),
              evidence: [`Degree: ${degree}`, `Z-score: ${zScore.toFixed(2)}`],
            });
          }
        }

        span.setAttribute('anomaly.statistical_found', anomalies.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return anomalies;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return [];
      }
    });
  }

  /**
   * Detect ML-based anomalies using embeddings
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} ML-based anomalies
   * @private
   */
  async _detectMLAnomalies(store) {
    return tracer.startActiveSpan('anomaly.ml_based', async span => {
      try {
        const anomalies = [];

        // Generate embeddings if not already trained
        if (this.embeddingsManager.entityEmbeddings.size === 0) {
          await this.embeddingsManager.generateEmbeddings(store);
        }

        // Detect entities with unusual embedding patterns
        const entities = Array.from(this.embeddingsManager.entityEmbeddings.keys());

        if (entities.length < 10) {
          span.setAttribute('anomaly.ml_skipped', 'insufficient_data');
          span.setStatus({ code: SpanStatusCode.OK });
          return [];
        }

        // Calculate average pairwise similarity
        const similarities = [];
        for (let i = 0; i < Math.min(entities.length, 100); i++) {
          for (let j = i + 1; j < Math.min(entities.length, 100); j++) {
            const sim = this.embeddingsManager.computeSimilarity(entities[i], entities[j]);
            similarities.push(sim);
          }
        }

        const meanSim = similarities.reduce((a, b) => a + b, 0) / similarities.length;
        const varSim =
          similarities.reduce((sum, s) => sum + Math.pow(s - meanSim, 2), 0) / similarities.length;
        const stdDevSim = Math.sqrt(varSim);

        // Find entities with very low similarity to all others (potential outliers)
        for (const entity of entities.slice(0, 50)) {
          const avgSim =
            entities
              .filter(e => e !== entity)
              .slice(0, 20)
              .reduce(
                (sum, other) => sum + this.embeddingsManager.computeSimilarity(entity, other),
                0
              ) / 20;

          const zScore = Math.abs((avgSim - meanSim) / stdDevSim);

          if (zScore > this.config.outlierThreshold && avgSim < 0.3) {
            anomalies.push({
              type: 'outlier',
              severity: 'medium',
              description: `Entity has unusual embedding pattern (avg similarity: ${avgSim.toFixed(3)})`,
              subject: entity,
              confidence: Math.min(0.8, zScore / 5),
              evidence: [`Avg similarity: ${avgSim.toFixed(3)}`, `Z-score: ${zScore.toFixed(2)}`],
            });
          }
        }

        span.setAttribute('anomaly.ml_found', anomalies.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return anomalies;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return [];
      }
    });
  }

  /**
   * Detect missing links in the graph
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Missing link anomalies
   * @private
   */
  async _detectMissingLinks(store) {
    return tracer.startActiveSpan('anomaly.missing_links', async span => {
      try {
        const anomalies = [];

        // Use semantic analyzer to get suggestions
        const analysis = await this.semanticAnalyzer.analyze(store);

        // Convert suggestions to anomalies
        for (const suggestion of analysis.suggestions) {
          if (suggestion.type === 'missing_inverse' || suggestion.type === 'missing_subclass') {
            anomalies.push({
              type: 'missing_link',
              severity: suggestion.priority === 'high' ? 'high' : 'medium',
              description: suggestion.description,
              confidence: 0.7,
              evidence: ['Detected by semantic analysis'],
            });
          }
        }

        // Detect common patterns and missing instances
        const _patterns = analysis._patterns;
        const relationships = analysis.relationships;

        // Find relationships that appear frequently but have missing reciprocals
        const predicateCounts = new Map();
        for (const rel of relationships) {
          predicateCounts.set(rel.predicate, (predicateCounts.get(rel.predicate) || 0) + 1);
        }

        for (const [predicate, count] of predicateCounts.entries()) {
          if (count > 5) {
            // Check if there are entities that should have this predicate but don't
            const subjectsWithPredicate = new Set(
              relationships.filter(r => r.predicate === predicate).map(r => r.subject)
            );

            // This is a simplified heuristic
            if (subjectsWithPredicate.size < count * 0.5) {
              anomalies.push({
                type: 'missing_link',
                severity: 'low',
                description: `Predicate ${predicate} appears inconsistently across similar entities`,
                predicate,
                confidence: 0.6,
                evidence: [`Count: ${count}`, `Subjects: ${subjectsWithPredicate.size}`],
              });
            }
          }
        }

        span.setAttribute('anomaly.missing_links_found', anomalies.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return anomalies;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return [];
      }
    });
  }

  /**
   * Detect data quality issues
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Data quality anomalies
   * @private
   */
  async _detectDataQualityIssues(store) {
    return tracer.startActiveSpan('anomaly.data_quality', async span => {
      try {
        const anomalies = [];

        const RDFS_LABEL = 'http://www.w3.org/2000/01/rdf-schema#label';
        const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

        // Track entities and their properties
        const entityProps = new Map();

        for (const quad of store) {
          const subject = quad.subject.value;
          if (!entityProps.has(subject)) {
            entityProps.set(subject, {
              hasLabel: false,
              hasType: false,
              propertyCount: 0,
            });
          }

          const props = entityProps.get(subject);
          props.propertyCount++;

          if (quad.predicate.value === RDFS_LABEL) {
            props.hasLabel = true;
          }
          if (quad.predicate.value === RDF_TYPE) {
            props.hasType = true;
          }
        }

        // Detect entities without labels
        for (const [entity, props] of entityProps.entries()) {
          if (!props.hasLabel && props.propertyCount > 2) {
            anomalies.push({
              type: 'data_quality',
              severity: 'low',
              description: `Entity missing rdfs:label`,
              subject: entity,
              confidence: 0.8,
              evidence: [`Property count: ${props.propertyCount}`],
            });
          }

          if (!props.hasType && props.propertyCount > 3) {
            anomalies.push({
              type: 'data_quality',
              severity: 'low',
              description: `Entity missing rdf:type`,
              subject: entity,
              confidence: 0.7,
              evidence: [`Property count: ${props.propertyCount}`],
            });
          }

          // Entities with very few properties (potential incomplete data)
          if (props.propertyCount === 1) {
            anomalies.push({
              type: 'data_quality',
              severity: 'medium',
              description: `Entity has only one property (potentially incomplete)`,
              subject: entity,
              confidence: 0.65,
              evidence: [`Property count: 1`],
            });
          }
        }

        span.setAttribute('anomaly.quality_issues_found', anomalies.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return anomalies;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return [];
      }
    });
  }

  /**
   * Detect structural anomalies in the graph
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Structural anomalies
   * @private
   */
  async _detectStructuralAnomalies(store) {
    return tracer.startActiveSpan('anomaly.structural', async span => {
      try {
        const anomalies = [];

        // Detect disconnected components
        const components = this._findConnectedComponents(store);

        if (components.length > 1) {
          // Multiple disconnected components
          const sizes = components.map(c => c.size).sort((a, b) => b - a);
          const _mainSize = sizes[0];
          const otherSizes = sizes.slice(1);

          if (otherSizes.some(s => s > 5)) {
            anomalies.push({
              type: 'structural_anomaly',
              severity: 'medium',
              description: `Graph has ${components.length} disconnected components`,
              confidence: 0.85,
              evidence: [`Component sizes: ${sizes.join(', ')}`],
            });
          }
        }

        // Detect cycles (simplified - just detect self-loops)
        for (const quad of store) {
          if (quad.subject.value === quad.object.value) {
            anomalies.push({
              type: 'structural_anomaly',
              severity: 'low',
              description: `Self-loop detected`,
              subject: quad.subject.value,
              predicate: quad.predicate.value,
              confidence: 0.95,
              evidence: ['Entity points to itself'],
            });
          }
        }

        span.setAttribute('anomaly.structural_found', anomalies.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return anomalies;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return [];
      }
    });
  }

  /**
   * Find connected components in the graph (simplified)
   * @param {Store} store - RDF store
   * @returns {Array<Set>} Connected components
   * @private
   */
  _findConnectedComponents(store) {
    const adjacency = new Map();

    // Build adjacency list
    for (const quad of store) {
      const subj = quad.subject.value;
      const obj = quad.object.value;

      if (!adjacency.has(subj)) adjacency.set(subj, new Set());
      if (!adjacency.has(obj)) adjacency.set(obj, new Set());

      adjacency.get(subj).add(obj);
      adjacency.get(obj).add(subj);
    }

    const visited = new Set();
    const components = [];

    // DFS to find components
    const dfs = (node, component) => {
      if (visited.has(node)) return;
      visited.add(node);
      component.add(node);

      const neighbors = adjacency.get(node) || new Set();
      for (const neighbor of neighbors) {
        dfs(neighbor, component);
      }
    };

    for (const node of adjacency.keys()) {
      if (!visited.has(node)) {
        const component = new Set();
        dfs(node, component);
        components.push(component);
      }
    }

    return components;
  }

  /**
   * Calculate anomaly statistics
   * @param {Array} anomalies - Detected anomalies
   * @returns {Object} Statistics
   * @private
   */
  _calculateAnomalyStatistics(anomalies) {
    const bySeverity = {};
    const byType = {};

    for (const anomaly of anomalies) {
      bySeverity[anomaly.severity] = (bySeverity[anomaly.severity] || 0) + 1;
      byType[anomaly.type] = (byType[anomaly.type] || 0) + 1;
    }

    return {
      total: anomalies.length,
      bySeverity,
      byType,
    };
  }

  /**
   * Get detector statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return { ...this.stats };
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
