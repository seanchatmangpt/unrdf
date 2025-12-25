/**
 * ML Model Version Store - RDF-based versioning with cryptographic proofs
 * Uses UNRDF KGC-4D for time-travel capabilities and BLAKE3 for hash chains
 */

import * as tf from '@tensorflow/tfjs-node';
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { blake3 } from 'hash-wasm';
import { z } from 'zod';

// Validation schemas
const ModelMetadataSchema = z.object({
  name: z.string(),
  version: z.string().optional(),
  description: z.string().optional(),
  architecture: z.string().optional(),
  framework: z.string().default('tensorflow.js'),
});

const TrainingMetricsSchema = z.object({
  loss: z.number().optional(),
  accuracy: z.number().optional(),
  epoch: z.number().optional(),
  batchSize: z.number().optional(),
  learningRate: z.number().optional(),
  custom: z.record(z.unknown()).optional(),
});

// RDF Namespaces
const ML_NS = 'http://ml-versioning.io/ontology#';
const ML_MODEL_NS = 'http://ml-versioning.io/model/';
const ML_VERSION_NS = 'http://ml-versioning.io/version/';

/**
 * ML Model Version Store
 * Provides cryptographic provenance tracking and time-travel debugging for ML models
 */
export class MLVersionStore {
  /**
   * @param {Object} options
   * @param {string} [options.nodeId] - Node ID for distributed versioning
   * @param {string} [options.storageDir] - Directory for model artifacts
   */
  constructor(options = {}) {
    this.store = new KGCStore({ nodeId: options.nodeId });
    this.storageDir = options.storageDir || '.ml-models';
    this.hashChain = new Map(); // modelId -> latest hash
  }

  /**
   * Save a new model version with cryptographic proof
   * @param {tf.LayersModel} model - TensorFlow.js model
   * @param {Object} metadata - Model metadata
   * @param {Object} metrics - Training metrics
   * @returns {Promise<Object>} Version receipt with hash proof
   *
   * @example
   * const model = tf.sequential({
   *   layers: [
   *     tf.layers.dense({ units: 10, inputShape: [8], activation: 'relu' }),
   *     tf.layers.dense({ units: 2, activation: 'softmax' })
   *   ]
   * });
   * const receipt = await versionStore.saveVersion(model,
   *   { name: 'iris-classifier', version: 'v1.0' },
   *   { accuracy: 0.95, loss: 0.15 }
   * );
   */
  async saveVersion(model, metadata, metrics = {}) {
    // Validate inputs
    const validatedMetadata = ModelMetadataSchema.parse(metadata);
    const validatedMetrics = TrainingMetricsSchema.parse(metrics);

    // Generate version ID
    const versionId = this._generateVersionId();
    const modelId = validatedMetadata.name;
    const timestamp = Date.now();

    // Serialize model to JSON
    const modelJson = await this._serializeModel(model);

    // Get previous hash for chain (if exists)
    const previousHash = this.hashChain.get(modelId) || null;

    // Compute BLAKE3 hash of model + metadata + previous hash
    const currentHash = await this._computeHash(
      modelJson,
      validatedMetadata,
      validatedMetrics,
      previousHash
    );

    // Update hash chain
    this.hashChain.set(modelId, currentHash);

    // Create RDF triples for model version
    const deltas = this._createVersionTriples(
      versionId,
      modelId,
      modelJson,
      validatedMetadata,
      validatedMetrics,
      currentHash,
      previousHash,
      timestamp
    );

    // Store in KGC-4D with event logging
    const receipt = await this.store.appendEvent(
      {
        type: 'MODEL_VERSION_CREATED',
        payload: {
          versionId,
          modelId,
          hash: currentHash,
          previousHash,
          metadata: validatedMetadata,
          metrics: validatedMetrics,
        },
      },
      deltas
    );

    return {
      versionId,
      modelId,
      hash: currentHash,
      previousHash,
      timestamp,
      receipt: receipt.receipt,
    };
  }

  /**
   * Load a specific model version by ID
   * @param {string} versionId - Version identifier
   * @returns {Promise<Object>} Model and metadata
   */
  async loadVersion(versionId) {
    const query = `
      PREFIX ml: <${ML_NS}>
      PREFIX mlv: <${ML_VERSION_NS}>

      SELECT ?modelJson ?metadata ?metrics ?hash ?previousHash ?timestamp
      WHERE {
        mlv:${versionId} ml:modelJson ?modelJson ;
                        ml:metadata ?metadata ;
                        ml:metrics ?metrics ;
                        ml:hash ?hash ;
                        ml:timestamp ?timestamp .
        OPTIONAL { mlv:${versionId} ml:previousHash ?previousHash }
      }
    `;

    const results = await this.store.query(query);

    if (results.length === 0) {
      throw new Error(`Version ${versionId} not found`);
    }

    const result = results[0];
    const modelJson = JSON.parse(result.modelJson.value);
    const metadata = JSON.parse(result.metadata.value);
    const metrics = JSON.parse(result.metrics.value);

    // Reconstruct TensorFlow.js model
    const model = await tf.models.modelFromJSON(modelJson);

    return {
      model,
      metadata,
      metrics,
      hash: result.hash.value,
      previousHash: result.previousHash?.value || null,
      timestamp: parseInt(result.timestamp.value),
      versionId,
    };
  }

  /**
   * Time-travel: Get model at specific timestamp
   * @param {string} modelId - Model identifier
   * @param {number} timestamp - Unix timestamp in milliseconds
   * @returns {Promise<Object>} Model version at timestamp
   */
  async getVersionAtTime(modelId, timestamp) {
    const query = `
      PREFIX ml: <${ML_NS}>
      PREFIX mlv: <${ML_VERSION_NS}>
      PREFIX kgc: <http://kgc.io/ontology#>

      SELECT ?versionId ?timestamp
      WHERE {
        ?version ml:modelId "${modelId}" ;
                 ml:timestamp ?timestamp ;
                 ml:versionId ?versionId .
        FILTER(?timestamp <= ${timestamp})
      }
      ORDER BY DESC(?timestamp)
      LIMIT 1
    `;

    const results = await this.store.query(query);

    if (results.length === 0) {
      throw new Error(`No version found for model ${modelId} at timestamp ${timestamp}`);
    }

    const versionId = results[0].versionId.value;
    return this.loadVersion(versionId);
  }

  /**
   * Get version history for a model
   * @param {string} modelId - Model identifier
   * @returns {Promise<Array>} Version history with proofs
   */
  async getVersionHistory(modelId) {
    const query = `
      PREFIX ml: <${ML_NS}>
      PREFIX mlv: <${ML_VERSION_NS}>

      SELECT ?versionId ?timestamp ?hash ?previousHash ?metrics
      WHERE {
        ?version ml:modelId "${modelId}" ;
                 ml:versionId ?versionId ;
                 ml:timestamp ?timestamp ;
                 ml:hash ?hash ;
                 ml:metrics ?metrics .
        OPTIONAL { ?version ml:previousHash ?previousHash }
      }
      ORDER BY ?timestamp
    `;

    const results = await this.store.query(query);

    return results.map(r => ({
      versionId: r.versionId.value,
      timestamp: parseInt(r.timestamp.value),
      hash: r.hash.value,
      previousHash: r.previousHash?.value || null,
      metrics: JSON.parse(r.metrics.value),
    }));
  }

  /**
   * Verify hash chain integrity
   * @param {string} modelId - Model identifier
   * @returns {Promise<Object>} Verification result
   */
  async verifyHashChain(modelId) {
    const history = await this.getVersionHistory(modelId);

    const verifications = [];
    for (let i = 0; i < history.length; i++) {
      const current = history[i];
      const previous = i > 0 ? history[i - 1] : null;

      const expectedPreviousHash = previous ? previous.hash : null;
      const valid = current.previousHash === expectedPreviousHash;

      verifications.push({
        versionId: current.versionId,
        valid,
        hash: current.hash,
        previousHash: current.previousHash,
        expectedPreviousHash,
      });
    }

    const allValid = verifications.every(v => v.valid);

    return {
      modelId,
      valid: allValid,
      verifications,
      totalVersions: history.length,
    };
  }

  /**
   * Compare two model versions
   * @param {string} versionId1 - First version
   * @param {string} versionId2 - Second version
   * @returns {Promise<Object>} Comparison results
   */
  async compareVersions(versionId1, versionId2) {
    const [version1, version2] = await Promise.all([
      this.loadVersion(versionId1),
      this.loadVersion(versionId2),
    ]);

    return {
      version1: {
        id: versionId1,
        metadata: version1.metadata,
        metrics: version1.metrics,
        hash: version1.hash,
        timestamp: version1.timestamp,
      },
      version2: {
        id: versionId2,
        metadata: version2.metadata,
        metrics: version2.metrics,
        hash: version2.hash,
        timestamp: version2.timestamp,
      },
      metricsDelta: this._computeMetricsDelta(version1.metrics, version2.metrics),
      timestampDelta: version2.timestamp - version1.timestamp,
    };
  }

  // ===== Private Methods =====

  /**
   * Serialize TensorFlow.js model to JSON
   */
  async _serializeModel(model) {
    return await model.toJSON();
  }

  /**
   * Compute BLAKE3 hash for provenance chain
   */
  async _computeHash(modelJson, metadata, metrics, previousHash) {
    const data = JSON.stringify({
      model: modelJson,
      metadata,
      metrics,
      previousHash,
    });

    return await blake3(data);
  }

  /**
   * Generate unique version ID
   */
  _generateVersionId() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return crypto.randomUUID();
    }
    try {
      const crypto = require('crypto');
      return crypto.randomUUID();
    } catch {
      return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    }
  }

  /**
   * Create RDF triples for version storage
   */
  _createVersionTriples(
    versionId,
    modelId,
    modelJson,
    metadata,
    metrics,
    hash,
    previousHash,
    timestamp
  ) {
    const versionNode = dataFactory.namedNode(`${ML_VERSION_NS}${versionId}`);
    const deltas = [];

    // Helper to add triple
    const addTriple = (predicate, object) => {
      deltas.push({
        type: 'add',
        subject: versionNode,
        predicate: dataFactory.namedNode(`${ML_NS}${predicate}`),
        object,
      });
    };

    // Core version metadata
    addTriple('versionId', dataFactory.literal(versionId));
    addTriple('modelId', dataFactory.literal(modelId));
    addTriple('timestamp', dataFactory.literal(timestamp.toString()));
    addTriple('hash', dataFactory.literal(hash));

    if (previousHash) {
      addTriple('previousHash', dataFactory.literal(previousHash));
    }

    // Serialize complex objects as JSON literals
    addTriple('modelJson', dataFactory.literal(JSON.stringify(modelJson)));
    addTriple('metadata', dataFactory.literal(JSON.stringify(metadata)));
    addTriple('metrics', dataFactory.literal(JSON.stringify(metrics)));

    // RDF type
    deltas.push({
      type: 'add',
      subject: versionNode,
      predicate: dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: dataFactory.namedNode(`${ML_NS}ModelVersion`),
    });

    return deltas;
  }

  /**
   * Compute delta between metrics
   */
  _computeMetricsDelta(metrics1, metrics2) {
    const delta = {};
    const allKeys = new Set([...Object.keys(metrics1), ...Object.keys(metrics2)]);

    for (const key of allKeys) {
      const val1 = metrics1[key];
      const val2 = metrics2[key];

      if (typeof val1 === 'number' && typeof val2 === 'number') {
        delta[key] = {
          from: val1,
          to: val2,
          change: val2 - val1,
          percentChange: val1 !== 0 ? ((val2 - val1) / val1) * 100 : null,
        };
      }
    }

    return delta;
  }
}
