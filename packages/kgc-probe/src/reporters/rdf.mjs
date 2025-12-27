/**
 * @fileoverview RDF/Turtle converter for KGC Probe observations
 *
 * Converts observations to RDF/Turtle format with capability and constraint derivation.
 * Uses the observation schema from orchestrator (category, severity, message format).
 *
 * Design principles:
 * - Deterministic: Same observations → same Turtle output
 * - Provenance: Links capabilities/constraints to observation hashes
 * - Minimal vocabulary: kgc:Observation, kgc:Capability, kgc:Constraint
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import crypto from 'crypto';

// ===== RDF Vocabulary Constants =====

const KGC_NS = 'https://unrdf.org/kgc/probe#';
const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';
const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const RDFS_NS = 'http://www.w3.org/2000/01/rdf-schema#';

/**
 * Create a KGC namespace URI
 *
 * @param {string} localName - Local name in KGC namespace
 * @returns {string} Full URI
 */
function kgcUri(localName) {
  return `${KGC_NS}${localName}`;
}

/**
 * Generate deterministic hash for observation
 *
 * @param {Object} observation - Observation object
 * @returns {string} SHA-256 hash (first 16 chars)
 */
function generateHash(observation) {
  if (observation.hash) return observation.hash;
  if (observation.receiptHash) return observation.receiptHash;

  const content = JSON.stringify({
    method: observation.method,
    category: observation.category,
    message: observation.message,
    outputs: observation.outputs || observation.data,
    timestamp: observation.timestamp || observation.metadata?.timestamp,
  });

  return crypto.createHash('sha256').update(content).digest('hex').substring(0, 16);
}

/**
 * Derive capabilities from observations
 *
 * Capability = feature/resource available in the environment
 * Derives when:
 * - observation.outputs.available === true
 * - observation.data indicates successful feature detection
 * - No error present
 *
 * @param {Array<Object>} observations - Array of observations
 * @returns {Array<Object>} Capabilities with provenance
 */
function deriveCapabilities(observations) {
  const capabilities = [];

  // Group by domain/method for analysis
  for (const obs of observations) {
    const hash = generateHash(obs);
    const data = obs.outputs || obs.data || {};

    // Check for explicit availability flag
    if (data.available === true) {
      const domain = obs.domain || obs.category || 'unknown';
      const method = obs.method || obs.message || 'unknown';

      capabilities.push({
        name: `${domain}.${method}`,
        available: true,
        derivedFrom: [`urn:kgc:obs:${hash}`],
        data
      });
    }

    // Check for specific capability indicators
    if (data.worker_threads === true || data.workerThreads === true) {
      capabilities.push({
        name: 'concurrency.worker_threads',
        available: true,
        derivedFrom: [`urn:kgc:obs:${hash}`],
        data: { module: 'worker_threads' }
      });
    }

    if (data.wasm === true || data.WebAssembly === true) {
      capabilities.push({
        name: 'runtime.wasm',
        available: true,
        derivedFrom: [`urn:kgc:obs:${hash}`],
        data: { support: 'available' }
      });
    }
  }

  return capabilities;
}

/**
 * Derive constraints from observations
 *
 * Constraint = limitation/boundary discovered in the environment
 * Derives when:
 * - observation shows guard denial
 * - observation shows error/limit
 * - observation shows restricted access
 *
 * @param {Array<Object>} observations - Array of observations
 * @returns {Array<Object>} Constraints with provenance
 */
function deriveConstraints(observations) {
  const constraints = [];

  for (const obs of observations) {
    const hash = generateHash(obs);
    const data = obs.outputs || obs.data || {};

    // Check for guard denials
    if (obs.guardDecision === 'denied' || obs.category === 'guard' || data.guardDecision === 'denied') {
      constraints.push({
        type: 'guard-denial',
        description: obs.message || 'Operation denied by guard',
        derivedFrom: [`urn:kgc:obs:${hash}`],
        data: { guard: data.guardName || 'unknown' }
      });
    }

    // Check for errors indicating limits
    if (obs.error || data.error) {
      constraints.push({
        type: 'error-boundary',
        description: obs.error || data.error || 'Error encountered',
        derivedFrom: [`urn:kgc:obs:${hash}`],
        data: { errorType: data.errorType || 'unknown' }
      });
    }

    // Check for explicit limits
    if (data.maxMemory !== undefined) {
      constraints.push({
        type: 'memory-limit',
        description: `Maximum memory: ${data.maxMemory}`,
        derivedFrom: [`urn:kgc:obs:${hash}`],
        data: { limit: data.maxMemory }
      });
    }

    if (data.maxStackDepth !== undefined) {
      constraints.push({
        type: 'stack-depth-limit',
        description: `Maximum stack depth: ${data.maxStackDepth}`,
        derivedFrom: [`urn:kgc:obs:${hash}`],
        data: { limit: data.maxStackDepth }
      });
    }
  }

  return constraints;
}

/**
 * Convert observations to RDF/Turtle format
 *
 * Generates Turtle with:
 * - kgc:Observation for each observation
 * - kgc:Capability for derived capabilities
 * - kgc:Constraint for derived constraints
 * - Provenance links via kgc:derivedFrom
 *
 * @param {Array<Object>} observations - Array of observation objects
 * @returns {Promise<string>} Turtle-formatted RDF string
 *
 * @example
 * const obs = [{ method: 'runtime.node-version', outputs: { version: 'v22.21.1' } }];
 * const turtle = await convertToTurtle(obs);
 * console.log(turtle); // @prefix kgc: <https://unrdf.org/kgc/probe#> . ...
 */
export async function convertToTurtle(observations) {
  // Create RDF store
  const store = createStore();

  // Sort observations by hash for deterministic output
  const sortedObs = [...observations].sort((a, b) => {
    const hashA = generateHash(a);
    const hashB = generateHash(b);
    return hashA.localeCompare(hashB);
  });

  // Add observations as RDF quads
  for (const obs of sortedObs) {
    const hash = generateHash(obs);
    const obsUri = dataFactory.namedNode(`urn:kgc:obs:${hash}`);

    // Type declaration
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(`${RDF_NS}type`),
      dataFactory.namedNode(kgcUri('Observation'))
    ));

    // Domain (category or inferred from method)
    const domain = obs.domain || obs.category || 'unknown';
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('domain')),
      dataFactory.literal(domain)
    ));

    // Method (from method field or message)
    const method = obs.method || obs.message || 'unknown';
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('method')),
      dataFactory.literal(method)
    ));

    // Timestamp
    const timestamp = obs.timestamp || obs.metadata?.timestamp || Date.now();
    const timestampDate = typeof timestamp === 'number'
      ? new Date(timestamp).toISOString()
      : timestamp;

    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('timestamp')),
      dataFactory.literal(timestampDate, dataFactory.namedNode(`${XSD_NS}dateTime`))
    ));

    // Hash
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('hash')),
      dataFactory.literal(hash)
    ));

    // Outputs (serialize data/outputs as JSON)
    const outputs = obs.outputs || obs.data || {};
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('outputs')),
      dataFactory.literal(JSON.stringify(outputs), dataFactory.namedNode(`${RDF_NS}JSON`))
    ));

    // Optional: Guard decision
    if (obs.guardDecision) {
      store.add(dataFactory.quad(
        obsUri,
        dataFactory.namedNode(kgcUri('guardDecision')),
        dataFactory.literal(obs.guardDecision)
      ));
    }

    // Optional: Error
    if (obs.error) {
      store.add(dataFactory.quad(
        obsUri,
        dataFactory.namedNode(kgcUri('error')),
        dataFactory.literal(obs.error)
      ));
    }

    // Optional: Severity (if from orchestrator schema)
    if (obs.severity) {
      store.add(dataFactory.quad(
        obsUri,
        dataFactory.namedNode(kgcUri('severity')),
        dataFactory.literal(obs.severity)
      ));
    }
  }

  // Derive and add capabilities
  const capabilities = deriveCapabilities(sortedObs);
  for (let i = 0; i < capabilities.length; i++) {
    const cap = capabilities[i];
    const capUri = dataFactory.namedNode(`urn:kgc:cap:${i + 1}`);

    store.add(dataFactory.quad(
      capUri,
      dataFactory.namedNode(`${RDF_NS}type`),
      dataFactory.namedNode(kgcUri('Capability'))
    ));

    store.add(dataFactory.quad(
      capUri,
      dataFactory.namedNode(kgcUri('name')),
      dataFactory.literal(cap.name)
    ));

    store.add(dataFactory.quad(
      capUri,
      dataFactory.namedNode(kgcUri('available')),
      dataFactory.literal(String(cap.available), dataFactory.namedNode(`${XSD_NS}boolean`))
    ));

    // Link to observations
    for (const obsRef of cap.derivedFrom) {
      store.add(dataFactory.quad(
        capUri,
        dataFactory.namedNode(kgcUri('derivedFrom')),
        dataFactory.namedNode(obsRef)
      ));
    }
  }

  // Derive and add constraints
  const constraints = deriveConstraints(sortedObs);
  for (let i = 0; i < constraints.length; i++) {
    const constraint = constraints[i];
    const constraintUri = dataFactory.namedNode(`urn:kgc:constraint:${i + 1}`);

    store.add(dataFactory.quad(
      constraintUri,
      dataFactory.namedNode(`${RDF_NS}type`),
      dataFactory.namedNode(kgcUri('Constraint'))
    ));

    store.add(dataFactory.quad(
      constraintUri,
      dataFactory.namedNode(kgcUri('constraintType')),
      dataFactory.literal(constraint.type)
    ));

    store.add(dataFactory.quad(
      constraintUri,
      dataFactory.namedNode(kgcUri('description')),
      dataFactory.literal(constraint.description)
    ));

    // Link to observations
    for (const obsRef of constraint.derivedFrom) {
      store.add(dataFactory.quad(
        constraintUri,
        dataFactory.namedNode(kgcUri('derivedFrom')),
        dataFactory.namedNode(obsRef)
      ));
    }
  }

  // Serialize to Turtle using N3.js Writer
  // Note: Oxigraph dump() may not support 'turtle' format in all versions
  // So we extract quads and use N3.js Writer for compatibility
  const N3 = await import('n3');
  const writer = new N3.Writer({ format: 'Turtle' });

  // Get all quads from store
  const quads = store.match();

  // Add quads to writer
  for (const quad of quads) {
    writer.addQuad(quad);
  }

  // Return serialized Turtle
  return new Promise((resolve, reject) => {
    writer.end((error, result) => {
      if (error) reject(error);
      else resolve(result);
    });
  });
}

export {
  deriveCapabilities,
  deriveConstraints
};

export default {
  convertToTurtle,
  deriveCapabilities,
  deriveConstraints
};
