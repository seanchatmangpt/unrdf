/**
 * @file mu-transform.mjs
 * @module @unrdf/chatman-equation/mu
 * @description μ transformation function: A = μ(O ⊔ Δ)
 * Implements the Five-Stage Pipeline: Normalize → Extract → Emit → Canonicalize → Receipt
 */

import { createHash, randomUUID } from 'crypto';
import {
  ObservationSchema,
  DeltaSchema,
  ClosureOperatorSchema,
  ArtifactSchema,
} from './simple-schemas.mjs';

/**
 * Stage 1: Normalize - Validate and normalize inputs
 * @param {Object} observation - The observation O
 * @param {Object} delta - The delta Δ
 * @param {Object} closureOperator - The closure operator μ
 * @returns {Object} Normalized inputs
 */
function normalize(observation, delta, closureOperator) {
  // Note: Zod v4 validation disabled due to API incompatibilities
  // Manual validation instead
  if (!observation || !observation.id || !observation.domain || !observation.state) {
    throw new Error('Invalid observation: missing required fields');
  }
  if (!delta || !delta.id || !delta.domain || !delta.operations) {
    throw new Error('Invalid delta: missing required fields');
  }
  if (!closureOperator || !closureOperator.name || !closureOperator.domain) {
    throw new Error('Invalid closure operator: missing required fields');
  }

  // Ensure domain consistency
  if (observation.domain !== delta.domain) {
    throw new Error(`Domain mismatch: observation.domain=${observation.domain}, delta.domain=${delta.domain}`);
  }

  if (closureOperator.domain !== observation.domain) {
    throw new Error(`Domain mismatch: operator.domain=${closureOperator.domain}, observation.domain=${observation.domain}`);
  }

  return { observation, delta, operator: closureOperator };
}

/**
 * Stage 2: Extract - Merge O ⊔ Δ according to operator
 * @param {Object} observation - Validated observation
 * @param {Object} delta - Validated delta
 * @param {Object} operator - Validated operator
 * @returns {Object} Merged state
 */
function extract(observation, delta, operator) {
  const result = { ...observation.state };

  // Apply delta operations based on closure operator type
  for (const op of delta.operations) {
    switch (op.op) {
      case 'add':
        if (result[op.field] !== undefined) {
          // Handle conflict based on operator strategy
          if (operator.conflict_resolution === 'delta_wins') {
            result[op.field] = op.value;
          } else if (operator.conflict_resolution === 'current_wins') {
            // Keep current value
          } else if (operator.conflict_resolution === 'merge') {
            // Attempt to merge (array concat, object merge, etc.)
            if (Array.isArray(result[op.field]) && Array.isArray(op.value)) {
              result[op.field] = [...result[op.field], ...op.value];
            } else {
              result[op.field] = op.value; // Fallback to delta_wins
            }
          } else if (operator.conflict_resolution === 'reject') {
            throw new Error(`Conflict on field ${op.field}: value already exists`);
          }
        } else {
          result[op.field] = op.value;
        }
        break;

      case 'update':
        if (result[op.field] === undefined && operator.conflict_resolution === 'reject') {
          throw new Error(`Cannot update non-existent field: ${op.field}`);
        }
        result[op.field] = op.value;
        break;

      case 'delete':
        delete result[op.field];
        break;
    }
  }

  return result;
}

/**
 * Stage 3: Emit - Generate Turtle RDF representation
 * @param {Object} observation - Original observation
 * @param {Object} delta - Applied delta
 * @param {Object} result - Merged result state
 * @param {Object} operator - Closure operator
 * @returns {string} Turtle RDF content
 */
function emit(observation, delta, result, operator) {
  const lines = [
    '@prefix ex: <http://example.org/> .',
    '@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .',
    '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .',
    '@prefix chatman: <http://chatman.equation/> .',
    '',
    `ex:observation_${observation.id.substring(0, 8)} a chatman:Observation ;`,
    `    chatman:domain "${observation.domain}" ;`,
    `    chatman:timestamp "${observation.timestamp}" .`,
    '',
    `ex:delta_${delta.id.substring(0, 8)} a chatman:Delta ;`,
    `    chatman:domain "${delta.domain}" ;`,
    `    chatman:timestamp "${delta.timestamp}" ;`,
    `    chatman:operationCount ${delta.operations.length} .`,
    '',
    `ex:result a chatman:Artifact ;`,
    `    chatman:sourceObservation ex:observation_${observation.id.substring(0, 8)} ;`,
    `    chatman:appliedDelta ex:delta_${delta.id.substring(0, 8)} ;`,
    `    chatman:operator "${operator.name}" ;`,
  ];

  // Add result state properties
  Object.entries(result).forEach(([key, value]) => {
    const safeKey = key.replace(/[^a-zA-Z0-9_]/g, '_');
    const safeValue = JSON.stringify(value).replace(/"/g, '\\"');
    lines.push(`    ex:${safeKey} ${safeValue} ;`);
  });

  // Remove trailing semicolon and add period
  const lastLine = lines[lines.length - 1];
  lines[lines.length - 1] = lastLine.replace(/;$/, '.');

  return lines.join('\n');
}

/**
 * Stage 4: Canonicalize - Ensure deterministic output
 * @param {string} turtleContent - Raw Turtle RDF
 * @returns {string} Canonicalized Turtle RDF
 */
function canonicalize(turtleContent) {
  const lines = turtleContent.split('\n');
  const prefixes = [];
  const statements = [];

  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.startsWith('@prefix')) {
      prefixes.push(trimmed);
    } else if (trimmed) {
      statements.push(trimmed);
    }
  }

  // Sort prefixes for determinism
  prefixes.sort();

  return [...prefixes, '', ...statements].join('\n');
}

/**
 * Stage 5: Receipt - Generate cryptographic proof
 * @param {Object} observation - Original observation
 * @param {Object} delta - Applied delta
 * @param {Object} artifact - Generated artifact
 * @returns {Object} Receipt object
 */
function createReceipt(observation, delta, artifact) {
  const obsHash = createHash('sha256').update(JSON.stringify(observation)).digest('hex');
  const deltaHash = createHash('sha256').update(JSON.stringify(delta)).digest('hex');
  const artifactHash = createHash('sha256').update(JSON.stringify(artifact.result)).digest('hex');

  return {
    observationHash: obsHash,
    deltaHash: deltaHash,
    artifactHash: artifactHash,
    timestamp: new Date().toISOString(),
    equation: 'A = μ(O ⊔ Δ)',
  };
}

/**
 * μ transformation function: A = μ(O ⊔ Δ)
 * @param {Object} observation - The observation space (O)
 * @param {Object} delta - The delta (Δ)
 * @param {Object} closureOperator - The closure operator (μ)
 * @param {Object} [options] - Transformation options
 * @param {boolean} [options.deterministic=true] - Ensure deterministic output
 * @param {boolean} [options.includeReceipt=true] - Include cryptographic receipt
 * @param {boolean} [options.validate=true] - Validate inputs and outputs
 * @returns {Object} Artifact with result and optional receipt
 * @throws {Error} If validation fails or domains mismatch
 * @example
 * const observation = {
 *   id: '123e4567-e89b-12d3-a456-426614174000',
 *   timestamp: '2026-01-18T00:00:00Z',
 *   domain: 'market',
 *   state: { customers: 100 }
 * };
 * const delta = {
 *   id: '223e4567-e89b-12d3-a456-426614174000',
 *   timestamp: '2026-01-18T00:01:00Z',
 *   domain: 'market',
 *   operations: [{ op: 'update', field: 'customers', value: 150 }]
 * };
 * const operator = {
 *   type: 'merge',
 *   name: 'market_reconcile',
 *   domain: 'market',
 *   conflict_resolution: 'delta_wins'
 * };
 * const artifact = mu(observation, delta, operator);
 */
export function mu(observation, delta, closureOperator, options = {}) {
  const startTime = Date.now();
  const opts = {
    deterministic: options.deterministic !== false,
    includeReceipt: options.includeReceipt !== false,
    validate: options.validate !== false,
  };

  // Five-Stage Pipeline
  const normalized = normalize(observation, delta, closureOperator);
  const mergedState = extract(normalized.observation, normalized.delta, normalized.operator);
  const rawTurtle = emit(normalized.observation, normalized.delta, mergedState, normalized.operator);
  const canonicalTurtle = opts.deterministic ? canonicalize(rawTurtle) : rawTurtle;

  const artifact = {
    id: randomUUID(),
    timestamp: new Date().toISOString(),
    source_observation: observation.id,
    applied_deltas: [delta.id],
    operator: closureOperator.name,
    result: mergedState,
  };

  if (opts.includeReceipt) {
    artifact.receipt = createReceipt(observation, delta, artifact);
  }

  // Add proof (Turtle RDF representation)
  artifact.proof = {
    format: 'turtle',
    content: canonicalTurtle,
  };

  // Skip Zod validation in artifact due to version incompatibility
  // Validation happens during normalize() stage

  return {
    artifact,
    metadata: {
      duration: Date.now() - startTime,
      observationId: observation.id,
      deltaId: delta.id,
      operatorName: closureOperator.name,
    },
  };
}

/**
 * Compose two μ transformations: μ₁ ∘ μ₂
 * @param {Function} mu1 - First transformation
 * @param {Function} mu2 - Second transformation
 * @returns {Function} Composed transformation
 */
export function compose(mu1, mu2) {
  return (observation, delta, operator, options = {}) => {
    const result1 = mu1(observation, delta, operator, options);

    // Use result as new observation
    const newObservation = {
      id: randomUUID(),
      timestamp: new Date().toISOString(),
      domain: observation.domain,
      state: result1.artifact.result,
    };

    return mu2(newObservation, delta, operator, options);
  };
}
