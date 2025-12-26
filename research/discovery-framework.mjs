#!/usr/bin/env node
/**
 * @file Capability Field Discovery Framework
 * @module research/discovery-framework
 *
 * @description
 * Systematic discovery of emergent capabilities through:
 * 1. Capability atomization (already completed)
 * 2. Composition graph (already built)
 * 3. Constraint-guided search (this module)
 * 4. Synergy measurement with receipts
 * 5. Executable research notebooks
 */

import { createHash } from 'node:crypto';
import { writeFile, mkdir } from 'node:fs/promises';
import { dirname } from 'node:path';

// ============================================================================
// Receipt Generation (Deterministic, Verifiable)
// ============================================================================

/**
 * Generate deterministic receipt for experiment
 * @param {string} experimentId - Unique experiment ID
 * @param {object} inputs - Experiment inputs
 * @param {object} outputs - Experiment results
 * @param {number} timestamp - Nanosecond timestamp
 * @returns {object} Receipt with hash
 */
export function generateReceipt(experimentId, inputs, outputs, timestamp) {
  const data = {
    experimentId,
    inputs: JSON.stringify(inputs, Object.keys(inputs).sort()),
    outputs: JSON.stringify(outputs, Object.keys(outputs).sort()),
    timestamp
  };

  const hash = createHash('sha256');
  hash.update(JSON.stringify(data, null, 2));
  const contentHash = hash.digest('hex');

  return {
    experimentId,
    timestamp,
    contentHash,
    inputs,
    outputs,
    metadata: {
      discoveredAt: new Date(Math.floor(timestamp / 1_000_000)).toISOString(),
      hashAlgorithm: 'sha256'
    }
  };
}

/**
 * Verify receipt integrity
 * @param {object} receipt - Receipt to verify
 * @returns {boolean} True if receipt is valid
 */
export function verifyReceipt(receipt) {
  const data = {
    experimentId: receipt.experimentId,
    inputs: JSON.stringify(receipt.inputs, Object.keys(receipt.inputs).sort()),
    outputs: JSON.stringify(receipt.outputs, Object.keys(receipt.outputs).sort()),
    timestamp: receipt.timestamp
  };

  const hash = createHash('sha256');
  hash.update(JSON.stringify(data, null, 2));
  const expectedHash = hash.digest('hex');

  return receipt.contentHash === expectedHash;
}

// ============================================================================
// Synergy Measurement (Δ = Composite - Baseline)
// ============================================================================

/**
 * Baseline measurement for a single atom/package
 * @param {string} atomName - Atom name (e.g., '@unrdf/core')
 * @param {object} metrics - Metrics to measure (latency, throughput, determinism, safety)
 * @returns {object} Baseline score and metrics
 */
export function measureBaseline(atomName, metrics = {}) {
  return {
    atomName,
    score: calculateScore(metrics),
    metrics: {
      latency_ms: metrics.latency_ms ?? 0,
      throughput_ops_sec: metrics.throughput_ops_sec ?? 0,
      determinism_pct: metrics.determinism_pct ?? 100,
      poka_yoke_boundaries: metrics.poka_yoke_boundaries ?? 0,
      safety_invariants: metrics.safety_invariants ?? 0
    },
    timestamp: BigInt(Date.now() * 1_000_000) // Nanoseconds
  };
}

/**
 * Composition measurement for multiple atoms combined
 * @param {string} compositionId - Composition ID
 * @param {string[]} atoms - List of atom names
 * @param {object} metrics - Measured metrics
 * @returns {object} Composition score and metrics
 */
export function measureComposition(compositionId, atoms, metrics = {}) {
  return {
    compositionId,
    atoms,
    score: calculateScore(metrics),
    metrics: {
      latency_ms: metrics.latency_ms ?? 0,
      throughput_ops_sec: metrics.throughput_ops_sec ?? 0,
      determinism_pct: metrics.determinism_pct ?? 100,
      poka_yoke_boundaries: metrics.poka_yoke_boundaries ?? 0,
      safety_invariants: metrics.safety_invariants ?? 0
    },
    timestamp: BigInt(Date.now() * 1_000_000) // Nanoseconds
  };
}

/**
 * Calculate synergy delta (Composite - Baseline)
 * @param {object} baseline - Baseline measurement
 * @param {object} composite - Composition measurement
 * @returns {object} Synergy analysis with delta
 */
export function calculateSynergy(baseline, composite) {
  // Normalize baseline scores (if measuring multiple atoms, take average)
  const baselineScore = Array.isArray(baseline)
    ? baseline.reduce((sum, b) => sum + b.score, 0) / baseline.length
    : baseline.score;

  const compositeScore = composite.score;
  const delta = compositeScore - baselineScore;
  const synergy_pct = (delta / Math.max(baselineScore, 1)) * 100;

  return {
    composition: composite.compositionId || composite.atomName,
    atoms: composite.atoms || [composite.atomName],
    baseline_score: baselineScore,
    composite_score: compositeScore,
    delta,
    synergy_pct,
    emergent: delta > 0,
    verdict: synergy_pct > 20 ? 'HIGH_SYNERGY' : synergy_pct > 0 ? 'POSITIVE' : 'NO_SYNERGY'
  };
}

/**
 * Score calculation (0-100 scale)
 * Lower latency = higher score
 * Higher throughput = higher score
 * Higher determinism = higher score
 * More safety invariants = higher score
 * @param {object} metrics - Metrics object
 * @returns {number} Score 0-100
 */
function calculateScore(metrics) {
  const latencyScore = Math.max(0, 100 - (metrics.latency_ms ?? 0) / 10); // Penalize latency
  const throughputScore = Math.min(100, (metrics.throughput_ops_sec ?? 0) / 100); // Reward throughput
  const determinismScore = metrics.determinism_pct ?? 100;
  const safetyScore = Math.min(100, (metrics.poka_yoke_boundaries ?? 0) * 10);

  return (latencyScore + throughputScore + determinismScore + safetyScore) / 4;
}

// ============================================================================
// Constraint-Guided Search
// ============================================================================

/**
 * Constraint set for valid compositions
 */
export const COMPOSITION_CONSTRAINTS = {
  DETERMINISM: { name: 'determinism', required: true, value: (m) => m.determinism_pct >= 95 },
  PROOF: { name: 'proof', required: true, value: (m) => m.has_receipt === true },
  POKA_YOKE: { name: 'poka_yoke', required: true, value: (m) => m.poka_yoke_boundaries > 0 },
  SLA: { name: 'sla', required: false, value: (m) => m.latency_ms <= 100 }
};

/**
 * Check if composition satisfies constraints
 * @param {object} composition - Composition measurement
 * @param {object} constraints - Constraint set
 * @returns {object} Constraint satisfaction report
 */
export function checkConstraints(composition, constraints = COMPOSITION_CONSTRAINTS) {
  const results = {};
  let allRequired = true;

  for (const [key, constraint] of Object.entries(constraints)) {
    results[key] = {
      name: constraint.name,
      satisfied: constraint.value(composition.metrics),
      required: constraint.required
    };

    if (constraint.required && !results[key].satisfied) {
      allRequired = false;
    }
  }

  return {
    composition: composition.compositionId,
    allConstraintsSatisfied: allRequired,
    details: results
  };
}

// ============================================================================
// Executable Research Notebooks
// ============================================================================

/**
 * Research notebook structure
 * Each notebook is a runnable experiment with inputs, logic, and receipt
 */
export class ResearchNotebook {
  constructor(experimentId, description, atoms) {
    this.experimentId = experimentId;
    this.description = description;
    this.atoms = atoms; // Array of atom names
    this.results = null;
    this.receipt = null;
  }

  /**
   * Run the experiment
   * Must be overridden in subclasses
   */
  async run() {
    throw new Error('Subclass must implement run()');
  }

  /**
   * Generate receipt after successful run
   */
  generateReceipt() {
    if (!this.results) {
      throw new Error('Cannot generate receipt: experiment not run yet');
    }

    this.receipt = generateReceipt(
      this.experimentId,
      { atoms: this.atoms, description: this.description },
      this.results,
      Number(BigInt(Date.now() * 1_000_000)) // Nanoseconds
    );

    return this.receipt;
  }

  /**
   * Export notebook as JSON
   */
  toJSON() {
    return {
      experimentId: this.experimentId,
      description: this.description,
      atoms: this.atoms,
      results: this.results,
      receipt: this.receipt
    };
  }
}

// ============================================================================
// Top Candidate Compositions (from composition-graph analysis)
// ============================================================================

/**
 * High-value compositions to test
 * Sorted by theoretical synergy (Δ)
 */
export const TOP_COMPOSITIONS = [
  {
    rank: 1,
    id: 'composition-1-yawl-kgc4d-blockchain',
    atoms: ['@unrdf/yawl', '@unrdf/kgc-4d', '@unrdf/blockchain'],
    signal: 'Verifiable time-travel workflows with cryptographic anchoring',
    theoretical_delta: 142,
    constraints_required: ['determinism', 'proof', 'poka_yoke', 'sla']
  },
  {
    rank: 2,
    id: 'composition-2-yawl-kgc4d',
    atoms: ['@unrdf/yawl', '@unrdf/kgc-4d'],
    signal: 'Low-latency provable state machines with event sourcing',
    theoretical_delta: 98,
    constraints_required: ['determinism', 'proof', 'poka_yoke']
  },
  {
    rank: 3,
    id: 'composition-3-consensus-federation-kgc4d',
    atoms: ['@unrdf/consensus', '@unrdf/federation', '@unrdf/kgc-4d'],
    signal: 'Distributed deterministic consensus with time-travel',
    theoretical_delta: 87,
    constraints_required: ['determinism', 'proof']
  },
  {
    rank: 4,
    id: 'composition-4-dark-matter-semantic-core',
    atoms: ['@unrdf/dark-matter', '@unrdf/semantic-search', '@unrdf/core'],
    signal: 'AI-powered semantic query optimization',
    theoretical_delta: 76,
    constraints_required: ['proof']
  },
  {
    rank: 5,
    id: 'composition-5-streaming-composables-core',
    atoms: ['@unrdf/streaming', '@unrdf/composables', '@unrdf/core'],
    signal: 'Zero-downtime reactive knowledge graph sync',
    theoretical_delta: 64,
    constraints_required: ['determinism']
  }
];

