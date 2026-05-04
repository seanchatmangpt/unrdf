/**
 * Drift Detector - Discovery/Convergence Detection
 *
 * Δ (discovery / drift):
 *   A_t = μ(O_{≤t})
 *   Δ_t = A_t ⊖ A_{t-1}
 *   drift(A_t) = |Δ_t|
 *   stop ⇔ argmin drift(A_t) s.t. budget(B)
 *
 * @module @unrdf/kgc-claude/drift-detector
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Artifact snapshot schema
 */
export const ArtifactSnapshotSchema = z.object({
  epoch: z.number().int(),
  hash: z.string(),
  size: z.number().int(),
  observations: z.number().int(),
  deltas: z.number().int(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof ArtifactSnapshotSchema>} ArtifactSnapshot
 */

/**
 * Drift measurement schema
 */
export const DriftMeasurementSchema = z.object({
  epoch: z.number().int(),
  drift: z.number().min(0).max(1),
  delta_observations: z.number().int(),
  delta_deltas: z.number().int(),
  delta_size: z.number().int(),
  hash_changed: z.boolean(),
  convergence_rate: z.number(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof DriftMeasurementSchema>} DriftMeasurement
 */

/**
 * Convergence state schema
 */
export const ConvergenceStateSchema = z.object({
  converged: z.boolean(),
  epochs: z.number().int(),
  final_drift: z.number(),
  drift_history: z.array(z.number()),
  reason: z.enum(['epsilon', 'budget', 'max_epochs', 'stalled']),
});

/**
 * @typedef {z.infer<typeof ConvergenceStateSchema>} ConvergenceState
 */

/**
 * Drift detector for monitoring swarm convergence
 */
export class DriftDetector {
  /**
   * @param {Object} [options]
   * @param {number} [options.epsilon] - Convergence threshold
   * @param {number} [options.minEpochs] - Minimum epochs before convergence
   * @param {number} [options.maxEpochs] - Maximum epochs
   * @param {number} [options.stallThreshold] - Epochs with no change before stall
   */
  constructor(options = {}) {
    this.epsilon = options.epsilon ?? 0.01;
    this.minEpochs = options.minEpochs ?? 3;
    this.maxEpochs = options.maxEpochs ?? 100;
    this.stallThreshold = options.stallThreshold ?? 5;

    /** @type {ArtifactSnapshot[]} */
    this.snapshots = [];
    /** @type {DriftMeasurement[]} */
    this.measurements = [];
    this.stallCount = 0;
  }

  /**
   * Take snapshot of artifact
   * A_t = μ(O_{≤t})
   * @param {Object} artifact
   * @param {number} epoch
   * @returns {Promise<ArtifactSnapshot>}
   */
  async snapshot(artifact, epoch) {
    const serialized = JSON.stringify(artifact);
    const hash = await blake3(serialized);

    const snapshot = ArtifactSnapshotSchema.parse({
      epoch,
      hash,
      size: serialized.length,
      observations: artifact.observations?.length ?? 0,
      deltas: artifact.merged_deltas ?? 0,
      t_ns: now(),
    });

    this.snapshots.push(snapshot);
    return snapshot;
  }

  /**
   * Compute drift between current and previous snapshot
   * Δ_t = A_t ⊖ A_{t-1}
   * drift(A_t) = |Δ_t|
   * @returns {DriftMeasurement | null}
   */
  computeDrift() {
    if (this.snapshots.length < 2) return null;

    const current = this.snapshots[this.snapshots.length - 1];
    const previous = this.snapshots[this.snapshots.length - 2];

    const deltaObservations = Math.abs(current.observations - previous.observations);
    const deltaDeltas = Math.abs(current.deltas - previous.deltas);
    const deltaSize = Math.abs(current.size - previous.size);
    const hashChanged = current.hash !== previous.hash;

    // Normalize drift to [0, 1]
    const maxObsDiff = Math.max(current.observations, previous.observations, 1);
    const maxDeltaDiff = Math.max(current.deltas, previous.deltas, 1);
    const maxSizeDiff = Math.max(current.size, previous.size, 1);

    const normObsDrift = deltaObservations / maxObsDiff;
    const normDeltaDrift = deltaDeltas / maxDeltaDiff;
    const normSizeDrift = deltaSize / maxSizeDiff;

    // Combined drift metric
    const drift = hashChanged
      ? (normObsDrift + normDeltaDrift + normSizeDrift) / 3
      : 0;

    // Convergence rate (derivative of drift)
    const prevMeasurement = this.measurements[this.measurements.length - 1];
    const convergenceRate = prevMeasurement
      ? prevMeasurement.drift - drift
      : 0;

    // Track stalls
    if (drift === 0) {
      this.stallCount++;
    } else {
      this.stallCount = 0;
    }

    const measurement = DriftMeasurementSchema.parse({
      epoch: current.epoch,
      drift,
      delta_observations: deltaObservations,
      delta_deltas: deltaDeltas,
      delta_size: deltaSize,
      hash_changed: hashChanged,
      convergence_rate: convergenceRate,
      t_ns: now(),
    });

    this.measurements.push(measurement);
    return measurement;
  }

  /**
   * Check if converged
   * stop ⇔ argmin drift(A_t) s.t. budget(B)
   * @returns {ConvergenceState}
   */
  checkConvergence() {
    const epochs = this.snapshots.length;
    const driftHistory = this.measurements.map(m => m.drift);

    if (epochs < this.minEpochs) {
      return ConvergenceStateSchema.parse({
        converged: false,
        epochs,
        final_drift: driftHistory[driftHistory.length - 1] ?? 1,
        drift_history: driftHistory,
        reason: 'epsilon',
      });
    }

    const latestDrift = this.measurements.length > 0
      ? this.measurements[this.measurements.length - 1].drift
      : 1;

    // Check epsilon convergence
    if (latestDrift <= this.epsilon) {
      return ConvergenceStateSchema.parse({
        converged: true,
        epochs,
        final_drift: latestDrift,
        drift_history: driftHistory,
        reason: 'epsilon',
      });
    }

    // Check max epochs
    if (epochs >= this.maxEpochs) {
      return ConvergenceStateSchema.parse({
        converged: true,
        epochs,
        final_drift: latestDrift,
        drift_history: driftHistory,
        reason: 'max_epochs',
      });
    }

    // Check stall
    if (this.stallCount >= this.stallThreshold) {
      return ConvergenceStateSchema.parse({
        converged: true,
        epochs,
        final_drift: latestDrift,
        drift_history: driftHistory,
        reason: 'stalled',
      });
    }

    return ConvergenceStateSchema.parse({
      converged: false,
      epochs,
      final_drift: latestDrift,
      drift_history: driftHistory,
      reason: 'epsilon',
    });
  }

  /**
   * Get moving average of drift
   * @param {number} window
   * @returns {number}
   */
  movingAverage(window = 3) {
    if (this.measurements.length === 0) return 1;

    const recent = this.measurements.slice(-window);
    const sum = recent.reduce((acc, m) => acc + m.drift, 0);
    return sum / recent.length;
  }

  /**
   * Predict epochs until convergence
   * Based on exponential decay model
   * @returns {number}
   */
  predictEpochsToConvergence() {
    if (this.measurements.length < 2) return this.maxEpochs;

    const current = this.measurements[this.measurements.length - 1];

    if (current.convergence_rate <= 0) {
      return this.maxEpochs; // Not converging
    }

    // Linear extrapolation
    const epochsNeeded = Math.ceil(current.drift / current.convergence_rate);
    return Math.min(epochsNeeded, this.maxEpochs - this.snapshots.length);
  }

  /**
   * Get all measurements
   * @returns {DriftMeasurement[]}
   */
  getMeasurements() {
    return [...this.measurements];
  }

  /**
   * Get latest drift value
   * @returns {number}
   */
  getLatestDrift() {
    return this.measurements.length > 0
      ? this.measurements[this.measurements.length - 1].drift
      : 1;
  }

  /**
   * Reset detector
   */
  reset() {
    this.snapshots = [];
    this.measurements = [];
    this.stallCount = 0;
  }
}

/**
 * Create drift detector with defaults
 * @param {Object} [options]
 * @returns {DriftDetector}
 */
export function createDriftDetector(options = {}) {
  return new DriftDetector(options);
}

export default DriftDetector;
