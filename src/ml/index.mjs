/**
 * @file index.mjs
 * @description ML module for KGC-SWARM - Pattern recognition, Q-learning, and anomaly detection
 *
 * Features:
 * - Pattern recognition for observables O_τ
 * - Q-learning for agent action selection
 * - Anomaly detection for drift patterns
 * - Native JavaScript implementation (no TensorFlow.js)
 * - Optimized with typed arrays
 */

export {
  PatternRecognizer,
  createPatternRecognizer,
  ObservablePatternSchema,
  PatternResultSchema,
} from './pattern-recognition.mjs';

export {
  QLearningAgent,
  createQLearningAgent,
  calculateReward,
  StateSchema,
  ActionSchema,
  ExperienceSchema,
} from './q-learning.mjs';

export {
  AnomalyDetector,
  createAnomalyDetector,
  DriftObservationSchema,
  AnomalySchema,
} from './anomaly-detection.mjs';
