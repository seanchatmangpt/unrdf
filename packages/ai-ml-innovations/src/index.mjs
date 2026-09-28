/**
 * @file AI/ML Innovations for UNRDF
 * @module ai-ml-innovations
 *
 * @description
 * Novel AI/ML integration patterns for UNRDF knowledge graphs:
 * - Temporal Graph Neural Networks (TGNN)
 * - Neural-Symbolic Hybrid Reasoning
 * - Federated Knowledge Graph Embeddings
 *
 * Research findings: 15 high-impact patterns identified
 * Implementation status: 3 working prototypes
 */

// Temporal Graph Neural Network
export {
  TemporalGraphNeuralNetwork,
  createTemporalGNN,
  default as TGNN,
} from './temporal-gnn.mjs';

// Neural-Symbolic Reasoner
export {
  NeuralSymbolicReasoner,
  createNeuralSymbolicReasoner,
  default as NeuralSymbolic,
} from './neural-symbolic-reasoner.mjs';

// Federated Embeddings
export {
  FederatedEmbeddingTrainer,
  createFederatedEmbeddingTrainer,
  default as Federated,
} from './federated-embeddings.mjs';

// Federated Learning Core
export {
  FedAvgAggregator,
  createFedAvgAggregator,
} from './fedavg.mjs';

export {
  DPMechanism,
  createDPMechanism,
} from './dp-mechanism.mjs';

export {
  PrivacyBudgetTracker,
  createPrivacyBudgetTracker,
} from './privacy-budget.mjs';

export {
  SecureAggregation,
  createSecureAggregation,
} from './secure-aggregation.mjs';

// Schemas
export * as schemas from './schemas.mjs';

/**
 * Package version and metadata
 */
export const VERSION = '0.1.0';

export const INNOVATION_PATTERNS = {
  implemented: [
    'Temporal Graph Neural Networks',
    'Neural-Symbolic Hybrid Reasoning',
    'Federated Knowledge Graph Embeddings',
  ],
  planned: [
    'Active Learning for SHACL',
    'Multi-Modal Embeddings',
    'Causal Discovery from RDF',
    'RL Query Optimization',
    'Explainable AI with SHACL Attention',
    'Knowledge Graph Completion',
    'Streaming Anomaly Detection',
  ],
};

/**
 * Performance targets (baseline measurements)
 */
export const PERFORMANCE_TARGETS = {
  tgnn: {
    linkPredictionLatency: '<50ms (P95)',
    temporalWindow: '10-100 snapshots',
    accuracy: '>85%',
  },
  neuralSymbolic: {
    inferenceLatency: '<10ms (P95)',
    precision: '>90%',
    recall: '>80%',
  },
  federated: {
    communicationRounds: '<50',
    privacyBudget: 'ε ≤ 1.0',
    accuracy: '≥95% of centralized',
  },
};

export default {
  VERSION,
  INNOVATION_PATTERNS,
  PERFORMANCE_TARGETS,
};
