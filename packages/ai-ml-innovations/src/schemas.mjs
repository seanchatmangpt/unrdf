/**
 * @file Zod Schemas for AI/ML Innovations
 * @module ai-ml-innovations/schemas
 *
 * @description
 * Comprehensive validation schemas for federated learning,
 * temporal GNNs, and neural-symbolic reasoning.
 */

import { z } from 'zod';

/**
 * Federated Learning Schemas
 */

export const NodeUpdateSchema = z.object({
  nodeId: z.string().min(1),
  gradients: z.record(z.string(), z.array(z.number())),
  sampleCount: z.number().int().positive(),
  epoch: z.number().int().nonnegative(),
  timestamp: z.number(),
  nonce: z.string().optional(),
});

export const ModelSchema = z.object({
  entityEmbeddings: z.record(z.string(), z.array(z.number())),
  relationEmbeddings: z.record(z.string(), z.array(z.number())),
  version: z.number().int().nonnegative(),
  timestamp: z.number(),
  checksum: z.string().optional(),
});

export const FederatedConfigSchema = z.object({
  embeddingDim: z.number().min(32).max(512).default(128),
  aggregationStrategy: z.enum(['fedavg', 'fedprox', 'fedadam']).default('fedavg'),
  privacyBudget: z.number().min(0).max(10).default(1.0),
  noiseMultiplier: z.number().min(0).max(2).default(0.1),
  clippingNorm: z.number().min(0.1).max(10).default(1.0),
  minNodesPerRound: z.number().min(1).max(1000).default(2),
  enableDifferentialPrivacy: z.boolean().default(true),
  secureAggregation: z.boolean().default(false),
  convergenceThreshold: z.number().min(0).max(1).default(0.001),
});

export const PrivacyBudgetSchema = z.object({
  epsilon: z.number().min(0).max(10),
  delta: z.number().min(0).max(1).default(1e-5),
  spent: z.number().min(0).default(0),
  remaining: z.number().min(0),
  rounds: z.number().int().nonnegative().default(0),
  composition: z.enum(['basic', 'advanced', 'rdp', 'moments']).default('moments'),
});

export const DPMechanismSchema = z.object({
  mechanism: z.enum(['gaussian', 'laplace', 'exponential']).default('gaussian'),
  sensitivity: z.number().positive(),
  epsilon: z.number().positive(),
  delta: z.number().min(0).max(1).default(1e-5),
  clippingNorm: z.number().positive(),
});

export const SecureAggregationConfigSchema = z.object({
  threshold: z.number().int().min(1),
  totalNodes: z.number().int().min(1),
  keySize: z.number().int().min(128).max(4096).default(256),
  enableEncryption: z.boolean().default(true),
});

export const TrainingMetricsSchema = z.object({
  epoch: z.number().int().nonnegative(),
  loss: z.number(),
  accuracy: z.number().min(0).max(1),
  privacySpent: z.number().min(0),
  roundTime: z.number().positive(),
  convergence: z.boolean().default(false),
  gradientNorm: z.number().nonnegative().optional(),
});

export const FedAvgConfigSchema = z.object({
  learningRate: z.number().positive().default(0.01),
  momentum: z.number().min(0).max(1).default(0.9),
  weightDecay: z.number().min(0).max(1).default(0.0001),
  clientSampling: z.number().min(0).max(1).default(1.0),
  minClients: z.number().int().positive().default(1),
});

/**
 * Temporal GNN Schemas
 */

export const TemporalGraphSchema = z.object({
  nodes: z.array(z.string()),
  edges: z.array(
    z.object({
      source: z.string(),
      target: z.string(),
      relation: z.string(),
      timestamp: z.number(),
    })
  ),
  snapshots: z.array(
    z.object({
      timestamp: z.number(),
      nodeCount: z.number().int().nonnegative(),
      edgeCount: z.number().int().nonnegative(),
    })
  ),
});

export const TGNNConfigSchema = z.object({
  hiddenDim: z.number().min(16).max(512).default(128),
  numLayers: z.number().int().min(1).max(10).default(2),
  temporalWindow: z.number().int().min(1).max(1000).default(10),
  attentionHeads: z.number().int().min(1).max(16).default(4),
  dropout: z.number().min(0).max(1).default(0.1),
});

/**
 * Neural-Symbolic Schemas
 */

export const RuleSchema = z.object({
  id: z.string(),
  antecedent: z.array(z.string()),
  consequent: z.string(),
  confidence: z.number().min(0).max(1),
  support: z.number().min(0).max(1),
  learned: z.boolean().default(false),
});

export const NeuralSymbolicConfigSchema = z.object({
  embeddingDim: z.number().min(32).max(512).default(128),
  ruleThreshold: z.number().min(0).max(1).default(0.7),
  maxRuleLength: z.number().int().min(1).max(10).default(3),
  attentionHeads: z.number().int().min(1).max(8).default(4),
});

/**
 * Export all schemas
 */
export default {
  // Federated Learning
  NodeUpdateSchema,
  ModelSchema,
  FederatedConfigSchema,
  PrivacyBudgetSchema,
  DPMechanismSchema,
  SecureAggregationConfigSchema,
  TrainingMetricsSchema,
  FedAvgConfigSchema,

  // Temporal GNN
  TemporalGraphSchema,
  TGNNConfigSchema,

  // Neural-Symbolic
  RuleSchema,
  NeuralSymbolicConfigSchema,
};
