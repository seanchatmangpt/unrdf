/**
 * @file Advanced feature Zod schemas
 * @module advanced-schemas
 *
 * @description
 * Zod schemas for query optimization, resolution, sandbox, lockchain, and policy packs.
 */

import { z } from 'zod';
import { FileRefSchema } from './hook-schemas.mjs';

/**
 * Schema for query plans
 */
export const QueryPlanSchema = z.object({
  id: z.string(),
  query: z.string(),
  type: z.enum(['sparql-ask', 'sparql-select', 'shacl']),
  hash: z.string(),
  plan: z.object({
    operations: z.array(
      z.object({
        type: z.string(),
        cost: z.number(),
        selectivity: z.number(),
        dependencies: z.array(z.string()).optional(),
      })
    ),
    estimatedCost: z.number(),
    estimatedRows: z.number(),
    indexes: z.array(z.string()).optional(),
  }),
  createdAt: z.number(),
  lastUsed: z.number(),
  hitCount: z.number().default(0),
});

/**
 * Schema for index definitions
 */
export const IndexSchema = z.object({
  id: z.string(),
  name: z.string(),
  type: z.enum(['predicate', 'subject', 'object', 'graph', 'composite']),
  fields: z.array(z.string()),
  selectivity: z.number().min(0).max(1),
  size: z.number().nonnegative(),
  createdAt: z.number(),
  lastUpdated: z.number(),
});

/**
 * Schema for delta-aware evaluation context
 */
export const DeltaAwareContextSchema = z.object({
  delta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
  }),
  affectedSubjects: z.set(z.string()).optional(),
  affectedPredicates: z.set(z.string()).optional(),
  affectedObjects: z.set(z.string()).optional(),
  affectedGraphs: z.set(z.string()).optional(),
});

/**
 * Schema for agent proposals
 */
export const AgentProposalSchema = z.object({
  id: z.string().uuid(),
  agentId: z.string().min(1),
  delta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
    metadata: z.record(z.any()).optional(),
  }),
  confidence: z.number().min(0).max(1),
  priority: z.number().int().min(0).max(100).default(50),
  timestamp: z.number(),
  metadata: z.record(z.any()).optional(),
  dependencies: z.array(z.string()).optional(),
  conflicts: z.array(z.string()).optional(),
});

/**
 * Schema for resolution strategies
 */
export const ResolutionStrategySchema = z.object({
  type: z.enum(['voting', 'merging', 'crdt', 'consensus', 'priority', 'random']),
  parameters: z.record(z.any()).optional(),
  timeout: z.number().int().positive().max(300000).default(30000),
  quorum: z.number().min(0).max(1).default(0.5),
  maxRetries: z.number().int().nonnegative().max(10).default(3),
});

/**
 * Schema for resolution results
 */
export const ResolutionResultSchema = z.object({
  id: z.string().uuid(),
  strategy: z.string(),
  proposals: z.array(AgentProposalSchema),
  resolvedDelta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
    metadata: z.record(z.any()).optional(),
  }),
  confidence: z.number().min(0).max(1),
  consensus: z.boolean(),
  conflicts: z
    .array(
      z.object({
        type: z.enum(['addition', 'removal', 'metadata']),
        proposals: z.array(z.string()),
        resolution: z.string(),
      })
    )
    .optional(),
  timestamp: z.number(),
  duration: z.number().nonnegative(),
});

/**
 * Schema for sandbox configuration
 */
export const SandboxConfigSchema = z.object({
  type: z.enum(['vm2', 'worker', 'isolate']).default('worker'),
  timeout: z.number().int().positive().max(300000).default(30000),
  memoryLimit: z
    .number()
    .int()
    .positive()
    .max(1024 * 1024 * 1024)
    .default(64 * 1024 * 1024), // 64MB
  cpuLimit: z.number().int().positive().max(100).default(50), // 50% CPU
  allowedModules: z.array(z.string()).default([]),
  allowedGlobals: z.array(z.string()).default(['console', 'Date', 'Math', 'JSON']),
  enableNetwork: z.boolean().default(false),
  enableFileSystem: z.boolean().default(false),
  enableProcess: z.boolean().default(false),
  strictMode: z.boolean().default(true),
});

/**
 * Schema for sandbox execution context
 */
export const SandboxContextSchema = z.object({
  event: z.any(),
  store: z.any(),
  delta: z.any(),
  metadata: z.record(z.any()).optional(),
  allowedFunctions: z.array(z.string()).default(['emitEvent', 'log', 'assert']),
});

/**
 * Schema for sandbox execution result
 */
export const SandboxResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  duration: z.number().nonnegative(),
  memoryUsed: z.number().nonnegative().optional(),
  cpuUsed: z.number().nonnegative().optional(),
  assertions: z
    .array(
      z.object({
        subject: z.string(),
        predicate: z.string(),
        object: z.string(),
        graph: z.string().optional(),
      })
    )
    .optional(),
  events: z.array(z.any()).optional(),
});

/**
 * Schema for lockchain entries
 */
export const LockchainEntrySchema = z.object({
  id: z.string().uuid(),
  timestamp: z.number(),
  receipt: z.any(), // Transaction receipt
  signature: z.object({
    algorithm: z.string(),
    value: z.string(),
    publicKey: z.string().optional(),
  }),
  previousHash: z.string().optional().nullable(),
  merkleRoot: z.string().optional(),
  gitCommit: z.string().optional(),
  gitRef: z.string().optional(),
});

/**
 * Schema for lockchain configuration
 */
export const LockchainConfigSchema = z.object({
  gitRepo: z.string().default(process.cwd()),
  refName: z.string().default('refs/notes/lockchain'),
  signingKey: z.string().optional(),
  algorithm: z.enum(['ed25519', 'ecdsa', 'rsa']).default('ed25519'),
  batchSize: z.number().int().positive().default(10),
  enableMerkle: z.boolean().default(true),
  enableGitAnchoring: z.boolean().default(true),
  storagePath: z.string().optional(),
});

/**
 * Schema for policy pack metadata
 */
export const PolicyPackMetaSchema = z.object({
  name: z.string().min(1).max(100),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  description: z.string().min(1).max(500).optional(),
  author: z.string().min(1).max(100).optional(),
  license: z.string().min(1).max(100).optional(),
  homepage: z.string().url().optional(),
  repository: z.string().url().optional(),
  keywords: z.array(z.string().min(1).max(50)).max(20).optional(),
  ontology: z.array(z.string().min(1).max(50)).max(10).optional(),
  dependencies: z.array(z.string()).optional(),
  createdAt: z.coerce.date().optional(),
  updatedAt: z.coerce.date().optional(),
});

/**
 * Schema for policy pack configuration
 */
export const PolicyPackConfigSchema = z.object({
  basePath: z.string().min(1).default(process.cwd()),
  manifestPath: z.string().default('./policy-pack.json'),
  hooksPath: z.string().default('./hooks'),
  activateOnLoad: z.boolean().default(false),
  enableValidation: z.boolean().default(true),
  maxHooks: z.number().int().positive().max(100).default(50),
  timeout: z.number().int().positive().max(300000).default(30000),
  strictMode: z.boolean().default(false),
});

/**
 * Schema for policy pack manifest
 */
export const PolicyPackManifestSchema = z.object({
  meta: PolicyPackMetaSchema,
  hooks: z.array(
    z.object({
      file: z.string().min(1),
      condition: z.object({
        kind: z.enum(['sparql-ask', 'sparql-select', 'shacl']),
        ref: FileRefSchema,
      }),
      priority: z.number().int().min(0).max(100).default(50).optional(),
      enabled: z.boolean().default(true).optional(),
    })
  ),
  rules: z
    .array(
      z.object({
        id: z.string().min(1),
        description: z.string().min(1).max(500),
        severity: z.enum(['error', 'warn', 'info']).default('info'),
        condition: z.string().min(1),
        appliesTo: z.array(z.string()).optional(),
        excludeFrom: z.array(z.string()).optional(),
      })
    )
    .optional(),
  dependencies: z.array(z.string()).optional(),
  peerDependencies: z.array(z.string()).optional(),
});
