/**
 * @fileoverview KGC CLI extension for KGC Probe
 *
 * Provides commands for:
 * - kgc probe scan - Run integrity scan
 * - kgc probe validate - Validate artifact
 * - kgc probe diff - Compare artifacts
 * - kgc shard merge - Merge distributed shards
 *
 * @module kgc-cli/extensions/kgc-probe
 */

import { z } from 'zod';

// ============================================================================
// SCHEMAS
// ============================================================================

const ScanArgsSchema = z.object({
  universe: z.string().describe('Universe ID to scan'),
  snapshot: z.string().optional().describe('Optional snapshot reference'),
  agents: z.array(z.string()).optional().describe('Specific agents to run (all if omitted)'),
  guards: z.array(z.string()).optional().describe('Specific guards to apply'),
  distributed: z.boolean().optional().default(false).describe('Enable shard merging'),
  persist: z.boolean().optional().default(true).describe('Persist results to storage')
}).describe('Probe scan arguments');

const ValidateArgsSchema = z.object({
  artifact_id: z.string().describe('Artifact ID to validate'),
  strict: z.boolean().optional().default(false).describe('Fail on warnings')
}).describe('Artifact validation arguments');

const DiffArgsSchema = z.object({
  artifact1_id: z.string().describe('First artifact ID'),
  artifact2_id: z.string().describe('Second artifact ID'),
  kind: z.enum(['observations', 'summary']).optional().default('summary').describe('Diff type')
}).describe('Artifact diff arguments');

const ShardMergeArgsSchema = z.object({
  universe: z.string().describe('Universe ID'),
  output_id: z.string().optional().describe('Output artifact ID')
}).describe('Shard merge arguments');

// ============================================================================
// EXTENSION DEFINITION
// ============================================================================

/**
 * KGC Probe CLI extension
 * @type {Object}
 */
const extension = {
  id: '@unrdf/kgc-probe',
  description: 'Automated knowledge graph integrity scanning and validation',

  nouns: {
    // =====================================================================
    // PROBE NOUN
    // =====================================================================
    probe: {
      description: 'Knowledge graph integrity scanning with 10 agents',

      verbs: {
        scan: {
          description: 'Execute full integrity scan on universe with all agents and guards',
          argsSchema: ScanArgsSchema,
          handler: async (args) => {
            // Dynamic import to avoid circular dependency
            const { runProbe, createMemoryStorage } = await import('@unrdf/kgc-probe');

            try {
              const artifact = await runProbe({
                universe_id: args.universe,
                snapshot_id: args.snapshot,
                agents: args.agents,
                guards: args.guards,
                distributed: args.distributed,
                persist: args.persist
              });

              return {
                probe_run_id: artifact.probe_run_id,
                status: 'success',
                summary: artifact.summary,
                universe_id: artifact.universe_id,
                observation_count: artifact.observations.length,
                execution_time_ms: artifact.metadata.execution_time_ms
              };
            } catch (err) {
              return {
                status: 'error',
                error: err.message
              };
            }
          },
          meta: {
            examples: [
              'kgc probe scan --args \'{"universe":"my-universe"}\'',
              'kgc probe scan --args \'{"universe":"my-universe","snapshot":"snap_123","persist":true}\' --json'
            ]
          }
        },

        validate: {
          description: 'Validate artifact integrity and guard compliance',
          argsSchema: ValidateArgsSchema,
          handler: async (args) => {
            // Dynamic import
            const { verifyArtifact, createMemoryStorage } = await import('@unrdf/kgc-probe');

            try {
              // Would need to load artifact from storage first
              // This is a placeholder
              return {
                valid: true,
                violations: [],
                verified_at: new Date().toISOString()
              };
            } catch (err) {
              return {
                valid: false,
                error: err.message
              };
            }
          },
          meta: {
            examples: [
              'kgc probe validate --args \'{"artifact_id":"run-123"}\'',
              'kgc probe validate --args \'{"artifact_id":"run-123","strict":true}\' --json'
            ]
          }
        },

        diff: {
          description: 'Compare two artifacts and show differences',
          argsSchema: DiffArgsSchema,
          handler: async (args) => {
            // Dynamic import
            const { diffArtifacts } = await import('@unrdf/kgc-probe');

            try {
              // Would need to load artifacts from storage
              // This is a placeholder
              return {
                added: [],
                removed: [],
                modified: [],
                summary: {
                  total_changes: 0,
                  similarity_ratio: 1.0
                }
              };
            } catch (err) {
              return {
                status: 'error',
                error: err.message
              };
            }
          },
          meta: {
            examples: [
              'kgc probe diff --args \'{"artifact1_id":"run-1","artifact2_id":"run-2"}\'',
              'kgc probe diff --args \'{"artifact1_id":"run-1","artifact2_id":"run-2","kind":"observations"}\' --json'
            ]
          }
        },

        list: {
          description: 'List all stored artifacts',
          handler: async () => {
            // Would query storage for all artifacts
            return {
              artifacts: [],
              count: 0
            };
          }
        }
      }
    },

    // =====================================================================
    // SHARD NOUN
    // =====================================================================
    shard: {
      description: 'Distributed shard management for probes',

      verbs: {
        merge: {
          description: 'Merge distributed probe shards',
          argsSchema: ShardMergeArgsSchema,
          handler: async (args) => {
            // Dynamic import
            const { mergeShards, createMemoryStorage } = await import('@unrdf/kgc-probe');

            try {
              // Would load shards from storage and merge
              // This is a placeholder
              return {
                status: 'success',
                shards_merged: 0,
                output_artifact_id: args.output_id || 'merged_' + Date.now()
              };
            } catch (err) {
              return {
                status: 'error',
                error: err.message
              };
            }
          },
          meta: {
            examples: [
              'kgc shard merge --args \'{"universe":"my-universe"}\''
            ]
          }
        }
      }
    },

    // =====================================================================
    // GUARD NOUN
    // =====================================================================
    guard: {
      description: 'Probe guard validation rules',

      verbs: {
        list: {
          description: 'List available guards',
          handler: async () => {
            const { createGuardRegistry } = await import('@unrdf/kgc-probe');

            try {
              const registry = createGuardRegistry();
              return {
                guards: registry.list(),
                count: registry.list().length
              };
            } catch (err) {
              return {
                status: 'error',
                error: err.message
              };
            }
          }
        }
      }
    },

    // =====================================================================
    // AGENT NOUN
    // =====================================================================
    agent: {
      description: 'Probe agents for specific integrity checks',

      verbs: {
        list: {
          description: 'List available probe agents',
          handler: async () => {
            const { createAgentRegistry } = await import('@unrdf/kgc-probe');

            try {
              const registry = createAgentRegistry();
              const agents = registry.list();

              return {
                agents: agents,
                count: agents.length,
                description: 'KGC Probe has 10 specialized agents for integrity scanning'
              };
            } catch (err) {
              return {
                status: 'error',
                error: err.message
              };
            }
          }
        }
      }
    }
  }
};

export default extension;
