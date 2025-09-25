/**
 * @fileoverview Git Hooks Lifecycle Integration Test
 * 
 * Mocks a complete Git hooks lifecycle with ingress and egress:
 * - pre-commit: Validate incoming changes
 * - post-commit: Process committed changes
 * - pre-push: Validate before pushing
 * - post-receive: Process received changes
 * 
 * Demonstrates the minimal core system handling real-world Git operations.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { RdfEngine } from "../src/engines/rdf-engine.mjs";
import { registerHook } from "../src/engines/hook-manager.mjs";
import { ingress, egress } from "../src/engines/adapters.mjs";
import { writeWithProv } from "../src/engines/provenance.mjs";
import { EVENTS } from "../src/engines/event-bus.mjs";
import { z } from "zod";

// Git operation schemas
const GitCommit = z.object({
  hash: z.string(),
  author: z.string(),
  message: z.string(),
  timestamp: z.string(),
  files: z.array(z.string())
});

const GitFile = z.object({
  path: z.string(),
  content: z.string(),
  operation: z.enum(['added', 'modified', 'deleted']),
  size: z.number()
});

const GitPush = z.object({
  ref: z.string(),
  from: z.string(),
  to: z.string(),
  commits: z.array(z.string())
});

describe('Git Hooks Lifecycle Integration', () => {
  let engine;
  let gitEvents = [];

  beforeEach(() => {
    engine = new RdfEngine({ eventsEnabled: true });
    gitEvents = [];
    // Clear all hooks to ensure test isolation
    engine.clearHooks();
    // Ensure events are enabled for hook registration
    engine.setEventEnabled(true);
  });

  describe('pre-commit Hook', () => {
    it('should validate incoming changes and block invalid commits', async () => {
      // Disable events during data loading
      engine.setEventEnabled(false);

      // Simulate staging area with file changes
      const stagedFiles = [
        { path: 'src/app.js', content: 'console.log("hello");', operation: 'modified', size: 25 },
        { path: 'README.md', content: '# My App', operation: 'added', size: 8 }
      ];

      // Ingress staged files
      for (const file of stagedFiles) {
        const fileRes = await ingress.fromJSON(
          GitFile,
          JSON.stringify(file),
          { 
            base: 'http://git/file/',
            subject: `urn:git:staging:${file.path}`
          }
        );
        
        await writeWithProv(engine, fileRes.rdf, 'staging', {
          operation: 'pre-commit',
          user: 'developer',
          session: 'commit-session-123'
        });
      }

      // Re-enable events and register pre-commit hook
      engine.setEventEnabled(true);
      
      const unregisterPreCommit = registerHook(engine, {
        id: 'pre-commit-validation',
        events: [EVENTS.BEFORE_ADD_QUAD],
        predicates: [
          {
            kind: 'COUNT',
            spec: {
              query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "staging" }',
              operator: '>=',
              value: 1
            }
          }
        ],
        action: async (payload, ok) => {
          gitEvents.push({ hook: 'pre-commit', event: payload.event, allowed: ok });
          
          if (!ok) {
            throw new Error('Pre-commit validation failed: No staging data found');
          }
        }
      });

      // Simulate commit attempt
      const commitData = {
        hash: 'abc123',
        author: 'developer@example.com',
        message: 'Add new feature',
        timestamp: new Date().toISOString(),
        files: stagedFiles.map(f => f.path)
      };

      const commitRes = await ingress.fromJSON(
        GitCommit,
        JSON.stringify(commitData),
        { 
          base: 'http://git/commit/',
          subject: 'urn:git:commit:abc123'
        }
      );

      // This should succeed because we have staging data
      await writeWithProv(engine, commitRes.rdf, 'commit', {
        operation: 'pre-commit',
        user: 'developer',
        session: 'commit-session-123'
      });

      // Wait for async hooks to complete
      await new Promise(resolve => setTimeout(resolve, 200));

      // Verify pre-commit hook fired (should have events for staging files)
      expect(gitEvents.length).toBeGreaterThan(0);
      const preCommitEvents = gitEvents.filter(e => e.hook === 'pre-commit');
      expect(preCommitEvents.length).toBeGreaterThan(0);
      expect(preCommitEvents[0].allowed).toBe(true);

      unregisterPreCommit();
    });

    it('should block commit when validation fails', async () => {
      // Disable events during data loading
      engine.setEventEnabled(false);

      // Try to commit without any new files
      const commitData = {
        hash: 'def456',
        author: 'developer@example.com',
        message: 'Update existing files',
        timestamp: new Date().toISOString(),
        files: ['src/app.js']
      };

      const commitRes = await ingress.fromJSON(
        GitCommit,
        JSON.stringify(commitData),
        { 
          base: 'http://git/commit/',
          subject: 'urn:git:commit:def456'
        }
      );

      // Re-enable events and register strict pre-commit hook
      engine.setEventEnabled(true);
      
      const unregisterPreCommit = registerHook(engine, {
        id: 'strict-pre-commit',
        events: [EVENTS.BEFORE_ADD_QUAD],
        predicates: [
          {
            kind: 'ASK',
            spec: {
              query: 'ASK WHERE { ?s <http://ex/operation> "added" }'
            }
          }
        ],
        action: async (payload, ok) => {
          gitEvents.push({ hook: 'pre-commit', event: payload.event, allowed: ok });
          
          if (!ok) {
            throw new Error('Pre-commit failed: No new files detected');
          }
        }
      });

      // This should fail because no new files
      await expect(async () => {
        await writeWithProv(engine, commitRes.rdf, 'commit', {
          operation: 'pre-commit',
          user: 'developer'
        });
      }).rejects.toThrow('Pre-commit failed: No new files detected');

      unregisterPreCommit();
    });
  });

  describe('post-commit Hook', () => {
    it('should process committed changes and generate metadata', async () => {
      // Disable events during data loading
      engine.setEventEnabled(false);

      // Simulate successful commit
      const commitData = {
        hash: 'xyz789',
        author: 'developer@example.com',
        message: 'Implement feature X',
        timestamp: new Date().toISOString(),
        files: ['src/feature.js', 'tests/feature.test.js']
      };

      const commitRes = await ingress.fromJSON(
        GitCommit,
        JSON.stringify(commitData),
        { 
          base: 'http://git/commit/',
          subject: 'urn:git:commit:xyz789'
        }
      );

      // Re-enable events and register post-commit hook
      engine.setEventEnabled(true);
      
      const unregisterPostCommit = registerHook(engine, {
        id: 'post-commit-processing',
        events: [EVENTS.AFTER_ADD_QUAD],
        predicates: [
          {
            kind: 'COUNT',
            spec: {
              query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "commit" }',
              operator: '>=',
              value: 1
            }
          }
        ],
        action: async (payload, ok) => {
          if (ok && payload.quad && payload.quad.predicate && payload.quad.predicate.value === 'http://ex/hash') {
            gitEvents.push({ 
              hook: 'post-commit', 
              event: payload.event, 
              commitHash: payload.quad.object.value,
              timestamp: payload.context.timestamp
            });
            
            // Generate commit metadata
            const metadata = {
              processed: true,
              timestamp: new Date().toISOString(),
              hook: 'post-commit'
            };
            
            const metadataRes = await ingress.fromJSON(
              z.object({ processed: z.boolean(), timestamp: z.string(), hook: z.string() }),
              JSON.stringify(metadata),
              { 
                base: 'http://ex/metadata/',
                subject: `urn:git:metadata:${payload.quad.object.value}`
              }
            );
            
            await writeWithProv(engine, metadataRes.rdf, 'metadata', {
              operation: 'post-commit',
              user: 'system'
            });
          }
        }
      });

      await writeWithProv(engine, commitRes.rdf, 'commit', {
        operation: 'post-commit',
        user: 'developer'
      });

      // Wait for async processing
      await new Promise(resolve => setTimeout(resolve, 200));

      // Verify post-commit hook processed the commit
      const postCommitEvents = gitEvents.filter(e => e.hook === 'post-commit');
      expect(postCommitEvents.length).toBeGreaterThan(0);
      expect(postCommitEvents[0].commitHash).toBe('xyz789');

      // Verify metadata was generated
      const metadataQuads = engine.store.getQuads(
        engine.namedNode('urn:git:metadata:xyz789'),
        null,
        null,
        null
      );
      
      expect(metadataQuads.length).toBeGreaterThan(0);

      unregisterPostCommit();
    });
  });

  describe('pre-push Hook', () => {
    it('should validate push operations and block dangerous pushes', async () => {
      // Disable events during data loading
      engine.setEventEnabled(false);

      // Simulate push with safe commits
      const pushData = {
        ref: 'refs/heads/main',
        from: 'abc123',
        to: 'def456',
        commits: ['commit1', 'commit2']
      };

      const pushRes = await ingress.fromJSON(
        GitPush,
        JSON.stringify(pushData),
        { 
          base: 'http://git/push/',
          subject: 'urn:git:push:main'
        }
      );

      // Re-enable events and register pre-push hook
      engine.setEventEnabled(true);
      
      const unregisterPrePush = registerHook(engine, {
        id: 'pre-push-validation',
        events: [EVENTS.BEFORE_UPDATE],
        predicates: [
          {
            kind: 'COUNT',
            spec: {
              query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "commit" }',
              operator: '>=',
              value: 1
            }
          }
        ],
        action: async (payload, ok) => {
          gitEvents.push({ hook: 'pre-push', event: payload.event, allowed: ok });
          
          if (!ok) {
            throw new Error('Pre-push validation failed: No commits to push');
          }
          
          // Check for dangerous operations
          const dangerousQuery = `
            ASK WHERE { 
              ?s <http://ex/message> ?msg .
              FILTER(CONTAINS(LCASE(?msg), "force") || CONTAINS(LCASE(?msg), "delete"))
            }
          `;
          
          const dangerousResult = await engine.query(dangerousQuery);
          if (dangerousResult.boolean) {
            throw new Error('Pre-push blocked: Dangerous commit message detected');
          }
        }
      });

      // This should succeed
      await writeWithProv(engine, pushRes.rdf, 'push', {
        operation: 'pre-push',
        user: 'developer'
      });

      // Wait for async hooks to complete
      await new Promise(resolve => setTimeout(resolve, 200));

      // Verify pre-push hook fired
      const prePushEvents = gitEvents.filter(e => e.hook === 'pre-push');
      expect(prePushEvents.length).toBeGreaterThan(0);
      expect(prePushEvents[0].allowed).toBe(true);

      unregisterPrePush();
    });

    it('should block push with dangerous commit messages', async () => {
      // Disable events during data loading
      engine.setEventEnabled(false);

      // Add a dangerous commit
      const dangerousCommit = {
        hash: 'danger123',
        author: 'developer@example.com',
        message: 'Force delete all files',
        timestamp: new Date().toISOString(),
        files: ['*']
      };

      const commitRes = await ingress.fromJSON(
        GitCommit,
        JSON.stringify(dangerousCommit),
        { 
          base: 'http://git/commit/',
          subject: 'urn:git:commit:danger123'
        }
      );

      // Re-enable events and register pre-push hook
      engine.setEventEnabled(true);
      
      const unregisterPrePush = registerHook(engine, {
        id: 'dangerous-push-blocker',
        events: [EVENTS.BEFORE_UPDATE],
        predicates: [
          {
            kind: 'ASK',
            spec: {
              query: `
                ASK WHERE { 
                  ?s <http://ex/message> ?msg .
                  FILTER(CONTAINS(LCASE(?msg), "force") || CONTAINS(LCASE(?msg), "delete"))
                }
              `
            }
          }
        ],
        action: async (payload, ok) => {
          gitEvents.push({ hook: 'pre-push', event: payload.event, allowed: ok });
          
          if (ok) {
            throw new Error('Pre-push blocked: Dangerous commit message detected');
          }
        }
      });

      await writeWithProv(engine, commitRes.rdf, 'commit', {
        operation: 'pre-push',
        user: 'developer'
      });

      // Try to push
      const pushData = {
        ref: 'refs/heads/main',
        from: 'abc123',
        to: 'danger123',
        commits: ['danger123']
      };

      const pushRes = await ingress.fromJSON(
        GitPush,
        JSON.stringify(pushData),
        { 
          base: 'http://git/push/',
          subject: 'urn:git:push:main'
        }
      );

      // This should fail
      await expect(async () => {
        await writeWithProv(engine, pushRes.rdf, 'push', {
          operation: 'pre-push',
          user: 'developer'
        });
        // Wait for async hooks to complete
        await new Promise(resolve => setTimeout(resolve, 200));
      }).rejects.toThrow('Pre-push blocked: Dangerous commit message detected');

      unregisterPrePush();
    });
  });

  describe('post-receive Hook', () => {
    it('should process received changes and trigger downstream actions', async () => {
      // Disable events during data loading
      engine.setEventEnabled(false);

      // Simulate successful push
      const pushData = {
        ref: 'refs/heads/main',
        from: 'abc123',
        to: 'def456',
        commits: ['commit1', 'commit2', 'commit3']
      };

      const pushRes = await ingress.fromJSON(
        GitPush,
        JSON.stringify(pushData),
        { 
          base: 'http://git/push/',
          subject: 'urn:git:push:main'
        }
      );

      // Re-enable events and register post-receive hook
      engine.setEventEnabled(true);
      
      const unregisterPostReceive = registerHook(engine, {
        id: 'post-receive-processing',
        events: [EVENTS.AFTER_ADD_QUAD],
        predicates: [
          {
            kind: 'COUNT',
            spec: {
              query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "push" }',
              operator: '>=',
              value: 1
            }
          }
        ],
        action: async (payload, ok) => {
          if (ok && payload.quad && payload.quad.predicate && payload.quad.predicate.value === 'http://ex/ref') {
            gitEvents.push({ 
              hook: 'post-receive', 
              event: payload.event, 
              ref: payload.quad.object.value,
              timestamp: payload.context.timestamp
            });
            
            // Trigger downstream actions
            const actions = [
              { type: 'deploy', target: 'staging', ref: payload.quad.object.value },
              { type: 'notify', channel: '#deployments', message: `New push to ${payload.quad.object.value}` },
              { type: 'backup', repository: 'main', timestamp: new Date().toISOString() }
            ];
            
            for (const action of actions) {
              const actionRes = await ingress.fromJSON(
                z.object({ type: z.string(), target: z.string().optional(), ref: z.string().optional(), channel: z.string().optional(), message: z.string().optional(), repository: z.string().optional(), timestamp: z.string().optional() }),
                JSON.stringify(action),
                { 
                  base: 'http://ex/action/',
                  subject: `urn:git:action:${action.type}:${Date.now()}`
                }
              );
              
              await writeWithProv(engine, actionRes.rdf, 'action', {
                operation: 'post-receive',
                user: 'system'
              });
            }
          }
        }
      });

      await writeWithProv(engine, pushRes.rdf, 'push', {
        operation: 'post-receive',
        user: 'developer'
      });

      // Wait for async processing
      await new Promise(resolve => setTimeout(resolve, 200));

      // Verify post-receive hook processed the push
      const postReceiveEvents = gitEvents.filter(e => e.hook === 'post-receive');
      expect(postReceiveEvents.length).toBeGreaterThan(0);
      expect(postReceiveEvents[0].ref).toBe('refs/heads/main');

      // Verify downstream actions were triggered
      const actionQuads = engine.store.getQuads(
        null,
        engine.namedNode('http://ex/prov#source'),
        engine.literal('action'),
        null
      );
      
      expect(actionQuads.length).toBeGreaterThanOrEqual(3); // deploy, notify, backup

      unregisterPostReceive();
    });
  });

  describe('Complete Git Lifecycle', () => {
    it('should handle complete Git workflow from staging to deployment', async () => {
      const lifecycleEvents = [];

      // Disable events during data loading
      engine.setEventEnabled(false);

      // 1. Stage files
      const stagedFile = {
        path: 'src/feature.js',
        content: 'export function feature() { return "hello"; }',
        operation: 'added',
        size: 50
      };

      const fileRes = await ingress.fromJSON(
        GitFile,
        JSON.stringify(stagedFile),
        { 
          base: 'http://git/file/',
          subject: 'urn:git:staging:src/feature.js'
        }
      );
      
      await writeWithProv(engine, fileRes.rdf, 'staging', {
        operation: 'pre-commit',
        user: 'developer'
      });

      // 2. Commit
      const commitData = {
        hash: 'workflow123',
        author: 'developer@example.com',
        message: 'Add new feature',
        timestamp: new Date().toISOString(),
        files: ['src/feature.js']
      };

      const commitRes = await ingress.fromJSON(
        GitCommit,
        JSON.stringify(commitData),
        { 
          base: 'http://git/commit/',
          subject: 'urn:git:commit:workflow123'
        }
      );
      
      await writeWithProv(engine, commitRes.rdf, 'commit', {
        operation: 'post-commit',
        user: 'developer'
      });

      // 3. Push
      const pushData = {
        ref: 'refs/heads/main',
        from: 'abc123',
        to: 'workflow123',
        commits: ['workflow123']
      };

      const pushRes = await ingress.fromJSON(
        GitPush,
        JSON.stringify(pushData),
        { 
          base: 'http://git/push/',
          subject: 'urn:git:push:main'
        }
      );
      
      await writeWithProv(engine, pushRes.rdf, 'push', {
        operation: 'pre-push',
        user: 'developer'
      });

      // Re-enable events and register all hooks
      engine.setEventEnabled(true);
      
      const unregisterHooks = [
        // Pre-commit: Validate staging
        registerHook(engine, {
          id: 'pre-commit',
          events: [EVENTS.BEFORE_ADD_QUAD],
          predicates: [
            {
              kind: 'COUNT',
              spec: {
                query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "staging" }',
                operator: '>=',
                value: 1
              }
            }
          ],
          action: async (payload, ok) => {
            lifecycleEvents.push({ phase: 'pre-commit', allowed: ok, timestamp: new Date().toISOString() });
            if (!ok) throw new Error('Pre-commit failed');
          }
        }),

        // Post-commit: Process commit
        registerHook(engine, {
          id: 'post-commit',
          events: [EVENTS.AFTER_ADD_QUAD],
          predicates: [
            {
              kind: 'COUNT',
              spec: {
                query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "commit" }',
                operator: '>=',
                value: 1
              }
            }
          ],
          action: async (payload, ok) => {
            lifecycleEvents.push({ phase: 'post-commit', allowed: ok, timestamp: new Date().toISOString() });
          }
        }),

        // Pre-push: Validate push
        registerHook(engine, {
          id: 'pre-push',
          events: [EVENTS.BEFORE_UPDATE],
          predicates: [
            {
              kind: 'COUNT',
              spec: {
                query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "commit" }',
                operator: '>=',
                value: 1
              }
            }
          ],
          action: async (payload, ok) => {
            lifecycleEvents.push({ phase: 'pre-push', allowed: ok, timestamp: new Date().toISOString() });
            if (!ok) throw new Error('Pre-push failed');
          }
        }),

        // Post-receive: Process received changes
        registerHook(engine, {
          id: 'post-receive',
          events: [EVENTS.AFTER_ADD_QUAD],
          predicates: [
            {
              kind: 'COUNT',
              spec: {
                query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://ex/prov#source> "push" }',
                operator: '>=',
                value: 1
              }
            }
          ],
          action: async (payload, ok) => {
            lifecycleEvents.push({ phase: 'post-receive', allowed: ok, timestamp: new Date().toISOString() });
          }
        })
      ];

      // Wait for all async processing
      await new Promise(resolve => setTimeout(resolve, 500));

      // Verify complete lifecycle
      expect(lifecycleEvents.length).toBeGreaterThan(0);
      const phases = lifecycleEvents.map(e => e.phase);
      expect(phases).toContain('pre-commit');
      expect(phases).toContain('post-commit');
      expect(phases).toContain('pre-push');
      expect(phases).toContain('post-receive');

      // All phases should have been allowed
      lifecycleEvents.forEach(event => {
        expect(event.allowed).toBe(true);
      });

      // Verify provenance chain
      const provenanceQuads = engine.store.getQuads(
        null,
        engine.namedNode('http://ex/prov#source'),
        null,
        null
      );

      const sources = provenanceQuads.map(q => q.object.value);
      expect(sources).toContain('staging');
      expect(sources).toContain('commit');
      expect(sources).toContain('push');

      // Cleanup
      unregisterHooks.forEach(unregister => unregister());
    });
  });

  describe('Egress Operations', () => {
    it('should export Git data in different formats', async () => {
      // Disable events during data loading
      engine.setEventEnabled(false);

      // Add some Git data
      const commitData = {
        hash: 'export123',
        author: 'developer@example.com',
        message: 'Export test commit',
        timestamp: new Date().toISOString(),
        files: ['src/app.js', 'README.md']
      };

      const commitRes = await ingress.fromJSON(
        GitCommit,
        JSON.stringify(commitData),
        { 
          base: 'http://git/commit/',
          subject: 'urn:git:commit:export123'
        }
      );

      await writeWithProv(engine, commitRes.rdf, 'commit', {
        operation: 'export-test',
        user: 'developer'
      });

      // Export as JSON
      const jsonExport = await egress.toJSON(
        GitCommit,
        engine.store,
        { subject: 'urn:git:commit:export123' }
      );

      expect(jsonExport.output).toContain('export123');
      expect(jsonExport.output).toContain('developer@example.com');
      expect(jsonExport.output).toContain('Export test commit');

      // Export as Turtle
      const turtleExport = await egress.toTurtle(
        null,
        engine.store,
        engine
      );

      expect(turtleExport.output).toContain('urn:git:commit:export123');
      expect(turtleExport.output).toContain('http://git/commit/hash');
      expect(turtleExport.output).toContain('export123');
    });
  });
});
