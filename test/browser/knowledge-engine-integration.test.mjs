/**
 * @fileoverview Browser Knowledge Engine Integration Tests (HIGH - P3)
 * Tests end-to-end browser functionality
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { Store, DataFactory } from 'n3';
import {
  createBrowserKnowledgeHookManager,
  createBrowserResolutionLayer,
  createBrowserFileResolver,
  createBrowserPolicyPackManager,
  createEffectSandbox,
  createBrowserLockchainWriter
} from '../../src/knowledge-engine/browser.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('browser knowledge-engine integration (HIGH)', () => {
  let manager;
  let store;

  beforeEach(() => {
    manager = createBrowserKnowledgeHookManager({
      basePath: '/',
      enableKnowledgeHooks: true,
      strictMode: false
    });

    store = new Store();
    store.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/name'),
      literal('Alice')
    );
  });

  afterEach(() => {
    manager = null;
    store = null;
  });

  describe('Knowledge Hook Manager Integration', () => {
    it('should create manager with browser components', () => {
      expect(manager).toBeDefined();
      expect(manager.hookExecutor).toBeDefined();
      expect(manager.conditionEvaluator).toBeDefined();
      expect(manager.policyPackManager).toBeDefined();
    });

    it('should add and execute knowledge hook', async () => {
      let hookExecuted = false;

      const hook = {
        meta: { name: 'test-hook', version: '1.0.0' },
        run: async (event) => {
          hookExecuted = true;
          return { success: true, data: 'hook executed' };
        }
      };

      await manager.addKnowledgeHook(hook);

      const result = await manager.executeKnowledgeHook('test-hook', {
        type: 'test-event',
        store,
        delta: { additions: [], removals: [] }
      });

      expect(result.success).toBe(true);
      expect(hookExecuted).toBe(true);
    });

    it('should execute all knowledge hooks', async () => {
      const hooks = [
        {
          meta: { name: 'hook-1' },
          run: async () => ({ success: true, id: 1 })
        },
        {
          meta: { name: 'hook-2' },
          run: async () => ({ success: true, id: 2 })
        },
        {
          meta: { name: 'hook-3' },
          run: async () => ({ success: true, id: 3 })
        }
      ];

      for (const hook of hooks) {
        await manager.addKnowledgeHook(hook);
      }

      const results = await manager.executeAllKnowledgeHooks({
        type: 'batch-event',
        store
      });

      expect(results).toHaveLength(3);
      expect(results.every(r => r.success)).toBe(true);
    });

    it('should get hook statistics', async () => {
      const hook = {
        meta: { name: 'stats-hook' },
        run: async () => ({ success: true })
      };

      await manager.addKnowledgeHook(hook);

      const stats = manager.getStats();

      expect(stats.knowledgeHooks.total).toBe(1);
      expect(stats.knowledgeHooks.enabled).toBe(true);
      expect(stats.hookExecutor).toBeDefined();
    });

    it('should remove knowledge hook', async () => {
      const hook = {
        meta: { name: 'removable-hook' },
        run: async () => ({ success: true })
      };

      await manager.addKnowledgeHook(hook);
      expect(manager.getStats().knowledgeHooks.total).toBe(1);

      const removed = manager.removeKnowledgeHook('removable-hook');

      expect(removed).toBe(true);
      expect(manager.getStats().knowledgeHooks.total).toBe(0);
    });
  });

  describe('Resolution Layer Integration', () => {
    let resolutionLayer;

    beforeEach(() => {
      resolutionLayer = createBrowserResolutionLayer({
        defaultStrategy: 'voting',
        maxProposals: 100,
        enableConflictDetection: true
      });
    });

    it('should register agents', () => {
      resolutionLayer.registerAgent('agent-1', {
        type: 'validator',
        priority: 1
      });

      resolutionLayer.registerAgent('agent-2', {
        type: 'proposer',
        priority: 2
      });

      const stats = resolutionLayer.getStats();
      expect(stats.agents).toBe(2);
    });

    it('should submit and resolve proposals', async () => {
      const proposal1 = {
        id: 'prop-1',
        agentId: 'agent-1',
        delta: {
          additions: [
            quad(
              namedNode('http://example.org/bob'),
              namedNode('http://example.org/name'),
              literal('Bob')
            )
          ],
          removals: []
        }
      };

      resolutionLayer.submitProposal(proposal1);

      const resolution = await resolutionLayer.resolveProposals('voting');

      expect(resolution).toBeDefined();
      expect(resolution.strategy).toBe('voting');
      expect(resolution.proposals).toHaveLength(1);
      expect(resolution.resolvedDelta).toBeDefined();
    });

    it('should calculate consensus confidence', async () => {
      const proposals = [
        { id: 'p1', agentId: 'a1', delta: { additions: [], removals: [] } },
        { id: 'p2', agentId: 'a2', delta: { additions: [], removals: [] } }
      ];

      proposals.forEach(p => resolutionLayer.submitProposal(p));

      const resolution = await resolutionLayer.resolveProposals();

      expect(resolution.confidence).toBeDefined();
      expect(resolution.confidence).toBeGreaterThan(0);
      expect(resolution.confidence).toBeLessThanOrEqual(1);
    });

    it('should track resolution history', async () => {
      const proposal = {
        id: 'p1',
        agentId: 'a1',
        delta: { additions: [], removals: [] }
      };

      resolutionLayer.submitProposal(proposal);
      await resolutionLayer.resolveProposals();

      const stats = resolutionLayer.getStats();
      expect(stats.totalResolutions).toBe(1);
    });
  });

  describe('File Resolver Integration', () => {
    let fileResolver;

    beforeEach(() => {
      fileResolver = createBrowserFileResolver({
        basePath: '/',
        enableCache: true,
        maxFileSize: 1024 * 1024
      });
    });

    it('should resolve file URIs', async () => {
      const uri = 'file:///data/query.sparql';
      const resolved = await fileResolver.resolveFileUri(uri);

      expect(resolved).toBe('/data/query.sparql');
    });

    it('should resolve HTTP URIs', async () => {
      const uri = 'https://example.org/data.ttl';
      const resolved = await fileResolver.resolveFileUri(uri);

      expect(resolved).toBe('https://example.org/data.ttl');
    });

    it('should resolve relative URIs', async () => {
      const uri = 'queries/select.sparql';
      const resolved = await fileResolver.resolveFileUri(uri, '/base');

      expect(resolved).toBe('/base/queries/select.sparql');
    });

    it('should calculate file hashes', async () => {
      const hash = await fileResolver.calculateFileHash('/test.txt');

      expect(hash).toBeDefined();
      expect(typeof hash).toBe('string');
    });

    it('should load SPARQL files', async () => {
      const content = await fileResolver.loadSparqlFile(
        '/query.sparql',
        'mock-hash'
      );

      expect(typeof content).toBe('string');
    });
  });

  describe('Policy Pack Manager Integration', () => {
    let policyManager;

    beforeEach(() => {
      policyManager = createBrowserPolicyPackManager('/policies');
    });

    it('should load policy pack', async () => {
      const manifest = {
        name: 'security-pack',
        version: '1.0.0',
        hooks: [
          { id: 'h1', mode: 'pre', type: 'authorization' },
          { id: 'h2', mode: 'post', type: 'audit' }
        ],
        rules: [
          { id: 'r1', type: 'access-control' }
        ]
      };

      await policyManager.loadPolicyPack('security-pack', manifest);

      const pack = policyManager.getPolicyPack('security-pack');

      expect(pack).toBeDefined();
      expect(pack.name).toBe('security-pack');
      expect(pack.loaded).toBe(true);
      expect(pack.hooks).toHaveLength(2);
    });

    it('should activate and deactivate policy packs', async () => {
      const manifest = {
        name: 'test-pack',
        hooks: [{ id: 'h1', mode: 'pre' }]
      };

      await policyManager.loadPolicyPack('test-pack', manifest);

      const activated = policyManager.activatePolicyPack('test-pack');
      expect(activated).toBe(true);

      const activePacks = policyManager.getActivePolicyPacks();
      expect(activePacks).toHaveLength(1);

      const deactivated = policyManager.deactivatePolicyPack('test-pack');
      expect(deactivated).toBe(true);

      const activeAfter = policyManager.getActivePolicyPacks();
      expect(activeAfter).toHaveLength(0);
    });

    it('should get active hooks from policy packs', async () => {
      const manifest1 = {
        name: 'pack-1',
        hooks: [{ id: 'h1' }, { id: 'h2' }]
      };

      const manifest2 = {
        name: 'pack-2',
        hooks: [{ id: 'h3' }]
      };

      await policyManager.loadPolicyPack('pack-1', manifest1);
      await policyManager.loadPolicyPack('pack-2', manifest2);

      policyManager.activatePolicyPack('pack-1');
      policyManager.activatePolicyPack('pack-2');

      const activeHooks = policyManager.getActiveHooks();

      expect(activeHooks).toHaveLength(3);
    });
  });

  describe('End-to-End Workflows', () => {
    it('should complete full transaction workflow in browser', async () => {
      // Setup components
      const hookManager = createBrowserKnowledgeHookManager();
      const sandbox = createEffectSandbox();
      const lockchain = createBrowserLockchainWriter();

      // Add hook
      await hookManager.addKnowledgeHook({
        meta: { name: 'transaction-hook' },
        run: async (event) => {
          return { success: true, validated: true };
        }
      });

      // Create delta
      const delta = {
        additions: [
          quad(
            namedNode('http://example.org/bob'),
            namedNode('http://example.org/age'),
            literal('25')
          )
        ],
        removals: []
      };

      // Execute hook
      const hookResult = await hookManager.executeKnowledgeHook(
        'transaction-hook',
        { type: 'transaction', store, delta }
      );

      expect(hookResult.success).toBe(true);

      // Write to lockchain
      const receipt = {
        id: 'tx-001',
        delta,
        timestamp: Date.now(),
        hookResults: [hookResult]
      };

      const entry = await lockchain.writeReceipt(receipt);

      expect(entry).toBeDefined();
      expect(entry.signature).toBeDefined();

      // Verify integrity
      const verification = await lockchain.verifyIntegrity();

      expect(verification.valid).toBe(true);
    });

    it('should handle multi-agent consensus workflow', async () => {
      const resolutionLayer = createBrowserResolutionLayer();

      // Register agents
      ['agent-1', 'agent-2', 'agent-3'].forEach((id, index) => {
        resolutionLayer.registerAgent(id, {
          priority: index + 1,
          type: 'validator'
        });
      });

      // Submit proposals
      const proposals = [
        {
          id: 'p1',
          agentId: 'agent-1',
          delta: {
            additions: [
              quad(
                namedNode('http://example.org/resource'),
                namedNode('http://example.org/value'),
                literal('1')
              )
            ],
            removals: []
          }
        },
        {
          id: 'p2',
          agentId: 'agent-2',
          delta: {
            additions: [
              quad(
                namedNode('http://example.org/resource'),
                namedNode('http://example.org/value'),
                literal('2')
              )
            ],
            removals: []
          }
        }
      ];

      proposals.forEach(p => resolutionLayer.submitProposal(p));

      // Resolve
      const resolution = await resolutionLayer.resolveProposals('voting');

      expect(resolution).toBeDefined();
      expect(resolution.consensus).toBe(true);
      expect(resolution.confidence).toBeGreaterThan(0);
      expect(resolution.resolvedDelta).toBeDefined();
    });

    it('should integrate policy packs with hook execution', async () => {
      const hookManager = createBrowserKnowledgeHookManager();
      const policyManager = hookManager.policyPackManager;

      // Load policy pack
      await policyManager.loadPolicyPack('auth-policy', {
        name: 'auth-policy',
        hooks: [
          {
            id: 'auth-hook',
            mode: 'pre',
            meta: { name: 'auth-check' },
            run: async (event) => {
              return { authorized: event.userId === 'admin' };
            }
          }
        ]
      });

      policyManager.activatePolicyPack('auth-policy');

      // Get and execute hooks
      const activeHooks = policyManager.getActiveHooks();

      expect(activeHooks).toHaveLength(1);
    });
  });

  describe('Performance and Scalability', () => {
    it('should handle high-volume hook executions', async () => {
      const hooks = [];
      for (let i = 0; i < 50; i++) {
        hooks.push({
          meta: { name: `hook-${i}` },
          run: async () => ({ success: true, id: i })
        });
      }

      for (const hook of hooks) {
        await manager.addKnowledgeHook(hook);
      }

      const start = performance.now();
      const results = await manager.executeAllKnowledgeHooks({
        type: 'batch-test',
        store
      });
      const duration = performance.now() - start;

      expect(results).toHaveLength(50);
      expect(duration).toBeLessThan(1000); // <1s for 50 hooks
    });

    it('should handle large resolution workflows', async () => {
      const resolutionLayer = createBrowserResolutionLayer({
        maxProposals: 200
      });

      for (let i = 0; i < 100; i++) {
        resolutionLayer.submitProposal({
          id: `prop-${i}`,
          agentId: `agent-${i % 10}`,
          delta: { additions: [], removals: [] }
        });
      }

      const start = performance.now();
      const resolution = await resolutionLayer.resolveProposals();
      const duration = performance.now() - start;

      expect(resolution.proposals).toHaveLength(100);
      expect(duration).toBeLessThan(500); // <500ms for 100 proposals
    });
  });

  describe('Error Handling and Recovery', () => {
    it('should handle hook execution errors gracefully', async () => {
      const errorHook = {
        meta: { name: 'error-hook' },
        run: async () => {
          throw new Error('Intentional hook error');
        }
      };

      await manager.addKnowledgeHook(errorHook);

      const result = await manager.executeKnowledgeHook('error-hook', {
        type: 'test',
        store
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain('Intentional hook error');
    });

    it('should validate hook structure', async () => {
      const invalidHooks = [
        null,
        {},
        { meta: {} },
        { meta: { name: 'test' } },
        { run: () => {} }
      ];

      for (const hook of invalidHooks) {
        await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
      }
    });

    it('should prevent duplicate hook names', async () => {
      const hook1 = {
        meta: { name: 'duplicate' },
        run: async () => ({})
      };

      await manager.addKnowledgeHook(hook1);

      const hook2 = {
        meta: { name: 'duplicate' },
        run: async () => ({})
      };

      await expect(manager.addKnowledgeHook(hook2)).rejects.toThrow(/already exists/);
    });
  });

  describe('Browser-Specific Behaviors', () => {
    it('should use browser-compatible storage', () => {
      const lockchain = createBrowserLockchainWriter({
        storageType: 'localStorage'
      });

      expect(lockchain.config.storageType).toBe('localStorage');
      expect(lockchain.storage).toBeDefined();
    });

    it('should use Web Workers for sandboxing', () => {
      const sandbox = createEffectSandbox({ type: 'worker' });

      expect(sandbox.config.type).toBe('worker');
    });

    it('should handle browser environment correctly', () => {
      const fileResolver = createBrowserFileResolver();

      expect(fileResolver.config.basePath).toBe('/');
    });
  });
});
