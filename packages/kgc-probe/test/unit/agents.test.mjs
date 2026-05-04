/**
 * @fileoverview Unit Tests for Agents Module
 *
 * Tests agent registry and individual agents.
 *
 * @module @unrdf/kgc-probe/test/unit/agents
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createAgentRegistry,
  AgentRegistry,
  Agent,
  OrchestratorAgent,
  RuntimeAgent,
  FilesystemAgent,
  WasmAgent,
  PerformanceAgent,
  NetworkAgent,
  ToolingAgent,
  StorageAgent,
  ConcurrencyAgent,
  SystemAgent,
  createOrchestratorAgent,
  createRuntimeAgent,
  createFilesystemAgent,
  createWasmAgent,
  createPerformanceAgent,
  createNetworkAgent,
  createToolingAgent,
  createStorageAgent,
  createConcurrencyAgent,
  createSystemAgent
} from '../../src/agents/index.mjs';

describe('Agents Module', () => {
  describe('AgentRegistry', () => {
    let registry;

    beforeEach(() => {
      registry = createAgentRegistry();
    });

    it('should register 10 default agents', () => {
      expect(registry.count()).toBe(10);
    });

    it('should list all agent IDs', () => {
      const agents = registry.list();

      expect(agents).toContain('orchestrator');
      expect(agents).toContain('runtime');
      expect(agents).toContain('filesystem');
      expect(agents).toContain('wasm');
      expect(agents).toContain('performance');
      expect(agents).toContain('network');
      expect(agents).toContain('tooling');
      expect(agents).toContain('storage');
      expect(agents).toContain('concurrency');
      expect(agents).toContain('system');
    });

    it('should get agent by ID', () => {
      const agent = registry.get('orchestrator');

      expect(agent).toBeDefined();
      expect(agent.id).toBe('orchestrator');
      expect(agent.domain).toBe('orchestration');
    });

    it('should return undefined for nonexistent agent', () => {
      const agent = registry.get('nonexistent');
      expect(agent).toBeUndefined();
    });

    it('should register custom agent', () => {
      const customAgent = new Agent('custom', 'custom_kind', 'Custom agent');

      registry.register('custom', customAgent);

      expect(registry.count()).toBe(11);
      expect(registry.get('custom')).toBe(customAgent);
    });

    it('should allow adding custom agent', () => {
      const newAgent = new Agent('custom-new', 'new_domain', 'New custom agent');

      registry.register('custom-new', newAgent);

      expect(registry.count()).toBe(11);
      expect(registry.get('custom-new').domain).toBe('new_domain');
    });
  });

  describe('Base Agent Class', () => {
    it('should create agent with properties', () => {
      const agent = new Agent('test', 'test_domain', 'Test description');

      expect(agent.id).toBe('test');
      expect(agent.domain).toBe('test_domain');
      expect(agent.description).toBe('Test description');
    });

    it('should have scan method returning empty array', async () => {
      const agent = new Agent('test', 'test_domain', 'Test');

      const result = await agent.scan({});

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });
  });

  describe('OrchestratorAgent', () => {
    it('should have correct properties', () => {
      const agent = createOrchestratorAgent();

      expect(agent.id).toBe('orchestrator');
      expect(agent.domain).toBe('orchestration');
      expect(agent.description).toContain('Coordinates');
    });

    it('should return observations on scan', async () => {
      const agent = createOrchestratorAgent();
      const config = { universe_id: 'test' };

      const result = await agent.scan(config);

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);

      const obs = result[0];
      expect(obs.agent).toBe('orchestrator');
      expect(obs.id).toBeDefined();
      expect(obs.timestamp).toBeDefined();
      expect(obs.metrics).toBeDefined();
    });
  });

  describe('RuntimeAgent', () => {
    it('should have correct properties', () => {
      const agent = createRuntimeAgent();

      expect(agent.id).toBe('runtime');
      expect(agent.domain).toBe('runtime');
    });

    it('should scan without errors', async () => {
      const agent = createRuntimeAgent();
      const result = await agent.scan({});

      expect(Array.isArray(result)).toBe(true);
    });
  });

  describe('FilesystemAgent', () => {
    it('should have correct properties', () => {
      const agent = createFilesystemAgent();

      expect(agent.id).toBe('filesystem');
      expect(agent.domain).toBe('filesystem');
    });
  });

  describe('WasmAgent', () => {
    it('should have correct properties', () => {
      const agent = createWasmAgent();

      expect(agent.id).toBe('wasm');
      expect(agent.domain).toBe('wasm');
    });
  });

  describe('PerformanceAgent', () => {
    it('should have correct properties', () => {
      const agent = createPerformanceAgent();

      expect(agent.id).toBe('performance');
      expect(agent.domain).toBe('performance');
    });
  });

  describe('NetworkAgent', () => {
    it('should have correct properties', () => {
      const agent = createNetworkAgent();

      expect(agent.id).toBe('network');
      expect(agent.domain).toBe('network');
    });
  });

  describe('ToolingAgent', () => {
    it('should have correct properties', () => {
      const agent = createToolingAgent();

      expect(agent.id).toBe('tooling');
      expect(agent.domain).toBe('tooling');
    });
  });

  describe('StorageAgent', () => {
    it('should have correct properties', () => {
      const agent = createStorageAgent();

      expect(agent.id).toBe('storage');
      expect(agent.domain).toBe('storage');
    });
  });

  describe('ConcurrencyAgent', () => {
    it('should have correct properties', () => {
      const agent = createConcurrencyAgent();

      expect(agent.id).toBe('concurrency');
      expect(agent.domain).toBe('concurrency');
    });
  });

  describe('SystemAgent', () => {
    it('should have correct properties', () => {
      const agent = createSystemAgent();

      expect(agent.id).toBe('system');
      expect(agent.domain).toBe('system');
    });
  });

  describe('All Agents Scan', () => {
    it('should execute scan on all agents without errors', async () => {
      const registry = createAgentRegistry();
      const config = { universe_id: 'test' };

      for (const agentId of registry.list()) {
        const agent = registry.get(agentId);
        const result = await agent.scan(config);

        expect(Array.isArray(result)).toBe(true);
      }
    });
  });
});
