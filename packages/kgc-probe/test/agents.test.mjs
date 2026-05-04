/**
 * @file Agent Tests - 10 Domain Agents
 * @description Comprehensive tests for all probe agents
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  AgentRegistry,
  createAgentRegistry,
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
} from '../src/agents/index.mjs';

// ============================================================================
// AGENT REGISTRY TESTS
// ============================================================================

describe('AgentRegistry', () => {
  let registry;

  beforeEach(() => {
    registry = createAgentRegistry();
  });

  it('should create registry with 10 default agents', () => {
    expect(registry.count()).toBe(10);
  });

  it('should list all agent IDs', () => {
    const ids = registry.list();
    expect(ids).toContain('orchestrator');
    expect(ids).toContain('runtime');
    expect(ids).toContain('filesystem');
    expect(ids).toContain('wasm');
    expect(ids).toContain('performance');
    expect(ids).toContain('network');
    expect(ids).toContain('tooling');
    expect(ids).toContain('storage');
    expect(ids).toContain('concurrency');
    expect(ids).toContain('system');
  });

  it('should get agent by ID', () => {
    const agent = registry.get('runtime');
    expect(agent).toBeDefined();
    expect(agent.id).toBe('runtime');
  });

  it('should return undefined for unknown agent', () => {
    const agent = registry.get('nonexistent');
    expect(agent).toBeUndefined();
  });

  it('should register custom agent', () => {
    const customAgent = new Agent('custom', 'custom', 'Custom agent');
    registry.register('custom', customAgent);
    expect(registry.count()).toBe(11);
    expect(registry.get('custom')).toBe(customAgent);
  });

  it('should run all agents in parallel', async () => {
    const observations = await registry.probeAll({});
    expect(Array.isArray(observations)).toBe(true);
    expect(observations.length).toBeGreaterThan(0);
  });
});

// ============================================================================
// BASE AGENT CLASS TESTS
// ============================================================================

describe('Agent (Base Class)', () => {
  it('should create agent with id, domain, description', () => {
    const agent = new Agent('test', 'testing', 'Test agent');
    expect(agent.id).toBe('test');
    expect(agent.domain).toBe('testing');
    expect(agent.description).toBe('Test agent');
    expect(agent.name).toBe('agent-test');
  });

  it('should return empty observations by default', async () => {
    const agent = new Agent('test', 'testing', 'Test');
    const obs = await agent.probe({});
    expect(obs).toEqual([]);
  });

  it('should have scan as alias for probe', async () => {
    const agent = new Agent('test', 'testing', 'Test');
    const probeResult = await agent.probe({});
    const scanResult = await agent.scan({});
    expect(probeResult).toEqual(scanResult);
  });
});

// ============================================================================
// ORCHESTRATOR AGENT TESTS
// ============================================================================

describe('OrchestratorAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createOrchestratorAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('orchestrator');
    expect(agent.domain).toBe('orchestration');
  });

  it('should produce observations about merge capability', async () => {
    const obs = await agent.probe({});
    expect(obs.length).toBeGreaterThan(0);

    const mergeObs = obs.find(o => o.predicate === 'probe:hasMergeCapability');
    expect(mergeObs).toBeDefined();
    expect(mergeObs.object).toBe('true');
  });

  it('should produce observations about receipt capability', async () => {
    const obs = await agent.probe({});
    const receiptObs = obs.find(o => o.predicate === 'probe:hasReceiptCapability');
    expect(receiptObs).toBeDefined();
    expect(receiptObs.object).toBe('true');
  });

  it('should report 10 agents', async () => {
    const obs = await agent.probe({});
    const countObs = obs.find(o => o.predicate === 'probe:agentCount');
    expect(countObs).toBeDefined();
    expect(countObs.object).toBe('10');
  });

  it('should list all 10 agent domains', () => {
    const domains = agent.getAgentDomains();
    expect(domains.length).toBe(10);
    expect(domains).toContain('orchestration');
    expect(domains).toContain('system');
  });
});

// ============================================================================
// RUNTIME AGENT TESTS
// ============================================================================

describe('RuntimeAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createRuntimeAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('runtime');
    expect(agent.domain).toBe('runtime');
  });

  it('should detect Node version', async () => {
    const obs = await agent.probe({});
    const nodeObs = obs.find(o => o.predicate === 'probe:nodeVersion');
    expect(nodeObs).toBeDefined();
    expect(nodeObs.object).toMatch(/^v\d+\.\d+\.\d+/);
  });

  it('should detect V8 version', async () => {
    const obs = await agent.probe({});
    const v8Obs = obs.find(o => o.predicate === 'probe:v8Version');
    expect(v8Obs).toBeDefined();
    expect(v8Obs.object).toMatch(/^\d+\.\d+/);
  });

  it('should detect WASM support', async () => {
    const obs = await agent.probe({});
    const wasmObs = obs.find(o => o.predicate === 'probe:wasmSupport');
    expect(wasmObs).toBeDefined();
    expect(['true', 'false']).toContain(wasmObs.object);
  });

  it('should detect worker threads support', async () => {
    const obs = await agent.probe({});
    const workerObs = obs.find(o => o.predicate === 'probe:workerThreads');
    expect(workerObs).toBeDefined();
    expect(['true', 'false']).toContain(workerObs.object);
  });
});

// ============================================================================
// FILESYSTEM AGENT TESTS
// ============================================================================

describe('FilesystemAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createFilesystemAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('filesystem');
    expect(agent.domain).toBe('filesystem');
  });

  it('should detect working directory', async () => {
    const obs = await agent.probe({});
    const cwdObs = obs.find(o => o.predicate === 'probe:workingDirectory');
    expect(cwdObs).toBeDefined();
    expect(cwdObs.object).toBe(process.cwd());
  });

  it('should detect max path length', async () => {
    const obs = await agent.probe({});
    const pathObs = obs.find(o => o.predicate === 'probe:maxPathLength');
    expect(pathObs).toBeDefined();
    expect(parseInt(pathObs.object)).toBeGreaterThan(0);
  });

  it('should test write capability', async () => {
    const obs = await agent.probe({});
    const writeObs = obs.find(o => o.predicate === 'probe:writeCapable');
    expect(writeObs).toBeDefined();
    expect(['true', 'false']).toContain(writeObs.object);
  });
});

// ============================================================================
// WASM AGENT TESTS
// ============================================================================

describe('WasmAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createWasmAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('wasm');
    expect(agent.domain).toBe('wasm');
  });

  it('should detect WASM availability', async () => {
    const obs = await agent.probe({});
    const wasmObs = obs.find(o => o.predicate === 'probe:available');
    expect(wasmObs).toBeDefined();
    expect(['true', 'false']).toContain(wasmObs.object);
  });

  it('should test WASM compile time', async () => {
    const obs = await agent.probe({});
    if (typeof WebAssembly !== 'undefined') {
      const compileObs = obs.find(o => o.predicate === 'probe:compileTime');
      expect(compileObs).toBeDefined();
    }
  });
});

// ============================================================================
// PERFORMANCE AGENT TESTS
// ============================================================================

describe('PerformanceAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createPerformanceAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('performance');
    expect(agent.domain).toBe('performance');
  });

  it('should benchmark JSON parsing', async () => {
    const obs = await agent.probe({});
    const jsonObs = obs.find(o => o.predicate === 'probe:jsonParseTime');
    expect(jsonObs).toBeDefined();
    expect(parseFloat(jsonObs.object)).toBeGreaterThan(0);
  });

  it('should measure hash throughput', async () => {
    const obs = await agent.probe({});
    const hashObs = obs.find(o => o.predicate === 'probe:hashThroughput');
    expect(hashObs).toBeDefined();
  });

  it('should measure timer resolution', async () => {
    const obs = await agent.probe({});
    const timerObs = obs.find(o => o.predicate === 'probe:timerResolution');
    expect(timerObs).toBeDefined();
    expect(timerObs.object).toContain('p50=');
    expect(timerObs.object).toContain('p99=');
  });
});

// ============================================================================
// NETWORK AGENT TESTS
// ============================================================================

describe('NetworkAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createNetworkAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('network');
    expect(agent.domain).toBe('network');
  });

  it('should check DNS availability', async () => {
    const obs = await agent.probe({});
    const dnsObs = obs.find(o => o.predicate === 'probe:dnsAvailable');
    expect(dnsObs).toBeDefined();
  });

  it('should report allowlist size', async () => {
    const obs = await agent.probe({ networkAllowlist: ['http://example.com'] });
    const allowlistObs = obs.find(o => o.predicate === 'probe:allowlistSize');
    expect(allowlistObs).toBeDefined();
    expect(allowlistObs.object).toBe('1');
  });

  it('should report max payload', async () => {
    const obs = await agent.probe({});
    const payloadObs = obs.find(o => o.predicate === 'probe:maxPayload');
    expect(payloadObs).toBeDefined();
  });
});

// ============================================================================
// TOOLING AGENT TESTS
// ============================================================================

describe('ToolingAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createToolingAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('tooling');
    expect(agent.domain).toBe('tooling');
  });

  it('should detect node availability', async () => {
    const obs = await agent.probe({});
    const nodeObs = obs.find(o => o.subject === 'probe:tooling:node');
    expect(nodeObs).toBeDefined();
    expect(nodeObs.object).toBe('true'); // node must be available
  });

  it('should check multiple tools', async () => {
    const obs = await agent.probe({});
    expect(obs.length).toBeGreaterThanOrEqual(4); // node, npm, git, pnpm
  });
});

// ============================================================================
// STORAGE AGENT TESTS
// ============================================================================

describe('StorageAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createStorageAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('storage');
    expect(agent.domain).toBe('storage');
  });

  it('should report available bytes', async () => {
    const obs = await agent.probe({});
    const bytesObs = obs.find(o => o.predicate === 'probe:availableBytes');
    expect(bytesObs).toBeDefined();
    expect(parseInt(bytesObs.object)).toBeGreaterThan(0);
  });

  it('should list persistence types', async () => {
    const obs = await agent.probe({});
    const typesObs = obs.find(o => o.predicate === 'probe:persistenceTypes');
    expect(typesObs).toBeDefined();
    expect(typesObs.object).toContain('memory');
    expect(typesObs.object).toContain('file');
    expect(typesObs.object).toContain('database');
  });

  it('should report temp directory', async () => {
    const obs = await agent.probe({});
    const tmpObs = obs.find(o => o.predicate === 'probe:tempDirectory');
    expect(tmpObs).toBeDefined();
    expect(tmpObs.object.length).toBeGreaterThan(0);
  });
});

// ============================================================================
// CONCURRENCY AGENT TESTS
// ============================================================================

describe('ConcurrencyAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createConcurrencyAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('concurrency');
    expect(agent.domain).toBe('concurrency');
  });

  it('should detect CPU count', async () => {
    const obs = await agent.probe({});
    const cpuObs = obs.find(o => o.predicate === 'probe:cpuCount');
    expect(cpuObs).toBeDefined();
    expect(parseInt(cpuObs.object)).toBeGreaterThan(0);
  });

  it('should check worker threads availability', async () => {
    const obs = await agent.probe({});
    const workerObs = obs.find(o => o.predicate === 'probe:workerThreadsAvailable');
    expect(workerObs).toBeDefined();
  });

  it('should measure event loop latency', async () => {
    const obs = await agent.probe({});
    const loopObs = obs.find(o => o.predicate === 'probe:eventLoopLatency');
    expect(loopObs).toBeDefined();
    expect(parseFloat(loopObs.object)).toBeGreaterThanOrEqual(0);
  });
});

// ============================================================================
// SYSTEM AGENT TESTS
// ============================================================================

describe('SystemAgent', () => {
  let agent;

  beforeEach(() => {
    agent = createSystemAgent();
  });

  it('should have correct identity', () => {
    expect(agent.id).toBe('system');
    expect(agent.domain).toBe('system');
  });

  it('should detect platform', async () => {
    const obs = await agent.probe({});
    const platformObs = obs.find(o => o.predicate === 'probe:platform');
    expect(platformObs).toBeDefined();
    expect(['linux', 'darwin', 'win32']).toContain(platformObs.object);
  });

  it('should detect architecture', async () => {
    const obs = await agent.probe({});
    const archObs = obs.find(o => o.predicate === 'probe:architecture');
    expect(archObs).toBeDefined();
    expect(['x64', 'arm64', 'arm', 'ia32']).toContain(archObs.object);
  });

  it('should check containerization', async () => {
    const obs = await agent.probe({});
    const containerObs = obs.find(o => o.predicate === 'probe:containerized');
    expect(containerObs).toBeDefined();
    expect(['true', 'false']).toContain(containerObs.object);
  });

  it('should report total memory', async () => {
    const obs = await agent.probe({});
    const memObs = obs.find(o => o.predicate === 'probe:totalMemory');
    expect(memObs).toBeDefined();
    expect(parseInt(memObs.object)).toBeGreaterThan(0);
  });
});

// ============================================================================
// OBSERVATION STRUCTURE TESTS
// ============================================================================

describe('Observation Structure', () => {
  it('should have all required fields', async () => {
    const agent = createSystemAgent();
    const obs = await agent.probe({});

    for (const o of obs) {
      expect(o).toHaveProperty('id');
      expect(o).toHaveProperty('agent');
      expect(o).toHaveProperty('timestamp');
      expect(o).toHaveProperty('kind');
      expect(o).toHaveProperty('severity');
      expect(o).toHaveProperty('subject');
      expect(o).toHaveProperty('evidence');
      expect(o).toHaveProperty('metrics');
    }
  });

  it('should have valid evidence structure', async () => {
    const agent = createRuntimeAgent();
    const obs = await agent.probe({});

    for (const o of obs) {
      expect(o.evidence).toHaveProperty('query');
      expect(o.evidence).toHaveProperty('result');
      expect(o.evidence).toHaveProperty('witnesses');
    }
  });

  it('should have valid metrics structure', async () => {
    const agent = createPerformanceAgent();
    const obs = await agent.probe({});

    for (const o of obs) {
      expect(o.metrics).toHaveProperty('confidence');
      expect(o.metrics).toHaveProperty('coverage');
      expect(o.metrics).toHaveProperty('latency_ms');
      expect(o.metrics.confidence).toBeGreaterThanOrEqual(0);
      expect(o.metrics.confidence).toBeLessThanOrEqual(1);
    }
  });
});
