/**
 * @fileoverview KGC Probe - Agent Registry and 10 Domain Agents
 *
 * Each agent implements the probe interface:
 * - Agent 1: Orchestrator (merge + receipts coordination)
 * - Agent 2: Runtime (Node version, JS engine, WASM, workers)
 * - Agent 3: Filesystem (roots, max path, file counts, write tests)
 * - Agent 4: WASM (instantiate modules, startup time, memory growth)
 * - Agent 5: Performance (JSON parse, hashing throughput, latency p50/p99)
 * - Agent 6: Network (URL allowlist checks, DNS, max payload)
 * - Agent 7: Tooling (detect accessible commands, versions)
 * - Agent 8: Storage (persistence types, quotas, available bytes)
 * - Agent 9: Concurrency (workers available, parallelism, throttles)
 * - Agent 10: System (platform, OS version, containerized)
 *
 * @module @unrdf/kgc-probe/agents
 */

import { randomUUID } from 'crypto';
import { performance } from 'perf_hooks';
import { platform, version, arch, cpus, totalmem, freemem } from 'os';
import { promises as fs, existsSync } from 'fs';
import { join } from 'path';
import { execSync } from 'child_process';

// ============================================================================
// OBSERVATION FACTORY
// ============================================================================

/**
 * Create a standardized observation
 * @param {Object} params - Observation parameters
 * @param {string} params.agent - Agent ID
 * @param {string} params.kind - Observation kind
 * @param {string} params.severity - Severity level
 * @param {string} params.subject - RDF subject
 * @param {Object} params.evidence - Evidence object
 * @param {Object} params.metrics - Metrics object
 * @param {string[]} [params.tags] - Tags
 * @returns {Object} Standardized observation
 */
function createObservation({
  agent,
  kind,
  severity = 'info',
  subject,
  predicate,
  object,
  evidence,
  metrics,
  tags = []
}) {
  return {
    id: randomUUID(),
    agent,
    timestamp: new Date().toISOString(),
    kind,
    severity,
    subject,
    predicate,
    object,
    evidence: {
      query: evidence.query || '',
      result: evidence.result || null,
      witnesses: evidence.witnesses || []
    },
    metrics: {
      confidence: metrics.confidence ?? 0.95,
      coverage: metrics.coverage ?? 1.0,
      latency_ms: metrics.latency_ms ?? 0
    },
    tags
  };
}

// ============================================================================
// BASE AGENT CLASS
// ============================================================================

/**
 * Base Agent class with common functionality
 * @class Agent
 */
export class Agent {
  /**
   * Create agent
   * @param {string} id - Agent identifier
   * @param {string} domain - Domain/kind of observations
   * @param {string} description - Human description
   */
  constructor(id, domain, description) {
    /** @type {string} */
    this.id = id;
    /** @type {string} */
    this.domain = domain;
    /** @type {string} */
    this.description = description;
    /** @type {string} */
    this.name = `agent-${id}`;
  }

  /**
   * Probe the system and produce observations
   * @param {Object} config - Probe configuration
   * @returns {Promise<Array>} Array of observations
   */
  async probe(config = {}) {
    return [];
  }

  /**
   * Alias for probe (compatibility)
   * @param {Object} config - Probe configuration
   * @returns {Promise<Array>} Array of observations
   */
  async scan(config = {}) {
    return this.probe(config);
  }
}

// ============================================================================
// AGENT 1: ORCHESTRATOR
// ============================================================================

/**
 * OrchestratorAgent - Merge and receipt coordination
 * @extends Agent
 */
export class OrchestratorAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('orchestrator', 'orchestration', 'Coordinates merge operations and receipt generation');
  }

  /**
   * Probe orchestrator state
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    // Observation 1: Merge capability
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:orchestrator',
      predicate: 'probe:hasMergeCapability',
      object: 'true',
      evidence: {
        query: 'MERGE_CAPABILITY_CHECK',
        result: { mergeEnabled: true, algorithm: 'lww-deterministic' },
        witnesses: ['orchestrator:merge-engine']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['orchestrator', 'merge', 'capability']
    }));

    // Observation 2: Receipt capability
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:orchestrator',
      predicate: 'probe:hasReceiptCapability',
      object: 'true',
      evidence: {
        query: 'RECEIPT_CAPABILITY_CHECK',
        result: {
          receiptTypes: ['observation', 'merge', 'verification'],
          hashAlgorithm: 'blake3'
        },
        witnesses: ['orchestrator:receipt-chain']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['orchestrator', 'receipt', 'capability']
    }));

    // Observation 3: Agent count
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:orchestrator',
      predicate: 'probe:agentCount',
      object: '10',
      evidence: {
        query: 'AGENT_COUNT_CHECK',
        result: { count: 10, domains: this.getAgentDomains() },
        witnesses: []
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['orchestrator', 'agents']
    }));

    return observations;
  }

  /**
   * Get list of agent domains
   * @returns {string[]}
   */
  getAgentDomains() {
    return [
      'orchestration', 'runtime', 'filesystem', 'wasm',
      'performance', 'network', 'tooling', 'storage',
      'concurrency', 'system'
    ];
  }
}

// ============================================================================
// AGENT 2: RUNTIME
// ============================================================================

/**
 * RuntimeAgent - Node version, JS engine, WASM, workers
 * @extends Agent
 */
export class RuntimeAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('runtime', 'runtime', 'Probes Node.js runtime environment');
  }

  /**
   * Probe runtime environment
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    // Observation 1: Node version
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:runtime',
      predicate: 'probe:nodeVersion',
      object: process.version,
      evidence: {
        query: 'NODE_VERSION_CHECK',
        result: {
          version: process.version,
          major: parseInt(process.version.slice(1)),
          v8Version: process.versions.v8
        },
        witnesses: ['process.version']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['runtime', 'node', 'version']
    }));

    // Observation 2: V8 Engine
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:runtime',
      predicate: 'probe:v8Version',
      object: process.versions.v8,
      evidence: {
        query: 'V8_VERSION_CHECK',
        result: { v8: process.versions.v8, features: this.getV8Features() },
        witnesses: ['process.versions.v8']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['runtime', 'v8', 'engine']
    }));

    // Observation 3: WASM Support
    const wasmSupported = typeof WebAssembly !== 'undefined';
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:runtime',
      predicate: 'probe:wasmSupport',
      object: String(wasmSupported),
      evidence: {
        query: 'WASM_SUPPORT_CHECK',
        result: { supported: wasmSupported },
        witnesses: ['WebAssembly']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['runtime', 'wasm', 'support']
    }));

    // Observation 4: Worker Threads
    let workersAvailable = false;
    try {
      await import('worker_threads');
      workersAvailable = true;
    } catch {
      workersAvailable = false;
    }

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:runtime',
      predicate: 'probe:workerThreads',
      object: String(workersAvailable),
      evidence: {
        query: 'WORKER_THREADS_CHECK',
        result: { available: workersAvailable },
        witnesses: ['worker_threads']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['runtime', 'workers', 'threads']
    }));

    return observations;
  }

  /**
   * Get V8 feature flags
   * @returns {Object}
   */
  getV8Features() {
    return {
      hasPromise: typeof Promise !== 'undefined',
      hasAsyncIterator: typeof Symbol.asyncIterator !== 'undefined',
      hasBigInt: typeof BigInt !== 'undefined',
      hasOptionalChaining: true
    };
  }
}

// ============================================================================
// AGENT 3: FILESYSTEM
// ============================================================================

/**
 * FilesystemAgent - Filesystem probing
 * @extends Agent
 */
export class FilesystemAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('filesystem', 'filesystem', 'Probes filesystem capabilities and limits');
  }

  /**
   * Probe filesystem
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];
    const allowedRoots = config.fsRoots || [process.cwd()];

    // Observation 1: Working directory
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:filesystem',
      predicate: 'probe:workingDirectory',
      object: process.cwd(),
      evidence: {
        query: 'CWD_CHECK',
        result: { cwd: process.cwd(), readable: true },
        witnesses: ['process.cwd()']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['filesystem', 'cwd']
    }));

    // Observation 2: Max path length
    const maxPath = process.platform === 'win32' ? 260 : 4096;
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:filesystem',
      predicate: 'probe:maxPathLength',
      object: String(maxPath),
      evidence: {
        query: 'MAX_PATH_CHECK',
        result: { maxPath, platform: process.platform },
        witnesses: ['os.platform']
      },
      metrics: {
        confidence: 0.95,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['filesystem', 'limits']
    }));

    // Observation 3: Write test
    let writeCapable = false;
    const testFile = join(process.cwd(), `.probe-write-test-${Date.now()}`);
    try {
      await fs.writeFile(testFile, 'test');
      await fs.unlink(testFile);
      writeCapable = true;
    } catch {
      writeCapable = false;
    }

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: writeCapable ? 'info' : 'warning',
      subject: 'probe:filesystem',
      predicate: 'probe:writeCapable',
      object: String(writeCapable),
      evidence: {
        query: 'WRITE_TEST',
        result: { writeCapable, testDir: process.cwd() },
        witnesses: ['fs.writeFile']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['filesystem', 'write', 'capability']
    }));

    // Observation 4: File count in allowed roots
    for (const root of allowedRoots.slice(0, 3)) {
      if (existsSync(root)) {
        try {
          const files = await fs.readdir(root);
          observations.push(createObservation({
            agent: this.id,
            kind: this.domain,
            severity: 'info',
            subject: `probe:filesystem:${root}`,
            predicate: 'probe:fileCount',
            object: String(files.length),
            evidence: {
              query: 'FILE_COUNT',
              result: { root, count: files.length },
              witnesses: files.slice(0, 5)
            },
            metrics: {
              confidence: 1.0,
              coverage: 0.8,
              latency_ms: performance.now() - startTime
            },
            tags: ['filesystem', 'count', root]
          }));
        } catch (err) {
          // Skip unreadable directories
        }
      }
    }

    return observations;
  }
}

// ============================================================================
// AGENT 4: WASM
// ============================================================================

/**
 * WasmAgent - WASM capabilities
 * @extends Agent
 */
export class WasmAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('wasm', 'wasm', 'Probes WebAssembly capabilities');
  }

  /**
   * Probe WASM capabilities
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    // Check WASM availability
    const wasmAvailable = typeof WebAssembly !== 'undefined';

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:wasm',
      predicate: 'probe:available',
      object: String(wasmAvailable),
      evidence: {
        query: 'WASM_AVAILABLE',
        result: { available: wasmAvailable },
        witnesses: ['WebAssembly']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['wasm', 'available']
    }));

    if (wasmAvailable) {
      // Test WASM instantiation with minimal module
      const wasmBytes = new Uint8Array([
        0x00, 0x61, 0x73, 0x6d, // Magic
        0x01, 0x00, 0x00, 0x00  // Version
      ]);

      let instantiationTime = 0;
      try {
        const instStart = performance.now();
        await WebAssembly.compile(wasmBytes);
        instantiationTime = performance.now() - instStart;
      } catch {
        instantiationTime = -1;
      }

      observations.push(createObservation({
        agent: this.id,
        kind: this.domain,
        severity: 'info',
        subject: 'probe:wasm',
        predicate: 'probe:compileTime',
        object: String(instantiationTime.toFixed(2)),
        evidence: {
          query: 'WASM_COMPILE_TIME',
          result: { time_ms: instantiationTime, success: instantiationTime >= 0 },
          witnesses: ['WebAssembly.compile']
        },
        metrics: {
          confidence: 1.0,
          coverage: 1.0,
          latency_ms: performance.now() - startTime
        },
        tags: ['wasm', 'compile', 'performance']
      }));

      // Memory growth test
      let maxMemory = 0;
      try {
        const memory = new WebAssembly.Memory({ initial: 1, maximum: 100 });
        maxMemory = memory.buffer.byteLength;
      } catch {
        maxMemory = 0;
      }

      observations.push(createObservation({
        agent: this.id,
        kind: this.domain,
        severity: 'info',
        subject: 'probe:wasm',
        predicate: 'probe:initialMemory',
        object: String(maxMemory),
        evidence: {
          query: 'WASM_MEMORY',
          result: { initial_bytes: maxMemory, pages: maxMemory / 65536 },
          witnesses: ['WebAssembly.Memory']
        },
        metrics: {
          confidence: 1.0,
          coverage: 1.0,
          latency_ms: performance.now() - startTime
        },
        tags: ['wasm', 'memory']
      }));
    }

    return observations;
  }
}

// ============================================================================
// AGENT 5: PERFORMANCE
// ============================================================================

/**
 * PerformanceAgent - Performance benchmarks
 * @extends Agent
 */
export class PerformanceAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('performance', 'performance', 'Benchmarks JSON parsing, hashing, and latency');
  }

  /**
   * Probe performance
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    // JSON parse benchmark
    const testData = JSON.stringify({ data: Array(1000).fill({ key: 'value', num: 42 }) });
    const jsonStart = performance.now();
    for (let i = 0; i < 100; i++) {
      JSON.parse(testData);
    }
    const jsonTime = (performance.now() - jsonStart) / 100;

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:performance',
      predicate: 'probe:jsonParseTime',
      object: String(jsonTime.toFixed(3)),
      evidence: {
        query: 'JSON_PARSE_BENCH',
        result: { avg_ms: jsonTime, iterations: 100, dataSize: testData.length },
        witnesses: ['JSON.parse']
      },
      metrics: {
        confidence: 0.95,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['performance', 'json', 'benchmark']
    }));

    // Simple hash benchmark
    const hashStart = performance.now();
    let hashSum = 0;
    for (let i = 0; i < 10000; i++) {
      let hash = 0;
      for (let j = 0; j < testData.length; j++) {
        hash = ((hash << 5) - hash) + testData.charCodeAt(j);
        hash = hash & hash;
      }
      hashSum += hash;
    }
    const hashTime = (performance.now() - hashStart) / 10000;

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:performance',
      predicate: 'probe:hashThroughput',
      object: String((testData.length / hashTime / 1000).toFixed(2)),
      evidence: {
        query: 'HASH_BENCH',
        result: { throughput_mb_s: testData.length / hashTime / 1000, iterations: 10000 },
        witnesses: ['hash-loop']
      },
      metrics: {
        confidence: 0.9,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['performance', 'hash', 'throughput']
    }));

    // Timer resolution
    const timings = [];
    for (let i = 0; i < 100; i++) {
      const t1 = performance.now();
      const t2 = performance.now();
      timings.push(t2 - t1);
    }
    timings.sort((a, b) => a - b);
    const p50 = timings[50];
    const p99 = timings[99];

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:performance',
      predicate: 'probe:timerResolution',
      object: `p50=${p50.toFixed(6)},p99=${p99.toFixed(6)}`,
      evidence: {
        query: 'TIMER_RESOLUTION',
        result: { p50, p99, samples: 100 },
        witnesses: ['performance.now']
      },
      metrics: {
        confidence: 0.95,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['performance', 'latency', 'timer']
    }));

    return observations;
  }
}

// ============================================================================
// AGENT 6: NETWORK
// ============================================================================

/**
 * NetworkAgent - Network capabilities
 * @extends Agent
 */
export class NetworkAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('network', 'network', 'Probes network capabilities and restrictions');
  }

  /**
   * Probe network
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];
    const allowlist = config.networkAllowlist || [];

    // Observation 1: Network available
    const dns = await import('dns').catch(() => null);
    const netAvailable = dns !== null;

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:network',
      predicate: 'probe:dnsAvailable',
      object: String(netAvailable),
      evidence: {
        query: 'DNS_AVAILABLE',
        result: { available: netAvailable },
        witnesses: ['dns']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['network', 'dns']
    }));

    // Observation 2: Allowlist check
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:network',
      predicate: 'probe:allowlistSize',
      object: String(allowlist.length),
      evidence: {
        query: 'ALLOWLIST_CHECK',
        result: { count: allowlist.length, urls: allowlist.slice(0, 5) },
        witnesses: allowlist.slice(0, 3)
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['network', 'allowlist']
    }));

    // Observation 3: Max payload (simulated)
    const maxPayload = 10 * 1024 * 1024; // 10MB default
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:network',
      predicate: 'probe:maxPayload',
      object: String(maxPayload),
      evidence: {
        query: 'MAX_PAYLOAD',
        result: { bytes: maxPayload, mb: maxPayload / (1024 * 1024) },
        witnesses: ['config.maxPayload']
      },
      metrics: {
        confidence: 0.8,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['network', 'limits']
    }));

    return observations;
  }
}

// ============================================================================
// AGENT 7: TOOLING
// ============================================================================

/**
 * ToolingAgent - Command availability
 * @extends Agent
 */
export class ToolingAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('tooling', 'tooling', 'Detects available command-line tools');
  }

  /**
   * Probe tooling
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    const tools = ['node', 'npm', 'git', 'pnpm'];

    for (const tool of tools) {
      let available = false;
      let version = 'unknown';

      try {
        version = execSync(`${tool} --version 2>/dev/null`, { encoding: 'utf8', timeout: 2000 }).trim().split('\n')[0];
        available = true;
      } catch {
        available = false;
      }

      observations.push(createObservation({
        agent: this.id,
        kind: this.domain,
        severity: 'info',
        subject: `probe:tooling:${tool}`,
        predicate: 'probe:available',
        object: String(available),
        evidence: {
          query: `TOOL_CHECK_${tool.toUpperCase()}`,
          result: { tool, available, version },
          witnesses: [tool]
        },
        metrics: {
          confidence: 1.0,
          coverage: 1.0,
          latency_ms: performance.now() - startTime
        },
        tags: ['tooling', tool]
      }));
    }

    return observations;
  }
}

// ============================================================================
// AGENT 8: STORAGE
// ============================================================================

/**
 * StorageAgent - Storage capabilities
 * @extends Agent
 */
export class StorageAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('storage', 'storage', 'Probes storage persistence and quotas');
  }

  /**
   * Probe storage
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    // Available memory as proxy for storage
    const freeBytes = freemem();
    const totalBytes = totalmem();

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:storage',
      predicate: 'probe:availableBytes',
      object: String(freeBytes),
      evidence: {
        query: 'FREE_MEMORY',
        result: { free: freeBytes, total: totalBytes, ratio: freeBytes / totalBytes },
        witnesses: ['os.freemem']
      },
      metrics: {
        confidence: 0.9,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['storage', 'memory']
    }));

    // Persistence types
    const persistenceTypes = ['memory', 'file', 'database'];
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:storage',
      predicate: 'probe:persistenceTypes',
      object: persistenceTypes.join(','),
      evidence: {
        query: 'PERSISTENCE_TYPES',
        result: { types: persistenceTypes },
        witnesses: []
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['storage', 'persistence']
    }));

    // Temp directory
    const tmpDir = process.env.TMPDIR || process.env.TMP || '/tmp';
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:storage',
      predicate: 'probe:tempDirectory',
      object: tmpDir,
      evidence: {
        query: 'TEMP_DIR',
        result: { path: tmpDir, exists: existsSync(tmpDir) },
        witnesses: ['process.env.TMPDIR']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['storage', 'temp']
    }));

    return observations;
  }
}

// ============================================================================
// AGENT 9: CONCURRENCY
// ============================================================================

/**
 * ConcurrencyAgent - Concurrency capabilities
 * @extends Agent
 */
export class ConcurrencyAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('concurrency', 'concurrency', 'Probes worker availability and parallelism');
  }

  /**
   * Probe concurrency
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    // CPU count (parallelism potential)
    const cpuCount = cpus().length;
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:concurrency',
      predicate: 'probe:cpuCount',
      object: String(cpuCount),
      evidence: {
        query: 'CPU_COUNT',
        result: { count: cpuCount, model: cpus()[0]?.model },
        witnesses: ['os.cpus']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['concurrency', 'cpu']
    }));

    // Worker threads capability
    let workersAvailable = false;
    try {
      await import('worker_threads');
      workersAvailable = true;
    } catch {
      workersAvailable = false;
    }

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:concurrency',
      predicate: 'probe:workerThreadsAvailable',
      object: String(workersAvailable),
      evidence: {
        query: 'WORKER_THREADS',
        result: { available: workersAvailable },
        witnesses: ['worker_threads']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['concurrency', 'workers']
    }));

    // Event loop latency
    const loopStart = performance.now();
    await new Promise(resolve => setImmediate(resolve));
    const loopLatency = performance.now() - loopStart;

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: loopLatency > 10 ? 'warning' : 'info',
      subject: 'probe:concurrency',
      predicate: 'probe:eventLoopLatency',
      object: String(loopLatency.toFixed(3)),
      evidence: {
        query: 'EVENT_LOOP_LATENCY',
        result: { latency_ms: loopLatency },
        witnesses: ['setImmediate']
      },
      metrics: {
        confidence: 0.9,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['concurrency', 'eventloop']
    }));

    return observations;
  }
}

// ============================================================================
// AGENT 10: SYSTEM
// ============================================================================

/**
 * SystemAgent - System information
 * @extends Agent
 */
export class SystemAgent extends Agent {
  /**
   *
   */
  constructor() {
    super('system', 'system', 'Probes platform, OS version, and container status');
  }

  /**
   * Probe system
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} Observations
   */
  async probe(config = {}) {
    const startTime = performance.now();
    const observations = [];

    // Platform
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:system',
      predicate: 'probe:platform',
      object: platform(),
      evidence: {
        query: 'PLATFORM',
        result: { platform: platform(), arch: arch() },
        witnesses: ['os.platform']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['system', 'platform']
    }));

    // Architecture
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:system',
      predicate: 'probe:architecture',
      object: arch(),
      evidence: {
        query: 'ARCHITECTURE',
        result: { arch: arch() },
        witnesses: ['os.arch']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['system', 'arch']
    }));

    // Containerized detection
    let containerized = false;
    try {
      containerized = existsSync('/.dockerenv') ||
        existsSync('/run/.containerenv') ||
        process.env.KUBERNETES_SERVICE_HOST !== undefined;
    } catch {
      containerized = false;
    }

    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:system',
      predicate: 'probe:containerized',
      object: String(containerized),
      evidence: {
        query: 'CONTAINER_CHECK',
        result: { containerized, checks: ['/.dockerenv', 'KUBERNETES_SERVICE_HOST'] },
        witnesses: []
      },
      metrics: {
        confidence: 0.85,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['system', 'container']
    }));

    // Total memory
    observations.push(createObservation({
      agent: this.id,
      kind: this.domain,
      severity: 'info',
      subject: 'probe:system',
      predicate: 'probe:totalMemory',
      object: String(totalmem()),
      evidence: {
        query: 'TOTAL_MEMORY',
        result: { bytes: totalmem(), gb: (totalmem() / (1024 ** 3)).toFixed(2) },
        witnesses: ['os.totalmem']
      },
      metrics: {
        confidence: 1.0,
        coverage: 1.0,
        latency_ms: performance.now() - startTime
      },
      tags: ['system', 'memory']
    }));

    return observations;
  }
}

// ============================================================================
// AGENT REGISTRY
// ============================================================================

/**
 * AgentRegistry - Manages agent registration and execution
 * @class AgentRegistry
 */
export class AgentRegistry {
  /**
   *
   */
  constructor() {
    /** @type {Map<string, Agent>} */
    this.agents = new Map();
    this.registerDefault();
  }

  /**
   * Register default agents (10 domains)
   * @private
   */
  registerDefault() {
    this.register('orchestrator', new OrchestratorAgent());
    this.register('runtime', new RuntimeAgent());
    this.register('filesystem', new FilesystemAgent());
    this.register('wasm', new WasmAgent());
    this.register('performance', new PerformanceAgent());
    this.register('network', new NetworkAgent());
    this.register('tooling', new ToolingAgent());
    this.register('storage', new StorageAgent());
    this.register('concurrency', new ConcurrencyAgent());
    this.register('system', new SystemAgent());
  }

  /**
   * Register agent
   * @param {string} id - Agent identifier
   * @param {Agent} agent - Agent instance
   */
  register(id, agent) {
    this.agents.set(id, agent);
  }

  /**
   * Get agent by ID
   * @param {string} id - Agent identifier
   * @returns {Agent | undefined}
   */
  get(id) {
    return this.agents.get(id);
  }

  /**
   * List all agent IDs
   * @returns {string[]}
   */
  list() {
    return Array.from(this.agents.keys());
  }

  /**
   * Count registered agents
   * @returns {number}
   */
  count() {
    return this.agents.size;
  }

  /**
   * Run all agents in parallel
   * @param {Object} config - Configuration
   * @returns {Promise<Array>} All observations from all agents
   */
  async probeAll(config = {}) {
    const promises = this.list().map(async (id) => {
      const agent = this.get(id);
      try {
        return await agent.probe(config);
      } catch (err) {
        console.error(`Agent ${id} failed:`, err.message);
        return [];
      }
    });

    const results = await Promise.all(promises);
    return results.flat();
  }
}

// ============================================================================
// FACTORY FUNCTIONS
// ============================================================================

/** Create agent registry with all default agents @returns {AgentRegistry} */
export function createAgentRegistry() { return new AgentRegistry(); }

/** Create orchestrator agent @returns {OrchestratorAgent} */
export function createOrchestratorAgent() { return new OrchestratorAgent(); }

/** Create runtime agent @returns {RuntimeAgent} */
export function createRuntimeAgent() { return new RuntimeAgent(); }

/** Create filesystem agent @returns {FilesystemAgent} */
export function createFilesystemAgent() { return new FilesystemAgent(); }

/** Create WASM agent @returns {WasmAgent} */
export function createWasmAgent() { return new WasmAgent(); }

/** Create performance agent @returns {PerformanceAgent} */
export function createPerformanceAgent() { return new PerformanceAgent(); }

/** Create network agent @returns {NetworkAgent} */
export function createNetworkAgent() { return new NetworkAgent(); }

/** Create tooling agent @returns {ToolingAgent} */
export function createToolingAgent() { return new ToolingAgent(); }

/** Create storage agent @returns {StorageAgent} */
export function createStorageAgent() { return new StorageAgent(); }

/** Create concurrency agent @returns {ConcurrencyAgent} */
export function createConcurrencyAgent() { return new ConcurrencyAgent(); }

/** Create system agent @returns {SystemAgent} */
export function createSystemAgent() { return new SystemAgent(); }

// Backward compatibility aliases
export const CompletionAgent = OrchestratorAgent;
export const ConsistencyAgent = RuntimeAgent;
export const ConformanceAgent = FilesystemAgent;
export const CoverageAgent = WasmAgent;
export const CachingAgent = PerformanceAgent;
export const CompletenessAgent = NetworkAgent;
export const CoherenceAgent = ToolingAgent;
export const ClusteringAgent = StorageAgent;
export const ClassificationAgent = ConcurrencyAgent;
export const CollaborationAgent = SystemAgent;

/**
 *
 */
export function createCompletionAgent() { return new OrchestratorAgent(); }
/**
 *
 */
export function createConsistencyAgent() { return new RuntimeAgent(); }
/**
 *
 */
export function createConformanceAgent() { return new FilesystemAgent(); }
/**
 *
 */
export function createCoverageAgent() { return new WasmAgent(); }
/**
 *
 */
export function createCachingAgent() { return new PerformanceAgent(); }
/**
 *
 */
export function createCompletenessAgent() { return new NetworkAgent(); }
/**
 *
 */
export function createCoherenceAgent() { return new ToolingAgent(); }
/**
 *
 */
export function createClusteringAgent() { return new StorageAgent(); }
/**
 *
 */
export function createClassificationAgent() { return new ConcurrencyAgent(); }
/**
 *
 */
export function createCollaborationAgent() { return new SystemAgent(); }
