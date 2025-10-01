/**
 * @file Browser-Compatible Knowledge Engine
 * @module knowledge-engine-browser
 * 
 * @description
 * Browser-compatible version of the knowledge engine with Node.js APIs
 * replaced by browser-compatible alternatives.
 */

import { isBrowser } from './browser-shims.mjs';
import { randomUUID, path, fs } from './browser-shims.mjs';
import { process as browserProcess } from './browser-shims.mjs';

// Browser-compatible versions
import { EffectSandbox, createEffectSandbox } from './effect-sandbox-browser.mjs';
import { BrowserLockchainWriter, createBrowserLockchainWriter } from './lockchain-writer-browser.mjs';

// Re-export schemas (these are already browser-compatible)
export * from './schemas.mjs';

// Core Engine Components (Browser-compatible)
import { KnowledgeHookManager } from './knowledge-hook-manager.mjs';
import { QueryOptimizer } from './query-optimizer.mjs';
import { QueryEngine } from '@comunica/query-sparql';

// Browser-compatible Transaction Manager base class
class BrowserTransactionManager {
  constructor(options = {}) {
    this.options = {
      basePath: options.basePath || '/',
      strictMode: options.strictMode || false,
      enableConditionEvaluation: options.enableConditionEvaluation ?? true,
      maxHooks: options.maxHooks || 100,
      timeout: options.timeout || 30000,
      enableCache: options.enableCache ?? true,
      cacheMaxAge: options.cacheMaxAge || 300000,
      enableMetrics: options.enableMetrics ?? true,
      logLevel: options.logLevel || 'info',
      ...options
    };
    
    this.hooks = [];
    this.executionCount = 0;
    this.totalDuration = 0;
  }

  getStats() {
    return {
      hooks: this.hooks.length,
      executions: this.executionCount,
      averageDuration: this.executionCount > 0 ? this.totalDuration / this.executionCount : 0,
      config: this.options
    };
  }
}

// Browser-compatible hook executor
export class BrowserHookExecutor {
  constructor(config = {}) {
    this.config = {
      basePath: config.basePath || '/',
      strictMode: config.strictMode || false,
      enableConditionEvaluation: config.enableConditionEvaluation ?? true,
      enableMetrics: config.enableMetrics ?? true,
      timeout: config.timeout || 30000,
      ...config
    };
    
    this.metrics = {
      executions: 0,
      errors: 0,
      totalDuration: 0,
      cacheHits: 0,
      cacheMisses: 0
    };
    
    this.cache = new Map();
  }

  async execute(hook, event, options = {}) {
    const startTime = Date.now();
    this.metrics.executions++;
    
    try {
      let result;
      
      if (hook.run) {
        result = await hook.run(event);
      } else {
        result = { success: true };
      }
      
      const duration = Date.now() - startTime;
      this.metrics.totalDuration += duration;
      
      return {
        success: true,
        result,
        duration,
        timestamp: Date.now()
      };
    } catch (error) {
      this.metrics.errors++;
      const duration = Date.now() - startTime;
      this.metrics.totalDuration += duration;
      
      return {
        success: false,
        error: error.message,
        duration,
        timestamp: Date.now()
      };
    }
  }

  async executeAll(hooks, event, options = {}) {
    const results = [];
    
    for (const hook of hooks) {
      try {
        const result = await this.execute(hook, event, options);
        results.push(result);
      } catch (error) {
        results.push({
          success: false,
          error: error.message,
          hook: hook.meta?.name || 'unknown'
        });
      }
    }
    
    return results;
  }

  getMetrics() {
    return { ...this.metrics };
  }

  clearCache() {
    this.cache.clear();
  }
}

// Browser-compatible condition evaluator
export class BrowserConditionEvaluator {
  constructor(config = {}) {
    this.config = {
      basePath: config.basePath || '/',
      enableCache: config.enableCache ?? true,
      cacheMaxAge: config.cacheMaxAge || 300000,
      timeout: config.timeout || 10000,
      strict: config.strict || false,
      ...config
    };
    
    this.cache = new Map();
    this.cacheStats = {
      hits: 0,
      misses: 0
    };
  }

  async evaluateCondition(condition, graph, options = {}) {
    const cacheKey = `${JSON.stringify(condition)}_${graph.size || 0}`;
    
    if (this.cache.has(cacheKey)) {
      this.cacheStats.hits++;
      return this.cache.get(cacheKey);
    }
    
    this.cacheStats.misses++;
    
    try {
      let result = false;
      
      // Simple condition evaluation for browser
      if (condition.kind === 'sparql-ask') {
        // Mock SPARQL ASK evaluation - in real implementation would use Comunica
        result = graph.size > 0;
      } else if (condition.kind === 'sparql-select') {
        // Mock SPARQL SELECT evaluation
        result = graph.size > 0;
      } else if (condition.kind === 'shacl') {
        // Mock SHACL validation
        result = true; // Assume valid for browser demo
      }
      
      const evaluationResult = {
        satisfied: result,
        evaluationTime: Date.now(),
        condition
      };
      
      this.cache.set(cacheKey, evaluationResult);
      return evaluationResult;
    } catch (error) {
      return {
        satisfied: false,
        error: error.message,
        evaluationTime: Date.now(),
        condition
      };
    }
  }

  getCacheStats() {
    return { ...this.cacheStats };
  }

  clearCache() {
    this.cache.clear();
    this.cacheStats = { hits: 0, misses: 0 };
  }
}

// Browser-compatible policy pack manager
export class BrowserPolicyPackManager {
  constructor(basePath = '/') {
    this.basePath = basePath;
    this.policyPacks = new Map();
    this.activePacks = new Set();
  }

  async loadPolicyPack(packName, manifest) {
    this.policyPacks.set(packName, {
      name: packName,
      manifest,
      hooks: manifest.hooks || [],
      rules: manifest.rules || [],
      loaded: true,
      activated: false
    });
  }

  activatePolicyPack(packName) {
    const pack = this.policyPacks.get(packName);
    if (pack) {
      pack.activated = true;
      this.activePacks.add(packName);
      return true;
    }
    return false;
  }

  deactivatePolicyPack(packName) {
    const pack = this.policyPacks.get(packName);
    if (pack) {
      pack.activated = false;
      this.activePacks.delete(packName);
      return true;
    }
    return false;
  }

  getPolicyPack(packName) {
    return this.policyPacks.get(packName);
  }

  getActivePolicyPacks() {
    return Array.from(this.activePacks).map(name => this.policyPacks.get(name)).filter(Boolean);
  }

  getActiveHooks() {
    return this.getActivePolicyPacks().flatMap(pack => pack.hooks);
  }
}

// Browser-compatible file resolver
export class BrowserFileResolver {
  constructor(config = {}) {
    this.config = {
      basePath: config.basePath || '/',
      enableCache: config.enableCache ?? true,
      cacheMaxAge: config.cacheMaxAge || 300000,
      maxFileSize: config.maxFileSize || 1024 * 1024,
      allowedMediaTypes: config.allowedMediaTypes || [
        'application/sparql-query',
        'text/turtle',
        'application/rdf+xml',
        'application/ld+json'
      ],
      timeout: config.timeout || 5000,
      ...config
    };
    
    this.cache = new Map();
  }

  async resolveFileUri(uri, basePath = this.config.basePath) {
    if (uri.startsWith('file://')) {
      return uri.replace('file://', '');
    } else if (uri.startsWith('http://') || uri.startsWith('https://')) {
      return uri;
    } else {
      return path.join(basePath, uri);
    }
  }

  async calculateFileHash(filePath) {
    // Mock hash calculation for browser
    return 'mock-hash-' + randomUUID().slice(0, 16);
  }

  async loadFileWithHash(uri, expectedHash, basePath = this.config.basePath) {
    const filePath = await this.resolveFileUri(uri, basePath);
    const cacheKey = `${filePath}:${expectedHash}`;
    
    if (this.cache.has(cacheKey)) {
      return this.cache.get(cacheKey);
    }
    
    // Mock file loading for browser
    const mockContent = `-- Mock content from ${filePath} --`;
    const content = {
      content: mockContent,
      size: mockContent.length,
      lastModified: Date.now(),
      hash: await this.calculateFileHash(filePath)
    };
    
    this.cache.set(cacheKey, content);
    return content;
  }

  async loadSparqlFile(uri, expectedHash, basePath = this.config.basePath) {
    const file = await this.loadFileWithHash(uri, expectedHash, basePath);
    return file.content;
  }
}

// Browser-compatible resolution layer
export class BrowserResolutionLayer {
  constructor(config = {}) {
    this.config = {
      defaultStrategy: config.defaultStrategy || 'voting',
      maxProposals: config.maxProposals || 100,
      enableConflictDetection: config.enableConflictDetection ?? true,
      enableConsensus: config.enableConsensus ?? true,
      timeout: config.timeout || 30000,
      ...config
    };
    
    this.proposals = new Map();
    this.resolutionHistory = [];
    this.agents = new Map();
    this.strategies = new Map();
  }

  registerAgent(agentId, metadata = {}) {
    this.agents.set(agentId, {
      id: agentId,
      ...metadata,
      registeredAt: Date.now()
    });
  }

  submitProposal(proposal) {
    this.proposals.set(proposal.id, proposal);
    return proposal.id;
  }

  async resolveProposals(strategy = this.config.defaultStrategy) {
    const pendingProposals = Array.from(this.proposals.values());
    if (pendingProposals.length === 0) {
      return null;
    }
    
    const resolution = {
      id: randomUUID(),
      strategy,
      proposals: pendingProposals,
      resolvedDelta: this._mergeProposals(pendingProposals),
      confidence: this._calculateConsensus(pendingProposals),
      consensus: pendingProposals.length > 1,
      conflicts: this._detectConflicts(pendingProposals),
      timestamp: Date.now(),
      duration: 0
    };
    
    this.resolutionHistory.push(resolution);
    
    // Clear processed proposals
    this.proposals.clear();
    
    return resolution;
  }

  _mergeProposals(proposals) {
    const additions = [];
    const removals = [];
    
    for (const proposal of proposals) {
      additions.push(...proposal.delta.additions);
      removals.push(...proposal.delta.removals);
    }
    
    return {
      additions,
      removals,
      metadata: { mergedFrom: proposals.length }
    };
  }

  _calculateConsensus(proposals) {
    return proposals.length > 0 ? Math.min(0.8, proposals.length * 0.2) : 0;
  }

  _detectConflicts(proposals) {
    // Simple conflict detection
    return [];
  }

  getStats() {
    return {
      agents: this.agents.size,
      pendingProposals: this.proposals.size,
      totalResolutions: this.resolutionHistory.length,
      config: this.config
    };
  }
}

// Browser-compatible knowledge hook manager
export class BrowserKnowledgeHookManager extends BrowserTransactionManager {
  constructor(options = {}) {
    super(options);
    
    this.basePath = options.basePath || '/';
    this.enableKnowledgeHooks = options.enableKnowledgeHooks ?? true;
    this.strictMode = options.strictMode || false;
    
    // Browser-compatible components
    this.hookExecutor = new BrowserHookExecutor({
      basePath: this.basePath,
      strictMode: this.strictMode,
      enableMetrics: true
    });
    
    this.conditionEvaluator = new BrowserConditionEvaluator({
      basePath: this.basePath,
      strictMode: this.strictMode,
      enableCache: true
    });
    
    this.policyPackManager = new BrowserPolicyPackManager(this.basePath);
    this.knowledgeHooks = new Map();
  }

  async addKnowledgeHook(hook) {
    if (!this.enableKnowledgeHooks) {
      throw new Error('Knowledge hooks are disabled');
    }
    
    if (!hook || typeof hook !== 'object') {
      throw new TypeError('Hook must be an object');
    }
    
    if (!hook.meta || !hook.meta.name) {
      throw new TypeError('Hook must have meta.name');
    }
    
    if (!hook.run || typeof hook.run !== 'function') {
      throw new TypeError('Hook must have a run function');
    }
    
    if (this.knowledgeHooks.has(hook.meta.name)) {
      throw new Error(`Knowledge hook "${hook.meta.name}" already exists`);
    }
    
    this.knowledgeHooks.set(hook.meta.name, hook);
    return hook;
  }

  removeKnowledgeHook(hookName) {
    const existed = this.knowledgeHooks.has(hookName);
    if (existed) {
      this.knowledgeHooks.delete(hookName);
    }
    return existed;
  }

  getKnowledgeHooks() {
    return Array.from(this.knowledgeHooks.values());
  }

  async executeKnowledgeHook(hookName, event, options = {}) {
    const hook = this.knowledgeHooks.get(hookName);
    if (!hook) {
      throw new Error(`Knowledge hook "${hookName}" not found`);
    }
    
    return this.hookExecutor.execute(hook, event, options);
  }

  async executeAllKnowledgeHooks(event, options = {}) {
    const hooks = this.getKnowledgeHooks();
    return this.hookExecutor.executeAll(hooks, event, options);
  }

  getStats() {
    const baseStats = super.getStats();
    const hookExecutorStats = this.hookExecutor.getMetrics();
    const conditionEvaluatorStats = this.conditionEvaluator.getCacheStats();
    
    return {
      ...baseStats,
      knowledgeHooks: {
        total: this.knowledgeHooks.size,
        enabled: this.enableKnowledgeHooks,
        strictMode: this.strictMode
      },
      hookExecutor: hookExecutorStats,
      conditionEvaluator: conditionEvaluatorStats
    };
  }
}

// Export factory functions for browser compatibility
export function createBrowserHookExecutor(config = {}) {
  return new BrowserHookExecutor(config);
}

export function createBrowserConditionEvaluator(config = {}) {
  return new BrowserConditionEvaluator(config);
}

export function createBrowserPolicyPackManager(basePath = '/') {
  return new BrowserPolicyPackManager(basePath);
}

export function createBrowserFileResolver(config = {}) {
  return new BrowserFileResolver(config);
}

export function createBrowserResolutionLayer(config = {}) {
  return new BrowserResolutionLayer(config);
}

export function createBrowserKnowledgeHookManager(options = {}) {
  return new BrowserKnowledgeHookManager(options);
}

// Export core components for browser
export {
  EffectSandbox,
  createEffectSandbox,
  BrowserLockchainWriter,
  createBrowserLockchainWriter,
  BrowserLockchainWriter as LockchainWriter,
  createBrowserLockchainWriter as createLockchainWriter,
  BrowserKnowledgeHookManager as KnowledgeHookManager,
  createBrowserKnowledgeHookManager as createKnowledgeHookManager
};

// Export QueryEngine from Comunica (browser-compatible)
export { QueryEngine } from '@comunica/query-sparql';

export default {
  EffectSandbox,
  createEffectSandbox,
  LockchainWriter: BrowserLockchainWriter,
  createLockchainWriter: createBrowserLockchainWriter,
  KnowledgeHookManager: BrowserKnowledgeHookManager,
  createKnowledgeHookManager: createBrowserKnowledgeHookManager,
  BrowserHookExecutor,
  BrowserConditionEvaluator,
  BrowserPolicyPackManager,
  BrowserFileResolver,
  BrowserResolutionLayer,
  QueryEngine
};
