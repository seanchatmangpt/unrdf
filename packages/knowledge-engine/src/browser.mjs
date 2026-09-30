/**
 * @file Browser-Compatible Knowledge Engine
 * @module knowledge-engine-browser
 *
 * @description
 * Browser-compatible version of the knowledge engine with Node.js APIs
 * replaced by browser-compatible alternatives.
 */

import { _isBrowser } from './browser-shims.mjs';
import { randomUUID, path } from './browser-shims.mjs';
import { process as _browserProcess } from './browser-shims.mjs';

// Browser-compatible versions
import { EffectSandbox, createEffectSandbox } from './effect-sandbox-browser.mjs';
import {
  BrowserLockchainWriter,
  createBrowserLockchainWriter,
} from './lockchain-writer-browser.mjs';

// Re-export schemas (these are already browser-compatible)
export * from './schemas.mjs';

// Core Engine Components (Browser-compatible)
import { _KnowledgeHookManager } from './knowledge-hook-manager.mjs';
import { _QueryOptimizer } from './query-optimizer.mjs';

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
      ...options,
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
      config: this.options,
    };
  }
}

// Browser-compatible hook executor
/**
 *
 */
export class BrowserHookExecutor {
  /**
   *
   */
  constructor(config = {}) {
    this.config = {
      basePath: config.basePath || '/',
      strictMode: config.strictMode || false,
      enableConditionEvaluation: config.enableConditionEvaluation ?? true,
      enableMetrics: config.enableMetrics ?? true,
      timeout: config.timeout || 30000,
      ...config,
    };

    this.metrics = {
      executions: 0,
      errors: 0,
      totalDuration: 0,
      cacheHits: 0,
      cacheMisses: 0,
    };

    this.cache = new Map();
  }

  /**
   *
   */
  async execute(hook, event, _options = {}) {
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
        timestamp: Date.now(),
      };
    } catch (error) {
      this.metrics.errors++;
      const duration = Date.now() - startTime;
      this.metrics.totalDuration += duration;

      return {
        success: false,
        error: error.message,
        duration,
        timestamp: Date.now(),
      };
    }
  }

  /**
   *
   */
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
          hook: hook.meta?.name || 'unknown',
        });
      }
    }

    return results;
  }

  /**
   *
   */
  getMetrics() {
    return { ...this.metrics };
  }

  /**
   *
   */
  clearCache() {
    this.cache.clear();
  }
}

// Browser-compatible condition evaluator
/**
 *
 */
export class BrowserConditionEvaluator {
  /**
   *
   */
  constructor(config = {}) {
    this.config = {
      basePath: config.basePath || '/',
      enableCache: config.enableCache ?? true,
      cacheMaxAge: config.cacheMaxAge || 300000,
      timeout: config.timeout || 10000,
      strict: config.strict || false,
      ...config,
    };

    this.cache = new Map();
    this.cacheStats = {
      hits: 0,
      misses: 0,
    };
  }

  /**
   *
   */
  async evaluateCondition(condition, graph, _options = {}) {
    const cacheKey = `${JSON.stringify(condition)}_${graph.size || 0}`;

    if (this.cache.has(cacheKey)) {
      this.cacheStats.hits++;
      return this.cache.get(cacheKey);
    }

    this.cacheStats.misses++;

    try {
      let result = false;

      // Browser-compatible condition evaluation using pattern matching
      if (condition.kind === 'sparql-ask') {
        result = await this._evaluateSparqlAsk(condition, graph);
      } else if (condition.kind === 'sparql-select') {
        result = await this._evaluateSparqlSelect(condition, graph);
      } else if (condition.kind === 'shacl') {
        result = await this._evaluateShacl(condition, graph);
      }

      const evaluationResult = {
        satisfied: result,
        evaluationTime: Date.now(),
        condition,
      };

      this.cache.set(cacheKey, evaluationResult);
      return evaluationResult;
    } catch (error) {
      return {
        satisfied: false,
        error: error.message,
        evaluationTime: Date.now(),
        condition,
      };
    }
  }

  /**
   *
   */
  getCacheStats() {
    return { ...this.cacheStats };
  }

  /**
   *
   */
  clearCache() {
    this.cache.clear();
    this.cacheStats = { hits: 0, misses: 0 };
  }

  /**
   * Evaluate SPARQL ASK query using pattern matching
   * @param {Object} condition - Condition with query property
   * @param {Store} graph - RDF graph
   * @returns {Promise<boolean>} Query result
   * @private
   */
  async _evaluateSparqlAsk(condition, graph) {
    const query = condition.query || condition.ref?.query || '';
    if (!query) {
      return graph.size > 0; // Fallback
    }

    // Parse basic SPARQL ASK patterns
    const whereMatch = query.match(/WHERE\s*\{([^}]+)\}/is);
    if (!whereMatch) {
      return graph.size > 0; // Fallback if can't parse
    }

    const patterns = whereMatch[1]
      .split(/\.\s*/)
      .map(p => p.trim())
      .filter(p => p && !p.startsWith('#'));

    // For each pattern, check if it matches in the graph
    for (const pattern of patterns) {
      const match = pattern.match(/(\S+)\s+(\S+)\s+(\S+)/);
      if (!match) continue;

      const [, s, p, o] = match;
      const subject = this._normalizeTerm(s);
      const predicate = this._normalizeTerm(p);
      const object = this._normalizeTerm(o);

      // Check if any quad matches this pattern
      const quads = graph.getQuads(subject, predicate, object, null);
      if (quads.length === 0) {
        return false; // Pattern doesn't match
      }
    }

    return true; // All patterns matched
  }

  /**
   * Evaluate SPARQL SELECT query using pattern matching
   * @param {Object} condition - Condition with query property
   * @param {Store} graph - RDF graph
   * @returns {Promise<Array>} Query results
   * @private
   */
  async _evaluateSparqlSelect(condition, graph) {
    const query = condition.query || condition.ref?.query || '';
    if (!query) {
      return [];
    }

    // Extract SELECT variables
    const selectMatch = query.match(/SELECT\s+(.+?)\s+WHERE/is);
    if (!selectMatch) {
      return [];
    }

    const _variables = selectMatch[1]
      .split(/\s+/)
      .map(v => v.replace(/^\?/, '').trim())
      .filter(v => v);

    // Extract WHERE patterns
    const whereMatch = query.match(/WHERE\s*\{([^}]+)\}/is);
    if (!whereMatch) {
      return [];
    }

    const patterns = whereMatch[1]
      .split(/\.\s*/)
      .map(p => p.trim())
      .filter(p => p && !p.startsWith('#'));

    // Simple pattern matching - find matching quads
    const results = [];

    for (const pattern of patterns) {
      const match = pattern.match(/(\S+)\s+(\S+)\s+(\S+)/);
      if (!match) continue;

      const [, s, p, o] = match;
      const subject = this._normalizeTerm(s);
      const predicate = this._normalizeTerm(p);
      const object = this._normalizeTerm(o);

      const quads = graph.getQuads(subject, predicate, object, null);
      for (const quad of quads) {
        const binding = {};
        if (s.startsWith('?')) {
          binding[s.substring(1)] = quad.subject.value;
        }
        if (p.startsWith('?')) {
          binding[p.substring(1)] = quad.predicate.value;
        }
        if (o.startsWith('?')) {
          binding[o.substring(1)] = quad.object.value;
        }
        if (Object.keys(binding).length > 0) {
          results.push(binding);
        }
      }
    }

    return results;
  }

  /**
   * Evaluate SHACL validation using basic pattern matching
   * @param {Object} condition - Condition with shapes
   * @param {Store} graph - RDF graph
   * @returns {Promise<boolean>} Validation result
   * @private
   */
  async _evaluateShacl(condition, graph) {
    // Basic SHACL validation - check for required properties
    // Full SHACL validation would require a proper SHACL engine
    const shapes = condition.shapes || condition.ref?.shapes;
    if (!shapes) {
      return graph.size > 0; // No shapes to validate against
    }

    // For now, return true if graph has data
    // Full implementation would parse SHACL shapes and validate
    return graph.size > 0;
  }

  /**
   * Normalize SPARQL term (variable, IRI, literal) to match format
   * @param {string} term - SPARQL term
   * @returns {string|null} Normalized term or null for variables
   * @private
   */
  _normalizeTerm(term) {
    if (!term) return null;
    term = term.trim();

    // Variable - return null to match any
    if (term.startsWith('?')) {
      return null;
    }

    // IRI with angle brackets
    if (term.startsWith('<') && term.endsWith('>')) {
      return term.slice(1, -1);
    }

    // Literal with quotes
    if (
      (term.startsWith('"') && term.endsWith('"')) ||
      (term.startsWith("'") && term.endsWith("'"))
    ) {
      return term.slice(1, -1);
    }

    // Plain IRI or term
    return term;
  }
}

// Browser-compatible policy pack manager
/**
 *
 */
export class BrowserPolicyPackManager {
  /**
   *
   */
  constructor(basePath = '/') {
    this.basePath = basePath;
    this.policyPacks = new Map();
    this.activePacks = new Set();
  }

  /**
   *
   */
  async loadPolicyPack(packName, manifest) {
    this.policyPacks.set(packName, {
      name: packName,
      manifest,
      hooks: manifest.hooks || [],
      rules: manifest.rules || [],
      loaded: true,
      activated: false,
    });
  }

  /**
   *
   */
  activatePolicyPack(packName) {
    const pack = this.policyPacks.get(packName);
    if (pack) {
      pack.activated = true;
      this.activePacks.add(packName);
      return true;
    }
    return false;
  }

  /**
   *
   */
  deactivatePolicyPack(packName) {
    const pack = this.policyPacks.get(packName);
    if (pack) {
      pack.activated = false;
      this.activePacks.delete(packName);
      return true;
    }
    return false;
  }

  /**
   *
   */
  getPolicyPack(packName) {
    return this.policyPacks.get(packName);
  }

  /**
   *
   */
  getActivePolicyPacks() {
    return Array.from(this.activePacks)
      .map(name => this.policyPacks.get(name))
      .filter(Boolean);
  }

  /**
   *
   */
  getActiveHooks() {
    return this.getActivePolicyPacks().flatMap(pack => pack.hooks);
  }
}

// Browser-compatible file resolver
/**
 *
 */
export class BrowserFileResolver {
  /**
   *
   */
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
        'application/ld+json',
      ],
      timeout: config.timeout || 5000,
      ...config,
    };

    this.cache = new Map();
  }

  /**
   *
   */
  async resolveFileUri(uri, basePath = this.config.basePath) {
    if (uri.startsWith('file://')) {
      return uri.replace('file://', '');
    } else if (uri.startsWith('http://') || uri.startsWith('https://')) {
      return uri;
    } else {
      return path.join(basePath, uri);
    }
  }

  /**
   *
   */
  async calculateFileHash(filePath) {
    // Use Web Crypto API for SHA-256 hash calculation
    if (
      typeof globalThis?.window === 'undefined' ||
      !globalThis?.window?.crypto ||
      !globalThis?.window?.crypto?.subtle
    ) {
      // Fallback for environments without Web Crypto API
      return 'fallback-hash-' + Date.now().toString(36);
    }

    try {
      // Read file content (in browser, this would typically be from File/Blob)
      // For now, we'll hash the file path and any available content
      let content = filePath;

      // If filePath is a File or Blob, read it
      if (filePath instanceof File || filePath instanceof Blob) {
        const arrayBuffer = await filePath.arrayBuffer();
        content = new Uint8Array(arrayBuffer);
      } else if (typeof filePath === 'string') {
        // Convert string to Uint8Array
        const encoder = new TextEncoder();
        content = encoder.encode(filePath);
      }

      // Calculate SHA-256 hash
      const hashBuffer = await globalThis.window.crypto.subtle.digest('SHA-256', content);
      const hashArray = Array.from(new Uint8Array(hashBuffer));
      const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');

      return hashHex;
    } catch (error) {
      // Fallback on error
      console.warn('Failed to calculate file hash with Web Crypto API:', error);
      return 'error-hash-' + Date.now().toString(36);
    }
  }

  /**
   *
   */
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
      hash: await this.calculateFileHash(filePath),
    };

    this.cache.set(cacheKey, content);
    return content;
  }

  /**
   *
   */
  async loadSparqlFile(uri, expectedHash, basePath = this.config.basePath) {
    const file = await this.loadFileWithHash(uri, expectedHash, basePath);
    return file.content;
  }
}

// Browser-compatible resolution layer
/**
 *
 */
export class BrowserResolutionLayer {
  /**
   *
   */
  constructor(config = {}) {
    this.config = {
      defaultStrategy: config.defaultStrategy || 'voting',
      maxProposals: config.maxProposals || 100,
      enableConflictDetection: config.enableConflictDetection ?? true,
      enableConsensus: config.enableConsensus ?? true,
      timeout: config.timeout || 30000,
      ...config,
    };

    this.proposals = new Map();
    this.resolutionHistory = [];
    this.agents = new Map();
    this.strategies = new Map();
  }

  /**
   *
   */
  registerAgent(agentId, metadata = {}) {
    this.agents.set(agentId, {
      id: agentId,
      ...metadata,
      registeredAt: Date.now(),
    });
  }

  /**
   *
   */
  submitProposal(proposal) {
    this.proposals.set(proposal.id, proposal);
    return proposal.id;
  }

  /**
   *
   */
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
      duration: 0,
    };

    this.resolutionHistory.push(resolution);

    // Clear processed proposals
    this.proposals.clear();

    return resolution;
  }

  /**
   *
   */
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
      metadata: { mergedFrom: proposals.length },
    };
  }

  /**
   *
   */
  _calculateConsensus(proposals) {
    return proposals.length > 0 ? Math.min(0.8, proposals.length * 0.2) : 0;
  }

  /**
   *
   */
  _detectConflicts(_proposals) {
    // Simple conflict detection
    return [];
  }

  /**
   *
   */
  getStats() {
    return {
      agents: this.agents.size,
      pendingProposals: this.proposals.size,
      totalResolutions: this.resolutionHistory.length,
      config: this.config,
    };
  }
}

// Browser-compatible knowledge hook manager
/**
 *
 */
export class BrowserKnowledgeHookManager extends BrowserTransactionManager {
  /**
   *
   */
  constructor(options = {}) {
    super(options);

    this.basePath = options.basePath || '/';
    this.enableKnowledgeHooks = options.enableKnowledgeHooks ?? true;
    this.strictMode = options.strictMode || false;

    // Browser-compatible components
    this.hookExecutor = new BrowserHookExecutor({
      basePath: this.basePath,
      strictMode: this.strictMode,
      enableMetrics: true,
    });

    this.conditionEvaluator = new BrowserConditionEvaluator({
      basePath: this.basePath,
      strictMode: this.strictMode,
      enableCache: true,
    });

    this.policyPackManager = new BrowserPolicyPackManager(this.basePath);
    this.knowledgeHooks = new Map();
  }

  /**
   *
   */
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

  /**
   *
   */
  removeKnowledgeHook(hookName) {
    const existed = this.knowledgeHooks.has(hookName);
    if (existed) {
      this.knowledgeHooks.delete(hookName);
    }
    return existed;
  }

  /**
   *
   */
  getKnowledgeHooks() {
    return Array.from(this.knowledgeHooks.values());
  }

  /**
   *
   */
  async executeKnowledgeHook(hookName, event, options = {}) {
    const hook = this.knowledgeHooks.get(hookName);
    if (!hook) {
      throw new Error(`Knowledge hook "${hookName}" not found`);
    }

    return this.hookExecutor.execute(hook, event, options);
  }

  /**
   *
   */
  async executeAllKnowledgeHooks(event, options = {}) {
    const hooks = this.getKnowledgeHooks();
    return this.hookExecutor.executeAll(hooks, event, options);
  }

  /**
   *
   */
  getStats() {
    const baseStats = super.getStats();
    const hookExecutorStats = this.hookExecutor.getMetrics();
    const conditionEvaluatorStats = this.conditionEvaluator.getCacheStats();

    return {
      ...baseStats,
      knowledgeHooks: {
        total: this.knowledgeHooks.size,
        enabled: this.enableKnowledgeHooks,
        strictMode: this.strictMode,
      },
      hookExecutor: hookExecutorStats,
      conditionEvaluator: conditionEvaluatorStats,
    };
  }
}

// Export factory functions for browser compatibility
/**
 *
 */
export function createBrowserHookExecutor(config = {}) {
  return new BrowserHookExecutor(config);
}

/**
 *
 */
export function createBrowserConditionEvaluator(config = {}) {
  return new BrowserConditionEvaluator(config);
}

/**
 *
 */
export function createBrowserPolicyPackManager(basePath = '/') {
  return new BrowserPolicyPackManager(basePath);
}

/**
 *
 */
export function createBrowserFileResolver(config = {}) {
  return new BrowserFileResolver(config);
}

/**
 *
 */
export function createBrowserResolutionLayer(config = {}) {
  return new BrowserResolutionLayer(config);
}

/**
 *
 */
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
  createBrowserKnowledgeHookManager as createKnowledgeHookManager,
};

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
};
