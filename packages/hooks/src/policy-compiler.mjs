/**
 * Policy Compiler - JIT Compilation for Hook Policies
 *
 * Optimizations:
 * 1. Compile policies to optimized functions on first use
 * 2. Cache compiled policies using WeakMap (auto-cleanup)
 * 3. Inline common patterns for sub-microsecond execution
 * 4. Batch policy evaluation for multiple quads
 *
 * Target: <500us p95 for hook execution (vs variable baseline)
 *
 * @module @unrdf/hooks/policy-compiler
 */

// =============================================================================
// Policy Cache
// =============================================================================

/**
 * WeakMap cache for compiled policies
 * Automatically cleans up when hook objects are garbage collected
 * @type {WeakMap<Object, Function>}
 */
const compiledPolicyCache = new WeakMap();

/**
 * Map for string-based policy patterns
 * @type {Map<string, Function>}
 */
const patternCache = new Map();

/**
 * Compiler statistics
 */
const compilerStats = {
  compiled: 0,
  cacheHits: 0,
  cacheMisses: 0,
  evaluations: 0,
  totalCompileTimeMs: 0,
  totalEvalTimeMs: 0,
};

// =============================================================================
// Policy Patterns
// =============================================================================

/**
 * Common policy pattern types
 * @enum {string}
 */
export const PolicyPatterns = {
  /** Always allow */
  ALLOW_ALL: 'ALLOW_ALL',
  /** Always deny */
  DENY_ALL: 'DENY_ALL',
  /** Match subject IRI pattern */
  SUBJECT_PATTERN: 'SUBJECT_PATTERN',
  /** Match predicate IRI pattern */
  PREDICATE_PATTERN: 'PREDICATE_PATTERN',
  /** Match object value pattern */
  OBJECT_PATTERN: 'OBJECT_PATTERN',
  /** Match namespace */
  NAMESPACE: 'NAMESPACE',
  /** Custom function */
  CUSTOM: 'CUSTOM',
};

/**
 * @typedef {Object} PolicyDefinition
 * @property {string} type - Policy pattern type
 * @property {Object} [config] - Pattern-specific configuration
 * @property {Function} [evaluate] - Custom evaluation function
 */

// =============================================================================
// Policy Compilation
// =============================================================================

/**
 * Compile a policy definition into an optimized evaluation function
 *
 * @param {PolicyDefinition} policy - Policy to compile
 * @returns {Function} Compiled evaluation function (quad) => boolean
 */
export function compilePolicy(policy) {
  const startTime = performance.now();

  // Check pattern cache for string-based policies
  const cacheKey = getCacheKey(policy);
  if (patternCache.has(cacheKey)) {
    compilerStats.cacheHits++;
    return patternCache.get(cacheKey);
  }

  compilerStats.cacheMisses++;
  compilerStats.compiled++;

  let compiledFn;

  switch (policy.type) {
    case PolicyPatterns.ALLOW_ALL:
      compiledFn = () => true;
      break;

    case PolicyPatterns.DENY_ALL:
      compiledFn = () => false;
      break;

    case PolicyPatterns.SUBJECT_PATTERN: {
      const pattern = policy.config?.pattern;
      if (pattern instanceof RegExp) {
        compiledFn = (quad) => pattern.test(quad.subject?.value || '');
      } else if (typeof pattern === 'string') {
        // Compile string to regex for performance
        const regex = new RegExp(pattern);
        compiledFn = (quad) => regex.test(quad.subject?.value || '');
      } else {
        compiledFn = () => true;
      }
      break;
    }

    case PolicyPatterns.PREDICATE_PATTERN: {
      const pattern = policy.config?.pattern;
      if (pattern instanceof RegExp) {
        compiledFn = (quad) => pattern.test(quad.predicate?.value || '');
      } else if (typeof pattern === 'string') {
        const regex = new RegExp(pattern);
        compiledFn = (quad) => regex.test(quad.predicate?.value || '');
      } else {
        compiledFn = () => true;
      }
      break;
    }

    case PolicyPatterns.OBJECT_PATTERN: {
      const pattern = policy.config?.pattern;
      if (pattern instanceof RegExp) {
        compiledFn = (quad) => pattern.test(quad.object?.value || '');
      } else if (typeof pattern === 'string') {
        const regex = new RegExp(pattern);
        compiledFn = (quad) => regex.test(quad.object?.value || '');
      } else {
        compiledFn = () => true;
      }
      break;
    }

    case PolicyPatterns.NAMESPACE: {
      const namespace = policy.config?.namespace;
      if (namespace) {
        // Optimized prefix check
        compiledFn = (quad) => {
          const s = quad.subject?.value || '';
          const p = quad.predicate?.value || '';
          return s.startsWith(namespace) || p.startsWith(namespace);
        };
      } else {
        compiledFn = () => true;
      }
      break;
    }

    case PolicyPatterns.CUSTOM:
      if (typeof policy.evaluate === 'function') {
        compiledFn = policy.evaluate;
      } else {
        compiledFn = () => true;
      }
      break;

    default:
      // Unknown pattern - allow by default
      compiledFn = () => true;
  }

  // Cache compiled function
  patternCache.set(cacheKey, compiledFn);

  const elapsed = performance.now() - startTime;
  compilerStats.totalCompileTimeMs += elapsed;

  return compiledFn;
}

/**
 * Generate cache key for policy
 * @param {PolicyDefinition} policy
 * @returns {string}
 */
function getCacheKey(policy) {
  if (policy.type === PolicyPatterns.CUSTOM) {
    // Custom functions can't be reliably cached by key
    return `CUSTOM_${Date.now()}_${Math.random()}`;
  }

  const config = policy.config
    ? JSON.stringify(policy.config, (key, value) => {
        if (value instanceof RegExp) return value.toString();
        return value;
      })
    : '';

  return `${policy.type}|${config}`;
}

// =============================================================================
// Hook Policy Compilation
// =============================================================================

/**
 * Compile a hook's validation/transform into optimized form
 *
 * @param {Object} hook - Hook object with validate/transform functions
 * @returns {Object} Compiled hook with optimized functions
 */
export function compileHook(hook) {
  // Check WeakMap cache
  if (compiledPolicyCache.has(hook)) {
    compilerStats.cacheHits++;
    return compiledPolicyCache.get(hook);
  }

  compilerStats.cacheMisses++;

  const startTime = performance.now();

  // Create optimized wrapper
  const compiledHook = {
    ...hook,
    _compiled: true,
    _originalValidate: hook.validate,
    _originalTransform: hook.transform,
  };

  // Optimize validation if present
  if (typeof hook.validate === 'function') {
    // If validation is simple pattern check, optimize it
    if (hook.policy) {
      const compiledPolicy = compilePolicy(hook.policy);
      compiledHook.validate = compiledPolicy;
    } else {
      // Wrap original for tracking
      compiledHook.validate = hook.validate;
    }
  }

  // Optimize transformation if present
  if (typeof hook.transform === 'function') {
    // Most transforms can't be pre-compiled, just cache
    compiledHook.transform = hook.transform;
  }

  // Cache in WeakMap
  compiledPolicyCache.set(hook, compiledHook);

  const elapsed = performance.now() - startTime;
  compilerStats.totalCompileTimeMs += elapsed;
  compilerStats.compiled++;

  return compiledHook;
}

/**
 * Compile an array of hooks for batch execution
 *
 * @param {Object[]} hooks - Hooks to compile
 * @returns {Object[]} Compiled hooks
 */
export function compileHooks(hooks) {
  return hooks.map(compileHook);
}

// =============================================================================
// Optimized Execution
// =============================================================================

/**
 * Execute compiled hook on a quad
 *
 * @param {Object} hook - Compiled or raw hook
 * @param {Object} quad - RDF quad
 * @returns {{valid: boolean, quad: Object, hookName: string}}
 */
export function executeCompiledHook(hook, quad) {
  const startTime = performance.now();
  compilerStats.evaluations++;

  // Ensure hook is compiled
  const compiledHook = hook._compiled ? hook : compileHook(hook);

  const result = {
    valid: true,
    quad: quad,
    hookName: compiledHook.name || 'anonymous',
  };

  try {
    // Run validation
    if (compiledHook.validate) {
      if (!compiledHook.validate(quad)) {
        result.valid = false;
        result.error = `Validation failed: ${compiledHook.name}`;
        return result;
      }
    }

    // Run transformation
    if (compiledHook.transform && result.valid) {
      result.quad = compiledHook.transform(quad);
    }

  } catch (error) {
    result.valid = false;
    result.error = error.message;
  }

  const elapsed = performance.now() - startTime;
  compilerStats.totalEvalTimeMs += elapsed;

  return result;
}

/**
 * Execute compiled hooks in chain
 *
 * @param {Object[]} hooks - Hooks to execute
 * @param {Object} quad - Initial quad
 * @returns {{valid: boolean, quad: Object, results: Object[]}}
 */
export function executeCompiledChain(hooks, quad) {
  const results = [];
  let currentQuad = quad;
  let chainValid = true;
  let chainError;

  // Compile all hooks upfront
  const compiledHooks = hooks.map(h => h._compiled ? h : compileHook(h));

  for (const hook of compiledHooks) {
    const result = executeCompiledHook(hook, currentQuad);
    results.push(result);

    if (!result.valid) {
      chainValid = false;
      chainError = result.error;
      break;
    }

    currentQuad = result.quad;
  }

  return {
    valid: chainValid,
    quad: currentQuad,
    results,
    error: chainError,
  };
}

/**
 * Batch validate quads against compiled hooks
 *
 * @param {Object[]} hooks - Pre-compiled hooks
 * @param {Object[]} quads - Quads to validate
 * @returns {Uint8Array} Bitmap of valid quads (1 = valid, 0 = invalid)
 */
export function batchValidateCompiled(hooks, quads) {
  // Ensure all hooks are compiled
  const compiledHooks = hooks.map(h => h._compiled ? h : compileHook(h));

  // Filter to validation hooks only
  const validationHooks = compiledHooks.filter(h => h.validate);

  const bitmap = new Uint8Array(quads.length);

  for (let i = 0; i < quads.length; i++) {
    const quad = quads[i];
    let isValid = true;

    for (const hook of validationHooks) {
      try {
        if (!hook.validate(quad)) {
          isValid = false;
          break;
        }
      } catch {
        isValid = false;
        break;
      }
    }

    bitmap[i] = isValid ? 1 : 0;
  }

  return bitmap;
}

// =============================================================================
// Precompilation
// =============================================================================

/**
 * Precompile common policy patterns for fast lookup
 * Call at application startup to warm the cache
 *
 * @param {PolicyDefinition[]} policies - Policies to precompile
 * @returns {{compiled: number, errors: string[]}}
 */
export function precompilePolicies(policies) {
  const errors = [];
  let compiled = 0;

  for (const policy of policies) {
    try {
      compilePolicy(policy);
      compiled++;
    } catch (error) {
      errors.push(`Policy ${policy.type}: ${error.message}`);
    }
  }

  return { compiled, errors };
}

/**
 * Precompile hooks for faster first execution
 *
 * @param {Object[]} hooks - Hooks to precompile
 * @returns {{compiled: number, errors: string[]}}
 */
export function precompileHooks(hooks) {
  const errors = [];
  let compiled = 0;

  for (const hook of hooks) {
    try {
      compileHook(hook);
      compiled++;
    } catch (error) {
      errors.push(`Hook ${hook.name || 'anonymous'}: ${error.message}`);
    }
  }

  return { compiled, errors };
}

// =============================================================================
// Cache Management
// =============================================================================

/**
 * Clear pattern cache (WeakMap auto-clears)
 */
export function clearPolicyCache() {
  patternCache.clear();
}

/**
 * Get compiler statistics
 * @returns {Object}
 */
export function getCompilerStats() {
  return {
    ...compilerStats,
    patternCacheSize: patternCache.size,
    avgCompileTimeMs: compilerStats.totalCompileTimeMs / compilerStats.compiled || 0,
    avgEvalTimeUs: (compilerStats.totalEvalTimeMs / compilerStats.evaluations) * 1000 || 0,
    cacheHitRate: compilerStats.cacheHits / (compilerStats.cacheHits + compilerStats.cacheMisses) || 0,
  };
}

/**
 * Reset compiler statistics
 */
export function resetCompilerStats() {
  compilerStats.compiled = 0;
  compilerStats.cacheHits = 0;
  compilerStats.cacheMisses = 0;
  compilerStats.evaluations = 0;
  compilerStats.totalCompileTimeMs = 0;
  compilerStats.totalEvalTimeMs = 0;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  PolicyPatterns,
  compilePolicy,
  compileHook,
  compileHooks,
  executeCompiledHook,
  executeCompiledChain,
  batchValidateCompiled,
  precompilePolicies,
  precompileHooks,
  clearPolicyCache,
  getCompilerStats,
  resetCompilerStats,
};
