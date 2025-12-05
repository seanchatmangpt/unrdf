/**
 * @file JIT Hook Chain Compiler for UNRDF Knowledge Hooks.
 * @module hooks/hook-chain-compiler
 *
 * @description
 * Compiles hook chains into optimized single functions to eliminate
 * dispatch overhead (18μs → ~0μs per chain execution).
 *
 * Uses `new Function()` for JIT compilation with CSP fallback.
 */

/**
 * Cache for compiled chain functions.
 * Key: chain signature (hook names joined by '|')
 * Value: compiled function
 * @type {Map<string, Function>}
 */
const compiledChains = new Map();

/**
 * Check if JIT compilation is available (CSP may block it).
 * @type {boolean}
 */
let jitAvailable = true;

// Test JIT availability once at module load
try {
  // eslint-disable-next-line no-new-func
  new Function('return true')();
} catch {
  jitAvailable = false;
}

/**
 * Check if hook has validation function.
 * @param {object} hook - Hook to check
 * @returns {boolean} - True if hook has validate function
 */
function hasValidation(hook) {
  return typeof hook.validate === 'function';
}

/**
 * Check if hook has transformation function.
 * @param {object} hook - Hook to check
 * @returns {boolean} - True if hook has transform function
 */
function hasTransformation(hook) {
  return typeof hook.transform === 'function';
}

/**
 * Generate a unique cache key for a hook chain.
 *
 * @param {Array<object>} hooks - Array of validated hooks
 * @returns {string} - Cache key
 */
export function getChainKey(hooks) {
  return hooks.map(h => h.name).join('|');
}

/**
 * Compile a hook chain into an optimized function.
 *
 * The compiled function:
 * - Eliminates loop dispatch overhead
 * - Inlines validation/transformation calls
 * - Returns { valid: boolean, quad: Quad } directly
 *
 * @param {Array<object>} hooks - Array of validated hooks
 * @returns {Function} - Compiled chain function (hooks, quad) => result
 *
 * @example
 * const compiledFn = compileHookChain([validator, transformer]);
 * const result = compiledFn(hooks, quad);
 */
export function compileHookChain(hooks) {
  const chainKey = getChainKey(hooks);

  // Return cached compiled function if available
  if (compiledChains.has(chainKey)) {
    return compiledChains.get(chainKey);
  }

  // Fallback to interpreted mode if JIT not available
  if (!jitAvailable) {
    const interpretedFn = createInterpretedChain(hooks);
    compiledChains.set(chainKey, interpretedFn);
    return interpretedFn;
  }

  // Generate inline validation steps
  const validationSteps = hooks
    .map((h, i) =>
      hasValidation(h)
        ? `if (!hooks[${i}].validate(quad)) return { valid: false, quad, failedHook: hooks[${i}].name };`
        : ''
    )
    .filter(Boolean)
    .join('\n    ');

  // Generate inline transformation steps
  const transformSteps = hooks
    .map((h, i) => (hasTransformation(h) ? `quad = hooks[${i}].transform(quad);` : ''))
    .filter(Boolean)
    .join('\n    ');

  // Compile the chain function
  const fnBody = `
    ${validationSteps}
    ${transformSteps}
    return { valid: true, quad };
  `;

  try {
    // eslint-disable-next-line no-new-func
    const compiledFn = new Function('hooks', 'quad', fnBody);
    compiledChains.set(chainKey, compiledFn);
    return compiledFn;
  } catch {
    // Fallback to interpreted mode on compilation error
    jitAvailable = false;
    const interpretedFn = createInterpretedChain(hooks);
    compiledChains.set(chainKey, interpretedFn);
    return interpretedFn;
  }
}

/**
 * Create an interpreted (non-JIT) chain function.
 * Used as fallback when CSP blocks `new Function()`.
 *
 * @param {Array<object>} hooks - Array of validated hooks
 * @returns {Function} - Interpreted chain function
 */
function createInterpretedChain(hooks) {
  // Capture hooks array for closure
  const capturedHooks = hooks;

  return function interpretedChain(_hooks, quad) {
    let currentQuad = quad;

    for (const hook of capturedHooks) {
      if (hasValidation(hook)) {
        if (!hook.validate(currentQuad)) {
          return { valid: false, quad: currentQuad, failedHook: hook.name };
        }
      }

      if (hasTransformation(hook)) {
        currentQuad = hook.transform(currentQuad);
      }
    }

    return { valid: true, quad: currentQuad };
  };
}

/**
 * Compile a validation-only chain (even faster, skips transforms).
 *
 * @param {Array<object>} hooks - Array of validated hooks
 * @returns {Function} - Compiled validation function (hooks, quad) => boolean
 */
export function compileValidationOnlyChain(hooks) {
  const chainKey = `validate:${getChainKey(hooks)}`;

  if (compiledChains.has(chainKey)) {
    return compiledChains.get(chainKey);
  }

  const validationHooks = hooks.filter(hasValidation);

  if (!jitAvailable || validationHooks.length === 0) {
    const fn =
      validationHooks.length === 0
        ? () => true
        : (_hooks, quad) => validationHooks.every(h => h.validate(quad));
    compiledChains.set(chainKey, fn);
    return fn;
  }

  // Generate inline validation checks
  const checks = validationHooks.map((_, i) => `hooks[${i}].validate(quad)`).join(' && ');

  const fnBody = `return ${checks || 'true'};`;

  try {
    // eslint-disable-next-line no-new-func
    const compiledFn = new Function('hooks', 'quad', fnBody);

    // Return wrapper that uses validation hooks only
    const wrapper = (_, quad) => compiledFn(validationHooks, quad);
    compiledChains.set(chainKey, wrapper);
    return wrapper;
  } catch {
    const fn = (_hooks, quad) => validationHooks.every(h => h.validate(quad));
    compiledChains.set(chainKey, fn);
    return fn;
  }
}

/**
 * Clear the compiled chain cache.
 * Useful for testing or when hooks are redefined.
 */
export function clearCompiledChainCache() {
  compiledChains.clear();
}

/**
 * Get cache statistics.
 *
 * @returns {{size: number, jitAvailable: boolean}} - Cache stats
 */
export function getCompilerStats() {
  return {
    size: compiledChains.size,
    jitAvailable,
  };
}

/**
 * Check if JIT compilation is available.
 *
 * @returns {boolean} - True if JIT is available
 */
export function isJitAvailable() {
  return jitAvailable;
}

export default {
  compileHookChain,
  compileValidationOnlyChain,
  clearCompiledChainCache,
  getCompilerStats,
  isJitAvailable,
  getChainKey,
};
