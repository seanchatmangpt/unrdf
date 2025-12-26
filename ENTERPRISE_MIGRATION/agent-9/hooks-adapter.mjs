/**
 * @file Hooks adapter for substrate operations
 * @module agent-9/hooks-adapter
 */

/**
 * @typedef {Object} Hook
 * @property {string} id - Hook ID
 * @property {string} name - Hook name
 * @property {Function} execute - Hook execution function
 * @property {Array<string>} triggers - Trigger event types
 * @property {number} priority - Execution priority (higher = earlier)
 * @property {boolean} enabled - Whether hook is enabled
 */

/**
 * @typedef {Object} HooksSubstrate
 * @property {Map<string, *>} triples - Triple store
 * @property {Map<string, Set<string>>} indexes - Index structures
 * @property {Map<string, Hook>} hooks - Registered hooks
 * @property {Array<Object>} hookHistory - Hook execution history
 * @property {'hooks'} type - Substrate type
 * @property {boolean} frozen - Whether substrate is frozen
 * @property {*} [transaction] - Current transaction
 * @property {Map<string, *>} metadata - Substrate metadata
 */

/**
 * Create hooks-backed substrate
 * @param {Object} [options] - Configuration options
 * @param {boolean} [options.trackHistory=true] - Whether to track hook history
 * @returns {HooksSubstrate} New hooks substrate
 */
export function createHooksSubstrate(options = {}) {
  return {
    triples: new Map(),
    indexes: new Map([
      ['subject', new Map()],
      ['predicate', new Map()],
      ['object', new Map()]
    ]),
    hooks: new Map(),
    hookHistory: [],
    type: 'hooks',
    frozen: false,
    transaction: null,
    metadata: new Map([
      ['trackHistory', options.trackHistory !== false]
    ])
  };
}

/**
 * Register hook with substrate
 * @param {HooksSubstrate} substrate - Target substrate
 * @param {Hook} hook - Hook to register
 * @returns {boolean} True if registered successfully
 * @throws {Error} If hook is invalid or already registered
 */
export function registerHookAdapter(substrate, hook) {
  if (!hook || typeof hook !== 'object') {
    throw new Error('Hook must be an object');
  }

  if (!hook.id || typeof hook.id !== 'string') {
    throw new Error('Hook must have an id');
  }

  if (!hook.execute || typeof hook.execute !== 'function') {
    throw new Error('Hook must have an execute function');
  }

  if (substrate.hooks.has(hook.id)) {
    throw new Error(`Hook already registered: ${hook.id}`);
  }

  const normalizedHook = {
    id: hook.id,
    name: hook.name || hook.id,
    execute: hook.execute,
    triggers: Array.isArray(hook.triggers) ? hook.triggers : [],
    priority: typeof hook.priority === 'number' ? hook.priority : 0,
    enabled: hook.enabled !== false
  };

  substrate.hooks.set(hook.id, normalizedHook);

  return true;
}

/**
 * Execute hook
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @param {string} hookId - Hook ID to execute
 * @param {Object} context - Execution context
 * @returns {Promise<*>} Hook execution result
 * @throws {Error} If hook not found or execution fails
 */
export async function executeHook(substrate, hookId, context = {}) {
  const hook = substrate.hooks.get(hookId);

  if (!hook) {
    throw new Error(`Hook not found: ${hookId}`);
  }

  if (!hook.enabled) {
    throw new Error(`Hook is disabled: ${hookId}`);
  }

  const startTime = Date.now();

  try {
    const result = await hook.execute({
      substrate,
      context,
      hookId
    });

    // Track history
    if (substrate.metadata.get('trackHistory')) {
      substrate.hookHistory.push({
        hookId,
        timestamp: Date.now(),
        duration: Date.now() - startTime,
        success: true,
        result
      });
    }

    return result;
  } catch (error) {
    // Track failure
    if (substrate.metadata.get('trackHistory')) {
      substrate.hookHistory.push({
        hookId,
        timestamp: Date.now(),
        duration: Date.now() - startTime,
        success: false,
        error: error.message
      });
    }

    throw new Error(`Hook execution failed: ${error.message}`);
  }
}

/**
 * Execute hooks by trigger
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @param {string} trigger - Trigger event type
 * @param {Object} context - Execution context
 * @returns {Promise<Array<*>>} Results from all matching hooks
 */
export async function executeHooksByTrigger(substrate, trigger, context = {}) {
  const matchingHooks = [];

  for (const hook of substrate.hooks.values()) {
    if (hook.enabled && hook.triggers.includes(trigger)) {
      matchingHooks.push(hook);
    }
  }

  // Sort by priority (higher first)
  matchingHooks.sort((a, b) => b.priority - a.priority);

  const results = [];

  for (const hook of matchingHooks) {
    try {
      const result = await executeHook(substrate, hook.id, {
        ...context,
        trigger
      });
      results.push({ hookId: hook.id, success: true, result });
    } catch (error) {
      results.push({ hookId: hook.id, success: false, error: error.message });
    }
  }

  return results;
}

/**
 * Unregister hook
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @param {string} hookId - Hook ID to unregister
 * @returns {boolean} True if unregistered
 */
export function unregisterHook(substrate, hookId) {
  return substrate.hooks.delete(hookId);
}

/**
 * Enable hook
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @param {string} hookId - Hook ID to enable
 * @returns {boolean} True if enabled
 */
export function enableHook(substrate, hookId) {
  const hook = substrate.hooks.get(hookId);
  if (!hook) {
    throw new Error(`Hook not found: ${hookId}`);
  }

  hook.enabled = true;
  return true;
}

/**
 * Disable hook
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @param {string} hookId - Hook ID to disable
 * @returns {boolean} True if disabled
 */
export function disableHook(substrate, hookId) {
  const hook = substrate.hooks.get(hookId);
  if (!hook) {
    throw new Error(`Hook not found: ${hookId}`);
  }

  hook.enabled = false;
  return true;
}

/**
 * Get all registered hooks
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @returns {Array<Hook>} All registered hooks
 */
export function getHooks(substrate) {
  return Array.from(substrate.hooks.values());
}

/**
 * Get hook by ID
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @param {string} hookId - Hook ID
 * @returns {Hook|null} Hook or null if not found
 */
export function getHook(substrate, hookId) {
  return substrate.hooks.get(hookId) || null;
}

/**
 * Get hook execution history
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @param {Object} [options] - Filter options
 * @param {string} [options.hookId] - Filter by hook ID
 * @param {number} [options.limit] - Limit results
 * @returns {Array<Object>} Hook execution history
 */
export function getHookHistory(substrate, options = {}) {
  let history = substrate.hookHistory;

  // Filter by hook ID
  if (options.hookId) {
    history = history.filter(entry => entry.hookId === options.hookId);
  }

  // Limit results
  if (options.limit && typeof options.limit === 'number') {
    history = history.slice(-options.limit);
  }

  return history;
}

/**
 * Clear hook execution history
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @returns {number} Number of entries cleared
 */
export function clearHookHistory(substrate) {
  const count = substrate.hookHistory.length;
  substrate.hookHistory = [];
  return count;
}

/**
 * Get hooks statistics
 * @param {HooksSubstrate} substrate - Substrate with hooks
 * @returns {Object} Statistics
 */
export function getHooksStats(substrate) {
  const hooks = Array.from(substrate.hooks.values());
  const enabled = hooks.filter(h => h.enabled).length;
  const disabled = hooks.filter(h => !h.enabled).length;

  const historyStats = {
    total: substrate.hookHistory.length,
    successful: substrate.hookHistory.filter(h => h.success).length,
    failed: substrate.hookHistory.filter(h => !h.success).length
  };

  return {
    totalHooks: substrate.hooks.size,
    enabled,
    disabled,
    history: historyStats,
    tripleCount: substrate.triples.size
  };
}
