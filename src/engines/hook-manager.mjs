/**
 * @fileoverview Definitive Hook Manager
 * 
 * Comprehensive hook registration and predicate evaluation system.
 * Integrates with EventBus for universal event handling.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

/**
 * @typedef {'AND'|'OR'} CombineOp
 * @typedef {{kind:'COUNT'|'ASK'|'THRESHOLD'|'SHACL'|'DELTA'|'CUSTOM', spec?:any}} Predicate
 */

/**
 * Hook Registry for managing all hooks across the system
 */
export class HookRegistry {
  constructor(eventBus) {
    this.eventBus = eventBus;
    this.hooks = new Map(); // id -> hook config
    this.stats = {
      totalHooks: 0,
      activeHooks: 0,
      executedHooks: 0,
      failedHooks: 0
    };
  }

  /**
   * Register a hook with event triggers and predicates
   * @param {Object} config - Hook configuration
   * @param {string} config.id - Unique hook identifier
   * @param {string[]} config.events - Events to listen for
   * @param {Predicate[]} [config.predicates] - Predicates to evaluate
   * @param {CombineOp} [config.combine='AND'] - How to combine predicate results
   * @param {Function} config.action - Hook action function
   * @param {Object} [config.options] - Additional options
   * @returns {Function} Unregister function
   */
  register(config) {
    if (this.hooks.has(config.id)) {
      throw new Error(`Hook with id '${config.id}' already registered`);
    }

    const unsubscribers = [];
    
    for (const event of config.events) {
      const registration = this.eventBus.on(event, async (payload) => {
        try {
          this.stats.executedHooks++;
          
          const ok = config.predicates 
            ? await evaluatePredicates(this.eventBus.engine, config.predicates, config.combine || 'AND') 
            : true;
          
          await config.action({ ...payload, store: this.eventBus.engine.store, engine: this.eventBus.engine }, ok);
        } catch (error) {
          this.stats.failedHooks++;
          console.error(`Hook ${config.id} failed:`, error);
          
          // Emit hook error event
          if (this.eventBus.engine) {
            this.eventBus.emit('hookError', {
              event: 'hookError',
              quad: payload.quad,
              context: this.eventBus.createContext('hook', { hookId: config.id, error: error.message }),
              store: this.eventBus.engine.store,
              engine: this.eventBus.engine
            });
          }
        }
      }, {
        id: Symbol(`hook_${config.id}_${event}`),
        ...config.options
      });
      
      unsubscribers.push(registration.unregister);
    }

    this.hooks.set(config.id, {
      ...config,
      unsubscribers,
      registeredAt: new Date().toISOString()
    });

    this.stats.totalHooks++;
    this.stats.activeHooks++;

    return () => this.unregister(config.id);
  }

  /**
   * Unregister a hook by ID
   * @param {string} id - Hook identifier
   */
  unregister(id) {
    const hook = this.hooks.get(id);
    if (!hook) {
      throw new Error(`Hook with id '${id}' not found`);
    }

    // Unregister from all events
    for (const unsub of hook.unsubscribers) {
      unsub();
    }

    this.hooks.delete(id);
    this.stats.activeHooks--;
  }

  /**
   * Get hook by ID
   * @param {string} id - Hook identifier
   * @returns {Object|null} Hook configuration
   */
  getHook(id) {
    return this.hooks.get(id) || null;
  }

  /**
   * Get all registered hooks
   * @returns {Array} Array of hook configurations
   */
  getAllHooks() {
    return Array.from(this.hooks.values());
  }

  /**
   * Get hook statistics
   * @returns {Object} Statistics object
   */
  getStats() {
    return {
      ...this.stats,
      hooksByEvent: this.getHooksByEvent()
    };
  }

  /**
   * Get hooks grouped by event
   * @returns {Object} Hooks grouped by event
   */
  getHooksByEvent() {
    const byEvent = {};
    for (const hook of this.hooks.values()) {
      for (const event of hook.events) {
        if (!byEvent[event]) byEvent[event] = [];
        byEvent[event].push(hook.id);
      }
    }
    return byEvent;
  }

  /**
   * Clear all hooks
   */
  clear() {
    for (const id of this.hooks.keys()) {
      this.unregister(id);
    }
  }
}

/**
 * Register a hook with event triggers and predicates (legacy function)
 * @param {RdfEngine} engine - RDF engine instance
 * @param {Object} cfg - Hook configuration
 * @returns {Function} Unregister function
 */
export function registerHook(engine, cfg) {
  if (!engine.hookRegistry) {
    engine.hookRegistry = new HookRegistry(engine.eventBus);
  }
  
  return engine.hookRegistry.register(cfg);
}

/**
 * Evaluate predicates against the current store state
 * @param {RdfEngine} engine - RDF engine instance
 * @param {Predicate[]} preds - Predicates to evaluate
 * @param {CombineOp} combine - How to combine results
 * @returns {Promise<boolean>} Combined evaluation result
 */
async function evaluatePredicates(engine, preds, combine) {
  const results = [];
  
  for (const p of preds) {
    let result = false;
    
    try {
      if (p.kind === 'COUNT') {
        const q = p.spec.query; // SELECT (COUNT(*) AS ?n) ...
        const r = await engine.query(q);
        const n = Number(r.results?.[0]?.n?.value || 0);
        result = compare(n, p.spec.operator, p.spec.value);
      } else if (p.kind === 'ASK') {
        const r = await engine.query(p.spec.query);
        result = !!r.boolean;
      } else if (p.kind === 'THRESHOLD') {
        const q = p.spec.query; // SELECT ?value WHERE { ... }
        const r = await engine.query(q);
        const values = r.results?.map(row => Number(row[p.spec.variable]?.value || 0)) || [];
        const avgValue = values.length > 0 ? values.reduce((sum, val) => sum + val, 0) / values.length : 0;
        result = compare(avgValue, p.spec.operator, p.spec.value);
      } else if (p.kind === 'SHACL') {
        // SHACL validation predicate
        const validationResult = await engine.validateShacl(engine.store, p.spec.shapes);
        result = validationResult.conforms;
      } else if (p.kind === 'DELTA') {
        // Delta-based predicate (compare current state with previous)
        const currentQuery = p.spec.currentQuery;
        const previousQuery = p.spec.previousQuery;
        const currentResult = await engine.query(currentQuery);
        const previousResult = await engine.query(previousQuery);
        
        const currentValue = Number(currentResult.results?.[0]?.value?.value || 0);
        const previousValue = Number(previousResult.results?.[0]?.value?.value || 0);
        const delta = currentValue - previousValue;
        
        result = compare(delta, p.spec.operator, p.spec.value);
      } else if (p.kind === 'CUSTOM') {
        // Custom predicate function
        result = await p.spec.function(engine, p.spec);
      } else {
        // Default to true for unknown predicate types
        result = true;
      }
    } catch (error) {
      console.error(`Predicate evaluation failed:`, error);
      result = false;
    }
    
    results.push(result);
  }
  
  return combine === 'AND' ? results.every(Boolean) : results.some(Boolean);
}

/**
 * Compare two values using the specified operator
 * @param {number} n - First value
 * @param {string} op - Comparison operator
 * @param {number} v - Second value
 * @returns {boolean} Comparison result
 */
function compare(n, op, v) {
  if (op === '>') return n > v;
  if (op === '>=') return n >= v;
  if (op === '<') return n < v;
  if (op === '<=') return n <= v;
  if (op === '=') return n === v;
  if (op === '!=') return n !== v;
  if (op === '~=') return Math.abs(n - v) < 0.001; // Approximate equality
  return false;
}

/**
 * Create a hook test runner for testing hooks in isolation
 */
export class HookTestRunner {
  constructor(engine) {
    this.engine = engine;
  }

  /**
   * Test a hook with various scenarios
   * @param {Object} hookConfig - Hook configuration
   * @param {Array} scenarios - Test scenarios
   * @returns {Promise<Object>} Test results
   */
  async testHook(hookConfig, scenarios) {
    const results = {
      hookId: hookConfig.id,
      totalScenarios: scenarios.length,
      passed: 0,
      failed: 0,
      errors: [],
      performance: []
    };

    for (const scenario of scenarios) {
      try {
        const startTime = performance.now();
        
        // Set up test data
        if (scenario.setup) {
          await scenario.setup(this.engine);
        }

        // Register the hook
        const unregister = registerHook(this.engine, hookConfig);

        // Execute the trigger
        if (scenario.trigger) {
          await scenario.trigger(this.engine);
        }

        // Wait for async operations
        if (scenario.wait) {
          await new Promise(resolve => setTimeout(resolve, scenario.wait));
        }

        // Verify results
        const passed = scenario.verify ? await scenario.verify(this.engine) : true;
        
        const endTime = performance.now();
        const duration = endTime - startTime;

        if (passed) {
          results.passed++;
        } else {
          results.failed++;
          results.errors.push({
            scenario: scenario.name || 'unnamed',
            error: 'Verification failed'
          });
        }

        results.performance.push({
          scenario: scenario.name || 'unnamed',
          duration
        });

        // Clean up
        unregister();
        if (scenario.cleanup) {
          await scenario.cleanup(this.engine);
        }

      } catch (error) {
        results.failed++;
        results.errors.push({
          scenario: scenario.name || 'unnamed',
          error: error.message
        });
      }
    }

    return results;
  }
}
