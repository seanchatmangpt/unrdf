/**
 * @file UNRDF Test Utilities
 * @module test-utils
 *
 * @description
 * Comprehensive testing utilities for UNRDF knowledge engine with
 * scenario DSL, fluent assertions, and helper functions.
 */

import { createStore } from '@unrdf/oxigraph';
import { randomUUID } from 'crypto';
import { z } from 'zod';

// NOTE: Legacy imports commented out - these are for internal use only
// import { KnowledgeHookManager } from '../knowledge-engine/knowledge-hook-manager.mjs';
// import { _PolicyPackManager } from '../knowledge-engine/policy-pack.mjs';
// import { _createLockchainWriter } from '../knowledge-engine/lockchain-writer.mjs';
// import { _createEffectSandbox } from '../knowledge-engine/effect-sandbox.mjs';

/**
 * Schema for test scenario
 */
const TestScenarioSchema = z.object({
  name: z.string().min(1),
  description: z.string().optional(),
  setup: z.function().optional().nullable(),
  teardown: z.function().optional().nullable(),
  steps: z
    .array(
      z.object({
        name: z.string().min(1),
        action: z.function(),
        assertions: z.array(z.function()).optional(),
      })
    )
    .min(1),
});

/**
 * Schema for test context
 */
const _TestContextSchema = z.object({
  store: z.any(), // Store instance
  manager: z.any(), // KnowledgeHookManager instance
  policyPackManager: z.any().optional(), // PolicyPackManager instance
  lockchainWriter: z.any().optional(), // LockchainWriter instance
  sandbox: z.any().optional(), // EffectSandbox instance
  metadata: z.record(z.any()).optional(),
});

/**
 * Test Scenario Builder
 */
export class TestScenario {
  /**
   * Create a new test scenario
   * @param {string} name - Scenario name
   * @param {string} [description] - Scenario description
   */
  constructor(name, description = '') {
    this.name = name;
    this.description = description;
    this.setup = null;
    this.teardown = null;
    this.steps = [];
    this.context = null;
  }

  /**
   * Set up scenario
   * @param {Function} setupFn - Setup function
   * @returns {TestScenario} This instance
   */
  setupScenario(setupFn) {
    this.setup = setupFn;
    return this;
  }

  /**
   * Tear down scenario
   * @param {Function} teardownFn - Teardown function
   * @returns {TestScenario} This instance
   */
  teardownScenario(teardownFn) {
    this.teardown = teardownFn;
    return this;
  }

  /**
   * Add a test step
   * @param {string} name - Step name
   * @param {Function} action - Step action
   * @param {Array<Function>} [assertions] - Step assertions
   * @returns {TestScenario} This instance
   */
  step(name, action, assertions = []) {
    this.steps.push({ name, action, assertions });
    return this;
  }

  /**
   * Execute the scenario
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async execute(_options = {}) {
    const result = {
      name: this.name,
      description: this.description,
      success: true,
      steps: [],
      errors: [],
      duration: 0,
    };

    const startTime = Date.now();

    try {
      // Validate scenario
      try {
        TestScenarioSchema.parse({
          name: this.name,
          description: this.description,
          setup: this.setup,
          teardown: this.teardown,
          steps: this.steps,
        });
      } catch (validationError) {
        if (validationError.issues) {
          throw new Error(
            `Invalid scenario: ${validationError.issues.map(e => e.message).join(', ')}`
          );
        }
        throw validationError;
      }

      // Setup
      if (this.setup) {
        this.context = await this.setup();
      }

      // Execute steps
      for (const step of this.steps) {
        const stepResult = await this._executeStep(step);
        result.steps.push(stepResult);

        if (!stepResult.success) {
          result.success = false;
          result.errors.push(`Step "${step.name}" failed: ${stepResult.error}`);
        }
      }

      // Teardown
      if (this.teardown) {
        await this.teardown(this.context);
      }
    } catch (error) {
      result.success = false;
      result.errors.push(error.message);
    }

    result.duration = Date.now() - startTime;
    return result;
  }

  /**
   * Execute a single step
   * @param {Object} step - Step definition
   * @returns {Promise<Object>} Step result
   * @private
   */
  async _executeStep(step) {
    const stepResult = {
      name: step.name,
      success: true,
      error: null,
      assertions: step.assertions || [],
      duration: 0,
    };

    const startTime = Date.now();

    try {
      // Execute action
      const actionResult = await step.action(this.context);
      stepResult.actionResult = actionResult;

      // Execute assertions if they exist
      if (step.assertions && Array.isArray(step.assertions)) {
        for (const assertion of step.assertions) {
          try {
            const assertionResult = await assertion(this.context, actionResult);
            stepResult.assertions.push({
              success: true,
              result: assertionResult,
            });
          } catch (assertionError) {
            stepResult.assertions.push({
              success: false,
              error: assertionError.message,
            });
            stepResult.success = false;
          }
        }
      } else {
        // Initialize empty assertions array if not provided
        stepResult.assertions = [];
      }
    } catch (error) {
      stepResult.success = false;
      stepResult.error = error.message;
    }

    stepResult.duration = Date.now() - startTime;
    return stepResult;
  }
}

/**
 * Fluent Assertions Builder
 */
export class FluentAssertions {
  /**
   * Create fluent assertions
   * @param {Object} context - Test context
   * @param {any} result - Action result
   */
  constructor(context, result) {
    this.context = context;
    this.result = result;
  }

  /**
   * Assert receipt is committed
   * @returns {FluentAssertions} This instance
   */
  toBeCommitted() {
    if (!this.result || !this.result.receipt) {
      throw new Error('Expected result to have a receipt');
    }
    if (!this.result.receipt.committed) {
      throw new Error('Expected receipt to be committed');
    }
    return this;
  }

  /**
   * Assert receipt is not committed
   * @returns {FluentAssertions} This instance
   */
  toNotBeCommitted() {
    if (!this.result || !this.result.receipt) {
      throw new Error('Expected result to have a receipt');
    }
    if (this.result.receipt.committed) {
      throw new Error('Expected receipt to not be committed');
    }
    return this;
  }

  /**
   * Assert hook was vetoed
   * @param {string} hookName - Hook name
   * @param {boolean} [vetoed=true] - Expected veto status
   * @returns {FluentAssertions} This instance
   */
  expectHook(hookName, vetoed = true) {
    if (!this.result || !this.result.receipt) {
      throw new Error('Expected result to have a receipt');
    }

    const hookResult = this.result.receipt.hookResults?.find(h => h.hookId === hookName);
    if (!hookResult) {
      throw new Error(`Hook "${hookName}" not found in results`);
    }

    if (vetoed && hookResult.result) {
      throw new Error(`Expected hook "${hookName}" to be vetoed`);
    }
    if (!vetoed && !hookResult.result) {
      throw new Error(`Expected hook "${hookName}" to not be vetoed`);
    }

    return this;
  }

  /**
   * Assert store contains quads
   * @param {Array} expectedQuads - Expected quads
   * @returns {FluentAssertions} This instance
   */
  toContainQuads(expectedQuads) {
    if (!this.context || !this.context.store) {
      throw new Error('Expected context to have a store');
    }

    const store = this.context.store;
    for (const expectedQuad of expectedQuads) {
      const found = Array.from(
        store.match(
          expectedQuad.subject,
          expectedQuad.predicate,
          expectedQuad.object,
          expectedQuad.graph
        )
      );
      if (found.length === 0) {
        throw new Error(`Expected quad not found: ${JSON.stringify(expectedQuad)}`);
      }
    }
    return this;
  }

  /**
   * Assert store does not contain quads
   * @param {Array} unexpectedQuads - Unexpected quads
   * @returns {FluentAssertions} This instance
   */
  toNotContainQuads(unexpectedQuads) {
    if (!this.context || !this.context.store) {
      throw new Error('Expected context to have a store');
    }

    const store = this.context.store;
    for (const unexpectedQuad of unexpectedQuads) {
      const found = Array.from(
        store.match(
          unexpectedQuad.subject,
          unexpectedQuad.predicate,
          unexpectedQuad.object,
          unexpectedQuad.graph
        )
      );
      if (found.length > 0) {
        throw new Error(`Unexpected quad found: ${JSON.stringify(unexpectedQuad)}`);
      }
    }
    return this;
  }

  /**
   * Assert store size
   * @param {number} expectedSize - Expected size
   * @returns {FluentAssertions} This instance
   */
  toHaveSize(expectedSize) {
    if (!this.context || !this.context.store) {
      throw new Error('Expected context to have a store');
    }

    const actualSize = this.context.store.size;
    if (actualSize !== expectedSize) {
      throw new Error(`Expected store size ${expectedSize}, got ${actualSize}`);
    }
    return this;
  }

  /**
   * Assert result has property
   * @param {string} property - Property name
   * @param {any} [expectedValue] - Expected value
   * @returns {FluentAssertions} This instance
   */
  toHaveProperty(property, expectedValue) {
    if (!(property in this.result)) {
      throw new Error(`Expected result to have property "${property}"`);
    }

    if (expectedValue !== undefined && this.result[property] !== expectedValue) {
      throw new Error(
        `Expected property "${property}" to be ${expectedValue}, got ${this.result[property]}`
      );
    }
    return this;
  }

  /**
   * Assert result does not have property
   * @param {string} property - Property name
   * @returns {FluentAssertions} This instance
   */
  toNotHaveProperty(property) {
    if (property in this.result) {
      throw new Error(`Expected result to not have property "${property}"`);
    }
    return this;
  }

  /**
   * Assert result matches schema
   * @param {z.ZodSchema} schema - Zod schema
   * @returns {FluentAssertions} This instance
   */
  toMatchSchema(schema) {
    try {
      schema.parse(this.result);
    } catch (error) {
      throw new Error(`Result does not match schema: ${error.message}`);
    }
    return this;
  }

  /**
   * Assert duration is within range
   * @param {number} maxDuration - Maximum duration in milliseconds
   * @returns {FluentAssertions} This instance
   */
  toCompleteWithin(maxDuration) {
    if (!this.result || typeof this.result.duration !== 'number') {
      throw new Error('Expected result to have a duration property');
    }

    if (this.result.duration > maxDuration) {
      throw new Error(
        `Expected duration to be within ${maxDuration}ms, got ${this.result.duration}ms`
      );
    }
    return this;
  }
}

/**
 * Test Context Builder
 */
export class TestContextBuilder {
  /**
   * Create a new test context builder
   */
  constructor() {
    this.context = {
      store: createStore(),
      metadata: {},
    };
  }

  /**
   * Set the store
   * @param {Store} store - RDF store
   * @returns {TestContextBuilder} This instance
   */
  withStore(store) {
    this.context.store = store;
    return this;
  }

  /**
   * Add quads to the store
   * @param {Array} quads - Quads to add
   * @returns {TestContextBuilder} This instance
   */
  withQuads(quads) {
    for (const quad of quads) {
      this.context.store.add(quad);
    }
    return this;
  }

  /**
   * Set the knowledge hook manager
   * @param {KnowledgeHookManager} manager - Manager instance
   * @returns {TestContextBuilder} This instance
   */
  withManager(manager) {
    this.context.manager = manager;
    return this;
  }

  /**
   * Set the policy pack manager
   * @param {PolicyPackManager} policyPackManager - Policy pack manager
   * @returns {TestContextBuilder} This instance
   */
  withPolicyPackManager(policyPackManager) {
    this.context.policyPackManager = policyPackManager;
    return this;
  }

  /**
   * Set the lockchain writer
   * @param {Object} lockchainWriter - Lockchain writer
   * @returns {TestContextBuilder} This instance
   */
  withLockchainWriter(lockchainWriter) {
    this.context.lockchainWriter = lockchainWriter;
    return this;
  }

  /**
   * Set the effect sandbox
   * @param {Object} sandbox - Effect sandbox
   * @returns {TestContextBuilder} This instance
   */
  withSandbox(sandbox) {
    this.context.sandbox = sandbox;
    return this;
  }

  /**
   * Add metadata
   * @param {Object} metadata - Metadata to add
   * @returns {TestContextBuilder} This instance
   */
  withMetadata(metadata) {
    this.context.metadata = { ...this.context.metadata, ...metadata };
    return this;
  }

  /**
   * Build the context
   * @returns {Object} Test context
   */
  build() {
    return this.context;
  }
}

/**
 * Helper functions for creating test data
 */
export const TestHelpers = {
  /**
   * Create a simple quad
   * @param {string} subject - Subject URI
   * @param {string} predicate - Predicate URI
   * @param {string} object - Object URI
   * @param {string} [graph] - Graph URI
   * @returns {Object} RDF quad
   */
  createQuad(subject, predicate, object, graph) {
    return {
      subject: { value: subject, termType: 'NamedNode' },
      predicate: { value: predicate, termType: 'NamedNode' },
      object: { value: object, termType: 'NamedNode' },
      graph: graph ? { value: graph, termType: 'NamedNode' } : undefined,
    };
  },

  /**
   * Create a delta
   * @param {Array} additions - Quads to add
   * @param {Array} [removals=[]] - Quads to remove
   * @returns {Object} Transaction delta
   */
  createDelta(additions, removals = []) {
    return {
      additions,
      removals,
      id: randomUUID(),
      timestamp: new Date(),
    };
  },

  /**
   * Create a knowledge hook
   * @param {string} name - Hook name
   * @param {Function} run - Run function
   * @param {Object} [when] - When condition
   * @param {Object} [options] - Additional options
   * @returns {Object} Knowledge hook
   */
  createKnowledgeHook(name, run, when, options = {}) {
    return {
      meta: {
        name,
        description: options.description || `Test hook ${name}`,
        version: options.version || '1.0.0',
      },
      when: when || {
        kind: 'sparql-ask',
        ref: { uri: 'file://test.rq', sha256: 'test' },
      },
      run,
      ...options,
    };
  },

  /**
   * Create a policy pack manifest
   * @param {string} name - Pack name
   * @param {Array} hooks - Hook definitions
   * @param {Object} [options] - Additional options
   * @returns {Object} Policy pack manifest
   */
  createPolicyPackManifest(name, hooks, options = {}) {
    return {
      id: `test-${name}-${Date.now()}`,
      meta: {
        name,
        version: options.version || '1.0.0',
        description: options.description || `Test policy pack ${name}`,
        author: options.author || 'test',
        license: options.license || 'MIT',
      },
      config: {
        enabled: true,
        priority: options.priority || 50,
        strictMode: options.strictMode || false,
      },
      hooks: hooks.map(hook => ({
        name: hook.meta.name,
        file: `${hook.meta.name}.mjs`,
        enabled: true,
        priority: hook.priority || 50,
      })),
    };
  },
};

/**
 * Create a test scenario
 * @param {string} name - Scenario name
 * @param {string} [description] - Scenario description
 * @returns {TestScenario} Test scenario
 */
export function scenario(name, description = '') {
  return new TestScenario(name, description);
}

/**
 * Create fluent assertions
 * @param {Object} context - Test context
 * @param {any} result - Action result
 * @returns {FluentAssertions} Fluent assertions
 */
export function expect(context, result) {
  return new FluentAssertions(context, result);
}

/**
 * Create a test context builder
 * @returns {TestContextBuilder} Test context builder
 */
export function createTestContext() {
  return new TestContextBuilder();
}

/**
 * Create a default test context
 * @returns {Object} Default test context
 */
export function createDefaultTestContext() {
  return new TestContextBuilder()
    .withStore(createStore())
    // .withManager(new KnowledgeHookManager()) // Commented out - internal use only
    .build();
}

// Export enhanced helpers
export {
  createTestStore,
  createTestWorkflow,
  mockOTEL,
  waitForCondition,
  createQuad,
  measureTime,
  testBatch,
  snapshotStore,
  assertSnapshotsEqual,
} from './helpers.mjs';

// Export fixtures
export {
  sampleRDF,
  sampleWorkflows,
  sampleCaseData,
  sampleHooks,
  sampleQueries,
  performanceFixtures,
  errorScenarios,
} from './fixtures.mjs';
