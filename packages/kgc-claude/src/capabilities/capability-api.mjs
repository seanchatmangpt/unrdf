/**
 * @file Capability API - Unified Entry Point
 * @module kgc-claude/capabilities/capability-api
 *
 * @description
 * Single entry point for all Claude Code capabilities.
 * Provides capability discovery, initialization, and usage examples.
 */

import { createCapabilityMap } from './capability-map.mjs';
import { GovernanceEngine, createGovernanceEngine } from './governance-engine.mjs';
import { HookComposer, createHookComposer } from './hook-composition.mjs';
import { IDEIntegration, createIDEIntegration } from './ide-integration.mjs';
import { TimeTravelManager, createTimeTravelManager } from './time-travel.mjs';

/* ========================================================================= */
/* Capability Registry                                                       */
/* ========================================================================= */

/**
 * CapabilityRegistry - Discover and initialize capabilities
 */
export class CapabilityRegistry {
  constructor() {
    /** @type {CapabilityMap} */
    this.map = createCapabilityMap();

    /** @type {Map<string, Object>} */
    this.instances = new Map();

    this._registerImplementations();
  }

  /**
   * Register available capability implementations
   * @private
   */
  _registerImplementations() {
    // Register governance engine
    this._registerCapability('governance', {
      factory: createGovernanceEngine,
      class: GovernanceEngine,
      capabilityIds: ['hooks', 'tool_permissions'],
      description: 'RBAC and tool permission governance',
    });

    // Register hook composer
    this._registerCapability('hook-composer', {
      factory: createHookComposer,
      class: HookComposer,
      capabilityIds: ['hooks'],
      description: 'Advanced hook composition and chaining',
    });

    // Register IDE integration
    this._registerCapability('ide-integration', {
      factory: createIDEIntegration,
      class: IDEIntegration,
      capabilityIds: ['ide_surface'],
      description: 'Code actions, diagnostics, and suggestions',
    });

    // Register time travel
    this._registerCapability('time-travel', {
      factory: createTimeTravelManager,
      class: TimeTravelManager,
      capabilityIds: ['checkpointing'],
      description: 'Named checkpoints and time travel',
    });
  }

  /**
   * Register a capability implementation
   * @private
   */
  _registerCapability(name, config) {
    this.instances.set(name, {
      name,
      ...config,
      initialized: false,
      instance: null,
    });
  }

  /**
   * Get capability map
   * @returns {CapabilityMap}
   */
  getMap() {
    return this.map;
  }

  /**
   * List all available implementations
   * @returns {Object[]}
   */
  listImplementations() {
    return Array.from(this.instances.values()).map(impl => ({
      name: impl.name,
      description: impl.description,
      capabilityIds: impl.capabilityIds,
      initialized: impl.initialized,
    }));
  }

  /**
   * Get capability implementation
   * @param {string} name - Implementation name
   * @returns {Object|undefined}
   */
  getImplementation(name) {
    const impl = this.instances.get(name);
    return impl ? impl.instance : undefined;
  }

  /**
   * Initialize capability implementation
   * @param {string} name - Implementation name
   * @param {Object} [options] - Initialization options
   * @returns {Object}
   */
  initialize(name, options = {}) {
    const impl = this.instances.get(name);
    if (!impl) {
      throw new Error(`Unknown capability implementation: ${name}`);
    }

    if (!impl.initialized) {
      impl.instance = impl.factory(options);
      impl.initialized = true;
    }

    return impl.instance;
  }

  /**
   * Check if capability is available
   * @param {string} capabilityId - Capability ID
   * @returns {boolean}
   */
  hasCapability(capabilityId) {
    return this.map.getCapability(capabilityId) !== undefined;
  }

  /**
   * Get implementations for capability
   * @param {string} capabilityId - Capability ID
   * @returns {Object[]}
   */
  getImplementationsForCapability(capabilityId) {
    return Array.from(this.instances.values())
      .filter(impl => impl.capabilityIds.includes(capabilityId))
      .map(impl => ({
        name: impl.name,
        description: impl.description,
        initialized: impl.initialized,
      }));
  }

  /**
   * Get usage examples
   * @returns {Object}
   */
  getExamples() {
    return {
      governance: this._getGovernanceExample(),
      hookComposer: this._getHookComposerExample(),
      ideIntegration: this._getIDEIntegrationExample(),
      timeTravel: this._getTimeTravelExample(),
    };
  }

  /* ======================================================================= */
  /* Usage Examples                                                          */
  /* ======================================================================= */

  /**
   * Get governance engine example
   * @private
   */
  _getGovernanceExample() {
    return {
      title: 'Governance Engine - RBAC and Tool Permissions',
      code: `
import { createCapabilityRegistry } from '@unrdf/kgc-claude/capabilities';

const registry = createCapabilityRegistry();

// Initialize governance with custom policy
const governance = registry.initialize('governance', {
  name: 'my-governance',
  defaultPermission: 'deny',
  roles: [
    {
      name: 'developer',
      permissions: [
        { tool: 'Read', permission: 'allow', priority: 80 },
        { tool: 'Write', permission: 'allow', priority: 80 },
        { tool: 'Bash(git:*)', permission: 'allow', priority: 80 },
        { tool: 'Bash(rm:*)', permission: 'ask', priority: 90 },
      ],
    },
  ],
});

// Register actor
governance.registerActor({
  id: 'alice',
  roles: ['developer'],
});

// Check permission
const decision = governance.checkPermission('alice', 'Bash(git status)');
console.log(decision); // { decision: 'allow', reason: '...' }

// Get audit log
const audit = governance.getAuditLog({ actor: 'alice' });
console.log(audit);
      `.trim(),
    };
  }

  /**
   * Get hook composer example
   * @private
   */
  _getHookComposerExample() {
    return {
      title: 'Hook Composer - Before/After/Around Hooks',
      code: `
import { createCapabilityRegistry } from '@unrdf/kgc-claude/capabilities';
import { createBeforeHook, createAfterHook } from '@unrdf/kgc-claude/capabilities/hook-composition';

const registry = createCapabilityRegistry();

// Create hooks
const validateHook = createBeforeHook('validate', async (context) => {
  if (!context.input) throw new Error('Input required');
}, { priority: 90 });

const logHook = createAfterHook('log', async (context) => {
  console.log('Result:', context.result);
});

// Initialize composer
const composer = registry.initialize('hook-composer', {
  name: 'my-workflow',
  hooks: [validateHook, logHook],
});

// Execute with hooks
const result = await composer.execute(
  async (ctx) => \`Processed: \${ctx.input}\`,
  { input: 'test data' }
);

console.log(result);
      `.trim(),
    };
  }

  /**
   * Get IDE integration example
   * @private
   */
  _getIDEIntegrationExample() {
    return {
      title: 'IDE Integration - Code Actions and Diagnostics',
      code: `
import { createCapabilityRegistry } from '@unrdf/kgc-claude/capabilities';

const registry = createCapabilityRegistry();
const ide = registry.initialize('ide-integration');

// Register code action
ide.registerCodeAction('src/index.mjs', {
  title: 'Extract to function',
  kind: 'refactor',
  edits: [{
    range: {
      start: { file: 'src/index.mjs', line: 10, column: 0 },
      end: { file: 'src/index.mjs', line: 15, column: 0 },
    },
    newText: 'function extracted() { ... }',
  }],
});

// Add diagnostic
ide.addDiagnostic('src/index.mjs', {
  range: {
    start: { file: 'src/index.mjs', line: 5, column: 0 },
    end: { file: 'src/index.mjs', line: 5, column: 10 },
  },
  severity: 'error',
  message: 'Undefined variable',
});

// Get diagnostics
const diagnostics = ide.getDiagnostics('src/index.mjs', 'error');
console.log(diagnostics);
      `.trim(),
    };
  }

  /**
   * Get time travel example
   * @private
   */
  _getTimeTravelExample() {
    return {
      title: 'Time Travel - Named Checkpoints',
      code: `
import { createCapabilityRegistry } from '@unrdf/kgc-claude/capabilities';

const registry = createCapabilityRegistry();

// Initialize time travel manager
const timeTravel = registry.initialize('time-travel', {
  store: myStore,
  gitBackbone: myGitBackbone,
});

// Create named checkpoint
const checkpoint = await timeTravel.createCheckpoint('before-refactor', {
  description: 'Safe state before major refactoring',
  tags: ['stable', 'v1.0'],
});

console.log('Checkpoint created:', checkpoint.id);

// ... make changes ...

// Restore to checkpoint
await timeTravel.restoreToCheckpoint('before-refactor');

// List all checkpoints
const checkpoints = timeTravel.listCheckpoints({
  branch: 'main',
  tag: 'stable',
});

console.log('Checkpoints:', checkpoints);
      `.trim(),
    };
  }
}

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create capability registry
 * @returns {CapabilityRegistry}
 */
export function createCapabilityRegistry() {
  return new CapabilityRegistry();
}

/**
 * Quick capability discovery
 * @param {string} keyword - Search keyword
 * @returns {Object}
 */
export function discoverCapability(keyword) {
  const registry = createCapabilityRegistry();
  const results = registry.getMap().search(keyword);

  return {
    capabilities: results,
    implementations: results.flatMap(cap =>
      registry.getImplementationsForCapability(cap.id)
    ),
  };
}

/**
 * Get quick start guide
 * @returns {Object}
 */
export function getQuickStart() {
  const registry = createCapabilityRegistry();

  return {
    overview: {
      totalCapabilities: registry.getMap().getAllCapabilities().length,
      implementations: registry.listImplementations().length,
      compositions: registry.getMap().getCompositions().length,
    },
    gettingStarted: {
      step1: 'Import capability registry',
      step2: 'Initialize desired capability',
      step3: 'Use capability API',
      example: registry.getExamples().governance,
    },
    capabilities: registry.getMap().getAllCapabilities().map(cap => ({
      id: cap.id,
      name: cap.name,
      category: cap.category,
      description: cap.description,
    })),
  };
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  CapabilityRegistry,
  createCapabilityRegistry,
  discoverCapability,
  getQuickStart,
};

// Re-export all capability implementations
export * from './capability-map.mjs';
export * from './governance-engine.mjs';
export * from './hook-composition.mjs';
export * from './ide-integration.mjs';
export * from './time-travel.mjs';
