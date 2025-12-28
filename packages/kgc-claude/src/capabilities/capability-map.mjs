/**
 * @file Capability Map - Master Capability Index
 * @module kgc-claude/capabilities/capability-map
 *
 * @description
 * Master index of all Claude Code capabilities synthesized from research agents 1-10.
 * Provides capability taxonomy, dependency graph, and discovery mechanisms.
 *
 * **Research Sources:**
 * - Agent 1: Subagents & Delegation
 * - Agent 2: Hooks & Tool Governance
 * - Agent 3: Plugins
 * - Agent 4: Slash Commands
 * - Agent 5: Model Context Protocol (MCP)
 * - Agent 6: Programmatic/Headless Execution
 * - Agent 7: Checkpointing & Rewind
 * - Agent 8: IDE/VS Code Surface
 * - Agent 9: Composition Hunter
 * - Agent 10: Synthesis Librarian
 */

import { z } from 'zod';

/* ========================================================================= */
/* Capability Taxonomy                                                       */
/* ========================================================================= */

/**
 * Capability categories
 * @enum {string}
 */
export const CapabilityCategory = {
  EXECUTION: 'execution',        // Subagents, background tasks
  CONTROL: 'control',            // Hooks, slash commands, permissions
  EXTENSION: 'extension',        // Plugins
  INTEGRATION: 'integration',    // MCP
  AUTOMATION: 'automation',      // Programmatic mode, output formats
  SAFETY: 'safety',              // Checkpointing, time travel
  INTERFACE: 'interface',        // IDE surface
  CAPABILITY: 'capability',      // Skills
};

/**
 * Schema for capability atom
 */
export const CapabilityAtomSchema = z.object({
  id: z.string(),
  name: z.string(),
  category: z.nativeEnum(CapabilityCategory),
  description: z.string(),
  primitives: z.array(z.string()),
  implementations: z.array(z.string()).optional(),
  docRef: z.string().url().optional(),
  researchAgent: z.string().optional(),
});

/**
 * Schema for capability dependency
 */
export const CapabilityDependencySchema = z.object({
  from: z.string(),
  to: z.string(),
  relationship: z.string(),
  strength: z.enum(['required', 'optional', 'enhances']),
});

/**
 * Schema for capability composition
 */
export const CapabilityCompositionSchema = z.object({
  components: z.array(z.string()).min(2),
  emergentProperty: z.string(),
  measurableMetrics: z.array(z.string()),
  tested: z.boolean().default(false),
  verdict: z.enum(['productive', 'not_productive', 'pending']).optional(),
});

/* ========================================================================= */
/* Capability Atoms                                                          */
/* ========================================================================= */

/**
 * All capability atoms from research agents
 */
export const CAPABILITY_ATOMS = [
  // Agent 1: Subagents & Delegation
  {
    id: 'subagents',
    name: 'Subagents & Delegation',
    category: CapabilityCategory.EXECUTION,
    description: 'Custom subagents for specialized work roles invoked via delegation tasks',
    primitives: ['Task tool', 'agent types', 'parallel spawning', 'stateless execution'],
    implementations: ['agent-harness.mjs'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/agents',
    researchAgent: 'cc-agent-01-subagents',
  },

  // Agent 2: Hooks & Tool Governance
  {
    id: 'hooks',
    name: 'Hooks & Tool Governance',
    category: CapabilityCategory.CONTROL,
    description: 'Shell commands that execute at tool lifecycle points with match rules',
    primitives: ['PreToolUse', 'PostToolUse', 'PreCompact', 'Stop', 'matchers', 'allow/deny/ask'],
    implementations: ['hook-composition.mjs', 'governance-engine.mjs'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/hooks',
    researchAgent: 'cc-agent-02-hooks',
  },

  // Agent 3: Plugins
  {
    id: 'plugins',
    name: 'Plugins',
    category: CapabilityCategory.EXTENSION,
    description: 'Shareable bundles of commands, agents, hooks, skills, MCP configs, LSP servers',
    primitives: ['namespaced routing', 'versioning', 'bundling', 'distribution'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/plugins',
    researchAgent: 'cc-agent-03-plugins',
  },

  // Agent 4: Slash Commands
  {
    id: 'slash_commands',
    name: 'Slash Commands',
    category: CapabilityCategory.CONTROL,
    description: 'Built-in and custom programmable commands with frontmatter and arguments',
    primitives: ['project commands', 'personal commands', 'arguments', 'namespacing'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/slash-commands',
    researchAgent: 'cc-agent-04-slash-commands',
  },

  // Agent 5: MCP
  {
    id: 'mcp',
    name: 'Model Context Protocol',
    category: CapabilityCategory.INTEGRATION,
    description: 'External tools and data connection with permissioning',
    primitives: ['server config', 'tool discovery', 'permission rules', 'dynamic invocation'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/mcp',
    researchAgent: 'cc-agent-05-mcp',
  },

  // Agent 6: Programmatic/Headless Execution
  {
    id: 'programmatic',
    name: 'Programmatic/Headless Execution',
    category: CapabilityCategory.AUTOMATION,
    description: 'Non-interactive execution via CLI with structured outputs',
    primitives: ['claude -p', 'text/json/stream-json', 'session resume', '--allowedTools'],
    docRef: 'https://docs.anthropic.com/en/docs/agent-sdk',
    researchAgent: 'cc-agent-06-programmatic',
  },

  // Agent 7: Checkpointing & Rewind
  {
    id: 'checkpointing',
    name: 'Checkpointing & Rewind',
    category: CapabilityCategory.SAFETY,
    description: 'Automatic state checkpoints with conversation/code rewind',
    primitives: ['/rewind', 'escape shortcut', 'code-only rewind', 'conversation-only rewind'],
    implementations: ['time-travel.mjs'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/checkpoints',
    researchAgent: 'cc-agent-07-checkpointing',
  },

  // Agent 8: IDE/VS Code Surface
  {
    id: 'ide_surface',
    name: 'IDE/VS Code Surface',
    category: CapabilityCategory.INTERFACE,
    description: 'Native extension with diff-and-approval workflow',
    primitives: ['plan review', 'inline diffs', '@-mentions', 'multi-conversation tabs'],
    implementations: ['ide-integration.mjs'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/ide',
    researchAgent: 'cc-agent-08-ide',
  },

  // Supplementary capabilities (from lattice)
  {
    id: 'background_tasks',
    name: 'Background Tasks',
    category: CapabilityCategory.EXECUTION,
    description: 'Long-running work that does not block interactive control',
    primitives: ['/bashes', 'async execution', 'output monitoring'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/background',
  },

  {
    id: 'skills',
    name: 'Agent Skills',
    category: CapabilityCategory.CAPABILITY,
    description: 'Model-invoked capabilities based on context',
    primitives: ['context triggers', 'automatic invocation', 'capability injection'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/skills',
  },

  {
    id: 'tool_permissions',
    name: 'Tool Permission Rules',
    category: CapabilityCategory.CONTROL,
    description: 'Explicit allow/deny patterns for tool usage',
    primitives: ['permission patterns', 'glob matching', 'safety rules'],
    implementations: ['governance-engine.mjs'],
    docRef: 'https://docs.anthropic.com/en/docs/claude-code/permissions',
  },

  {
    id: 'output_formats',
    name: 'Output Formats',
    category: CapabilityCategory.AUTOMATION,
    description: 'Structured output modes for downstream processing',
    primitives: ['text', 'json', 'stream-json', 'machine-readable'],
    docRef: 'https://docs.anthropic.com/en/docs/agent-sdk/output',
  },
];

/* ========================================================================= */
/* Capability Dependencies                                                   */
/* ========================================================================= */

/**
 * Known capability dependencies and relationships
 */
export const CAPABILITY_DEPENDENCIES = [
  {
    from: 'hooks',
    to: 'subagents',
    relationship: 'hooks can enforce policy per-subagent',
    strength: 'enhances',
  },
  {
    from: 'plugins',
    to: 'slash_commands',
    relationship: 'plugins can bundle custom slash commands',
    strength: 'enhances',
  },
  {
    from: 'plugins',
    to: 'mcp',
    relationship: 'plugins can include MCP server configs',
    strength: 'enhances',
  },
  {
    from: 'plugins',
    to: 'hooks',
    relationship: 'plugins can include hook configurations',
    strength: 'enhances',
  },
  {
    from: 'programmatic',
    to: 'output_formats',
    relationship: 'programmatic mode requires output format specification',
    strength: 'required',
  },
  {
    from: 'checkpointing',
    to: 'subagents',
    relationship: 'checkpoints enable safe parallel exploration',
    strength: 'enhances',
  },
  {
    from: 'tool_permissions',
    to: 'hooks',
    relationship: 'permissions can be enforced via hooks',
    strength: 'optional',
  },
  {
    from: 'skills',
    to: 'hooks',
    relationship: 'skills benefit from policy enforcement',
    strength: 'enhances',
  },
];

/* ========================================================================= */
/* Capability Compositions                                                   */
/* ========================================================================= */

/**
 * High-value capability compositions (from Agent 9 research)
 */
export const CAPABILITY_COMPOSITIONS = [
  {
    components: ['hooks', 'subagents', 'programmatic'],
    emergentProperty: 'Parallel execution with enforceable policy and machine-readable outputs',
    measurableMetrics: ['policy_strength', 'parallel_throughput', 'reproducibility'],
    tested: false,
    verdict: 'pending',
  },
  {
    components: ['checkpointing', 'subagents', 'ide_surface'],
    emergentProperty: 'Aggressive parallel exploration with visual review and rapid recovery',
    measurableMetrics: ['recovery_time', 'exploration_branches', 'operator_steps'],
    tested: false,
    verdict: 'pending',
  },
  {
    components: ['plugins', 'mcp', 'slash_commands'],
    emergentProperty: 'Portable, shareable capability products with external integration',
    measurableMetrics: ['operator_steps', 'reproducibility', 'setup_time'],
    tested: false,
    verdict: 'pending',
  },
  {
    components: ['skills', 'hooks'],
    emergentProperty: 'Automatic capability injection with safety guardrails',
    measurableMetrics: ['policy_strength', 'automation_level'],
    tested: false,
    verdict: 'pending',
  },
];

/* ========================================================================= */
/* Capability Map                                                            */
/* ========================================================================= */

/**
 * CapabilityMap - Master index and discovery
 */
export class CapabilityMap {
  constructor() {
    /** @type {Map<string, Object>} */
    this.atoms = new Map();

    /** @type {Map<string, Set<string>>} */
    this.categoryIndex = new Map();

    /** @type {Array<Object>} */
    this.dependencies = [];

    /** @type {Array<Object>} */
    this.compositions = [];

    this._initialize();
  }

  /**
   * Initialize capability map
   * @private
   */
  _initialize() {
    // Index atoms
    for (const atom of CAPABILITY_ATOMS) {
      const validated = CapabilityAtomSchema.parse(atom);
      this.atoms.set(validated.id, validated);

      // Category index
      if (!this.categoryIndex.has(validated.category)) {
        this.categoryIndex.set(validated.category, new Set());
      }
      this.categoryIndex.get(validated.category).add(validated.id);
    }

    // Index dependencies
    for (const dep of CAPABILITY_DEPENDENCIES) {
      this.dependencies.push(CapabilityDependencySchema.parse(dep));
    }

    // Index compositions
    for (const comp of CAPABILITY_COMPOSITIONS) {
      this.compositions.push(CapabilityCompositionSchema.parse(comp));
    }
  }

  /**
   * Get capability by ID
   * @param {string} id - Capability ID
   * @returns {Object|undefined}
   */
  getCapability(id) {
    return this.atoms.get(id);
  }

  /**
   * Get all capabilities
   * @returns {Object[]}
   */
  getAllCapabilities() {
    return Array.from(this.atoms.values());
  }

  /**
   * Get capabilities by category
   * @param {string} category - Category name
   * @returns {Object[]}
   */
  getCapabilitiesByCategory(category) {
    const ids = this.categoryIndex.get(category) || new Set();
    return Array.from(ids).map(id => this.atoms.get(id));
  }

  /**
   * Get dependencies for capability
   * @param {string} capabilityId - Capability ID
   * @param {string} [direction] - 'from' or 'to'
   * @returns {Object[]}
   */
  getDependencies(capabilityId, direction = null) {
    if (!direction) {
      return this.dependencies.filter(
        d => d.from === capabilityId || d.to === capabilityId
      );
    }

    if (direction === 'from') {
      return this.dependencies.filter(d => d.from === capabilityId);
    }

    if (direction === 'to') {
      return this.dependencies.filter(d => d.to === capabilityId);
    }

    return [];
  }

  /**
   * Get compositions containing capability
   * @param {string} capabilityId - Capability ID
   * @returns {Object[]}
   */
  getCompositionsForCapability(capabilityId) {
    return this.compositions.filter(c => c.components.includes(capabilityId));
  }

  /**
   * Get all compositions
   * @param {Object} [filters] - Filter options
   * @param {boolean} [filters.tested] - Filter by tested status
   * @param {string} [filters.verdict] - Filter by verdict
   * @returns {Object[]}
   */
  getCompositions(filters = {}) {
    let result = this.compositions;

    if (filters.tested !== undefined) {
      result = result.filter(c => c.tested === filters.tested);
    }

    if (filters.verdict) {
      result = result.filter(c => c.verdict === filters.verdict);
    }

    return result;
  }

  /**
   * Search capabilities by keyword
   * @param {string} keyword - Search keyword
   * @returns {Object[]}
   */
  search(keyword) {
    const lowerKeyword = keyword.toLowerCase();
    return Array.from(this.atoms.values()).filter(
      atom =>
        atom.name.toLowerCase().includes(lowerKeyword) ||
        atom.description.toLowerCase().includes(lowerKeyword) ||
        atom.primitives.some(p => p.toLowerCase().includes(lowerKeyword))
    );
  }

  /**
   * Export capability map as JSON
   * @returns {Object}
   */
  export() {
    return {
      atoms: Array.from(this.atoms.values()),
      dependencies: this.dependencies,
      compositions: this.compositions,
      categories: Object.values(CapabilityCategory),
    };
  }
}

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create capability map
 * @returns {CapabilityMap}
 */
export function createCapabilityMap() {
  return new CapabilityMap();
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  CapabilityCategory,
  CapabilityMap,
  createCapabilityMap,
  CAPABILITY_ATOMS,
  CAPABILITY_DEPENDENCIES,
  CAPABILITY_COMPOSITIONS,
};
