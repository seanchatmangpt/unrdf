/**
 * @fileoverview Manifest of extensions to load into the CLI registry.
 *
 * This is the AUTHORITATIVE list of which extensions load, in what order.
 * Order (Λ) is ≺-total (completely ordered, no ambiguity).
 *
 * Loading order:
 * 1. Core infrastructure (0-9)
 * 2. High-priority integrations (10-19)
 * 3. Standard packages (20-99)
 *
 * Collisions are resolved by:
 * 1. Load order (lower priority wins, then later load order)
 * 2. Explicit override rules (if unresolvable by order)
 * 3. Fail closed (default) if unresolved
 */

/**
 * Extension manifest entry.
 * @typedef {Object} ManifestEntry
 * @property {string} id - Package name (e.g., @unrdf/kgc-4d)
 * @property {string} path - Relative path to extension module
 * @property {number} loadOrder - Λ ordering (lower = earlier)
 * @property {boolean} enabled - Whether to load this extension
 */

/**
 * Ordered list of extensions to load.
 * @type {Array<ManifestEntry>}
 */
export const extensions = [
  // ===== CORE (0-9) =====
  // Reserved for core infra, currently none

  // ===== HIGH PRIORITY (10-19) =====
  // KGC Suite: 4D snapshot/universe management
  {
    id: '@unrdf/kgc-4d',
    path: '../extensions/kgc-4d.mjs',
    loadOrder: 10,
    enabled: true
  },
  // Blockchain/merkle receipts
  {
    id: '@unrdf/blockchain',
    path: '../extensions/blockchain.mjs',
    loadOrder: 11,
    enabled: true
  },
  // Hooks and policies
  {
    id: '@unrdf/hooks',
    path: '../extensions/hooks.mjs',
    loadOrder: 12,
    enabled: true
  },
  // KGC Probe - Automated integrity scanning
  {
    id: '@unrdf/kgc-probe',
    path: '../extensions/kgc-probe.mjs',
    loadOrder: 13,
    enabled: true
  },

  // ===== STANDARD PACKAGES (20-99) =====
  // Query layer (oxigraph, federation, knowledge engine, semantic search)
  {
    id: '@unrdf/oxigraph',
    path: '../extensions/oxigraph.mjs',
    loadOrder: 20,
    enabled: true
  },
  {
    id: '@unrdf/federation',
    path: '../extensions/federation.mjs',
    loadOrder: 21,
    enabled: true
  },
  {
    id: '@unrdf/semantic-search',
    path: '../extensions/semantic-search.mjs',
    loadOrder: 22,
    enabled: true
  },
  {
    id: '@unrdf/knowledge-engine',
    path: '../extensions/knowledge-engine.mjs',
    loadOrder: 23,
    enabled: true
  },

  // Event/streaming (yawl ecosystem)
  {
    id: '@unrdf/streaming',
    path: '../extensions/streaming.mjs',
    loadOrder: 30,
    enabled: true
  },
  {
    id: '@unrdf/yawl',
    path: '../extensions/yawl.mjs',
    loadOrder: 31,
    enabled: true
  },
  {
    id: '@unrdf/yawl-observability',
    path: '../extensions/yawl-observability.mjs',
    loadOrder: 32,
    enabled: true
  },

  // AI/ML stack
  {
    id: '@unrdf/ml-inference',
    path: '../extensions/ml-inference.mjs',
    loadOrder: 40,
    enabled: true
  },
  {
    id: '@unrdf/ml-versioning',
    path: '../extensions/ml-versioning.mjs',
    loadOrder: 41,
    enabled: true
  },

  // Observability and utilities
  {
    id: '@unrdf/observability',
    path: '../extensions/observability.mjs',
    loadOrder: 50,
    enabled: true
  },
  {
    id: '@unrdf/caching',
    path: '../extensions/caching.mjs',
    loadOrder: 51,
    enabled: true
  },
  {
    id: '@unrdf/graph-analytics',
    path: '../extensions/graph-analytics.mjs',
    loadOrder: 52,
    enabled: true
  },

  // YAWL Extended Ecosystem (60-69)
  {
    id: '@unrdf/yawl-api',
    path: '../extensions/yawl-api.mjs',
    loadOrder: 60,
    enabled: true
  },
  {
    id: '@unrdf/yawl-queue',
    path: '../extensions/yawl-queue.mjs',
    loadOrder: 61,
    enabled: true
  },
  {
    id: '@unrdf/yawl-viz',
    path: '../extensions/yawl-viz.mjs',
    loadOrder: 62,
    enabled: true
  },
  {
    id: '@unrdf/yawl-durable',
    path: '../extensions/yawl-durable.mjs',
    loadOrder: 63,
    enabled: true
  },
  {
    id: '@unrdf/yawl-langchain',
    path: '../extensions/yawl-langchain.mjs',
    loadOrder: 64,
    enabled: true
  },
  {
    id: '@unrdf/yawl-realtime',
    path: '../extensions/yawl-realtime.mjs',
    loadOrder: 65,
    enabled: true
  },

  // KGC Suite Extensions (70-74)
  {
    id: '@unrdf/kgc-substrate',
    path: '../extensions/kgc-substrate.mjs',
    loadOrder: 70,
    enabled: true
  },
  {
    id: '@unrdf/kgc-claude',
    path: '../extensions/kgc-claude.mjs',
    loadOrder: 71,
    enabled: true
  },
  {
    id: '@unrdf/kgn',
    path: '../extensions/kgn.mjs',
    loadOrder: 72,
    enabled: true
  },

  // GraphQL & Domain (75-77)
  {
    id: '@unrdf/rdf-graphql',
    path: '../extensions/rdf-graphql.mjs',
    loadOrder: 75,
    enabled: true
  },
  {
    id: '@unrdf/domain',
    path: '../extensions/domain.mjs',
    loadOrder: 76,
    enabled: true
  },
  {
    id: '@unrdf/fusion',
    path: '../extensions/fusion.mjs',
    loadOrder: 77,
    enabled: true
  },

  // Infrastructure & Platform (80-89)
  {
    id: '@unrdf/core',
    path: '../extensions/core.mjs',
    loadOrder: 80,
    enabled: true
  },
  {
    id: '@unrdf/composables',
    path: '../extensions/composables.mjs',
    loadOrder: 81,
    enabled: true
  },
  {
    id: '@unrdf/consensus',
    path: '../extensions/consensus.mjs',
    loadOrder: 82,
    enabled: true
  },
  {
    id: '@unrdf/validation',
    path: '../extensions/validation.mjs',
    loadOrder: 83,
    enabled: true
  },

  // Developer Tools (90-99)
  {
    id: '@unrdf/test-utils',
    path: '../extensions/test.mjs',
    loadOrder: 90,
    enabled: true
  },
  {
    id: '@unrdf/docs',
    path: '../extensions/docs.mjs',
    loadOrder: 91,
    enabled: true
  },
  {
    id: '@unrdf/serverless',
    path: '../extensions/serverless.mjs',
    loadOrder: 92,
    enabled: true
  },
  {
    id: '@unrdf/dark-matter',
    path: '../extensions/dark-matter.mjs',
    loadOrder: 93,
    enabled: true
  },

  // Additional Extensions (94-99)
  {
    id: '@unrdf/atomvm',
    path: '../extensions/atomvm.mjs',
    loadOrder: 94,
    enabled: true
  },
  {
    id: '@unrdf/cli',
    path: '../extensions/cli.mjs',
    loadOrder: 95,
    enabled: true
  },
  {
    id: '@unrdf/collab',
    path: '../extensions/collab.mjs',
    loadOrder: 96,
    enabled: true
  },
  {
    id: '@unrdf/engine-gateway',
    path: '../extensions/engine-gateway.mjs',
    loadOrder: 97,
    enabled: true
  },
  {
    id: '@unrdf/project-engine',
    path: '../extensions/project-engine.mjs',
    loadOrder: 98,
    enabled: true
  },
  {
    id: '@unrdf/yawl-ai',
    path: '../extensions/yawl-ai.mjs',
    loadOrder: 99,
    enabled: true
  },
  {
    id: '@unrdf/yawl-kafka',
    path: '../extensions/yawl-kafka.mjs',
    loadOrder: 100,
    enabled: true
  },

  // Aliases/Duplicates (101-105)
  {
    id: '@unrdf/analytics',
    path: '../extensions/analytics.mjs',
    loadOrder: 101,
    enabled: true
  },
  {
    id: '@unrdf/claude',
    path: '../extensions/claude.mjs',
    loadOrder: 102,
    enabled: true
  },
  {
    id: '@unrdf/deploy',
    path: '../extensions/deploy.mjs',
    loadOrder: 103,
    enabled: true
  },
  {
    id: '@unrdf/graphql',
    path: '../extensions/graphql.mjs',
    loadOrder: 104,
    enabled: true
  },
  {
    id: '@unrdf/substrate',
    path: '../extensions/substrate.mjs',
    loadOrder: 105,
    enabled: true
  }
];

/**
 * Collision override rules.
 *
 * Format: { rule: "noun:verb", package: "winner-package-id", reason: "why" }
 *
 * If two packages try to register the same noun:verb, the explicit override
 * determines the winner. If no override exists and failOnCollision=true,
 * the registry throws an error.
 *
 * @type {Array<{rule: string, package: string, reason?: string}>}
 */
export const overrides = [
  // Collision resolution for noun:verb conflicts
  {
    rule: "store:create",
    package: "@unrdf/oxigraph",
    reason: "Oxigraph specializes in RDF store operations"
  }
];

/**
 * Load enabled extensions from this manifest.
 *
 * @param {Registry} registry - Registry instance
 * @param {Object} options - Load options
 * @returns {Promise<void>}
 */
export async function loadManifest(registry, options = {}) {
  const { failOnMissing = false } = options;

  for (const entry of extensions) {
    if (!entry.enabled) {
      continue;
    }

    try {
      // Dynamic import of extension module
      const module = await import(entry.path);
      const ext = module.default || module.extension;

      if (!ext) {
        throw new Error(`No default export or .extension found in ${entry.path}`);
      }

      registry.registerExtension(ext, entry.loadOrder);
    } catch (e) {
      if (failOnMissing) {
        throw new Error(
          `Failed to load extension ${entry.id} from ${entry.path}: ${e.message}`
        );
      } else {
        // Soft fail: log warning but continue
        console.warn(
          `[kgc-cli] Warning: Could not load extension ${entry.id}: ${e.message}`
        );
      }
    }
  }
}

/**
 * Get the load order for a package ID.
 * @param {string} id - Package name
 * @returns {number|undefined}
 */
export function getLoadOrder(id) {
  return extensions.find(e => e.id === id)?.loadOrder;
}

export default {
  extensions,
  overrides,
  loadManifest,
  getLoadOrder
};
