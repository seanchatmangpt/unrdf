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
  // Example (uncomment if needed):
  // { rule: "query:advanced", package: "@unrdf/knowledge-engine", reason: "KE has better semantics" }
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
