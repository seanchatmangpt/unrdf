/**
 * @file Measurement Registry
 * @module @unrdf/measurement/registry
 *
 * @description
 * Central registry defining which packages measure what metrics,
 * dependency graph for measurement rollup, and aggregation rules.
 *
 * @version 1.0.0
 */

import {
  TIER_CAPACITY,
  TIER_WEIGHTS,
  getExtractionTemplate
} from './schema.mjs';

// ============================================================================
// SECTION 1: PACKAGE REGISTRY
// ============================================================================

/**
 * Complete package registry with tier assignments and measurement config
 * @type {Map<string, PackageRegistryEntry>}
 */
export const PACKAGE_REGISTRY = new Map([
  // -------------------------------------------------------------------------
  // TIER 1: CORE PACKAGES (5)
  // -------------------------------------------------------------------------
  ['core', {
    name: 'core',
    path: 'packages/core',
    tier: 1,
    description: 'RDF Graph Operations, SPARQL Execution, Foundational Substrate',
    measures: { D_t: true, TC: true, TE: true, C_t: true },
    dependencies: ['oxigraph'],
    dependents: ['*'], // All packages depend on core
    criticality: 'critical',
    policyInfluencing: true
  }],
  ['oxigraph', {
    name: 'oxigraph',
    path: 'packages/oxigraph',
    tier: 1,
    description: 'OxiGraph Store Integration',
    measures: { D_t: true, TC: true, TE: true, C_t: true },
    dependencies: [],
    dependents: ['core', 'kgc-4d', 'kgn'],
    criticality: 'critical',
    policyInfluencing: true
  }],
  ['validation', {
    name: 'validation',
    path: 'packages/validation',
    tier: 1,
    description: 'SHACL and Schema Validation',
    measures: { D_t: true, TC: true, TE: true, C_t: true },
    dependencies: ['core'],
    dependents: ['hooks', 'admission'],
    criticality: 'critical',
    policyInfluencing: true
  }],
  ['domain', {
    name: 'domain',
    path: 'packages/domain',
    tier: 1,
    description: 'Domain Models and Types',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: [],
    dependents: ['core', 'kgc-4d'],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['test-utils', {
    name: 'test-utils',
    path: 'packages/test-utils',
    tier: 1,
    description: 'Testing Utilities and Fixtures',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: ['integration-tests'],
    criticality: 'high',
    policyInfluencing: false
  }],

  // -------------------------------------------------------------------------
  // TIER 2: INFRASTRUCTURE PACKAGES (8)
  // -------------------------------------------------------------------------
  ['hooks', {
    name: 'hooks',
    path: 'packages/hooks',
    tier: 2,
    description: 'Lifecycle Hooks and Middleware',
    measures: { D_t: true, TC: true, TE: true, C_t: true },
    dependencies: ['core', 'validation'],
    dependents: ['yawl', 'federation'],
    criticality: 'critical',
    policyInfluencing: true
  }],
  ['streaming', {
    name: 'streaming',
    path: 'packages/streaming',
    tier: 2,
    description: 'RDF Streaming and N-Quads Processing',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: ['federation', 'kgn'],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['observability', {
    name: 'observability',
    path: 'packages/observability',
    tier: 2,
    description: 'OTEL Integration and Metrics',
    measures: { D_t: true, TC: true, TE: true, C_t: true },
    dependencies: [],
    dependents: ['*'], // Observable by all
    criticality: 'critical',
    policyInfluencing: true
  }],
  ['caching', {
    name: 'caching',
    path: 'packages/caching',
    tier: 2,
    description: 'Query and Store Caching',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: ['kgc-4d', 'engine-gateway'],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['consensus', {
    name: 'consensus',
    path: 'packages/consensus',
    tier: 2,
    description: 'Distributed Consensus Protocols',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: ['federation', 'blockchain'],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['engine-gateway', {
    name: 'engine-gateway',
    path: 'packages/engine-gateway',
    tier: 2,
    description: 'API Gateway and Request Routing',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'observability'],
    dependents: ['serverless'],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['project-engine', {
    name: 'project-engine',
    path: 'packages/project-engine',
    tier: 2,
    description: 'Project Management Engine',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['serverless', {
    name: 'serverless',
    path: 'packages/serverless',
    tier: 2,
    description: 'Serverless Function Adapters',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'engine-gateway'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],

  // -------------------------------------------------------------------------
  // TIER 3: APPLICATION PACKAGES (12)
  // -------------------------------------------------------------------------
  ['kgc-4d', {
    name: 'kgc-4d',
    path: 'packages/kgc-4d',
    tier: 3,
    description: 'Knowledge Graph Compiler 4D',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'oxigraph', 'validation'],
    dependents: [],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['kgn', {
    name: 'kgn',
    path: 'packages/kgn',
    tier: 3,
    description: 'Knowledge Graph Network',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'streaming'],
    dependents: ['federation'],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['yawl', {
    name: 'yawl',
    path: 'packages/yawl',
    tier: 3,
    description: 'Yet Another Workflow Language',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'hooks'],
    dependents: ['yawl-*'],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['federation', {
    name: 'federation',
    path: 'packages/federation',
    tier: 3,
    description: 'Graph Federation Engine',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'streaming', 'kgn'],
    dependents: [],
    criticality: 'high',
    policyInfluencing: false
  }],
  ['graph-analytics', {
    name: 'graph-analytics',
    path: 'packages/graph-analytics',
    tier: 3,
    description: 'Graph Analytics Algorithms',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: ['knowledge-engine'],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['knowledge-engine', {
    name: 'knowledge-engine',
    path: 'packages/knowledge-engine',
    tier: 3,
    description: 'Knowledge Processing Engine',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core', 'graph-analytics'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['semantic-search', {
    name: 'semantic-search',
    path: 'packages/semantic-search',
    tier: 3,
    description: 'Semantic Search and Indexing',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['collab', {
    name: 'collab',
    path: 'packages/collab',
    tier: 3,
    description: 'Collaborative Editing',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['composables', {
    name: 'composables',
    path: 'packages/composables',
    tier: 3,
    description: 'Composable Functions Library',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: ['react'],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['blockchain', {
    name: 'blockchain',
    path: 'packages/blockchain',
    tier: 3,
    description: 'Blockchain Integration',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'consensus'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['ml-inference', {
    name: 'ml-inference',
    path: 'packages/ml-inference',
    tier: 3,
    description: 'ML Model Inference',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: ['yawl-ai'],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['ml-versioning', {
    name: 'ml-versioning',
    path: 'packages/ml-versioning',
    tier: 3,
    description: 'ML Model Versioning',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core', 'ml-inference'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],

  // -------------------------------------------------------------------------
  // TIER 4: INTEGRATION PACKAGES (12)
  // -------------------------------------------------------------------------
  ['yawl-ai', {
    name: 'yawl-ai',
    path: 'packages/yawl-ai',
    tier: 4,
    description: 'YAWL AI Integration',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl', 'ml-inference'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-api', {
    name: 'yawl-api',
    path: 'packages/yawl-api',
    tier: 4,
    description: 'YAWL REST/GraphQL API',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-durable', {
    name: 'yawl-durable',
    path: 'packages/yawl-durable',
    tier: 4,
    description: 'YAWL Durable Objects',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-kafka', {
    name: 'yawl-kafka',
    path: 'packages/yawl-kafka',
    tier: 4,
    description: 'YAWL Kafka Integration',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-langchain', {
    name: 'yawl-langchain',
    path: 'packages/yawl-langchain',
    tier: 4,
    description: 'YAWL LangChain Integration',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl', 'yawl-ai'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-observability', {
    name: 'yawl-observability',
    path: 'packages/yawl-observability',
    tier: 4,
    description: 'YAWL Observability Extension',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl', 'observability'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-queue', {
    name: 'yawl-queue',
    path: 'packages/yawl-queue',
    tier: 4,
    description: 'YAWL Queue Adapters',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-realtime', {
    name: 'yawl-realtime',
    path: 'packages/yawl-realtime',
    tier: 4,
    description: 'YAWL Realtime Updates',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['yawl'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['yawl-viz', {
    name: 'yawl-viz',
    path: 'packages/yawl-viz',
    tier: 4,
    description: 'YAWL Visualization',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['yawl'],
    dependents: [],
    criticality: 'low',
    policyInfluencing: false
  }],
  ['rdf-graphql', {
    name: 'rdf-graphql',
    path: 'packages/rdf-graphql',
    tier: 4,
    description: 'RDF to GraphQL Bridge',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['react', {
    name: 'react',
    path: 'packages/react',
    tier: 4,
    description: 'React Components and Hooks',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core', 'composables'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],
  ['cli', {
    name: 'cli',
    path: 'packages/cli',
    tier: 4,
    description: 'Command Line Interface',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: [],
    criticality: 'medium',
    policyInfluencing: false
  }],

  // -------------------------------------------------------------------------
  // TIER 5: DOCUMENTATION PACKAGES (6)
  // -------------------------------------------------------------------------
  ['docs', {
    name: 'docs',
    path: 'packages/docs',
    tier: 5,
    description: 'Documentation Site',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: [],
    dependents: [],
    criticality: 'low',
    policyInfluencing: false
  }],
  ['nextra', {
    name: 'nextra',
    path: 'packages/nextra',
    tier: 5,
    description: 'Nextra Documentation Theme',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: [],
    dependents: ['docs'],
    criticality: 'low',
    policyInfluencing: false
  }],
  ['diataxis-kit', {
    name: 'diataxis-kit',
    path: 'packages/diataxis-kit',
    tier: 5,
    description: 'Diataxis Documentation Framework',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: [],
    dependents: ['docs'],
    criticality: 'low',
    policyInfluencing: false
  }],
  ['dark-matter', {
    name: 'dark-matter',
    path: 'packages/dark-matter',
    tier: 5,
    description: 'Experimental Features',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: ['core'],
    dependents: [],
    criticality: 'low',
    policyInfluencing: false
  }],
  ['atomvm', {
    name: 'atomvm',
    path: 'packages/atomvm',
    tier: 5,
    description: 'AtomVM Playground',
    measures: { D_t: true, TC: false, TE: false, C_t: true },
    dependencies: [],
    dependents: [],
    criticality: 'low',
    policyInfluencing: false
  }],
  ['integration-tests', {
    name: 'integration-tests',
    path: 'packages/integration-tests',
    tier: 5,
    description: 'Cross-Package Integration Tests',
    measures: { D_t: true, TC: true, TE: false, C_t: true },
    dependencies: ['test-utils', 'core'],
    dependents: [],
    criticality: 'high',
    policyInfluencing: false
  }]
]);

// ============================================================================
// SECTION 2: DEPENDENCY GRAPH
// ============================================================================

/**
 * Build dependency graph from registry
 * @returns {Map<string, string[]>} Package -> dependencies
 */
export function buildDependencyGraph() {
  const graph = new Map();

  for (const [name, entry] of PACKAGE_REGISTRY) {
    graph.set(name, entry.dependencies);
  }

  return graph;
}

/**
 * Build reverse dependency graph (dependents)
 * @returns {Map<string, string[]>} Package -> dependents
 */
export function buildReverseDependencyGraph() {
  const graph = new Map();

  // Initialize all packages
  for (const name of PACKAGE_REGISTRY.keys()) {
    graph.set(name, []);
  }

  // Build reverse edges
  for (const [name, entry] of PACKAGE_REGISTRY) {
    for (const dep of entry.dependencies) {
      const dependents = graph.get(dep);
      if (dependents && !dependents.includes(name)) {
        dependents.push(name);
      }
    }
  }

  return graph;
}

/**
 * Get topological sort of packages (dependencies first)
 * @returns {string[]} Ordered package names
 */
export function topologicalSort() {
  const graph = buildDependencyGraph();
  const sorted = [];
  const visited = new Set();
  const visiting = new Set();

  /**
   * DFS visit for topological sort
   * @param {string} node - Package name
   */
  function visit(node) {
    if (visited.has(node)) return;
    if (visiting.has(node)) {
      throw new Error(`Circular dependency detected at ${node}`);
    }

    visiting.add(node);

    for (const dep of graph.get(node) ?? []) {
      visit(dep);
    }

    visiting.delete(node);
    visited.add(node);
    sorted.push(node);
  }

  for (const name of PACKAGE_REGISTRY.keys()) {
    visit(name);
  }

  return sorted;
}

// ============================================================================
// SECTION 3: MEASUREMENT CONFIGURATION
// ============================================================================

/**
 * Get packages that measure a specific metric
 *
 * @param {'D_t' | 'TC' | 'TE' | 'C_t'} metric - Metric name
 * @returns {string[]} Package names
 */
export function getPackagesMeasuring(metric) {
  const result = [];

  for (const [name, entry] of PACKAGE_REGISTRY) {
    if (entry.measures[metric]) {
      result.push(name);
    }
  }

  return result;
}

/**
 * Get policy-influencing packages (measure TE)
 * @returns {string[]} Package names
 */
export function getPolicyInfluencingPackages() {
  const result = [];

  for (const [name, entry] of PACKAGE_REGISTRY) {
    if (entry.policyInfluencing) {
      result.push(name);
    }
  }

  return result;
}

/**
 * Get packages by tier
 *
 * @param {number} tier - Tier number (1-5)
 * @returns {string[]} Package names
 */
export function getPackagesByTier(tier) {
  const result = [];

  for (const [name, entry] of PACKAGE_REGISTRY) {
    if (entry.tier === tier) {
      result.push(name);
    }
  }

  return result;
}

/**
 * Get packages by criticality
 *
 * @param {'critical' | 'high' | 'medium' | 'low'} criticality
 * @returns {string[]} Package names
 */
export function getPackagesByCriticality(criticality) {
  const result = [];

  for (const [name, entry] of PACKAGE_REGISTRY) {
    if (entry.criticality === criticality) {
      result.push(name);
    }
  }

  return result;
}

// ============================================================================
// SECTION 4: ROLLUP RULES
// ============================================================================

/**
 * Rollup configuration for aggregating measurements
 * @type {Object}
 */
export const ROLLUP_CONFIG = Object.freeze({
  // Entropy aggregation
  entropy: {
    // Sum weighted by tier
    method: 'weighted_sum',
    weights: TIER_WEIGHTS
  },

  // Capacity aggregation
  capacity: {
    // Average across packages
    method: 'mean'
  },

  // TC aggregation
  temporalCoupling: {
    // Build full matrix, report density
    method: 'matrix',
    threshold: 0.1  // Minimum TC to count
  },

  // TE aggregation
  transitiveEntropy: {
    // Max and average
    method: 'statistics',
    metrics: ['max', 'mean', 'stddev']
  }
});

/**
 * Apply rollup rules to aggregate package measurements
 *
 * @param {import('./schema.mjs').PackageMeasurement[]} measurements
 * @returns {import('./schema.mjs').SystemMeasurement}
 */
export function applyRollupRules(measurements) {
  const n = measurements.length;

  // Entropy rollup
  let D_t = 0;
  let D_t_weighted = 0;

  for (const m of measurements) {
    const weight = TIER_WEIGHTS[m.tier] ?? 1.0;
    D_t += m.D_t;
    D_t_weighted += m.D_t * weight;
  }

  // Capacity rollup
  const C_t_values = measurements.map(m => m.C_t);
  const C_t = C_t_values.reduce((a, b) => a + b, 0) / n;
  const C_t_total = C_t_values.reduce((a, b) => a + b, 0);

  // TC rollup (if present)
  let TC_density = 0;
  let TC_max = 0;

  const packagesWithTC = measurements.filter(m => m.TC && m.TC.length > 0);
  if (packagesWithTC.length > 0) {
    const threshold = ROLLUP_CONFIG.temporalCoupling.threshold;
    let aboveThreshold = 0;
    let total = 0;

    for (const m of packagesWithTC) {
      for (let i = 0; i < (m.TC ?? []).length; i++) {
        const val = m.TC?.[i] ?? 0;
        if (i !== measurements.indexOf(m)) { // Skip diagonal
          total++;
          if (val > threshold) aboveThreshold++;
          if (val > TC_max) TC_max = val;
        }
      }
    }

    TC_density = total > 0 ? aboveThreshold / total : 0;
  }

  // TE rollup
  const teValues = measurements.filter(m => m.TE !== undefined).map(m => m.TE ?? 0);
  const TE_max = teValues.length > 0 ? Math.max(...teValues) : 0;
  const TE_avg = teValues.length > 0 ? teValues.reduce((a, b) => a + b, 0) / teValues.length : 0;

  // Tier distribution
  const tierDistribution = {};
  for (const m of measurements) {
    tierDistribution[m.tier] = (tierDistribution[m.tier] ?? 0) + 1;
  }

  // Health counts
  const packagesOverCapacity = measurements.filter(m => m.C_t < 0).length;
  const packagesNearCapacity = measurements.filter(m => m.C_t >= 0 && m.C_t < 3).length;

  return {
    D_t,
    D_t_weighted,
    C_t,
    C_t_total,
    TC_density,
    TC_max,
    TC_clusters: 0, // Computed separately if needed
    TE_max,
    TE_avg,
    package_count: n,
    tier_distribution: tierDistribution,
    packages_over_capacity: packagesOverCapacity,
    packages_near_capacity: packagesNearCapacity
  };
}

// ============================================================================
// SECTION 5: MEASUREMENT SCHEDULE
// ============================================================================

/**
 * Measurement schedule configuration
 * @type {Object}
 */
export const MEASUREMENT_SCHEDULE = Object.freeze({
  // On every commit
  on_commit: {
    D_t: true,   // Always measure entropy
    TC: false,   // Too expensive per commit
    TE: false,   // Only for policy packages
    C_t: true    // Always check capacity
  },

  // On PR creation
  on_pr: {
    D_t: true,
    TC: true,    // Check coupling with changed packages
    TE: true,    // Check policy impact
    C_t: true
  },

  // Nightly build
  nightly: {
    D_t: true,
    TC: true,    // Full matrix
    TE: true,    // All packages
    C_t: true,
    historical_comparison: true
  },

  // On release
  release: {
    D_t: true,
    TC: true,
    TE: true,
    C_t: true,
    certificate: true,  // Generate signed certificate
    publish: true       // Publish to artifact store
  }
});

// ============================================================================
// SECTION 6: ALERT THRESHOLDS
// ============================================================================

/**
 * Alert thresholds for measurement violations
 * @type {Object}
 */
export const ALERT_THRESHOLDS = Object.freeze({
  // Entropy alerts
  entropy: {
    // Per-package
    package_warning: 0.75,  // 75% of C_max
    package_critical: 0.90, // 90% of C_max
    package_overflow: 1.0,  // Over C_max

    // System-wide
    system_warning: 25,     // Total bits
    system_critical: 35,    // Total bits
  },

  // Coupling alerts
  coupling: {
    high_coupling: 0.7,     // TC > 0.7 triggers alert
    cluster_size: 5         // More than 5 packages in cluster
  },

  // Capacity alerts
  capacity: {
    low_capacity_warning: 3,   // Less than 3 bits remaining
    low_capacity_critical: 1   // Less than 1 bit remaining
  },

  // Change rate alerts
  changeRate: {
    entropy_spike: 2.0,     // D_t increased by >2 bits in one epoch
    velocity_warning: 5     // More than 5 packages changed
  }
});

// ============================================================================
// SECTION 7: UTILITY FUNCTIONS
// ============================================================================

/**
 * Get registry entry for a package
 *
 * @param {string} name - Package name
 * @returns {Object | undefined} Registry entry
 */
export function getPackageEntry(name) {
  return PACKAGE_REGISTRY.get(name);
}

/**
 * Get all package names
 * @returns {string[]} Package names
 */
export function getAllPackageNames() {
  return Array.from(PACKAGE_REGISTRY.keys());
}

/**
 * Get package count
 * @returns {number} Number of packages
 */
export function getPackageCount() {
  return PACKAGE_REGISTRY.size;
}

/**
 * Check if a package exists in registry
 *
 * @param {string} name - Package name
 * @returns {boolean} True if exists
 */
export function hasPackage(name) {
  return PACKAGE_REGISTRY.has(name);
}

/**
 * Get measurement configuration for a package
 *
 * @param {string} name - Package name
 * @returns {Object} Measurement configuration
 */
export function getMeasurementConfig(name) {
  const entry = PACKAGE_REGISTRY.get(name);
  if (!entry) {
    return { D_t: true, TC: false, TE: false, C_t: true };
  }

  return {
    ...entry.measures,
    tier: entry.tier,
    template: getExtractionTemplate(entry.tier),
    C_max: TIER_CAPACITY[entry.tier]
  };
}

// ============================================================================
// SECTION 8: EXPORTS
// ============================================================================

export default {
  // Registry
  PACKAGE_REGISTRY,

  // Graph functions
  buildDependencyGraph,
  buildReverseDependencyGraph,
  topologicalSort,

  // Query functions
  getPackagesMeasuring,
  getPolicyInfluencingPackages,
  getPackagesByTier,
  getPackagesByCriticality,

  // Rollup
  ROLLUP_CONFIG,
  applyRollupRules,

  // Schedule
  MEASUREMENT_SCHEDULE,

  // Alerts
  ALERT_THRESHOLDS,

  // Utilities
  getPackageEntry,
  getAllPackageNames,
  getPackageCount,
  hasPackage,
  getMeasurementConfig
};
