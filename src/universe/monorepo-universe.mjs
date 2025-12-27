/**
 * @file monorepo-universe.mjs
 * @description Universe orchestrator for the entire UNRDF monorepo
 *
 * Manages 43 packages organized into 10 partition groups (O_t):
 * - O_foundational: core, oxigraph, caching (immutable, version-locked)
 * - O_knowledge: hooks, knowledge-engine, kgc-4d (stable API, additive extensions)
 * - O_workflow: yawl and 9 YAWL-variant packages (workflow orchestration)
 * - O_ml: ml-inference, ml-versioning, ml-langchain (emerging, high churn)
 * - O_infrastructure: serverless, kafka, realtime, collab (deployment-specific)
 * - O_observability: observability, yawl-observability (tracing/metrics)
 * - O_documentation: docs, nextra, diataxis-kit (documentation projections)
 * - O_integration: integration-tests, test-utils, validation (testing/validation)
 * - O_emerging: dark-matter, atomvm (experimental, bounded)
 * - O_ledger: execution ledger (all receipts from all packages)
 *
 * The universe is the source of truth for the entire monorepo.
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';
import * as fs from 'node:fs/promises';
import * as path from 'node:path';

import {
  PKG_NS,
  DEP_NS,
  MOD_NS,
  pkg,
  dep,
  rdf,
  xsd,
  PackageClasses,
  PackageProperties,
  packageToIri,
  parsePackageToQuads,
  addPartitionSchema,
  validateWorkspaceDependencies,
  PackageJsonSchema,
} from './package-rdf-model.mjs';

const { namedNode, literal, quad, defaultGraph } = dataFactory;

// ============================================================================
// Partition Definitions
// ============================================================================

/**
 * Partition definitions with ordering and properties
 * @type {Array<import('./package-rdf-model.mjs').PartitionDefinition>}
 */
export const PARTITION_DEFINITIONS = [
  {
    name: 'O_foundational',
    order: 0,
    readOnly: true,
    description: 'Foundational packages (core, oxigraph, caching) - immutable, version-locked',
    packages: ['@unrdf/core', '@unrdf/oxigraph', '@unrdf/caching'],
  },
  {
    name: 'O_knowledge',
    order: 1,
    readOnly: false,
    description: 'Knowledge packages (hooks, knowledge-engine, kgc-4d) - stable API, additive extensions',
    packages: ['@unrdf/hooks', '@unrdf/knowledge-engine', '@unrdf/kgc-4d', '@unrdf/kgn'],
  },
  {
    name: 'O_workflow',
    order: 2,
    readOnly: false,
    description: 'YAWL workflow packages - workflow orchestration',
    packages: [
      '@unrdf/yawl',
      '@unrdf/yawl-ai',
      '@unrdf/yawl-api',
      '@unrdf/yawl-durable',
      '@unrdf/yawl-kafka',
      '@unrdf/yawl-langchain',
      '@unrdf/yawl-queue',
      '@unrdf/yawl-realtime',
      '@unrdf/yawl-viz',
    ],
  },
  {
    name: 'O_ml',
    order: 3,
    readOnly: false,
    description: 'Machine learning packages - emerging, high churn',
    packages: ['@unrdf/ml-inference', '@unrdf/ml-versioning'],
  },
  {
    name: 'O_infrastructure',
    order: 4,
    readOnly: false,
    description: 'Infrastructure packages - deployment-specific',
    packages: [
      '@unrdf/serverless',
      '@unrdf/collab',
      '@unrdf/streaming',
      '@unrdf/federation',
      '@unrdf/consensus',
      '@unrdf/blockchain',
      '@unrdf/engine-gateway',
    ],
  },
  {
    name: 'O_observability',
    order: 5,
    readOnly: false,
    description: 'Observability packages - tracing and metrics',
    packages: ['@unrdf/observability', '@unrdf/yawl-observability'],
  },
  {
    name: 'O_documentation',
    order: 6,
    readOnly: false,
    description: 'Documentation packages - documentation projections',
    packages: ['docs', '@unrdf/nextra-docs', '@unrdf/diataxis-kit'],
  },
  {
    name: 'O_integration',
    order: 7,
    readOnly: false,
    description: 'Integration and testing packages',
    packages: ['@unrdf/integration-tests', '@unrdf/test-utils', '@unrdf/validation'],
  },
  {
    name: 'O_emerging',
    order: 8,
    readOnly: false,
    description: 'Experimental packages - bounded exploration',
    packages: ['@unrdf/dark-matter', '@unrdf/atomvm'],
  },
  {
    name: 'O_platform',
    order: 9,
    readOnly: false,
    description: 'Platform packages - UI, CLI, and developer tooling',
    packages: [
      '@unrdf/cli',
      '@unrdf/composables',
      '@unrdf/domain',
      '@unrdf/graph-analytics',
      '@unrdf/project-engine',
      '@unrdf/rdf-graphql',
      '@unrdf/semantic-search',
    ],
  },
];

/**
 * Build a map from package name to partition name
 * @returns {Map<string, string>}
 */
export function buildPartitionMap() {
  const map = new Map();
  for (const partition of PARTITION_DEFINITIONS) {
    for (const pkgName of partition.packages) {
      map.set(pkgName, partition.name);
    }
  }
  return map;
}

/**
 * Get partition definition by name
 * @param {string} name - Partition name
 * @returns {import('./package-rdf-model.mjs').PartitionDefinition | undefined}
 */
export function getPartitionByName(name) {
  return PARTITION_DEFINITIONS.find((p) => p.name === name);
}

/**
 * Get partition for a package
 * @param {string} packageName - Package name
 * @returns {string | undefined} Partition name
 */
export function getPackagePartition(packageName) {
  return buildPartitionMap().get(packageName);
}

// ============================================================================
// Dependency Graph Rules
// ============================================================================

/**
 * Allowed dependency directions between partitions
 * Key = source partition, Value = allowed target partitions
 * @type {Map<string, Set<string>>}
 */
export const DEPENDENCY_RULES = new Map([
  // O_foundational can only depend on itself (and externals)
  ['O_foundational', new Set(['O_foundational'])],

  // O_knowledge can depend on foundational
  ['O_knowledge', new Set(['O_foundational', 'O_knowledge'])],

  // O_workflow can depend on foundational and knowledge
  ['O_workflow', new Set(['O_foundational', 'O_knowledge', 'O_workflow'])],

  // O_ml can depend on foundational, knowledge
  ['O_ml', new Set(['O_foundational', 'O_knowledge', 'O_ml'])],

  // O_infrastructure can depend on foundational, knowledge, workflow
  ['O_infrastructure', new Set(['O_foundational', 'O_knowledge', 'O_workflow', 'O_infrastructure'])],

  // O_observability can depend on foundational, knowledge, workflow
  ['O_observability', new Set(['O_foundational', 'O_knowledge', 'O_workflow', 'O_observability'])],

  // O_documentation can depend on anything except emerging/ledger
  ['O_documentation', new Set([
    'O_foundational', 'O_knowledge', 'O_workflow', 'O_ml',
    'O_infrastructure', 'O_observability', 'O_documentation',
  ])],

  // O_integration can depend on anything (it tests everything)
  ['O_integration', new Set([
    'O_foundational', 'O_knowledge', 'O_workflow', 'O_ml',
    'O_infrastructure', 'O_observability', 'O_documentation',
    'O_platform', 'O_integration',
  ])],

  // O_emerging can depend on foundational, knowledge
  ['O_emerging', new Set(['O_foundational', 'O_knowledge', 'O_emerging'])],

  // O_platform can depend on foundational, knowledge, workflow
  ['O_platform', new Set([
    'O_foundational', 'O_knowledge', 'O_workflow',
    'O_infrastructure', 'O_platform',
  ])],
]);

/**
 * Check if a dependency between partitions is allowed
 * @param {string} sourcePartition - Source partition name
 * @param {string} targetPartition - Target partition name
 * @returns {boolean}
 */
export function isDependencyAllowed(sourcePartition, targetPartition) {
  const allowed = DEPENDENCY_RULES.get(sourcePartition);
  if (!allowed) return false;
  return allowed.has(targetPartition);
}

// ============================================================================
// MonorepoUniverse Class
// ============================================================================

/**
 * MonorepoUniverse - manages all packages and their RDF representation
 *
 * @class MonorepoUniverse
 *
 * @example
 * ```javascript
 * const universe = new MonorepoUniverse('/path/to/unrdf');
 * await universe.load();
 *
 * // Query packages
 * const results = universe.query(`
 *   SELECT ?pkg ?name WHERE {
 *     ?pkg a pkg:Package .
 *     ?pkg pkg:name ?name .
 *   }
 * `);
 *
 * // Get dependency graph
 * const deps = universe.getDependencyGraph();
 * ```
 */
export class MonorepoUniverse {
  /**
   * Create a new MonorepoUniverse
   *
   * @param {string} rootDir - Root directory of the monorepo
   * @param {Object} [options] - Configuration options
   * @param {boolean} [options.loadTests=false] - Whether to load test file metadata
   * @param {boolean} [options.loadDocs=false] - Whether to load documentation metadata
   */
  constructor(rootDir, options = {}) {
    /** @type {string} */
    this.rootDir = rootDir;

    /** @type {Object} */
    this.options = {
      loadTests: options.loadTests ?? false,
      loadDocs: options.loadDocs ?? false,
    };

    /** @type {import('@unrdf/oxigraph').OxigraphStore} */
    this.store = createStore();

    /** @type {Map<string, Object>} Package name -> package.json */
    this._packages = new Map();

    /** @type {Map<string, string>} Package name -> partition name */
    this._partitionMap = buildPartitionMap();

    /** @type {boolean} */
    this._loaded = false;

    /** @type {string} */
    this._packagesDir = path.join(rootDir, 'packages');
  }

  /**
   * Load all packages into the universe
   *
   * @returns {Promise<void>}
   */
  async load() {
    if (this._loaded) return;

    // Add partition schema
    addPartitionSchema(this.store, PARTITION_DEFINITIONS);

    // Discover and load all packages
    const packageDirs = await this._discoverPackages();

    for (const packageDir of packageDirs) {
      await this._loadPackage(packageDir);
    }

    // Add dependency relationship quads
    this._addDependencyRelationships();

    this._loaded = true;
  }

  /**
   * Discover all package directories
   *
   * @returns {Promise<Array<string>>} Array of package directory paths
   * @private
   */
  async _discoverPackages() {
    const entries = await fs.readdir(this._packagesDir, { withFileTypes: true });
    return entries
      .filter((e) => e.isDirectory())
      .map((e) => path.join(this._packagesDir, e.name));
  }

  /**
   * Load a single package
   *
   * @param {string} packageDir - Package directory path
   * @returns {Promise<void>}
   * @private
   */
  async _loadPackage(packageDir) {
    const packageJsonPath = path.join(packageDir, 'package.json');

    try {
      const content = await fs.readFile(packageJsonPath, 'utf-8');
      const packageJson = JSON.parse(content);

      // Validate and parse
      const result = PackageJsonSchema.safeParse(packageJson);
      if (!result.success) {
        console.warn(`Invalid package.json in ${packageDir}:`, result.error.message);
        return;
      }

      const validated = result.data;
      this._packages.set(validated.name, validated);

      // Get partition
      const partition = this._partitionMap.get(validated.name);

      // Convert to RDF quads
      const quads = parsePackageToQuads(validated, {
        packagePath: packageDir,
        partition,
      });

      // Add to store
      for (const q of quads) {
        this.store.add(q);
      }

      // Load test metadata if enabled
      if (this.options.loadTests) {
        await this._loadTestMetadata(packageDir, validated.name);
      }

      // Load documentation metadata if enabled
      if (this.options.loadDocs) {
        await this._loadDocMetadata(packageDir, validated.name);
      }
    } catch (error) {
      if (error.code !== 'ENOENT') {
        console.warn(`Error loading ${packageDir}:`, error.message);
      }
    }
  }

  /**
   * Load test file metadata for a package
   *
   * @param {string} packageDir - Package directory
   * @param {string} packageName - Package name
   * @returns {Promise<void>}
   * @private
   */
  async _loadTestMetadata(packageDir, packageName) {
    const testDir = path.join(packageDir, 'test');
    const graph = defaultGraph();
    const pkgIri = packageToIri(packageName);

    try {
      const testFiles = await this._findFiles(testDir, /\.test\.(m?js|ts)$/);

      for (const testFile of testFiles) {
        const relativePath = path.relative(packageDir, testFile);
        const testIri = namedNode(`${PKG_NS}test/${packageName.replace('@', '').replace('/', '-')}/${relativePath}`);

        this.store.add(quad(testIri, rdf('type'), PackageClasses.Test, graph));
        this.store.add(quad(pkgIri, PackageProperties.testFile, testIri, graph));
        this.store.add(quad(testIri, PackageProperties.name, literal(relativePath), graph));
      }
    } catch {
      // Test directory may not exist
    }
  }

  /**
   * Load documentation metadata for a package
   *
   * @param {string} packageDir - Package directory
   * @param {string} packageName - Package name
   * @returns {Promise<void>}
   * @private
   */
  async _loadDocMetadata(packageDir, packageName) {
    const graph = defaultGraph();
    const pkgIri = packageToIri(packageName);

    // Check for README
    const readmePath = path.join(packageDir, 'README.md');
    try {
      await fs.access(readmePath);
      const docIri = namedNode(`${PKG_NS}doc/${packageName.replace('@', '').replace('/', '-')}/README.md`);
      this.store.add(quad(docIri, rdf('type'), PackageClasses.Documentation, graph));
      this.store.add(quad(pkgIri, PackageProperties.hasDocumentation, docIri, graph));
      this.store.add(quad(docIri, PackageProperties.documentationPath, literal('README.md'), graph));
    } catch {
      // README may not exist
    }
  }

  /**
   * Find files matching a pattern recursively
   *
   * @param {string} dir - Directory to search
   * @param {RegExp} pattern - File pattern to match
   * @returns {Promise<Array<string>>}
   * @private
   */
  async _findFiles(dir, pattern) {
    const results = [];

    const entries = await fs.readdir(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        const subResults = await this._findFiles(fullPath, pattern);
        results.push(...subResults);
      } else if (pattern.test(entry.name)) {
        results.push(fullPath);
      }
    }

    return results;
  }

  /**
   * Add additional dependency relationship quads
   * @private
   */
  _addDependencyRelationships() {
    const graph = defaultGraph();

    for (const [pkgName, pkgJson] of this._packages.entries()) {
      const srcPartition = this._partitionMap.get(pkgName);
      const pkgIri = packageToIri(pkgName);

      for (const [depName, depVersion] of Object.entries(pkgJson.dependencies || {})) {
        const isWorkspace = depVersion === 'workspace:*' || depVersion.startsWith('workspace:');
        if (!isWorkspace) continue;

        const targetPartition = this._partitionMap.get(depName);

        // Check partition dependency rules
        if (srcPartition && targetPartition) {
          const allowed = isDependencyAllowed(srcPartition, targetPartition);
          const ruleIri = namedNode(`${DEP_NS}rule/${pkgName.replace('@', '').replace('/', '-')}/${depName.replace('@', '').replace('/', '-')}`);

          this.store.add(quad(
            ruleIri,
            dep('allowed'),
            literal(String(allowed), xsd('boolean')),
            graph
          ));
          this.store.add(quad(ruleIri, dep('sourcePartition'), literal(srcPartition), graph));
          this.store.add(quad(ruleIri, dep('targetPartition'), literal(targetPartition), graph));
        }
      }
    }
  }

  // ============================================================================
  // Query Methods
  // ============================================================================

  /**
   * Execute a SPARQL query on the universe
   *
   * @param {string} sparqlQuery - SPARQL query string
   * @returns {*} Query results
   *
   * @example
   * ```javascript
   * const results = universe.query(`
   *   PREFIX pkg: <http://unrdf.org/package#>
   *   SELECT ?name WHERE {
   *     ?pkg a pkg:Package .
   *     ?pkg pkg:name ?name .
   *   }
   * `);
   * ```
   */
  query(sparqlQuery) {
    if (!this._loaded) {
      throw new Error('Universe not loaded. Call load() first.');
    }
    return this.store.query(sparqlQuery);
  }

  /**
   * Match quads in the universe
   *
   * @param {*} subject - Subject pattern
   * @param {*} predicate - Predicate pattern
   * @param {*} object - Object pattern
   * @param {*} graph - Graph pattern
   * @returns {Array<import('oxigraph').Quad>}
   */
  match(subject, predicate, object, graph) {
    if (!this._loaded) {
      throw new Error('Universe not loaded. Call load() first.');
    }
    return this.store.match(subject, predicate, object, graph);
  }

  // ============================================================================
  // Convenience Methods
  // ============================================================================

  /**
   * Get all packages in the universe
   *
   * @returns {Array<Object>} Array of package.json objects
   */
  getAllPackages() {
    return Array.from(this._packages.values());
  }

  /**
   * Get a package by name
   *
   * @param {string} name - Package name
   * @returns {Object|undefined} Package.json object
   */
  getPackage(name) {
    return this._packages.get(name);
  }

  /**
   * Get all packages in a partition
   *
   * @param {string} partitionName - Partition name
   * @returns {Array<Object>} Array of package.json objects
   */
  getPackagesInPartition(partitionName) {
    const partition = getPartitionByName(partitionName);
    if (!partition) return [];

    return partition.packages
      .map((name) => this._packages.get(name))
      .filter(Boolean);
  }

  /**
   * Get package count
   *
   * @returns {number}
   */
  get packageCount() {
    return this._packages.size;
  }

  /**
   * Get total quad count
   *
   * @returns {number}
   */
  get quadCount() {
    return this.store.size;
  }

  /**
   * Get dependency graph as adjacency list
   *
   * @returns {Map<string, Set<string>>} Package name -> set of dependency names
   */
  getDependencyGraph() {
    const graph = new Map();

    for (const [pkgName, pkgJson] of this._packages.entries()) {
      const deps = new Set();

      for (const [depName, depVersion] of Object.entries(pkgJson.dependencies || {})) {
        if (depVersion === 'workspace:*' || depVersion.startsWith('workspace:')) {
          deps.add(depName);
        }
      }

      graph.set(pkgName, deps);
    }

    return graph;
  }

  /**
   * Get transitive dependencies for a package
   *
   * @param {string} packageName - Package name
   * @returns {Set<string>} Set of all transitive dependency names
   */
  getTransitiveDependencies(packageName) {
    const graph = this.getDependencyGraph();
    const visited = new Set();
    const queue = [packageName];

    while (queue.length > 0) {
      const current = queue.shift();
      if (visited.has(current)) continue;
      visited.add(current);

      const deps = graph.get(current) || new Set();
      for (const dep of deps) {
        if (!visited.has(dep)) {
          queue.push(dep);
        }
      }
    }

    // Remove the starting package
    visited.delete(packageName);
    return visited;
  }

  /**
   * Detect circular dependencies
   *
   * @returns {Array<Array<string>>} Array of circular dependency paths
   */
  detectCircularDependencies() {
    const graph = this.getDependencyGraph();
    const cycles = [];

    for (const startNode of graph.keys()) {
      const visited = new Set();
      const path = [];

      const dfs = (node) => {
        if (path.includes(node)) {
          // Found a cycle
          const cycleStart = path.indexOf(node);
          cycles.push([...path.slice(cycleStart), node]);
          return;
        }

        if (visited.has(node)) return;
        visited.add(node);
        path.push(node);

        const deps = graph.get(node) || new Set();
        for (const dep of deps) {
          dfs(dep);
        }

        path.pop();
      };

      dfs(startNode);
    }

    // Deduplicate cycles (same cycle can be detected from different starting points)
    const uniqueCycles = [];
    const seen = new Set();

    for (const cycle of cycles) {
      const normalized = [...cycle].sort().join(',');
      if (!seen.has(normalized)) {
        seen.add(normalized);
        uniqueCycles.push(cycle);
      }
    }

    return uniqueCycles;
  }

  /**
   * Get packages that depend on a given package
   *
   * @param {string} packageName - Package name
   * @returns {Array<string>} Array of package names that depend on this package
   */
  getDependents(packageName) {
    const dependents = [];

    for (const [pkgName, pkgJson] of this._packages.entries()) {
      const deps = pkgJson.dependencies || {};
      if (packageName in deps) {
        dependents.push(pkgName);
      }
    }

    return dependents;
  }

  /**
   * Validate partition dependency rules
   *
   * @returns {{ valid: boolean, violations: Array<{from: string, to: string, fromPartition: string, toPartition: string}> }}
   */
  validatePartitionDependencies() {
    const violations = [];

    for (const [pkgName, pkgJson] of this._packages.entries()) {
      const srcPartition = this._partitionMap.get(pkgName);
      if (!srcPartition) continue;

      for (const [depName, depVersion] of Object.entries(pkgJson.dependencies || {})) {
        if (depVersion !== 'workspace:*' && !depVersion.startsWith('workspace:')) continue;

        const targetPartition = this._partitionMap.get(depName);
        if (!targetPartition) continue;

        if (!isDependencyAllowed(srcPartition, targetPartition)) {
          violations.push({
            from: pkgName,
            to: depName,
            fromPartition: srcPartition,
            toPartition: targetPartition,
          });
        }
      }
    }

    return {
      valid: violations.length === 0,
      violations,
    };
  }

  /**
   * Get partition statistics
   *
   * @returns {Object} Partition statistics
   */
  getPartitionStats() {
    const stats = {};

    for (const partition of PARTITION_DEFINITIONS) {
      const packages = this.getPackagesInPartition(partition.name);
      const totalDeps = packages.reduce((sum, pkg) => {
        return sum + Object.keys(pkg.dependencies || {}).length;
      }, 0);

      stats[partition.name] = {
        name: partition.name,
        order: partition.order,
        readOnly: partition.readOnly,
        packageCount: packages.length,
        packages: packages.map((p) => p.name),
        totalDependencies: totalDeps,
      };
    }

    return stats;
  }

  // ============================================================================
  // Projection Methods (mu projections)
  // ============================================================================

  /**
   * Project the universe to a dependency visualization format
   *
   * @returns {Object} D3-compatible graph format
   */
  projectToDependencyGraph() {
    const nodes = [];
    const links = [];
    const nodeMap = new Map();

    // Create nodes
    let idx = 0;
    for (const [pkgName] of this._packages.entries()) {
      const partition = this._partitionMap.get(pkgName);
      nodeMap.set(pkgName, idx);
      nodes.push({
        id: pkgName,
        name: pkgName.replace('@unrdf/', ''),
        partition: partition || 'unknown',
        index: idx,
      });
      idx++;
    }

    // Create links
    const graph = this.getDependencyGraph();
    for (const [source, deps] of graph.entries()) {
      for (const target of deps) {
        if (nodeMap.has(source) && nodeMap.has(target)) {
          links.push({
            source: nodeMap.get(source),
            target: nodeMap.get(target),
          });
        }
      }
    }

    return { nodes, links };
  }

  /**
   * Project to coverage matrix format
   *
   * @returns {Array<Object>} Coverage matrix rows
   */
  projectToCoverageMatrix() {
    const matrix = [];

    for (const [pkgName, pkgJson] of this._packages.entries()) {
      const hasTests = pkgJson.scripts?.test !== undefined;
      const hasCoverage = pkgJson.scripts?.['test:coverage'] !== undefined;
      const partition = this._partitionMap.get(pkgName);

      matrix.push({
        package: pkgName,
        partition,
        hasTests,
        hasCoverage,
        testCommand: pkgJson.scripts?.test || null,
      });
    }

    return matrix;
  }

  /**
   * Project to API surface matrix
   *
   * @returns {Array<Object>} API surface rows
   */
  projectToApiSurface() {
    const matrix = [];

    for (const [pkgName, pkgJson] of this._packages.entries()) {
      const exports = pkgJson.exports || {};
      const exportPaths = typeof exports === 'string'
        ? ['.']
        : Object.keys(exports);

      matrix.push({
        package: pkgName,
        main: pkgJson.main || null,
        type: pkgJson.type || 'commonjs',
        sideEffects: pkgJson.sideEffects ?? true,
        exportCount: exportPaths.length,
        exports: exportPaths,
      });
    }

    return matrix;
  }

  /**
   * Serialize universe to JSON
   *
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      rootDir: this.rootDir,
      packageCount: this.packageCount,
      quadCount: this.quadCount,
      partitions: PARTITION_DEFINITIONS.map((p) => ({
        name: p.name,
        order: p.order,
        readOnly: p.readOnly,
        packageCount: p.packages.length,
      })),
      packages: this.getAllPackages().map((p) => ({
        name: p.name,
        version: p.version,
        partition: this._partitionMap.get(p.name),
      })),
    };
  }

  /**
   * Get summary statistics
   *
   * @returns {Object} Summary statistics
   */
  getSummary() {
    const depGraph = this.getDependencyGraph();
    let totalDeps = 0;
    let workspaceDeps = 0;

    for (const deps of depGraph.values()) {
      totalDeps += deps.size;
    }

    for (const [, pkgJson] of this._packages.entries()) {
      for (const [, version] of Object.entries(pkgJson.dependencies || {})) {
        if (version === 'workspace:*' || version.startsWith('workspace:')) {
          workspaceDeps++;
        }
      }
    }

    return {
      packages: this.packageCount,
      quads: this.quadCount,
      partitions: PARTITION_DEFINITIONS.length,
      workspaceDependencies: workspaceDeps,
      totalDependencies: totalDeps,
      circularDependencies: this.detectCircularDependencies().length,
    };
  }
}

// ============================================================================
// Factory Functions
// ============================================================================

/**
 * Create and load a MonorepoUniverse
 *
 * @param {string} rootDir - Root directory of the monorepo
 * @param {Object} [options] - Options
 * @returns {Promise<MonorepoUniverse>}
 *
 * @example
 * ```javascript
 * const universe = await createMonorepoUniverse('/path/to/unrdf');
 * console.log(`Loaded ${universe.packageCount} packages`);
 * ```
 */
export async function createMonorepoUniverse(rootDir, options = {}) {
  const universe = new MonorepoUniverse(rootDir, options);
  await universe.load();
  return universe;
}

/**
 * Load universe from current working directory
 *
 * @param {Object} [options] - Options
 * @returns {Promise<MonorepoUniverse>}
 */
export async function loadCurrentUniverse(options = {}) {
  const cwd = process.cwd();
  return createMonorepoUniverse(cwd, options);
}

// ============================================================================
// SPARQL Prefixes
// ============================================================================

/**
 * Common SPARQL prefixes for queries
 */
export const SPARQL_PREFIXES = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX pkg: <${PKG_NS}>
PREFIX dep: <${DEP_NS}>
PREFIX mod: <${MOD_NS}>
`;

export default {
  // Classes
  MonorepoUniverse,

  // Partition definitions
  PARTITION_DEFINITIONS,
  DEPENDENCY_RULES,

  // Partition utilities
  buildPartitionMap,
  getPartitionByName,
  getPackagePartition,
  isDependencyAllowed,

  // Factory functions
  createMonorepoUniverse,
  loadCurrentUniverse,

  // Constants
  SPARQL_PREFIXES,
};
