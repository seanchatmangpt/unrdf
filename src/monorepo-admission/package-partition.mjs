/**
 * @fileoverview Package Partition - Models each monorepo package as an admissible partition
 *
 * In the Governed Ontology Substrate (GOS) extension for monorepo governance,
 * each package is treated as a partition within the unified universe O.
 *
 * Partition Categories:
 * - O_core: Core RDF/SPARQL/SHACL functionality (@unrdf/core, @unrdf/oxigraph)
 * - O_hooks: Knowledge hooks and event sourcing (@unrdf/hooks)
 * - O_yawl: Workflow orchestration (9 YAWL packages)
 * - O_ml: Machine learning layers (@unrdf/ml-inference, @unrdf/ml-versioning, @unrdf/yawl-langchain)
 * - O_infrastructure: Deployment packages (@unrdf/serverless, @unrdf/streaming, etc.)
 * - O_observability: Monitoring and tracing (@unrdf/observability)
 * - O_documentation: Nextra/Diataxis documentation (@unrdf/docs, @unrdf/nextra)
 * - O_testing: Integration tests and validation (@unrdf/integration-tests, @unrdf/test-utils)
 *
 * @module monorepo-admission/package-partition
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';

/**
 * Semantic versioning schema
 */
export const SemVerSchema = z.string().regex(
  /^\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?(\+[a-zA-Z0-9.-]+)?$/,
  'Invalid semantic version format'
);

/**
 * Package dependency schema
 */
export const DependencySchema = z.object({
  name: z.string().min(1),
  version: z.string(),
  isWorkspace: z.boolean().default(false),
  isDevDependency: z.boolean().default(false),
  isPeerDependency: z.boolean().default(false)
});

/**
 * Export entry schema
 */
export const ExportSchema = z.object({
  path: z.string(),
  entryPoint: z.string(),
  isPublic: z.boolean().default(true)
});

/**
 * Partition category enum
 */
export const PartitionCategory = {
  CORE: 'O_core',
  HOOKS: 'O_hooks',
  YAWL: 'O_yawl',
  ML: 'O_ml',
  INFRASTRUCTURE: 'O_infrastructure',
  OBSERVABILITY: 'O_observability',
  DOCUMENTATION: 'O_documentation',
  TESTING: 'O_testing',
  UTILITY: 'O_utility'
};

/**
 * Package partition configuration schema
 */
export const PackagePartitionConfigSchema = z.object({
  name: z.string().min(1),
  version: SemVerSchema,
  category: z.enum([
    'O_core', 'O_hooks', 'O_yawl', 'O_ml',
    'O_infrastructure', 'O_observability',
    'O_documentation', 'O_testing', 'O_utility'
  ]),
  description: z.string().optional(),
  dependencies: z.array(DependencySchema).default([]),
  exports: z.array(ExportSchema).default([]),
  isProtected: z.boolean().default(false),
  isDeprecated: z.boolean().default(false),
  license: z.string().default('MIT'),
  testCoverage: z.number().min(0).max(100).optional(),
  docCoverage: z.number().min(0).max(100).optional(),
  maintainers: z.array(z.string()).default([]),
  metadata: z.record(z.any()).optional()
});

/**
 * Package to category mapping
 */
export const PACKAGE_CATEGORY_MAP = {
  // O_core - Core RDF functionality
  '@unrdf/core': PartitionCategory.CORE,
  '@unrdf/oxigraph': PartitionCategory.CORE,
  '@unrdf/validation': PartitionCategory.CORE,
  '@unrdf/domain': PartitionCategory.CORE,

  // O_hooks - Knowledge hooks
  '@unrdf/hooks': PartitionCategory.HOOKS,

  // O_yawl - Workflow orchestration (9 packages)
  '@unrdf/yawl': PartitionCategory.YAWL,
  '@unrdf/yawl-ai': PartitionCategory.YAWL,
  '@unrdf/yawl-api': PartitionCategory.YAWL,
  '@unrdf/yawl-durable': PartitionCategory.YAWL,
  '@unrdf/yawl-kafka': PartitionCategory.YAWL,
  '@unrdf/yawl-langchain': PartitionCategory.YAWL,
  '@unrdf/yawl-observability': PartitionCategory.YAWL,
  '@unrdf/yawl-queue': PartitionCategory.YAWL,
  '@unrdf/yawl-realtime': PartitionCategory.YAWL,
  '@unrdf/yawl-viz': PartitionCategory.YAWL,

  // O_ml - Machine learning
  '@unrdf/ml-inference': PartitionCategory.ML,
  '@unrdf/ml-versioning': PartitionCategory.ML,

  // O_infrastructure - Deployment
  '@unrdf/serverless': PartitionCategory.INFRASTRUCTURE,
  '@unrdf/streaming': PartitionCategory.INFRASTRUCTURE,
  '@unrdf/caching': PartitionCategory.INFRASTRUCTURE,
  '@unrdf/consensus': PartitionCategory.INFRASTRUCTURE,
  '@unrdf/federation': PartitionCategory.INFRASTRUCTURE,
  '@unrdf/blockchain': PartitionCategory.INFRASTRUCTURE,
  '@unrdf/collab': PartitionCategory.INFRASTRUCTURE,
  '@unrdf/engine-gateway': PartitionCategory.INFRASTRUCTURE,

  // O_observability - Monitoring
  '@unrdf/observability': PartitionCategory.OBSERVABILITY,
  '@unrdf/kgc-4d': PartitionCategory.OBSERVABILITY,
  '@unrdf/kgn': PartitionCategory.OBSERVABILITY,

  // O_documentation - Docs
  '@unrdf/docs': PartitionCategory.DOCUMENTATION,
  '@unrdf/nextra': PartitionCategory.DOCUMENTATION,
  '@unrdf/diataxis-kit': PartitionCategory.DOCUMENTATION,

  // O_testing - Tests
  '@unrdf/integration-tests': PartitionCategory.TESTING,
  '@unrdf/test-utils': PartitionCategory.TESTING,

  // O_utility - Misc utilities
  '@unrdf/cli': PartitionCategory.UTILITY,
  '@unrdf/composables': PartitionCategory.UTILITY,
  '@unrdf/graph-analytics': PartitionCategory.UTILITY,
  '@unrdf/knowledge-engine': PartitionCategory.UTILITY,
  '@unrdf/project-engine': PartitionCategory.UTILITY,
  '@unrdf/rdf-graphql': PartitionCategory.UTILITY,
  '@unrdf/react': PartitionCategory.UTILITY,
  '@unrdf/semantic-search': PartitionCategory.UTILITY,
  '@unrdf/atomvm': PartitionCategory.UTILITY,
  '@unrdf/dark-matter': PartitionCategory.UTILITY
};

/**
 * Protected packages that require special approval for changes
 */
export const PROTECTED_PACKAGES = new Set([
  '@unrdf/core',
  '@unrdf/oxigraph',
  '@unrdf/hooks',
  '@unrdf/validation'
]);

/**
 * Compatible licenses for MIT base license
 */
export const COMPATIBLE_LICENSES = new Set([
  'MIT',
  'ISC',
  'BSD-2-Clause',
  'BSD-3-Clause',
  'Apache-2.0',
  '0BSD',
  'Unlicense',
  'CC0-1.0'
]);

/**
 * PackagePartition - Represents a package as an admissible partition
 *
 * @class PackagePartition
 * @example
 * const partition = new PackagePartition({
 *   name: '@unrdf/core',
 *   version: '5.0.1',
 *   category: 'O_core',
 *   dependencies: [{ name: '@unrdf/oxigraph', version: '5.0.1', isWorkspace: true }]
 * });
 */
export class PackagePartition {
  /**
   * Create a new package partition
   * @param {Object} config - Partition configuration
   */
  constructor(config) {
    const validated = PackagePartitionConfigSchema.parse(config);
    Object.assign(this, validated);

    // Compute content hash
    this._hash = this._computeHash();
  }

  /**
   * Get the partition category
   * @returns {string} Category identifier
   */
  getCategory() {
    return this.category;
  }

  /**
   * Check if this is a protected partition
   * @returns {boolean} True if protected
   */
  isProtectedPartition() {
    return this.isProtected || PROTECTED_PACKAGES.has(this.name);
  }

  /**
   * Get all workspace dependencies (internal monorepo deps)
   * @returns {Array<Object>} Workspace dependencies
   */
  getWorkspaceDependencies() {
    return this.dependencies.filter(dep => dep.isWorkspace);
  }

  /**
   * Get all external dependencies
   * @returns {Array<Object>} External dependencies
   */
  getExternalDependencies() {
    return this.dependencies.filter(dep => !dep.isWorkspace);
  }

  /**
   * Check if package depends on another
   * @param {string} packageName - Package name to check
   * @returns {boolean} True if depends on package
   */
  dependsOn(packageName) {
    return this.dependencies.some(dep => dep.name === packageName);
  }

  /**
   * Get public exports
   * @returns {Array<Object>} Public exports
   */
  getPublicExports() {
    return this.exports.filter(exp => exp.isPublic);
  }

  /**
   * Get private exports (internal API)
   * @returns {Array<Object>} Private exports
   */
  getPrivateExports() {
    return this.exports.filter(exp => !exp.isPublic);
  }

  /**
   * Parse semantic version
   * @returns {Object} Parsed version {major, minor, patch, prerelease, build}
   */
  parseVersion() {
    const match = this.version.match(
      /^(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9.-]+))?(?:\+([a-zA-Z0-9.-]+))?$/
    );

    if (!match) {
      throw new Error(`Invalid version: ${this.version}`);
    }

    return {
      major: parseInt(match[1], 10),
      minor: parseInt(match[2], 10),
      patch: parseInt(match[3], 10),
      prerelease: match[4] || null,
      build: match[5] || null
    };
  }

  /**
   * Check if version is compatible with another version
   * @param {string} otherVersion - Version to compare
   * @param {string} constraint - 'major' | 'minor' | 'patch'
   * @returns {boolean} True if compatible
   */
  isVersionCompatible(otherVersion, constraint = 'major') {
    const thisVersion = this.parseVersion();

    const match = otherVersion.match(/^(\d+)\.(\d+)\.(\d+)/);
    if (!match) return false;

    const other = {
      major: parseInt(match[1], 10),
      minor: parseInt(match[2], 10),
      patch: parseInt(match[3], 10)
    };

    switch (constraint) {
      case 'major':
        return thisVersion.major === other.major;
      case 'minor':
        return thisVersion.major === other.major &&
               thisVersion.minor === other.minor;
      case 'patch':
        return thisVersion.major === other.major &&
               thisVersion.minor === other.minor &&
               thisVersion.patch === other.patch;
      default:
        return false;
    }
  }

  /**
   * Check if license is compatible with base license
   * @param {string} baseLicense - Base license to check against
   * @returns {boolean} True if compatible
   */
  isLicenseCompatible(baseLicense = 'MIT') {
    return COMPATIBLE_LICENSES.has(this.license);
  }

  /**
   * Compute content hash for determinism
   * @returns {string} SHA-256 hash
   * @private
   */
  _computeHash() {
    const canonical = {
      name: this.name,
      version: this.version,
      category: this.category,
      dependencies: this.dependencies
        .map(d => `${d.name}@${d.version}`)
        .sort(),
      exports: this.exports
        .map(e => `${e.path}:${e.entryPoint}`)
        .sort()
    };

    return createHash('sha256')
      .update(JSON.stringify(canonical))
      .digest('hex');
  }

  /**
   * Get partition hash
   * @returns {string} Content hash
   */
  getHash() {
    return this._hash;
  }

  /**
   * Convert to JSON representation
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      name: this.name,
      version: this.version,
      category: this.category,
      description: this.description,
      dependencies: this.dependencies,
      exports: this.exports,
      isProtected: this.isProtectedPartition(),
      isDeprecated: this.isDeprecated,
      license: this.license,
      testCoverage: this.testCoverage,
      docCoverage: this.docCoverage,
      maintainers: this.maintainers,
      hash: this._hash
    };
  }

  /**
   * Create PackagePartition from package.json content
   * @param {Object} packageJson - package.json content
   * @param {Object} options - Additional options
   * @returns {PackagePartition} New partition instance
   */
  static fromPackageJson(packageJson, options = {}) {
    const name = packageJson.name;
    const category = PACKAGE_CATEGORY_MAP[name] || PartitionCategory.UTILITY;

    // Parse dependencies
    const dependencies = [];

    const addDeps = (deps, opts) => {
      if (!deps) return;
      for (const [depName, depVersion] of Object.entries(deps)) {
        dependencies.push({
          name: depName,
          version: depVersion.replace(/^workspace:\*$/, '*'),
          isWorkspace: depVersion.startsWith('workspace:'),
          ...opts
        });
      }
    };

    addDeps(packageJson.dependencies, { isDevDependency: false, isPeerDependency: false });
    addDeps(packageJson.devDependencies, { isDevDependency: true, isPeerDependency: false });
    addDeps(packageJson.peerDependencies, { isDevDependency: false, isPeerDependency: true });

    // Parse exports
    const exports = [];
    if (packageJson.exports) {
      for (const [path, entryPoint] of Object.entries(packageJson.exports)) {
        if (typeof entryPoint === 'string') {
          exports.push({
            path,
            entryPoint,
            isPublic: !path.includes('internal')
          });
        }
      }
    }

    return new PackagePartition({
      name,
      version: packageJson.version,
      category,
      description: packageJson.description,
      dependencies,
      exports,
      isProtected: PROTECTED_PACKAGES.has(name),
      license: packageJson.license || 'MIT',
      ...options
    });
  }
}

/**
 * PackageChange - Represents a proposed change to a package
 * Zod v4 compatible
 */
export const PackageChangeSchema = z.object({
  packageName: z.string(),
  changeType: z.string(), // enum simplified for Zod v4
  details: z.object({}).passthrough().optional(),
  affectedFiles: z.array(z.string()).optional(),
  reason: z.string().optional()
});

/**
 * PackageChange class
 */
export class PackageChange {
  constructor(config) {
    const validated = PackageChangeSchema.parse(config);
    Object.assign(this, validated);
  }

  /**
   * Check if this is a breaking change
   * @returns {boolean} True if breaking
   */
  isBreaking() {
    return this.changeType === 'api_breaking' ||
           this.changeType === 'export_remove' ||
           this.changeType === 'license_change';
  }

  /**
   * Check if this change affects public API
   * @returns {boolean} True if affects public API
   */
  affectsPublicApi() {
    return ['api_breaking', 'api_enhancement', 'export_add',
            'export_remove', 'export_modify'].includes(this.changeType);
  }
}

/**
 * PackageDelta - A capsule of changes to multiple packages
 * Zod v4 compatible schema
 */
export const PackageDeltaSchema = z.object({
  id: z.string(),
  timestamp: z.string(),
  agent: z.string(),
  changes: z.array(PackageChangeSchema),
  justification: z.string().optional(),
  metadata: z.record(z.string(), z.unknown()).optional()
});

/**
 * PackageDelta class - Represents a batch of package changes
 */
export class PackageDelta {
  constructor(config) {
    const validated = PackageDeltaSchema.parse({
      id: config.id || crypto.randomUUID(),
      timestamp: config.timestamp || new Date().toISOString(),
      agent: config.agent,
      changes: config.changes,
      justification: config.justification,
      metadata: config.metadata
    });

    Object.assign(this, validated);
    this._hash = this._computeHash();
  }

  /**
   * Get all affected packages
   * @returns {Set<string>} Set of package names
   */
  getAffectedPackages() {
    return new Set(this.changes.map(c => c.packageName));
  }

  /**
   * Get changes for a specific package
   * @param {string} packageName - Package name
   * @returns {Array<Object>} Changes for package
   */
  getChangesForPackage(packageName) {
    return this.changes.filter(c => c.packageName === packageName);
  }

  /**
   * Check if any changes are breaking
   * @returns {boolean} True if contains breaking changes
   */
  hasBreakingChanges() {
    return this.changes.some(c => new PackageChange(c).isBreaking());
  }

  /**
   * Get all breaking changes
   * @returns {Array<Object>} Breaking changes
   */
  getBreakingChanges() {
    return this.changes.filter(c => new PackageChange(c).isBreaking());
  }

  /**
   * Compute deterministic hash
   * @returns {string} SHA-256 hash
   * @private
   */
  _computeHash() {
    const canonical = {
      changes: this.changes
        .map(c => ({
          packageName: c.packageName,
          changeType: c.changeType,
          details: c.details
        }))
        .sort((a, b) => `${a.packageName}${a.changeType}`.localeCompare(
          `${b.packageName}${b.changeType}`
        ))
    };

    return createHash('sha256')
      .update(JSON.stringify(canonical))
      .digest('hex');
  }

  /**
   * Get delta hash
   * @returns {string} Content hash
   */
  getHash() {
    return this._hash;
  }

  /**
   * Convert to JSON
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      id: this.id,
      timestamp: this.timestamp,
      agent: this.agent,
      changes: this.changes,
      justification: this.justification,
      metadata: this.metadata,
      hash: this._hash,
      stats: {
        packageCount: this.getAffectedPackages().size,
        changeCount: this.changes.length,
        hasBreakingChanges: this.hasBreakingChanges()
      }
    };
  }
}

/**
 * Get category for a package name
 * @param {string} packageName - Package name
 * @returns {string} Category identifier
 */
export function getCategoryForPackage(packageName) {
  return PACKAGE_CATEGORY_MAP[packageName] || PartitionCategory.UTILITY;
}

/**
 * Check if package is protected
 * @param {string} packageName - Package name
 * @returns {boolean} True if protected
 */
export function isProtectedPackage(packageName) {
  return PROTECTED_PACKAGES.has(packageName);
}

/**
 * Get all packages in a category
 * @param {string} category - Category identifier
 * @returns {Array<string>} Package names in category
 */
export function getPackagesInCategory(category) {
  return Object.entries(PACKAGE_CATEGORY_MAP)
    .filter(([_, cat]) => cat === category)
    .map(([name]) => name);
}
