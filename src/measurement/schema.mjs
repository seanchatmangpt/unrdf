/**
 * @file Measurement Schema and Algorithms
 * @module @unrdf/measurement/schema
 *
 * @description
 * Comprehensive type definitions and algorithms for the multi-package
 * measurement framework. Provides feature extraction templates, aggregation
 * algorithms, and certificate validation logic.
 *
 * @version 1.0.0
 */

import { z } from 'zod';
import * as crypto from 'node:crypto';

// ============================================================================
// SECTION 1: CONSTANTS AND CONFIGURATION
// ============================================================================

/**
 * Normalization constants for entropy computation
 * @type {Object}
 */
export const NORMALIZATION_CONSTANTS = Object.freeze({
  MAX_FILES: 100,
  MAX_LINES: 10000,
  MAX_FUNCS: 500,
  MAX_DEPS: 50,
  MAX_COMPLEXITY: 30
});

/**
 * Tier weight multipliers for entropy aggregation
 * @type {Object.<number, number>}
 */
export const TIER_WEIGHTS = Object.freeze({
  1: 2.0,   // Core packages - highest criticality
  2: 1.5,   // Infrastructure packages
  3: 1.0,   // Application packages
  4: 0.8,   // Integration packages
  5: 0.5    // Documentation packages
});

/**
 * Maximum entropy capacity per tier (in bits)
 * @type {Object.<number, number>}
 */
export const TIER_CAPACITY = Object.freeze({
  1: 24,  // Core
  2: 20,  // Infrastructure
  3: 18,  // Application
  4: 16,  // Integration
  5: 12   // Documentation
});

/**
 * Partition weights for per-package entropy calculation
 * @type {Object.<string, number>}
 */
export const PARTITION_WEIGHTS = Object.freeze({
  src: 0.40,
  tests: 0.30,
  docs: 0.20,
  config: 0.10
});

/**
 * Entropy thresholds for alerts
 * @type {Object.<string, number>}
 */
export const ENTROPY_THRESHOLDS = Object.freeze({
  LOW: 10,
  MODERATE: 20,
  HIGH: 30,
  CRITICAL: 40
});

/**
 * Transitive entropy decay factor
 * @type {number}
 */
export const TE_DECAY_FACTOR = 0.7;

// ============================================================================
// SECTION 2: ZOD SCHEMAS (TYPE DEFINITIONS)
// ============================================================================

/**
 * Package features schema
 * @type {import('zod').ZodObject}
 */
export const PackageFeaturesSchema = z.object({
  // File counts
  src_file_count: z.number().int().nonnegative().default(0),
  test_file_count: z.number().int().nonnegative().default(0),
  doc_file_count: z.number().int().nonnegative().default(0),
  config_file_count: z.number().int().nonnegative().default(0),

  // Code metrics
  total_lines: z.number().int().nonnegative().default(0),
  avg_file_length: z.number().nonnegative().default(0),
  max_file_length: z.number().int().nonnegative().default(0),

  // Complexity metrics
  function_count: z.number().int().nonnegative().default(0),
  cyclomatic_complexity: z.number().nonnegative().default(0),
  max_cyclomatic_complexity: z.number().nonnegative().default(0),

  // Dependency metrics
  internal_deps: z.number().int().nonnegative().default(0),
  external_deps: z.number().int().nonnegative().default(0),
  dev_deps: z.number().int().nonnegative().default(0),

  // Test metrics
  test_coverage_percent: z.number().min(0).max(100).default(0),
  test_pass_rate: z.number().min(0).max(100).default(100),
  test_count: z.number().int().nonnegative().default(0)
});

/**
 * @typedef {z.infer<typeof PackageFeaturesSchema>} PackageFeatures
 */

/**
 * Package measurement schema
 * @type {import('zod').ZodObject}
 */
export const PackageMeasurementSchema = z.object({
  name: z.string().min(1),
  path: z.string().min(1),
  tier: z.number().int().min(1).max(5),

  // Entropy measurements
  D_t: z.number().nonnegative(),            // Structural entropy
  C_t: z.number(),                           // Change capacity (can be negative)
  C_max: z.number().positive(),             // Maximum capacity for tier

  // Temporal coupling (array of 43 values)
  TC: z.array(z.number().min(0).max(1)).optional(),

  // Transitive entropy
  TE: z.number().nonnegative().optional(),

  // Raw features
  features: PackageFeaturesSchema,

  // Metadata
  measured_at: z.string().datetime(),
  git_hash: z.string().optional()
});

/**
 * @typedef {z.infer<typeof PackageMeasurementSchema>} PackageMeasurement
 */

/**
 * System-wide measurement schema
 * @type {import('zod').ZodObject}
 */
export const SystemMeasurementSchema = z.object({
  // Aggregate entropy metrics
  D_t: z.number().nonnegative(),            // Total system entropy
  D_t_weighted: z.number().nonnegative(),   // Tier-weighted entropy
  C_t: z.number(),                           // Average change capacity
  C_t_total: z.number(),                     // Total change capacity

  // Coupling metrics
  TC_density: z.number().min(0).max(1),     // Density of TC matrix
  TC_max: z.number().min(0).max(1),         // Maximum off-diagonal TC
  TC_clusters: z.number().int().nonnegative(), // Number of high-coupling clusters

  // Transitive entropy
  TE_max: z.number().nonnegative(),         // Maximum TE across packages
  TE_avg: z.number().nonnegative(),         // Average TE

  // Package statistics
  package_count: z.number().int().positive(),
  tier_distribution: z.record(z.string(), z.number().int()),

  // Health indicators
  packages_over_capacity: z.number().int().nonnegative(),
  packages_near_capacity: z.number().int().nonnegative(), // C_t < 3 bits
  entropy_trend: z.enum(['increasing', 'stable', 'decreasing']).optional()
});

/**
 * @typedef {z.infer<typeof SystemMeasurementSchema>} SystemMeasurement
 */

/**
 * Dimension certificate schema
 * @type {import('zod').ZodObject}
 */
export const DimensionCertificateSchema = z.object({
  // Identification
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  certificate_id: z.string().uuid(),

  // Temporal markers
  timestamp: z.string().datetime(),
  epoch: z.number().int().nonnegative(),

  // Cryptographic integrity
  checksum: z.string().regex(/^sha256:[a-f0-9]{64}$/),
  prev_checksum: z.string().regex(/^sha256:[a-f0-9]{64}$/).optional(),

  // Measurements
  system: SystemMeasurementSchema,
  packages: z.array(PackageMeasurementSchema),

  // TC matrix (flattened for storage)
  tc_matrix: z.array(z.number()).optional(),

  // Signature
  signature: z.string().optional(),
  signed_by: z.string().optional()
});

/**
 * @typedef {z.infer<typeof DimensionCertificateSchema>} DimensionCertificate
 */

/**
 * Feature extraction template schema
 * @type {import('zod').ZodObject}
 */
export const FeatureExtractionTemplateSchema = z.object({
  name: z.string(),
  tier: z.number().int().min(1).max(5),
  file_patterns: z.object({
    src: z.array(z.string()),
    tests: z.array(z.string()),
    docs: z.array(z.string()),
    config: z.array(z.string())
  }),
  extract_complexity: z.boolean().default(true),
  extract_coverage: z.boolean().default(true),
  custom_extractors: z.array(z.string()).optional()
});

/**
 * @typedef {z.infer<typeof FeatureExtractionTemplateSchema>} FeatureExtractionTemplate
 */

// ============================================================================
// SECTION 3: ENTROPY COMPUTATION ALGORITHMS
// ============================================================================

/**
 * Compute Shannon entropy contribution for a single metric
 *
 * @param {number} value - Observed value
 * @param {number} maxValue - Normalization constant
 * @returns {number} Entropy contribution (0 to ~1 bits)
 */
export function computeShannonContribution(value, maxValue) {
  if (value <= 0 || maxValue <= 0) return 0;

  const p = Math.min(value / maxValue, 1.0);

  // Shannon entropy: -p * log2(p)
  // When p=1, contribution is 0
  // Maximum contribution when p=0.368 (1/e)
  return p > 0 ? -p * Math.log2(p) : 0;
}

/**
 * Compute per-package structural entropy (D_t)
 *
 * @param {PackageFeatures} features - Extracted package features
 * @returns {number} Entropy in bits
 */
export function computePackageEntropy(features) {
  const { MAX_FILES, MAX_LINES, MAX_FUNCS, MAX_DEPS, MAX_COMPLEXITY } = NORMALIZATION_CONSTANTS;

  // Compute individual contributions
  const H_files = computeShannonContribution(features.src_file_count, MAX_FILES);
  const H_lines = computeShannonContribution(features.total_lines, MAX_LINES);
  const H_funcs = computeShannonContribution(features.function_count, MAX_FUNCS);
  const H_deps = computeShannonContribution(
    features.internal_deps + features.external_deps,
    MAX_DEPS
  );
  const H_complexity = computeShannonContribution(
    features.cyclomatic_complexity,
    MAX_COMPLEXITY
  );

  // Weighted combination (weights sum to 1.0)
  const entropy = (
    0.25 * H_files +
    0.25 * H_lines +
    0.20 * H_funcs +
    0.15 * H_deps +
    0.15 * H_complexity
  );

  // Scale to typical bit range (multiply by 16 for ~0-16 bit range)
  return entropy * 16;
}

/**
 * Compute per-package change capacity (C_t)
 *
 * @param {number} D_t - Current entropy
 * @param {number} tier - Package tier (1-5)
 * @returns {number} Remaining capacity in bits
 */
export function computeChangeCapacity(D_t, tier) {
  const C_max = TIER_CAPACITY[tier] ?? TIER_CAPACITY[3];
  return C_max - D_t;
}

/**
 * Compute temporal coupling between two packages
 * Uses Jaccard similarity of commit sets
 *
 * @param {Set<string>} commits_i - Commit hashes touching package i
 * @param {Set<string>} commits_j - Commit hashes touching package j
 * @returns {number} Coupling coefficient [0, 1]
 */
export function computeTemporalCoupling(commits_i, commits_j) {
  if (commits_i.size === 0 && commits_j.size === 0) return 0;
  if (commits_i === commits_j) return 1;

  // Intersection
  const intersection = new Set([...commits_i].filter(x => commits_j.has(x)));

  // Union
  const union = new Set([...commits_i, ...commits_j]);

  return union.size > 0 ? intersection.size / union.size : 0;
}

/**
 * Build the TC matrix for all packages
 *
 * @param {Map<string, Set<string>>} commitsByPackage - Map of package name to commit sets
 * @param {string[]} packageOrder - Ordered list of package names
 * @returns {number[][]} 2D TC matrix
 */
export function buildTCMatrix(commitsByPackage, packageOrder) {
  const n = packageOrder.length;
  const matrix = Array(n).fill(null).map(() => Array(n).fill(0));

  for (let i = 0; i < n; i++) {
    for (let j = i; j < n; j++) {
      if (i === j) {
        matrix[i][j] = 1.0;
      } else {
        const commits_i = commitsByPackage.get(packageOrder[i]) ?? new Set();
        const commits_j = commitsByPackage.get(packageOrder[j]) ?? new Set();
        const tc = computeTemporalCoupling(commits_i, commits_j);
        matrix[i][j] = tc;
        matrix[j][i] = tc; // Symmetric
      }
    }
  }

  return matrix;
}

/**
 * Compute transitive entropy using topological propagation
 *
 * @param {Map<string, PackageMeasurement>} measurements - Package measurements
 * @param {Map<string, string[]>} dependencyGraph - Package -> dependencies
 * @param {number} [decay=TE_DECAY_FACTOR] - Decay factor per hop
 * @returns {Map<string, number>} Package name -> TE value
 */
export function computeTransitiveEntropy(measurements, dependencyGraph, decay = TE_DECAY_FACTOR) {
  const teMap = new Map();
  const visited = new Set();

  /**
   * Recursive TE computation with memoization
   * @param {string} pkg - Package name
   * @returns {number} TE value
   */
  function computeTE(pkg) {
    if (teMap.has(pkg)) return teMap.get(pkg);

    // Guard against cycles
    if (visited.has(pkg)) return 0;
    visited.add(pkg);

    const measurement = measurements.get(pkg);
    if (!measurement) return 0;

    const baseEntropy = measurement.D_t;
    const deps = dependencyGraph.get(pkg) ?? [];

    // Sum of dependencies' TE with decay
    const depContribution = deps.reduce((sum, dep) => {
      return sum + decay * computeTE(dep);
    }, 0);

    const te = baseEntropy + depContribution;
    teMap.set(pkg, te);
    visited.delete(pkg);

    return te;
  }

  // Compute TE for all packages
  for (const pkg of measurements.keys()) {
    computeTE(pkg);
  }

  return teMap;
}

// ============================================================================
// SECTION 4: AGGREGATION ALGORITHMS
// ============================================================================

/**
 * Aggregate package measurements into system-wide metrics
 *
 * @param {PackageMeasurement[]} packages - Array of package measurements
 * @returns {SystemMeasurement} System-wide measurement
 */
export function aggregateToSystem(packages) {
  const n = packages.length;
  if (n === 0) {
    throw new Error('Cannot aggregate empty package list');
  }

  // Total and weighted entropy
  let D_t_total = 0;
  let D_t_weighted = 0;
  let C_t_total = 0;
  let packagesOverCapacity = 0;
  let packagesNearCapacity = 0;

  const tierCounts = { 1: 0, 2: 0, 3: 0, 4: 0, 5: 0 };

  for (const pkg of packages) {
    D_t_total += pkg.D_t;
    D_t_weighted += pkg.D_t * (TIER_WEIGHTS[pkg.tier] ?? 1.0);
    C_t_total += pkg.C_t;
    tierCounts[pkg.tier] = (tierCounts[pkg.tier] ?? 0) + 1;

    if (pkg.C_t < 0) packagesOverCapacity++;
    else if (pkg.C_t < 3) packagesNearCapacity++;
  }

  // TC matrix statistics (if present)
  let TC_density = 0;
  let TC_max = 0;
  let TC_clusters = 0;

  const packagesWithTC = packages.filter(p => p.TC && p.TC.length > 0);
  if (packagesWithTC.length > 0) {
    const allTC = packagesWithTC.flatMap(p => p.TC ?? []);
    const offDiagonalTC = allTC.filter((v, i) => i % (n + 1) !== 0 && v > 0);

    if (offDiagonalTC.length > 0) {
      TC_density = offDiagonalTC.filter(v => v > 0.1).length / offDiagonalTC.length;
      TC_max = Math.max(...offDiagonalTC);
    }

    // Count clusters (packages with TC > 0.5 to any other)
    TC_clusters = packagesWithTC.filter(p =>
      (p.TC ?? []).some((v, i) => i !== packages.indexOf(p) && v > 0.5)
    ).length;
  }

  // TE statistics
  const teValues = packages.filter(p => p.TE !== undefined).map(p => p.TE ?? 0);
  const TE_max = teValues.length > 0 ? Math.max(...teValues) : 0;
  const TE_avg = teValues.length > 0 ? teValues.reduce((a, b) => a + b, 0) / teValues.length : 0;

  return {
    D_t: D_t_total,
    D_t_weighted,
    C_t: C_t_total / n,
    C_t_total,
    TC_density,
    TC_max,
    TC_clusters,
    TE_max,
    TE_avg,
    package_count: n,
    tier_distribution: Object.fromEntries(
      Object.entries(tierCounts).filter(([_, v]) => v > 0)
    ),
    packages_over_capacity: packagesOverCapacity,
    packages_near_capacity: packagesNearCapacity,
    entropy_trend: undefined // Set by historical comparison
  };
}

/**
 * Flatten TC matrix for storage
 *
 * @param {number[][]} matrix - 2D TC matrix
 * @returns {number[]} Flattened array (row-major order)
 */
export function flattenTCMatrix(matrix) {
  return matrix.flat();
}

/**
 * Unflatten TC matrix from storage
 *
 * @param {number[]} flat - Flattened array
 * @param {number} n - Matrix dimension
 * @returns {number[][]} 2D TC matrix
 */
export function unflattenTCMatrix(flat, n) {
  const matrix = [];
  for (let i = 0; i < n; i++) {
    matrix.push(flat.slice(i * n, (i + 1) * n));
  }
  return matrix;
}

// ============================================================================
// SECTION 5: CERTIFICATE GENERATION AND VALIDATION
// ============================================================================

/**
 * Generate certificate checksum from measurements
 *
 * @param {SystemMeasurement} system - System measurement
 * @param {PackageMeasurement[]} packages - Package measurements
 * @returns {string} SHA256 checksum prefixed with "sha256:"
 */
export function generateChecksum(system, packages) {
  // Canonical JSON representation
  const payload = JSON.stringify({
    system: {
      D_t: system.D_t,
      C_t: system.C_t,
      package_count: system.package_count
    },
    packages: packages.map(p => ({
      name: p.name,
      D_t: p.D_t,
      C_t: p.C_t,
      tier: p.tier
    })).sort((a, b) => a.name.localeCompare(b.name))
  });

  const hash = crypto.createHash('sha256').update(payload).digest('hex');
  return `sha256:${hash}`;
}

/**
 * Create a dimension certificate from measurements
 *
 * @param {PackageMeasurement[]} packages - Package measurements
 * @param {Object} [options] - Generation options
 * @param {string} [options.prevChecksum] - Previous certificate checksum
 * @param {number[][]} [options.tcMatrix] - TC matrix
 * @returns {DimensionCertificate} Generated certificate
 */
export function createCertificate(packages, options = {}) {
  const system = aggregateToSystem(packages);
  const checksum = generateChecksum(system, packages);

  const certificate = {
    version: '1.0.0',
    certificate_id: crypto.randomUUID(),
    timestamp: new Date().toISOString(),
    epoch: Date.now(),
    checksum,
    prev_checksum: options.prevChecksum,
    system,
    packages,
    tc_matrix: options.tcMatrix ? flattenTCMatrix(options.tcMatrix) : undefined,
    signature: undefined,
    signed_by: undefined
  };

  return DimensionCertificateSchema.parse(certificate);
}

/**
 * Verify certificate integrity
 *
 * @param {DimensionCertificate} certificate - Certificate to verify
 * @returns {Object} Verification result
 */
export function verifyCertificate(certificate) {
  const errors = [];
  const warnings = [];

  // 1. Schema validation
  try {
    DimensionCertificateSchema.parse(certificate);
  } catch (e) {
    errors.push(`Schema validation failed: ${e.message}`);
    return { valid: false, errors, warnings };
  }

  // 2. Checksum verification
  const expectedChecksum = generateChecksum(certificate.system, certificate.packages);
  if (expectedChecksum !== certificate.checksum) {
    errors.push(`Checksum mismatch: expected ${expectedChecksum}, got ${certificate.checksum}`);
  }

  // 3. Completeness check
  if (certificate.packages.length !== certificate.system.package_count) {
    errors.push(`Package count mismatch: ${certificate.packages.length} vs ${certificate.system.package_count}`);
  }

  // 4. Invariant checks
  for (const pkg of certificate.packages) {
    // Tier consistency
    const expectedCapacity = TIER_CAPACITY[pkg.tier];
    if (expectedCapacity && pkg.C_max !== expectedCapacity) {
      warnings.push(`Package ${pkg.name}: C_max ${pkg.C_max} doesn't match tier ${pkg.tier} capacity ${expectedCapacity}`);
    }

    // Entropy bounds
    if (pkg.D_t < 0) {
      errors.push(`Package ${pkg.name}: negative entropy D_t = ${pkg.D_t}`);
    }

    // Capacity calculation
    const expectedCt = pkg.C_max - pkg.D_t;
    if (Math.abs(pkg.C_t - expectedCt) > 0.001) {
      warnings.push(`Package ${pkg.name}: C_t calculation mismatch`);
    }
  }

  // 5. TC matrix symmetry check (if present)
  if (certificate.tc_matrix && certificate.tc_matrix.length > 0) {
    const n = certificate.packages.length;
    const matrix = unflattenTCMatrix(certificate.tc_matrix, n);

    for (let i = 0; i < n; i++) {
      // Diagonal should be 1
      if (Math.abs(matrix[i][i] - 1.0) > 0.001) {
        errors.push(`TC matrix diagonal [${i}][${i}] = ${matrix[i][i]}, expected 1.0`);
      }

      // Symmetry
      for (let j = i + 1; j < n; j++) {
        if (Math.abs(matrix[i][j] - matrix[j][i]) > 0.001) {
          errors.push(`TC matrix not symmetric at [${i}][${j}]`);
        }
      }
    }
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings
  };
}

// ============================================================================
// SECTION 6: FEATURE EXTRACTION TEMPLATES
// ============================================================================

/**
 * Default feature extraction template for all package types
 * @type {FeatureExtractionTemplate}
 */
export const DEFAULT_EXTRACTION_TEMPLATE = {
  name: 'default',
  tier: 3,
  file_patterns: {
    src: ['src/**/*.mjs', 'src/**/*.ts', 'src/**/*.js'],
    tests: ['**/*.test.mjs', '**/*.spec.mjs', 'test/**/*.mjs'],
    docs: ['**/*.md', '**/*.mdx', 'docs/**/*'],
    config: ['package.json', '*.config.mjs', '*.config.js', 'tsconfig.json']
  },
  extract_complexity: true,
  extract_coverage: true
};

/**
 * Template for core packages (Tier 1)
 * @type {FeatureExtractionTemplate}
 */
export const CORE_EXTRACTION_TEMPLATE = {
  ...DEFAULT_EXTRACTION_TEMPLATE,
  name: 'core',
  tier: 1,
  custom_extractors: ['api_surface', 'type_exports', 'breaking_changes']
};

/**
 * Template for infrastructure packages (Tier 2)
 * @type {FeatureExtractionTemplate}
 */
export const INFRASTRUCTURE_EXTRACTION_TEMPLATE = {
  ...DEFAULT_EXTRACTION_TEMPLATE,
  name: 'infrastructure',
  tier: 2,
  custom_extractors: ['observability_hooks', 'config_schema']
};

/**
 * Template for documentation packages (Tier 5)
 * @type {FeatureExtractionTemplate}
 */
export const DOCUMENTATION_EXTRACTION_TEMPLATE = {
  name: 'documentation',
  tier: 5,
  file_patterns: {
    src: [],
    tests: [],
    docs: ['**/*.md', '**/*.mdx', 'pages/**/*'],
    config: ['package.json', 'next.config.mjs']
  },
  extract_complexity: false,
  extract_coverage: false
};

/**
 * Get extraction template for a package tier
 *
 * @param {number} tier - Package tier (1-5)
 * @returns {FeatureExtractionTemplate} Appropriate template
 */
export function getExtractionTemplate(tier) {
  switch (tier) {
    case 1: return CORE_EXTRACTION_TEMPLATE;
    case 2: return INFRASTRUCTURE_EXTRACTION_TEMPLATE;
    case 5: return DOCUMENTATION_EXTRACTION_TEMPLATE;
    default: return DEFAULT_EXTRACTION_TEMPLATE;
  }
}

// ============================================================================
// SECTION 7: UTILITY FUNCTIONS
// ============================================================================

/**
 * Calculate entropy trend from historical data
 *
 * @param {number[]} historicalDt - Array of D_t values (oldest first)
 * @returns {'increasing' | 'stable' | 'decreasing'} Trend direction
 */
export function calculateEntropyTrend(historicalDt) {
  if (historicalDt.length < 2) return 'stable';

  // Simple linear regression slope
  const n = historicalDt.length;
  const xMean = (n - 1) / 2;
  const yMean = historicalDt.reduce((a, b) => a + b, 0) / n;

  let numerator = 0;
  let denominator = 0;

  for (let i = 0; i < n; i++) {
    numerator += (i - xMean) * (historicalDt[i] - yMean);
    denominator += (i - xMean) ** 2;
  }

  const slope = denominator !== 0 ? numerator / denominator : 0;

  // Threshold for significance (0.1 bits per epoch)
  if (slope > 0.1) return 'increasing';
  if (slope < -0.1) return 'decreasing';
  return 'stable';
}

/**
 * Format entropy value for display
 *
 * @param {number} bits - Entropy in bits
 * @returns {string} Formatted string (e.g., "18.79 bits")
 */
export function formatEntropy(bits) {
  return `${bits.toFixed(2)} bits`;
}

/**
 * Get health status for a package based on capacity
 *
 * @param {number} C_t - Change capacity
 * @param {number} C_max - Maximum capacity
 * @returns {'healthy' | 'warning' | 'critical' | 'overflow'} Status
 */
export function getPackageHealth(C_t, C_max) {
  const ratio = C_t / C_max;

  if (C_t < 0) return 'overflow';
  if (ratio < 0.1) return 'critical';
  if (ratio < 0.25) return 'warning';
  return 'healthy';
}

/**
 * Compare two certificates and detect changes
 *
 * @param {DimensionCertificate} prev - Previous certificate
 * @param {DimensionCertificate} curr - Current certificate
 * @returns {Object} Diff summary
 */
export function diffCertificates(prev, curr) {
  const changes = {
    D_t_delta: curr.system.D_t - prev.system.D_t,
    C_t_delta: curr.system.C_t - prev.system.C_t,
    packages_added: [],
    packages_removed: [],
    packages_changed: []
  };

  const prevNames = new Set(prev.packages.map(p => p.name));
  const currNames = new Set(curr.packages.map(p => p.name));

  // Detect added/removed
  for (const name of currNames) {
    if (!prevNames.has(name)) {
      changes.packages_added.push(name);
    }
  }

  for (const name of prevNames) {
    if (!currNames.has(name)) {
      changes.packages_removed.push(name);
    }
  }

  // Detect entropy changes
  const prevMap = new Map(prev.packages.map(p => [p.name, p]));
  for (const currPkg of curr.packages) {
    const prevPkg = prevMap.get(currPkg.name);
    if (prevPkg && Math.abs(currPkg.D_t - prevPkg.D_t) > 0.01) {
      changes.packages_changed.push({
        name: currPkg.name,
        D_t_prev: prevPkg.D_t,
        D_t_curr: currPkg.D_t,
        D_t_delta: currPkg.D_t - prevPkg.D_t
      });
    }
  }

  return changes;
}

// ============================================================================
// SECTION 8: EXPORTS SUMMARY
// ============================================================================

export default {
  // Constants
  NORMALIZATION_CONSTANTS,
  TIER_WEIGHTS,
  TIER_CAPACITY,
  PARTITION_WEIGHTS,
  ENTROPY_THRESHOLDS,
  TE_DECAY_FACTOR,

  // Schemas
  PackageFeaturesSchema,
  PackageMeasurementSchema,
  SystemMeasurementSchema,
  DimensionCertificateSchema,
  FeatureExtractionTemplateSchema,

  // Entropy algorithms
  computeShannonContribution,
  computePackageEntropy,
  computeChangeCapacity,
  computeTemporalCoupling,
  buildTCMatrix,
  computeTransitiveEntropy,

  // Aggregation
  aggregateToSystem,
  flattenTCMatrix,
  unflattenTCMatrix,

  // Certificate operations
  generateChecksum,
  createCertificate,
  verifyCertificate,

  // Templates
  DEFAULT_EXTRACTION_TEMPLATE,
  CORE_EXTRACTION_TEMPLATE,
  INFRASTRUCTURE_EXTRACTION_TEMPLATE,
  DOCUMENTATION_EXTRACTION_TEMPLATE,
  getExtractionTemplate,

  // Utilities
  calculateEntropyTrend,
  formatEntropy,
  getPackageHealth,
  diffCertificates
};
