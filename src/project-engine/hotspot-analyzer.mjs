/**
 * @file Hotspot analyzer - identify high-risk features by complexity metrics
 * @module project-engine/hotspot-analyzer
 */

import { DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { z } from 'zod';

const { namedNode, _literal } = DataFactory;

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

/**
 * Scoring weights for hotspot calculation
 * @type {{fileCount: number, testCoverage: number, dependencies: number, complexity: number}}
 */
const SCORING_WEIGHTS = {
  fileCount: 0.3,
  testCoverage: 0.4,
  dependencies: 0.2,
  complexity: 0.1,
};

/**
 * Risk thresholds
 */
const RISK_THRESHOLDS = {
  HIGH: 70,
  MEDIUM: 40,
  LOW: 0,
};

const HotspotOptionsSchema = z.object({
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  domainStore: z
    .custom(val => val && typeof val.getQuads === 'function', {
      message: 'domainStore must be an RDF store with getQuads method',
    })
    .optional(),
  stackProfile: z
    .object({
      testFramework: z.string().nullable().optional(),
      sourceRoot: z.string().optional(),
    })
    .optional(),
  baseIri: z.string().default('http://example.org/unrdf/hotspot#'),
});

const FeatureMetricsSchema = z.object({
  fileCount: z.number().min(0),
  lineCount: z.number().min(0),
  testCount: z.number().min(0),
  testCoverage: z.number().min(0).max(100),
  dependencies: z.number().min(0),
});

/**
 * @typedef {Object} FeatureMetrics
 * @property {number} fileCount - Total files in the feature
 * @property {number} lineCount - Approximate line count (from byte size)
 * @property {number} testCount - Number of test files
 * @property {number} testCoverage - Test coverage percentage (testCount / fileCount * 100)
 * @property {number} dependencies - Number of dependencies/relationships
 */

/**
 * @typedef {Object} HotspotEntry
 * @property {string} feature - Feature name
 * @property {number} score - Hotspot score (0-100)
 * @property {'HIGH'|'MEDIUM'|'LOW'} risk - Risk level
 * @property {FeatureMetrics} metrics - Detailed metrics
 * @property {string} recommendation - Actionable recommendation
 */

/**
 * @typedef {Object} HotspotResult
 * @property {HotspotEntry[]} hotspots - All features with scores
 * @property {{feature: string, score: number, reason: string}[]} topRisks - Top risk features
 * @property {string} summary - Human-readable summary
 */

/**
 * Analyze project for hotspots - high-risk or high-complexity features
 *
 * @param {Object} options
 * @param {Store} options.projectStore - Project model store (with features, file roles)
 * @param {Store} [options.domainStore] - Domain model store (optional, for relationships)
 * @param {Object} [options.stackProfile] - Stack information
 * @param {string} [options.baseIri] - Base IRI for hotspot resources
 * @returns {HotspotResult} Hotspot analysis results
 */
export function analyzeHotspots(options) {
  const validated = HotspotOptionsSchema.parse(options);
  const { projectStore, domainStore, _baseIri } = validated;

  // Extract all features from the project store
  const features = extractFeatures(projectStore);

  // Calculate metrics for each feature
  const hotspots = [];
  for (const [featureName, featureData] of Object.entries(features)) {
    const metrics = calculateFeatureMetrics(featureName, featureData, projectStore, domainStore);
    const score = scoreFeature(featureName, metrics);
    const risk = getRiskLevel(score);
    const recommendation = generateRecommendation(featureName, metrics, risk);

    hotspots.push({
      feature: featureName,
      score,
      risk,
      metrics,
      recommendation,
    });
  }

  // Sort by score descending (highest risk first)
  hotspots.sort((a, b) => b.score - a.score);

  // Extract top risks (score > 40 or top 5)
  const topRisks = hotspots
    .filter(h => h.score > RISK_THRESHOLDS.MEDIUM)
    .slice(0, 5)
    .map(h => ({
      feature: h.feature,
      score: h.score,
      reason: `${h.metrics.fileCount} files, ${h.metrics.testCoverage}% coverage`,
    }));

  // Generate summary
  const highRiskCount = hotspots.filter(h => h.risk === 'HIGH').length;
  const summary =
    highRiskCount > 0
      ? `${highRiskCount} high-risk feature${highRiskCount > 1 ? 's' : ''} identified`
      : 'No high-risk features identified';

  return {
    hotspots,
    topRisks,
    summary,
  };
}

/**
 * Score a feature based on its metrics
 *
 * Formula:
 * - File count: 30% weight - more files = higher score
 * - Test coverage: 40% weight - LESS coverage = higher score
 * - Dependencies: 20% weight - more deps = higher score
 * - Complexity: 10% weight - larger files = higher score
 *
 * @param {string} feature - Feature name
 * @param {FeatureMetrics} metrics - Feature metrics
 * @returns {number} Score 0-100 (higher = more risk)
 */
export function scoreFeature(feature, metrics) {
  const validatedMetrics = FeatureMetricsSchema.parse(metrics);

  // File count score: normalized to 0-100 (40 files = 100)
  const fileCountScore = Math.min(100, (validatedMetrics.fileCount / 40) * 100);

  // Test coverage score: inverted (less coverage = higher score)
  const testCoverageScore = 100 - validatedMetrics.testCoverage;

  // Dependencies score: normalized to 0-100 (15 deps = 100)
  const dependencyScore = Math.min(100, (validatedMetrics.dependencies / 15) * 100);

  // Complexity score: based on average lines per file (200 lines/file = 100)
  const avgLinesPerFile =
    validatedMetrics.fileCount > 0 ? validatedMetrics.lineCount / validatedMetrics.fileCount : 0;
  const complexityScore = Math.min(100, (avgLinesPerFile / 200) * 100);

  // Weighted average
  const score =
    fileCountScore * SCORING_WEIGHTS.fileCount +
    testCoverageScore * SCORING_WEIGHTS.testCoverage +
    dependencyScore * SCORING_WEIGHTS.dependencies +
    complexityScore * SCORING_WEIGHTS.complexity;

  return Math.round(score);
}

/**
 * Extract features from project store
 *
 * @private
 * @param {Store} projectStore
 * @returns {Object<string, {iri: string, files: string[]}>}
 */
function extractFeatures(projectStore) {
  const features = {};

  // Get all feature IRIs
  const featureQuads = projectStore.getQuads(
    null,
    namedNode(`${NS.rdf}type`),
    namedNode(`${NS.proj}Feature`)
  );

  for (const quad of featureQuads) {
    const featureIri = quad.subject.value;

    // Get feature label
    const labelQuads = projectStore.getQuads(quad.subject, namedNode(`${NS.rdfs}label`), null);
    const featureName =
      labelQuads.length > 0 ? labelQuads[0].object.value : extractNameFromIri(featureIri);

    // Get files belonging to this feature
    const fileQuads = projectStore.getQuads(
      null,
      namedNode(`${NS.proj}belongsToFeature`),
      quad.subject
    );

    const files = fileQuads.map(fq => {
      const pathQuads = projectStore.getQuads(fq.subject, namedNode(`${NS.fs}relativePath`), null);
      return pathQuads.length > 0 ? pathQuads[0].object.value : fq.subject.value;
    });

    features[featureName] = {
      iri: featureIri,
      files,
    };
  }

  return features;
}

/**
 * Calculate metrics for a feature
 *
 * @private
 * @param {string} featureName
 * @param {{iri: string, files: string[]}} featureData
 * @param {Store} projectStore
 * @param {Store} [domainStore]
 * @returns {FeatureMetrics}
 */
function calculateFeatureMetrics(featureName, featureData, projectStore, domainStore) {
  const files = featureData.files;
  const fileCount = files.length;

  // Count test files (files with Test role or .test./.spec. in name)
  let testCount = 0;
  let totalByteSize = 0;

  for (const filePath of files) {
    // Check if test file
    if (isTestFile(filePath, projectStore)) {
      testCount++;
    }

    // Get byte size for line count approximation
    const fileIri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(filePath)}`);
    const sizeQuads = projectStore.getQuads(fileIri, namedNode(`${NS.fs}byteSize`), null);
    if (sizeQuads.length > 0) {
      totalByteSize += parseInt(sizeQuads[0].object.value, 10) || 0;
    }
  }

  // Approximate line count (assume ~40 bytes per line average)
  const lineCount = Math.round(totalByteSize / 40);

  // Test coverage: percentage of test files
  const testCoverage = fileCount > 0 ? Math.round((testCount / fileCount) * 100) : 100; // No files = 100% coverage (nothing to test)

  // Count dependencies from domain store or project relationships
  const dependencies = countDependencies(featureData, projectStore, domainStore);

  return {
    fileCount,
    lineCount,
    testCount,
    testCoverage,
    dependencies,
  };
}

/**
 * Check if a file is a test file
 *
 * @private
 * @param {string} filePath
 * @param {Store} projectStore
 * @returns {boolean}
 */
function isTestFile(filePath, projectStore) {
  // Check by file path pattern
  if (/\.(test|spec)\.(tsx?|jsx?|mjs)$/.test(filePath)) {
    return true;
  }
  if (/^(test|tests|__tests__|spec)\//.test(filePath)) {
    return true;
  }

  // Check by role in store
  const fileIri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(filePath)}`);
  const roleQuads = projectStore.getQuads(fileIri, namedNode(`${NS.proj}roleString`), null);

  return roleQuads.some(q => q.object.value === 'Test');
}

/**
 * Count dependencies for a feature
 *
 * @private
 * @param {{iri: string, files: string[]}} featureData
 * @param {Store} projectStore
 * @param {Store} [domainStore]
 * @returns {number}
 */
function countDependencies(featureData, projectStore, domainStore) {
  let deps = 0;

  // Count relationships from project store
  const featureIri = namedNode(featureData.iri);
  const relQuads = projectStore.getQuads(featureIri, null, null);

  // Count outgoing relationships (excluding type and label)
  for (const quad of relQuads) {
    if (quad.predicate.value !== `${NS.rdf}type` && quad.predicate.value !== `${NS.rdfs}label`) {
      deps++;
    }
  }

  // If domain store provided, count entity relationships
  if (domainStore) {
    const domainRels = domainStore.getQuads(
      null,
      namedNode('http://example.org/unrdf/domain#relatesTo'),
      null
    );
    // Count unique relationships (simplified - just total)
    deps += Math.min(domainRels.length, 20);
  }

  return deps;
}

/**
 * Get risk level from score
 *
 * @private
 * @param {number} score
 * @returns {'HIGH'|'MEDIUM'|'LOW'}
 */
function getRiskLevel(score) {
  if (score >= RISK_THRESHOLDS.HIGH) return 'HIGH';
  if (score >= RISK_THRESHOLDS.MEDIUM) return 'MEDIUM';
  return 'LOW';
}

/**
 * Generate recommendation based on metrics and risk
 *
 * @private
 * @param {string} featureName
 * @param {FeatureMetrics} metrics
 * @param {'HIGH'|'MEDIUM'|'LOW'} risk
 * @returns {string}
 */
function generateRecommendation(featureName, metrics, risk) {
  const issues = [];

  if (metrics.fileCount > 30) {
    issues.push('high file count');
  }
  if (metrics.testCoverage < 50) {
    issues.push('low test coverage');
  }
  if (metrics.dependencies > 10) {
    issues.push('many dependencies');
  }
  if (metrics.lineCount > 5000) {
    issues.push('large codebase');
  }

  if (issues.length === 0) {
    return 'Feature is within acceptable complexity thresholds.';
  }

  const issueStr = issues.join(' + ');
  const action =
    risk === 'HIGH'
      ? 'Add tests or refactor.'
      : risk === 'MEDIUM'
        ? 'Consider adding tests.'
        : 'Monitor for growth.';

  return `${issueStr.charAt(0).toUpperCase() + issueStr.slice(1)}. ${action}`;
}

/**
 * Extract name from IRI
 *
 * @private
 * @param {string} iri
 * @returns {string}
 */
function extractNameFromIri(iri) {
  const parts = iri.split(/[#/]/);
  const last = parts[parts.length - 1];
  return decodeURIComponent(last);
}
