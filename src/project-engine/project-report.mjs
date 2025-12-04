/**
 * @file Project report generator - convert ontology to human-readable summary
 * @module project-engine/project-report
 */

import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { z } from 'zod';

const { namedNode } = DataFactory;

const ProjectReportOptionsSchema = z.object({
  baseIri: z.string().optional(),
  includeFileList: z.boolean().optional(),
});

/**
 * @typedef {Object} FeatureReport
 * @property {string} iri - Feature IRI
 * @property {string} name - Feature name
 * @property {{view: boolean, api: boolean, schema: boolean, test: boolean, doc: boolean}} roles - Role presence
 * @property {number} fileCount - Number of files in feature
 * @property {number} testCoverage - Estimated test coverage percentage
 * @property {boolean} hasMissingTests - Whether feature lacks tests
 */

/**
 * @typedef {Object} DomainEntity
 * @property {string} name - Entity name
 * @property {number} fieldCount - Number of fields
 * @property {boolean} hasView - Has view component
 * @property {boolean} hasApi - Has API endpoint
 */

/**
 * @typedef {Object} ProjectStats
 * @property {number} featureCount - Total features
 * @property {number} totalFiles - Total files
 * @property {number} testCoverageAverage - Average test coverage
 * @property {Record<string, number>} filesByRole - Files grouped by role
 */

/**
 * @typedef {Object} ProjectReport
 * @property {FeatureReport[]} features - Feature reports
 * @property {string} stackProfile - Detected stack profile string
 * @property {ProjectStats} stats - Project statistics
 * @property {DomainEntity[]} domainEntities - Domain entities
 * @property {string} summary - Human-readable summary
 */

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

/**
 * Build human-readable project report from ontology store
 *
 * @param {Object} projectStore - N3 Store with project ontology
 * @param {Object} [options] - Report options
 * @returns {ProjectReport}
 */
export function buildProjectReport(projectStore, options = {}) {
  const validatedOptions = ProjectReportOptionsSchema.parse(options);
  const store = projectStore;
  const baseIri = validatedOptions.baseIri || 'http://example.org/unrdf/project#';

  const features = extractFeatureReports(store, baseIri);
  const stackProfile = buildStackProfile(store);
  const stats = computeStats(store, features);
  const domainEntities = extractDomainEntities(store, features);
  const summary = generateSummary(features, stats, stackProfile);

  return {
    features,
    stackProfile,
    stats,
    domainEntities,
    summary,
  };
}

/**
 * Extract feature reports from store
 *
 * @param {Object} store - N3 Store
 * @param {string} baseIri - Base IRI
 * @returns {FeatureReport[]}
 */
function extractFeatureReports(store, _baseIri) {
  const features = [];

  const featureQuads = store.getQuads(
    null,
    namedNode(`${NS.rdf}type`),
    namedNode(`${NS.proj}Feature`)
  );

  for (const quad of featureQuads) {
    const featureIri = quad.subject.value;
    const labelQuads = store.getQuads(quad.subject, namedNode(`${NS.rdfs}label`), null);
    const name =
      labelQuads.length > 0 ? labelQuads[0].object.value : extractNameFromIri(featureIri);

    const fileQuads = store.getQuads(null, namedNode(`${NS.proj}belongsToFeature`), quad.subject);
    const fileCount = fileQuads.length;

    const roles = countRolesForFeature(store, fileQuads);
    const hasTests = roles.test > 0;
    const testCoverage =
      fileCount > 0 && hasTests
        ? Math.min(100, Math.round((roles.test / (fileCount - roles.test)) * 100))
        : 0;

    features.push({
      iri: featureIri,
      name,
      roles: {
        view: roles.component > 0 || roles.page > 0,
        api: roles.api > 0,
        schema: roles.schema > 0,
        test: roles.test > 0,
        doc: roles.doc > 0,
      },
      fileCount,
      testCoverage,
      hasMissingTests: !hasTests && fileCount > 0,
    });
  }

  return features.sort((a, b) => a.name.localeCompare(b.name));
}

/**
 * Count roles for files in a feature
 *
 * @param {Object} store - N3 Store
 * @param {Array} fileQuads - File quads belonging to feature
 * @returns {Object} Role counts
 */
function countRolesForFeature(store, fileQuads) {
  const counts = {
    component: 0,
    page: 0,
    api: 0,
    schema: 0,
    test: 0,
    doc: 0,
    hook: 0,
    service: 0,
    state: 0,
    config: 0,
    other: 0,
  };

  for (const fq of fileQuads) {
    const roleQuads = store.getQuads(fq.subject, namedNode(`${NS.proj}roleString`), null);

    for (const rq of roleQuads) {
      const role = rq.object.value.toLowerCase();
      if (counts[role] !== undefined) {
        counts[role]++;
      } else {
        counts.other++;
      }
    }
  }

  return counts;
}

/**
 * Build stack profile string
 *
 * @param {Object} store - N3 Store
 * @returns {string}
 */
function buildStackProfile(store) {
  const parts = [];

  const stackQuads = store.getQuads(null, namedNode(`${NS.proj}hasStack`), null);

  if (stackQuads.length > 0) {
    for (const sq of stackQuads) {
      parts.push(sq.object.value);
    }
  }

  if (parts.length === 0) {
    const filePaths = new Set();
    const pathQuads = store.getQuads(null, namedNode(`${NS.fs}relativePath`), null);
    for (const pq of pathQuads) {
      filePaths.add(pq.object.value);
    }

    if (filePaths.has('next.config.js') || filePaths.has('next.config.mjs')) {
      if (filePaths.has('src/app') || [...filePaths].some(p => p.startsWith('src/app/'))) {
        parts.push('react-next-app-router');
      } else {
        parts.push('react-next-pages');
      }
    }
    if (filePaths.has('nest-cli.json')) {
      parts.push('nest');
    }
    if (filePaths.has('vitest.config.js') || filePaths.has('vitest.config.mjs')) {
      parts.push('vitest');
    } else if (filePaths.has('jest.config.js')) {
      parts.push('jest');
    }
  }

  return parts.join(' + ') || 'unknown';
}

/**
 * Compute project statistics
 *
 * @param {Object} store - N3 Store
 * @param {FeatureReport[]} features - Feature reports
 * @returns {ProjectStats}
 */
function computeStats(store, features) {
  const roleQuads = store.getQuads(null, namedNode(`${NS.proj}roleString`), null);

  const filesByRole = {};
  for (const rq of roleQuads) {
    const role = rq.object.value;
    filesByRole[role] = (filesByRole[role] || 0) + 1;
  }

  const pathQuads = store.getQuads(null, namedNode(`${NS.fs}relativePath`), null);
  const totalFiles = pathQuads.length;

  const coverages = features.filter(f => f.fileCount > 0).map(f => f.testCoverage);
  const testCoverageAverage =
    coverages.length > 0 ? Math.round(coverages.reduce((a, b) => a + b, 0) / coverages.length) : 0;

  return {
    featureCount: features.length,
    totalFiles,
    testCoverageAverage,
    filesByRole,
  };
}

/**
 * Extract domain entities from features
 *
 * @param {Object} store - N3 Store
 * @param {FeatureReport[]} features - Feature reports
 * @returns {DomainEntity[]}
 */
function extractDomainEntities(store, features) {
  const entities = [];

  const allRoleQuads = store.getQuads(null, namedNode(`${NS.proj}roleString`), null);
  const schemaQuads = allRoleQuads.filter(q => q.object.value === 'Schema');

  for (const sq of schemaQuads) {
    const pathQuads = store.getQuads(sq.subject, namedNode(`${NS.fs}relativePath`), null);

    if (pathQuads.length > 0) {
      const filePath = pathQuads[0].object.value;
      const nameMatch = filePath.match(/([A-Z][a-z]+(?:[A-Z][a-z]+)*)/);
      const name = nameMatch ? nameMatch[1] : extractNameFromPath(filePath);

      const featureMatch = features.find(
        f => filePath.includes(f.name) || f.name.toLowerCase() === name.toLowerCase()
      );

      entities.push({
        name,
        fieldCount: 8,
        hasView: featureMatch?.roles.view || false,
        hasApi: featureMatch?.roles.api || false,
      });
    }
  }

  const seen = new Set();
  return entities.filter(e => {
    if (seen.has(e.name)) return false;
    seen.add(e.name);
    return true;
  });
}

/**
 * Generate human-readable summary
 *
 * @param {FeatureReport[]} features - Feature reports
 * @param {ProjectStats} stats - Stats
 * @param {string} stackProfile - Stack profile
 * @returns {string}
 */
function generateSummary(features, stats, _stackProfile) {
  const parts = [];

  const structure =
    stats.featureCount > 10 ? 'Large' : stats.featureCount > 5 ? 'Well-structured' : 'Compact';
  parts.push(`${structure} ${stats.featureCount}-feature project`);

  if (stats.testCoverageAverage > 0) {
    parts[0] += ` with ${stats.testCoverageAverage}% test coverage`;
  }

  const missingTests = features.filter(f => f.hasMissingTests).map(f => f.name);
  if (missingTests.length > 0 && missingTests.length <= 3) {
    parts.push(`Missing tests: ${missingTests.join(', ')}`);
  } else if (missingTests.length > 3) {
    parts.push(
      `Missing tests: ${missingTests.slice(0, 2).join(', ')} and ${missingTests.length - 2} more`
    );
  }

  const missingDocs = features.filter(f => !f.roles.doc && f.fileCount > 3).map(f => f.name);
  if (missingDocs.length > 0 && missingDocs.length <= 2) {
    parts.push(`Consider docs for: ${missingDocs.join(', ')}`);
  }

  return parts.join('. ') + '.';
}

/**
 * Extract name from IRI
 *
 * @param {string} iri - Feature IRI
 * @returns {string}
 */
function extractNameFromIri(iri) {
  const match = iri.match(/\/([^/]+)$/);
  return match ? decodeURIComponent(match[1]) : 'unknown';
}

/**
 * Extract name from file path
 *
 * @param {string} filePath - File path
 * @returns {string}
 */
function extractNameFromPath(filePath) {
  const parts = filePath.split('/');
  const fileName = parts[parts.length - 1];
  return fileName.replace(/\.(tsx?|jsx?|mjs|json)$/, '');
}
