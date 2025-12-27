/**
 * @file Drift detection system - baseline snapshots and deviation detection
 * @module project-engine/drift-snapshot
 */

import { createHash } from 'crypto';
import { Store, DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { z } from 'zod';
import { diffGraphFromStores, _summarizeChangesByKind } from '../diff.mjs';
import { ProjectStructureLens } from './lens/project-structure.mjs';

const { namedNode, literal } = DataFactory;

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

const CreateSnapshotOptionsSchema = z.object({
  fsStore: z.object({}).passthrough(),
  domainStore: z.object({}).passthrough().optional().nullable(),
  templateMappings: z.record(z.string(), z.string()).optional(),
  baseIri: z.string().default('http://example.org/unrdf/snapshot#'),
});

const SnapshotReceiptSchema = z.object({
  hash: z.string(),
  createdAt: z.string(),
  summary: z.object({
    fileCount: z.number(),
    featureCount: z.number(),
    roleCount: z.number(),
    domainEntityCount: z.number(),
    templateMappingCount: z.number(),
  }),
});

const DriftResultSchema = z.object({
  ontologyDiff: z.object({
    triples: z.object({
      added: z.array(z.any()),
      removed: z.array(z.any()),
    }),
    changes: z.array(z.any()),
  }),
  summary: z.array(z.string()),
  driftSeverity: z.enum(['none', 'minor', 'major']),
});

/* ========================================================================= */
/* Snapshot Creation                                                        */
/* ========================================================================= */

/**
 * Create a structure snapshot from filesystem and domain stores
 * Encodes: FS structure + domain + template mappings
 *
 * @param {Store} fsStore - Filesystem structure store
 * @param {Store} [domainStore] - Domain ontology store (entities + fields)
 * @param {Object} [options] - Snapshot options
 * @param {Record<string, string>} [options.templateMappings] - File to template mappings
 * @param {string} [options.baseIri] - Base IRI for snapshot resources
 * @returns {{snapshotStore: Store, receipt: {hash: string, createdAt: string, summary: Object}}}
 */
export async function createStructureSnapshot(fsStore, domainStore, options = {}) {
  const validated = CreateSnapshotOptionsSchema.parse({
    fsStore,
    domainStore,
    ...options,
  });

  const { baseIri, templateMappings = {} } = validated;
  const snapshotStore = await createStore();
  const createdAt = new Date().toISOString();

  // Summary counters
  const summary = {
    fileCount: 0,
    featureCount: 0,
    roleCount: 0,
    domainEntityCount: 0,
    templateMappingCount: 0,
  };

  // 1. Copy FS structure graph (file -> feature -> role)
  const fsQuads = fsStore.getQuads(null, null, null, null);
  for (const quad of fsQuads) {
    snapshotStore.addQuad(quad.subject, quad.predicate, quad.object, quad.graph);

    // Count files
    if (
      quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
      quad.object.value ===
        'http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject'
    ) {
      summary.fileCount++;
    }

    // Count features
    if (
      quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
      quad.object.value === 'http://example.org/unrdf/project#Feature'
    ) {
      summary.featureCount++;
    }

    // Count roles
    if (quad.predicate.value === 'http://example.org/unrdf/project#hasRole') {
      summary.roleCount++;
    }
  }

  // 2. Copy domain ontology if provided
  if (domainStore) {
    const domainQuads = domainStore.getQuads(null, null, null, null);
    for (const quad of domainQuads) {
      snapshotStore.addQuad(quad.subject, quad.predicate, quad.object, quad.graph);

      // Count domain entities
      if (
        quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
        (quad.object.value.includes('Entity') || quad.object.value.includes('Class'))
      ) {
        summary.domainEntityCount++;
      }
    }
  }

  // 3. Encode template mappings (file -> template -> entity)
  for (const [filePath, templateId] of Object.entries(templateMappings)) {
    const fileIri = namedNode(`${baseIri}file/${encodeURIComponent(filePath)}`);
    const templateIri = namedNode(`${baseIri}template/${encodeURIComponent(templateId)}`);

    snapshotStore.addQuad(
      fileIri,
      namedNode('http://example.org/unrdf/project#usesTemplate'),
      templateIri
    );
    summary.templateMappingCount++;
  }

  // 4. Add snapshot metadata
  const snapshotIri = namedNode(`${baseIri}snapshot`);
  snapshotStore.addQuad(
    snapshotIri,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/snapshot#StructureSnapshot')
  );
  snapshotStore.addQuad(
    snapshotIri,
    namedNode('http://example.org/unrdf/snapshot#createdAt'),
    literal(createdAt, namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
  );
  snapshotStore.addQuad(
    snapshotIri,
    namedNode('http://example.org/unrdf/snapshot#fileCount'),
    literal(summary.fileCount, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
  );
  snapshotStore.addQuad(
    snapshotIri,
    namedNode('http://example.org/unrdf/snapshot#featureCount'),
    literal(summary.featureCount, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
  );

  // 5. Compute hash of combined state
  const hash = computeSnapshotHash(snapshotStore, createdAt);

  snapshotStore.addQuad(
    snapshotIri,
    namedNode('http://example.org/unrdf/snapshot#contentHash'),
    literal(hash)
  );

  const receipt = SnapshotReceiptSchema.parse({
    hash,
    createdAt,
    summary,
  });

  return { snapshotStore, receipt };
}

/* ========================================================================= */
/* Drift Detection                                                          */
/* ========================================================================= */

/**
 * Compute drift between current snapshot and baseline
 * Uses diffOntologyFromDelta + ProjectStructureLens
 *
 * @param {Store} currentSnapshot - Current project state snapshot
 * @param {Store} baselineSnapshot - Baseline snapshot to compare against
 * @returns {{ontologyDiff: Object, summary: string[], driftSeverity: 'none'|'minor'|'major'}}
 */
export async function computeDrift(currentSnapshot, baselineSnapshot) {
  // Compute graph-level diff
  const graphDiff = diffGraphFromStores(baselineSnapshot, currentSnapshot);

  // Apply ProjectStructureLens to get semantic changes
  const changes = [];

  for (const triple of graphDiff.added) {
    const change = ProjectStructureLens(triple, 'added');
    if (change) {
      changes.push(change);
    }
  }

  for (const triple of graphDiff.removed) {
    const change = ProjectStructureLens(triple, 'removed');
    if (change) {
      changes.push(change);
    }
  }

  const ontologyDiff = {
    triples: graphDiff,
    changes,
  };

  // Generate human-readable summary
  const summary = generateDriftSummary(graphDiff, changes, currentSnapshot, baselineSnapshot);

  // Compute severity
  const driftSeverity = computeDriftSeverity(graphDiff, changes);

  return DriftResultSchema.parse({
    ontologyDiff,
    summary,
    driftSeverity,
  });
}

/* ========================================================================= */
/* Drift Summary Generation                                                 */
/* ========================================================================= */

/**
 * Generate human-readable drift summary
 *
 * @param {Object} graphDiff - Low-level triple diff
 * @param {Object[]} changes - Semantic changes from lens
 * @param {Store} currentSnapshot - Current snapshot
 * @param {Store} baselineSnapshot - Baseline snapshot
 * @returns {string[]}
 */
async function generateDriftSummary(graphDiff, changes, currentSnapshot, baselineSnapshot) {
  const summary = [];

  // Group changes by kind
  const changesByKind = {};
  for (const change of changes) {
    if (!changesByKind[change.kind]) {
      changesByKind[change.kind] = [];
    }
    changesByKind[change.kind].push(change);
  }

  // Missing tests detection
  const missingTests = detectMissingTests(graphDiff, changes, currentSnapshot, baselineSnapshot);
  for (const msg of missingTests) {
    summary.push(msg);
  }

  // Unmatched template patterns
  const unmatchedPatterns = detectUnmatchedPatterns(graphDiff, changes, currentSnapshot);
  for (const msg of unmatchedPatterns) {
    summary.push(msg);
  }

  // Domain entity coverage gaps
  const coverageGaps = detectCoverageGaps(graphDiff, changes, currentSnapshot, baselineSnapshot);
  for (const msg of coverageGaps) {
    summary.push(msg);
  }

  // Test coverage changes
  const coverageChanges = detectTestCoverageChanges(currentSnapshot, baselineSnapshot);
  for (const msg of coverageChanges) {
    summary.push(msg);
  }

  // Feature additions/removals
  if (changesByKind['FeatureAdded']) {
    for (const change of changesByKind['FeatureAdded']) {
      const name = extractNameFromIri(change.entity);
      summary.push(`Feature "${name}" added since baseline`);
    }
  }

  if (changesByKind['FeatureRemoved']) {
    for (const change of changesByKind['FeatureRemoved']) {
      const name = extractNameFromIri(change.entity);
      summary.push(`Feature "${name}" removed (was in baseline)`);
    }
  }

  // Role changes
  if (changesByKind['RoleRemoved']) {
    for (const change of changesByKind['RoleRemoved']) {
      const entity = extractNameFromIri(change.entity);
      const role = extractNameFromIri(change.role);
      summary.push(`Role "${role}" removed from "${entity}"`);
    }
  }

  // If no drift detected
  if (summary.length === 0 && graphDiff.added.length === 0 && graphDiff.removed.length === 0) {
    summary.push('No structural drift detected - code matches baseline model');
  }

  return summary;
}

/**
 * Detect features missing required tests
 *
 * @param {Object} graphDiff
 * @param {Object[]} changes
 * @param {Store} currentSnapshot
 * @param {Store} baselineSnapshot
 * @returns {string[]}
 */
async function detectMissingTests(graphDiff, changes, currentSnapshot, baselineSnapshot) {
  const messages = [];

  // Get features in current snapshot
  const currentFeatures = new Set();
  const featureQuads = currentSnapshot.getQuads(
    null,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/project#Feature')
  );
  for (const quad of featureQuads) {
    currentFeatures.add(quad.subject.value);
  }

  // Check which features have tests
  const featuresWithTests = new Set();
  const testRoleQuads = currentSnapshot.getQuads(
    null,
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Test')
  );
  for (const quad of testRoleQuads) {
    // Find feature this file belongs to
    const belongsQuads = currentSnapshot.getQuads(
      quad.subject,
      namedNode('http://example.org/unrdf/project#belongsToFeature'),
      null
    );
    for (const bq of belongsQuads) {
      featuresWithTests.add(bq.object.value);
    }
  }

  // Check baseline for features that should have tests
  const baselineTestRequirements = new Set();
  const baselineTestQuads = baselineSnapshot.getQuads(
    null,
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Test')
  );
  for (const quad of baselineTestQuads) {
    const belongsQuads = baselineSnapshot.getQuads(
      quad.subject,
      namedNode('http://example.org/unrdf/project#belongsToFeature'),
      null
    );
    for (const bq of belongsQuads) {
      baselineTestRequirements.add(bq.object.value);
    }
  }

  // Report features that had tests in baseline but not now
  for (const featureIri of baselineTestRequirements) {
    if (currentFeatures.has(featureIri) && !featuresWithTests.has(featureIri)) {
      const name = extractNameFromIri(featureIri);
      messages.push(`Feature "${name}" missing tests when model requires them`);
    }
  }

  return messages;
}

/**
 * Detect files that don't match any template pattern
 *
 * @param {Object} graphDiff
 * @param {Object[]} changes
 * @param {Store} currentSnapshot
 * @returns {string[]}
 */
async function detectUnmatchedPatterns(graphDiff, changes, currentSnapshot) {
  const messages = [];

  // Find files without roles
  const filesWithRoles = new Set();
  const roleQuads = currentSnapshot.getQuads(
    null,
    namedNode('http://example.org/unrdf/project#hasRole'),
    null
  );
  for (const quad of roleQuads) {
    filesWithRoles.add(quad.subject.value);
  }

  // Find all files in feature directories
  const featureFiles = [];
  const pathQuads = currentSnapshot.getQuads(
    null,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    null
  );
  for (const quad of pathQuads) {
    const path = quad.object.value;
    if (path.includes('/features/') || path.includes('/modules/')) {
      if (!filesWithRoles.has(quad.subject.value)) {
        featureFiles.push(path);
      }
    }
  }

  // Group unmatched files by feature
  const unmatchedByFeature = {};
  for (const path of featureFiles) {
    const match = path.match(/\/(features|modules)\/([^/]+)\//);
    if (match) {
      const featureName = match[2];
      if (!unmatchedByFeature[featureName]) {
        unmatchedByFeature[featureName] = [];
      }
      unmatchedByFeature[featureName].push(path);
    }
  }

  for (const [featureName, files] of Object.entries(unmatchedByFeature)) {
    if (files.length > 0) {
      messages.push(
        `Files in features/${featureName} don't match any template pattern (${files.length} files)`
      );
    }
  }

  return messages;
}

/**
 * Detect domain entities without corresponding views/APIs
 *
 * @param {Object} graphDiff
 * @param {Object[]} changes
 * @param {Store} currentSnapshot
 * @param {Store} baselineSnapshot
 * @returns {string[]}
 */
async function detectCoverageGaps(graphDiff, changes, currentSnapshot, baselineSnapshot) {
  const messages = [];

  // Find domain entities in current snapshot
  const currentEntities = new Set();
  const entityQuads = currentSnapshot.getQuads(
    null,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    null
  );
  for (const quad of entityQuads) {
    if (quad.object.value.includes('Entity') || quad.object.value.includes('DomainClass')) {
      currentEntities.add(quad.subject.value);
    }
  }

  // Find entities in baseline
  const baselineEntities = new Set();
  const baselineEntityQuads = baselineSnapshot.getQuads(
    null,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    null
  );
  for (const quad of baselineEntityQuads) {
    if (quad.object.value.includes('Entity') || quad.object.value.includes('DomainClass')) {
      baselineEntities.add(quad.subject.value);
    }
  }

  // Find new entities (in current but not baseline)
  for (const entityIri of currentEntities) {
    if (!baselineEntities.has(entityIri)) {
      // Check if entity has views/APIs
      const hasViews = checkEntityHasViews(entityIri, currentSnapshot);
      const hasApis = checkEntityHasApis(entityIri, currentSnapshot);

      if (!hasViews && !hasApis) {
        const name = extractNameFromIri(entityIri);
        messages.push(`Domain entity "${name}" added but no views/APIs for it`);
      }
    }
  }

  return messages;
}

/**
 * Detect test coverage changes
 *
 * @param {Store} currentSnapshot
 * @param {Store} baselineSnapshot
 * @returns {string[]}
 */
async function detectTestCoverageChanges(currentSnapshot, baselineSnapshot) {
  const messages = [];

  // Count tests in baseline
  const baselineTestCount = baselineSnapshot.getQuads(
    null,
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Test')
  ).length;

  // Count tests in current
  const currentTestCount = currentSnapshot.getQuads(
    null,
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Test')
  ).length;

  // Count total files
  const baselineFileCount = baselineSnapshot.getQuads(
    null,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject')
  ).length;

  const currentFileCount = currentSnapshot.getQuads(
    null,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject')
  ).length;

  // Compute coverage ratios
  const baselineCoverage = baselineFileCount > 0 ? baselineTestCount / baselineFileCount : 0;
  const currentCoverage = currentFileCount > 0 ? currentTestCount / currentFileCount : 0;

  // Report if coverage dropped
  if (currentCoverage < baselineCoverage * 0.9) {
    const baselinePct = (baselineCoverage * 100).toFixed(1);
    const currentPct = (currentCoverage * 100).toFixed(1);
    messages.push(`Test coverage dropped below baseline (was ${baselinePct}%, now ${currentPct}%)`);
  }

  return messages;
}

/**
 * Check if entity has view files
 *
 * @param {string} entityIri
 * @param {Store} store
 * @returns {boolean}
 */
async function checkEntityHasViews(entityIri, store) {
  const entityName = extractNameFromIri(entityIri).toLowerCase();

  // Look for view/component files containing entity name
  const pathQuads = store.getQuads(
    null,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    null
  );

  for (const quad of pathQuads) {
    const path = quad.object.value.toLowerCase();
    if (
      path.includes(entityName) &&
      (path.includes('view') || path.includes('page') || path.includes('component'))
    ) {
      return true;
    }
  }

  return false;
}

/**
 * Check if entity has API files
 *
 * @param {string} entityIri
 * @param {Store} store
 * @returns {boolean}
 */
async function checkEntityHasApis(entityIri, store) {
  const entityName = extractNameFromIri(entityIri).toLowerCase();

  // Look for API/route files containing entity name
  const pathQuads = store.getQuads(
    null,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    null
  );

  for (const quad of pathQuads) {
    const path = quad.object.value.toLowerCase();
    if (
      path.includes(entityName) &&
      (path.includes('api') || path.includes('route') || path.includes('controller'))
    ) {
      return true;
    }
  }

  return false;
}

/* ========================================================================= */
/* Drift Severity Computation                                               */
/* ========================================================================= */

/**
 * Compute drift severity based on changes
 *
 * @param {Object} graphDiff
 * @param {Object[]} changes
 * @returns {'none'|'minor'|'major'}
 */
async function computeDriftSeverity(graphDiff, changes) {
  const totalTripleChanges = graphDiff.added.length + graphDiff.removed.length;

  // No changes = no drift
  if (totalTripleChanges === 0 && changes.length === 0) {
    return 'none';
  }

  // Count critical changes
  let criticalCount = 0;
  let minorCount = 0;

  for (const change of changes) {
    if (change.kind === 'FeatureRemoved' || change.kind === 'ModuleRemoved') {
      criticalCount++;
    } else if (change.kind === 'RoleRemoved') {
      // Removed tests or docs is critical
      if (change.role && (change.role.includes('Test') || change.role.includes('Doc'))) {
        criticalCount++;
      } else {
        minorCount++;
      }
    } else {
      minorCount++;
    }
  }

  // Major if any critical changes or many minor changes
  if (criticalCount > 0 || minorCount > 10) {
    return 'major';
  }

  // Minor if few changes
  if (minorCount > 0 || totalTripleChanges > 0) {
    return 'minor';
  }

  return 'none';
}

/* ========================================================================= */
/* Utility Functions                                                        */
/* ========================================================================= */

/**
 * Compute SHA-256 hash of snapshot
 *
 * @param {Store} store
 * @param {string} timestamp
 * @returns {string}
 */
async function computeSnapshotHash(store, timestamp) {
  const hash = createHash('sha256');

  // Hash store size and timestamp
  hash.update(String(store.size));
  hash.update(timestamp);

  // Hash a sample of quads for content fingerprint
  const quads = store.getQuads(null, null, null, null);
  const sample = quads.slice(0, Math.min(100, quads.length));

  for (const quad of sample) {
    hash.update(quad.subject.value);
    hash.update(quad.predicate.value);
    hash.update(quad.object.value);
  }

  return hash.digest('hex').substring(0, 32);
}

/**
 * Extract name from IRI
 *
 * @param {string} iri
 * @returns {string}
 */
async function extractNameFromIri(iri) {
  if (!iri) return 'unknown';

  // Try hash fragment
  const hashMatch = iri.match(/#([^#]+)$/);
  if (hashMatch) return decodeURIComponent(hashMatch[1]);

  // Try last path segment
  const slashMatch = iri.match(/\/([^/]+)$/);
  if (slashMatch) return decodeURIComponent(slashMatch[1]);

  return iri;
}

/* ========================================================================= */
/* Convenience Exports                                                      */
/* ========================================================================= */

/**
 * Create empty baseline snapshot
 *
 * @returns {Store}
 */
export async function createEmptyBaseline() {
  return await createStore();
}

/**
 * Serialize snapshot to JSON for persistence
 *
 * @param {Store} snapshotStore
 * @param {Object} receipt
 * @returns {string}
 */
export async function serializeSnapshot(snapshotStore, receipt) {
  const quads = snapshotStore.getQuads(null, null, null, null);
  const serialized = {
    version: '1.0.0',
    receipt,
    quads: quads.map(q => ({
      subject: q.subject.value,
      predicate: q.predicate.value,
      object: q.object.value,
    })),
  };
  return JSON.stringify(serialized, null, 2);
}

/**
 * Deserialize snapshot from JSON
 *
 * @param {string} json
 * @returns {{snapshotStore: Store, receipt: Object}}
 */
export async function deserializeSnapshot(json) {
  const data = JSON.parse(json);
  const store = await createStore();

  for (const q of data.quads) {
    store.addQuad(
      namedNode(q.subject),
      namedNode(q.predicate),
      q.object.startsWith('http') ? namedNode(q.object) : literal(q.object)
    );
  }

  return {
    snapshotStore: store,
    receipt: data.receipt,
  };
}
