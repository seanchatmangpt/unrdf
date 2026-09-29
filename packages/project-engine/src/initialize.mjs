/**
 * @file Project initialization pipeline
 * @module project-engine/initialize
 *
 * @description
 * Scans a project, detects its stack, builds a feature model, classifies file roles,
 * infers a domain model and generator templates, takes a content-hash baseline snapshot,
 * derives hooks, and (unless `dryRun`) persists everything under `<root>/.unrdf/`.
 *
 * All graph access goes through the Oxigraph store's pattern API (`getQuads`); no SPARQL
 * strings are built anywhere in this module.
 */

import { z } from 'zod';
import { createHash } from 'crypto';
import { createReadStream, promises as fs } from 'fs';
import path from 'path';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { scanFileSystemToStore } from './fs-scan.mjs';
import { detectStackFromFs } from './stack-detect.mjs';
import { buildProjectModelFromFs } from './project-model.mjs';
import { classifyFiles } from './file-roles.mjs';
import { RDF_TYPE, RDFS_LABEL, FS, PROJECT, DOMAIN } from './namespaces.mjs';

const { namedNode, literal } = dataFactory;

/** Files larger than this are snapshotted by size only (contentHash: null). */
const MAX_HASHED_BYTES = 16 * 1024 * 1024;

const PHASES = [
  'scan',
  'stackDetection',
  'projectModel',
  'fileRoles',
  'domainInference',
  'templateInference',
  'snapshot',
  'hooks',
];

const ConventionsSchema = z.object({
  sourcePaths: z.array(z.string()).default(['src']),
  featurePaths: z.array(z.string()).default(['features', 'modules']),
  testPaths: z.array(z.string()).default(['__tests__', 'test', 'tests', 'spec']),
});

const InitializeOptionsSchema = z.object({
  ignorePatterns: z.array(z.string()).optional(),
  baseIri: z.string().default('http://example.org/unrdf/'),
  conventions: ConventionsSchema.optional(),
  skipPhases: z.array(z.enum(PHASES)).default([]),
  dryRun: z.boolean().default(false),
  skipSnapshot: z.boolean().default(false),
  skipHooks: z.boolean().default(false),
  verbose: z.boolean().default(false),
  outputDir: z.string().min(1).default('.unrdf'),
});

/**
 * @typedef {Object} PhaseReceipt
 * @property {number} duration - Phase duration in ms
 * @property {boolean} success
 * @property {string} [error]
 * @property {Object} [data]
 */

/**
 * @typedef {Object} InitializationResult
 * @property {boolean} success
 * @property {string} [error] - Set when success is false
 * @property {{phases: Object<string, PhaseReceipt>, totalDuration: number, success: boolean, failedPhase?: string, error?: string}} receipt
 * @property {Object} report - Human/CLI facing report (also persisted as init-report.json)
 * @property {Object} state - In-memory stores and graphs
 * @property {string[]} outputs - Absolute paths of files written (empty on dry run)
 */

/**
 * Run the project initialization pipeline.
 *
 * @param {string} projectRoot - Root directory of the project
 * @param {Object} [options]
 * @param {string[]} [options.ignorePatterns] - Names/globs ignored by the scan
 * @param {string} [options.baseIri] - Base IRI for generated RDF resources
 * @param {Object} [options.conventions] - sourcePaths / featurePaths / testPaths
 * @param {string[]} [options.skipPhases] - Phases to skip
 * @param {boolean} [options.dryRun] - Compute everything, write nothing
 * @param {boolean} [options.skipSnapshot] - Skip the baseline snapshot
 * @param {boolean} [options.skipHooks] - Skip hook derivation
 * @param {string} [options.outputDir] - Output directory relative to the root (default `.unrdf`)
 * @returns {Promise<InitializationResult>}
 */
export async function createProjectInitializationPipeline(projectRoot, options = {}) {
  const opts = InitializeOptionsSchema.parse(options);
  if (typeof projectRoot !== 'string' || projectRoot.length === 0) {
    throw new TypeError('projectRoot must be a non-empty string');
  }
  const root = path.resolve(projectRoot);
  const startTime = Date.now();
  const skip = new Set(opts.skipPhases);
  if (opts.skipSnapshot) skip.add('snapshot');
  if (opts.skipHooks) skip.add('hooks');

  const fsBaseIri = `${opts.baseIri}fs#`;
  const projectBaseIri = `${opts.baseIri}project#`;
  const domainBaseIri = `${opts.baseIri}domain#`;

  const phases = {};
  const state = {
    fsStore: null,
    projectStore: null,
    domainStore: null,
    templateGraph: null,
    snapshot: null,
  };

  /** Run one phase, record its receipt, and report failure. */
  const run = async (name, fn) => {
    const started = Date.now();
    try {
      const data = await fn();
      phases[name] = { duration: Date.now() - started, success: true, data };
      return true;
    } catch (error) {
      phases[name] = { duration: Date.now() - started, success: false, error: error.message };
      return false;
    }
  };
  const fail = name => buildFailureResult(phases, startTime, name, phases[name].error);

  if (!skip.has('scan')) {
    const ok = await run('scan', async () => {
      const { store, summary } = await scanFileSystemToStore({
        root,
        ignorePatterns: opts.ignorePatterns,
        baseIri: fsBaseIri,
      });
      state.fsStore = store;
      return {
        files: summary.fileCount,
        folders: summary.folderCount,
        ignored: summary.ignoredCount,
      };
    });
    if (!ok) return fail('scan');
  }

  if (!skip.has('stackDetection') && state.fsStore) {
    const ok = await run('stackDetection', () => {
      const profile = detectStackFromFs({ fsStore: state.fsStore });
      const frameworks = [
        ...new Set(
          [
            profile.uiFramework,
            profile.webFramework,
            profile.apiFramework,
            profile.testFramework,
          ].filter(Boolean)
        ),
      ];
      return { profile, frameworks };
    });
    if (!ok) return fail('stackDetection');
  }

  if (!skip.has('projectModel') && state.fsStore) {
    const ok = await run('projectModel', () => {
      state.projectStore = buildProjectModelFromFs({
        fsStore: state.fsStore,
        baseIri: projectBaseIri,
        fsBaseIri,
        conventions: opts.conventions,
      });
      return {
        features: state.projectStore.getQuads(null, namedNode(RDF_TYPE), namedNode(PROJECT.Feature))
          .length,
      };
    });
    if (!ok) return fail('projectModel');
  }

  if (!skip.has('fileRoles') && state.projectStore) {
    const ok = await run('fileRoles', () => {
      classifyFiles({ fsStore: state.projectStore, baseIri: projectBaseIri });
      const classified = state.projectStore.getQuads(
        null,
        namedNode(PROJECT.roleString),
        null
      ).length;
      return { classified, unclassified: listFiles(state.projectStore).length - classified };
    });
    if (!ok) return fail('fileRoles');
  }

  if (!skip.has('domainInference') && state.projectStore) {
    const ok = await run('domainInference', () => {
      const { store, entities, fields } = inferDomain(state.projectStore, domainBaseIri);
      state.domainStore = store;
      return { entities, fields };
    });
    if (!ok) return fail('domainInference');
  }

  if (!skip.has('templateInference') && state.domainStore) {
    const ok = await run('templateInference', () => {
      const templates = readEntities(state.domainStore).flatMap(entity =>
        templatesForEntity(entity.name)
      );
      state.templateGraph = { templates, patterns: inferPatterns(templates) };
      return { templates: templates.length, patterns: state.templateGraph.patterns };
    });
    if (!ok) return fail('templateInference');
  }

  if (!skip.has('snapshot') && state.fsStore) {
    const ok = await run('snapshot', async () => {
      state.snapshot = await createSnapshot(root, state.fsStore);
      return { hash: state.snapshot.hash, files: state.snapshot.files.length };
    });
    if (!ok) return fail('snapshot');
  }

  let hooksDoc = null;
  if (!skip.has('hooks') && state.domainStore) {
    const ok = await run('hooks', () => {
      hooksDoc = deriveHooks(readEntities(state.domainStore));
      return { registered: hooksDoc.hooks.length, invariants: hooksDoc.invariants.length };
    });
    if (!ok) return fail('hooks');
  }

  const report = buildReport(state, phases, hooksDoc);
  phases.report = { duration: 0, success: true, data: { report } };

  const outputs = [];
  if (!opts.dryRun) {
    const ok = await run('persist', async () => {
      const outDir = path.resolve(root, opts.outputDir);
      if (
        path.relative(root, outDir).startsWith('..') ||
        path.isAbsolute(path.relative(root, outDir))
      ) {
        throw new Error(`outputDir must be inside the project root: ${opts.outputDir}`);
      }
      await fs.mkdir(outDir, { recursive: true });
      const files = {
        'init-report.json': report,
        ...(state.snapshot ? { 'snapshot.json': state.snapshot } : {}),
        ...(hooksDoc ? { 'hooks.json': hooksDoc } : {}),
        ...(state.templateGraph ? { 'templates.json': state.templateGraph } : {}),
      };
      for (const [name, doc] of Object.entries(files)) {
        const target = path.join(outDir, name);
        await fs.writeFile(target, JSON.stringify(doc, null, 2) + '\n', 'utf8');
        outputs.push(target);
      }
      if (state.domainStore) {
        const target = path.join(outDir, 'domain.nt');
        await fs.writeFile(
          target,
          state.domainStore.dump({
            format: 'application/n-triples',
            from_graph_name: dataFactory.defaultGraph(),
          }),
          'utf8'
        );
        outputs.push(target);
      }
      return { outputs: outputs.map(p => path.relative(root, p)) };
    });
    if (!ok) return fail('persist');
  }

  return {
    success: true,
    receipt: { phases, totalDuration: Date.now() - startTime, success: true },
    report,
    state,
    outputs,
  };
}

/* ------------------------------------------------------------------------- */
/* Graph helpers (pattern API only)                                          */
/* ------------------------------------------------------------------------- */

/** @returns {Array<{iri: string, path: string}>} file subjects with their relative paths */
function listFiles(store) {
  const fileIris = new Set(
    store.getQuads(null, namedNode(FS.byteSize), null).map(q => q.subject.value)
  );
  return store
    .getQuads(null, namedNode(FS.relativePath), null)
    .filter(q => fileIris.has(q.subject.value))
    .map(q => ({ iri: q.subject.value, path: q.object.value }))
    .sort((a, b) => (a.path < b.path ? -1 : a.path > b.path ? 1 : 0));
}

function labelOf(store, subject) {
  return store.getQuads(subject, namedNode(RDFS_LABEL), null)[0]?.object.value;
}

/** @returns {Array<{iri: string, name: string, files: string[]}>} */
function readFeatures(store) {
  const files = new Map(listFiles(store).map(f => [f.iri, f.path]));
  return store
    .getQuads(null, namedNode(RDF_TYPE), namedNode(PROJECT.Feature))
    .map(q => {
      const members = store
        .getQuads(null, namedNode(PROJECT.belongsToFeature), q.subject)
        .map(m => files.get(m.subject.value))
        .filter(Boolean)
        .sort();
      return {
        iri: q.subject.value,
        name: labelOf(store, q.subject) ?? q.subject.value,
        files: members,
      };
    })
    .sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
}

function readEntities(domainStore) {
  return domainStore
    .getQuads(null, namedNode(RDF_TYPE), namedNode(DOMAIN.Entity))
    .map(q => ({
      iri: q.subject,
      name: labelOf(domainStore, q.subject) ?? 'Unknown',
      fieldCount: domainStore.getQuads(q.subject, namedNode(DOMAIN.hasField), null).length,
    }))
    .sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
}

/* ------------------------------------------------------------------------- */
/* Domain, templates, hooks                                                  */
/* ------------------------------------------------------------------------- */

const NON_ENTITY_FEATURES = new Set([
  'utils',
  'util',
  'helpers',
  'lib',
  'shared',
  'common',
  'config',
  'hooks',
  'types',
  'test',
  'tests',
  'components',
  'assets',
  'styles',
  'constants',
]);

const COMMON_FIELDS = [
  { name: 'id', type: 'string' },
  { name: 'createdAt', type: 'datetime' },
  { name: 'updatedAt', type: 'datetime' },
];

const SPECIFIC_FIELDS = {
  User: [
    { name: 'email', type: 'string' },
    { name: 'name', type: 'string' },
    { name: 'role', type: 'string' },
  ],
  Product: [
    { name: 'name', type: 'string' },
    { name: 'price', type: 'number' },
    { name: 'description', type: 'string' },
  ],
  Order: [
    { name: 'status', type: 'string' },
    { name: 'total', type: 'number' },
    { name: 'items', type: 'array' },
  ],
  Post: [
    { name: 'title', type: 'string' },
    { name: 'content', type: 'string' },
    { name: 'author', type: 'reference' },
  ],
};

/** `user-profiles` -> `UserProfile`; non-entity folders -> null. */
export function inferEntityName(featureName) {
  if (NON_ENTITY_FEATURES.has(featureName.toLowerCase())) return null;
  const words = featureName.split(/[^A-Za-z0-9]+/).filter(Boolean);
  if (words.length === 0) return null;
  const last = words[words.length - 1];
  if (/ies$/i.test(last) && last.length > 3) words[words.length - 1] = last.slice(0, -3) + 'y';
  else if (/(?:ss|us|is)$/i.test(last)) words[words.length - 1] = last;
  else if (/s$/i.test(last) && last.length > 1) words[words.length - 1] = last.slice(0, -1);
  return words.map(w => w.charAt(0).toUpperCase() + w.slice(1)).join('');
}

function inferDomain(projectStore, domainBaseIri) {
  const store = createStore();
  const seen = new Set();
  let fields = 0;

  for (const feature of readFeatures(projectStore)) {
    const name = inferEntityName(feature.name);
    if (!name || seen.has(name)) continue;
    seen.add(name);

    const entityIri = namedNode(`${domainBaseIri}${encodeURIComponent(name)}`);
    store.addQuad(entityIri, namedNode(RDF_TYPE), namedNode(DOMAIN.Entity));
    store.addQuad(entityIri, namedNode(RDFS_LABEL), literal(name));

    for (const field of [...COMMON_FIELDS, ...(SPECIFIC_FIELDS[name] ?? [])]) {
      const fieldIri = namedNode(
        `${domainBaseIri}${encodeURIComponent(name)}/${encodeURIComponent(field.name)}`
      );
      store.addQuad(entityIri, namedNode(DOMAIN.hasField), fieldIri);
      store.addQuad(fieldIri, namedNode(RDFS_LABEL), literal(field.name));
      store.addQuad(fieldIri, namedNode(DOMAIN.fieldType), literal(field.type));
      fields++;
    }
  }
  return { store, entities: seen.size, fields };
}

function templatesForEntity(name) {
  return ['component', 'form', 'list', 'service', 'schema'].map(type => ({
    name: `${name}${type.charAt(0).toUpperCase()}${type.slice(1)}`,
    type,
    entity: name,
  }));
}

function inferPatterns(templates) {
  const types = new Set(templates.map(t => t.type));
  const patterns = [];
  if (types.has('component') && types.has('form')) patterns.push('crud-ui');
  if (types.has('service') && types.has('schema')) patterns.push('api-first');
  if (types.has('list')) patterns.push('data-table');
  return patterns;
}

function deriveHooks(entities) {
  const hooks = [];
  const invariants = [];
  for (const { name } of entities) {
    hooks.push({
      name: `validate${name}`,
      type: 'validation',
      entity: name,
      trigger: 'before-insert',
    });
    hooks.push({ name: `sync${name}ToStore`, type: 'sync', entity: name, trigger: 'after-insert' });
    invariants.push({
      name: `${name}RequiredFields`,
      entity: name,
      rule: 'all-required-fields-present',
    });
  }
  return { hooks, invariants };
}

/* ------------------------------------------------------------------------- */
/* Snapshot                                                                  */
/* ------------------------------------------------------------------------- */

function hashFile(file) {
  return new Promise((resolve, reject) => {
    const hash = createHash('sha256');
    createReadStream(file)
      .on('error', reject)
      .on('data', chunk => hash.update(chunk))
      .on('end', () => resolve(hash.digest('hex')));
  });
}

/**
 * Content-hash baseline of every scanned file. The overall hash depends only on
 * paths, sizes and contents, so identical trees give identical hashes.
 */
async function createSnapshot(root, fsStore) {
  const sizes = new Map(
    fsStore
      .getQuads(null, namedNode(FS.byteSize), null)
      .map(q => [q.subject.value, Number(q.object.value)])
  );
  const files = [];
  for (const file of listFiles(fsStore)) {
    const size = sizes.get(file.iri) ?? 0;
    const contentHash =
      size <= MAX_HASHED_BYTES ? await hashFile(path.join(root, file.path)) : null;
    files.push({ path: file.path, size, contentHash });
  }
  const overall = createHash('sha256');
  for (const f of files) overall.update(`${f.path}\0${f.size}\0${f.contentHash ?? ''}\n`);
  return { hash: overall.digest('hex'), timestamp: new Date().toISOString(), files };
}

/* ------------------------------------------------------------------------- */
/* Report                                                                    */
/* ------------------------------------------------------------------------- */

function buildReport(state, phases, hooksDoc) {
  const profile = phases.stackDetection?.data?.profile ?? null;
  const frameworks = phases.stackDetection?.data?.frameworks ?? [];

  const roleByFile = new Map();
  if (state.projectStore) {
    for (const q of state.projectStore.getQuads(null, namedNode(PROJECT.roleString), null)) {
      roleByFile.set(q.subject.value, q.object.value);
    }
  }
  const files = state.projectStore ? listFiles(state.projectStore) : [];
  const filesByRole = {};
  for (const f of files) {
    const role = roleByFile.get(f.iri) ?? 'Other';
    filesByRole[role] = (filesByRole[role] ?? 0) + 1;
  }
  const roleByPath = new Map(files.map(f => [f.path, roleByFile.get(f.iri) ?? 'Other']));
  const testPaths = files
    .filter(f => roleByPath.get(f.path) === 'Test')
    .map(f => f.path.toLowerCase());

  // A feature has tests when any of its files is a Test, or any test file path mentions the feature name.
  const features = state.projectStore
    ? readFeatures(state.projectStore).map(feature => {
        const roles = {};
        for (const p of feature.files) roles[roleByPath.get(p) ?? 'Other'] = true;
        const name = feature.name.toLowerCase();
        const hasTests = roles.Test === true || testPaths.some(p => p.includes(name));
        return {
          name: feature.name,
          fileCount: feature.files.length,
          roles,
          hasMissingTests: !hasTests,
        };
      })
    : [];

  const domainEntities = state.domainStore
    ? readEntities(state.domainStore).map(({ name, fieldCount }) => ({ name, fieldCount }))
    : [];

  const testCoverageAverage =
    features.length > 0
      ? (100 * features.filter(f => !f.hasMissingTests).length) / features.length
      : undefined;

  const summary = summarize(phases);
  return {
    summary,
    stackProfile: frameworks.length > 0 ? frameworks.join(' + ') : 'unknown',
    stack: profile,
    stats: {
      frameworks,
      featureCount: features.length,
      domainEntityCount: domainEntities.length,
      totalFiles: files.length,
      filesByRole,
      ...(testCoverageAverage === undefined ? {} : { testCoverageAverage }),
    },
    features,
    domainEntities,
    templates: state.templateGraph?.templates ?? [],
    hooks: hooksDoc?.hooks ?? [],
    snapshotHash: state.snapshot?.hash ?? null,
  };
}

function summarize(phases) {
  const parts = [];
  if (phases.scan?.success)
    parts.push(`Scanned ${phases.scan.data.files} files in ${phases.scan.data.folders} folders`);
  if (phases.stackDetection?.data?.frameworks.length)
    parts.push(`Detected stack: ${phases.stackDetection.data.frameworks.join(', ')}`);
  if (phases.projectModel?.success)
    parts.push(`Found ${phases.projectModel.data.features} features`);
  if (phases.fileRoles?.success) parts.push(`Classified ${phases.fileRoles.data.classified} files`);
  if (phases.domainInference?.success)
    parts.push(
      `Inferred ${phases.domainInference.data.entities} entities with ${phases.domainInference.data.fields} fields`
    );
  if (phases.templateInference?.success)
    parts.push(`Generated ${phases.templateInference.data.templates} templates`);
  if (phases.hooks?.success) parts.push(`Registered ${phases.hooks.data.registered} hooks`);
  return parts.length > 0 ? `${parts.join('. ')}.` : 'Nothing to report.';
}

function buildFailureResult(phases, startTime, failedPhase, errorMessage) {
  return {
    success: false,
    error: `${failedPhase}: ${errorMessage}`,
    receipt: {
      phases,
      totalDuration: Date.now() - startTime,
      success: false,
      failedPhase,
      error: errorMessage,
    },
    report: { summary: `Initialization failed at phase: ${failedPhase}`, error: errorMessage },
    state: {
      fsStore: null,
      projectStore: null,
      domainStore: null,
      templateGraph: null,
      snapshot: null,
    },
    outputs: [],
  };
}
