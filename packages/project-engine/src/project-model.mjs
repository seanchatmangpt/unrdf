/**
 * @file Project model builder - derive features from the filesystem graph
 * @module project-engine/project-model
 */

import { z } from 'zod';
import { dataFactory } from '@unrdf/oxigraph';
import { RDF_TYPE, RDFS_LABEL, FS, PROJECT } from './namespaces.mjs';

const { namedNode, literal } = dataFactory;

const ConventionsSchema = z.object({
  sourcePaths: z.array(z.string()).default(['src']),
  featurePaths: z.array(z.string()).default(['features', 'modules']),
  testPaths: z.array(z.string()).default(['__tests__', 'test', 'tests', 'spec']),
});

const ProjectModelOptionsSchema = z.object({
  fsStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'fsStore must be an RDF store with getQuads method',
  }),
  baseIri: z.string().default('http://example.org/unrdf/project#'),
  fsBaseIri: z.string().default('http://example.org/unrdf/fs#'),
  conventions: ConventionsSchema.optional(),
});

/**
 * Add the project model (Project, Feature, belongsToFeature) to a filesystem graph.
 * The store is extended in place and returned.
 *
 * A feature is a directory directly under a source or feature root
 * (`src/<feature>/...`, `features/<feature>/...`); files that sit directly in the
 * root (`src/index.mjs`) are not features.
 *
 * @param {Object} options
 * @param {Object} options.fsStore - Store produced by scanFileSystemToStore
 * @param {string} [options.baseIri] - Base IRI for project resources
 * @param {string} [options.fsBaseIri] - Base IRI the scanner used for file resources
 * @param {Object} [options.conventions] - Directory conventions
 * @returns {Object} The same store, extended
 */
export function buildProjectModelFromFs(options) {
  const {
    fsStore: store,
    baseIri,
    fsBaseIri,
    conventions,
  } = ProjectModelOptionsSchema.parse(options);
  const projectIri = namedNode(`${baseIri}project`);

  store.addQuad(projectIri, namedNode(RDF_TYPE), namedNode(PROJECT.Project));

  const features = extractFeatures(store, conventions ?? ConventionsSchema.parse({}));

  for (const [featureName, files] of [...features.entries()].sort()) {
    const featureIri = namedNode(`${baseIri}feature/${encodeURIComponent(featureName)}`);
    store.addQuad(projectIri, namedNode(PROJECT.hasFeature), featureIri);
    store.addQuad(featureIri, namedNode(RDF_TYPE), namedNode(PROJECT.Feature));
    store.addQuad(featureIri, namedNode(RDFS_LABEL), literal(featureName));

    for (const filePath of files) {
      const fileIri = namedNode(`${fsBaseIri}${encodeURIComponent(filePath)}`);
      store.addQuad(fileIri, namedNode(PROJECT.belongsToFeature), featureIri);
    }
  }

  return store;
}

/**
 * Group files by feature directory.
 * @returns {Map<string, string[]>} feature name -> file paths
 */
function extractFeatures(store, conventions) {
  const roots = [...conventions.sourcePaths, ...conventions.featurePaths];
  const features = new Map();

  const filePaths = store
    .getQuads(null, namedNode(FS.relativePath), null)
    .map(quad => quad.object.value);

  for (const filePath of filePaths) {
    for (const root of roots) {
      const prefix = `${root}/`;
      if (!filePath.startsWith(prefix)) continue;
      const rest = filePath.slice(prefix.length);
      const slash = rest.indexOf('/');
      if (slash <= 0) continue; // file directly in root, or the root itself
      const featureName = rest.slice(0, slash);
      if (!features.has(featureName)) features.set(featureName, []);
      features.get(featureName).push(filePath);
      break;
    }
  }

  // Directories also carry relativePath; keep only paths that are files (have an extension quad or byteSize)
  const fileSet = new Set(
    store.getQuads(null, namedNode(FS.byteSize), null).map(quad => quad.subject.value)
  );
  const fileIriByPath = new Map(
    store
      .getQuads(null, namedNode(FS.relativePath), null)
      .filter(quad => fileSet.has(quad.subject.value))
      .map(quad => [quad.object.value, quad.subject.value])
  );
  for (const [name, paths] of features) {
    const onlyFiles = paths.filter(p => fileIriByPath.has(p));
    if (onlyFiles.length === 0) features.delete(name);
    else features.set(name, onlyFiles);
  }

  return features;
}
