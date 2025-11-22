/**
 * @file Project model builder - convert FS graph to project structure
 * @module project-engine/project-model
 */

import { DataFactory } from 'n3'
import { z } from 'zod'

const { namedNode, literal } = DataFactory

const ProjectModelOptionsSchema = z.object({
  fsStore: z.object({}).passthrough(),
  baseIri: z.string().default('http://example.org/unrdf/project#'),
  conventions: z.object({
    sourcePaths: z.array(z.string()).default(['src']),
    featurePaths: z.array(z.string()).default(['features', 'modules']),
    testPaths: z.array(z.string()).default(['__tests__', 'test', 'tests', 'spec']),
  }).optional(),
})

/**
 * Build project model from filesystem graph
 *
 * @param {Object} options
 * @param {Store} options.fsStore - FS store from scanFileSystemToStore
 * @param {string} [options.baseIri] - Base IRI for project resources
 * @param {Object} [options.conventions] - Naming conventions
 * @returns {Store} Enhanced store with project structure
 */
export function buildProjectModelFromFs(options) {
  const validated = ProjectModelOptionsSchema.parse(options)
  const { fsStore, baseIri, conventions } = validated

  const store = fsStore
  const projectIri = namedNode(`${baseIri}project`)

  store.addQuad(
    projectIri,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/project#Project')
  )

  const features = extractFeatures(store, baseIri, conventions)

  for (const [featureName, feature] of Object.entries(features)) {
    const featureIri = namedNode(`${baseIri}feature/${encodeURIComponent(featureName)}`)

    store.addQuad(
      projectIri,
      namedNode('http://example.org/unrdf/project#hasFeature'),
      featureIri
    )
    store.addQuad(
      featureIri,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/unrdf/project#Feature')
    )
    store.addQuad(
      featureIri,
      namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      literal(featureName)
    )

    for (const filePath of feature.files) {
      const fileIri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(filePath)}`)
      store.addQuad(
        fileIri,
        namedNode('http://example.org/unrdf/project#belongsToFeature'),
        featureIri
      )
    }
  }

  return store
}

/**
 * Extract features from directory structure
 *
 * @private
 */
function extractFeatures(store, baseIri, conventions) {
  const features = {}
  const sourcePaths = conventions?.sourcePaths || ['src']

  const fileQuads = store.getQuads(
    null,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    null
  )

  for (const quad of fileQuads) {
    const filePath = quad.object.value
    const inSourcePath = sourcePaths.some((srcPath) => filePath.startsWith(srcPath + '/') || filePath === srcPath)

    if (inSourcePath) {
      const match = filePath.match(/^src\/([^/]+)/)
      if (match) {
        const featureName = match[1]
        if (!features[featureName]) {
          features[featureName] = { files: [] }
        }
        features[featureName].files.push(filePath)
      }
    }
  }

  return features
}
