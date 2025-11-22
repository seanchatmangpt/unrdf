/**
 * @file File role classification
 * @module project-engine/file-roles
 */

import { DataFactory } from 'n3'
import { z } from 'zod'

const { namedNode, literal } = DataFactory

const FileRolesOptionsSchema = z.object({
  fsStore: z.custom((val) => val && typeof val.getQuads === 'function', {
    message: 'fsStore must be an RDF store with getQuads method',
  }),
  stackInfo: z.object({
    uiFramework: z.string().nullable(),
    webFramework: z.string().nullable(),
    testFramework: z.string().nullable(),
  }).optional(),
  baseIri: z.string().default('http://example.org/unrdf/project#'),
})

const ROLE_PATTERNS = {
  Component: [/\.(tsx?|jsx?)$/, /(Component|View)\.tsx?$/],
  Page: [/^(pages|src\/pages|src\/app|app)\//, /page\.(tsx?|jsx?)$/],
  Api: [/(api|server|route)\.(tsx?|js)$/, /^(api|server|routes)\//],
  Hook: [/use[A-Z]\w+\.(tsx?|jsx?)$/, /^hooks?\//],
  Service: [/(service|client|repository)\.(tsx?|js)$/, /^(services?|clients?)\//],
  Schema: [/(schema|type|interface)\.(tsx?|js)$/, /(schema|types?|interfaces?)\.(tsx?|json)$/],
  State: [/(store|state|reducer|context)\.(tsx?|js)$/, /^(store|state|redux)\//],
  Test: [/\.(test|spec)\.(tsx?|jsx?)$/, /^(__tests__|test|tests|spec)\//],
  Doc: [/\.(md|mdx)$/, /^(docs?|README)/],
  Config: [/\.(json|yaml|yml|toml|conf)$/, /(package\.json|tsconfig|eslintrc|prettier)/],
  Build: [/(build|webpack|rollup|esbuild|vite|next\.config)/, /^scripts\//],
  Other: [],
}

/**
 * Classify files with semantic roles
 *
 * @param {Object} options
 * @param {Store} options.fsStore - FS store with project model
 * @param {Object} [options.stackInfo] - Stack information from detectStackFromFs
 * @param {string} [options.baseIri] - Base IRI for roles
 * @returns {Store} Store with role classifications
 */
export function classifyFiles(options) {
  const validated = FileRolesOptionsSchema.parse(options)
  const { fsStore, baseIri } = validated

  const store = fsStore
  let roleCount = 0

  const fileQuads = store.getQuads(
    null,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    null
  )

  for (const quad of fileQuads) {
    const filePath = quad.object.value
    const fileIri = quad.subject
    const role = classifyPath(filePath)

    if (role && role !== 'Other') {
      const roleIri = namedNode(`${baseIri}${role}`)
      store.addQuad(fileIri, namedNode('http://example.org/unrdf/project#hasRole'), roleIri)
      store.addQuad(fileIri, namedNode('http://example.org/unrdf/project#roleString'), literal(role))
      roleCount++
    }
  }

  return store
}

/**
 * Classify a file path to determine its role
 *
 * @private
 */
function classifyPath(filePath) {
  for (const [role, patterns] of Object.entries(ROLE_PATTERNS)) {
    if (patterns.some((pattern) => pattern.test(filePath))) {
      return role
    }
  }
  return 'Other'
}
