/**
 * @file Project initialization pipeline - orchestrates all 8 capabilities
 * @module project-engine/initialize
 */

import { z } from 'zod'
import { createHash } from 'crypto'
import { Store, DataFactory } from 'n3'
import { scanFileSystemToStore } from './fs-scan.mjs'
import { detectStackFromFs } from './stack-detect.mjs'
import { buildProjectModelFromFs } from './project-model.mjs'
import { classifyFiles } from './file-roles.mjs'

const { namedNode, literal } = DataFactory

/**
 * @typedef {Object} PhaseReceipt
 * @property {number} duration - Phase duration in ms
 * @property {boolean} success - Phase success status
 * @property {string} [error] - Error message if failed
 * @property {Object} [data] - Phase-specific data
 */

/**
 * @typedef {Object} InitializationReceipt
 * @property {Object} phases - Receipt for each phase
 * @property {number} totalDuration - Total duration in ms
 * @property {boolean} success - Overall success
 */

/**
 * @typedef {Object} InitializationState
 * @property {Store} fsStore - Filesystem RDF store
 * @property {Store} projectStore - Project model store
 * @property {Store} domainStore - Domain inference store
 * @property {Object} templateGraph - Template inference graph
 * @property {Object} snapshot - Baseline snapshot
 */

/**
 * @typedef {Object} InitializationResult
 * @property {boolean} success
 * @property {InitializationReceipt} receipt
 * @property {Object} report
 * @property {InitializationState} state
 */

const InitializeOptionsSchema = z.object({
  ignorePatterns: z.array(z.string()).optional(),
  baseIri: z.string().default('http://example.org/unrdf/'),
  conventions: z.object({
    sourcePaths: z.array(z.string()).default(['src']),
    featurePaths: z.array(z.string()).default(['features', 'modules']),
    testPaths: z.array(z.string()).default(['__tests__', 'test', 'tests', 'spec']),
  }).optional(),
  skipPhases: z.array(z.string()).optional(),
})

/**
 * Create and execute the project initialization pipeline
 *
 * @param {string} projectRoot - Root directory of the project
 * @param {Object} [options] - Pipeline options
 * @param {string[]} [options.ignorePatterns] - Patterns to ignore during scan
 * @param {string} [options.baseIri] - Base IRI for RDF resources
 * @param {Object} [options.conventions] - Naming conventions
 * @param {string[]} [options.skipPhases] - Phases to skip
 * @returns {Promise<InitializationResult>}
 */
export async function createProjectInitializationPipeline(projectRoot, options = {}) {
  const validated = InitializeOptionsSchema.parse(options)
  const startTime = Date.now()

  const phases = {}
  const state = {
    fsStore: null,
    projectStore: null,
    domainStore: null,
    templateGraph: null,
    snapshot: null,
  }

  const skipPhases = new Set(validated.skipPhases || [])

  // Phase 1: Scan FS
  if (!skipPhases.has('scan')) {
    phases.scan = await executeScanPhase(projectRoot, validated)
    if (!phases.scan.success) {
      return buildFailureResult(phases, startTime, 'scan', phases.scan.error)
    }
    state.fsStore = phases.scan.data.store
  }

  // Phase 2: Detect stack
  if (!skipPhases.has('stackDetection') && state.fsStore) {
    phases.stackDetection = executeStackDetectionPhase(state.fsStore, validated)
    if (!phases.stackDetection.success) {
      return buildFailureResult(phases, startTime, 'stackDetection', phases.stackDetection.error)
    }
  }

  // Phase 3: Build project model
  if (!skipPhases.has('projectModel') && state.fsStore) {
    phases.projectModel = executeProjectModelPhase(state.fsStore, validated)
    if (!phases.projectModel.success) {
      return buildFailureResult(phases, startTime, 'projectModel', phases.projectModel.error)
    }
    state.projectStore = phases.projectModel.data.store
  }

  // Phase 4: Classify file roles
  if (!skipPhases.has('fileRoles') && state.projectStore) {
    phases.fileRoles = executeFileRolesPhase(state.projectStore, phases.stackDetection?.data?.profile, validated)
    if (!phases.fileRoles.success) {
      return buildFailureResult(phases, startTime, 'fileRoles', phases.fileRoles.error)
    }
  }

  // Phase 5: Infer domain model
  if (!skipPhases.has('domainInference') && state.projectStore) {
    phases.domainInference = executeDomainInferencePhase(state.projectStore, validated)
    if (!phases.domainInference.success) {
      return buildFailureResult(phases, startTime, 'domainInference', phases.domainInference.error)
    }
    state.domainStore = phases.domainInference.data.store
  }

  // Phase 6: Infer templates
  if (!skipPhases.has('templateInference') && state.domainStore) {
    phases.templateInference = executeTemplateInferencePhase(state.domainStore, validated)
    if (!phases.templateInference.success) {
      return buildFailureResult(phases, startTime, 'templateInference', phases.templateInference.error)
    }
    state.templateGraph = phases.templateInference.data.templateGraph
  }

  // Phase 7: Create baseline snapshot
  if (!skipPhases.has('snapshot')) {
    phases.snapshot = executeSnapshotPhase(state, validated)
    if (!phases.snapshot.success) {
      return buildFailureResult(phases, startTime, 'snapshot', phases.snapshot.error)
    }
    state.snapshot = phases.snapshot.data.snapshot
  }

  // Phase 8: Derive and register hooks
  if (!skipPhases.has('hooks') && state.domainStore) {
    phases.hooks = executeHooksPhase(state, validated)
    if (!phases.hooks.success) {
      return buildFailureResult(phases, startTime, 'hooks', phases.hooks.error)
    }
  }

  // Phase 9: Generate report
  phases.report = generateReportPhase(state, phases, validated)

  const totalDuration = Date.now() - startTime

  return {
    success: true,
    receipt: {
      phases,
      totalDuration,
      success: true,
    },
    report: phases.report.data.report,
    state,
  }
}

/**
 * Execute filesystem scan phase
 *
 * @private
 * @param {string} projectRoot
 * @param {Object} options
 * @returns {Promise<PhaseReceipt>}
 */
async function executeScanPhase(projectRoot, options) {
  const startTime = Date.now()

  try {
    const { store, summary } = await scanFileSystemToStore({
      root: projectRoot,
      ignorePatterns: options.ignorePatterns,
      baseIri: `${options.baseIri}fs#`,
    })

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        store,
        files: summary.fileCount,
        folders: summary.folderCount,
        ignored: summary.ignoredCount,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Execute stack detection phase
 *
 * @private
 * @param {Store} fsStore
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function executeStackDetectionPhase(fsStore, options) {
  const startTime = Date.now()

  try {
    const profile = detectStackFromFs({
      fsStore,
      projectIri: `${options.baseIri}project#project`,
    })

    const frameworks = []
    if (profile.uiFramework) frameworks.push(profile.uiFramework)
    if (profile.webFramework) frameworks.push(profile.webFramework)
    if (profile.apiFramework && profile.apiFramework !== profile.webFramework) {
      frameworks.push(profile.apiFramework)
    }
    if (profile.testFramework) frameworks.push(profile.testFramework)

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        profile,
        frameworks,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Execute project model building phase
 *
 * @private
 * @param {Store} fsStore
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function executeProjectModelPhase(fsStore, options) {
  const startTime = Date.now()

  try {
    const store = buildProjectModelFromFs({
      fsStore,
      baseIri: `${options.baseIri}project#`,
      conventions: options.conventions,
    })

    // Count features
    const featureQuads = store.getQuads(
      null,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/unrdf/project#Feature')
    )

    // Count files
    const fileQuads = store.getQuads(
      null,
      namedNode('http://example.org/unrdf/filesystem#relativePath'),
      null
    )

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        store,
        features: featureQuads.length,
        files: fileQuads.length,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Execute file roles classification phase
 *
 * @private
 * @param {Store} projectStore
 * @param {Object} stackProfile
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function executeFileRolesPhase(projectStore, stackProfile, options) {
  const startTime = Date.now()

  try {
    const store = classifyFiles({
      fsStore: projectStore,
      stackInfo: stackProfile,
      baseIri: `${options.baseIri}project#`,
    })

    // Count classified files
    const classifiedQuads = store.getQuads(
      null,
      namedNode('http://example.org/unrdf/project#roleString'),
      null
    )

    // Count total files
    const totalQuads = store.getQuads(
      null,
      namedNode('http://example.org/unrdf/filesystem#relativePath'),
      null
    )

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        classified: classifiedQuads.length,
        unclassified: totalQuads.length - classifiedQuads.length,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Execute domain inference phase
 *
 * @private
 * @param {Store} projectStore
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function executeDomainInferencePhase(projectStore, options) {
  const startTime = Date.now()

  try {
    // Create domain store from project store
    const domainStore = new Store()
    const baseIri = `${options.baseIri}domain#`

    // Extract entities from feature names and file patterns
    const featureQuads = projectStore.getQuads(
      null,
      namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      null
    )

    const entities = new Map()

    for (const quad of featureQuads) {
      const featureName = quad.object.value
      const entityName = inferEntityFromFeatureName(featureName)

      if (entityName && !entities.has(entityName)) {
        entities.set(entityName, { fields: [], source: featureName })

        const entityIri = namedNode(`${baseIri}${encodeURIComponent(entityName)}`)
        domainStore.addQuad(
          entityIri,
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/unrdf/domain#Entity')
        )
        domainStore.addQuad(
          entityIri,
          namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
          literal(entityName)
        )
      }
    }

    // Infer common fields for entities
    let totalFields = 0
    for (const [entityName, entity] of entities) {
      const fields = inferEntityFields(entityName)
      totalFields += fields.length

      const entityIri = namedNode(`${baseIri}${encodeURIComponent(entityName)}`)
      for (const field of fields) {
        const fieldIri = namedNode(`${baseIri}${encodeURIComponent(entityName)}/${encodeURIComponent(field.name)}`)
        domainStore.addQuad(
          entityIri,
          namedNode('http://example.org/unrdf/domain#hasField'),
          fieldIri
        )
        domainStore.addQuad(
          fieldIri,
          namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
          literal(field.name)
        )
        domainStore.addQuad(
          fieldIri,
          namedNode('http://example.org/unrdf/domain#fieldType'),
          literal(field.type)
        )
      }
    }

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        store: domainStore,
        entities: entities.size,
        fields: totalFields,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Execute template inference phase
 *
 * @private
 * @param {Store} domainStore
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function executeTemplateInferencePhase(domainStore, options) {
  const startTime = Date.now()

  try {
    const templateGraph = {
      templates: [],
      patterns: [],
    }

    // Infer templates from entities
    const entityQuads = domainStore.getQuads(
      null,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/unrdf/domain#Entity')
    )

    for (const quad of entityQuads) {
      const entityIri = quad.subject.value
      const labelQuads = domainStore.getQuads(
        quad.subject,
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        null
      )
      const entityName = labelQuads[0]?.object.value || 'Unknown'

      // Infer templates for each entity
      const templates = inferTemplatesForEntity(entityName)
      templateGraph.templates.push(...templates)
    }

    // Infer patterns from template collection
    const patterns = inferPatternsFromTemplates(templateGraph.templates)
    templateGraph.patterns = patterns

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        templateGraph,
        templates: templateGraph.templates.length,
        patterns: templateGraph.patterns,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Execute snapshot phase
 *
 * @private
 * @param {InitializationState} state
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function executeSnapshotPhase(state, options) {
  const startTime = Date.now()

  try {
    const snapshot = {
      hash: createSnapshotHash(state),
      timestamp: new Date().toISOString(),
      stores: {
        fsStoreSize: state.fsStore?.size || 0,
        projectStoreSize: state.projectStore?.size || 0,
        domainStoreSize: state.domainStore?.size || 0,
      },
      templateCount: state.templateGraph?.templates?.length || 0,
    }

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        snapshot,
        hash: snapshot.hash,
        timestamp: snapshot.timestamp,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Execute hooks registration phase
 *
 * @private
 * @param {InitializationState} state
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function executeHooksPhase(state, options) {
  const startTime = Date.now()

  try {
    const hooks = []
    const invariants = []

    // Derive hooks from domain entities
    const entityQuads = state.domainStore.getQuads(
      null,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/unrdf/domain#Entity')
    )

    for (const quad of entityQuads) {
      const labelQuads = state.domainStore.getQuads(
        quad.subject,
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        null
      )
      const entityName = labelQuads[0]?.object.value || 'Unknown'

      // Register validation hooks
      hooks.push({
        name: `validate${entityName}`,
        type: 'validation',
        entity: entityName,
        trigger: 'pre-commit',
      })

      // Register sync hooks
      hooks.push({
        name: `sync${entityName}ToStore`,
        type: 'sync',
        entity: entityName,
        trigger: 'post-save',
      })

      // Register invariants
      invariants.push({
        name: `${entityName}RequiredFields`,
        entity: entityName,
        rule: 'all-required-fields-present',
      })
    }

    return {
      duration: Date.now() - startTime,
      success: true,
      data: {
        registered: hooks.length,
        invariants,
        hooks,
      },
    }
  } catch (error) {
    return {
      duration: Date.now() - startTime,
      success: false,
      error: error.message,
    }
  }
}

/**
 * Generate final report phase
 *
 * @private
 * @param {InitializationState} state
 * @param {Object} phases
 * @param {Object} options
 * @returns {PhaseReceipt}
 */
function generateReportPhase(state, phases, options) {
  const startTime = Date.now()

  const report = {
    summary: generateSummary(phases),
    features: extractFeaturesList(state.projectStore),
    stack: phases.stackDetection?.data?.profile || null,
    entities: extractEntitiesList(state.domainStore),
    templates: state.templateGraph?.templates || [],
    hooks: phases.hooks?.data?.hooks || [],
  }

  return {
    duration: Date.now() - startTime,
    success: true,
    data: {
      report,
    },
  }
}

/**
 * Build failure result
 *
 * @private
 */
function buildFailureResult(phases, startTime, failedPhase, errorMessage) {
  return {
    success: false,
    receipt: {
      phases,
      totalDuration: Date.now() - startTime,
      success: false,
      failedPhase,
      error: errorMessage,
    },
    report: {
      summary: `Initialization failed at phase: ${failedPhase}`,
      error: errorMessage,
    },
    state: {
      fsStore: null,
      projectStore: null,
      domainStore: null,
      templateGraph: null,
      snapshot: null,
    },
  }
}

/**
 * Infer entity name from feature name
 *
 * @private
 */
function inferEntityFromFeatureName(featureName) {
  // Skip common non-entity folders
  const nonEntities = ['utils', 'helpers', 'lib', 'shared', 'common', 'config', 'hooks', 'types']
  if (nonEntities.includes(featureName.toLowerCase())) {
    return null
  }

  // Singularize common patterns
  let entity = featureName
  if (entity.endsWith('s') && !entity.endsWith('ss')) {
    entity = entity.slice(0, -1)
  }

  // PascalCase
  return entity.charAt(0).toUpperCase() + entity.slice(1)
}

/**
 * Infer common fields for an entity
 *
 * @private
 */
function inferEntityFields(entityName) {
  // Common fields for all entities
  const commonFields = [
    { name: 'id', type: 'string' },
    { name: 'createdAt', type: 'datetime' },
    { name: 'updatedAt', type: 'datetime' },
  ]

  // Entity-specific fields
  const specificFields = {
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
  }

  return [...commonFields, ...(specificFields[entityName] || [])]
}

/**
 * Infer templates for an entity
 *
 * @private
 */
function inferTemplatesForEntity(entityName) {
  return [
    { name: `${entityName}Component`, type: 'component', entity: entityName },
    { name: `${entityName}Form`, type: 'form', entity: entityName },
    { name: `${entityName}List`, type: 'list', entity: entityName },
    { name: `${entityName}Service`, type: 'service', entity: entityName },
    { name: `${entityName}Schema`, type: 'schema', entity: entityName },
  ]
}

/**
 * Infer patterns from templates
 *
 * @private
 */
function inferPatternsFromTemplates(templates) {
  const patterns = []
  const templateTypes = new Set(templates.map((t) => t.type))

  if (templateTypes.has('component') && templateTypes.has('form')) {
    patterns.push('crud-ui')
  }
  if (templateTypes.has('service') && templateTypes.has('schema')) {
    patterns.push('api-first')
  }
  if (templateTypes.has('list')) {
    patterns.push('data-table')
  }

  return patterns
}

/**
 * Create hash for snapshot
 *
 * @private
 */
function createSnapshotHash(state) {
  const hash = createHash('sha256')

  if (state.fsStore) hash.update(String(state.fsStore.size))
  if (state.projectStore) hash.update(String(state.projectStore.size))
  if (state.domainStore) hash.update(String(state.domainStore.size))
  if (state.templateGraph) hash.update(String(state.templateGraph.templates?.length || 0))

  hash.update(new Date().toISOString())

  return hash.digest('hex').substring(0, 16)
}

/**
 * Generate summary from phases
 *
 * @private
 */
function generateSummary(phases) {
  const parts = []

  if (phases.scan?.success) {
    parts.push(`Scanned ${phases.scan.data.files} files in ${phases.scan.data.folders} folders`)
  }
  if (phases.stackDetection?.success) {
    const frameworks = phases.stackDetection.data.frameworks
    if (frameworks.length > 0) {
      parts.push(`Detected stack: ${frameworks.join(', ')}`)
    }
  }
  if (phases.projectModel?.success) {
    parts.push(`Found ${phases.projectModel.data.features} features`)
  }
  if (phases.fileRoles?.success) {
    parts.push(`Classified ${phases.fileRoles.data.classified} files`)
  }
  if (phases.domainInference?.success) {
    parts.push(`Inferred ${phases.domainInference.data.entities} entities with ${phases.domainInference.data.fields} fields`)
  }
  if (phases.templateInference?.success) {
    parts.push(`Generated ${phases.templateInference.data.templates} templates`)
  }
  if (phases.hooks?.success) {
    parts.push(`Registered ${phases.hooks.data.registered} hooks`)
  }

  return parts.join('. ') + '.'
}

/**
 * Extract features list from project store
 *
 * @private
 */
function extractFeaturesList(projectStore) {
  if (!projectStore) return []

  const features = []
  const labelQuads = projectStore.getQuads(
    null,
    namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
    null
  )

  for (const quad of labelQuads) {
    // Check if this is a feature
    const typeQuads = projectStore.getQuads(
      quad.subject,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/unrdf/project#Feature')
    )

    if (typeQuads.length > 0) {
      features.push(quad.object.value)
    }
  }

  return features
}

/**
 * Extract entities list from domain store
 *
 * @private
 */
function extractEntitiesList(domainStore) {
  if (!domainStore) return []

  const entities = []
  const entityQuads = domainStore.getQuads(
    null,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/domain#Entity')
  )

  for (const quad of entityQuads) {
    const labelQuads = domainStore.getQuads(
      quad.subject,
      namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      null
    )

    if (labelQuads.length > 0) {
      entities.push(labelQuads[0].object.value)
    }
  }

  return entities
}
