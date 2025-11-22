/**
 * @file Policy derivation from observed project patterns
 * @module project-engine/policy-derivation
 *
 * @description
 * Automatically derives transaction hook policies from observed project patterns.
 * Analyzes the project store and stack profile to detect invariant violations
 * and generates hooks that enforce these patterns on future transactions.
 */

import { DataFactory } from 'n3'
import { z } from 'zod'

const { namedNode } = DataFactory

// ============================================================================
// Zod Schemas
// ============================================================================

const StackProfileSchema = z.object({
  uiFramework: z.string().nullable().optional(),
  webFramework: z.string().nullable().optional(),
  apiFramework: z.string().nullable().optional(),
  testFramework: z.string().nullable().optional(),
  packageManager: z.string().nullable().optional(),
})

const DeriveOptionsSchema = z.object({
  enableFeatureViewPolicy: z.boolean().default(true),
  enableApiTestPolicy: z.boolean().default(true),
  enableOrphanFilePolicy: z.boolean().default(true),
  enableTestCompanionPolicy: z.boolean().default(true),
  enableStackPolicies: z.boolean().default(true),
  strictMode: z.boolean().default(false),
})

const HookConditionSchema = z.object({
  kind: z.enum(['sparql-ask', 'sparql-select', 'shacl', 'delta', 'threshold', 'count']),
  ref: z.object({
    uri: z.string(),
    sha256: z.string().optional(),
    mediaType: z.string().optional(),
  }).optional(),
  query: z.string().optional(),
  spec: z.any().optional(),
})

/**
 * @typedef {Object} DerivedHook
 * @property {Object} meta - Hook metadata
 * @property {string} meta.name - Unique hook name
 * @property {string} [meta.description] - Hook description
 * @property {string[]} [meta.ontology] - Related ontologies
 * @property {Object} [channel] - Observation channel
 * @property {Object} when - Trigger condition
 * @property {Function} run - Main execution body
 * @property {Function} [before] - Pre-condition gate
 * @property {Function} [after] - Post-execution step
 * @property {Object} [determinism] - Determinism config
 * @property {Object} [receipt] - Receipt config
 */

/**
 * @typedef {Object} PatternViolation
 * @property {string} type - Violation type
 * @property {string} subject - Subject IRI
 * @property {string} message - Human-readable message
 * @property {string} [relatedEntity] - Related entity IRI
 */

// ============================================================================
// Pattern Detection
// ============================================================================

/**
 * Detect features without views
 * @param {Object} store - N3 Store
 * @returns {PatternViolation[]}
 */
function detectFeaturesWithoutViews(store) {
  const violations = []
  const projectNs = 'http://example.org/unrdf/project#'
  const rdfType = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'

  // Find all features
  const featureQuads = store.getQuads(
    null,
    namedNode(rdfType),
    namedNode(`${projectNs}Feature`)
  )

  for (const featureQuad of featureQuads) {
    const featureIri = featureQuad.subject.value

    // Check if feature has any file with Component or Page role
    const roleQuads = store.getQuads(
      null,
      namedNode(`${projectNs}belongsToFeature`),
      namedNode(featureIri)
    )

    let hasView = false
    for (const roleQuad of roleQuads) {
      const fileIri = roleQuad.subject
      const fileRoles = store.getQuads(
        fileIri,
        namedNode(`${projectNs}roleString`),
        null
      )

      for (const fileRole of fileRoles) {
        if (['Component', 'Page'].includes(fileRole.object.value)) {
          hasView = true
          break
        }
      }
      if (hasView) break
    }

    if (!hasView) {
      violations.push({
        type: 'feature-without-view',
        subject: featureIri,
        message: `Feature "${featureIri}" has no Component or Page view`,
      })
    }
  }

  return violations
}

/**
 * Detect features with API but no test
 * @param {Object} store - N3 Store
 * @returns {PatternViolation[]}
 */
function detectApiWithoutTest(store) {
  const violations = []
  const projectNs = 'http://example.org/unrdf/project#'
  const rdfType = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'

  const featureQuads = store.getQuads(
    null,
    namedNode(rdfType),
    namedNode(`${projectNs}Feature`)
  )

  for (const featureQuad of featureQuads) {
    const featureIri = featureQuad.subject.value

    const roleQuads = store.getQuads(
      null,
      namedNode(`${projectNs}belongsToFeature`),
      namedNode(featureIri)
    )

    let hasApi = false
    let hasTest = false

    for (const roleQuad of roleQuads) {
      const fileIri = roleQuad.subject
      const fileRoles = store.getQuads(
        fileIri,
        namedNode(`${projectNs}roleString`),
        null
      )

      for (const fileRole of fileRoles) {
        if (fileRole.object.value === 'Api') hasApi = true
        if (fileRole.object.value === 'Test') hasTest = true
      }
    }

    if (hasApi && !hasTest) {
      violations.push({
        type: 'api-without-test',
        subject: featureIri,
        message: `Feature "${featureIri}" has API but no test`,
      })
    }
  }

  return violations
}

/**
 * Detect orphan files (files under features/* without Feature association)
 * @param {Object} store - N3 Store
 * @returns {PatternViolation[]}
 */
function detectOrphanFiles(store) {
  const violations = []
  const projectNs = 'http://example.org/unrdf/project#'
  const fsNs = 'http://example.org/unrdf/filesystem#'

  const fileQuads = store.getQuads(
    null,
    namedNode(`${fsNs}relativePath`),
    null
  )

  for (const fileQuad of fileQuads) {
    const filePath = fileQuad.object.value
    const fileIri = fileQuad.subject

    // Check if file is under features/ directory
    if (filePath.match(/^src\/features\/[^/]+\//)) {
      // Check if file belongs to a feature
      const belongsQuads = store.getQuads(
        fileIri,
        namedNode(`${projectNs}belongsToFeature`),
        null
      )

      if (belongsQuads.length === 0) {
        violations.push({
          type: 'orphan-file',
          subject: fileIri.value,
          message: `File "${filePath}" is under features/ but has no Feature association`,
          relatedEntity: filePath,
        })
      }
    }
  }

  return violations
}

/**
 * Detect main files without test companions
 * @param {Object} store - N3 Store
 * @returns {PatternViolation[]}
 */
function detectFilesWithoutTests(store) {
  const violations = []
  const projectNs = 'http://example.org/unrdf/project#'
  const fsNs = 'http://example.org/unrdf/filesystem#'

  const fileQuads = store.getQuads(
    null,
    namedNode(`${fsNs}relativePath`),
    null
  )

  const filePaths = new Set()
  const testPaths = new Set()

  for (const fileQuad of fileQuads) {
    const filePath = fileQuad.object.value
    filePaths.add(filePath)

    if (filePath.match(/\.(test|spec)\.(tsx?|jsx?|mjs)$/)) {
      testPaths.add(filePath)
    }
  }

  for (const filePath of filePaths) {
    // Skip test files, config, docs
    if (filePath.match(/\.(test|spec|config|d)\.(tsx?|jsx?|mjs)$/)) continue
    if (filePath.match(/\.(md|json|yaml|yml)$/)) continue
    if (!filePath.match(/\.(tsx?|jsx?|mjs)$/)) continue

    // Only check src files
    if (!filePath.startsWith('src/')) continue

    // Check for corresponding test file
    const baseName = filePath.replace(/\.(tsx?|jsx?|mjs)$/, '')
    const possibleTests = [
      `${baseName}.test.ts`,
      `${baseName}.test.tsx`,
      `${baseName}.test.js`,
      `${baseName}.test.jsx`,
      `${baseName}.test.mjs`,
      `${baseName}.spec.ts`,
      `${baseName}.spec.tsx`,
      `${baseName}.spec.js`,
      `${baseName}.spec.jsx`,
      `${baseName}.spec.mjs`,
    ]

    const hasTest = possibleTests.some((t) => testPaths.has(t))

    if (!hasTest) {
      violations.push({
        type: 'file-without-test',
        subject: filePath,
        message: `Source file "${filePath}" has no corresponding test file`,
      })
    }
  }

  return violations
}

/**
 * Detect Next.js app router violations
 * @param {Object} store - N3 Store
 * @param {Object} stackProfile - Stack profile
 * @returns {PatternViolation[]}
 */
function detectNextAppRouterViolations(store, stackProfile) {
  const violations = []

  if (stackProfile.webFramework !== 'next-app-router') {
    return violations
  }

  const fsNs = 'http://example.org/unrdf/filesystem#'

  const fileQuads = store.getQuads(
    null,
    namedNode(`${fsNs}relativePath`),
    null
  )

  const filePaths = new Set()
  for (const fileQuad of fileQuads) {
    filePaths.add(fileQuad.object.value)
  }

  // Find page.tsx files and check for route.ts
  for (const filePath of filePaths) {
    if (filePath.match(/app\/.*\/page\.(tsx?|jsx?)$/)) {
      const routeDir = filePath.replace(/page\.(tsx?|jsx?)$/, '')
      const routeFile = `${routeDir}route.ts`
      const routeFileTsx = `${routeDir}route.tsx`

      // Note: route.ts is optional, but if it's an API route it should exist
      // This is a soft check - only flag if directory looks like API route
      if (routeDir.match(/\/api\//)) {
        if (!filePaths.has(routeFile) && !filePaths.has(routeFileTsx)) {
          violations.push({
            type: 'next-app-router-missing-route',
            subject: filePath,
            message: `API route "${routeDir}" has page.tsx but no route.ts`,
            relatedEntity: routeFile,
          })
        }
      }
    }
  }

  return violations
}

// ============================================================================
// Hook Generation
// ============================================================================

/**
 * Create hook for feature-must-have-view invariant
 * @param {PatternViolation[]} violations - Detected violations
 * @returns {DerivedHook}
 */
function createFeatureViewHook(violations) {
  return {
    meta: {
      name: 'derived:feature-must-have-view',
      description: 'Every Feature must have at least one Component or Page view',
      ontology: ['unrdf-project'],
    },
    channel: {
      graphs: ['urn:graph:project'],
      view: 'after',
    },
    when: {
      kind: 'sparql-ask',
      query: `
        PREFIX unproj: <http://example.org/unrdf/project#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK {
          ?feature rdf:type unproj:Feature .
          FILTER NOT EXISTS {
            ?file unproj:belongsToFeature ?feature ;
                  unproj:roleString ?role .
            FILTER(?role IN ("Component", "Page"))
          }
        }
      `,
    },
    determinism: { seed: 42 },
    receipt: { anchor: 'none' },

    before({ payload }) {
      return payload
    },

    run({ payload, context }) {
      const store = context.graph
      if (!store) {
        return { result: { valid: true, violations: [] } }
      }

      const projectNs = 'http://example.org/unrdf/project#'
      const rdfType = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'

      const currentViolations = []
      const featureQuads = store.getQuads(
        null,
        namedNode(rdfType),
        namedNode(`${projectNs}Feature`)
      )

      for (const featureQuad of featureQuads) {
        const featureIri = featureQuad.subject.value
        const roleQuads = store.getQuads(
          null,
          namedNode(`${projectNs}belongsToFeature`),
          namedNode(featureIri)
        )

        let hasView = false
        for (const roleQuad of roleQuads) {
          const fileRoles = store.getQuads(
            roleQuad.subject,
            namedNode(`${projectNs}roleString`),
            null
          )
          for (const fileRole of fileRoles) {
            if (['Component', 'Page'].includes(fileRole.object.value)) {
              hasView = true
              break
            }
          }
          if (hasView) break
        }

        if (!hasView) {
          currentViolations.push({
            type: 'feature-without-view',
            subject: featureIri,
            message: `Feature "${featureIri}" must have at least one view`,
          })
        }
      }

      return {
        result: {
          valid: currentViolations.length === 0,
          violations: currentViolations,
        },
      }
    },

    after({ result, cancelled, reason }) {
      if (cancelled) {
        return { result: { status: 'cancelled', reason } }
      }
      return { result: { status: 'completed', ...result } }
    },
  }
}

/**
 * Create hook for api-must-have-test invariant
 * @param {PatternViolation[]} violations - Detected violations
 * @returns {DerivedHook}
 */
function createApiTestHook(violations) {
  return {
    meta: {
      name: 'derived:api-must-have-test',
      description: 'Every Feature with API must have Test',
      ontology: ['unrdf-project'],
    },
    channel: {
      graphs: ['urn:graph:project'],
      view: 'after',
    },
    when: {
      kind: 'sparql-ask',
      query: `
        PREFIX unproj: <http://example.org/unrdf/project#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK {
          ?feature rdf:type unproj:Feature .
          ?apiFile unproj:belongsToFeature ?feature ;
                   unproj:roleString "Api" .
          FILTER NOT EXISTS {
            ?testFile unproj:belongsToFeature ?feature ;
                      unproj:roleString "Test" .
          }
        }
      `,
    },
    determinism: { seed: 42 },
    receipt: { anchor: 'none' },

    before({ payload }) {
      return payload
    },

    run({ payload, context }) {
      const store = context.graph
      if (!store) {
        return { result: { valid: true, violations: [] } }
      }

      const currentViolations = detectApiWithoutTest(store)

      return {
        result: {
          valid: currentViolations.length === 0,
          violations: currentViolations,
        },
      }
    },

    after({ result, cancelled, reason }) {
      if (cancelled) {
        return { result: { status: 'cancelled', reason } }
      }
      return { result: { status: 'completed', ...result } }
    },
  }
}

/**
 * Create hook for orphan-file invariant
 * @param {PatternViolation[]} violations - Detected violations
 * @returns {DerivedHook}
 */
function createOrphanFileHook(violations) {
  return {
    meta: {
      name: 'derived:no-orphan-files',
      description: 'Files under features/* must belong to some Feature',
      ontology: ['unrdf-project', 'unrdf-filesystem'],
    },
    channel: {
      graphs: ['urn:graph:project', 'urn:graph:fs'],
      view: 'after',
    },
    when: {
      kind: 'sparql-ask',
      query: `
        PREFIX unproj: <http://example.org/unrdf/project#>
        PREFIX fs: <http://example.org/unrdf/filesystem#>
        ASK {
          ?file fs:relativePath ?path .
          FILTER(REGEX(?path, "^src/features/[^/]+/"))
          FILTER NOT EXISTS {
            ?file unproj:belongsToFeature ?feature .
          }
        }
      `,
    },
    determinism: { seed: 42 },
    receipt: { anchor: 'none' },

    before({ payload }) {
      return payload
    },

    run({ payload, context }) {
      const store = context.graph
      if (!store) {
        return { result: { valid: true, violations: [] } }
      }

      const currentViolations = detectOrphanFiles(store)

      return {
        result: {
          valid: currentViolations.length === 0,
          violations: currentViolations,
        },
      }
    },

    after({ result, cancelled, reason }) {
      if (cancelled) {
        return { result: { status: 'cancelled', reason } }
      }
      return { result: { status: 'completed', ...result } }
    },
  }
}

/**
 * Create hook for test-companion invariant
 * @param {PatternViolation[]} violations - Detected violations
 * @returns {DerivedHook}
 */
function createTestCompanionHook(violations) {
  return {
    meta: {
      name: 'derived:test-companion-required',
      description: 'Test file must accompany main file',
      ontology: ['unrdf-project', 'unrdf-filesystem'],
    },
    channel: {
      graphs: ['urn:graph:project', 'urn:graph:fs'],
      view: 'after',
    },
    when: {
      kind: 'count',
      spec: {
        op: '>',
        value: 0,
        query: 'SELECT (COUNT(?file) AS ?count) WHERE { ?file a <http://example.org/unrdf/filesystem#File> }',
      },
    },
    determinism: { seed: 42 },
    receipt: { anchor: 'none' },

    before({ payload }) {
      return payload
    },

    run({ payload, context }) {
      const store = context.graph
      if (!store) {
        return { result: { valid: true, violations: [] } }
      }

      const currentViolations = detectFilesWithoutTests(store)

      return {
        result: {
          valid: currentViolations.length === 0,
          violations: currentViolations,
        },
      }
    },

    after({ result, cancelled, reason }) {
      if (cancelled) {
        return { result: { status: 'cancelled', reason } }
      }
      return { result: { status: 'completed', ...result } }
    },
  }
}

/**
 * Create hook for Next.js app router structure
 * @param {PatternViolation[]} violations - Detected violations
 * @param {Object} stackProfile - Stack profile
 * @returns {DerivedHook}
 */
function createNextAppRouterHook(violations, stackProfile) {
  return {
    meta: {
      name: 'derived:next-app-router-structure',
      description: 'Next.js app/[route]/page.tsx must have route.ts for API routes',
      ontology: ['unrdf-project', 'next-app-router'],
    },
    channel: {
      graphs: ['urn:graph:project', 'urn:graph:fs'],
      view: 'after',
    },
    when: {
      kind: 'sparql-ask',
      query: `
        PREFIX fs: <http://example.org/unrdf/filesystem#>
        ASK {
          ?file fs:relativePath ?path .
          FILTER(REGEX(?path, "app/.*/api/.*/page\\\\.(tsx?|jsx?)$"))
        }
      `,
    },
    determinism: { seed: 42 },
    receipt: { anchor: 'none' },

    before({ payload }) {
      return payload
    },

    run({ payload, context }) {
      const store = context.graph
      if (!store) {
        return { result: { valid: true, violations: [] } }
      }

      const currentViolations = detectNextAppRouterViolations(store, stackProfile)

      return {
        result: {
          valid: currentViolations.length === 0,
          violations: currentViolations,
          stackProfile,
        },
      }
    },

    after({ result, cancelled, reason }) {
      if (cancelled) {
        return { result: { status: 'cancelled', reason } }
      }
      return { result: { status: 'completed', ...result } }
    },
  }
}

// ============================================================================
// Main API
// ============================================================================

/**
 * Derive transaction hook policies from observed project patterns
 *
 * Analyzes the project store to detect pattern violations and generates
 * hooks that enforce these patterns on future transactions.
 *
 * @param {Object} projectStore - N3 Store with project structure
 * @param {Object} stackProfile - Stack profile from detectStackFromFs
 * @param {Object} [options] - Derivation options
 * @param {boolean} [options.enableFeatureViewPolicy=true] - Enable feature-view policy
 * @param {boolean} [options.enableApiTestPolicy=true] - Enable api-test policy
 * @param {boolean} [options.enableOrphanFilePolicy=true] - Enable orphan-file policy
 * @param {boolean} [options.enableTestCompanionPolicy=true] - Enable test-companion policy
 * @param {boolean} [options.enableStackPolicies=true] - Enable stack-specific policies
 * @param {boolean} [options.strictMode=false] - Fail on validation errors
 * @returns {DerivedHook[]} Array of Hook objects ready for KnowledgeHookManager.registerHook()
 *
 * @example
 * import { deriveHooksFromStructure } from './policy-derivation.mjs'
 * import { KnowledgeHookManager } from '../knowledge-engine/knowledge-hook-manager.mjs'
 *
 * const hooks = deriveHooksFromStructure(projectStore, stackProfile)
 * const manager = new KnowledgeHookManager()
 *
 * for (const hook of hooks) {
 *   manager.addKnowledgeHook(hook)
 * }
 */
export function deriveHooksFromStructure(projectStore, stackProfile, options = {}) {
  // Validate inputs
  if (!projectStore || typeof projectStore.getQuads !== 'function') {
    throw new TypeError('deriveHooksFromStructure: projectStore must be an N3 Store')
  }

  const validatedStack = StackProfileSchema.parse(stackProfile || {})
  const validatedOptions = DeriveOptionsSchema.parse(options)

  const hooks = []

  // Detect violations for each pattern type
  if (validatedOptions.enableFeatureViewPolicy) {
    const featureViewViolations = detectFeaturesWithoutViews(projectStore)
    hooks.push(createFeatureViewHook(featureViewViolations))
  }

  if (validatedOptions.enableApiTestPolicy) {
    const apiTestViolations = detectApiWithoutTest(projectStore)
    hooks.push(createApiTestHook(apiTestViolations))
  }

  if (validatedOptions.enableOrphanFilePolicy) {
    const orphanViolations = detectOrphanFiles(projectStore)
    hooks.push(createOrphanFileHook(orphanViolations))
  }

  if (validatedOptions.enableTestCompanionPolicy) {
    const testCompanionViolations = detectFilesWithoutTests(projectStore)
    hooks.push(createTestCompanionHook(testCompanionViolations))
  }

  // Stack-specific policies
  if (validatedOptions.enableStackPolicies) {
    if (validatedStack.webFramework === 'next-app-router') {
      const nextViolations = detectNextAppRouterViolations(projectStore, validatedStack)
      hooks.push(createNextAppRouterHook(nextViolations, validatedStack))
    }
  }

  return hooks
}

/**
 * Analyze project store and return violation report without generating hooks
 *
 * @param {Object} projectStore - N3 Store with project structure
 * @param {Object} stackProfile - Stack profile from detectStackFromFs
 * @param {Object} [options] - Analysis options
 * @returns {Object} Violation report
 */
export function analyzePatternViolations(projectStore, stackProfile, options = {}) {
  if (!projectStore || typeof projectStore.getQuads !== 'function') {
    throw new TypeError('analyzePatternViolations: projectStore must be an N3 Store')
  }

  const validatedStack = StackProfileSchema.parse(stackProfile || {})
  const validatedOptions = DeriveOptionsSchema.parse(options)

  const report = {
    timestamp: new Date().toISOString(),
    stackProfile: validatedStack,
    violations: {
      featureWithoutView: [],
      apiWithoutTest: [],
      orphanFiles: [],
      filesWithoutTests: [],
      stackSpecific: [],
    },
    summary: {
      total: 0,
      byType: {},
    },
  }

  if (validatedOptions.enableFeatureViewPolicy) {
    report.violations.featureWithoutView = detectFeaturesWithoutViews(projectStore)
  }

  if (validatedOptions.enableApiTestPolicy) {
    report.violations.apiWithoutTest = detectApiWithoutTest(projectStore)
  }

  if (validatedOptions.enableOrphanFilePolicy) {
    report.violations.orphanFiles = detectOrphanFiles(projectStore)
  }

  if (validatedOptions.enableTestCompanionPolicy) {
    report.violations.filesWithoutTests = detectFilesWithoutTests(projectStore)
  }

  if (validatedOptions.enableStackPolicies) {
    if (validatedStack.webFramework === 'next-app-router') {
      report.violations.stackSpecific = detectNextAppRouterViolations(projectStore, validatedStack)
    }
  }

  // Compute summary
  for (const [type, violations] of Object.entries(report.violations)) {
    report.summary.byType[type] = violations.length
    report.summary.total += violations.length
  }

  return report
}

/**
 * Create a custom hook from a pattern specification
 *
 * @param {Object} spec - Pattern specification
 * @param {string} spec.name - Hook name
 * @param {string} spec.description - Hook description
 * @param {string} spec.pattern - Pattern type
 * @param {Object} spec.condition - Trigger condition
 * @param {Function} spec.validator - Validation function
 * @returns {DerivedHook} Hook object
 */
export function createCustomPatternHook(spec) {
  const SpecSchema = z.object({
    name: z.string().min(1).max(100),
    description: z.string().min(1).max(500),
    pattern: z.string().min(1),
    condition: HookConditionSchema,
    validator: z.function(),
  })

  const validated = SpecSchema.parse(spec)

  return {
    meta: {
      name: `custom:${validated.name}`,
      description: validated.description,
      ontology: ['unrdf-project'],
    },
    channel: {
      graphs: ['urn:graph:project'],
      view: 'after',
    },
    when: validated.condition,
    determinism: { seed: 42 },
    receipt: { anchor: 'none' },

    before({ payload }) {
      return payload
    },

    run({ payload, context }) {
      const store = context.graph
      if (!store) {
        return { result: { valid: true, violations: [] } }
      }

      const violations = validated.validator(store, payload)

      return {
        result: {
          valid: Array.isArray(violations) ? violations.length === 0 : violations,
          violations: Array.isArray(violations) ? violations : [],
          pattern: validated.pattern,
        },
      }
    },

    after({ result, cancelled, reason }) {
      if (cancelled) {
        return { result: { status: 'cancelled', reason } }
      }
      return { result: { status: 'completed', ...result } }
    },
  }
}
