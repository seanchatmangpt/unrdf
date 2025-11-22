/**
 * @file Artifact materialization - plan and receipt for generation
 * @module project-engine/materialize
 */

import { z } from 'zod'
import { createHash } from 'crypto'

const MaterializeOptionsSchema = z.object({
  ontologyStore: z.object({}).passthrough(),
  templateConfig: z.record(z.string(), z.any()).optional(),
  options: z.object({
    dryRun: z.boolean().optional(),
    outputRoot: z.string().optional(),
  }).optional(),
})

/**
 * Compute materialization plan from ontology
 *
 * @param {Object} params
 * @param {Store} params.ontologyStore - Current ontology
 * @param {Object} [params.templateConfig] - Template-to-generator mappings
 * @param {Object} [params.options] - Materialization options
 * @returns {{plan: Object, receipt: Object}}
 */
export function materializeArtifacts(params) {
  const validated = MaterializeOptionsSchema.parse(params)
  const { ontologyStore, options = {} } = validated

  const plan = {
    writes: [],
    deletes: [],
    moves: [],
    metadata: {
      timestamp: new Date().toISOString(),
      storeSize: ontologyStore.size,
    },
  }

  const features = extractFeaturesFromStore(ontologyStore)

  for (const [featureName, feature] of Object.entries(features)) {
    for (const role of feature.roles) {
      const filePath = generatePathForRole(featureName, role)
      plan.writes.push({
        path: filePath,
        type: role,
        feature: featureName,
      })
    }
  }

  const beforeHash = hashStore(ontologyStore)
  const planHash = hashPlan(plan)

  const receipt = {
    beforeHash,
    afterHash: planHash,
    planHash,
    changes: plan.writes.length + plan.deletes.length + plan.moves.length,
    dryRun: options.dryRun || false,
    timestamp: new Date().toISOString(),
  }

  return { plan, receipt }
}

/**
 * Extract features from ontology store
 *
 * @private
 */
function extractFeaturesFromStore(store) {
  const features = {}

  try {
    const featureQuads = store.getQuads(
      null,
      { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
      null
    )

    for (const quad of featureQuads) {
      const subjectStr = quad.subject.value
      if (quad.object.value && quad.object.value.includes('Feature')) {
        features[subjectStr] = { roles: ['Component', 'Hook', 'Test', 'Doc'] }
      }
    }
  } catch (e) {
    // Ignore errors, return empty features
  }

  return features
}

/**
 * Generate file path for a feature and role
 *
 * @private
 */
function generatePathForRole(featureName, role) {
  const roleMap = {
    Component: 'components/index.tsx',
    Hook: 'hooks/index.ts',
    Test: '__tests__/index.test.tsx',
    Doc: 'README.md',
    Api: 'api/route.ts',
  }

  const suffix = roleMap[role] || `${role.toLowerCase()}/index.ts`
  return `src/features/${featureName}/${suffix}`
}

/**
 * Hash a store for provenance
 *
 * @private
 */
function hashStore(store) {
  const hash = createHash('sha256')
  hash.update(String(store.size))
  return hash.digest('hex').substring(0, 16)
}

/**
 * Hash a plan for receipt
 *
 * @private
 */
function hashPlan(plan) {
  const hash = createHash('sha256')
  hash.update(JSON.stringify(plan.metadata))
  return hash.digest('hex').substring(0, 16)
}
