/**
 * @vitest-environment node
 * @file Tests for materialize-plan.mjs
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  planMaterialization,
  validatePlan,
  createEmptyPlan,
  mergePlans,
} from '../../src/project-engine/materialize-plan.mjs'

const { namedNode, literal } = DataFactory

describe('materialize-plan', () => {
  /** @type {Store} */
  let ontologyStore
  /** @type {Store} */
  let templateGraph

  beforeEach(() => {
    ontologyStore = new Store()
    templateGraph = new Store()
  })

  describe('planMaterialization', () => {
    it('returns empty plan for empty stores', () => {
      const { plan, receipt } = planMaterialization(ontologyStore, templateGraph)

      expect(plan.writes).toEqual([])
      expect(plan.updates).toEqual([])
      expect(plan.deletes).toEqual([])
      expect(receipt.mappings).toEqual([])
      expect(receipt.ontologyHash).toBeDefined()
      expect(receipt.templateHash).toBeDefined()
      expect(receipt.planHash).toBeDefined()
      expect(receipt.timestamp).toBeDefined()
    })

    it('generates writes when entity matches template target', () => {
      // Add a User entity to ontology
      const userIri = namedNode('http://example.org/entity/User')
      const userType = namedNode('http://example.org/schema#DomainEntity')

      ontologyStore.addQuad(
        userIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        userType
      )
      ontologyStore.addQuad(
        userIri,
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        literal('User')
      )

      // Add a template targeting DomainEntity
      const templateIri = namedNode('http://example.org/template/component')
      templateGraph.addQuad(
        templateIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/template#Template')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#targetsClass'),
        userType
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#outputPattern'),
        literal('src/{{entity}}/index.mjs')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#content'),
        literal('export const {{Entity}} = {}')
      )

      const { plan, receipt } = planMaterialization(ontologyStore, templateGraph)

      expect(plan.writes.length).toBe(1)
      expect(plan.writes[0].path).toBe('src/User/index.mjs')
      expect(plan.writes[0].content).toBe('export const User = {}')
      expect(plan.writes[0].templateIri).toBe(templateIri.value)
      expect(plan.writes[0].entityIri).toBe(userIri.value)
      expect(receipt.mappings.length).toBe(1)
    })

    it('applies variable substitutions correctly', () => {
      // Add entity
      const productIri = namedNode('http://example.org/entity/ProductItem')
      const entityType = namedNode('http://example.org/schema#Model')

      ontologyStore.addQuad(
        productIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        entityType
      )
      ontologyStore.addQuad(
        productIri,
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        literal('productItem')
      )

      // Add template with multiple variable types
      const templateIri = namedNode('http://example.org/template/full')
      templateGraph.addQuad(
        templateIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/template#Template')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#targetsClass'),
        entityType
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#outputPattern'),
        literal('src/{{entity}}/{{entity-kebab}}.mjs')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#content'),
        literal('// {{ENTITY}}\nexport class {{Entity}} {}')
      )

      const { plan } = planMaterialization(ontologyStore, templateGraph)

      expect(plan.writes.length).toBe(1)
      expect(plan.writes[0].path).toBe('src/productItem/product-item.mjs')
      expect(plan.writes[0].content).toBe('// PRODUCTITEM\nexport class ProductItem {}')
    })

    it('respects outputRoot option', () => {
      // Add entity and template
      const entityIri = namedNode('http://example.org/entity/Test')
      const entityType = namedNode('http://example.org/schema#Entity')

      ontologyStore.addQuad(
        entityIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        entityType
      )
      ontologyStore.addQuad(
        entityIri,
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        literal('Test')
      )

      const templateIri = namedNode('http://example.org/template/t1')
      templateGraph.addQuad(
        templateIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/template#Template')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#targetsClass'),
        entityType
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#outputPattern'),
        literal('{{entity}}.mjs')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#content'),
        literal('// content')
      )

      const { plan } = planMaterialization(ontologyStore, templateGraph, {
        outputRoot: 'output/generated',
      })

      expect(plan.writes[0].path).toBe('output/generated/Test.mjs')
    })

    it('detects updates for existing files with different content', () => {
      // Add entity
      const entityIri = namedNode('http://example.org/entity/Item')
      const entityType = namedNode('http://example.org/schema#Type')

      ontologyStore.addQuad(
        entityIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        entityType
      )
      ontologyStore.addQuad(
        entityIri,
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        literal('Item')
      )

      // Add template
      const templateIri = namedNode('http://example.org/template/t1')
      templateGraph.addQuad(
        templateIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/template#Template')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#targetsClass'),
        entityType
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#outputPattern'),
        literal('src/{{entity}}.mjs')
      )
      templateGraph.addQuad(
        templateIri,
        namedNode('http://example.org/unrdf/template#content'),
        literal('new content')
      )

      const { plan } = planMaterialization(ontologyStore, templateGraph, {
        existingFiles: {
          'src/Item.mjs': 'oldhash123',
        },
      })

      expect(plan.writes.length).toBe(0)
      expect(plan.updates.length).toBe(1)
      expect(plan.updates[0].path).toBe('src/Item.mjs')
      expect(plan.updates[0].oldHash).toBe('oldhash123')
    })
  })

  describe('validatePlan', () => {
    it('returns valid for empty plan', () => {
      const plan = createEmptyPlan()
      const result = validatePlan(plan)

      expect(result.valid).toBe(true)
      expect(result.errors).toEqual([])
    })

    it('detects duplicate output paths', () => {
      const plan = {
        writes: [
          { path: 'src/file.mjs', content: 'a', hash: 'h1', templateIri: 't1', entityIri: 'e1', entityType: 'Type' },
          { path: 'src/file.mjs', content: 'b', hash: 'h2', templateIri: 't2', entityIri: 'e2', entityType: 'Type' },
        ],
        updates: [],
        deletes: [],
      }

      const result = validatePlan(plan)

      expect(result.valid).toBe(false)
      expect(result.errors.length).toBe(1)
      expect(result.errors[0]).toContain('Duplicate')
    })

    it('detects absolute paths', () => {
      const plan = {
        writes: [
          { path: '/etc/passwd', content: 'a', hash: 'h1', templateIri: 't1', entityIri: 'e1', entityType: 'Type' },
        ],
        updates: [],
        deletes: [],
      }

      const result = validatePlan(plan)

      expect(result.valid).toBe(false)
      expect(result.errors[0]).toContain('Absolute path')
    })

    it('detects path traversal', () => {
      const plan = {
        writes: [
          { path: '../../../etc/passwd', content: 'a', hash: 'h1', templateIri: 't1', entityIri: 'e1', entityType: 'Type' },
        ],
        updates: [],
        deletes: [],
      }

      const result = validatePlan(plan)

      expect(result.valid).toBe(false)
      expect(result.errors[0]).toContain('Path traversal')
    })
  })

  describe('createEmptyPlan', () => {
    it('creates empty plan structure', () => {
      const plan = createEmptyPlan()

      expect(plan.writes).toEqual([])
      expect(plan.updates).toEqual([])
      expect(plan.deletes).toEqual([])
    })
  })

  describe('mergePlans', () => {
    it('merges two plans', () => {
      const plan1 = {
        writes: [{ path: 'a.mjs', content: 'a', hash: 'h1', templateIri: 't1', entityIri: 'e1', entityType: 'Type' }],
        updates: [],
        deletes: [],
      }

      const plan2 = {
        writes: [{ path: 'b.mjs', content: 'b', hash: 'h2', templateIri: 't2', entityIri: 'e2', entityType: 'Type' }],
        updates: [{ path: 'c.mjs', content: 'c', oldHash: 'old', newHash: 'new', templateIri: 't3', entityIri: 'e3' }],
        deletes: [],
      }

      const merged = mergePlans(plan1, plan2)

      expect(merged.writes.length).toBe(2)
      expect(merged.updates.length).toBe(1)
      expect(merged.deletes.length).toBe(0)
    })
  })
})
