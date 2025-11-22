/**
 * @file Gap finder tests - Chicago School TDD with real collaborators
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest'
import { Store, DataFactory } from 'n3'
import { findMissingRoles, scoreMissingRole } from '../../src/project-engine/gap-finder.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
}

/**
 * Create domain store with entities
 * @param {Array<{name: string, fields?: string[]}>} entities
 */
function createDomainStore(entities) {
  const store = new Store()
  for (const { name, fields = [] } of entities) {
    const iri = namedNode(`${NS.dom}${name}`)
    store.addQuad(iri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`))
    store.addQuad(iri, namedNode(`${NS.rdfs}label`), literal(name))
    for (const field of fields) {
      store.addQuad(iri, namedNode(`${NS.dom}hasField`), namedNode(`${NS.dom}${name}.${field}`))
    }
  }
  return store
}

/**
 * Create project store with classified files
 * @param {Array<{path: string, role: string}>} files
 */
function createProjectStore(files) {
  const store = new Store()
  for (const { path, role } of files) {
    const iri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(path)}`)
    store.addQuad(iri, namedNode(`${NS.fs}relativePath`), literal(path))
    if (role) {
      store.addQuad(iri, namedNode(`${NS.proj}roleString`), literal(role))
    }
  }
  return store
}

describe('gap-finder', () => {
  describe('findMissingRoles', () => {
    it('reports missing API when entity has no Api role', () => {
      const domainStore = createDomainStore([{ name: 'User', fields: ['id', 'email'] }])
      const projectStore = createProjectStore([
        { path: 'src/components/User.tsx', role: 'Component' },
      ])

      const result = findMissingRoles({ domainStore, projectStore })

      expect(result.gaps).toContainEqual(
        expect.objectContaining({ entity: 'User', missingRoles: expect.arrayContaining(['Api']) })
      )
    })

    it('reports missing Test when entity has no test files', () => {
      const domainStore = createDomainStore([{ name: 'Product', fields: ['id', 'name', 'price'] }])
      const projectStore = createProjectStore([
        { path: 'src/api/product.ts', role: 'Api' },
        { path: 'src/components/ProductView.tsx', role: 'Component' },
      ])

      const result = findMissingRoles({ domainStore, projectStore })

      const productGaps = result.gaps.find(g => g.entity === 'Product')
      expect(productGaps.missingRoles).toContain('Test')
    })

    it('returns no gaps when all required roles present', () => {
      const domainStore = createDomainStore([{ name: 'User', fields: ['id', 'email'] }])
      const projectStore = createProjectStore([
        { path: 'src/api/user.ts', role: 'Api' },
        { path: 'src/components/UserView.tsx', role: 'Component' },
        { path: 'test/user.test.ts', role: 'Test' },
        { path: 'src/types/user.ts', role: 'Schema' },
      ])

      const result = findMissingRoles({ domainStore, projectStore })

      const userGaps = result.gaps.find(g => g.entity === 'User')
      expect(userGaps.missingRoles).toHaveLength(0)
    })

    it('prioritizes by framework - Next.js needs Page', () => {
      const domainStore = createDomainStore([{ name: 'Product', fields: ['id'] }])
      const projectStore = createProjectStore([
        { path: 'src/api/product.ts', role: 'Api' },
      ])

      const result = findMissingRoles({
        domainStore,
        projectStore,
        stackProfile: { webFramework: 'next' },
      })

      const productGaps = result.gaps.find(g => g.entity === 'Product')
      expect(productGaps.missingRoles).toContain('Page')
    })

    it('prioritizes by framework - Express needs Route', () => {
      const domainStore = createDomainStore([{ name: 'Order', fields: ['id'] }])
      const projectStore = createProjectStore([
        { path: 'src/models/order.ts', role: 'Schema' },
      ])

      const result = findMissingRoles({
        domainStore,
        projectStore,
        stackProfile: { webFramework: 'express' },
      })

      const orderGaps = result.gaps.find(g => g.entity === 'Order')
      expect(orderGaps.missingRoles).toContain('Route')
    })

    it('returns empty gaps array for empty domain store', () => {
      const domainStore = new Store()
      const projectStore = createProjectStore([
        { path: 'src/api/random.ts', role: 'Api' },
      ])

      const result = findMissingRoles({ domainStore, projectStore })

      expect(result.gaps).toEqual([])
      expect(result.summary).toContain('0 gaps')
    })

    it('generates summary with gap counts', () => {
      const domainStore = createDomainStore([
        { name: 'User', fields: ['id'] },
        { name: 'Product', fields: ['id'] },
      ])
      const projectStore = createProjectStore([])

      const result = findMissingRoles({ domainStore, projectStore })

      expect(result.summary).toMatch(/\d+ missing/i)
      expect(result.callToAction).toContain('unrdf')
    })

    it('matches files to entities by name pattern', () => {
      const domainStore = createDomainStore([{ name: 'User', fields: ['id'] }])
      const projectStore = createProjectStore([
        { path: 'src/api/users.ts', role: 'Api' },  // plural form
        { path: 'src/components/user-profile.tsx', role: 'Component' },  // kebab-case
      ])

      const result = findMissingRoles({ domainStore, projectStore })

      const userGaps = result.gaps.find(g => g.entity === 'User')
      expect(userGaps.missingRoles).not.toContain('Api')
      expect(userGaps.missingRoles).not.toContain('Component')
    })
  })

  describe('scoreMissingRole', () => {
    it('scores Api role at 95 (critical)', () => {
      const score = scoreMissingRole('User', 'Api', {})
      expect(score).toBe(95)
    })

    it('scores Test role at 90 (high priority)', () => {
      const score = scoreMissingRole('User', 'Test', {})
      expect(score).toBe(90)
    })

    it('scores Component role at 80', () => {
      const score = scoreMissingRole('User', 'Component', {})
      expect(score).toBe(80)
    })

    it('scores Schema role at 70', () => {
      const score = scoreMissingRole('User', 'Schema', {})
      expect(score).toBe(70)
    })

    it('scores Doc role at 50 (lower priority)', () => {
      const score = scoreMissingRole('User', 'Doc', {})
      expect(score).toBe(50)
    })

    it('boosts Page score for Next.js stack', () => {
      const baseScore = scoreMissingRole('Product', 'Page', {})
      const nextScore = scoreMissingRole('Product', 'Page', { webFramework: 'next' })
      expect(nextScore).toBeGreaterThan(baseScore)
    })

    it('boosts Route score for Express stack', () => {
      const baseScore = scoreMissingRole('Product', 'Route', {})
      const expressScore = scoreMissingRole('Product', 'Route', { webFramework: 'express' })
      expect(expressScore).toBeGreaterThan(baseScore)
    })
  })
})
