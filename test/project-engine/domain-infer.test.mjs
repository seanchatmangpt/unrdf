/**
 * @file Domain inference tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  inferDomainModel,
  DomainModelLens,
} from '../../src/project-engine/domain-infer.mjs'
import { diffOntologyFromStores } from '../../src/diff.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
}

/**
 * Create a mock FS store with file paths
 * @param {string[]} paths
 * @returns {Store}
 */
function createMockFsStore(paths) {
  const store = new Store()

  for (const path of paths) {
    const fileIri = namedNode(`http://example.org/fs#${encodeURIComponent(path)}`)
    store.addQuad(
      fileIri,
      namedNode(`${NS.fs}relativePath`),
      literal(path)
    )
  }

  return store
}

describe('domain-infer', () => {
  describe('inferDomainModel', () => {
    it('should return empty result for empty store', async () => {
      const fsStore = new Store()

      const { store, summary } = await inferDomainModel({ fsStore })

      expect(summary.entityCount).toBe(0)
      expect(summary.fieldCount).toBe(0)
      expect(summary.relationshipCount).toBe(0)
      expect(store.size).toBe(0)
    })

    it('should return empty result when no projectRoot provided', async () => {
      const fsStore = createMockFsStore([
        'src/schemas/user.ts',
        'src/schemas/product.ts',
      ])

      const { store, summary } = await inferDomainModel({
        fsStore,
        stackProfile: { hasZod: true, hasTypescript: true },
      })

      // Without projectRoot, cannot read file contents
      expect(summary.entityCount).toBe(0)
    })

    it('should detect stack profile from fsStore', async () => {
      const fsStore = createMockFsStore([
        'package.json',
        'tsconfig.json',
        'prisma/schema.prisma',
        'src/schemas/user.ts',
      ])

      // Just verify it doesn't throw
      const { store, summary } = await inferDomainModel({ fsStore })

      expect(store).toBeDefined()
      expect(summary).toHaveProperty('entityCount')
      expect(summary).toHaveProperty('fieldCount')
      expect(summary).toHaveProperty('relationshipCount')
    })

    it('should respect provided stackProfile', async () => {
      const fsStore = createMockFsStore([
        'src/types/user.ts',
      ])

      const { store, summary } = await inferDomainModel({
        fsStore,
        stackProfile: {
          hasZod: false,
          hasPrisma: false,
          hasTypeORM: false,
          hasSequelize: false,
          hasDrizzle: false,
          hasTypescript: true,
          sourceRoot: 'src',
        },
      })

      expect(store).toBeDefined()
      expect(summary).toHaveProperty('entityCount')
    })

    it('should use custom baseIri', async () => {
      const fsStore = createMockFsStore([
        'src/schemas/user.ts',
      ])
      const customIri = 'http://custom.example.org/domain#'

      const { store, summary } = await inferDomainModel({
        fsStore,
        baseIri: customIri,
      })

      expect(store).toBeDefined()
      // Even without entities, the baseIri should be respected
    })
  })

  describe('DomainModelLens', () => {
    it('should detect EntityAdded changes', () => {
      const triple = {
        subject: `${NS.dom}User`,
        predicate: `${NS.rdf}type`,
        object: `${NS.dom}Entity`,
      }

      const change = DomainModelLens(triple, 'added')

      expect(change).not.toBeNull()
      expect(change.kind).toBe('EntityAdded')
      expect(change.entity).toBe(`${NS.dom}User`)
      expect(change.details.name).toBe('User')
    })

    it('should detect EntityRemoved changes', () => {
      const triple = {
        subject: `${NS.dom}Product`,
        predicate: `${NS.rdf}type`,
        object: `${NS.dom}Entity`,
      }

      const change = DomainModelLens(triple, 'removed')

      expect(change).not.toBeNull()
      expect(change.kind).toBe('EntityRemoved')
      expect(change.entity).toBe(`${NS.dom}Product`)
    })

    it('should detect FieldAdded changes', () => {
      const triple = {
        subject: `${NS.dom}User`,
        predicate: `${NS.dom}hasField`,
        object: `${NS.dom}User.email`,
      }

      const change = DomainModelLens(triple, 'added')

      expect(change).not.toBeNull()
      expect(change.kind).toBe('FieldAdded')
      expect(change.entity).toBe(`${NS.dom}User`)
      expect(change.role).toBe('email')
    })

    it('should detect FieldRemoved changes', () => {
      const triple = {
        subject: `${NS.dom}User`,
        predicate: `${NS.dom}hasField`,
        object: `${NS.dom}User.name`,
      }

      const change = DomainModelLens(triple, 'removed')

      expect(change).not.toBeNull()
      expect(change.kind).toBe('FieldRemoved')
      expect(change.role).toBe('name')
    })

    it('should detect RelationAdded changes', () => {
      const triple = {
        subject: `${NS.dom}User`,
        predicate: `${NS.dom}relatesTo`,
        object: `${NS.dom}Order`,
      }

      const change = DomainModelLens(triple, 'added')

      expect(change).not.toBeNull()
      expect(change.kind).toBe('RelationAdded')
      expect(change.details.from).toBe('User')
      expect(change.details.to).toBe('Order')
    })

    it('should detect RelationRemoved changes', () => {
      const triple = {
        subject: `${NS.dom}Order`,
        predicate: `${NS.dom}relatesTo`,
        object: `${NS.dom}Product`,
      }

      const change = DomainModelLens(triple, 'removed')

      expect(change).not.toBeNull()
      expect(change.kind).toBe('RelationRemoved')
      expect(change.details.from).toBe('Order')
      expect(change.details.to).toBe('Product')
    })

    it('should detect FieldTypeSet changes', () => {
      const triple = {
        subject: `${NS.dom}User.email`,
        predicate: `${NS.dom}fieldType`,
        object: `${NS.xsd}string`,
      }

      const change = DomainModelLens(triple, 'added')

      expect(change).not.toBeNull()
      expect(change.kind).toBe('FieldTypeSet')
      expect(change.details.type).toBe(`${NS.xsd}string`)
    })

    it('should return null for unrecognized predicates', () => {
      const triple = {
        subject: `${NS.dom}User`,
        predicate: `${NS.rdfs}label`,
        object: 'User Entity',
      }

      const change = DomainModelLens(triple, 'added')

      expect(change).toBeNull()
    })
  })

  describe('DomainModelLens with diffOntologyFromStores', () => {
    it('should compute ontology diff for domain changes', () => {
      const beforeStore = new Store()
      const afterStore = new Store()

      // Before: User entity with email field
      beforeStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      beforeStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.dom}hasField`),
        namedNode(`${NS.dom}User.email`)
      )

      // After: User entity with email + name fields
      afterStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      afterStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.dom}hasField`),
        namedNode(`${NS.dom}User.email`)
      )
      afterStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.dom}hasField`),
        namedNode(`${NS.dom}User.name`)
      )

      const diff = diffOntologyFromStores(beforeStore, afterStore, DomainModelLens)

      expect(diff.triples.added).toHaveLength(1)
      expect(diff.triples.removed).toHaveLength(0)
      expect(diff.changes).toHaveLength(1)
      expect(diff.changes[0].kind).toBe('FieldAdded')
      expect(diff.changes[0].role).toBe('name')
    })

    it('should detect entity additions', () => {
      const beforeStore = new Store()
      const afterStore = new Store()

      // After: Add new Product entity
      afterStore.addQuad(
        namedNode(`${NS.dom}Product`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      afterStore.addQuad(
        namedNode(`${NS.dom}Product`),
        namedNode(`${NS.rdfs}label`),
        literal('Product')
      )

      const diff = diffOntologyFromStores(beforeStore, afterStore, DomainModelLens)

      expect(diff.changes.some(c => c.kind === 'EntityAdded')).toBe(true)
    })

    it('should detect relation changes', () => {
      const beforeStore = new Store()
      const afterStore = new Store()

      // Before: User entity
      beforeStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )

      // After: User entity with relation to Order
      afterStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      afterStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.dom}relatesTo`),
        namedNode(`${NS.dom}Order`)
      )

      const diff = diffOntologyFromStores(beforeStore, afterStore, DomainModelLens)

      expect(diff.changes.some(c => c.kind === 'RelationAdded')).toBe(true)
      expect(diff.changes.find(c => c.kind === 'RelationAdded').details.to).toBe('Order')
    })
  })
})
