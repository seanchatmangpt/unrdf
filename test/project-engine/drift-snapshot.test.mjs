/**
 * @file Drift snapshot tests
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  createStructureSnapshot,
  computeDrift,
  createEmptyBaseline,
  serializeSnapshot,
  deserializeSnapshot,
} from '../../src/project-engine/drift-snapshot.mjs'

const { namedNode, literal } = DataFactory

describe('drift-snapshot', () => {
  describe('createStructureSnapshot', () => {
    it('creates snapshot with hash and receipt', () => {
      const fsStore = createMockFsStore()

      const { snapshotStore, receipt } = createStructureSnapshot(fsStore)

      expect(receipt.hash).toBeDefined()
      expect(receipt.hash.length).toBe(32)
      expect(receipt.createdAt).toBeDefined()
      expect(receipt.summary).toBeDefined()
      expect(snapshotStore.size).toBeGreaterThan(0)
    })

    it('includes file count in summary', () => {
      const fsStore = createMockFsStore()

      const { receipt } = createStructureSnapshot(fsStore)

      expect(receipt.summary.fileCount).toBe(3)
    })

    it('includes feature count in summary', () => {
      const fsStore = createMockFsStoreWithFeatures()

      const { receipt } = createStructureSnapshot(fsStore)

      expect(receipt.summary.featureCount).toBe(2)
    })

    it('includes role count in summary', () => {
      const fsStore = createMockFsStoreWithRoles()

      const { receipt } = createStructureSnapshot(fsStore)

      expect(receipt.summary.roleCount).toBe(4)
    })

    it('encodes template mappings', () => {
      const fsStore = createMockFsStore()
      const templateMappings = {
        'src/features/auth/index.tsx': 'react-component',
        'src/features/auth/hooks/useAuth.ts': 'react-hook',
      }

      const { snapshotStore, receipt } = createStructureSnapshot(fsStore, null, { templateMappings })

      expect(receipt.summary.templateMappingCount).toBe(2)

      const templateQuads = snapshotStore.getQuads(
        null,
        namedNode('http://example.org/unrdf/project#usesTemplate'),
        null
      )
      expect(templateQuads.length).toBe(2)
    })

    it('includes domain entities from domain store', () => {
      const fsStore = createMockFsStore()
      const domainStore = createMockDomainStore()

      const { receipt } = createStructureSnapshot(fsStore, domainStore)

      expect(receipt.summary.domainEntityCount).toBe(2)
    })

    it('adds snapshot metadata to store', () => {
      const fsStore = createMockFsStore()

      const { snapshotStore } = createStructureSnapshot(fsStore)

      const snapshotQuads = snapshotStore.getQuads(
        null,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/snapshot#StructureSnapshot')
      )
      expect(snapshotQuads.length).toBe(1)
    })
  })

  describe('computeDrift', () => {
    it('returns no drift for identical snapshots', () => {
      const store = createMockFsStoreWithFeatures()
      const { snapshotStore: baseline } = createStructureSnapshot(store)
      const { snapshotStore: current } = createStructureSnapshot(store)

      const result = computeDrift(current, baseline)

      expect(result.driftSeverity).toBe('none')
      expect(result.summary).toContain('No structural drift detected - code matches baseline model')
    })

    it('detects feature removal as major drift', () => {
      const baselineStore = createMockFsStoreWithFeatures()
      const currentStore = createMockFsStoreWithFeatures()

      // Remove a feature from current
      const featureQuads = currentStore.getQuads(
        namedNode('http://example.org/unrdf/project#feature/dashboard'),
        null,
        null
      )
      for (const quad of featureQuads) {
        currentStore.removeQuad(quad)
      }
      currentStore.removeQuads(currentStore.getQuads(
        null,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/project#Feature')
      ))

      const { snapshotStore: baseline } = createStructureSnapshot(baselineStore)
      const { snapshotStore: current } = createStructureSnapshot(currentStore)

      const result = computeDrift(current, baseline)

      expect(result.driftSeverity).toBe('major')
    })

    it('detects feature addition', () => {
      const baselineStore = createMockFsStore()
      const currentStore = createMockFsStoreWithFeatures()

      const { snapshotStore: baseline } = createStructureSnapshot(baselineStore)
      const { snapshotStore: current } = createStructureSnapshot(currentStore)

      const result = computeDrift(current, baseline)

      expect(result.ontologyDiff.changes.length).toBeGreaterThan(0)
      const featureAdded = result.ontologyDiff.changes.some(c => c.kind === 'FeatureAdded')
      expect(featureAdded).toBe(true)
    })

    it('returns ontologyDiff with triples and changes', () => {
      const baselineStore = createMockFsStore()
      const currentStore = createMockFsStoreWithRoles()

      const { snapshotStore: baseline } = createStructureSnapshot(baselineStore)
      const { snapshotStore: current } = createStructureSnapshot(currentStore)

      const result = computeDrift(current, baseline)

      expect(result.ontologyDiff).toBeDefined()
      expect(result.ontologyDiff.triples).toBeDefined()
      expect(result.ontologyDiff.triples.added).toBeDefined()
      expect(result.ontologyDiff.triples.removed).toBeDefined()
      expect(result.ontologyDiff.changes).toBeDefined()
    })

    it('classifies minor drift correctly', () => {
      const baselineStore = createMockFsStoreWithRoles()
      const currentStore = createMockFsStoreWithRoles()

      // Add a small change
      currentStore.addQuad(
        namedNode('http://example.org/unrdf/fs#newfile'),
        namedNode('http://example.org/unrdf/project#hasRole'),
        namedNode('http://example.org/unrdf/project#Config')
      )

      const { snapshotStore: baseline } = createStructureSnapshot(baselineStore)
      const { snapshotStore: current } = createStructureSnapshot(currentStore)

      const result = computeDrift(current, baseline)

      expect(result.driftSeverity).toBe('minor')
    })
  })

  describe('serialization', () => {
    it('serializes and deserializes snapshot', () => {
      const fsStore = createMockFsStoreWithFeatures()
      const { snapshotStore, receipt } = createStructureSnapshot(fsStore)

      const json = serializeSnapshot(snapshotStore, receipt)
      expect(typeof json).toBe('string')

      const { snapshotStore: restored, receipt: restoredReceipt } = deserializeSnapshot(json)

      expect(restoredReceipt.hash).toBe(receipt.hash)
      expect(restored.size).toBe(snapshotStore.size)
    })

    it('preserves quads after round-trip', () => {
      const fsStore = createMockFsStore()
      const { snapshotStore, receipt } = createStructureSnapshot(fsStore)

      const json = serializeSnapshot(snapshotStore, receipt)
      const { snapshotStore: restored } = deserializeSnapshot(json)

      const originalQuads = snapshotStore.getQuads(null, null, null, null)
      const restoredQuads = restored.getQuads(null, null, null, null)

      expect(restoredQuads.length).toBe(originalQuads.length)
    })
  })

  describe('createEmptyBaseline', () => {
    it('returns empty store', () => {
      const baseline = createEmptyBaseline()

      expect(baseline.size).toBe(0)
    })
  })
})

// Test helpers

function createMockFsStore() {
  const store = new Store()

  // Add 3 files
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file1'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file1'),
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    literal('src/index.ts')
  )

  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file2'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file2'),
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    literal('src/utils.ts')
  )

  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file3'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file3'),
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    literal('test/index.test.ts')
  )

  return store
}

function createMockFsStoreWithFeatures() {
  const store = createMockFsStore()

  // Add 2 features
  store.addQuad(
    namedNode('http://example.org/unrdf/project#feature/auth'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/project#Feature')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/project#feature/auth'),
    namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
    literal('auth')
  )

  store.addQuad(
    namedNode('http://example.org/unrdf/project#feature/dashboard'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/project#Feature')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/project#feature/dashboard'),
    namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
    literal('dashboard')
  )

  return store
}

function createMockFsStoreWithRoles() {
  const store = createMockFsStoreWithFeatures()

  // Add 4 roles
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file1'),
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Component')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file1'),
    namedNode('http://example.org/unrdf/project#belongsToFeature'),
    namedNode('http://example.org/unrdf/project#feature/auth')
  )

  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file2'),
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Hook')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file2'),
    namedNode('http://example.org/unrdf/project#belongsToFeature'),
    namedNode('http://example.org/unrdf/project#feature/auth')
  )

  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file3'),
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Test')
  )
  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file3'),
    namedNode('http://example.org/unrdf/project#belongsToFeature'),
    namedNode('http://example.org/unrdf/project#feature/auth')
  )

  store.addQuad(
    namedNode('http://example.org/unrdf/fs#file4'),
    namedNode('http://example.org/unrdf/project#hasRole'),
    namedNode('http://example.org/unrdf/project#Doc')
  )

  return store
}

function createMockDomainStore() {
  const store = new Store()

  // Add 2 domain entities
  store.addQuad(
    namedNode('http://example.org/domain#User'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/domain#Entity')
  )
  store.addQuad(
    namedNode('http://example.org/domain#User'),
    namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
    literal('User')
  )

  store.addQuad(
    namedNode('http://example.org/domain#Product'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/unrdf/domain#Entity')
  )
  store.addQuad(
    namedNode('http://example.org/domain#Product'),
    namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
    literal('Product')
  )

  return store
}
