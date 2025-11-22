/**
 * @file Hotspot analyzer tests - Chicago School TDD with real stores
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  analyzeHotspots,
  scoreFeature,
} from '../../src/project-engine/hotspot-analyzer.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
  dom: 'http://example.org/unrdf/domain#',
}

/**
 * Create a realistic project store with features and files
 * @param {Object} config - Configuration for project structure
 * @returns {Store}
 */
function createProjectStore(config = {}) {
  const store = new Store()
  const {
    features = {},
    baseIri = 'http://example.org/unrdf/project#',
  } = config

  // Add project
  const projectIri = namedNode(`${baseIri}project`)
  store.addQuad(
    projectIri,
    namedNode(`${NS.rdf}type`),
    namedNode(`${NS.proj}Project`)
  )

  // Add features with files
  for (const [featureName, featureConfig] of Object.entries(features)) {
    const featureIri = namedNode(`${baseIri}feature/${encodeURIComponent(featureName)}`)

    // Feature type and label
    store.addQuad(
      featureIri,
      namedNode(`${NS.rdf}type`),
      namedNode(`${NS.proj}Feature`)
    )
    store.addQuad(
      featureIri,
      namedNode(`${NS.rdfs}label`),
      literal(featureName)
    )
    store.addQuad(
      projectIri,
      namedNode(`${NS.proj}hasFeature`),
      featureIri
    )

    // Add files to feature
    const files = featureConfig.files || []
    const byteSizes = featureConfig.byteSizes || {}
    const roles = featureConfig.roles || {}

    for (const filePath of files) {
      const fileIri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(filePath)}`)

      // File path
      store.addQuad(
        fileIri,
        namedNode(`${NS.fs}relativePath`),
        literal(filePath)
      )

      // Belongs to feature
      store.addQuad(
        fileIri,
        namedNode(`${NS.proj}belongsToFeature`),
        featureIri
      )

      // Byte size (for line count estimation)
      const byteSize = byteSizes[filePath] || 2000 // Default ~50 lines
      store.addQuad(
        fileIri,
        namedNode(`${NS.fs}byteSize`),
        literal(byteSize, namedNode(`${NS.xsd}integer`))
      )

      // Role (if provided)
      const role = roles[filePath]
      if (role) {
        store.addQuad(
          fileIri,
          namedNode(`${NS.proj}roleString`),
          literal(role)
        )
      }
    }

    // Add dependencies (relationships)
    const dependencies = featureConfig.dependencies || []
    for (const dep of dependencies) {
      store.addQuad(
        featureIri,
        namedNode(`${NS.proj}dependsOn`),
        namedNode(`${baseIri}feature/${encodeURIComponent(dep)}`)
      )
    }
  }

  return store
}

describe('hotspot-analyzer', () => {
  describe('analyzeHotspots', () => {
    it('should return empty hotspots for empty project store', () => {
      const projectStore = new Store()

      const result = analyzeHotspots({ projectStore })

      expect(result.hotspots).toHaveLength(0)
      expect(result.topRisks).toHaveLength(0)
      expect(result.summary).toBe('No high-risk features identified')
    })

    it('should analyze single feature with files', () => {
      const projectStore = createProjectStore({
        features: {
          checkout: {
            files: [
              'src/checkout/cart.mjs',
              'src/checkout/payment.mjs',
              'src/checkout/shipping.mjs',
            ],
          },
        },
      })

      const result = analyzeHotspots({ projectStore })

      expect(result.hotspots).toHaveLength(1)
      expect(result.hotspots[0].feature).toBe('checkout')
      expect(result.hotspots[0].metrics.fileCount).toBe(3)
      expect(result.hotspots[0].score).toBeGreaterThan(0)
    })

    it('should score feature with many files high', () => {
      // Create feature with 45+ files (high complexity)
      const files = []
      for (let i = 0; i < 45; i++) {
        files.push(`src/large-feature/file${i}.mjs`)
      }

      const projectStore = createProjectStore({
        features: {
          'large-feature': { files },
        },
      })

      const result = analyzeHotspots({ projectStore })

      // Large feature (45+ files) should score > 70
      const largeFeature = result.hotspots.find(h => h.metrics.fileCount > 40)
      expect(largeFeature).toBeDefined()
      expect(largeFeature.score).toBeGreaterThan(70)
      expect(largeFeature.risk).toBe('HIGH')
    })

    it('should lower score for well-tested features', () => {
      // Create feature with 45 files and high test coverage
      const files = []
      const roles = {}

      // 25 source files
      for (let i = 0; i < 25; i++) {
        files.push(`src/well-tested/component${i}.mjs`)
      }
      // 20 test files (44% of total)
      for (let i = 0; i < 20; i++) {
        const testFile = `src/well-tested/component${i}.test.mjs`
        files.push(testFile)
        roles[testFile] = 'Test'
      }

      const projectStore = createProjectStore({
        features: {
          'well-tested': { files, roles },
        },
      })

      const result = analyzeHotspots({ projectStore })

      const wellTested = result.hotspots[0]
      expect(wellTested.metrics.fileCount).toBe(45)
      expect(wellTested.metrics.testCount).toBe(20)
      // Test coverage should reduce the score
      expect(wellTested.score).toBeLessThan(80)
    })

    it('should identify top risks with high complexity and low coverage', () => {
      const projectStore = createProjectStore({
        features: {
          // High risk: many files, no tests
          checkout: {
            files: Array.from({ length: 35 }, (_, i) => `src/checkout/file${i}.mjs`),
            dependencies: ['payment', 'shipping'],
          },
          // Medium risk: moderate files, some tests
          catalog: {
            files: [
              ...Array.from({ length: 15 }, (_, i) => `src/catalog/file${i}.mjs`),
              ...Array.from({ length: 5 }, (_, i) => `src/catalog/file${i}.test.mjs`),
            ],
            roles: Object.fromEntries(
              Array.from({ length: 5 }, (_, i) => [`src/catalog/file${i}.test.mjs`, 'Test'])
            ),
          },
          // Low risk: few files, well tested
          utils: {
            files: [
              'src/utils/helpers.mjs',
              'src/utils/helpers.test.mjs',
            ],
            roles: { 'src/utils/helpers.test.mjs': 'Test' },
          },
        },
      })

      const result = analyzeHotspots({ projectStore })

      // Top risk should be checkout (many files + no tests)
      expect(result.topRisks.length).toBeGreaterThan(0)
      const topRisk = result.topRisks[0]
      expect(topRisk.feature).toBe('checkout')
      expect(topRisk.score).toBeGreaterThan(70)
    })

    it('should generate appropriate recommendations', () => {
      const projectStore = createProjectStore({
        features: {
          'high-risk': {
            files: Array.from({ length: 50 }, (_, i) => `src/high-risk/file${i}.mjs`),
            byteSizes: Object.fromEntries(
              Array.from({ length: 50 }, (_, i) => [`src/high-risk/file${i}.mjs`, 10000])
            ),
          },
        },
      })

      const result = analyzeHotspots({ projectStore })

      const hotspot = result.hotspots[0]
      expect(hotspot.recommendation).toContain('Add tests or refactor')
    })

    it('should sort hotspots by score descending', () => {
      const projectStore = createProjectStore({
        features: {
          small: {
            files: ['src/small/a.mjs', 'src/small/a.test.mjs'],
            roles: { 'src/small/a.test.mjs': 'Test' },
          },
          medium: {
            files: Array.from({ length: 15 }, (_, i) => `src/medium/file${i}.mjs`),
          },
          large: {
            files: Array.from({ length: 40 }, (_, i) => `src/large/file${i}.mjs`),
          },
        },
      })

      const result = analyzeHotspots({ projectStore })

      // Should be sorted: large (highest), medium, small (lowest)
      expect(result.hotspots[0].feature).toBe('large')
      expect(result.hotspots[1].feature).toBe('medium')
      expect(result.hotspots[2].feature).toBe('small')

      // Verify descending order
      for (let i = 1; i < result.hotspots.length; i++) {
        expect(result.hotspots[i - 1].score).toBeGreaterThanOrEqual(result.hotspots[i].score)
      }
    })

    it('should handle features with domain store relationships', () => {
      const projectStore = createProjectStore({
        features: {
          orders: {
            files: Array.from({ length: 20 }, (_, i) => `src/orders/file${i}.mjs`),
          },
        },
      })

      // Domain store with entity relationships
      const domainStore = new Store()
      domainStore.addQuad(
        namedNode(`${NS.dom}Order`),
        namedNode(`${NS.dom}relatesTo`),
        namedNode(`${NS.dom}User`)
      )
      domainStore.addQuad(
        namedNode(`${NS.dom}Order`),
        namedNode(`${NS.dom}relatesTo`),
        namedNode(`${NS.dom}Product`)
      )
      domainStore.addQuad(
        namedNode(`${NS.dom}Order`),
        namedNode(`${NS.dom}relatesTo`),
        namedNode(`${NS.dom}Payment`)
      )

      const result = analyzeHotspots({ projectStore, domainStore })

      expect(result.hotspots[0].metrics.dependencies).toBeGreaterThan(0)
    })

    it('should generate summary with high-risk count', () => {
      const projectStore = createProjectStore({
        features: {
          risky1: {
            files: Array.from({ length: 50 }, (_, i) => `src/risky1/file${i}.mjs`),
          },
          risky2: {
            files: Array.from({ length: 45 }, (_, i) => `src/risky2/file${i}.mjs`),
          },
          safe: {
            files: ['src/safe/index.mjs', 'src/safe/index.test.mjs'],
            roles: { 'src/safe/index.test.mjs': 'Test' },
          },
        },
      })

      const result = analyzeHotspots({ projectStore })

      expect(result.summary).toMatch(/\d+ high-risk feature/)
    })
  })

  describe('scoreFeature', () => {
    it('should return 0 for minimal metrics', () => {
      const score = scoreFeature('empty', {
        fileCount: 0,
        lineCount: 0,
        testCount: 0,
        testCoverage: 100,
        dependencies: 0,
      })

      expect(score).toBe(0)
    })

    it('should return high score for many files with no tests', () => {
      const score = scoreFeature('risky', {
        fileCount: 40,
        lineCount: 8000,
        testCount: 0,
        testCoverage: 0,
        dependencies: 15,
      })

      // 40 files (30% * 100) + 0% coverage (40% * 100) + 15 deps (20% * 100) + 200 lines/file (10% * 100)
      // = 30 + 40 + 20 + 10 = 100
      expect(score).toBe(100)
    })

    it('should return lower score with good test coverage', () => {
      const lowCoverageScore = scoreFeature('low-coverage', {
        fileCount: 20,
        lineCount: 4000,
        testCount: 2,
        testCoverage: 10,
        dependencies: 5,
      })

      const highCoverageScore = scoreFeature('high-coverage', {
        fileCount: 20,
        lineCount: 4000,
        testCount: 18,
        testCoverage: 90,
        dependencies: 5,
      })

      // Higher coverage should mean lower score
      expect(highCoverageScore).toBeLessThan(lowCoverageScore)
    })

    it('should cap scores at 100', () => {
      const score = scoreFeature('massive', {
        fileCount: 100,
        lineCount: 50000,
        testCount: 0,
        testCoverage: 0,
        dependencies: 50,
      })

      expect(score).toBeLessThanOrEqual(100)
    })

    it('should weight test coverage most heavily (40%)', () => {
      // Same metrics except coverage
      const base = {
        fileCount: 20,
        lineCount: 2000,
        dependencies: 5,
      }

      const zeroCoverage = scoreFeature('zero', {
        ...base,
        testCount: 0,
        testCoverage: 0,
      })

      const fullCoverage = scoreFeature('full', {
        ...base,
        testCount: 20,
        testCoverage: 100,
      })

      // 40% weight difference should be significant
      const difference = zeroCoverage - fullCoverage
      expect(difference).toBeGreaterThanOrEqual(35)
      expect(difference).toBeLessThanOrEqual(45)
    })

    it('should validate metrics with Zod schema', () => {
      expect(() => scoreFeature('invalid', {
        fileCount: -1,
        lineCount: 0,
        testCount: 0,
        testCoverage: 0,
        dependencies: 0,
      })).toThrow()

      expect(() => scoreFeature('invalid', {
        fileCount: 0,
        lineCount: 0,
        testCount: 0,
        testCoverage: 150, // Over 100
        dependencies: 0,
      })).toThrow()
    })
  })

  describe('integration: real project patterns', () => {
    it('should handle e-commerce project pattern', () => {
      const projectStore = createProjectStore({
        features: {
          checkout: {
            files: [
              'src/checkout/cart.mjs',
              'src/checkout/cart-item.mjs',
              'src/checkout/cart-total.mjs',
              'src/checkout/payment.mjs',
              'src/checkout/payment-form.mjs',
              'src/checkout/shipping.mjs',
              'src/checkout/shipping-calculator.mjs',
              'src/checkout/order-summary.mjs',
              'src/checkout/promo-code.mjs',
              'src/checkout/checkout-flow.mjs',
              // ... many more files
              ...Array.from({ length: 35 }, (_, i) => `src/checkout/component${i}.mjs`),
            ],
            byteSizes: {
              'src/checkout/cart.mjs': 8000,
              'src/checkout/payment.mjs': 12000,
            },
            dependencies: ['payment', 'shipping', 'inventory', 'users'],
          },
          payment: {
            files: Array.from({ length: 20 }, (_, i) => `src/payment/file${i}.mjs`),
            dependencies: ['checkout'],
          },
          catalog: {
            files: [
              'src/catalog/product-list.mjs',
              'src/catalog/product-detail.mjs',
              'src/catalog/search.mjs',
              'src/catalog/filters.mjs',
              'src/catalog/product-list.test.mjs',
              'src/catalog/search.test.mjs',
            ],
            roles: {
              'src/catalog/product-list.test.mjs': 'Test',
              'src/catalog/search.test.mjs': 'Test',
            },
          },
        },
      })

      const result = analyzeHotspots({ projectStore })

      // Checkout should be highest risk
      expect(result.hotspots[0].feature).toBe('checkout')
      expect(result.hotspots[0].risk).toBe('HIGH')

      // Catalog should be lower risk (has tests)
      const catalog = result.hotspots.find(h => h.feature === 'catalog')
      expect(catalog.risk).toBe('LOW')
    })

    it('should handle monorepo pattern with packages', () => {
      const projectStore = createProjectStore({
        features: {
          'packages/core': {
            files: Array.from({ length: 30 }, (_, i) => `packages/core/src/file${i}.mjs`),
          },
          'packages/ui': {
            files: [
              ...Array.from({ length: 25 }, (_, i) => `packages/ui/src/component${i}.mjs`),
              ...Array.from({ length: 10 }, (_, i) => `packages/ui/src/component${i}.test.mjs`),
            ],
            roles: Object.fromEntries(
              Array.from({ length: 10 }, (_, i) => [`packages/ui/src/component${i}.test.mjs`, 'Test'])
            ),
          },
          'packages/utils': {
            files: [
              'packages/utils/src/helpers.mjs',
              'packages/utils/src/helpers.test.mjs',
            ],
            roles: { 'packages/utils/src/helpers.test.mjs': 'Test' },
          },
        },
      })

      const result = analyzeHotspots({ projectStore })

      expect(result.hotspots).toHaveLength(3)
      // Core should be highest (no tests)
      expect(result.hotspots[0].feature).toBe('packages/core')
    })
  })
})
