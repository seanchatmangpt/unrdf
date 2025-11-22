/**
 * @file Auto-Test Generator tests - Chicago School TDD with real N3 stores
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  inferTestPatterns,
  generateTestSkeleton,
  scoreTestCoverage,
  generateTestFactory,
} from '../../src/project-engine/auto-test-generator.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
}

/* ========================================================================= */
/* Test Helpers                                                              */
/* ========================================================================= */

/**
 * Create fs store with files and optional content
 * @param {Array<{path: string, content?: string}>} files
 * @returns {Store}
 */
function createFsStore(files) {
  const store = new Store()
  for (const { path, content } of files) {
    const iri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(path)}`)
    store.addQuad(iri, namedNode(`${NS.fs}relativePath`), literal(path))
    if (content) {
      store.addQuad(iri, namedNode(`${NS.fs}content`), literal(content))
    }
  }
  return store
}

/**
 * Create project store with file roles
 * @param {Array<{path: string, role?: string}>} files
 * @returns {Store}
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

/**
 * Create domain store with entities and fields
 * @param {Array<{name: string, fields?: string[]}>} entities
 * @returns {Store}
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
 * Sample vitest test content
 */
const SAMPLE_VITEST_TEST = `
import { describe, it, expect, beforeEach, vi } from 'vitest'
import { User } from '../../src/user.mjs'

describe('User', () => {
  let user

  beforeEach(() => {
    user = new User()
  })

  describe('creation', () => {
    it('should create a user instance', () => {
      expect(user).toBeDefined()
    })

    it('should have default values', () => {
      expect(user.id).toBe(null)
      expect(user.email).toEqual('')
    })

    it('should validate email format', () => {
      expect(() => user.setEmail('invalid')).toThrow()
    })
  })
})
`

/**
 * Sample jest test content
 */
const SAMPLE_JEST_TEST = `
import { Product } from '../src/product'

describe('Product', () => {
  it('should create product', () => {
    const product = new Product()
    expect(product).toBeDefined()
    expect(product.name).toBe('')
  })
})
`

/* ========================================================================= */
/* inferTestPatterns Tests                                                   */
/* ========================================================================= */

describe('inferTestPatterns', () => {
  it('returns default patterns when no test files exist', () => {
    const fsStore = createFsStore([
      { path: 'src/user.mjs' },
      { path: 'src/product.mjs' },
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    expect(result.testFramework).toBe('vitest')
    expect(result.fileExtension).toBe('mjs')
    expect(result.testSuffix).toBe('test')
    expect(result.describeBlocks).toEqual([])
    expect(result.assertionPatterns).toContain('toBe')
  })

  it('detects vitest framework from import statement', () => {
    const fsStore = createFsStore([
      { path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST },
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    expect(result.testFramework).toBe('vitest')
  })

  it('extracts describe blocks from test files', () => {
    const fsStore = createFsStore([
      { path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST },
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    expect(result.describeBlocks).toContain('User')
    expect(result.describeBlocks).toContain('creation')
  })

  it('extracts assertion patterns from test files', () => {
    const fsStore = createFsStore([
      { path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST },
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    expect(result.assertionPatterns).toContain('toBeDefined')
    expect(result.assertionPatterns).toContain('toBe')
    expect(result.assertionPatterns).toContain('toEqual')
    expect(result.assertionPatterns).toContain('toThrow')
  })

  it('detects beforeEach usage', () => {
    const fsStore = createFsStore([
      { path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST },
      { path: 'test/product.test.mjs', content: `
        import { describe, it, expect, beforeEach } from 'vitest'
        describe('Product', () => {
          beforeEach(() => {})
          it('works', () => expect(true).toBe(true))
        })
      `},
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    // Both files use beforeEach, so threshold (>30%) should be met
    expect(result.setupTeardown.hasBeforeEach).toBe(true)
  })

  it('detects file extension from existing tests (.mjs)', () => {
    const fsStore = createFsStore([
      { path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST },
      { path: 'test/product.test.mjs', content: SAMPLE_VITEST_TEST },
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    expect(result.fileExtension).toBe('mjs')
    expect(result.testSuffix).toBe('test')
  })

  it('detects .spec suffix when more common', () => {
    const fsStore = createFsStore([
      { path: 'test/user.spec.mjs', content: SAMPLE_VITEST_TEST },
      { path: 'test/product.spec.mjs', content: SAMPLE_VITEST_TEST },
      { path: 'test/order.spec.mjs', content: SAMPLE_VITEST_TEST },
      { path: 'test/cart.test.mjs', content: SAMPLE_VITEST_TEST },
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    expect(result.testSuffix).toBe('spec')
  })

  it('extracts import statements', () => {
    const fsStore = createFsStore([
      { path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST },
    ])
    const projectStore = createProjectStore([])

    const result = inferTestPatterns({ fsStore, projectStore })

    expect(result.imports.length).toBeGreaterThan(0)
    expect(result.imports.some(i => i.includes('vitest'))).toBe(true)
  })
})

/* ========================================================================= */
/* generateTestSkeleton Tests                                                */
/* ========================================================================= */

describe('generateTestSkeleton', () => {
  const defaultPatterns = {
    describeBlocks: ['User', 'creation'],
    assertionPatterns: ['toBe', 'toEqual', 'toBeDefined', 'toThrow'],
    setupTeardown: {
      hasBeforeEach: true,
      hasAfterEach: false,
      hasBeforeAll: false,
      hasAfterAll: false,
    },
    imports: [],
    testFramework: 'vitest',
    fileExtension: 'mjs',
    testSuffix: 'test',
  }

  it('generates test skeleton with correct filename', () => {
    const result = generateTestSkeleton({
      entity: 'User',
      existingTestPatterns: defaultPatterns,
    })

    expect(result.filename).toBe('user.test.mjs')
    expect(result.entity).toBe('User')
  })

  it('generates test skeleton with kebab-case filename for PascalCase entity', () => {
    const result = generateTestSkeleton({
      entity: 'UserProfile',
      existingTestPatterns: defaultPatterns,
    })

    expect(result.filename).toBe('user-profile.test.mjs')
  })

  it('generates vitest imports', () => {
    const result = generateTestSkeleton({
      entity: 'User',
      existingTestPatterns: defaultPatterns,
    })

    expect(result.content).toContain("import { describe, it, expect, beforeEach } from 'vitest'")
  })

  it('generates entity import', () => {
    const result = generateTestSkeleton({
      entity: 'User',
      existingTestPatterns: defaultPatterns,
    })

    expect(result.content).toContain("import { User } from '../../src/user.mjs'")
  })

  it('generates describe block with entity name', () => {
    const result = generateTestSkeleton({
      entity: 'Product',
      existingTestPatterns: defaultPatterns,
    })

    expect(result.content).toContain("describe('Product'")
  })

  it('generates beforeEach when pattern detected', () => {
    const result = generateTestSkeleton({
      entity: 'Order',
      existingTestPatterns: {
        ...defaultPatterns,
        setupTeardown: { ...defaultPatterns.setupTeardown, hasBeforeEach: true },
      },
    })

    expect(result.content).toContain('beforeEach')
    expect(result.content).toContain('let order')
  })

  it('skips beforeEach when not in patterns', () => {
    const result = generateTestSkeleton({
      entity: 'Order',
      existingTestPatterns: {
        ...defaultPatterns,
        setupTeardown: { hasBeforeEach: false, hasAfterEach: false, hasBeforeAll: false, hasAfterAll: false },
      },
    })

    expect(result.content).not.toContain('beforeEach')
  })

  it('generates field tests when domainStore provided', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email', 'name'] },
    ])

    const result = generateTestSkeleton({
      entity: 'User',
      existingTestPatterns: defaultPatterns,
      domainStore,
    })

    expect(result.suggestedTests).toContain('should have id property')
    expect(result.suggestedTests).toContain('should have email property')
    expect(result.content).toContain('user.id')
  })

  it('generates core existence test', () => {
    const result = generateTestSkeleton({
      entity: 'Cart',
      existingTestPatterns: defaultPatterns,
    })

    expect(result.suggestedTests).toContain('should create Cart instance')
    expect(result.content).toContain('createCart()')
    expect(result.content).toContain('expect(cart).toBeDefined()')
  })

  it('returns suggested test names', () => {
    const result = generateTestSkeleton({
      entity: 'Payment',
      existingTestPatterns: defaultPatterns,
    })

    expect(result.suggestedTests.length).toBeGreaterThan(0)
    expect(result.suggestedTests[0]).toBe('should create Payment instance')
  })

  it('uses .spec suffix when pattern indicates', () => {
    const result = generateTestSkeleton({
      entity: 'User',
      existingTestPatterns: { ...defaultPatterns, testSuffix: 'spec' },
    })

    expect(result.filename).toBe('user.spec.mjs')
  })

  it('uses .ts extension when pattern indicates', () => {
    const result = generateTestSkeleton({
      entity: 'User',
      existingTestPatterns: { ...defaultPatterns, fileExtension: 'ts' },
    })

    expect(result.filename).toBe('user.test.ts')
    // TypeScript imports don't include extension
    expect(result.content).toContain("import { User } from '../../src/user'")
  })
})

/* ========================================================================= */
/* scoreTestCoverage Tests                                                   */
/* ========================================================================= */

describe('scoreTestCoverage', () => {
  it('returns 0% coverage when no tests match entity', () => {
    const result = scoreTestCoverage({
      entity: 'User',
      testFiles: ['test/product.test.mjs', 'test/order.test.mjs'],
    })

    expect(result.coverage).toBe(0)
    expect(result.needsTests).toBe(true)
    expect(result.existingTests).toEqual([])
  })

  it('returns positive coverage when tests match entity name', () => {
    const result = scoreTestCoverage({
      entity: 'User',
      testFiles: ['test/user.test.mjs', 'test/product.test.mjs'],
    })

    expect(result.coverage).toBeGreaterThan(0)
    expect(result.existingTests).toContain('test/user.test.mjs')
  })

  it('matches kebab-case filenames to PascalCase entities', () => {
    const result = scoreTestCoverage({
      entity: 'UserProfile',
      testFiles: ['test/user-profile.test.mjs'],
    })

    expect(result.existingTests).toContain('test/user-profile.test.mjs')
    expect(result.coverage).toBeGreaterThan(0)
  })

  it('matches snake_case filenames to PascalCase entities', () => {
    const result = scoreTestCoverage({
      entity: 'UserProfile',
      testFiles: ['test/user_profile.test.mjs'],
    })

    expect(result.existingTests).toContain('test/user_profile.test.mjs')
  })

  it('calculates coverage based on source files', () => {
    const result = scoreTestCoverage({
      entity: 'User',
      testFiles: ['test/user.test.mjs'],
      sourceFiles: ['src/user.mjs', 'src/user-service.mjs'],
    })

    // 1 test / 2 sources = 50%
    expect(result.coverage).toBe(50)
  })

  it('caps coverage at 100%', () => {
    const result = scoreTestCoverage({
      entity: 'User',
      testFiles: ['test/user.test.mjs', 'test/user.integration.test.mjs', 'test/user.e2e.test.mjs'],
      sourceFiles: ['src/user.mjs'],
    })

    expect(result.coverage).toBe(100)
  })

  it('sets needsTests true when coverage below 80%', () => {
    const result = scoreTestCoverage({
      entity: 'User',
      testFiles: ['test/user.test.mjs'],
      sourceFiles: ['src/user.mjs', 'src/user-service.mjs', 'src/user-validator.mjs'],
    })

    expect(result.coverage).toBeLessThan(80)
    expect(result.needsTests).toBe(true)
  })

  it('sets needsTests false when coverage at or above 80%', () => {
    const result = scoreTestCoverage({
      entity: 'User',
      testFiles: ['test/user.test.mjs', 'test/user.integration.test.mjs'],
      sourceFiles: ['src/user.mjs'],
    })

    expect(result.coverage).toBeGreaterThanOrEqual(80)
    expect(result.needsTests).toBe(false)
  })

  it('suggests test count based on source files', () => {
    const result = scoreTestCoverage({
      entity: 'Product',
      testFiles: [],
      sourceFiles: ['src/product.mjs', 'src/product-service.mjs', 'src/product-validator.mjs'],
    })

    // 80% of 3 = 2.4, ceil = 3 suggested
    expect(result.suggestedCount).toBe(3)
  })

  it('reduces suggested count by existing tests', () => {
    const result = scoreTestCoverage({
      entity: 'Order',
      testFiles: ['test/order.test.mjs'],
      sourceFiles: ['src/order.mjs', 'src/order-service.mjs', 'src/order-processor.mjs'],
    })

    // 80% of 3 = 2.4, ceil = 3 ideal, minus 1 existing = 2 suggested
    expect(result.suggestedCount).toBe(2)
  })
})

/* ========================================================================= */
/* generateTestFactory Tests                                                 */
/* ========================================================================= */

describe('generateTestFactory', () => {
  it('generates factory function for entity', () => {
    const result = generateTestFactory('User')

    expect(result).toContain('function createUser(')
    expect(result).toContain('overrides = {}')
    expect(result).toContain('...overrides')
  })

  it('generates factory with PascalCase entity name', () => {
    const result = generateTestFactory('UserProfile')

    expect(result).toContain('function createUserProfile(')
    expect(result).toContain('@returns {UserProfile}')
  })
})

/* ========================================================================= */
/* Integration Tests                                                         */
/* ========================================================================= */

describe('integration: full test generation workflow', () => {
  it('generates tests matching inferred patterns from real test files', () => {
    // Step 1: Create fs store with existing test files
    const fsStore = createFsStore([
      { path: 'src/user.mjs', content: 'export class User {}' },
      { path: 'src/product.mjs', content: 'export class Product {}' },
      { path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST },
      { path: 'test/product.test.mjs', content: `
        import { describe, it, expect, beforeEach } from 'vitest'
        describe('Product', () => {
          beforeEach(() => {})
          it('creates product', () => {
            expect(true).toBe(true)
          })
        })
      `},
    ])
    const projectStore = createProjectStore([
      { path: 'test/user.test.mjs', role: 'Test' },
      { path: 'test/product.test.mjs', role: 'Test' },
    ])

    // Step 2: Infer patterns
    const patterns = inferTestPatterns({ fsStore, projectStore })

    expect(patterns.testFramework).toBe('vitest')
    expect(patterns.fileExtension).toBe('mjs')
    expect(patterns.setupTeardown.hasBeforeEach).toBe(true)

    // Step 3: Create domain store for new entity
    const domainStore = createDomainStore([
      { name: 'Order', fields: ['id', 'userId', 'total', 'status'] },
    ])

    // Step 4: Generate test skeleton
    const generated = generateTestSkeleton({
      entity: 'Order',
      existingTestPatterns: patterns,
      domainStore,
    })

    expect(generated.filename).toBe('order.test.mjs')
    expect(generated.content).toContain("import { describe, it, expect, beforeEach } from 'vitest'")
    expect(generated.content).toContain("describe('Order'")
    expect(generated.content).toContain('beforeEach')
    expect(generated.suggestedTests).toContain('should have id property')
    expect(generated.suggestedTests).toContain('should have userId property')
  })

  it('scores coverage and identifies entities needing tests', () => {
    const testFiles = [
      'test/user.test.mjs',
      'test/user.integration.test.mjs',
      'test/product.test.mjs',
    ]

    // User has good coverage
    const userCoverage = scoreTestCoverage({
      entity: 'User',
      testFiles,
      sourceFiles: ['src/user.mjs'],
    })
    expect(userCoverage.needsTests).toBe(false)

    // Order has no coverage
    const orderCoverage = scoreTestCoverage({
      entity: 'Order',
      testFiles,
      sourceFiles: ['src/order.mjs', 'src/order-service.mjs'],
    })
    expect(orderCoverage.needsTests).toBe(true)
    expect(orderCoverage.suggestedCount).toBeGreaterThan(0)
  })
})
