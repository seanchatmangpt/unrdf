/**
 * @file Refactoring guide tests - Chicago School TDD with real collaborators
 * @vitest-environment node
 *
 * Chicago School TDD: Tests use REAL N3 stores with actual data.
 * NO mocks - test real refactoring plan generation.
 */

import { describe, it, expect } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  planEntityRename,
  planEntityMerge,
  planServiceExtraction,
  validateRefactoringPlan,
} from '../../src/project-engine/refactoring-guide.mjs'

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
 * Create domain store with entities and relationships
 * @param {Array<{name: string, fields?: string[], relations?: string[]}>} entities
 */
function createDomainStore(entities) {
  const store = new Store()
  for (const { name, fields = [], relations = [] } of entities) {
    const iri = namedNode(`${NS.dom}${name}`)
    store.addQuad(iri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`))
    store.addQuad(iri, namedNode(`${NS.rdfs}label`), literal(name))

    for (const field of fields) {
      store.addQuad(iri, namedNode(`${NS.dom}hasField`), namedNode(`${NS.dom}${name}.${field}`))
    }

    for (const relation of relations) {
      store.addQuad(iri, namedNode(`${NS.dom}relatesTo`), namedNode(`${NS.dom}${relation}`))
    }
  }
  return store
}

/**
 * Create project store with classified files
 * @param {Array<{path: string, role?: string}>} files
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

/* ========================================================================= */
/* planEntityRename Tests                                                    */
/* ========================================================================= */

describe('planEntityRename', () => {
  it('generates file rename operations for entity files', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email', 'name'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/api/user.ts', role: 'Api' },
      { path: 'src/components/UserView.tsx', role: 'Component' },
    ])

    const result = planEntityRename('User', 'Account', projectStore, domainStore)

    expect(result.plan.files.length).toBeGreaterThan(0)
    expect(result.plan.files.some(f => f.oldValue.includes('user'))).toBe(true)
    expect(result.plan.files.some(f => f.newValue.includes('account'))).toBe(true)
    expect(result.summary).toContain('User')
    expect(result.summary).toContain('Account')
  })

  it('generates import update operations', () => {
    const domainStore = createDomainStore([
      { name: 'Product', fields: ['id', 'name', 'price'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/product.ts', role: 'Schema' },
      { path: 'src/api/products.ts', role: 'Api' },
    ])

    const result = planEntityRename('Product', 'Item', projectStore, domainStore)

    expect(result.plan.imports.length).toBeGreaterThan(0)
    expect(result.plan.imports.some(i => i.oldValue === 'Product' && i.newValue === 'Item')).toBe(true)
  })

  it('generates reference update operations', () => {
    const domainStore = createDomainStore([
      { name: 'Order', fields: ['id', 'total'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/services/order-service.ts', role: 'Service' },
    ])

    const result = planEntityRename('Order', 'Purchase', projectStore, domainStore)

    expect(result.plan.references.length).toBeGreaterThan(0)
    expect(result.plan.references[0].oldValue).toBe('Order')
    expect(result.plan.references[0].newValue).toBe('Purchase')
  })

  it('identifies test files that need updating', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'test/user.test.ts', role: 'Test' },
      { path: 'test/user-integration.test.ts', role: 'Test' },
    ])

    const result = planEntityRename('User', 'Account', projectStore, domainStore)

    expect(result.plan.tests.length).toBe(2)
    expect(result.plan.tests.every(t => t.type === 'update-test')).toBe(true)
  })

  it('warns when entity has relations', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'], relations: ['Order', 'Profile'] },
      { name: 'Order', fields: ['id'] },
      { name: 'Profile', fields: ['id'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])

    const result = planEntityRename('User', 'Account', projectStore, domainStore)

    expect(result.warnings.some(w => w.includes('relations'))).toBe(true)
    expect(result.warnings.some(w => w.includes('Order') || w.includes('Profile'))).toBe(true)
  })

  it('warns about dependent entities', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
      { name: 'Order', fields: ['id', 'userId'], relations: ['User'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/models/order.ts', role: 'Schema' },
    ])

    const result = planEntityRename('User', 'Account', projectStore, domainStore)

    expect(result.warnings.some(w => w.includes('depend') && w.includes('Order'))).toBe(true)
  })

  it('calculates risk level based on complexity', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'], relations: ['Order', 'Profile', 'Cart', 'Wishlist'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])

    const result = planEntityRename('User', 'Account', projectStore, domainStore)

    expect(['high', 'critical']).toContain(result.risk)
  })

  it('handles entity not found in domain model', () => {
    const domainStore = createDomainStore([])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])

    const result = planEntityRename('User', 'Account', projectStore, domainStore)

    expect(result.warnings.some(w => w.includes('not found'))).toBe(true)
  })
})

/* ========================================================================= */
/* planEntityMerge Tests                                                     */
/* ========================================================================= */

describe('planEntityMerge', () => {
  it('generates merge plan for two entities', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email'] },
      { name: 'Profile', fields: ['bio', 'avatar'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/models/profile.ts', role: 'Schema' },
    ])

    const result = planEntityMerge(['User', 'Profile'], 'UserProfile', projectStore, domainStore)

    expect(result.plan.files.length).toBeGreaterThan(0)
    expect(result.summary).toContain('User')
    expect(result.summary).toContain('Profile')
    expect(result.summary).toContain('UserProfile')
  })

  it('marks secondary entity files for merge-delete', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
      { name: 'Profile', fields: ['bio'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/models/profile.ts', role: 'Schema' },
    ])

    const result = planEntityMerge(['User', 'Profile'], 'UserProfile', projectStore, domainStore)

    const mergeDeleteOps = result.plan.files.filter(f => f.type === 'merge-delete')
    expect(mergeDeleteOps.length).toBeGreaterThan(0)
  })

  it('warns about duplicate field names', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'name'] },
      { name: 'Profile', fields: ['id', 'name', 'bio'] }, // 'id' and 'name' are duplicates
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/models/profile.ts', role: 'Schema' },
    ])

    const result = planEntityMerge(['User', 'Profile'], 'UserProfile', projectStore, domainStore)

    expect(result.warnings.some(w => w.includes('Duplicate field'))).toBe(true)
  })

  it('calculates higher risk for merges', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
      { name: 'Profile', fields: ['bio'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/models/profile.ts', role: 'Schema' },
    ])

    const result = planEntityMerge(['User', 'Profile'], 'UserProfile', projectStore, domainStore)

    expect(['medium', 'high', 'critical']).toContain(result.risk)
  })

  it('handles merging more than two entities', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
      { name: 'Profile', fields: ['bio'] },
      { name: 'Settings', fields: ['theme'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/models/profile.ts', role: 'Schema' },
      { path: 'src/models/settings.ts', role: 'Schema' },
    ])

    const result = planEntityMerge(['User', 'Profile', 'Settings'], 'UserAccount', projectStore, domainStore)

    expect(result.plan.files.length).toBe(3)
    expect(result.summary).toContain('3 fields combined')
  })
})

/* ========================================================================= */
/* planServiceExtraction Tests                                               */
/* ========================================================================= */

describe('planServiceExtraction', () => {
  it('generates new service file creation', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/api/user.ts', role: 'Api' },
    ])

    const result = planServiceExtraction('User', ['create', 'update', 'delete'], projectStore)

    expect(result.plan.files.length).toBe(1)
    expect(result.plan.files[0].type).toBe('create')
    expect(result.plan.files[0].newValue).toContain('service')
  })

  it('generates import additions for existing files', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/api/user.ts', role: 'Api' },
    ])

    const result = planServiceExtraction('User', ['create', 'update'], projectStore)

    expect(result.plan.imports.length).toBeGreaterThan(0)
    expect(result.plan.imports.some(i => i.type === 'add-import')).toBe(true)
    expect(result.plan.imports.some(i => i.newValue.includes('UserService'))).toBe(true)
  })

  it('generates reference updates for method calls', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])

    const methods = ['validateEmail', 'hashPassword']
    const result = planServiceExtraction('User', methods, projectStore)

    expect(result.plan.references.length).toBe(methods.length)
    expect(result.plan.references.every(r => r.type === 'update-reference')).toBe(true)
    expect(result.plan.references.some(r => r.oldValue.includes('validateEmail'))).toBe(true)
  })

  it('generates test file for new service', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])

    const result = planServiceExtraction('User', ['create'], projectStore)

    expect(result.plan.tests.length).toBe(1)
    expect(result.plan.tests[0].type).toBe('create-test')
    expect(result.plan.tests[0].newValue).toContain('test')
  })

  it('calculates risk based on number of methods', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])

    const lowRisk = planServiceExtraction('User', ['create'], projectStore)
    const highRisk = planServiceExtraction('User', ['a', 'b', 'c', 'd', 'e', 'f'], projectStore)

    expect(lowRisk.risk).toBe('low')
    expect(highRisk.risk).toBe('high')
  })

  it('warns when no methods specified', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])

    const result = planServiceExtraction('User', [], projectStore)

    expect(result.warnings.some(w => w.includes('No methods'))).toBe(true)
  })
})

/* ========================================================================= */
/* validateRefactoringPlan Tests                                             */
/* ========================================================================= */

describe('validateRefactoringPlan', () => {
  it('validates a correct plan', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])
    const plan = {
      files: [{ type: 'rename', file: 'src/models/user.ts', oldValue: 'src/models/user.ts', newValue: 'src/models/account.ts' }],
      imports: [{ type: 'update-import', file: 'src/models/account.ts', oldValue: 'User', newValue: 'Account' }],
      references: [{ type: 'update-reference', file: 'src/models/account.ts', oldValue: 'User', newValue: 'Account' }],
      tests: [{ type: 'update-test', file: 'test/user.test.ts', oldValue: 'User', newValue: 'Account' }],
      docs: [],
    }

    const result = validateRefactoringPlan(plan, projectStore)

    expect(result.valid).toBe(true)
    expect(result.errors).toHaveLength(0)
  })

  it('detects path conflicts', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/models/profile.ts', role: 'Schema' },
    ])
    const plan = {
      files: [
        { type: 'rename', file: 'src/models/user.ts', oldValue: 'src/models/user.ts', newValue: 'src/models/account.ts' },
        { type: 'rename', file: 'src/models/profile.ts', oldValue: 'src/models/profile.ts', newValue: 'src/models/account.ts' }, // Conflict!
      ],
      imports: [],
      references: [],
      tests: [],
      docs: [],
    }

    const result = validateRefactoringPlan(plan, projectStore)

    expect(result.valid).toBe(false)
    expect(result.errors.some(e => e.includes('conflict'))).toBe(true)
    expect(result.checks.noPathConflicts).toBe(false)
  })

  it('warns when source files not found', () => {
    const projectStore = createProjectStore([])
    const plan = {
      files: [{ type: 'rename', file: 'src/models/nonexistent.ts', oldValue: 'src/models/nonexistent.ts', newValue: 'src/models/new.ts' }],
      imports: [],
      references: [],
      tests: [],
      docs: [],
    }

    const result = validateRefactoringPlan(plan, projectStore)

    expect(result.warnings.some(w => w.includes('not found'))).toBe(true)
  })

  it('warns when no tests are updated', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])
    const plan = {
      files: [{ type: 'rename', file: 'src/models/user.ts', oldValue: 'src/models/user.ts', newValue: 'src/models/account.ts' }],
      imports: [],
      references: [],
      tests: [],
      docs: [],
    }

    const result = validateRefactoringPlan(plan, projectStore)

    expect(result.warnings.some(w => w.includes('test'))).toBe(true)
    expect(result.checks.testsUpdated).toBe(false)
  })

  it('warns when no docs are updated', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])
    const plan = {
      files: [{ type: 'rename', file: 'src/models/user.ts', oldValue: 'src/models/user.ts', newValue: 'src/models/account.ts' }],
      imports: [],
      references: [],
      tests: [{ type: 'update-test', file: 'test/user.test.ts', oldValue: 'User', newValue: 'Account' }],
      docs: [],
    }

    const result = validateRefactoringPlan(plan, projectStore)

    expect(result.warnings.some(w => w.includes('documentation'))).toBe(true)
    expect(result.checks.docsUpdated).toBe(false)
  })

  it('warns on empty plan', () => {
    const projectStore = createProjectStore([])
    const plan = {
      files: [],
      imports: [],
      references: [],
      tests: [],
      docs: [],
    }

    const result = validateRefactoringPlan(plan, projectStore, true)

    expect(result.valid).toBe(true) // Empty plan is valid, just useless
    expect(result.warnings.some(w => w.includes('empty'))).toBe(true)
  })

  it('validates import resolvability', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
    ])
    const plan = {
      files: [],
      imports: [{ type: 'update-import', file: 'src/nonexistent/file.ts', oldValue: 'User', newValue: 'Account' }],
      references: [],
      tests: [],
      docs: [],
    }

    const result = validateRefactoringPlan(plan, projectStore)

    expect(result.warnings.some(w => w.includes('may not exist'))).toBe(true)
    expect(result.checks.allImportsResolvable).toBe(false)
  })
})

/* ========================================================================= */
/* Integration Test                                                          */
/* ========================================================================= */

describe('refactoring-guide integration', () => {
  it('full refactoring workflow: rename -> validate', () => {
    // Setup: Real e-commerce domain model
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email', 'name', 'createdAt'], relations: ['Order', 'Cart'] },
      { name: 'Order', fields: ['id', 'userId', 'total', 'status'], relations: ['User', 'Product'] },
      { name: 'Cart', fields: ['id', 'userId', 'items'], relations: ['User', 'Product'] },
      { name: 'Product', fields: ['id', 'name', 'price', 'stock'] },
    ])

    const projectStore = createProjectStore([
      // User files
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/api/users.ts', role: 'Api' },
      { path: 'src/components/UserProfile.tsx', role: 'Component' },
      { path: 'test/user.test.ts', role: 'Test' },
      { path: 'docs/user.md', role: 'Doc' },
      // Related entity files
      { path: 'src/models/order.ts', role: 'Schema' },
      { path: 'src/models/cart.ts', role: 'Schema' },
    ])

    // Step 1: Plan the rename
    const renameResult = planEntityRename('User', 'Customer', projectStore, domainStore)

    expect(renameResult.plan.files.length).toBeGreaterThan(0)
    expect(renameResult.plan.tests.length).toBeGreaterThan(0)
    expect(renameResult.plan.docs.length).toBeGreaterThan(0)
    expect(renameResult.warnings.some(w => w.includes('Order'))).toBe(true)
    expect(renameResult.warnings.some(w => w.includes('Cart'))).toBe(true)

    // Step 2: Validate the plan
    const validationResult = validateRefactoringPlan(renameResult.plan, projectStore)

    expect(validationResult.valid).toBe(true)
    expect(validationResult.checks.noCircularDeps).toBe(true)
    expect(validationResult.checks.noPathConflicts).toBe(true)

    // Step 3: Check risk assessment
    expect(['medium', 'high', 'critical']).toContain(renameResult.risk)
  })

  it('full refactoring workflow: merge -> validate', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email', 'password'] },
      { name: 'Profile', fields: ['bio', 'avatar', 'settings'] },
    ])

    // Use non-overlapping file paths to avoid expected path conflicts
    const projectStore = createProjectStore([
      { path: 'src/entities/user/model.ts', role: 'Schema' },
      { path: 'src/entities/profile/model.ts', role: 'Schema' },
    ])

    // Step 1: Plan the merge
    const mergeResult = planEntityMerge(['User', 'Profile'], 'Account', projectStore, domainStore)

    expect(mergeResult.plan.files.length).toBeGreaterThan(0)
    expect(mergeResult.summary.toLowerCase()).toContain('merge')

    // Step 2: Validate - merge plans create files, validation checks structure
    const validationResult = validateRefactoringPlan(mergeResult.plan, projectStore)

    // Merge validation should not have circular deps
    expect(validationResult.checks.noCircularDeps).toBe(true)
    // Files with non-overlapping paths should not conflict
    expect(validationResult.checks.noPathConflicts).toBe(true)
  })

  it('full refactoring workflow: extract service -> validate', () => {
    const projectStore = createProjectStore([
      { path: 'src/models/user.ts', role: 'Schema' },
      { path: 'src/api/users.ts', role: 'Api' },
      { path: 'src/services/auth-service.ts', role: 'Service' },
    ])

    // Step 1: Plan service extraction
    const extractResult = planServiceExtraction(
      'User',
      ['validateCredentials', 'hashPassword', 'generateToken'],
      projectStore
    )

    expect(extractResult.plan.files.length).toBe(1)
    expect(extractResult.plan.files[0].newValue).toContain('service')
    expect(extractResult.plan.tests.length).toBe(1)

    // Step 2: Validate
    const validationResult = validateRefactoringPlan(extractResult.plan, projectStore)

    // Service extraction creates new files, so it's always valid
    expect(validationResult.valid).toBe(true)
  })
})
