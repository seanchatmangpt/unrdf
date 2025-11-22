/**
 * @file Documentation drift checker tests - Chicago School TDD with real collaborators
 * @vitest-environment node
 *
 * Tests use real markdown parsing, real N3 stores, and real file system operations
 * Only mocked: file system for isolation (using temp directories)
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest'
import { promises as fs } from 'fs'
import path from 'path'
import os from 'os'
import { Store, DataFactory } from 'n3'
import {
  checkDocConsistency,
  extractDocReferences,
  scoreDocDrift,
} from '../../src/project-engine/doc-drift-checker.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
}

/* ========================================================================= */
/* Test Fixtures                                                             */
/* ========================================================================= */

/**
 * Create domain store with entities and fields
 * @param {Array<{name: string, fields?: string[], relations?: string[]}>} entities
 */
function createDomainStore(entities) {
  const store = new Store()
  for (const { name, fields = [], relations = [] } of entities) {
    const iri = namedNode(`${NS.dom}${name}`)
    store.addQuad(iri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`))
    store.addQuad(iri, namedNode(`${NS.rdfs}label`), literal(name))

    for (const field of fields) {
      const fieldIri = namedNode(`${NS.dom}${name}.${field}`)
      store.addQuad(iri, namedNode(`${NS.dom}hasField`), fieldIri)
      store.addQuad(fieldIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Field`))
      store.addQuad(fieldIri, namedNode(`${NS.dom}fieldName`), literal(field))
    }

    for (const relation of relations) {
      store.addQuad(iri, namedNode(`${NS.dom}relatesTo`), namedNode(`${NS.dom}${relation}`))
    }
  }
  return store
}

/**
 * Create temp directory for test files - unique per test
 */
async function createTempDir() {
  const uniqueId = `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`
  const dir = path.join(os.tmpdir(), `doc-drift-test-${uniqueId}`)
  await fs.mkdir(dir, { recursive: true })
  await fs.mkdir(path.join(dir, 'docs'), { recursive: true })
  return dir
}

/**
 * Clean up temp directory
 */
async function cleanupTempDir(dir) {
  try {
    await fs.rm(dir, { recursive: true, force: true })
  } catch {
    // Ignore cleanup errors
  }
}

/* ========================================================================= */
/* extractDocReferences Tests                                                */
/* ========================================================================= */

describe('extractDocReferences', () => {
  it('extracts PascalCase entity references from markdown', () => {
    const markdown = `
# User Management

The User entity represents authenticated users in the system.
Each User has a unique identifier and email address.
`
    const result = extractDocReferences(markdown)

    expect(result.entities).toContain('User')
  })

  it('extracts entity.field references from code blocks', () => {
    const markdown = `
## API Reference

- \`User.email\` - The user's email address
- \`User.name\` - The user's display name
`
    const result = extractDocReferences(markdown)

    expect(result.fields).toContainEqual({ entity: 'User', field: 'email' })
    expect(result.fields).toContainEqual({ entity: 'User', field: 'name' })
  })

  it('extracts entities from API endpoint references', () => {
    const markdown = `
## Endpoints

- GET /users - List all users
- GET /products/:id - Get product by ID
- POST /order-items - Create order item
`
    const result = extractDocReferences(markdown)

    expect(result.entities).toContain('User')
    expect(result.entities).toContain('Product')
    expect(result.entities).toContain('OrderItem')
  })

  it('extracts entities from Schema references', () => {
    const markdown = `
## Validation

Use \`UserSchema\` for user input validation.
The \`ProductSchema\` validates product data.
`
    const result = extractDocReferences(markdown)

    expect(result.entities).toContain('User')
    expect(result.entities).toContain('Product')
  })

  it('filters out common non-entity words', () => {
    const markdown = `
# Introduction

This documentation describes the API.
See the Example section for usage.
`
    const result = extractDocReferences(markdown)

    expect(result.entities).not.toContain('This')
    expect(result.entities).not.toContain('See')
    expect(result.entities).not.toContain('Example')
  })

  it('tracks line numbers for references', () => {
    const markdown = `Line 1
Line 2
The UserAccount entity is defined here.
Line 4`
    const result = extractDocReferences(markdown)

    const userPattern = result.patterns.find(p => p.pattern === 'UserAccount')
    expect(userPattern).toBeDefined()
    expect(userPattern.line).toBe(3)
  })
})

/* ========================================================================= */
/* scoreDocDrift Tests                                                       */
/* ========================================================================= */

describe('scoreDocDrift', () => {
  it('returns 100 score when all references are valid', () => {
    const domainModel = {
      entities: new Map([
        ['User', new Set(['id', 'email', 'name'])],
        ['Product', new Set(['id', 'title', 'price'])],
      ]),
      relations: new Map([['User', []], ['Product', []]]),
    }

    const docRefs = {
      entities: ['User', 'Product'],
      fields: [{ entity: 'User', field: 'email' }],
      patterns: [],
    }

    const result = scoreDocDrift(domainModel, docRefs)

    expect(result.score).toBe(100)
    expect(result.driftSeverity).toBe('none')
    expect(result.outdatedReferences).toHaveLength(0)
  })

  it('returns 0 score when all references are invalid', () => {
    const domainModel = {
      entities: new Map([['User', new Set(['id'])]]),
      relations: new Map([['User', []]]),
    }

    const docRefs = {
      entities: ['Customer', 'Account'], // Neither exists
      fields: [],
      patterns: [],
    }

    const result = scoreDocDrift(domainModel, docRefs)

    expect(result.score).toBe(0)
    expect(result.driftSeverity).toBe('major')
    expect(result.outdatedReferences).toHaveLength(2)
  })

  it('detects renamed entity with suggestion', () => {
    const domainModel = {
      entities: new Map([['Account', new Set(['id'])]]),
      relations: new Map([['Account', []]]),
    }

    const docRefs = {
      entities: ['Acount'], // Typo: missing 'c'
      fields: [],
      patterns: [],
    }

    const result = scoreDocDrift(domainModel, docRefs)

    expect(result.outdatedReferences).toContainEqual(
      expect.objectContaining({
        entity: 'Acount',
        reference: expect.stringContaining('Did you mean'),
      })
    )
  })

  it('detects missing field with suggestion', () => {
    const domainModel = {
      entities: new Map([['User', new Set(['email', 'name'])]]),
      relations: new Map([['User', []]]),
    }

    const docRefs = {
      entities: ['User'],
      fields: [{ entity: 'User', field: 'mail' }], // Should be 'email'
      patterns: [],
    }

    const result = scoreDocDrift(domainModel, docRefs)

    expect(result.outdatedReferences).toContainEqual(
      expect.objectContaining({
        entity: 'User',
        reference: expect.stringContaining('Did you mean'),
      })
    )
  })

  it('calculates partial score for mixed valid/invalid references', () => {
    const domainModel = {
      entities: new Map([
        ['User', new Set(['id', 'email'])],
        ['Product', new Set(['id', 'name'])],
      ]),
      relations: new Map([['User', []], ['Product', []]]),
    }

    const docRefs = {
      entities: ['User', 'Customer'], // 1 valid, 1 invalid
      fields: [],
      patterns: [],
    }

    const result = scoreDocDrift(domainModel, docRefs)

    expect(result.score).toBe(50) // 1/2 = 50%
    expect(result.driftSeverity).toBe('minor')
  })

  it('returns severity "minor" for score 50-79', () => {
    const domainModel = {
      entities: new Map([
        ['A', new Set()],
        ['B', new Set()],
        ['C', new Set()],
      ]),
      relations: new Map([['A', []], ['B', []], ['C', []]]),
    }

    const docRefs = {
      entities: ['A', 'B', 'X'], // 2/3 = 67%
      fields: [],
      patterns: [],
    }

    const result = scoreDocDrift(domainModel, docRefs)

    expect(result.score).toBe(67)
    expect(result.driftSeverity).toBe('minor')
  })
})

/* ========================================================================= */
/* checkDocConsistency Integration Tests                                      */
/* ========================================================================= */

describe('checkDocConsistency', () => {
  let tempDir

  beforeEach(async () => {
    tempDir = await createTempDir()
  })

  afterEach(async () => {
    await cleanupTempDir(tempDir)
  })

  it('detects stale entity references in markdown', async () => {
    // Create domain store with 'Account' (not 'User')
    const domainStore = createDomainStore([
      { name: 'Account', fields: ['id', 'email'] },
    ])

    // Create doc that references 'User' (stale reference)
    const docContent = `
# User Guide

The User entity stores user information.
Access user data via the /users endpoint.
`
    await fs.writeFile(path.join(tempDir, 'docs', 'guide.md'), docContent)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    expect(result.issues.some(i => i.entity === 'User')).toBe(true)
    expect(result.staleReferences.some(r => r.entity === 'User')).toBe(true)
  })

  it('detects renamed entity and suggests correction', async () => {
    // Domain has 'CustomerProfile', doc references 'CustumerProfile' (typo)
    const domainStore = createDomainStore([
      { name: 'CustomerProfile', fields: ['id', 'name'] },
    ])

    const docContent = `
# Profile Management

The CustumerProfile entity manages customer data.
`
    await fs.writeFile(path.join(tempDir, 'docs', 'customer.md'), docContent)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    const renameIssue = result.issues.find(
      i => i.type === 'renamed_entity' && i.entity === 'CustumerProfile'
    )
    expect(renameIssue).toBeDefined()
    expect(renameIssue.suggestion).toContain('CustomerProfile')
  })

  it('detects missing field references', async () => {
    const domainStore = createDomainStore([
      { name: 'Product', fields: ['id', 'title', 'price'] },
    ])

    // Doc references 'name' which doesn't exist (should be 'title')
    const docContent = `
# Product API

- \`Product.id\` - Product identifier
- \`Product.name\` - Product name (STALE - should be title)
`
    await fs.writeFile(path.join(tempDir, 'docs', 'product.md'), docContent)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    expect(result.issues.some(
      i => i.message.includes('Product.name') && i.message.includes('not found')
    )).toBe(true)
  })

  it('identifies undocumented entities', async () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
      { name: 'Product', fields: ['id'] },
      { name: 'Order', fields: ['id'] }, // Not documented
    ])

    // Docs only mention User and Product
    const docContent = `
# API Reference

## User
The User entity...

## Product
The Product entity...
`
    await fs.writeFile(path.join(tempDir, 'docs', 'api.md'), docContent)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    expect(result.missingDocumentation).toContain('Order')
    expect(result.issues.some(
      i => i.type === 'undocumented_entity' && i.entity === 'Order'
    )).toBe(true)
  })

  it('returns summary with score and severity', async () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email'] },
    ])

    const docContent = `
# User Documentation

The User entity has an email field.
Access via \`User.email\`.
`
    await fs.writeFile(path.join(tempDir, 'docs', 'user.md'), docContent)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    expect(result.summary).toHaveProperty('score')
    expect(result.summary).toHaveProperty('severity')
    expect(result.summary).toHaveProperty('total')
    expect(result.summary).toHaveProperty('valid')
  })

  it('scans README.md in project root', async () => {
    const domainStore = createDomainStore([
      { name: 'Widget', fields: ['id', 'type'] },
    ])

    // Put reference in README, not docs folder
    const readmeContent = `
# My Project

This project uses the Widget entity for UI components.
`
    await fs.writeFile(path.join(tempDir, 'README.md'), readmeContent)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    // Widget should NOT be in missingDocumentation since it's in README
    expect(result.missingDocumentation).not.toContain('Widget')
  })

  it('handles empty docs directory gracefully', async () => {
    const domainStore = createDomainStore([
      { name: 'TestEntity', fields: ['id'] },
    ])

    // Create empty docs folder (no markdown files)
    const emptyDocsDir = path.join(tempDir, 'empty-docs')
    await fs.mkdir(emptyDocsDir, { recursive: true })

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'empty-docs',
    })

    expect(result.issues).toBeDefined()
    // With no docs, all domain entities are undocumented
    expect(result.missingDocumentation).toContain('TestEntity')
  })

  it('handles non-existent docs directory gracefully', async () => {
    const domainStore = createDomainStore([
      { name: 'Foo', fields: ['bar'] },
    ])

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'nonexistent-docs',
    })

    expect(result.issues).toBeDefined()
    expect(result.missingDocumentation).toContain('Foo')
  })

  it('processes nested docs directories', async () => {
    const domainStore = createDomainStore([
      { name: 'InvoiceRecord', fields: ['id', 'amount'] },
    ])

    // Create nested structure
    const nestedDir = path.join(tempDir, 'docs', 'api', 'billing')
    await fs.mkdir(nestedDir, { recursive: true })

    const docContent = `
# Billing API

The InvoiceRecord entity represents billing invoices.
`
    await fs.writeFile(path.join(nestedDir, 'invoice.md'), docContent)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    // InvoiceRecord should be found in nested docs
    expect(result.missingDocumentation).not.toContain('InvoiceRecord')
  })

  it('complete drift detection scenario', async () => {
    // Comprehensive test: domain model changed, docs are stale
    const domainStore = createDomainStore([
      { name: 'CustomerAccount', fields: ['id', 'emailAddress', 'fullName'] }, // Renamed
      { name: 'OrderLineItem', fields: ['id', 'quantity', 'unitPrice'] },      // New entity
    ])

    const staleDoc = `
# E-Commerce API

## UserAccount Entity (STALE - renamed to CustomerAccount)

The UserAccount entity represents customers.
- \`UserAccount.email\` - Email (STALE - renamed to emailAddress)
- \`UserAccount.name\` - Name (STALE - renamed to fullName)

## Endpoints
- GET /user-accounts - List accounts (STALE)
`
    await fs.writeFile(path.join(tempDir, 'docs', 'complete-test.md'), staleDoc)

    const result = await checkDocConsistency({
      domainStore,
      projectRoot: tempDir,
      docsPath: 'docs',
    })

    // Should detect UserAccount as stale (domain has CustomerAccount)
    expect(result.issues.some(i => i.entity === 'UserAccount')).toBe(true)

    // Should detect OrderLineItem as undocumented
    expect(result.missingDocumentation).toContain('OrderLineItem')

    // Summary should indicate drift
    expect(result.summary.severity).not.toBe('none')
  })
})
