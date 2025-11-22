/**
 * @file Generative Documentation tests - Chicago School TDD with real collaborators
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  generateEntityReference,
  generateAPIReference,
  generateArchitectureDiagram,
  generateCompleteDocumentation,
} from '../../src/project-engine/doc-generator.mjs'

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
 * Create domain store with entities and fields
 * @param {Array<{name: string, fields?: string[], relations?: string[], source?: string}>} entities
 */
function createDomainStore(entities) {
  const store = new Store()
  for (const { name, fields = [], relations = [], source = 'zod' } of entities) {
    const iri = namedNode(`${NS.dom}${name}`)
    store.addQuad(iri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`))
    store.addQuad(iri, namedNode(`${NS.rdfs}label`), literal(name))
    store.addQuad(iri, namedNode(`${NS.dom}source`), literal(source))

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
  for (const { path: filePath, role } of files) {
    const iri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(filePath)}`)
    store.addQuad(iri, namedNode(`${NS.fs}relativePath`), literal(filePath))
    if (role) {
      store.addQuad(iri, namedNode(`${NS.proj}roleString`), literal(role))
    }
  }
  return store
}

/* ========================================================================= */
/* Entity Reference Tests                                                    */
/* ========================================================================= */

describe('generateEntityReference', () => {
  it('generates markdown with entity header and fields', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email', 'name'], source: 'zod' },
    ])

    const result = generateEntityReference(domainStore)

    expect(result).toContain('# Entity Reference')
    expect(result).toContain('## User')
    expect(result).toContain('**Fields:** id, email, name')
  })

  it('includes source information for entities', () => {
    const domainStore = createDomainStore([
      { name: 'Product', fields: ['id', 'price'], source: 'prisma' },
    ])

    const result = generateEntityReference(domainStore)

    expect(result).toContain('**Source:** prisma')
  })

  it('documents relations between entities', () => {
    const domainStore = createDomainStore([
      { name: 'Order', fields: ['id'], relations: ['User', 'Product'] },
    ])

    const result = generateEntityReference(domainStore)

    expect(result).toContain('**Relations:** User, Product')
  })

  it('handles entities with no fields gracefully', () => {
    const domainStore = createDomainStore([
      { name: 'EmptyEntity', fields: [] },
    ])

    const result = generateEntityReference(domainStore)

    expect(result).toContain('## EmptyEntity')
    expect(result).toContain('_none defined_')
  })

  it('returns placeholder for empty domain store', () => {
    const domainStore = new Store()

    const result = generateEntityReference(domainStore)

    expect(result).toContain('No entities found')
  })

  it('includes entity count in header', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
      { name: 'Product', fields: ['id'] },
      { name: 'Order', fields: ['id'] },
    ])

    const result = generateEntityReference(domainStore)

    expect(result).toContain('3 entities documented')
  })
})

/* ========================================================================= */
/* API Reference Tests                                                       */
/* ========================================================================= */

describe('generateAPIReference', () => {
  it('generates markdown with API endpoints', () => {
    const projectStore = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
    ])

    const result = generateAPIReference(projectStore)

    expect(result).toContain('# API Reference')
    expect(result).toContain('/api/users')
  })

  it('groups endpoints by entity', () => {
    const projectStore = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
      { path: 'src/api/products.ts', role: 'Api' },
    ])

    const result = generateAPIReference(projectStore)

    expect(result).toContain('## Users')
    expect(result).toContain('## Products')
  })

  it('includes example request/response for endpoints', () => {
    const projectStore = createProjectStore([
      { path: 'src/api/orders.ts', role: 'Api' },
    ])

    const result = generateAPIReference(projectStore)

    expect(result).toContain('**Request:**')
    expect(result).toContain('**Response:**')
    expect(result).toContain('```json')
  })

  it('handles Next.js app router endpoints', () => {
    const projectStore = createProjectStore([
      { path: 'src/app/api/users/route.ts', role: 'Route' },
    ])

    const result = generateAPIReference(projectStore)

    expect(result).toContain('/api/users')
  })

  it('returns placeholder for project without API endpoints', () => {
    const projectStore = createProjectStore([
      { path: 'src/components/Button.tsx', role: 'Component' },
    ])

    const result = generateAPIReference(projectStore)

    expect(result).toContain('No API endpoints found')
  })

  it('includes endpoint count in header', () => {
    const projectStore = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
      { path: 'src/api/products.ts', role: 'Api' },
    ])

    const result = generateAPIReference(projectStore)

    expect(result).toContain('2 endpoints documented')
  })
})

/* ========================================================================= */
/* Architecture Diagram Tests                                                */
/* ========================================================================= */

describe('generateArchitectureDiagram', () => {
  it('generates mermaid diagram with entities', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email'] },
    ])
    const projectStore = createProjectStore([])

    const result = generateArchitectureDiagram(projectStore, domainStore)

    expect(result).toContain('```mermaid')
    expect(result).toContain('graph TB')
    expect(result).toContain('User')
  })

  it('shows entity relations in diagram', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'], relations: ['Order'] },
      { name: 'Order', fields: ['id'] },
    ])
    const projectStore = createProjectStore([])

    const result = generateArchitectureDiagram(projectStore, domainStore)

    expect(result).toContain('User --> Order')
  })

  it('includes features subgraph', () => {
    const domainStore = createDomainStore([])
    const projectStore = createProjectStore([
      { path: 'src/features/auth/Login.tsx', role: 'Component' },
    ])

    const result = generateArchitectureDiagram(projectStore, domainStore)

    expect(result).toContain('subgraph "Features"')
    expect(result).toContain('Auth')
  })

  it('includes API endpoints subgraph', () => {
    const domainStore = createDomainStore([])
    const projectStore = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
    ])

    const result = generateArchitectureDiagram(projectStore, domainStore)

    expect(result).toContain('subgraph "API Endpoints"')
    expect(result).toContain('/api/users')
  })

  it('includes ASCII fallback diagram', () => {
    const domainStore = createDomainStore([{ name: 'User', fields: ['id'] }])
    const projectStore = createProjectStore([])

    const result = generateArchitectureDiagram(projectStore, domainStore)

    expect(result).toContain('## ASCII Overview')
    expect(result).toContain('PROJECT ARCHITECTURE')
    expect(result).toContain('Entities: User')
  })

  it('truncates field list in entity nodes', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email', 'name', 'password', 'role'] },
    ])
    const projectStore = createProjectStore([])

    const result = generateArchitectureDiagram(projectStore, domainStore)

    expect(result).toContain('id, email, name...')
  })
})

/* ========================================================================= */
/* Complete Documentation Tests                                              */
/* ========================================================================= */

describe('generateCompleteDocumentation', () => {
  it('returns correct result structure without writing files', async () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id', 'email'] },
    ])
    const projectStore = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
    ])

    const result = await generateCompleteDocumentation(
      projectStore,
      domainStore,
      null,
      null // No docsPath - don't write files
    )

    // Check result structure
    expect(result.files).toHaveLength(0)
    expect(result.coverage).toBe(100)
    expect(result.timestamp).toBeGreaterThan(0)
  })

  it('returns 100% coverage for empty stores', async () => {
    const domainStore = new Store()
    const projectStore = new Store()

    const result = await generateCompleteDocumentation(
      projectStore,
      domainStore,
      null,
      null
    )

    expect(result.coverage).toBe(100) // 0/0 => 100% (nothing to document, nothing missing)
    expect(result.timestamp).toBeGreaterThan(0)
  })

  it('returns timestamp of generation', async () => {
    const domainStore = createDomainStore([{ name: 'User', fields: ['id'] }])
    const projectStore = createProjectStore([])
    const beforeTime = Date.now()

    const result = await generateCompleteDocumentation(
      projectStore,
      domainStore,
      null,
      null
    )

    const afterTime = Date.now()
    expect(result.timestamp).toBeGreaterThanOrEqual(beforeTime)
    expect(result.timestamp).toBeLessThanOrEqual(afterTime)
  })
})

/* ========================================================================= */
/* Markdown Syntax Validation                                                */
/* ========================================================================= */

describe('markdown syntax validation', () => {
  it('entity reference has valid markdown headers', () => {
    const domainStore = createDomainStore([
      { name: 'User', fields: ['id'] },
    ])

    const result = generateEntityReference(domainStore)

    // Valid H1 and H2 headers
    expect(result).toMatch(/^# [^\n]+/m)
    expect(result).toMatch(/^## [^\n]+/m)
  })

  it('api reference has valid code blocks', () => {
    const projectStore = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
    ])

    const result = generateAPIReference(projectStore)

    // Valid fenced code blocks
    const codeBlockCount = (result.match(/```json/g) || []).length
    const closeBlockCount = (result.match(/```\n/g) || []).length

    expect(codeBlockCount).toBeGreaterThan(0)
    expect(closeBlockCount).toBeGreaterThanOrEqual(codeBlockCount)
  })

  it('architecture diagram has valid mermaid block', () => {
    const domainStore = createDomainStore([{ name: 'User', fields: ['id'] }])
    const projectStore = createProjectStore([])

    const result = generateArchitectureDiagram(projectStore, domainStore)

    // Valid mermaid block
    expect(result).toContain('```mermaid')
    expect(result).toContain('graph TB')
    expect(result).toMatch(/```mermaid[\s\S]+```/)
  })
})

/* ========================================================================= */
/* Updateability Tracking Tests                                              */
/* ========================================================================= */

describe('updateability tracking', () => {
  it('generates different content when domain model changes', () => {
    // First generation
    const domainStoreV1 = createDomainStore([
      { name: 'User', fields: ['id'] },
    ])
    const resultV1 = generateEntityReference(domainStoreV1)
    expect(resultV1).toContain('## User')
    expect(resultV1).not.toContain('## Product')

    // Second generation with new entity
    const domainStoreV2 = createDomainStore([
      { name: 'User', fields: ['id'] },
      { name: 'Product', fields: ['id', 'price'] },
    ])
    const resultV2 = generateEntityReference(domainStoreV2)
    expect(resultV2).toContain('## User')
    expect(resultV2).toContain('## Product')
    expect(resultV2).toContain('2 entities documented')
  })

  it('generates different content when API endpoints change', () => {
    // First generation
    const projectStoreV1 = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
    ])
    const resultV1 = generateAPIReference(projectStoreV1)
    expect(resultV1).toContain('/api/users')
    expect(resultV1).not.toContain('/api/products')

    // Second generation with new endpoint
    const projectStoreV2 = createProjectStore([
      { path: 'src/api/users.ts', role: 'Api' },
      { path: 'src/api/products.ts', role: 'Api' },
    ])
    const resultV2 = generateAPIReference(projectStoreV2)
    expect(resultV2).toContain('/api/users')
    expect(resultV2).toContain('/api/products')
    expect(resultV2).toContain('2 endpoints documented')
  })
})
