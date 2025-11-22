/**
 * @file API Contract Validator tests - Chicago School TDD with real stores
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  generateAPISchema,
  generateAllAPISchemas,
  validateAPIFiles,
  detectContractBreaks,
  detectAllContractBreaks,
} from '../../src/project-engine/api-contract-validator.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  dom: 'http://example.org/unrdf/domain#',
}

/* ========================================================================= */
/* Test Helpers                                                              */
/* ========================================================================= */

/**
 * Create a domain store with an entity and fields
 * @param {string} entityName
 * @param {Array<{name: string, type?: string, optional?: boolean, array?: boolean}>} fields
 * @returns {Store}
 */
function createDomainStore(entityName, fields) {
  const store = new Store()
  const baseIri = 'http://example.org/unrdf/domain#'
  const entityIri = `${baseIri}${entityName}`

  // Add entity type
  store.addQuad(
    namedNode(entityIri),
    namedNode(`${NS.rdf}type`),
    namedNode(`${NS.dom}Entity`)
  )

  // Add entity label
  store.addQuad(
    namedNode(entityIri),
    namedNode(`${NS.rdfs}label`),
    literal(entityName)
  )

  // Add fields
  for (const field of fields) {
    const fieldIri = namedNode(`${entityIri}.${field.name}`)

    // Link entity to field
    store.addQuad(
      namedNode(entityIri),
      namedNode(`${NS.dom}hasField`),
      fieldIri
    )

    // Add field name
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}fieldName`),
      literal(field.name)
    )

    // Add field type
    const xsdType = field.type || `${NS.xsd}string`
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}fieldType`),
      namedNode(xsdType)
    )

    // Add optional flag
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}isOptional`),
      literal(field.optional ? 'true' : 'false', namedNode(`${NS.xsd}boolean`))
    )

    // Add array flag
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}isArray`),
      literal(field.array ? 'true' : 'false', namedNode(`${NS.xsd}boolean`))
    )
  }

  return store
}

/* ========================================================================= */
/* generateAPISchema Tests                                                   */
/* ========================================================================= */

describe('api-contract-validator', () => {
  describe('generateAPISchema', () => {
    it('should generate schema for entity with fields', () => {
      const store = createDomainStore('User', [
        { name: 'id', type: `${NS.xsd}string` },
        { name: 'email', type: `${NS.xsd}string` },
        { name: 'name', type: `${NS.xsd}string`, optional: true },
      ])

      const schema = generateAPISchema(store, 'User')

      expect(schema.entityName).toBe('User')
      expect(schema.fields).toHaveLength(3)
      expect(schema.fields.map(f => f.name)).toContain('id')
      expect(schema.fields.map(f => f.name)).toContain('email')
      expect(schema.fields.map(f => f.name)).toContain('name')
    })

    it('should mark optional fields correctly', () => {
      const store = createDomainStore('Product', [
        { name: 'id', type: `${NS.xsd}string` },
        { name: 'description', type: `${NS.xsd}string`, optional: true },
      ])

      const schema = generateAPISchema(store, 'Product')

      const idField = schema.fields.find(f => f.name === 'id')
      const descField = schema.fields.find(f => f.name === 'description')

      expect(idField.optional).toBe(false)
      expect(descField.optional).toBe(true)
    })

    it('should handle array fields', () => {
      const store = createDomainStore('Order', [
        { name: 'id', type: `${NS.xsd}string` },
        { name: 'items', type: `${NS.xsd}string`, array: true },
      ])

      const schema = generateAPISchema(store, 'Order')

      const itemsField = schema.fields.find(f => f.name === 'items')
      expect(itemsField.array).toBe(true)
    })

    it('should generate Zod schema string', () => {
      const store = createDomainStore('User', [
        { name: 'id', type: `${NS.xsd}string` },
        { name: 'age', type: `${NS.xsd}integer` },
      ])

      const schema = generateAPISchema(store, 'User')

      expect(schema.zodSchema).toContain('UserSchema')
      expect(schema.zodSchema).toContain('z.object')
      expect(schema.zodSchema).toContain('id:')
      expect(schema.zodSchema).toContain('z.string()')
      // Type is normalized to 'number' for XSD integer
      expect(schema.zodSchema).toContain('z.number()')
    })

    it('should handle empty entity', () => {
      const store = createDomainStore('EmptyEntity', [])

      const schema = generateAPISchema(store, 'EmptyEntity')

      expect(schema.entityName).toBe('EmptyEntity')
      expect(schema.fields).toHaveLength(0)
    })
  })

  describe('generateAllAPISchemas', () => {
    it('should generate schemas for all entities', () => {
      const store = new Store()
      const baseIri = 'http://example.org/unrdf/domain#'

      // Add User entity
      store.addQuad(
        namedNode(`${baseIri}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )

      // Add Product entity
      store.addQuad(
        namedNode(`${baseIri}Product`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )

      const schemas = generateAllAPISchemas(store)

      expect(schemas).toHaveLength(2)
      expect(schemas.map(s => s.entityName)).toContain('User')
      expect(schemas.map(s => s.entityName)).toContain('Product')
    })
  })

  /* ========================================================================= */
  /* validateAPIFiles Tests                                                    */
  /* ========================================================================= */

  describe('validateAPIFiles', () => {
    it('should validate valid Next.js API route', () => {
      const store = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
        { name: 'name', type: `${NS.xsd}string` },
      ])
      const schema = generateAPISchema(store, 'User')

      const apiContent = `
        import { NextResponse } from 'next/server'
        import { UserSchema } from '@/schemas/user'

        export async function POST(req) {
          const { email, name } = await req.json()
          UserSchema.parse({ email, name })
          return NextResponse.json({ email, name, id: '123' })
        }
      `

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: apiContent }],
        schema,
        { framework: 'nextjs' }
      )

      expect(result.violations.filter(v => v.type === 'missing_field')).toHaveLength(0)
      expect(result.coverage).toBeGreaterThan(50)
    })

    it('should detect missing required fields', () => {
      const store = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
        { name: 'name', type: `${NS.xsd}string` },
        { name: 'password', type: `${NS.xsd}string` },
      ])
      const schema = generateAPISchema(store, 'User')

      const apiContent = `
        export async function POST(req) {
          const { email } = await req.json()
          return NextResponse.json({ email })
        }
      `

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: apiContent }],
        schema
      )

      const missingFields = result.violations.filter(v => v.type === 'missing_field')
      expect(missingFields.length).toBeGreaterThan(0)
      expect(missingFields.some(v => v.field === 'name')).toBe(true)
    })

    it('should detect extra fields not in schema', () => {
      const store = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
      ])
      const schema = generateAPISchema(store, 'User')

      const apiContent = `
        export async function POST(req) {
          const { email, unknownField } = await req.json()
          return NextResponse.json({ email })
        }
      `

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: apiContent }],
        schema
      )

      const extraFields = result.violations.filter(v => v.type === 'extra_field')
      expect(extraFields.some(v => v.field === 'unknownField')).toBe(true)
    })

    it('should detect missing validation', () => {
      const store = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
      ])
      const schema = generateAPISchema(store, 'User')

      const apiContent = `
        export async function POST(req) {
          const { email } = await req.json()
          // No validation!
          return NextResponse.json({ email })
        }
      `

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: apiContent }],
        schema
      )

      expect(result.violations.some(v => v.type === 'missing_validation')).toBe(true)
    })

    it('should validate Express route handlers', () => {
      const store = createDomainStore('Product', [
        { name: 'name', type: `${NS.xsd}string` },
        { name: 'price', type: `${NS.xsd}decimal` },
      ])
      const schema = generateAPISchema(store, 'Product')

      const apiContent = `
        const express = require('express')
        const router = express.Router()

        router.post('/', (req, res) => {
          const { name, price } = req.body
          ProductSchema.parse({ name, price })
          res.json({ name, price, id: '123' })
        })
      `

      const result = validateAPIFiles(
        [{ path: 'routes/products.js', content: apiContent }],
        schema,
        { framework: 'express' }
      )

      expect(result.violations.filter(v => v.type === 'missing_field')).toHaveLength(0)
    })

    it('should calculate coverage correctly', () => {
      const store = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
        { name: 'name', type: `${NS.xsd}string` },
        { name: 'age', type: `${NS.xsd}integer`, optional: true },
        { name: 'bio', type: `${NS.xsd}string`, optional: true },
      ])
      const schema = generateAPISchema(store, 'User')

      const apiContent = `
        export async function POST(req) {
          const { email, name } = await req.json()
          return NextResponse.json({ email, name })
        }
      `

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: apiContent }],
        schema
      )

      expect(result.coverage).toBe(50) // 2/4 fields
    })

    it('should mark breaking violations correctly', () => {
      const store = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
        { name: 'password', type: `${NS.xsd}string` },
      ])
      const schema = generateAPISchema(store, 'User')

      const apiContent = `
        export async function POST(req) {
          const { email } = await req.json()
          return NextResponse.json({ email })
        }
      `

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: apiContent }],
        schema
      )

      expect(result.breaking).toBe(true) // Missing required field
    })
  })

  /* ========================================================================= */
  /* detectContractBreaks Tests                                                */
  /* ========================================================================= */

  describe('detectContractBreaks', () => {
    it('should detect field removal as breaking', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [
          { name: 'id', type: 'string', optional: false, array: false },
          { name: 'email', type: 'string', optional: false, array: false },
          { name: 'legacyField', type: 'string', optional: false, array: false },
        ],
      }

      const newSchema = {
        entityName: 'User',
        fields: [
          { name: 'id', type: 'string', optional: false, array: false },
          { name: 'email', type: 'string', optional: false, array: false },
        ],
      }

      const result = detectContractBreaks(oldSchema, newSchema)

      expect(result.safe).toBe(false)
      expect(result.breakingChanges.some(c => c.type === 'field_removed')).toBe(true)
      expect(result.breakingChanges.find(c => c.field === 'legacyField')).toBeDefined()
    })

    it('should detect optional to required as breaking', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: true, array: false },
        ],
      }

      const newSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: false, array: false },
        ],
      }

      const result = detectContractBreaks(oldSchema, newSchema)

      expect(result.safe).toBe(false)
      expect(result.breakingChanges.some(c => c.type === 'field_required')).toBe(true)
    })

    it('should detect type change as breaking', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [
          { name: 'age', type: 'string', optional: false, array: false },
        ],
      }

      const newSchema = {
        entityName: 'User',
        fields: [
          { name: 'age', type: 'number', optional: false, array: false },
        ],
      }

      const result = detectContractBreaks(oldSchema, newSchema)

      expect(result.safe).toBe(false)
      expect(result.breakingChanges.some(c => c.type === 'type_changed')).toBe(true)
    })

    it('should identify affected API files', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: false, array: false },
          { name: 'legacyField', type: 'string', optional: false, array: false },
        ],
      }

      const newSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: false, array: false },
        ],
      }

      const implementations = [
        {
          path: 'app/api/users/route.ts',
          content: `
            export async function POST(req) {
              const { email, legacyField } = await req.json()
              return NextResponse.json({ email, legacyField })
            }
          `,
        },
        {
          path: 'app/api/products/route.ts',
          content: `
            export async function POST(req) {
              const { name, price } = await req.json()
              return NextResponse.json({ name, price })
            }
          `,
        },
      ]

      const result = detectContractBreaks(oldSchema, newSchema, implementations)

      expect(result.affectedAPIs).toContain('app/api/users/route.ts')
      expect(result.affectedAPIs).not.toContain('app/api/products/route.ts')
    })

    it('should allow non-breaking additions', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: false, array: false },
        ],
      }

      const newSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: false, array: false },
          { name: 'newOptionalField', type: 'string', optional: true, array: false },
        ],
      }

      const result = detectContractBreaks(oldSchema, newSchema)

      expect(result.safe).toBe(true)
      expect(result.breakingChanges).toHaveLength(0)
    })

    it('should allow required to optional change', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: false, array: false },
        ],
      }

      const newSchema = {
        entityName: 'User',
        fields: [
          { name: 'email', type: 'string', optional: true, array: false },
        ],
      }

      const result = detectContractBreaks(oldSchema, newSchema)

      expect(result.safe).toBe(true)
    })
  })

  /* ========================================================================= */
  /* detectAllContractBreaks Tests                                             */
  /* ========================================================================= */

  describe('detectAllContractBreaks', () => {
    it('should detect entity removal', () => {
      const oldStore = new Store()
      const newStore = new Store()
      const baseIri = 'http://example.org/unrdf/domain#'

      // Old store has User and Product
      oldStore.addQuad(
        namedNode(`${baseIri}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      oldStore.addQuad(
        namedNode(`${baseIri}Product`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )

      // New store only has User
      newStore.addQuad(
        namedNode(`${baseIri}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )

      const results = detectAllContractBreaks(oldStore, newStore)

      expect(results.has('Product')).toBe(true)
      expect(results.get('Product').safe).toBe(false)
      expect(results.get('Product').breakingChanges[0].type).toBe('entity_removed')
    })

    it('should detect field changes across entities', () => {
      const oldStore = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
        { name: 'password', type: `${NS.xsd}string` },
      ])

      const newStore = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
        // password removed
      ])

      const results = detectAllContractBreaks(oldStore, newStore)

      expect(results.has('User')).toBe(true)
      expect(results.get('User').breakingChanges.some(c => c.field === 'password')).toBe(true)
    })
  })
})
