/**
 * @file Domain inference integration tests with real file parsing
 * @vitest-environment node
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import { Store, DataFactory } from 'n3'
import { promises as fs } from 'fs'
import path from 'path'
import os from 'os'
import {
  inferDomainModel,
  inferDomainModelFromPath,
  DomainModelLens,
} from '../../src/project-engine/domain-infer.mjs'
import { scanFileSystemToStore } from '../../src/project-engine/fs-scan.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  dom: 'http://example.org/unrdf/domain#',
}

describe('domain-infer integration', () => {
  let tempDir

  beforeAll(async () => {
    // Create temp directory for test files
    tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'domain-infer-test-'))
  })

  afterAll(async () => {
    // Cleanup temp directory
    if (tempDir) {
      await fs.rm(tempDir, { recursive: true, force: true })
    }
  })

  describe('Zod schema parsing', () => {
    it('should extract entities from Zod schema files', async () => {
      // Create test files
      await fs.mkdir(path.join(tempDir, 'src', 'schemas'), { recursive: true })

      const zodSchemaContent = `
import { z } from 'zod'

export const UserSchema = z.object({
  id: z.string(),
  email: z.string().email(),
  name: z.string().optional(),
  age: z.number(),
  isActive: z.boolean(),
  createdAt: z.date(),
})

export const ProductSchema = z.object({
  id: z.string(),
  name: z.string(),
  price: z.number(),
  inStock: z.boolean(),
})

export const OrderSchema = z.object({
  id: z.string(),
  userId: z.string(),
  items: z.array(ProductSchema),
  total: z.number(),
})
`
      await fs.writeFile(
        path.join(tempDir, 'src', 'schemas', 'user.ts'),
        zodSchemaContent
      )

      await fs.writeFile(
        path.join(tempDir, 'package.json'),
        JSON.stringify({
          name: 'test-project',
          dependencies: { zod: '^3.0.0' },
        })
      )

      // Scan and infer
      const { store: fsStore } = await scanFileSystemToStore({ root: tempDir })

      const { store, summary } = await inferDomainModel({
        fsStore,
        projectRoot: tempDir,
        stackProfile: { hasZod: true, hasTypescript: true },
      })

      // Verify entities were extracted
      expect(summary.entityCount).toBeGreaterThan(0)
      expect(summary.fieldCount).toBeGreaterThan(0)

      // Check for User entity
      const userQuads = store.getQuads(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      expect(userQuads.length).toBe(1)

      // Check for User fields
      const userFields = store.getQuads(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.dom}hasField`),
        null
      )
      expect(userFields.length).toBeGreaterThan(0)
    })
  })

  describe('Prisma schema parsing', () => {
    it('should extract entities from Prisma schema', async () => {
      // Create test files
      await fs.mkdir(path.join(tempDir, 'prisma'), { recursive: true })

      const prismaSchemaContent = `
generator client {
  provider = "prisma-client-js"
}

datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

model User {
  id        String   @id @default(uuid())
  email     String   @unique
  name      String?
  posts     Post[]
  createdAt DateTime @default(now())
}

model Post {
  id        String   @id @default(uuid())
  title     String
  content   String?
  published Boolean  @default(false)
  author    User     @relation(fields: [authorId], references: [id])
  authorId  String
}
`
      await fs.writeFile(
        path.join(tempDir, 'prisma', 'schema.prisma'),
        prismaSchemaContent
      )

      await fs.writeFile(
        path.join(tempDir, 'package.json'),
        JSON.stringify({
          name: 'test-project',
          dependencies: { '@prisma/client': '^5.0.0' },
          devDependencies: { prisma: '^5.0.0' },
        })
      )

      // Scan and infer
      const { store: fsStore } = await scanFileSystemToStore({ root: tempDir })

      const { store, summary } = await inferDomainModel({
        fsStore,
        projectRoot: tempDir,
        stackProfile: { hasPrisma: true },
      })

      // Verify entities were extracted
      expect(summary.entityCount).toBeGreaterThanOrEqual(2)

      // Check for User entity
      const userQuads = store.getQuads(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      expect(userQuads.length).toBe(1)

      // Check for Post entity
      const postQuads = store.getQuads(
        namedNode(`${NS.dom}Post`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      expect(postQuads.length).toBe(1)

      // Check for relation
      const relations = store.getQuads(
        null,
        namedNode(`${NS.dom}relatesTo`),
        null
      )
      expect(relations.length).toBeGreaterThan(0)
    })
  })

  describe('TypeScript type parsing', () => {
    it('should extract entities from TypeScript interfaces', async () => {
      // Create test files
      await fs.mkdir(path.join(tempDir, 'src', 'types'), { recursive: true })

      const typesContent = `
export interface Customer {
  id: string
  firstName: string
  lastName: string
  email: string
  phone?: string
  orders: Order[]
}

export type Order = {
  id: string
  customerId: string
  total: number
  status: 'pending' | 'shipped' | 'delivered'
  items: OrderItem[]
}

interface OrderItem {
  productId: string
  quantity: number
  price: number
}
`
      await fs.writeFile(
        path.join(tempDir, 'src', 'types', 'customer.ts'),
        typesContent
      )

      await fs.writeFile(path.join(tempDir, 'tsconfig.json'), '{}')

      // Scan and infer
      const { store: fsStore } = await scanFileSystemToStore({ root: tempDir })

      const { store, summary } = await inferDomainModel({
        fsStore,
        projectRoot: tempDir,
        stackProfile: { hasTypescript: true },
      })

      // Verify entities were extracted
      expect(summary.entityCount).toBeGreaterThan(0)

      // Check for Customer entity
      const customerQuads = store.getQuads(
        namedNode(`${NS.dom}Customer`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      )
      expect(customerQuads.length).toBe(1)
    })
  })

  describe('inferDomainModelFromPath convenience function', () => {
    it('should infer domain model directly from path', async () => {
      // Create minimal test files
      await fs.mkdir(path.join(tempDir, 'src', 'models'), { recursive: true })

      const modelContent = `
import { z } from 'zod'

export const ItemSchema = z.object({
  id: z.string(),
  name: z.string(),
})
`
      await fs.writeFile(
        path.join(tempDir, 'src', 'models', 'item.ts'),
        modelContent
      )

      await fs.writeFile(
        path.join(tempDir, 'package.json'),
        JSON.stringify({
          name: 'test-project',
          dependencies: { zod: '^3.0.0' },
        })
      )

      // Use convenience function
      const { store, summary } = await inferDomainModelFromPath(tempDir)

      expect(store).toBeDefined()
      expect(summary).toHaveProperty('entityCount')
      expect(summary).toHaveProperty('fieldCount')
      expect(summary).toHaveProperty('relationshipCount')
    })
  })

  describe('Store compatibility with diff module', () => {
    it('should produce stores compatible with diffOntologyFromStores', async () => {
      // Create test files
      await fs.mkdir(path.join(tempDir, 'src', 'schemas'), { recursive: true })

      const schemaV1 = `
import { z } from 'zod'

export const EntitySchema = z.object({
  id: z.string(),
  name: z.string(),
})
`
      await fs.writeFile(
        path.join(tempDir, 'src', 'schemas', 'entity.ts'),
        schemaV1
      )

      await fs.writeFile(
        path.join(tempDir, 'package.json'),
        JSON.stringify({
          name: 'test-project',
          dependencies: { zod: '^3.0.0' },
        })
      )

      // First inference
      const { store: fsStore } = await scanFileSystemToStore({ root: tempDir })
      const { store: storeV1 } = await inferDomainModel({
        fsStore,
        projectRoot: tempDir,
        stackProfile: { hasZod: true, hasTypescript: true },
      })

      // Verify store has getQuads method
      expect(typeof storeV1.getQuads).toBe('function')

      // Verify store can be used with getQuads
      const quads = storeV1.getQuads(null, null, null, null)
      expect(Array.isArray(quads)).toBe(true)
    })
  })
})
