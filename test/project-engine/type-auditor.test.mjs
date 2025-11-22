/**
 * @file Type-safety auditor tests - Chicago School TDD
 * @vitest-environment node
 *
 * Chicago School TDD: Tests use REAL Zod/TS parsing with actual content.
 * NO mocks - test real type extraction and comparison.
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { promises as fs } from 'fs'
import path from 'path'
import os from 'os'
import {
  auditTypeConsistency,
  auditEntityTypes,
  compareTypes,
} from '../../src/project-engine/type-auditor.mjs'

/**
 * Create temporary test project with Zod and TS files
 * @param {Object} files - Map of relative path to content
 * @returns {Promise<string>} - Temp directory path
 */
async function createTempProject(files) {
  const tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'type-auditor-test-'))

  for (const [relativePath, content] of Object.entries(files)) {
    const fullPath = path.join(tmpDir, relativePath)
    await fs.mkdir(path.dirname(fullPath), { recursive: true })
    await fs.writeFile(fullPath, content, 'utf-8')
  }

  return tmpDir
}

/**
 * Clean up temp directory
 * @param {string} dir
 */
async function cleanupTempProject(dir) {
  try {
    await fs.rm(dir, { recursive: true, force: true })
  } catch {
    // Ignore cleanup errors
  }
}

describe('type-auditor', () => {
  describe('compareTypes', () => {
    it('should detect when Zod has optional field but TS has required', () => {
      const zodFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        email: { name: 'email', type: 'string', optional: true, array: false },
        name: { name: 'name', type: 'string', optional: false, array: false },
      }

      const tsFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        email: { name: 'email', type: 'string', optional: false, array: false }, // Required in TS!
        name: { name: 'name', type: 'string', optional: false, array: false },
      }

      const result = compareTypes(zodFields, tsFields)

      expect(result.modified).toHaveLength(1)
      expect(result.modified[0].field).toBe('email')
      expect(result.modified[0].issue).toContain('optional in Zod')
      expect(result.modified[0].issue).toContain('required in TS')
      expect(result.consistent).toContain('id')
      expect(result.consistent).toContain('name')
    })

    it('should detect when TS has optional field but Zod has required', () => {
      const zodFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        role: { name: 'role', type: 'string', optional: false, array: false }, // Required in Zod
      }

      const tsFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        role: { name: 'role', type: 'string', optional: true, array: false }, // Optional in TS!
      }

      const result = compareTypes(zodFields, tsFields)

      expect(result.modified).toHaveLength(1)
      expect(result.modified[0].field).toBe('role')
      expect(result.modified[0].issue).toContain('required in Zod')
      expect(result.modified[0].issue).toContain('optional in TS')
    })

    it('should detect fields in Zod but not in TS', () => {
      const zodFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        newField: { name: 'newField', type: 'string', optional: false, array: false },
      }

      const tsFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
      }

      const result = compareTypes(zodFields, tsFields)

      expect(result.added).toContain('newField')
      expect(result.removed).toHaveLength(0)
    })

    it('should detect fields removed from TS but still in Zod', () => {
      const zodFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        deletedAt: { name: 'deletedAt', type: 'date', optional: true, array: false },
      }

      const tsFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        // deletedAt removed from TS!
      }

      const result = compareTypes(zodFields, tsFields)

      expect(result.added).toContain('deletedAt') // In Zod but not TS = "added" to Zod perspective
    })

    it('should detect fields in TS but not in Zod', () => {
      const zodFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
      }

      const tsFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        extraField: { name: 'extraField', type: 'string', optional: false, array: false },
      }

      const result = compareTypes(zodFields, tsFields)

      expect(result.removed).toContain('extraField') // In TS but not Zod = "removed" from Zod perspective
    })

    it('should report consistent fields when Zod and TS match perfectly', () => {
      const zodFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        email: { name: 'email', type: 'string', optional: true, array: false },
        tags: { name: 'tags', type: 'array', optional: false, array: true },
      }

      const tsFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        email: { name: 'email', type: 'string', optional: true, array: false },
        tags: { name: 'tags', type: 'array', optional: false, array: true },
      }

      const result = compareTypes(zodFields, tsFields)

      expect(result.consistent).toContain('id')
      expect(result.consistent).toContain('email')
      expect(result.consistent).toContain('tags')
      expect(result.added).toHaveLength(0)
      expect(result.removed).toHaveLength(0)
      expect(result.modified).toHaveLength(0)
    })

    it('should detect array type mismatches', () => {
      const zodFields = {
        tags: { name: 'tags', type: 'array', optional: false, array: true },
      }

      const tsFields = {
        tags: { name: 'tags', type: 'string', optional: false, array: false },
      }

      const result = compareTypes(zodFields, tsFields)

      expect(result.modified).toHaveLength(1)
      expect(result.modified[0].issue).toContain('array=true')
      expect(result.modified[0].issue).toContain('array=false')
    })
  })

  describe('auditEntityTypes', () => {
    it('should detect optionality mismatch from real Zod and TS content', () => {
      const zodContent = `
        import { z } from 'zod';

        export const UserSchema = z.object({
          id: z.string(),
          email: z.string().optional(),
          name: z.string(),
        });
      `

      const tsContent = `
        export interface User {
          id: string;
          email: string;  // Required in TS!
          name: string;
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'User')

      expect(mismatch).not.toBeNull()
      expect(mismatch.entity).toBe('User')
      expect(mismatch.issues.some(i => i.includes('email') && i.includes('optional'))).toBe(true)
      expect(mismatch.severity).toBe('high')
    })

    it('should detect removed field from TS but still in Zod', () => {
      const zodContent = `
        import { z } from 'zod';

        export const ProductSchema = z.object({
          id: z.string(),
          name: z.string(),
          deletedAt: z.date().optional(),
        });
      `

      const tsContent = `
        export interface Product {
          id: string;
          name: string;
          // deletedAt was removed!
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'Product')

      expect(mismatch).not.toBeNull()
      expect(mismatch.issues.some(i => i.includes('deletedAt'))).toBe(true)
    })

    it('should return null when Zod and TS match perfectly', () => {
      const zodContent = `
        import { z } from 'zod';

        export const OrderSchema = z.object({
          id: z.string(),
          total: z.number(),
          status: z.string(),
        });
      `

      const tsContent = `
        export interface Order {
          id: string;
          total: number;
          status: string;
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'Order')

      expect(mismatch).toBeNull()
    })

    it('should detect entity only in Zod', () => {
      const zodContent = `
        import { z } from 'zod';

        export const NewFeatureSchema = z.object({
          id: z.string(),
        });
      `

      const tsContent = `
        // No NewFeature type defined
        export interface OtherType {
          foo: string;
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'NewFeature')

      expect(mismatch).not.toBeNull()
      expect(mismatch.issues[0]).toContain('exists in Zod but not in TypeScript')
    })

    it('should detect entity only in TypeScript', () => {
      const zodContent = `
        import { z } from 'zod';
        // No LegacyEntity schema
      `

      const tsContent = `
        export interface LegacyEntity {
          id: string;
          oldField: string;
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'LegacyEntity')

      expect(mismatch).not.toBeNull()
      expect(mismatch.issues[0]).toContain('exists in TypeScript but not in Zod')
    })
  })

  describe('auditTypeConsistency', () => {
    let tmpDir = ''

    beforeEach(async () => {
      // Create temp project with real files
      tmpDir = await createTempProject({
        'src/schemas/user.ts': `
          import { z } from 'zod';

          export const UserSchema = z.object({
            id: z.string(),
            email: z.string().optional(),
            name: z.string(),
            role: z.string(),
          });
        `,
        'src/schemas/product.ts': `
          import { z } from 'zod';

          export const ProductSchema = z.object({
            id: z.string(),
            name: z.string(),
            price: z.number(),
          });
        `,
        'src/types/user.ts': `
          export interface User {
            id: string;
            email: string;  // Mismatch: required in TS, optional in Zod
            name: string;
            // Missing: role field
          }
        `,
        'src/types/product.ts': `
          export interface Product {
            id: string;
            name: string;
            price: number;
          }
        `,
      })
    })

    it('should detect mismatches when scanning real project files', async () => {
      const result = await auditTypeConsistency({
        projectRoot: tmpDir,
        schemaDir: 'src/schemas',
        typesDir: 'src/types',
      })

      expect(result.mismatches.length).toBeGreaterThan(0)

      // Should find User mismatch
      const userMismatch = result.mismatches.find(m => m.entity === 'User')
      expect(userMismatch).toBeDefined()
      expect(userMismatch.issues.some(i => i.includes('email'))).toBe(true)
      expect(userMismatch.issues.some(i => i.includes('role'))).toBe(true)

      // Product should be consistent (no mismatch)
      const productMismatch = result.mismatches.find(m => m.entity === 'Product')
      expect(productMismatch).toBeUndefined()

      // Clean up
      await cleanupTempProject(tmpDir)
    })

    it('should return 100 score when all types match', async () => {
      const matchingDir = await createTempProject({
        'src/schemas/order.ts': `
          import { z } from 'zod';

          export const OrderSchema = z.object({
            id: z.string(),
            total: z.number(),
          });
        `,
        'src/types/order.ts': `
          export interface Order {
            id: string;
            total: number;
          }
        `,
      })

      const result = await auditTypeConsistency({
        projectRoot: matchingDir,
        schemaDir: 'src/schemas',
        typesDir: 'src/types',
      })

      expect(result.score).toBe(100)
      expect(result.mismatches).toHaveLength(0)
      expect(result.summary).toContain('consistent')

      await cleanupTempProject(matchingDir)
    })

    it('should calculate correct score based on mismatch ratio', async () => {
      // 2 entities: 1 matching, 1 mismatching = 50% score
      const partialMatchDir = await createTempProject({
        'src/schemas/good.ts': `
          import { z } from 'zod';
          export const GoodSchema = z.object({
            id: z.string(),
          });
        `,
        'src/schemas/bad.ts': `
          import { z } from 'zod';
          export const BadSchema = z.object({
            id: z.string(),
            field: z.string().optional(),
          });
        `,
        'src/types/good.ts': `
          export interface Good {
            id: string;
          }
        `,
        'src/types/bad.ts': `
          export interface Bad {
            id: string;
            field: string;  // Required, not optional
          }
        `,
      })

      const result = await auditTypeConsistency({
        projectRoot: partialMatchDir,
        schemaDir: 'src/schemas',
        typesDir: 'src/types',
      })

      expect(result.score).toBe(50) // 1 out of 2 entities has issues
      expect(result.mismatches).toHaveLength(1)
      expect(result.mismatches[0].entity).toBe('Bad')

      await cleanupTempProject(partialMatchDir)
    })

    it('should return empty result when no project root provided', async () => {
      const result = await auditTypeConsistency({})

      expect(result.mismatches).toHaveLength(0)
      expect(result.score).toBe(100)
      expect(result.summary).toContain('No project root')
    })

    it('should handle missing directories gracefully', async () => {
      const emptyDir = await createTempProject({})

      const result = await auditTypeConsistency({
        projectRoot: emptyDir,
        schemaDir: 'src/schemas',
        typesDir: 'src/types',
      })

      expect(result.mismatches).toHaveLength(0)
      expect(result.score).toBe(100)

      await cleanupTempProject(emptyDir)
    })

    it('should detect high severity for optionality mismatches', async () => {
      const severeDir = await createTempProject({
        'src/schemas/critical.ts': `
          import { z } from 'zod';
          export const CriticalSchema = z.object({
            requiredField: z.string(),
          });
        `,
        'src/types/critical.ts': `
          export interface Critical {
            requiredField?: string;  // Optional in TS, required in Zod!
          }
        `,
      })

      const result = await auditTypeConsistency({
        projectRoot: severeDir,
        schemaDir: 'src/schemas',
        typesDir: 'src/types',
      })

      expect(result.mismatches).toHaveLength(1)
      expect(result.mismatches[0].severity).toBe('high')
      expect(result.recommendation).toContain('update TypeScript')

      await cleanupTempProject(severeDir)
    })
  })

  describe('real file parsing', () => {
    it('should parse simple Zod schemas correctly', () => {
      const zodContent = `
        import { z } from 'zod';

        export const SimpleSchema = z.object({
          id: z.string(),
          email: z.string().optional(),
          count: z.number(),
        });
      `

      const tsContent = `
        export interface Simple {
          id: string;
          email?: string;
          count: number;
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'Simple')

      // Simple schemas should match perfectly
      expect(mismatch).toBeNull()
    })

    it('should parse TypeScript type aliases with matching Zod', () => {
      const zodContent = `
        import { z } from 'zod';

        export const BasicSchema = z.object({
          enabled: z.boolean(),
          timeout: z.number(),
        });
      `

      const tsContent = `
        export type Basic = {
          enabled: boolean;
          timeout: number;
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'Basic')

      expect(mismatch).toBeNull() // Should match
    })

    it('should detect mismatches in complex nested schemas', () => {
      // Test that we CAN detect mismatches even in complex schemas
      const zodContent = `
        import { z } from 'zod';

        export const NestedSchema = z.object({
          id: z.string(),
          config: z.object(),
        });
      `

      const tsContent = `
        export interface Nested {
          id: string;
          config: object;
          extraField: string;
        }
      `

      const mismatch = auditEntityTypes(zodContent, tsContent, 'Nested')

      // Should detect the extraField in TS but not in Zod
      expect(mismatch).not.toBeNull()
      expect(mismatch.issues.some(i => i.includes('extraField'))).toBe(true)
    })
  })
})
