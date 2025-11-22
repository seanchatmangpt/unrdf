/**
 * @file Type auditor tests - 80/20 consolidated
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { promises as fs } from 'fs';
import path from 'path';
import os from 'os';
import {
  auditTypeConsistency,
  auditEntityTypes,
  compareTypes,
} from '../../src/project-engine/type-auditor.mjs';

async function createTempProject(files) {
  const tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'type-auditor-test-'));
  for (const [relativePath, content] of Object.entries(files)) {
    const fullPath = path.join(tmpDir, relativePath);
    await fs.mkdir(path.dirname(fullPath), { recursive: true });
    await fs.writeFile(fullPath, content, 'utf-8');
  }
  return tmpDir;
}

async function cleanupTempProject(dir) {
  try {
    await fs.rm(dir, { recursive: true, force: true });
  } catch {}
}

describe('type-auditor', () => {
  describe('compareTypes', () => {
    it('detects optionality and field mismatches', () => {
      const zodFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        email: { name: 'email', type: 'string', optional: true, array: false },
        newField: {
          name: 'newField',
          type: 'string',
          optional: false,
          array: false,
        },
      };
      const tsFields = {
        id: { name: 'id', type: 'string', optional: false, array: false },
        email: { name: 'email', type: 'string', optional: false, array: false },
        extraField: {
          name: 'extraField',
          type: 'string',
          optional: false,
          array: false,
        },
      };

      const result = compareTypes(zodFields, tsFields);
      expect(result.modified).toHaveLength(1);
      expect(result.modified[0].field).toBe('email');
      expect(result.added).toContain('newField');
      expect(result.removed).toContain('extraField');
      expect(result.consistent).toContain('id');
    });
  });

  describe('auditEntityTypes', () => {
    it('detects mismatches between Zod and TS', () => {
      const zodContent = `import { z } from 'zod'; export const UserSchema = z.object({ id: z.string(), email: z.string().optional() });`;
      const tsContent = `export interface User { id: string; email: string; }`;
      const mismatch = auditEntityTypes(zodContent, tsContent, 'User');
      expect(mismatch).not.toBeNull();
      expect(mismatch.issues.some(i => i.includes('email'))).toBe(true);
    });

    it('returns null when types match', () => {
      const zodContent = `import { z } from 'zod'; export const OrderSchema = z.object({ id: z.string(), total: z.number() });`;
      const tsContent = `export interface Order { id: string; total: number; }`;
      expect(auditEntityTypes(zodContent, tsContent, 'Order')).toBeNull();
    });

    it('detects entity only in one source', () => {
      const zodOnly = auditEntityTypes(
        `import { z } from 'zod'; export const NewSchema = z.object({ id: z.string() });`,
        `export interface Other { foo: string; }`,
        'New'
      );
      expect(zodOnly.issues[0]).toContain('exists in Zod but not in TypeScript');
    });
  });

  describe('auditTypeConsistency', () => {
    let tmpDir = '';

    beforeEach(async () => {
      tmpDir = await createTempProject({
        'src/schemas/user.ts': `import { z } from 'zod'; export const UserSchema = z.object({ id: z.string(), email: z.string().optional() });`,
        'src/types/user.ts': `export interface User { id: string; email: string; }`,
      });
    });

    it('scans project files and calculates score', async () => {
      const result = await auditTypeConsistency({
        projectRoot: tmpDir,
        schemaDir: 'src/schemas',
        typesDir: 'src/types',
      });
      expect(result.mismatches.length).toBeGreaterThan(0);
      expect(result.mismatches[0].entity).toBe('User');
      expect(result.score).toBeLessThan(100);
      await cleanupTempProject(tmpDir);
    });

    it('returns 100 score when types match', async () => {
      const matchDir = await createTempProject({
        'src/schemas/order.ts': `import { z } from 'zod'; export const OrderSchema = z.object({ id: z.string() });`,
        'src/types/order.ts': `export interface Order { id: string; }`,
      });
      const result = await auditTypeConsistency({
        projectRoot: matchDir,
        schemaDir: 'src/schemas',
        typesDir: 'src/types',
      });
      expect(result.score).toBe(100);
      await cleanupTempProject(matchDir);
    });
  });
});
