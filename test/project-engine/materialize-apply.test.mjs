/**
 * @vitest-environment node
 * @file Tests for materialize-apply.mjs
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest'
import { promises as fs } from 'fs'
import path from 'path'
import os from 'os'
import {
  applyMaterializationPlan,
  rollbackMaterialization,
  previewPlan,
  checkPlanApplicability,
} from '../../src/project-engine/materialize-apply.mjs'

describe('materialize-apply', () => {
  let testDir

  beforeEach(async () => {
    // Create a temp directory for each test
    testDir = await fs.mkdtemp(path.join(os.tmpdir(), 'materialize-test-'))
  })

  afterEach(async () => {
    // Cleanup temp directory
    try {
      await fs.rm(testDir, { recursive: true, force: true })
    } catch {
      // Ignore cleanup errors
    }
  })

  describe('applyMaterializationPlan', () => {
    it('applies empty plan successfully', async () => {
      const plan = { writes: [], updates: [], deletes: [] }

      const { result, receipt } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.appliedCount).toBe(0)
      expect(result.skippedCount).toBe(0)
      expect(result.errors).toEqual([])
      expect(receipt.success).toBe(true)
    })

    it('writes new files', async () => {
      const plan = {
        writes: [
          {
            path: 'src/test.mjs',
            content: '// Test file\nexport const x = 1',
            hash: 'abc123',
            templateIri: 'http://example.org/template/1',
            entityIri: 'http://example.org/entity/1',
            entityType: 'http://example.org/Type',
          },
        ],
        updates: [],
        deletes: [],
      }

      const { result, receipt } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.appliedCount).toBe(1)
      expect(result.writtenPaths).toContain('src/test.mjs')
      expect(receipt.success).toBe(true)

      // Verify file exists
      const content = await fs.readFile(path.join(testDir, 'src/test.mjs'), 'utf-8')
      expect(content).toBe('// Test file\nexport const x = 1')
    })

    it('creates directories when createDirectories is true', async () => {
      const plan = {
        writes: [
          {
            path: 'deep/nested/path/file.mjs',
            content: 'content',
            hash: 'abc',
            templateIri: 't1',
            entityIri: 'e1',
            entityType: 'Type',
          },
        ],
        updates: [],
        deletes: [],
      }

      const { result } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        createDirectories: true,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.appliedCount).toBe(1)

      const content = await fs.readFile(path.join(testDir, 'deep/nested/path/file.mjs'), 'utf-8')
      expect(content).toBe('content')
    })

    it('skips write if file already exists', async () => {
      // Create existing file
      await fs.mkdir(path.join(testDir, 'src'), { recursive: true })
      await fs.writeFile(path.join(testDir, 'src/exists.mjs'), 'existing content')

      const plan = {
        writes: [
          {
            path: 'src/exists.mjs',
            content: 'new content',
            hash: 'newhash',
            templateIri: 't1',
            entityIri: 'e1',
            entityType: 'Type',
          },
        ],
        updates: [],
        deletes: [],
      }

      const { result } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.skippedCount).toBe(1)
      expect(result.errors.length).toBe(1)
      expect(result.errors[0]).toContain('already exists')

      // Verify original content unchanged
      const content = await fs.readFile(path.join(testDir, 'src/exists.mjs'), 'utf-8')
      expect(content).toBe('existing content')
    })

    it('updates existing files when hash matches', async () => {
      // Create existing file with known content
      const existingContent = 'old content'
      const existingHash = require('crypto').createHash('sha256').update(existingContent).digest('hex')

      await fs.mkdir(path.join(testDir, 'src'), { recursive: true })
      await fs.writeFile(path.join(testDir, 'src/update.mjs'), existingContent)

      const plan = {
        writes: [],
        updates: [
          {
            path: 'src/update.mjs',
            content: 'new content',
            oldHash: existingHash,
            newHash: 'newhash',
            templateIri: 't1',
            entityIri: 'e1',
          },
        ],
        deletes: [],
      }

      const { result } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        validateHashes: true,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.appliedCount).toBe(1)
      expect(result.updatedPaths).toContain('src/update.mjs')

      const content = await fs.readFile(path.join(testDir, 'src/update.mjs'), 'utf-8')
      expect(content).toBe('new content')
    })

    it('skips update when hash does not match', async () => {
      await fs.mkdir(path.join(testDir, 'src'), { recursive: true })
      await fs.writeFile(path.join(testDir, 'src/file.mjs'), 'different content')

      const plan = {
        writes: [],
        updates: [
          {
            path: 'src/file.mjs',
            content: 'new content',
            oldHash: 'wronghash',
            newHash: 'newhash',
            templateIri: 't1',
            entityIri: 'e1',
          },
        ],
        deletes: [],
      }

      const { result } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        validateHashes: true,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.skippedCount).toBe(1)
      expect(result.errors[0]).toContain('Hash mismatch')
    })

    it('deletes files when hash matches', async () => {
      const content = 'to delete'
      const hash = require('crypto').createHash('sha256').update(content).digest('hex')

      await fs.mkdir(path.join(testDir, 'src'), { recursive: true })
      await fs.writeFile(path.join(testDir, 'src/delete.mjs'), content)

      const plan = {
        writes: [],
        updates: [],
        deletes: [
          {
            path: 'src/delete.mjs',
            hash,
            reason: 'No longer needed',
          },
        ],
      }

      const { result } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        validateHashes: true,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.appliedCount).toBe(1)
      expect(result.deletedPaths).toContain('src/delete.mjs')

      // Verify file is gone
      await expect(fs.access(path.join(testDir, 'src/delete.mjs'))).rejects.toThrow()
    })

    it('respects dryRun option', async () => {
      const plan = {
        writes: [
          {
            path: 'src/dry.mjs',
            content: 'content',
            hash: 'hash',
            templateIri: 't1',
            entityIri: 'e1',
            entityType: 'Type',
          },
        ],
        updates: [],
        deletes: [],
      }

      const { result } = await applyMaterializationPlan(plan, {
        outputRoot: testDir,
        dryRun: true,
        snapshotBefore: false,
        snapshotAfter: false,
      })

      expect(result.appliedCount).toBe(1)

      // File should NOT exist
      await expect(fs.access(path.join(testDir, 'src/dry.mjs'))).rejects.toThrow()
    })
  })

  describe('rollbackMaterialization', () => {
    it('deletes written files', async () => {
      // Create files that were "written"
      await fs.mkdir(path.join(testDir, 'src'), { recursive: true })
      await fs.writeFile(path.join(testDir, 'src/a.mjs'), 'a')
      await fs.writeFile(path.join(testDir, 'src/b.mjs'), 'b')

      const applyResult = {
        appliedCount: 2,
        skippedCount: 0,
        writtenPaths: ['src/a.mjs', 'src/b.mjs'],
        updatedPaths: [],
        deletedPaths: [],
        errors: [],
      }

      const { rolledBack, errors } = await rollbackMaterialization(applyResult, {
        outputRoot: testDir,
      })

      expect(rolledBack).toContain('src/a.mjs')
      expect(rolledBack).toContain('src/b.mjs')
      expect(errors).toEqual([])

      // Verify files are gone
      await expect(fs.access(path.join(testDir, 'src/a.mjs'))).rejects.toThrow()
      await expect(fs.access(path.join(testDir, 'src/b.mjs'))).rejects.toThrow()
    })

    it('handles missing files gracefully', async () => {
      const applyResult = {
        appliedCount: 1,
        skippedCount: 0,
        writtenPaths: ['nonexistent.mjs'],
        updatedPaths: [],
        deletedPaths: [],
        errors: [],
      }

      const { rolledBack, errors } = await rollbackMaterialization(applyResult, {
        outputRoot: testDir,
      })

      // Should not error for missing files
      expect(errors).toEqual([])
    })
  })

  describe('previewPlan', () => {
    it('summarizes plan without applying', () => {
      const plan = {
        writes: [
          { path: 'a.mjs', content: 'a', hash: 'h1', templateIri: 't1', entityIri: 'e1', entityType: 'Type' },
          { path: 'b.mjs', content: 'b', hash: 'h2', templateIri: 't2', entityIri: 'e2', entityType: 'Type' },
        ],
        updates: [
          { path: 'c.mjs', content: 'c', oldHash: 'old', newHash: 'new', templateIri: 't3', entityIri: 'e3' },
        ],
        deletes: [
          { path: 'd.mjs', hash: 'h4', reason: 'cleanup' },
        ],
      }

      const preview = previewPlan(plan)

      expect(preview.totalOperations).toBe(4)
      expect(preview.writes.length).toBe(2)
      expect(preview.updates.length).toBe(1)
      expect(preview.deletes.length).toBe(1)
      expect(preview.writes[0].path).toBe('a.mjs')
      expect(preview.deletes[0].reason).toBe('cleanup')
    })
  })

  describe('checkPlanApplicability', () => {
    it('returns canApply true for valid plan', async () => {
      const plan = {
        writes: [
          { path: 'new.mjs', content: 'content', hash: 'h', templateIri: 't', entityIri: 'e', entityType: 'T' },
        ],
        updates: [],
        deletes: [],
      }

      const { canApply, issues } = await checkPlanApplicability(plan, {
        outputRoot: testDir,
      })

      expect(canApply).toBe(true)
      expect(issues).toEqual([])
    })

    it('detects write conflicts', async () => {
      // Create existing file
      await fs.mkdir(path.join(testDir, 'src'), { recursive: true })
      await fs.writeFile(path.join(testDir, 'src/exists.mjs'), 'content')

      const plan = {
        writes: [
          { path: 'src/exists.mjs', content: 'new', hash: 'h', templateIri: 't', entityIri: 'e', entityType: 'T' },
        ],
        updates: [],
        deletes: [],
      }

      const { canApply, issues } = await checkPlanApplicability(plan, {
        outputRoot: testDir,
      })

      expect(canApply).toBe(false)
      expect(issues.length).toBe(1)
      expect(issues[0]).toContain('Write conflict')
    })

    it('detects missing update targets', async () => {
      const plan = {
        writes: [],
        updates: [
          { path: 'missing.mjs', content: 'new', oldHash: 'old', newHash: 'new', templateIri: 't', entityIri: 'e' },
        ],
        deletes: [],
      }

      const { canApply, issues } = await checkPlanApplicability(plan, {
        outputRoot: testDir,
      })

      expect(canApply).toBe(false)
      expect(issues[0]).toContain('Update target missing')
    })
  })
})
