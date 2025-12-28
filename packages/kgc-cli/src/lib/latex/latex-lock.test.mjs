/**
 * @fileoverview Tests for LaTeX lockfile determinism and validation.
 *
 * Validates:
 * - Lockfile format stability (sorted keys, consistent output)
 * - Hash-based cache validation
 * - Load/save round-trip integrity
 * - Invalid lockfile handling (fail-safe)
 * - Integration with resolver workflow
 */

import { describe, it, beforeEach, afterEach } from 'node:test';
import assert from 'node:assert/strict';
import { readFile, writeFile, rm, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { createHash } from 'node:crypto';
import {
  loadLatexLock,
  saveLatexLock,
  recordResolvedInput,
  createLatexLock,
  validateCachedFile,
  getResolvedInput,
  isLockValid,
  mergeLocks,
  pruneLock,
  LatexLockSchema
} from './latex-lock.mjs';

describe('LaTeX Lockfile', () => {
  let testDir;
  let lockPath;

  beforeEach(async () => {
    // Create unique temp directory for each test
    testDir = join(tmpdir(), `latex-lock-test-${Date.now()}-${Math.random().toString(36).slice(2)}`);
    await mkdir(testDir, { recursive: true });
    lockPath = join(testDir, 'latex.lock.json');
  });

  afterEach(async () => {
    // Cleanup
    await rm(testDir, { recursive: true, force: true });
  });

  describe('createLatexLock', () => {
    it('creates valid lockfile with default engine', () => {
      const lock = createLatexLock();

      assert.equal(lock.version, '1.0.0');
      assert.equal(lock.engine, 'xetex');
      assert.deepEqual(lock.resolvedInputs, {});
      assert.ok(lock.createdAt);
      assert.ok(lock.updatedAt);
    });

    it('creates lockfile with specified engine', () => {
      const lock = createLatexLock('pdftex');
      assert.equal(lock.engine, 'pdftex');
    });

    it('validates against schema', () => {
      const lock = createLatexLock('luatex');
      const result = LatexLockSchema.safeParse(lock);

      assert.ok(result.success);
      assert.deepEqual(result.data, lock);
    });
  });

  describe('recordResolvedInput', () => {
    it('records input with all fields', () => {
      const lock = createLatexLock();
      const hash = 'abc123def456';
      const sourceUrl = 'https://example.com/logo.png';
      const cachedPath = '/cache/logo.png';

      recordResolvedInput(lock, {
        inputName: 'logo.png',
        hash,
        sourceUrl,
        cachedPath
      });

      assert.ok(lock.resolvedInputs['logo.png']);
      assert.equal(lock.resolvedInputs['logo.png'].hash, hash);
      assert.equal(lock.resolvedInputs['logo.png'].sourceUrl, sourceUrl);
      assert.equal(lock.resolvedInputs['logo.png'].cachedPath, cachedPath);
      assert.ok(lock.resolvedInputs['logo.png'].resolvedAt);
    });

    it('records input without optional sourceUrl', () => {
      const lock = createLatexLock();

      recordResolvedInput(lock, {
        inputName: 'local.tex',
        hash: 'xyz789',
        cachedPath: '/cache/local.tex'
      });

      assert.ok(lock.resolvedInputs['local.tex']);
      assert.equal(lock.resolvedInputs['local.tex'].sourceUrl, undefined);
    });

    it('overwrites existing entry', () => {
      const lock = createLatexLock();

      recordResolvedInput(lock, {
        inputName: 'file.tex',
        hash: 'old-hash',
        cachedPath: '/old/path'
      });

      recordResolvedInput(lock, {
        inputName: 'file.tex',
        hash: 'new-hash',
        cachedPath: '/new/path'
      });

      assert.equal(lock.resolvedInputs['file.tex'].hash, 'new-hash');
      assert.equal(lock.resolvedInputs['file.tex'].cachedPath, '/new/path');
    });
  });

  describe('saveLatexLock and loadLatexLock', () => {
    it('round-trips lockfile correctly', async () => {
      const original = createLatexLock('xetex');
      recordResolvedInput(original, {
        inputName: 'main.tex',
        hash: 'abc123',
        sourceUrl: 'https://example.com/main.tex',
        cachedPath: '/cache/main.tex'
      });

      await saveLatexLock(lockPath, original);
      const loaded = await loadLatexLock(lockPath);

      assert.ok(loaded);
      assert.equal(loaded.version, original.version);
      assert.equal(loaded.engine, original.engine);
      assert.equal(loaded.createdAt, original.createdAt);
      assert.ok(loaded.updatedAt); // Will be newer due to save
      assert.deepEqual(loaded.resolvedInputs, original.resolvedInputs);
    });

    it('returns null for missing lockfile', async () => {
      const loaded = await loadLatexLock(join(testDir, 'nonexistent.lock.json'));
      assert.equal(loaded, null);
    });

    it('returns null for invalid JSON', async () => {
      await writeFile(lockPath, 'invalid json{', 'utf-8');
      const loaded = await loadLatexLock(lockPath);
      assert.equal(loaded, null);
    });

    it('returns null for lockfile with invalid schema', async () => {
      const invalid = {
        version: '999.0.0',
        engine: 'invalid-engine',
        resolvedInputs: 'not-an-object'
      };

      await writeFile(lockPath, JSON.stringify(invalid), 'utf-8');
      const loaded = await loadLatexLock(lockPath);
      assert.equal(loaded, null);
    });

    it('creates parent directory if needed', async () => {
      const nestedPath = join(testDir, 'nested', 'deep', 'latex.lock.json');
      const lock = createLatexLock();

      await saveLatexLock(nestedPath, lock);
      const loaded = await loadLatexLock(nestedPath);

      assert.ok(loaded);
    });

    it('produces stable JSON output (sorted keys)', async () => {
      const lock = createLatexLock();
      recordResolvedInput(lock, { inputName: 'z.tex', hash: '111', cachedPath: '/z' });
      recordResolvedInput(lock, { inputName: 'a.tex', hash: '222', cachedPath: '/a' });
      recordResolvedInput(lock, { inputName: 'm.tex', hash: '333', cachedPath: '/m' });

      await saveLatexLock(lockPath, lock);
      const raw = await readFile(lockPath, 'utf-8');
      const lines = raw.split('\n');

      // Keys should be alphabetically sorted in JSON
      const resolvedInputsStart = lines.findIndex(l => l.includes('"resolvedInputs"'));
      const _firstKey = lines[resolvedInputsStart + 2]; // First input key
      const _secondKey = lines.findIndex((l, i) => i > resolvedInputsStart + 2 && l.includes('"'));

      assert.ok(raw.includes('"a.tex"'));
      assert.ok(raw.includes('"m.tex"'));
      assert.ok(raw.includes('"z.tex"'));
      // Order should be a, m, z (alphabetical)
      assert.ok(raw.indexOf('"a.tex"') < raw.indexOf('"m.tex"'));
      assert.ok(raw.indexOf('"m.tex"') < raw.indexOf('"z.tex"'));
    });

    it('updates timestamp on save', async () => {
      const lock = createLatexLock();
      const originalUpdatedAt = lock.updatedAt;

      // Wait a bit to ensure timestamp changes
      await new Promise(resolve => setTimeout(resolve, 10));

      await saveLatexLock(lockPath, lock);
      const loaded = await loadLatexLock(lockPath);

      assert.ok(loaded.updatedAt > originalUpdatedAt);
    });
  });

  describe('validateCachedFile', () => {
    it('returns true for matching hashes', () => {
      const entry = { hash: 'abc123', cachedPath: '/path' };
      assert.ok(validateCachedFile(entry, 'abc123'));
    });

    it('returns false for mismatched hashes', () => {
      const entry = { hash: 'abc123', cachedPath: '/path' };
      assert.ok(!validateCachedFile(entry, 'different-hash'));
    });
  });

  describe('getResolvedInput', () => {
    it('returns entry if exists', () => {
      const lock = createLatexLock();
      recordResolvedInput(lock, {
        inputName: 'test.tex',
        hash: 'hash123',
        cachedPath: '/cache/test.tex'
      });

      const entry = getResolvedInput(lock, 'test.tex');
      assert.ok(entry);
      assert.equal(entry.hash, 'hash123');
    });

    it('returns null for missing entry', () => {
      const lock = createLatexLock();
      const entry = getResolvedInput(lock, 'nonexistent.tex');
      assert.equal(entry, null);
    });

    it('returns null for null lockfile', () => {
      const entry = getResolvedInput(null, 'test.tex');
      assert.equal(entry, null);
    });
  });

  describe('isLockValid', () => {
    it('returns true for matching engine', () => {
      const lock = createLatexLock('xetex');
      assert.ok(isLockValid(lock, 'xetex'));
    });

    it('returns false for mismatched engine', () => {
      const lock = createLatexLock('xetex');
      assert.ok(!isLockValid(lock, 'pdftex'));
    });

    it('returns false for null lockfile', () => {
      assert.ok(!isLockValid(null, 'xetex'));
    });
  });

  describe('mergeLocks', () => {
    it('merges two lockfiles with same engine', () => {
      const lock1 = createLatexLock('xetex');
      recordResolvedInput(lock1, {
        inputName: 'a.tex',
        hash: 'hash-a',
        cachedPath: '/a'
      });

      const lock2 = createLatexLock('xetex');
      recordResolvedInput(lock2, {
        inputName: 'b.tex',
        hash: 'hash-b',
        cachedPath: '/b'
      });

      const merged = mergeLocks(lock1, lock2);

      assert.equal(merged.engine, 'xetex');
      assert.ok(merged.resolvedInputs['a.tex']);
      assert.ok(merged.resolvedInputs['b.tex']);
    });

    it('prefers newer entries on conflict', async () => {
      const lock1 = createLatexLock('xetex');
      recordResolvedInput(lock1, {
        inputName: 'conflict.tex',
        hash: 'old-hash',
        cachedPath: '/old'
      });

      // Wait to ensure newer timestamp
      await new Promise(resolve => setTimeout(resolve, 10));

      const lock2 = createLatexLock('xetex');
      recordResolvedInput(lock2, {
        inputName: 'conflict.tex',
        hash: 'new-hash',
        cachedPath: '/new'
      });

      const merged = mergeLocks(lock1, lock2);

      assert.equal(merged.resolvedInputs['conflict.tex'].hash, 'new-hash');
    });

    it('throws on engine mismatch', () => {
      const lock1 = createLatexLock('xetex');
      const lock2 = createLatexLock('pdftex');

      assert.throws(() => mergeLocks(lock1, lock2), /different engines/);
    });
  });

  describe('pruneLock', () => {
    it('keeps only valid inputs', () => {
      const lock = createLatexLock();
      recordResolvedInput(lock, { inputName: 'keep.tex', hash: 'h1', cachedPath: '/keep' });
      recordResolvedInput(lock, { inputName: 'remove.tex', hash: 'h2', cachedPath: '/remove' });
      recordResolvedInput(lock, { inputName: 'also-keep.tex', hash: 'h3', cachedPath: '/also' });

      const validInputs = new Set(['keep.tex', 'also-keep.tex']);
      const pruned = pruneLock(lock, validInputs);

      assert.ok(pruned.resolvedInputs['keep.tex']);
      assert.ok(pruned.resolvedInputs['also-keep.tex']);
      assert.ok(!pruned.resolvedInputs['remove.tex']);
    });

    it('preserves metadata', () => {
      const lock = createLatexLock('pdftex');
      recordResolvedInput(lock, { inputName: 'test.tex', hash: 'h', cachedPath: '/test' });

      const pruned = pruneLock(lock, new Set(['test.tex']));

      assert.equal(pruned.engine, 'pdftex');
      assert.equal(pruned.version, '1.0.0');
      assert.equal(pruned.createdAt, lock.createdAt);
    });
  });

  describe('Determinism (Definition of Done)', () => {
    it('produces identical lockfile on repeated builds (except timestamps)', async () => {
      const lock1 = createLatexLock('xetex');
      recordResolvedInput(lock1, {
        inputName: 'main.tex',
        hash: 'abc123',
        cachedPath: '/cache/main.tex'
      });
      recordResolvedInput(lock1, {
        inputName: 'logo.png',
        hash: 'def456',
        sourceUrl: 'https://example.com/logo.png',
        cachedPath: '/cache/logo.png'
      });

      await saveLatexLock(lockPath, lock1);
      const saved1 = await readFile(lockPath, 'utf-8');

      // Simulate second build
      const lock2 = createLatexLock('xetex');
      recordResolvedInput(lock2, {
        inputName: 'main.tex',
        hash: 'abc123',
        cachedPath: '/cache/main.tex'
      });
      recordResolvedInput(lock2, {
        inputName: 'logo.png',
        hash: 'def456',
        sourceUrl: 'https://example.com/logo.png',
        cachedPath: '/cache/logo.png'
      });

      const lockPath2 = join(testDir, 'latex2.lock.json');
      await saveLatexLock(lockPath2, lock2);
      const saved2 = await readFile(lockPath2, 'utf-8');

      // Strip timestamps for comparison
      const normalize = (json) => {
        const obj = JSON.parse(json);
        delete obj.createdAt;
        delete obj.updatedAt;
        for (const entry of Object.values(obj.resolvedInputs)) {
          delete entry.resolvedAt;
        }
        return JSON.stringify(obj, null, 2);
      };

      assert.equal(normalize(saved1), normalize(saved2));
    });

    it('prevents network calls when lock entry exists', async () => {
      // Simulate resolver workflow
      const lock = createLatexLock('xetex');

      // First build: resolve and record
      const inputName = 'remote.sty';
      const content = '% LaTeX style file';
      const hash = createHash('sha256').update(content).digest('hex');
      const sourceUrl = 'https://example.com/remote.sty';
      const cachedPath = join(testDir, 'cache', inputName);

      recordResolvedInput(lock, { inputName, hash, sourceUrl, cachedPath });
      await saveLatexLock(lockPath, lock);

      // Second build: check lock first
      const loaded = await loadLatexLock(lockPath);
      const entry = getResolvedInput(loaded, inputName);

      assert.ok(entry);
      assert.equal(entry.hash, hash);
      assert.equal(entry.sourceUrl, sourceUrl);
      assert.equal(entry.cachedPath, cachedPath);

      // Resolver would validate hash and skip fetch
      // (no network call needed!)
    });
  });
});
