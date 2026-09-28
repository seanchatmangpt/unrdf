import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdtemp, rm, readFile, writeFile, mkdir, access } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createHash } from 'node:crypto';
import {
  applyMaterializationPlan,
  checkPlanApplicability,
  rollbackMaterialization,
  previewPlan,
} from '../src/index.mjs';

const sha = s => createHash('sha256').update(s).digest('hex');
const write = (path, content) => ({
  path,
  content,
  hash: sha(content),
  templateIri: 'http://example.org/t',
  entityIri: 'http://example.org/e',
  entityType: 'Thing',
});
const plan = (over = {}) => ({ writes: [], updates: [], deletes: [], ...over });
const exists = p =>
  access(p).then(
    () => true,
    () => false
  );

describe('materialize-apply', () => {
  let root;
  let outside;

  beforeEach(async () => {
    const base = await mkdtemp(join(tmpdir(), 'pe-'));
    root = join(base, 'root');
    outside = join(base, 'outside');
    await mkdir(root);
    await mkdir(outside);
  });

  afterEach(async () => {
    await rm(join(root, '..'), { recursive: true, force: true });
  });

  it('writes files, snapshots before/after and reports the diff', async () => {
    const { result, receipt } = await applyMaterializationPlan(
      plan({ writes: [write('src/a.mjs', 'export const a = 1;\n')] }),
      { outputRoot: root }
    );
    expect(result.writtenPaths).toEqual(['src/a.mjs']);
    expect(result.errors).toEqual([]);
    expect(await readFile(join(root, 'src/a.mjs'), 'utf-8')).toBe('export const a = 1;\n');
    expect(receipt.success).toBe(true);
    expect(receipt.fsDiff.added).toEqual(['src', 'src/a.mjs']);
    expect(receipt.fsDiff.removed).toEqual([]);
  });

  it('refuses to write outside outputRoot via ../ traversal', async () => {
    const target = join(outside, 'pwned.txt');
    const { result } = await applyMaterializationPlan(
      plan({ writes: [write('../outside/pwned.txt', 'x')] }),
      { outputRoot: root }
    );
    expect(result.writtenPaths).toEqual([]);
    expect(result.errors[0]).toMatch(/escapes output root/);
    expect(await exists(target)).toBe(false);
  });

  it('refuses absolute paths', async () => {
    const target = join(outside, 'abs.txt');
    const { result } = await applyMaterializationPlan(plan({ writes: [write(target, 'x')] }), {
      outputRoot: root,
    });
    expect(result.errors[0]).toMatch(/escapes output root/);
    expect(await exists(target)).toBe(false);
  });

  it('refuses to delete files outside outputRoot', async () => {
    const victim = join(outside, 'keep.txt');
    await writeFile(victim, 'keep');
    const { result } = await applyMaterializationPlan(
      plan({
        deletes: [{ path: '../outside/keep.txt', hash: sha('keep'), reason: 'test' }],
      }),
      { outputRoot: root }
    );
    expect(result.deletedPaths).toEqual([]);
    expect(result.errors[0]).toMatch(/escapes output root/);
    expect(await exists(victim)).toBe(true);
  });

  it('checkPlanApplicability and rollback also reject traversal', async () => {
    const check = await checkPlanApplicability(plan({ writes: [write('../outside/x', 'x')] }), {
      outputRoot: root,
    });
    expect(check.canApply).toBe(false);
    expect(check.issues[0]).toMatch(/escapes output root/);

    const victim = join(outside, 'r.txt');
    await writeFile(victim, 'r');
    const rb = await rollbackMaterialization(
      { writtenPaths: ['../outside/r.txt'] },
      { outputRoot: root }
    );
    expect(rb.rolledBack).toEqual([]);
    expect(rb.errors[0]).toMatch(/escapes output root/);
    expect(await exists(victim)).toBe(true);
  });

  it('updates only when the hash matches and deletes files', async () => {
    await writeFile(join(root, 'f.txt'), 'old');
    const bad = await applyMaterializationPlan(
      plan({
        updates: [
          {
            path: 'f.txt',
            content: 'new',
            oldHash: 'wrong',
            newHash: sha('new'),
            templateIri: 't',
            entityIri: 'e',
          },
        ],
      }),
      { outputRoot: root }
    );
    expect(bad.result.errors[0]).toMatch(/Hash mismatch/);
    expect(await readFile(join(root, 'f.txt'), 'utf-8')).toBe('old');

    const del = await applyMaterializationPlan(
      plan({ deletes: [{ path: 'f.txt', hash: sha('old'), reason: 'gone' }] }),
      { outputRoot: root }
    );
    expect(del.result.deletedPaths).toEqual(['f.txt']);
    expect(await exists(join(root, 'f.txt'))).toBe(false);
  });

  it('previewPlan summarises operations', () => {
    expect(previewPlan(plan({ writes: [write('a', 'x')] })).totalOperations).toBe(1);
  });
});
