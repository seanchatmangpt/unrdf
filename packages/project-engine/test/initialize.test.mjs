/**
 * @file Tests for the project initialization pipeline (real filesystem, real Oxigraph store)
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import os from 'os';
import path from 'path';
import {
  createProjectInitializationPipeline,
  inferEntityName,
  scanFileSystemToStore,
  classifyPath,
  diffProjectStructure,
} from '../src/index.mjs';

async function write(root, rel, content = 'x\n') {
  const file = path.join(root, rel);
  await fs.mkdir(path.dirname(file), { recursive: true });
  await fs.writeFile(file, content, 'utf8');
}

async function makeProject(root) {
  await write(root, 'package.json', '{"name":"demo"}\n');
  await write(root, 'pnpm-lock.yaml', 'lockfileVersion: 9\n');
  await write(root, 'vitest.config.mjs', 'export default {};\n');
  await write(root, 'src/index.mjs', 'export {};\n');
  await write(root, 'src/users/user.service.mjs', 'export const a = 1;\n');
  await write(root, 'src/orders/order.mjs', 'export const b = 2;\n');
  await write(root, 'src/utils/helpers.mjs', 'export const c = 3;\n');
  await write(root, 'test/users.test.mjs', 'export {};\n');
  await write(root, 'node_modules/dep/index.js', 'ignored\n');
}

describe('createProjectInitializationPipeline', () => {
  let root;

  beforeEach(async () => {
    root = await fs.mkdtemp(path.join(os.tmpdir(), 'unrdf-init-'));
    await makeProject(root);
  });

  afterEach(async () => {
    await fs.rm(root, { recursive: true, force: true });
  });

  it('runs every phase and writes the output files', async () => {
    const result = await createProjectInitializationPipeline(root);

    expect(result.success).toBe(true);
    for (const phase of [
      'scan', 'stackDetection', 'projectModel', 'fileRoles',
      'domainInference', 'templateInference', 'snapshot', 'hooks', 'report', 'persist',
    ]) {
      expect(result.receipt.phases[phase]?.success, phase).toBe(true);
    }

    // node_modules ignored: package.json, pnpm-lock.yaml, vitest.config.mjs, 4 src files, 1 test
    expect(result.receipt.phases.scan.data.files).toBe(8);

    const names = (await fs.readdir(path.join(root, '.unrdf'))).sort();
    expect(names).toEqual(['domain.nt', 'hooks.json', 'init-report.json', 'snapshot.json', 'templates.json']);
    expect(result.outputs).toHaveLength(5);

    const persisted = JSON.parse(await fs.readFile(path.join(root, '.unrdf/init-report.json'), 'utf8'));
    expect(persisted).toEqual(JSON.parse(JSON.stringify(result.report)));
  });

  it('detects stack, features, roles, entities and missing tests', async () => {
    const { report } = await createProjectInitializationPipeline(root);

    expect(report.stack).toMatchObject({ testFramework: 'vitest', packageManager: 'pnpm' });
    expect(report.stackProfile).toBe('vitest');
    expect(report.stats.totalFiles).toBe(8);
    expect(report.stats.featureCount).toBe(3);
    expect(report.stats.filesByRole).toMatchObject({ Test: 1, Service: 1 });

    const byName = Object.fromEntries(report.features.map(f => [f.name, f]));
    expect(Object.keys(byName).sort()).toEqual(['orders', 'users', 'utils']);
    expect(byName.users.roles.Service).toBe(true);
    expect(byName.users.hasMissingTests).toBe(false); // test/users.test.mjs
    expect(byName.orders.hasMissingTests).toBe(true);

    // utils is a non-entity folder; users/orders are singularised entities
    expect(report.domainEntities.map(e => e.name)).toEqual(['Order', 'User']);
    expect(report.domainEntities.every(e => e.fieldCount >= 3)).toBe(true);
    expect(report.stats.testCoverageAverage).toBeCloseTo(100 / 3, 5);

    expect(report.hooks.map(h => h.name)).toContain('validateUser');
    expect(report.hooks.every(h => ['before-insert', 'after-insert'].includes(h.trigger))).toBe(true);
  });

  it('writes a content-hash snapshot that is stable across runs and changes with content', async () => {
    const first = await createProjectInitializationPipeline(root);
    const second = await createProjectInitializationPipeline(root); // .unrdf must be ignored by the scan
    expect(second.report.snapshotHash).toBe(first.report.snapshotHash);
    expect(second.report.stats.totalFiles).toBe(8);

    const snapshot = JSON.parse(await fs.readFile(path.join(root, '.unrdf/snapshot.json'), 'utf8'));
    expect(snapshot.hash).toBe(first.report.snapshotHash);
    const entry = snapshot.files.find(f => f.path === 'src/orders/order.mjs');
    expect(entry.size).toBe('export const b = 2;\n'.length);
    expect(entry.contentHash).toMatch(/^[0-9a-f]{64}$/);

    await write(root, 'src/orders/order.mjs', 'export const b = 3;\n');
    const third = await createProjectInitializationPipeline(root);
    expect(third.report.snapshotHash).not.toBe(first.report.snapshotHash);
  });

  it('writes valid N-Triples for the domain model', async () => {
    await createProjectInitializationPipeline(root);
    const nt = await fs.readFile(path.join(root, '.unrdf/domain.nt'), 'utf8');
    expect(nt).toContain('<http://example.org/unrdf/domain#User>');
    expect(nt).toContain('http://example.org/unrdf/domain#Entity');
  });

  it('dryRun computes everything but writes nothing', async () => {
    const result = await createProjectInitializationPipeline(root, { dryRun: true });
    expect(result.success).toBe(true);
    expect(result.outputs).toEqual([]);
    expect(result.receipt.phases.persist).toBeUndefined();
    await expect(fs.stat(path.join(root, '.unrdf'))).rejects.toThrow(/ENOENT/);
  });

  it('skipSnapshot and skipHooks omit those phases and files', async () => {
    const result = await createProjectInitializationPipeline(root, { skipSnapshot: true, skipHooks: true });
    expect(result.receipt.phases.snapshot).toBeUndefined();
    expect(result.receipt.phases.hooks).toBeUndefined();
    const names = (await fs.readdir(path.join(root, '.unrdf'))).sort();
    expect(names).toEqual(['domain.nt', 'init-report.json', 'templates.json']);
  });

  it('fails cleanly for a missing root, without writing anything', async () => {
    const missing = path.join(root, 'does-not-exist');
    const result = await createProjectInitializationPipeline(missing);
    expect(result.success).toBe(false);
    expect(result.error).toMatch(/^scan: /);
    expect(result.receipt.failedPhase).toBe('scan');
    await expect(fs.stat(missing)).rejects.toThrow(/ENOENT/);
  });

  it('rejects an outputDir outside the project root', async () => {
    const result = await createProjectInitializationPipeline(root, { outputDir: '../escape' });
    expect(result.success).toBe(false);
    expect(result.error).toMatch(/outputDir must be inside the project root/);
  });

  it('validates options', async () => {
    await expect(createProjectInitializationPipeline(root, { skipPhases: ['nope'] })).rejects.toThrow();
    await expect(createProjectInitializationPipeline('')).rejects.toThrow(TypeError);
  });
});

describe('building blocks', () => {
  let root;

  beforeEach(async () => {
    root = await fs.mkdtemp(path.join(os.tmpdir(), 'unrdf-scan-'));
  });

  afterEach(async () => {
    await fs.rm(root, { recursive: true, force: true });
  });

  it('scanFileSystemToStore honours glob ignores with regex metacharacters escaped', async () => {
    await write(root, 'a.swp');
    await write(root, 'aXswp'); // must NOT match "*.swp" (the dot is literal)
    await write(root, 'keep.mjs');
    const { summary } = await scanFileSystemToStore({ root, ignorePatterns: ['*.swp'] });
    expect(summary.fileCount).toBe(2);
    expect(summary.ignoredCount).toBe(1);
  });

  it('inferEntityName singularises and PascalCases, skipping non-entities', () => {
    expect(inferEntityName('users')).toBe('User');
    expect(inferEntityName('categories')).toBe('Category');
    expect(inferEntityName('user-profiles')).toBe('UserProfile');
    expect(inferEntityName('status')).toBe('Status');
    expect(inferEntityName('utils')).toBeNull();
  });

  it('classifyPath puts specific roles before generic ones', () => {
    expect(classifyPath('src/a/b.test.ts')).toBe('Test');
    expect(classifyPath('src/Button.tsx')).toBe('Component');
    expect(classifyPath('src/users/user.service.mjs')).toBe('Service');
    expect(classifyPath('src/index.mjs')).toBe('Other');
  });

  it('diffProjectStructure reports added features between two scans', async () => {
    const { createProjectInitializationPipeline: run } = await import('../src/index.mjs');
    await write(root, 'src/users/a.mjs');
    const before = await run(root, { dryRun: true });
    await write(root, 'src/orders/b.mjs');
    const after = await run(root, { dryRun: true });

    const diff = diffProjectStructure({
      goldenStore: before.state.projectStore,
      actualStore: after.state.projectStore,
    });
    const kinds = diff.changes.map(c => c.kind);
    expect(kinds).toContain('FeatureAdded');
  });
});
