/**
 * @file Doctor Fixes Tests
 * @module cli/test/commands/doctor/fixes
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { getFixableChecks, applyAutoFix } from '../../../src/cli/commands/doctor/fixes/index.mjs';
import * as childProcess from 'node:child_process';
import * as fs from 'node:fs';
import { glob } from 'glob';

vi.mock('node:child_process', () => ({
  execSync: vi.fn(),
}));

vi.mock('node:fs', () => ({
  existsSync: vi.fn(),
  copyFileSync: vi.fn(),
  readFileSync: vi.fn(),
  writeFileSync: vi.fn(),
}));

vi.mock('glob', () => ({
  glob: {
    sync: vi.fn().mockReturnValue([]),
  }
}));

describe('Doctor Auto-Fixes', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('should list all fixable checks', () => {
    const checks = getFixableChecks();
    expect(checks).toContain('node_modules consistency');
    expect(checks).toContain('Workspace dependencies');
    expect(checks).toContain('Build artifacts');
    expect(checks).toContain('ESLint status');
    expect(checks).toContain('Environment variables');
    expect(checks).toContain('N3 import violations');
    expect(checks).toContain('Skipped tests');
  });

  it('should throw an error for non-fixable checks', async () => {
    await expect(applyAutoFix({ name: 'Node.js version' })).rejects.toThrow(/No auto-fix available/);
  });

  it('should execute fix for node_modules consistency', async () => {
    await applyAutoFix({ name: 'node_modules consistency' });
    expect(childProcess.execSync).toHaveBeenCalledWith('pnpm install --frozen-lockfile', expect.any(Object));
  });

  it('should execute fix for Workspace dependencies', async () => {
    await applyAutoFix({ name: 'Workspace dependencies' });
    expect(childProcess.execSync).toHaveBeenCalledWith('pnpm install', expect.any(Object));
  });

  it('should execute fix for Build artifacts', async () => {
    await applyAutoFix({ name: 'Build artifacts' });
    expect(childProcess.execSync).toHaveBeenCalledWith('pnpm build', expect.any(Object));
  });

  it('should execute fix for ESLint status', async () => {
    await applyAutoFix({ name: 'ESLint status' });
    expect(childProcess.execSync).toHaveBeenCalledWith('pnpm lint:fix', expect.any(Object));
  });

  it('should execute fix for Environment variables', async () => {
    fs.existsSync.mockImplementation((path) => {
      if (path.includes('.env.example')) return true;
      if (path.includes('.env.local')) return false;
      return false;
    });

    await applyAutoFix({ name: 'Environment variables' });
    expect(fs.copyFileSync).toHaveBeenCalled();
  });

  it('should not copy .env.local if it already exists', async () => {
    fs.existsSync.mockReturnValue(true);
    await applyAutoFix({ name: 'Environment variables' });
    expect(fs.copyFileSync).not.toHaveBeenCalled();
  });

  it('should execute fix for N3 import violations', async () => {
    glob.sync.mockReturnValue(['src/test.mjs']);
    fs.readFileSync.mockReturnValue("import { Parser } from 'n3';\nimport N3 from 'n3';");
    
    await applyAutoFix({ name: 'N3 import violations' });
    
    expect(fs.writeFileSync).toHaveBeenCalled();
    const [path, content] = fs.writeFileSync.mock.calls[0];
    expect(content).toContain("import { Parser  } from '@unrdf/core/rdf/n3-justified-only.mjs'");
    expect(content).toContain("import * as N3 from '@unrdf/core/rdf/n3-justified-only.mjs'");
  });

  it('should not write file if no N3 imports found', async () => {
    glob.sync.mockReturnValue(['src/clean.mjs']);
    fs.readFileSync.mockReturnValue("import { something } from 'other';");
    
    await applyAutoFix({ name: 'N3 import violations' });
    expect(fs.writeFileSync).not.toHaveBeenCalled();
  });

  it('should execute fix for Skipped tests', async () => {
    glob.sync.mockReturnValue(['test/example.test.mjs']);
    fs.readFileSync.mockReturnValue("describe.skip('test', () => {}); it.skip('test'); test.skip('test'); xit('test');");
    
    await applyAutoFix({ name: 'Skipped tests' });
    
    expect(fs.writeFileSync).toHaveBeenCalled();
    const [path, content] = fs.writeFileSync.mock.calls[0];
    expect(content).toBe("describe('test', () => {}); it('test'); test('test'); it('test');");
  });
});
