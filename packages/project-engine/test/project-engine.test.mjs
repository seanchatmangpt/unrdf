/**
 * @vitest-environment node
 */
import { describe, it, expect } from 'vitest';
import { writeFile, mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

import {
  generateApiDocs,
  generatePackageGuide,
  generateChangelog,
} from '../src/project-engine/doc-generator.mjs';

import {
  analyzePackage,
  findExports,
  countCoverage,
} from '../src/project-engine/code-analyzer.mjs';

import { buildPackage, verifyPackage, listPackages } from '../src/project-engine/build-utils.mjs';

import { reportMetrics } from '../src/project-engine/metrics.mjs';

import {
  createProjectConfig,
  setupDevEnvironment,
  createDeploymentConfig,
} from '../src/project-engine/infrastructure.mjs';

/**
 * @unrdf/project-engine Test Suite
 */

describe('doc-generator', () => {
  it('generateApiDocs should extract function documentation', async () => {
    const testDir = join(tmpdir(), 'test-pkg-' + Date.now());
    await mkdir(join(testDir, 'src'), { recursive: true });

    const srcCode = `
/**
 * Test function
 * @param {string} input - Input string
 * @returns {string} Output string
 */
export function testFn(input) {
  return input;
}
    `;

    await writeFile(join(testDir, 'src', 'index.mjs'), srcCode);

    const docs = await generateApiDocs(testDir);

    expect(docs).toContain('# API Documentation');
    expect(docs).toContain('testFn');
    expect(docs).toContain('Test function');

    await rm(testDir, { recursive: true, force: true });
  });

  it('generatePackageGuide should create guide template', async () => {
    const testDir = join(tmpdir(), 'test-pkg-' + Date.now());
    await mkdir(testDir, { recursive: true });

    const packageJson = {
      name: '@test/package',
      description: 'Test package',
      license: 'MIT',
    };

    await writeFile(join(testDir, 'package.json'), JSON.stringify(packageJson, null, 2));

    const guide = await generatePackageGuide(testDir);

    expect(guide).toContain('# @test/package');
    expect(guide).toContain('Test package');
    expect(guide).toContain('Installation');
    expect(guide).toContain('Quick Start');

    await rm(testDir, { recursive: true, force: true });
  });

  it('generateChangelog should fail gracefully for non-git directories', async () => {
    const testDir = join(tmpdir(), 'test-nongit-' + Date.now());
    await mkdir(testDir, { recursive: true });

    await expect(generateChangelog(testDir)).rejects.toThrow();

    await rm(testDir, { recursive: true, force: true });
  });

  it('generateApiDocs should throw TypeError for non-string input', async () => {
    await expect(generateApiDocs(123)).rejects.toThrow(TypeError);
  });

  it('generatePackageGuide should throw TypeError for non-string input', async () => {
    await expect(generatePackageGuide(null)).rejects.toThrow(TypeError);
  });

  it('generateChangelog should throw TypeError for non-string input', async () => {
    await expect(generateChangelog([])).rejects.toThrow(TypeError);
  });
});

describe('code-analyzer', () => {
  it('analyzePackage should analyze package metrics', async () => {
    const testDir = join(tmpdir(), 'test-pkg-' + Date.now());
    await mkdir(join(testDir, 'src'), { recursive: true });
    await mkdir(join(testDir, 'test'), { recursive: true });

    const packageJson = {
      name: '@test/analyzer',
      version: '1.0.0',
      type: 'module',
      dependencies: {},
      devDependencies: {},
    };

    const srcCode = `
export function add(a, b) {
  return a + b;
}

export const PI = 3.14;
    `;

    const testCode = `
import { expect } from 'vitest';
import { add } from '../src/index.mjs';

expect(add(1, 2)).toBe(3);
expect(add(2, 3)).toBe(5);
    `;

    await writeFile(join(testDir, 'package.json'), JSON.stringify(packageJson, null, 2));
    await writeFile(join(testDir, 'src', 'index.mjs'), srcCode);
    await writeFile(join(testDir, 'test', 'index.test.mjs'), testCode);

    const analysis = await analyzePackage(testDir);

    expect(analysis.name).toBe('@test/analyzer');
    expect(analysis.linesOfCode).toBeGreaterThan(0);
    expect(analysis.exportCount).toBeGreaterThan(0);
    expect(analysis.testCoverage).toBeGreaterThanOrEqual(0);
    expect(analysis.complexity).toMatch(/low|medium|high/);

    await rm(testDir, { recursive: true, force: true });
  });

  it('findExports should find all exports', async () => {
    const testDir = join(tmpdir(), 'test-exports-' + Date.now());
    await mkdir(testDir, { recursive: true });

    const srcCode = `
export function foo() {}
export const bar = 1;
export class Baz {}
export { qux };
const qux = 2;
    `;

    await writeFile(join(testDir, 'test.mjs'), srcCode);

    const exports = await findExports(join(testDir, 'test.mjs'));

    expect(exports).toContain('foo');
    expect(exports).toContain('bar');
    expect(exports).toContain('Baz');
    expect(exports).toContain('qux');

    await rm(testDir, { recursive: true, force: true });
  });

  it('countCoverage should estimate coverage', async () => {
    // countCoverage accepts empty arrays and returns 0
    const coverage = await countCoverage([], []);

    expect(coverage).toBe(0);
  });

  it('analyzePackage should throw TypeError for non-string input', async () => {
    await expect(analyzePackage(123)).rejects.toThrow(TypeError);
  });

  it('findExports should throw TypeError for non-string input', async () => {
    await expect(findExports({})).rejects.toThrow(TypeError);
  });

  it('countCoverage should throw TypeError for non-array inputs', async () => {
    await expect(countCoverage('not-array', [])).rejects.toThrow(TypeError);
    await expect(countCoverage([], 'not-array')).rejects.toThrow(TypeError);
  });
});

describe('build-utils', () => {
  it('verifyPackage should check package integrity', async () => {
    const testDir = join(tmpdir(), 'test-verify-' + Date.now());
    await mkdir(join(testDir, 'src'), { recursive: true });

    const packageJson = {
      name: '@test/verify',
      version: '1.0.0',
      type: 'module',
      main: 'src/index.mjs',
    };

    await writeFile(join(testDir, 'package.json'), JSON.stringify(packageJson, null, 2));
    await writeFile(join(testDir, 'src', 'index.mjs'), 'export const x = 1;');

    const result = await verifyPackage(testDir);

    expect(result.valid).toBe(true);
    expect(result.errors).toEqual([]);
    expect(result.checkedFiles.length).toBeGreaterThan(0);

    await rm(testDir, { recursive: true, force: true });
  });

  it('verifyPackage should detect missing package.json', async () => {
    const testDir = join(tmpdir(), 'test-missing-' + Date.now());
    await mkdir(testDir, { recursive: true });

    const result = await verifyPackage(testDir);

    expect(result.valid).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);

    await rm(testDir, { recursive: true, force: true });
  });

  it('buildPackage should handle missing build script', async () => {
    const testDir = join(tmpdir(), 'test-build-' + Date.now());
    await mkdir(testDir, { recursive: true });

    const packageJson = {
      name: '@test/build',
      version: '1.0.0',
      scripts: {},
    };

    await writeFile(join(testDir, 'package.json'), JSON.stringify(packageJson, null, 2));

    const result = await buildPackage(testDir);

    expect(result.success).toBe(true);
    expect(result.message).toContain('No build script');

    await rm(testDir, { recursive: true, force: true });
  });

  it('listPackages should find packages in monorepo', async () => {
    const testDir = join(tmpdir(), 'test-monorepo-' + Date.now());
    await mkdir(join(testDir, 'packages', 'pkg1'), { recursive: true });
    await mkdir(join(testDir, 'packages', 'pkg2'), { recursive: true });

    const pkg1Json = { name: '@test/pkg1', version: '1.0.0', description: 'Package 1' };
    const pkg2Json = { name: '@test/pkg2', version: '2.0.0', description: 'Package 2' };

    await writeFile(
      join(testDir, 'packages', 'pkg1', 'package.json'),
      JSON.stringify(pkg1Json, null, 2)
    );
    await writeFile(
      join(testDir, 'packages', 'pkg2', 'package.json'),
      JSON.stringify(pkg2Json, null, 2)
    );

    const packages = await listPackages(testDir);

    expect(packages.length).toBe(2);
    expect(packages.some(p => p.name === '@test/pkg1')).toBe(true);
    expect(packages.some(p => p.name === '@test/pkg2')).toBe(true);

    await rm(testDir, { recursive: true, force: true });
  });

  it('buildPackage should throw TypeError for non-string input', async () => {
    await expect(buildPackage(null)).rejects.toThrow(TypeError);
  });

  it('verifyPackage should throw TypeError for non-string input', async () => {
    await expect(verifyPackage(123)).rejects.toThrow(TypeError);
  });

  it('listPackages should throw TypeError for non-string input', async () => {
    await expect(listPackages([])).rejects.toThrow(TypeError);
  });
});

describe('metrics', () => {
  it('reportMetrics should format metrics as text', () => {
    const metrics = {
      timestamp: new Date().toISOString(),
      totalPackages: 2,
      totalLinesOfCode: 1000,
      totalExports: 20,
      averageTestCoverage: 85,
      packages: [
        {
          name: '@test/pkg1',
          linesOfCode: 600,
          exportCount: 12,
          testCoverage: 90,
          complexity: 'medium',
        },
        {
          name: '@test/pkg2',
          linesOfCode: 400,
          exportCount: 8,
          testCoverage: 80,
          complexity: 'low',
        },
      ],
    };

    const report = reportMetrics(metrics);

    expect(report).toContain('PROJECT METRICS REPORT');
    expect(report).toContain('Total Packages:        2');
    expect(report).toContain('Total Lines of Code:   1,000');
    expect(report).toContain('Average Test Coverage: 85%');
    expect(report).toContain('@test/pkg1');
    expect(report).toContain('@test/pkg2');
  });

  it('reportMetrics should throw TypeError for non-object input', () => {
    expect(() => reportMetrics('not-object')).toThrow(TypeError);
    expect(() => reportMetrics(null)).toThrow(TypeError);
  });
});

describe('infrastructure', () => {
  it('createProjectConfig should create config for library template', () => {
    const config = createProjectConfig('library');

    expect(config.name).toBe('my-library');
    expect(config.type).toBe('library');
    expect(config.runtime).toBe('both');
    expect(config.testing.framework).toBe('vitest');
    expect(config.testing.coverage).toBe(90);
  });

  it('createProjectConfig should create config for monorepo template', () => {
    const config = createProjectConfig('monorepo');

    expect(config.name).toBe('my-monorepo');
    expect(config.type).toBe('monorepo');
    expect(config.testing.framework).toBe('vitest');
  });

  it('createProjectConfig should throw for invalid template', () => {
    expect(() => createProjectConfig('invalid-template')).toThrow(
      /invalid template "invalid-template"/
    );
  });

  it('setupDevEnvironment should return dev config', () => {
    const devConfig = setupDevEnvironment();

    expect(devConfig.tools.packageManager).toBe('pnpm');
    expect(devConfig.tools.nodeVersion).toBe('>=18.0.0');
    expect(devConfig.scripts.dev).toBeDefined();
    expect(devConfig.scripts.test).toBeDefined();
  });

  it('createDeploymentConfig should create production config', () => {
    const config = createDeploymentConfig('production');

    expect(config.environment).toBe('production');
    expect(config.target).toBe('production-server');
    expect(config.build.minify).toBe(true);
    expect(config.env.NODE_ENV).toBe('production');
  });

  it('createDeploymentConfig should create staging config', () => {
    const config = createDeploymentConfig('staging');

    expect(config.environment).toBe('staging');
    expect(config.build.minify).toBe(true);
    expect(config.build.sourceMaps).toBe(true);
  });

  it('createDeploymentConfig should throw for invalid environment', () => {
    expect(() => createDeploymentConfig('invalid-env')).toThrow(
      /invalid environment "invalid-env"/
    );
  });

  it('createProjectConfig should throw TypeError for non-string input', () => {
    expect(() => createProjectConfig(123)).toThrow(TypeError);
  });

  it('createDeploymentConfig should throw TypeError for non-string input', () => {
    expect(() => createDeploymentConfig(null)).toThrow(TypeError);
  });
});
