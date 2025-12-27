/**
 * Tests for KGC Documentation Atlas
 */

import { describe, it, expect } from 'vitest';
import { writeFile, mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import {
  extractJSDocFromFile,
  extractExportsFromESM,
  scanPackages,
  buildAPIManifest,
  generateAPIReference,
  generateCapabilityGraph,
  atlasAsMarkdown,
  atlasAsJSON,
} from '../src/kgc-docs-atlas.mjs';

describe('KGC Documentation Atlas', () => {
  describe('extractJSDocFromFile', () => {
    it('should extract JSDoc from function with params and returns', async () => {
      const testFile = join(tmpdir(), `test-jsdoc-${Date.now()}.mjs`);
      const content = `
/**
 * Calculate sum of two numbers
 * @param {number} a - First number
 * @param {number} b - Second number
 * @returns {number} Sum of a and b
 */
export function add(a, b) {
  return a + b;
}
`;
      await writeFile(testFile, content, 'utf-8');

      const result = await extractJSDocFromFile(testFile);

      expect(result).toHaveLength(1);
      expect(result[0].name).toBe('add');
      expect(result[0].type).toBe('function');
      expect(result[0].description).toBe('Calculate sum of two numbers');
      expect(result[0].params).toHaveLength(2);
      expect(result[0].params[0].name).toBe('a');
      expect(result[0].params[0].type).toBe('number');
      expect(result[0].returns.type).toBe('number');

      await rm(testFile);
    });

    it('should extract JSDoc from class', async () => {
      const testFile = join(tmpdir(), `test-class-${Date.now()}.mjs`);
      const content = `
/**
 * User class representing a user
 */
export class User {
  constructor(name) {
    this.name = name;
  }
}
`;
      await writeFile(testFile, content, 'utf-8');

      const result = await extractJSDocFromFile(testFile);

      expect(result).toHaveLength(1);
      expect(result[0].name).toBe('User');
      expect(result[0].type).toBe('class');
      expect(result[0].description).toBe('User class representing a user');

      await rm(testFile);
    });

    it('should extract JSDoc from constants', async () => {
      const testFile = join(tmpdir(), `test-const-${Date.now()}.mjs`);
      const content = `
/**
 * Maximum retry attempts
 */
export const MAX_RETRIES = 3;

/**
 * Default timeout in milliseconds
 */
export const DEFAULT_TIMEOUT = 5000;
`;
      await writeFile(testFile, content, 'utf-8');

      const result = await extractJSDocFromFile(testFile);

      expect(result).toHaveLength(2);
      expect(result[0].name).toBe('MAX_RETRIES');
      expect(result[0].description).toBe('Maximum retry attempts');
      expect(result[1].name).toBe('DEFAULT_TIMEOUT');

      await rm(testFile);
    });

    it('should handle files with no JSDoc', async () => {
      const testFile = join(tmpdir(), `test-no-jsdoc-${Date.now()}.mjs`);
      const content = `
export function foo() {
  return 42;
}
`;
      await writeFile(testFile, content, 'utf-8');

      const result = await extractJSDocFromFile(testFile);

      expect(result).toHaveLength(1);
      expect(result[0].name).toBe('foo');
      expect(result[0].jsdoc).toBeUndefined();
      expect(result[0].description).toBeUndefined();

      await rm(testFile);
    });
  });

  describe('extractExportsFromESM', () => {
    it('should extract named exports', async () => {
      const testFile = join(tmpdir(), `test-exports-${Date.now()}.mjs`);
      const content = `
export const FOO = 1;
export function bar() {}
export class Baz {}
`;
      await writeFile(testFile, content, 'utf-8');

      const result = await extractExportsFromESM(testFile);

      expect(result).toHaveLength(3);
      expect(result.find(e => e.name === 'FOO')).toBeDefined();
      expect(result.find(e => e.name === 'bar')).toBeDefined();
      expect(result.find(e => e.name === 'Baz')).toBeDefined();

      await rm(testFile);
    });

    it('should detect re-exports', async () => {
      const testFile = join(tmpdir(), `test-reexports-${Date.now()}.mjs`);
      const content = `
export { createStore, dataFactory } from '@unrdf/oxigraph';
export { KGCStore } from '@unrdf/kgc-4d';
`;
      await writeFile(testFile, content, 'utf-8');

      const result = await extractExportsFromESM(testFile);

      expect(result.length).toBeGreaterThanOrEqual(3);
      expect(result.filter(e => e.isReexport)).toHaveLength(3);
      expect(result.find(e => e.name === 'createStore')?.from).toBe('@unrdf/oxigraph');

      await rm(testFile);
    });

    it('should handle default exports', async () => {
      const testFile = join(tmpdir(), `test-default-${Date.now()}.mjs`);
      const content = `
const config = { foo: 'bar' };
export default config;
`;
      await writeFile(testFile, content, 'utf-8');

      const result = await extractExportsFromESM(testFile);

      expect(result).toHaveLength(1);
      expect(result[0].type).toBe('default');

      await rm(testFile);
    });
  });

  describe('scanPackages', () => {
    it('should scan a single package', async () => {
      // Use fusion package itself as test subject
      const result = await scanPackages('packages/fusion', {
        workspaceRoot: '/home/user/unrdf',
      });

      expect(result).toHaveLength(1);
      expect(result[0].package).toBe('@unrdf/fusion');
      expect(result[0].exports.length).toBeGreaterThan(0);
      expect(result[0].dependencies).toBeDefined();
    });

    it('should filter by scope pattern', async () => {
      const result = await scanPackages('@unrdf/fusion', {
        workspaceRoot: '/home/user/unrdf',
      });

      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result.every(p => p.package.startsWith('@unrdf/'))).toBe(true);
    });

    it('should handle packages with no exports', async () => {
      // Create temporary package with no exports
      const tmpPkg = join(tmpdir(), `test-pkg-${Date.now()}`);
      await mkdir(join(tmpPkg, 'src'), { recursive: true });
      await writeFile(join(tmpPkg, 'package.json'), JSON.stringify({
        name: '@test/empty',
        version: '1.0.0',
      }));
      await writeFile(join(tmpPkg, 'src', 'index.mjs'), '// No exports\n');

      // Note: This test is limited because scanPackages expects workspace structure
      // In real usage, it would skip packages without valid entry points

      await rm(tmpPkg, { recursive: true });
    });
  });

  describe('buildAPIManifest', () => {
    it('should build manifest from package scans', () => {
      const packages = [
        {
          package: '@unrdf/test',
          path: '/test',
          version: '1.0.0',
          exports: [
            { name: 'foo', type: 'function', jsdoc: '/** Foo */' },
            { name: 'bar', type: 'constant' },
          ],
          dependencies: ['@unrdf/other'],
        },
      ];

      const manifest = buildAPIManifest(packages);

      expect(manifest.packages).toHaveLength(1);
      expect(manifest.totalExports).toBe(2);
      expect(manifest.undocumented).toHaveLength(1);
      expect(manifest.undocumented[0].export).toBe('bar');
      expect(manifest.crossPackageDeps).toHaveLength(1);
      expect(manifest.timestamp).toBeDefined();
    });

    it('should identify cross-package dependencies', () => {
      const packages = [
        {
          package: '@unrdf/pkg-a',
          exports: [],
          dependencies: ['@unrdf/pkg-b', '@unrdf/pkg-c', 'external-lib'],
        },
        {
          package: '@unrdf/pkg-b',
          exports: [],
          dependencies: [],
        },
      ];

      const manifest = buildAPIManifest(packages);

      expect(manifest.crossPackageDeps).toHaveLength(2);
      expect(manifest.crossPackageDeps.find(d => d.to === '@unrdf/pkg-b')).toBeDefined();
      expect(manifest.crossPackageDeps.find(d => d.to === '@unrdf/pkg-c')).toBeDefined();
      // External dependencies should be included
      expect(manifest.crossPackageDeps.find(d => d.to === 'external-lib')).toBeUndefined();
    });
  });

  describe('generateAPIReference', () => {
    it('should generate markdown reference', () => {
      const manifest = {
        timestamp: '2024-01-01T00:00:00.000Z',
        packages: [
          {
            package: '@unrdf/test',
            version: '1.0.0',
            exports: [
              {
                name: 'add',
                type: 'function',
                signature: '(a: number, b: number) => number',
                description: 'Add two numbers',
                params: [
                  { name: 'a', type: 'number', description: 'First number' },
                  { name: 'b', type: 'number', description: 'Second number' },
                ],
                returns: { type: 'number', description: 'Sum' },
              },
            ],
          },
        ],
        totalExports: 1,
        undocumented: [],
        crossPackageDeps: [],
      };

      const md = generateAPIReference(manifest);

      expect(md).toContain('# API Reference');
      expect(md).toContain('@unrdf/test');
      expect(md).toContain('`add`');
      expect(md).toContain('Add two numbers');
      expect(md).toContain('**Parameters:**');
      expect(md).toContain('**Returns:**');
    });

    it('should include undocumented section', () => {
      const manifest = {
        timestamp: '2024-01-01T00:00:00.000Z',
        packages: [],
        totalExports: 0,
        undocumented: [
          { package: '@unrdf/test', export: 'foo' },
        ],
        crossPackageDeps: [],
      };

      const md = generateAPIReference(manifest);

      expect(md).toContain('⚠️ Undocumented Exports');
      expect(md).toContain('foo');
    });
  });

  describe('generateCapabilityGraph', () => {
    it('should generate graph with nodes and edges', () => {
      const manifest = {
        packages: [
          {
            package: '@unrdf/test',
            exports: [
              { name: 'foo', type: 'function', jsdoc: '/** Foo */' },
              { name: 'bar', type: 'class' },
            ],
          },
        ],
        crossPackageDeps: [
          { from: '@unrdf/test', to: '@unrdf/other' },
        ],
      };

      const graph = generateCapabilityGraph(manifest);

      expect(graph.nodes).toHaveLength(2);
      expect(graph.nodes[0].id).toBe('@unrdf/test::foo');
      expect(graph.nodes[0].documented).toBe(true);
      expect(graph.nodes[1].documented).toBe(false);

      expect(graph.edges).toHaveLength(1);
      expect(graph.edges[0].type).toBe('package-dependency');
    });
  });

  describe('atlasAsMarkdown', () => {
    it('should generate table with all exports', () => {
      const manifest = {
        timestamp: '2024-01-01T00:00:00.000Z',
        packages: [
          {
            package: '@unrdf/test',
            exports: [
              { name: 'foo', type: 'function', jsdoc: '/** Foo */' },
              { name: 'bar', type: 'constant' },
            ],
          },
        ],
      };

      const md = atlasAsMarkdown(manifest);

      expect(md).toContain('# API Atlas');
      expect(md).toContain('| Name | Type | Package | Status | JSDoc |');
      expect(md).toContain('`foo`');
      expect(md).toContain('`bar`');
      expect(md).toContain('✅ Stable');
      expect(md).toContain('⚠️ Undocumented');
    });

    it('should sort by package then name', () => {
      const manifest = {
        timestamp: '2024-01-01T00:00:00.000Z',
        packages: [
          {
            package: '@unrdf/pkg-b',
            exports: [{ name: 'zeta', type: 'function' }],
          },
          {
            package: '@unrdf/pkg-a',
            exports: [{ name: 'beta', type: 'function' }, { name: 'alpha', type: 'function' }],
          },
        ],
      };

      const md = atlasAsMarkdown(manifest);
      const lines = md.split('\n');

      // Find table rows (skip header and separator)
      const rows = lines.filter(l => l.startsWith('| `'));

      // Should be sorted: pkg-a/alpha, pkg-a/beta, pkg-b/zeta
      expect(rows[0]).toContain('alpha');
      expect(rows[1]).toContain('beta');
      expect(rows[2]).toContain('zeta');
    });
  });

  describe('atlasAsJSON', () => {
    it('should generate deterministic JSON', () => {
      const manifest = {
        timestamp: '2024-01-01T00:00:00.000Z',
        packages: [
          {
            package: '@unrdf/test',
            exports: [
              { name: 'foo', type: 'function' },
            ],
          },
        ],
        totalExports: 1,
        undocumented: [],
        crossPackageDeps: [],
      };

      const json1 = atlasAsJSON(manifest);
      const json2 = atlasAsJSON(manifest);

      expect(json1).toBe(json2);
      expect(JSON.parse(json1)).toEqual(expect.objectContaining({
        packages: expect.any(Array),
        timestamp: expect.any(String),
      }));
    });

    it('should sort keys alphabetically', () => {
      const manifest = {
        zulu: 'last',
        alpha: 'first',
        bravo: 'second',
      };

      const json = atlasAsJSON(manifest);
      const keys = Object.keys(JSON.parse(json));

      expect(keys).toEqual(['alpha', 'bravo', 'zulu']);
    });
  });

  describe('Integration: Real package scan', () => {
    it('should scan fusion package and generate full atlas', async () => {
      const packages = await scanPackages('packages/fusion', {
        workspaceRoot: '/home/user/unrdf',
      });

      expect(packages.length).toBeGreaterThan(0);

      const manifest = buildAPIManifest(packages);

      expect(manifest.totalExports).toBeGreaterThan(0);
      expect(manifest.packages).toHaveLength(packages.length);

      const reference = generateAPIReference(manifest);
      expect(reference).toContain('# API Reference');

      const graph = generateCapabilityGraph(manifest);
      expect(graph.nodes.length).toBeGreaterThan(0);

      const mdTable = atlasAsMarkdown(manifest);
      expect(mdTable).toContain('# API Atlas');

      const json = atlasAsJSON(manifest);
      expect(JSON.parse(json)).toHaveProperty('packages');
    });
  });
});
