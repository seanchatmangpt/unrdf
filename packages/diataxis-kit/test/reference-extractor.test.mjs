/**
 * @fileoverview Tests for reference-extractor.mjs
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { extractReference } from '../src/reference-extractor.mjs';

describe('extractReference', () => {
  it('should extract from exports field (string)', () => {
    const pkg = {
      name: 'test-pkg',
      exports: './index.js'
    };
    const evidence = { readme: '' };

    const result = extractReference(pkg, evidence);

    assert.equal(result.id, 'reference');
    assert.equal(result.title, 'test-pkg API Reference');
    assert.equal(result.items.length, 1);
    assert.equal(result.items[0].name, '.');
    assert.equal(result.items[0].type, 'export');
    assert.equal(result.items[0].description, './index.js');
    assert.equal(result.confidenceScore, 1.0);
    assert.deepEqual(result.source, ['exports']);
  });

  it('should extract from exports field (object)', () => {
    const pkg = {
      name: 'test-pkg',
      exports: {
        '.': './index.js',
        './utils': './utils.js'
      }
    };
    const evidence = { readme: '' };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items.length, 2);
    assert.equal(result.items[0].name, '.');
    assert.equal(result.items[1].name, './utils');
    assert.equal(result.confidenceScore, 1.0);
  });

  it('should extract from bin field (string)', () => {
    const pkg = {
      name: 'cli-tool',
      bin: './cli.js'
    };
    const evidence = { readme: '' };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items.length, 1);
    assert.equal(result.items[0].name, 'cli-tool');
    assert.equal(result.items[0].type, 'bin');
    assert.ok(result.items[0].description.includes('CLI entry point'));
    assert.equal(result.confidenceScore, 0.8);
    assert.deepEqual(result.source, ['bin']);
  });

  it('should extract from bin field (object)', () => {
    const pkg = {
      name: 'multi-cli',
      bin: {
        'cmd1': './cmd1.js',
        'cmd2': './cmd2.js'
      }
    };
    const evidence = { readme: '' };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items.length, 2);
    assert.equal(result.items[0].name, 'cmd1');
    assert.equal(result.items[1].name, 'cmd2');
    assert.equal(result.confidenceScore, 0.8);
  });

  it('should extract from README API section', () => {
    const pkg = { name: 'readme-pkg' };
    const evidence = {
      readme: `
# Package

## API

- foo: Does something cool
- bar: Does another thing
      `
    };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items.length, 2);
    assert.equal(result.items[0].name, 'bar');
    assert.equal(result.items[0].type, 'api');
    assert.equal(result.items[0].description, 'Does another thing');
    assert.equal(result.items[1].name, 'foo');
    assert.equal(result.confidenceScore, 0.6);
    assert.deepEqual(result.source, ['readme']);
  });

  it('should combine multiple sources', () => {
    const pkg = {
      name: 'combo-pkg',
      exports: { '.': './index.js' },
      bin: './cli.js'
    };
    const evidence = {
      readme: '## API\n\n- method: A method'
    };

    const result = extractReference(pkg, evidence);

    assert.ok(result.items.length >= 3);
    assert.equal(result.confidenceScore, 1.0); // exports = highest
    assert.ok(result.source.includes('exports'));
    assert.ok(result.source.includes('bin'));
    assert.ok(result.source.includes('readme'));
  });

  it('should return unknown fallback when no data found', () => {
    const pkg = { name: 'empty-pkg' };
    const evidence = { readme: '' };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items.length, 1);
    assert.equal(result.items[0].name, 'unknown');
    assert.equal(result.items[0].type, 'unknown');
    assert.equal(result.items[0].description, 'API reference not found in documentation');
    assert.equal(result.confidenceScore, 0.1);
    assert.deepEqual(result.source, ['inferred']);
  });

  it('should sort items by name deterministically', () => {
    const pkg = {
      name: 'sort-test',
      exports: {
        './zebra': './z.js',
        './alpha': './a.js',
        './beta': './b.js'
      }
    };
    const evidence = { readme: '' };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items[0].name, './alpha');
    assert.equal(result.items[1].name, './beta');
    assert.equal(result.items[2].name, './zebra');
  });

  it('should handle conditional exports', () => {
    const pkg = {
      name: 'conditional-pkg',
      exports: {
        '.': {
          import: './esm/index.js',
          require: './cjs/index.js'
        }
      }
    };
    const evidence = { readme: '' };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items.length, 1);
    assert.equal(result.items[0].description, './esm/index.js'); // import first
  });

  it('should handle README with backticks in method names', () => {
    const pkg = { name: 'backtick-pkg' };
    const evidence = {
      readme: '## Methods\n\n- `createStore()`: Creates a new store\n- `query()`: Runs a query'
    };

    const result = extractReference(pkg, evidence);

    assert.equal(result.items.length, 2);
    assert.equal(result.items[0].name, 'createStore()');
    assert.equal(result.items[1].name, 'query()');
  });
});
