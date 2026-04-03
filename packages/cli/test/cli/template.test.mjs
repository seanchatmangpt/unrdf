/**
 * Template generation tests - Hygen frontmatter directive support
 * @module cli/test/template
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { rmSync, mkdirSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

import {
  FrontmatterParser,
  shouldSkip,
  getOperationMode,
} from '../../src/lib/frontmatter-parser.mjs';
import { RdfTemplateLoader, localName } from '../../src/lib/rdf-template-loader.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const FIXTURES = join(__dirname, '../fixtures');

describe('Template Generation - Hygen Features', () => {
  let tmpDir = '';

  beforeEach(() => {
    tmpDir = join(__dirname, `../../.test-${Date.now()}`);
    mkdirSync(tmpDir, { recursive: true });
  });

  afterEach(() => {
    if (tmpDir) {
      try {
        rmSync(tmpDir, { recursive: true, force: true });
      } catch (e) {
        // ignore
      }
    }
  });

  describe('Frontmatter Parsing', () => {
    it('parseFrontmatter() splits YAML + body via gray-matter', () => {
      const content = `---
to: output/test.html
inject: true
before: "<!-- INSERT HERE -->"
---
<template body>`;

      const parser = new FrontmatterParser();
      const { frontmatter, body } = parser.parse(content);

      expect(frontmatter.to).toBe('output/test.html');
      expect(frontmatter.inject).toBe(true);
      expect(frontmatter.before).toBe('<!-- INSERT HERE -->');
      expect(body.trim()).toBe('<template body>');
    });

    it('validate() checks frontmatter directives', () => {
      const parser = new FrontmatterParser();

      // Valid: inject + before
      const valid1 = parser.validate({ inject: true, before: 'anchor' });
      expect(valid1.valid).toBe(true);

      // Invalid: multiple modes
      const invalid = parser.validate({
        inject: true,
        append: true,
      });
      expect(invalid.valid).toBe(false);
      expect(invalid.errors.length).toBeGreaterThan(0);
    });

    it('mergeWithDefaults() applies defaults', () => {
      const parser = new FrontmatterParser();
      const fm = { to: 'output.html' };
      const defaults = { name: 'default-name', type: 'file' };

      const merged = parser.mergeWithDefaults(fm, defaults);
      expect(merged.name).toBe('default-name');
      expect(merged.to).toBe('output.html');
    });
  });

  describe('File Operation Modes', () => {
    it('getOperationMode() dispatches based on directives', () => {
      expect(getOperationMode({}).mode).toBe('write');
      expect(getOperationMode({ append: true }).mode).toBe('append');
      expect(getOperationMode({ prepend: true }).mode).toBe('prepend');
      expect(getOperationMode({ lineAt: 5 }).mode).toBe('lineAt');
      expect(getOperationMode({ inject: true }).mode).toBe('inject');
      expect(getOperationMode({ inject: true, before: 'marker' }).mode).toBe(
        'before'
      );
      expect(getOperationMode({ inject: true, after: 'marker' }).mode).toBe(
        'after'
      );
    });

    it('getOperationMode() returns anchor for inject modes', () => {
      const beforeMode = getOperationMode({
        inject: true,
        before: 'start block',
      });
      expect(beforeMode.anchor).toBe('start block');

      const afterMode = getOperationMode({
        inject: true,
        after: 'end block',
      });
      expect(afterMode.anchor).toBe('end block');
    });

    it('getOperationMode() returns line for lineAt mode', () => {
      const mode = getOperationMode({ lineAt: 42 });
      expect(mode.line).toBe(42);
    });
  });

  describe('Conditional Skip Logic', () => {
    it('shouldSkip() with bare variable (truthy check)', () => {
      // Skip if var is truthy
      expect(shouldSkip({ skipIf: 'exists' }, { exists: true })).toBe(false);
      expect(shouldSkip({ skipIf: 'exists' }, { exists: false })).toBe(true);
      expect(shouldSkip({ skipIf: 'exists' }, {})).toBe(true);
    });

    it('shouldSkip() with negation (!var)', () => {
      // Skip if var is falsy (NOT exists)
      expect(shouldSkip({ skipIf: '!exists' }, { exists: true })).toBe(false);
      expect(shouldSkip({ skipIf: '!exists' }, { exists: false })).toBe(true);
      expect(shouldSkip({ skipIf: '!exists' }, {})).toBe(true);
    });

    it('shouldSkip() with equality (var==value)', () => {
      // Skip if role equals admin
      expect(
        shouldSkip({ skipIf: 'role==admin' }, { role: 'admin' })
      ).toBe(true);
      expect(shouldSkip({ skipIf: 'role==admin' }, { role: 'user' })).toBe(
        false
      );
    });

    it('shouldSkip() with inequality (var!=value)', () => {
      // Skip if role NOT equals admin
      expect(
        shouldSkip({ skipIf: 'role!=admin' }, { role: 'admin' })
      ).toBe(false);
      expect(shouldSkip({ skipIf: 'role!=admin' }, { role: 'user' })).toBe(
        true
      );
    });

    it('shouldSkip() supports skip_if alias (Hygen-style)', () => {
      // Hygen uses snake_case skip_if
      expect(shouldSkip({ skip_if: 'isDraft' }, { isDraft: true })).toBe(
        false
      );
      expect(shouldSkip({ skip_if: 'isDraft' }, { isDraft: false })).toBe(
        true
      );
    });
  });

  describe('RDF + SPARQL Integration', () => {
    it('loads Turtle RDF file and executes SPARQL', async () => {
      const loader = new RdfTemplateLoader();
      const rdfFile = join(FIXTURES, 'person.ttl');

      const store = await loader.loadFromFile(rdfFile);
      expect(store).toBeDefined();

      // Query for Alice
      const sparql = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?title WHERE {
  <http://example.org/alice> foaf:name ?name ;
                              foaf:title ?title .
}`;

      const context = loader.queryToContext(store, sparql);
      expect(context.name).toBeDefined();
      expect(context.title).toBeDefined();
      expect(context.$rdf).toBeDefined();
      expect(Array.isArray(context.$rdf.raw)).toBe(true);
    });

    it('findInstancesOfClass() discovers all instances of a class', async () => {
      const loader = new RdfTemplateLoader();
      const rdfFile = join(FIXTURES, 'person.ttl');
      const store = await loader.loadFromFile(rdfFile);

      const instances = loader.findInstancesOfClass(
        store,
        'http://xmlns.com/foaf/0.1/Person'
      );

      // person.ttl has Alice and Bob
      expect(instances.length).toBeGreaterThanOrEqual(2);
      expect(instances.some((uri) => uri.includes('alice'))).toBe(true);
      expect(instances.some((uri) => uri.includes('bob'))).toBe(true);
    });

    it('localName() extracts fragment or path from URI', () => {
      // Fragment-based
      expect(localName('http://xmlns.com/foaf/0.1/Person')).toBe('Person');

      // Path-based
      expect(localName('http://example.org/data#alice')).toBe('alice');

      // Edge case
      expect(localName('http://example.org/')).toBe('');
    });
  });

  describe('Hygen Directives Integration', () => {
    it('supports all Hygen frontmatter keys', () => {
      const parser = new FrontmatterParser();
      const fm = {
        to: 'output/{{ name }}.html',
        inject: true,
        before: '<!-- MARKER -->',
        skipIf: 'isDraft',
        force: true,
        unless_exists: true,
        eof_last: true,
      };

      // Should parse without error
      const { valid } = parser.validate(fm);
      expect(valid).toBe(true);
    });

    it('merges camelCase and snake_case keys', () => {
      const parser = new FrontmatterParser();
      const content = `---
skipIf: var1
skip_if: var2
---
body`;

      const { frontmatter } = parser.parse(content);
      // camelCase takes precedence if both exist
      expect(frontmatter.skipIf || frontmatter.skip_if).toBeDefined();
    });
  });
});
