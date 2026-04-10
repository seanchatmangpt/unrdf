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
      } catch (_e) {
        // ignore cleanup failures
      }
    }
  });

  it('parseFrontmatter() splits YAML + body and validate() accepts valid frontmatter', () => {
    const parser = new FrontmatterParser();

    // Parse
    const content = `---
to: output/test.html
inject: true
before: "<!-- INSERT HERE -->"
---
<template body>`;

    const { frontmatter, body } = parser.parse(content);
    expect(frontmatter.to).toBe('output/test.html');
    expect(frontmatter.inject).toBe(true);
    expect(frontmatter.before).toBe('<!-- INSERT HERE -->');
    expect(body.trim()).toBe('<template body>');

    // Validate passes with required field
    const result = parser.validate(frontmatter);
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it('validate() rejects missing required field, unknown keys, and type mismatches', () => {
    const parser = new FrontmatterParser({ allowEmpty: false });

    // Missing required field "to:"
    const missingTo = parser.validate({ inject: true, before: 'anchor' });
    expect(missingTo.valid).toBe(false);
    expect(missingTo.errors).toEqual(
      expect.arrayContaining([expect.stringContaining('Required field missing: "to"')])
    );

    // Unknown key
    const unknown = parser.validate({ to: 'out.html', bogus_key: true });
    expect(unknown.valid).toBe(false);
    expect(unknown.errors).toEqual(
      expect.arrayContaining([expect.stringContaining('Unknown frontmatter key: "bogus_key"')])
    );

    // Type mismatch
    const badType = parser.validate({ to: 'out.html', inject: 'yes' });
    expect(badType.valid).toBe(false);
    expect(badType.errors).toEqual(
      expect.arrayContaining([expect.stringContaining('"inject" must be boolean')])
    );

    // Mutually exclusive modes
    const exclusive = parser.validate({ to: 'out.html', inject: true, append: true });
    expect(exclusive.valid).toBe(false);
    expect(exclusive.errors).toEqual(
      expect.arrayContaining([expect.stringContaining('mutually exclusive')])
    );

    // Dependency rule: before requires inject
    const dep = parser.validate({ to: 'out.html', before: 'marker' });
    expect(dep.valid).toBe(false);
    expect(dep.errors).toEqual(
      expect.arrayContaining([expect.stringContaining('"before" requires "inject: true"')])
    );
  });

  it('getOperationMode() and shouldSkip() handle all directive forms', () => {
    // getOperationMode dispatches correctly
    expect(getOperationMode({}).mode).toBe('write');
    expect(getOperationMode({ append: true }).mode).toBe('append');
    expect(getOperationMode({ prepend: true }).mode).toBe('prepend');
    expect(getOperationMode({ lineAt: 5 }).mode).toBe('lineAt');
    expect(getOperationMode({ inject: true }).mode).toBe('inject');
    expect(getOperationMode({ inject: true, before: 'm' }).mode).toBe('before');
    expect(getOperationMode({ inject: true, before: 'm' }).anchor).toBe('m');
    expect(getOperationMode({ inject: true, after: 'm' }).mode).toBe('after');
    expect(getOperationMode({ lineAt: 42 }).line).toBe(42);

    // shouldSkip with bare variable, negation, equality, inequality
    expect(shouldSkip({ skipIf: 'exists' }, { exists: true })).toBe(false);
    expect(shouldSkip({ skipIf: 'exists' }, { exists: false })).toBe(true);
    expect(shouldSkip({ skipIf: '!exists' }, { exists: true })).toBe(false);
    expect(shouldSkip({ skipIf: '!exists' }, { exists: false })).toBe(true);
    expect(shouldSkip({ skipIf: 'role==admin' }, { role: 'admin' })).toBe(true);
    expect(shouldSkip({ skipIf: 'role==admin' }, { role: 'user' })).toBe(false);
    expect(shouldSkip({ skipIf: 'role!=admin' }, { role: 'admin' })).toBe(false);
    expect(shouldSkip({ skipIf: 'role!=admin' }, { role: 'user' })).toBe(true);

    // Hygen-style skip_if alias
    expect(shouldSkip({ skip_if: 'isDraft' }, { isDraft: true })).toBe(false);
    expect(shouldSkip({ skip_if: 'isDraft' }, { isDraft: false })).toBe(true);
  });
});
