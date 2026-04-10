/**
 * @file Template Renderer Tests
 * @module cli/commands/sync/template-renderer.test
 * @description Tests for RDF-aware template rendering with Nunjucks
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, mkdir, rm, readFile } from 'fs/promises';
import { join } from 'path';
import { existsSync } from 'fs';
import { tmpdir } from 'os';
import {
  renderTemplate,
  createNunjucksEnvironment,
  renderWithOptions,
  DEFAULT_PREFIXES,
  preprocessFrontmatter,
} from '../../src/cli/commands/sync/template-renderer.mjs';
import { FrontmatterParser, getOperationMode, shouldSkip, FRONTMATTER_SCHEMA } from '../../src/lib/frontmatter-parser.mjs';

const SIMPLE_SPARQL_RESULTS = [
  { class: 'User', property: 'name', type: 'string' },
  { class: 'User', property: 'email', type: 'string' },
  { class: 'Post', property: 'title', type: 'string' },
  { class: 'Post', property: 'content', type: 'text' },
];

describe('Template Renderer', () => {
  let testDir, templatesDir, outputDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), `template-renderer-test-${Date.now()}`);
    templatesDir = join(testDir, 'templates');
    outputDir = join(testDir, 'output');
    await mkdir(templatesDir, { recursive: true });
    await mkdir(outputDir, { recursive: true });
  });

  afterEach(async () => {
    if (existsSync(testDir)) await rm(testDir, { recursive: true, force: true });
  });

  it('should render template with SPARQL results, context variables, frontmatter merge, and metadata', async () => {
    // SPARQL results rendering
    const sparqlPath = join(templatesDir, 'sparql.njk');
    await writeFile(sparqlPath, `---
to: output/api.ts
description: Generated API file
---
// Generated API
{% for result in sparql_results %}
// Class: {{ result.class }}
{% endfor %}`);

    const sparqlResult = await renderTemplate(sparqlPath, SIMPLE_SPARQL_RESULTS);
    expect(sparqlResult.content).toContain('// Class: User');
    expect(sparqlResult.content).toContain('// Class: Post');
    expect(sparqlResult.outputPath).toBe('output/api.ts');

    // Context variables
    const ctxPath = join(templatesDir, 'ctx.njk');
    await writeFile(ctxPath, `---
to: "{{ projectName }}/api.ts"
---
Project: {{ projectName }}`);
    const ctxResult = await renderTemplate(ctxPath, [], { projectName: 'MyApp' });
    expect(ctxResult.content).toContain('Project: MyApp');
    expect(ctxResult.outputPath).toBe('MyApp/api.ts');

    // Frontmatter variable merge
    const varPath = join(templatesDir, 'vars.njk');
    await writeFile(varPath, `---
to: output/result.txt
variables:
  defaultVersion: 0.1.0
---
Version: {{ version | default(defaultVersion) }}`);
    const varResult = await renderTemplate(varPath, [], { version: '2.0.0' });
    expect(varResult.content).toContain('Version: 2.0.0');

    // Metadata in result
    const metaPath = join(templatesDir, 'meta.njk');
    await writeFile(metaPath, `---
to: output/file.ts
mode: append
---
Content`);
    const metaResult = await renderTemplate(metaPath, []);
    expect(metaResult.mode).toBe('append');
    expect(metaResult.frontmatter.mode).toBe('append');
  });

  it('should handle missing template, Nunjucks filters, and filter edge cases', async () => {
    // Missing template
    await expect(renderTemplate(join(templatesDir, 'nonexistent.njk'), [])).rejects.toThrow('Template not found');

    // Nunjucks filters
    const env = createNunjucksEnvironment();
    expect(env.renderString('{{ "http://example.org/User" | localName }}', {})).toBe('User');
    expect(env.renderString('{{ "http://example.org/User" | namespace }}', {})).toBe('http://example.org/');
    const sorted = env.renderString('{{ (items | sortBy("property")) | length }}', { items: SIMPLE_SPARQL_RESULTS });
    expect(sorted).toBe('4');
    expect(env.renderString('{{ "my-class-name" | camelCase }}', {})).toBe('myClassName');
    expect(env.renderString('{{ "my-class-name" | pascalCase }}', {})).toBe('MyClassName');
    expect(env.renderString('{{ "myClassName" | snakeCase }}', {})).toBe('my_class_name');
    expect(env.renderString('{{ "string" | zodType }}', {})).toBe('z.string()');

    // Filter edge cases: null/undefined
    expect(env.renderString('{{ val | localName }}', { val: null })).toBe('');
    expect(env.renderString('{{ val | camelCase }}', { val: undefined })).toBe('');
    expect(env.renderString('{% set grouped = arr | groupBy("key") %}{{ grouped | keys | length }}', { arr: [] })).toBe('0');
  });

  it('should handle renderWithOptions dry-run, write, append, skip_existing, and nested dirs', async () => {
    // Dry-run
    const dryPath = join(templatesDir, 'dry.njk');
    await writeFile(dryPath, '---\nto: output.ts\n---\nDry content');
    const dryResult = await renderWithOptions(dryPath, [], { dryRun: true });
    expect(dryResult.status).toBe('dry-run');

    // Write file
    const writePath = join(templatesDir, 'write.njk');
    await writeFile(writePath, '---\nto: written.ts\n---\nWritten content');
    const writeResult = await renderWithOptions(writePath, [], { dryRun: false, outputDir });
    expect(writeResult.status).toBe('success');
    expect(existsSync(join(outputDir, 'written.ts'))).toBe(true);

    // Skip existing
    const skipPath = join(templatesDir, 'skip.njk');
    await writeFile(skipPath, '---\nto: existing.ts\nmode: skip_existing\n---\nNew');
    await writeFile(join(outputDir, 'existing.ts'), 'Original', 'utf-8');
    const skipResult = await renderWithOptions(skipPath, [], { dryRun: false, outputDir });
    expect(skipResult.status).toBe('skipped');
    expect((await readFile(join(outputDir, 'existing.ts'), 'utf-8'))).toBe('Original');

    // Append
    const appendPath = join(templatesDir, 'append.njk');
    await writeFile(appendPath, '---\nto: append.ts\nmode: append\n---\nLine 2');
    await writeFile(join(outputDir, 'append.ts'), 'Line 1', 'utf-8');
    await renderWithOptions(appendPath, [], { dryRun: false, outputDir });
    const appendContent = await readFile(join(outputDir, 'append.ts'), 'utf-8');
    expect(appendContent).toContain('Line 1');
    expect(appendContent).toContain('Line 2');
  });

  it('should support Hygen parity: inject/before, inject/after, prepend, lineAt, skipIf, and frontmatter validation', async () => {
    // inject: true + before: "anchor" — content inserted BEFORE anchor
    const beforePath = join(templatesDir, 'before.njk');
    await writeFile(beforePath, '---\nto: before.ts\ninject: true\nbefore: "// MARKER"\n---\nINSERTED');
    await writeFile(join(outputDir, 'before.ts'), 'top\n// MARKER\nbottom', 'utf-8');
    await renderWithOptions(beforePath, [], { dryRun: false, outputDir });
    const beforeContent = await readFile(join(outputDir, 'before.ts'), 'utf-8');
    expect(beforeContent.indexOf('INSERTED')).toBeLessThan(beforeContent.indexOf('// MARKER'));

    // inject: true + after: "anchor" — content inserted AFTER anchor
    const afterPath = join(templatesDir, 'after.njk');
    await writeFile(afterPath, '---\nto: after.ts\ninject: true\nafter: "// ANCHOR"\n---\nINSERTED');
    await writeFile(join(outputDir, 'after.ts'), 'top\n// ANCHOR\nbottom', 'utf-8');
    await renderWithOptions(afterPath, [], { dryRun: false, outputDir });
    const afterContent = await readFile(join(outputDir, 'after.ts'), 'utf-8');
    const anchorIdx = afterContent.indexOf('// ANCHOR');
    expect(afterContent.indexOf('INSERTED')).toBeGreaterThan(anchorIdx);

    // prepend: true — content added to beginning
    const prependPath = join(templatesDir, 'prepend.njk');
    await writeFile(prependPath, '---\nto: prepend.ts\nprepend: true\n---\nPREPENDED');
    await writeFile(join(outputDir, 'prepend.ts'), 'ORIGINAL', 'utf-8');
    await renderWithOptions(prependPath, [], { dryRun: false, outputDir });
    const prependContent = await readFile(join(outputDir, 'prepend.ts'), 'utf-8');
    expect(prependContent.indexOf('PREPENDED')).toBeLessThan(prependContent.indexOf('ORIGINAL'));

    // lineAt: N — content inserted at specific line
    const lineAtPath = join(templatesDir, 'lineat.njk');
    await writeFile(lineAtPath, '---\nto: lineat.ts\nlineAt: 1\n---\nINSERTED_AT_1');
    await writeFile(join(outputDir, 'lineat.ts'), 'Line0\nLine1\nLine2', 'utf-8');
    await renderWithOptions(lineAtPath, [], { dryRun: false, outputDir });
    const lineAtContent = await readFile(join(outputDir, 'lineat.ts'), 'utf-8');
    const lines = lineAtContent.split('\n');
    expect(lines[1]).toContain('INSERTED_AT_1');
    expect(lines[0]).toBe('Line0');
    expect(lines[2]).toBe('Line1');

    // skipIf: "var==value" — skip when condition matches
    const skipIfPath = join(templatesDir, 'skipif.njk');
    await writeFile(skipIfPath, '---\nto: skipif.ts\nskipIf: "env==production"\n---\nSHOULD SKIP');
    const skipIfResult = await renderWithOptions(skipIfPath, [], {
      dryRun: false,
      outputDir,
      context: { env: 'production' },
    });
    expect(skipIfResult.status).toBe('skipped');
    expect(skipIfResult.skipped).toBe(true);

    // skipIf with non-matching value — should NOT skip
    const skipIfNotPath = join(templatesDir, 'skipif-not.njk');
    await writeFile(skipIfNotPath, '---\nto: skipif-not.ts\nskipIf: "env==production"\n---\nSHOULD WRITE');
    const skipIfNotResult = await renderWithOptions(skipIfNotPath, [], {
      dryRun: false,
      outputDir,
      context: { env: 'development' },
    });
    expect(skipIfNotResult.status).toBe('success');
    expect(skipIfNotResult.skipped).toBeFalsy();

    // skip_if (snake_case alias) also works
    const skipIfSnakePath = join(templatesDir, 'skipif-snake.njk');
    await writeFile(skipIfSnakePath, '---\nto: skipif-snake.ts\nskip_if: "env==production"\n---\nSHOULD SKIP');
    const skipIfSnakeResult = await renderWithOptions(skipIfSnakePath, [], {
      dryRun: false,
      outputDir,
      context: { env: 'production' },
    });
    expect(skipIfSnakeResult.status).toBe('skipped');

    // unless_exists: true — skip when file already exists
    const unlessPath = join(templatesDir, 'unless.njk');
    await writeFile(unlessPath, '---\nto: unless.ts\nunless_exists: true\n---\nSHOULD NOT WRITE');
    await writeFile(join(outputDir, 'unless.ts'), 'EXISTING', 'utf-8');
    const unlessResult = await renderWithOptions(unlessPath, [], { dryRun: false, outputDir });
    expect(unlessResult.status).toBe('skipped');
    expect((await readFile(join(outputDir, 'unless.ts'), 'utf-8'))).toBe('EXISTING');

    // unless_exists: true — writes when file does NOT exist
    const unlessNewPath = join(templatesDir, 'unless-new.njk');
    await writeFile(unlessNewPath, '---\nto: unless-new.ts\nunless_exists: true\n---\nNEW FILE');
    const unlessNewResult = await renderWithOptions(unlessNewPath, [], { dryRun: false, outputDir });
    expect(unlessNewResult.status).toBe('success');
    expect(existsSync(join(outputDir, 'unless-new.ts'))).toBe(true);
    expect((await readFile(join(outputDir, 'unless-new.ts'), 'utf-8'))).toBe('NEW FILE');

    // Frontmatter validation rejects missing required field "to"
    const noToPath = join(templatesDir, 'noto.njk');
    await writeFile(noToPath, '---\nname: broken\ndescription: missing to\n---\nContent');
    await expect(renderTemplate(noToPath, [])).rejects.toThrow(/Required field missing: "to"/);

    // Frontmatter validation rejects unknown keys
    const unknownKeyPath = join(templatesDir, 'unknown.njk');
    await writeFile(unknownKeyPath, '---\nto: out.ts\nbogus_key: value\n---\nContent');
    await expect(renderTemplate(unknownKeyPath, [])).rejects.toThrow(/Unknown frontmatter key: "bogus_key"/);

    // Frontmatter validation rejects before without inject
    const beforeNoInjectPath = join(templatesDir, 'before-noinject.njk');
    await writeFile(beforeNoInjectPath, '---\nto: out.ts\nbefore: "pattern"\n---\nContent');
    await expect(renderTemplate(beforeNoInjectPath, [])).rejects.toThrow(/"before" requires "inject: true"/);

    // getOperationMode returns correct modes
    expect(getOperationMode({ inject: true, before: 'x' })).toEqual({ mode: 'before', anchor: 'x' });
    expect(getOperationMode({ inject: true, after: 'x' })).toEqual({ mode: 'after', anchor: 'x' });
    expect(getOperationMode({ inject: true })).toEqual({ mode: 'inject' });
    expect(getOperationMode({ lineAt: 5 })).toEqual({ mode: 'lineAt', line: 5 });
    expect(getOperationMode({ append: true })).toEqual({ mode: 'append' });
    expect(getOperationMode({ prepend: true })).toEqual({ mode: 'prepend' });
    expect(getOperationMode({})).toEqual({ mode: 'write' });
    expect(getOperationMode(null)).toEqual({ mode: 'write' });

    // shouldSkip evaluates conditions
    expect(shouldSkip({ skipIf: 'x==1' }, { x: 1 })).toBe(true);
    expect(shouldSkip({ skipIf: 'x==1' }, { x: 2 })).toBe(false);
    expect(shouldSkip({ skipIf: 'x!=1' }, { x: 2 })).toBe(true);
    expect(shouldSkip({ skipIf: '!flag' }, { flag: true })).toBe(false);
    expect(shouldSkip({ skipIf: '!flag' }, { flag: false })).toBe(true);
    expect(shouldSkip({ skipIf: 'flag' }, { flag: true })).toBe(false);
    expect(shouldSkip({ skipIf: 'flag' }, {})).toBe(true);
    expect(shouldSkip({}, {})).toBe(false);
  });

  it('should fix mode:prepend from frontmatter.mode (was silent no-op)', async () => {
    // mode: prepend via frontmatter.mode (not prepend: true)
    const prependModePath = join(templatesDir, 'prepend-mode.njk');
    await writeFile(prependModePath, '---\nto: prepend-mode.ts\nmode: prepend\n---\nPREPENDED');
    await writeFile(join(outputDir, 'prepend-mode.ts'), 'ORIGINAL', 'utf-8');
    const result = await renderWithOptions(prependModePath, [], { dryRun: false, outputDir });
    const content = await readFile(join(outputDir, 'prepend-mode.ts'), 'utf-8');
    expect(content.indexOf('PREPENDED')).toBeLessThan(content.indexOf('ORIGINAL'));
    expect(result.status).toBe('success');
  });

  it('should read force from frontmatter (Hygen parity)', async () => {
    // force: true in frontmatter should overwrite skip_existing
    const forcePath = join(templatesDir, 'force-fm.njk');
    await writeFile(forcePath, '---\nto: force-test.ts\nmode: skip_existing\nforce: true\n---\nFORCED');
    await writeFile(join(outputDir, 'force-test.ts'), 'ORIGINAL', 'utf-8');
    const result = await renderWithOptions(forcePath, [], { dryRun: false, outputDir });
    expect(result.status).toBe('success');
    expect(result.written).toBe(true);
    const content = await readFile(join(outputDir, 'force-test.ts'), 'utf-8');
    expect(content).toContain('FORCED');
  });

  it('should support regex patterns for before/after anchors (Hygen parity)', async () => {
    // before with regex pattern
    const regexBeforePath = join(templatesDir, 'regex-before.njk');
    await writeFile(regexBeforePath, '---\nto: regex-before.ts\ninject: true\nbefore: "/MARKER.*END/"\n---\nINSERTED');
    await writeFile(join(outputDir, 'regex-before.ts'), 'top\nMARKER_END\nbottom', 'utf-8');
    await renderWithOptions(regexBeforePath, [], { dryRun: false, outputDir });
    const beforeContent = await readFile(join(outputDir, 'regex-before.ts'), 'utf-8');
    expect(beforeContent.indexOf('INSERTED')).toBeLessThan(beforeContent.indexOf('MARKER_END'));

    // after with regex pattern
    const regexAfterPath = join(templatesDir, 'regex-after.njk');
    await writeFile(regexAfterPath, '---\nto: regex-after.ts\ninject: true\nafter: "/MARKER.*START/"\n---\nINSERTED');
    await writeFile(join(outputDir, 'regex-after.ts'), 'top\nMARKER_START\nbottom', 'utf-8');
    await renderWithOptions(regexAfterPath, [], { dryRun: false, outputDir });
    const afterContent = await readFile(join(outputDir, 'regex-after.ts'), 'utf-8');
    const anchorIdx = afterContent.indexOf('MARKER_START');
    expect(afterContent.indexOf('INSERTED')).toBeGreaterThan(anchorIdx);

    // String anchor still works (backward compat)
    const stringBeforePath = join(templatesDir, 'string-before.njk');
    await writeFile(stringBeforePath, '---\nto: string-before.ts\ninject: true\nbefore: "EXACT"\n---\nINSERTED');
    await writeFile(join(outputDir, 'string-before.ts'), 'top\nEXACT\nbottom', 'utf-8');
    await renderWithOptions(stringBeforePath, [], { dryRun: false, outputDir });
    const stringContent = await readFile(join(outputDir, 'string-before.ts'), 'utf-8');
    expect(stringContent.indexOf('INSERTED')).toBeLessThan(stringContent.indexOf('EXACT'));
  });

  it('should support regex in shouldSkip (Hygen parity)', async () => {
    // Regex matches any variable value
    expect(shouldSkip({ skipIf: '/prod/' }, { env: 'production' })).toBe(true);
    expect(shouldSkip({ skipIf: '/prod/' }, { env: 'development' })).toBe(false);
    expect(shouldSkip({ skipIf: '/^test/' }, { mode: 'test-value' })).toBe(true);
    expect(shouldSkip({ skipIf: '/^test/' }, { mode: 'production' })).toBe(false);
    expect(shouldSkip({ skipIf: '/\\d+/' }, { version: 'v1.2.3' })).toBe(true);
    expect(shouldSkip({ skipIf: '/\\d+/' }, { version: 'stable' })).toBe(false);

    // Legacy equality still works
    expect(shouldSkip({ skipIf: 'x==1' }, { x: 1 })).toBe(true);
    expect(shouldSkip({ skipIf: 'x==1' }, { x: 2 })).toBe(false);
  });

  it('should support from directive to load body from another file', async () => {
    // Create a partial template
    const partialPath = join(templatesDir, 'partial.njk');
    await writeFile(partialPath, 'Content from partial: {{ name }}');

    // Template with from: directive
    const fromPath = join(templatesDir, 'from.njk');
    await writeFile(fromPath, '---\nto: from-out.txt\nfrom: partial.njk\n---\n');
    const result = await renderWithOptions(fromPath, [], {
      dryRun: false,
      outputDir,
      context: { name: 'Test' },
    });
    expect(result.status).toBe('success');
    const content = await readFile(join(outputDir, 'from-out.txt'), 'utf-8');
    expect(content).toContain('Content from partial: Test');
  });

  it('should support eof_last directive for trailing newline control', async () => {
    // eof_last: false — trim trailing newline
    const eofFalsePath = join(templatesDir, 'eof-false.njk');
    await writeFile(eofFalsePath, '---\nto: eof-false.txt\neof_last: false\n---\nLine 1\nLine 2\n  ');
    await renderWithOptions(eofFalsePath, [], { dryRun: false, outputDir });
    const falseContent = await readFile(join(outputDir, 'eof-false.txt'), 'utf-8');
    expect(falseContent.endsWith('\n')).toBe(false);
    expect(falseContent).toContain('Line 2');

    // eof_last: true — ensure trailing newline
    const eofTruePath = join(templatesDir, 'eof-true.njk');
    await writeFile(eofTruePath, '---\nto: eof-true.txt\neof_last: true\n---\nNoNewline');
    await renderWithOptions(eofTruePath, [], { dryRun: false, outputDir });
    const trueContent = await readFile(join(outputDir, 'eof-true.txt'), 'utf-8');
    expect(trueContent.endsWith('\n')).toBe(true);
  });

  it('should have from, sh_ignore_exit in schema', () => {
    expect(FRONTMATTER_SCHEMA.allowed).toContain('from');
    expect(FRONTMATTER_SCHEMA.allowed).toContain('sh_ignore_exit');
    expect(FRONTMATTER_SCHEMA.types.from).toBe('string');
    expect(FRONTMATTER_SCHEMA.types.sh_ignore_exit).toBe('boolean');
  });
});

describe('preprocessFrontmatter', () => {
  it('should auto-quote unquoted values containing {{ }}', () => {
    const input = `---\nto: {{ output_dir }}/test.py\n---`;
    const expected = `---\nto: "{{ output_dir }}/test.py"\n---`;
    expect(preprocessFrontmatter(input)).toBe(expected);
  });

  it('should not double-quote already quoted values', () => {
    const input = `---\nto: "{{ output_dir }}/test.py"\n---`;
    const expected = `---\nto: "{{ output_dir }}/test.py"\n---`;
    expect(preprocessFrontmatter(input)).toBe(expected);
  });

  it('should not quote boolean, null, or numeric values', () => {
    const input = `---\nenabled: true\ncount: 42\nvalue: null\n---`;
    const expected = input; // Should remain unchanged
    expect(preprocessFrontmatter(input)).toBe(expected);
  });

  it('should handle values with {{ and }} separately', () => {
    const input = `---\nstart: {{\nend: }}\n---`;
    const result = preprocessFrontmatter(input);
    expect(result).toContain('start: "{{');
    expect(result).toContain('end: "');
  });

  it('should preserve YAML indentation and structure', () => {
    // Test with simple key-value pairs (actual template pattern)
    const input = `---\noutput_dir: {{ output_dir }}\nversion: {{ version }}\nmode: overwrite\n---`;
    const result = preprocessFrontmatter(input);
    expect(result).toContain('output_dir: "{{ output_dir }}"');
    expect(result).toContain('version: "{{ version }}"');
    expect(result).toContain('mode: overwrite'); // Non-template value unchanged
  });

  it('should return unchanged content if no frontmatter', () => {
    const input = `no frontmatter here`;
    expect(preprocessFrontmatter(input)).toBe(input);
  });
});
