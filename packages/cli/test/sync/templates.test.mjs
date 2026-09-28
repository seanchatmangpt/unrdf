/**
 * @file Template Verification Tests
 * @module cli/test/sync/templates
 * @description Validates sync templates exist and have valid structure
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { readFile, mkdir, rm } from 'fs/promises';
import { existsSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { tmpdir } from 'os';
import nunjucks from 'nunjucks';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const TEMPLATES_DIR = resolve(__dirname, '../../templates/sync');
const TEST_OUTPUT_DIR = join(tmpdir(), `templates-test-${Date.now()}`);

const TEMPLATES = {
  openapi: ['openapi-info.njk', 'openapi-schemas.njk', 'openapi-paths.njk', 'openapi-spec.njk'],
  zod: ['zod-entities.njk', 'zod-requests.njk', 'zod-index.njk'],
  types: ['jsdoc-entities.njk', 'jsdoc-requests.njk', 'jsdoc-guards.njk', 'jsdoc-index.njk'],
};

function createTestEnvironment() {
  const env = new nunjucks.Environment(
    new nunjucks.FileSystemLoader(TEMPLATES_DIR),
    { autoescape: false, trimBlocks: true, lstripBlocks: true }
  );
  const camelCase = s => (s || '').replace(/[-_\s]+(.)?/g, (_, c) => (c || '').toUpperCase());
  const pascalCase = s => { const c = camelCase(s); return c.charAt(0).toUpperCase() + c.slice(1); };
  env.addFilter('camelCase', camelCase);
  env.addFilter('pascalCase', pascalCase);
  env.addFilter('snakeCase', s => (s || '').replace(/([A-Z])/g, '_$1').toLowerCase().replace(/^_/, ''));
  env.addFilter('kebabCase', s => (s || '').replace(/([A-Z])/g, '-$1').toLowerCase().replace(/^-/, ''));
  env.addFilter('camel_case', s => { const c = (s || '').replace(/[-_\s]+(.)?/g, (_, ch) => (ch || '').toUpperCase()); return c.charAt(0).toLowerCase() + c.slice(1); });
  env.addFilter('pascal_case', s => { const c = (s || '').replace(/[-_\s]+(.)?/g, (_, ch) => (ch || '').toUpperCase()); return c.charAt(0).toUpperCase() + c.slice(1); });
  env.addFilter('lower', s => (s || '').toLowerCase());
  env.addFilter('upper', s => (s || '').toUpperCase());
  env.addFilter('split', (s, sep) => (s || '').split(sep || ','));
  env.addFilter('trim', s => (s || '').trim());
  env.addFilter('escape', s => String(s || '').replace(/'/g, "\\'").replace(/"/g, '\\"'));
  env.addFilter('localName', uri => (uri || '').split(/[#/]/).pop() || '');
  env.addFilter('namespace', uri => {
    const i = Math.max((uri || '').lastIndexOf('#'), (uri || '').lastIndexOf('/'));
    return i >= 0 ? uri.substring(0, i + 1) : '';
  });
  const typeMap = { string: 'z.string()', integer: 'z.number().int()', boolean: 'z.boolean()' };
  env.addFilter('zodType', t => typeMap[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'z.string()');
  env.addFilter('zod_type', t => typeMap[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'z.string()');
  env.addFilter('jsdocType', t => ({ string: 'string', integer: 'number', boolean: 'boolean' }[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'string'));
  env.addFilter('jsdoc_type', t => ({ string: 'string', integer: 'number', boolean: 'boolean' }[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'string'));
  env.addFilter('groupBy', (arr, key) => {
    const g = {};
    for (const i of arr || []) { const k = i[key] || i[`?${key}`] || 'default'; (g[k] = g[k] || []).push(i); }
    return g;
  });
  env.addFilter('distinctValues', (arr, key) => [...new Set((arr || []).map(i => i[key] || i[`?${key}`]).filter(Boolean))]);
  env.addFilter('sortBy', (arr, key) => [...(arr || [])].sort((a, b) => String(a[key] || '').localeCompare(String(b[key] || ''))));
  env.addFilter('keys', obj => obj ? Object.keys(obj) : []);
  env.addFilter('values', obj => obj ? Object.values(obj) : []);
  env.addFilter('items', obj => obj ? Object.entries(obj) : []);
  env.addFilter('dictsort', obj => obj ? Object.entries(obj).sort(([a], [b]) => a.localeCompare(b)) : []);
  env.addFilter('length', obj => obj?.length ?? Object.keys(obj || {}).length);
  env.addFilter('date', (d, fmt = 'YYYY-MM-DD') => {
    const dt = d instanceof Date ? d : new Date();
    return fmt.replace('YYYY', dt.getFullYear()).replace('MM', String(dt.getMonth() + 1).padStart(2, '0')).replace('DD', String(dt.getDate()).padStart(2, '0'));
  });
  return env;
}

function extractFrontmatter(content) {
  const match = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  if (!match) return { frontmatter: null, body: content };
  const frontmatter = {};
  for (const line of match[1].split('\n')) {
    const colonIndex = line.indexOf(':');
    if (colonIndex > 0) {
      let value = line.slice(colonIndex + 1).trim();
      // Strip quotes if present (handles both " and ' quotes)
      if ((value.startsWith('"') && value.endsWith('"')) || (value.startsWith("'") && value.endsWith("'"))) {
        value = value.slice(1, -1);
      }
      frontmatter[line.slice(0, colonIndex).trim()] = value;
    }
  }
  return { frontmatter, body: match[2] };
}

const SAMPLE_CONTEXT = {
  output_dir: TEST_OUTPUT_DIR, api_version: 'v1', version: '1.0.0',
  project: { name: 'Test API', version: '1.0.0', description: 'Test API from RDF', contact: { name: 'Test', email: 'test@example.com' } },
  sparql_results: [], results: [], entities: ['User'], operations: ['createUser'], now: new Date(), timestamp: new Date().toISOString(),
};

describe('Template Verification', () => {
  beforeAll(async () => { await mkdir(TEST_OUTPUT_DIR, { recursive: true }); });
  afterAll(async () => { if (existsSync(TEST_OUTPUT_DIR)) await rm(TEST_OUTPUT_DIR, { recursive: true, force: true }); });

  it('should have all 11 templates with frontmatter "to" field, non-empty bodies, and balanced Nunjucks blocks', async () => {
    const allTemplates = [...TEMPLATES.openapi, ...TEMPLATES.zod, ...TEMPLATES.types];
    expect(allTemplates).toHaveLength(11);

    for (const [category, templates] of Object.entries(TEMPLATES)) {
      for (const templateName of templates) {
        const templatePath = join(TEMPLATES_DIR, category, templateName);
        const content = await readFile(templatePath, 'utf-8');
        const { frontmatter, body } = extractFrontmatter(content);

        // Frontmatter with "to" field
        expect(frontmatter, `${templateName} should have frontmatter`).not.toBeNull();
        expect(frontmatter.to, `${templateName} should have "to" field`).toBeDefined();
        expect(frontmatter.to, `${templateName} "to" should contain {{`).toContain('{{');

        // Non-empty body
        expect(body.trim().length, `${templateName} should not be empty`).toBeGreaterThan(0);

        // Balanced Nunjucks blocks
        const forOpens = (body.match(/\{%\s*for\s/g) || []).length;
        const forCloses = (body.match(/\{%\s*endfor\s*%\}/g) || []).length;
        expect(forOpens, `${templateName} balanced for loops`).toBe(forCloses);
        const ifOpens = (body.match(/\{%\s*if\s/g) || []).length;
        const ifCloses = (body.match(/\{%\s*endif\s*%\}/g) || []).length;
        expect(ifOpens, `${templateName} balanced if blocks`).toBe(ifCloses);
        const macroOpens = (body.match(/\{%\s*macro\s/g) || []).length;
        const macroCloses = (body.match(/\{%\s*endmacro\s*%\}/g) || []).length;
        expect(macroOpens, `${templateName} balanced macros`).toBe(macroCloses);
      }
    }
  });

  it('should have correct output paths per category and render without syntax errors', async () => {
    // OpenAPI templates output to openapi/ directory with .yaml extension
    for (const templateName of TEMPLATES.openapi) {
      const content = await readFile(join(TEMPLATES_DIR, 'openapi', templateName), 'utf-8');
      const { frontmatter } = extractFrontmatter(content);
      expect(frontmatter.to).toContain('openapi/');
      expect(frontmatter.to).toMatch(/\.(yaml|yml)$/);
    }

    // Zod/types templates output .mjs files
    for (const templateName of [...TEMPLATES.zod, ...TEMPLATES.types]) {
      const category = TEMPLATES.zod.includes(templateName) ? 'zod' : 'types';
      const content = await readFile(join(TEMPLATES_DIR, category, templateName), 'utf-8');
      const { frontmatter } = extractFrontmatter(content);
      expect(frontmatter.to).toMatch(/\.mjs$/);
    }

    // Render without syntax errors
    const env = createTestEnvironment();
    const infoContent = await readFile(join(TEMPLATES_DIR, 'openapi/openapi-info.njk'), 'utf-8');
    const infoRendered = env.renderString(extractFrontmatter(infoContent).body, SAMPLE_CONTEXT);
    expect(infoRendered).toContain('info:');
    expect(infoRendered).toContain('Test API');

    const zodContent = await readFile(join(TEMPLATES_DIR, 'zod/zod-index.njk'), 'utf-8');
    const zodRendered = env.renderString(extractFrontmatter(zodContent).body, SAMPLE_CONTEXT);
    expect(zodRendered).toContain("from './entities.mjs'");
  });

  it('should have expected content in key templates', async () => {
    // openapi-spec.njk includes all parts
    const specContent = await readFile(join(TEMPLATES_DIR, 'openapi/openapi-spec.njk'), 'utf-8');
    const specBody = extractFrontmatter(specContent).body;
    expect(specBody).toContain('openapi: "3.0.3"');
    expect(specBody).toContain('{% include');

    // zod-entities.njk imports zod and exports schemas
    const zodContent = await readFile(join(TEMPLATES_DIR, 'zod/zod-entities.njk'), 'utf-8');
    const zodBody = extractFrontmatter(zodContent).body;
    expect(zodBody).toContain("import { z } from 'zod';");
    expect(zodBody).toContain('export function validateEntity');

    // jsdoc-index.njk is barrel export
    const jsdocContent = await readFile(join(TEMPLATES_DIR, 'types/jsdoc-index.njk'), 'utf-8');
    const jsdocBody = extractFrontmatter(jsdocContent).body;
    expect(jsdocBody).toContain("export * from './types/entities.mjs'");
    expect(jsdocBody).toContain('export const __generated__');
  });
});
