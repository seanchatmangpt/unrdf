/**
 * @file Template Verification Tests
 * @module cli/test/sync/templates
 * @description Validates sync templates exist and have valid structure
 *
 * Tests each template in /packages/cli/templates/sync/:
 * - openapi/*.njk (4 templates)
 * - zod/*.njk (3 templates)
 * - types/*.njk (4 templates)
 *
 * These tests catch:
 * - Missing template files
 * - Invalid template structure
 * - Template syntax errors (where possible)
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

/**
 * Path to templates directory
 */
const TEMPLATES_DIR = resolve(__dirname, '../../templates/sync');

/**
 * Temporary output directory for tests
 */
const TEST_OUTPUT_DIR = join(tmpdir(), `templates-test-${Date.now()}`);

/**
 * All template files to verify
 */
const TEMPLATES = {
  openapi: [
    'openapi-info.njk',
    'openapi-schemas.njk',
    'openapi-paths.njk',
    'openapi-spec.njk',
  ],
  zod: [
    'zod-entities.njk',
    'zod-requests.njk',
    'zod-index.njk',
  ],
  types: [
    'jsdoc-entities.njk',
    'jsdoc-requests.njk',
    'jsdoc-guards.njk',
    'jsdoc-index.njk',
  ],
};

/**
 * Creates a Nunjucks environment with filters
 * @returns {nunjucks.Environment}
 */
function createTestEnvironment() {
  const env = new nunjucks.Environment(
    new nunjucks.FileSystemLoader(TEMPLATES_DIR),
    { autoescape: false, trimBlocks: true, lstripBlocks: true }
  );

  // Case filters
  const camelCase = s => (s || '').replace(/[-_\s]+(.)?/g, (_, c) => (c || '').toUpperCase());
  const pascalCase = s => { const c = camelCase(s); return c.charAt(0).toUpperCase() + c.slice(1); };

  env.addFilter('camelCase', camelCase);
  env.addFilter('camel_case', camelCase);
  env.addFilter('pascalCase', pascalCase);
  env.addFilter('pascal_case', pascalCase);
  env.addFilter('snakeCase', s => (s || '').replace(/([A-Z])/g, '_$1').toLowerCase().replace(/^_/, ''));
  env.addFilter('kebabCase', s => (s || '').replace(/([A-Z])/g, '-$1').toLowerCase().replace(/^-/, ''));
  env.addFilter('lower', s => (s || '').toLowerCase());
  env.addFilter('upper', s => (s || '').toUpperCase());

  // String filters
  env.addFilter('split', (s, sep) => (s || '').split(sep || ','));
  env.addFilter('trim', s => (s || '').trim());
  env.addFilter('escape', s => String(s || '').replace(/'/g, "\\'").replace(/"/g, '\\"'));

  // RDF filters
  env.addFilter('localName', uri => (uri || '').split(/[#/]/).pop() || '');
  env.addFilter('namespace', uri => {
    const i = Math.max((uri || '').lastIndexOf('#'), (uri || '').lastIndexOf('/'));
    return i >= 0 ? uri.substring(0, i + 1) : '';
  });

  // Type filters
  const typeMap = { string: 'z.string()', integer: 'z.number().int()', boolean: 'z.boolean()' };
  env.addFilter('zodType', t => typeMap[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'z.string()');
  env.addFilter('zod_type', t => typeMap[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'z.string()');
  env.addFilter('jsdocType', t => ({ string: 'string', integer: 'number', boolean: 'boolean' }[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'string'));
  env.addFilter('jsdoc_type', t => ({ string: 'string', integer: 'number', boolean: 'boolean' }[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'string'));

  // Data filters
  env.addFilter('groupBy', (arr, key) => {
    const g = {};
    for (const i of arr || []) {
      const k = i[key] || i[`?${key}`] || 'default';
      (g[k] = g[k] || []).push(i);
    }
    return g;
  });
  env.addFilter('distinctValues', (arr, key) =>
    [...new Set((arr || []).map(i => i[key] || i[`?${key}`]).filter(Boolean))]);
  env.addFilter('sortBy', (arr, key) =>
    [...(arr || [])].sort((a, b) => String(a[key] || '').localeCompare(String(b[key] || ''))));
  env.addFilter('keys', obj => obj ? Object.keys(obj) : []);
  env.addFilter('values', obj => obj ? Object.values(obj) : []);
  env.addFilter('items', obj => obj ? Object.entries(obj) : []);
  env.addFilter('dictsort', obj => obj ? Object.entries(obj).sort(([a], [b]) => a.localeCompare(b)) : []);
  env.addFilter('length', obj => obj?.length ?? Object.keys(obj || {}).length);

  // Date filter
  env.addFilter('date', (d, fmt = 'YYYY-MM-DD') => {
    const dt = d instanceof Date ? d : new Date();
    return fmt.replace('YYYY', dt.getFullYear())
      .replace('MM', String(dt.getMonth() + 1).padStart(2, '0'))
      .replace('DD', String(dt.getDate()).padStart(2, '0'));
  });

  return env;
}

/**
 * Extracts frontmatter manually (more robust than gray-matter for templates)
 * @param {string} content - Template content
 * @returns {{frontmatter: Object|null, body: string}}
 */
function extractFrontmatter(content) {
  const match = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  if (!match) return { frontmatter: null, body: content };

  const frontmatterText = match[1];
  const body = match[2];

  // Parse simple key: value pairs
  const frontmatter = {};
  for (const line of frontmatterText.split('\n')) {
    const colonIndex = line.indexOf(':');
    if (colonIndex > 0) {
      const key = line.slice(0, colonIndex).trim();
      const value = line.slice(colonIndex + 1).trim();
      frontmatter[key] = value;
    }
  }

  return { frontmatter, body };
}

/**
 * Sample context for rendering
 */
const SAMPLE_CONTEXT = {
  output_dir: TEST_OUTPUT_DIR,
  api_version: 'v1',
  version: '1.0.0',
  project: {
    name: 'Test API',
    version: '1.0.0',
    description: 'Test API from RDF',
    auth: { bearer: true },
    contact: { name: 'Test', email: 'test@example.com' },
    license: { name: 'MIT' },
  },
  sparql_results: [],
  results: [],
  entities: ['User'],
  operations: ['createUser'],
  now: new Date(),
  timestamp: new Date().toISOString(),
};

// =============================================================================
// Tests
// =============================================================================

describe('Template Verification', () => {
  beforeAll(async () => {
    await mkdir(TEST_OUTPUT_DIR, { recursive: true });
  });

  afterAll(async () => {
    if (existsSync(TEST_OUTPUT_DIR)) {
      await rm(TEST_OUTPUT_DIR, { recursive: true, force: true });
    }
  });

  // ===========================================================================
  // Template Inventory
  // ===========================================================================

  describe('Template Inventory', () => {
    it('should have 4 openapi templates', () => {
      const dir = join(TEMPLATES_DIR, 'openapi');
      for (const template of TEMPLATES.openapi) {
        expect(existsSync(join(dir, template)), `${template} should exist`).toBe(true);
      }
    });

    it('should have 3 zod templates', () => {
      const dir = join(TEMPLATES_DIR, 'zod');
      for (const template of TEMPLATES.zod) {
        expect(existsSync(join(dir, template)), `${template} should exist`).toBe(true);
      }
    });

    it('should have 4 types templates', () => {
      const dir = join(TEMPLATES_DIR, 'types');
      for (const template of TEMPLATES.types) {
        expect(existsSync(join(dir, template)), `${template} should exist`).toBe(true);
      }
    });

    it('should have 11 total templates', () => {
      const total = TEMPLATES.openapi.length + TEMPLATES.zod.length + TEMPLATES.types.length;
      expect(total).toBe(11);
    });
  });

  // ===========================================================================
  // Template Structure
  // ===========================================================================

  describe('Template Structure', () => {
    for (const [category, templates] of Object.entries(TEMPLATES)) {
      describe(`${category} templates`, () => {
        for (const templateName of templates) {
          it(`${templateName} should have frontmatter with "to" field`, async () => {
            const templatePath = join(TEMPLATES_DIR, category, templateName);
            const content = await readFile(templatePath, 'utf-8');
            const { frontmatter } = extractFrontmatter(content);

            expect(frontmatter).not.toBeNull();
            expect(frontmatter.to).toBeDefined();
            expect(frontmatter.to).toContain('{{');
          });

          it(`${templateName} should start with frontmatter delimiters`, async () => {
            const templatePath = join(TEMPLATES_DIR, category, templateName);
            const content = await readFile(templatePath, 'utf-8');

            expect(content.startsWith('---\n')).toBe(true);
            expect(content.includes('\n---\n')).toBe(true);
          });

          it(`${templateName} should not be empty`, async () => {
            const templatePath = join(TEMPLATES_DIR, category, templateName);
            const content = await readFile(templatePath, 'utf-8');
            const { body } = extractFrontmatter(content);

            expect(body.trim().length).toBeGreaterThan(0);
          });
        }
      });
    }
  });

  // ===========================================================================
  // OpenAPI Template Content
  // ===========================================================================

  describe('OpenAPI Template Content', () => {
    it('openapi-info.njk should contain info section', async () => {
      const templatePath = join(TEMPLATES_DIR, 'openapi/openapi-info.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain('info:');
      expect(body).toContain('title:');
      expect(body).toContain('version:');
      expect(body).toContain('x-generated:');
    });

    it('openapi-schemas.njk should define schema structures', async () => {
      const templatePath = join(TEMPLATES_DIR, 'openapi/openapi-schemas.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain('schemas:');
      expect(body).toContain('Error:');
      expect(body).toContain('PaginatedResponse:');
      expect(body).toContain('Link:');
    });

    it('openapi-paths.njk should define paths structure', async () => {
      const templatePath = join(TEMPLATES_DIR, 'openapi/openapi-paths.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain('paths:');
      expect(body).toContain('responses:');
      expect(body).toContain('"200":');
    });

    it('openapi-spec.njk should include all parts', async () => {
      const templatePath = join(TEMPLATES_DIR, 'openapi/openapi-spec.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain('openapi: "3.0.3"');
      expect(body).toContain('servers:');
      expect(body).toContain('components:');
      expect(body).toContain('x-unrdf-metadata:');
      expect(body).toContain('{% include');
    });
  });

  // ===========================================================================
  // Zod Template Content
  // ===========================================================================

  describe('Zod Template Content', () => {
    it('zod-entities.njk should import zod and export schemas', async () => {
      const templatePath = join(TEMPLATES_DIR, 'zod/zod-entities.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain("import { z } from 'zod';");
      expect(body).toContain('export const baseSchemas');
      expect(body).toContain('export const entitySchemas');
      expect(body).toContain('export function getEntitySchema');
      expect(body).toContain('export function validateEntity');
    });

    it('zod-requests.njk should define request schemas', async () => {
      const templatePath = join(TEMPLATES_DIR, 'zod/zod-requests.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain("import { z } from 'zod';");
      expect(body).toContain('export const commonSchemas');
      expect(body).toContain('pagination:');
      expect(body).toContain('export const requestSchemas');
    });

    it('zod-index.njk should be a barrel export', async () => {
      const templatePath = join(TEMPLATES_DIR, 'zod/zod-index.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain("from './entities.mjs'");
      expect(body).toContain("from './requests.mjs'");
      expect(body).toContain("export { z } from 'zod';");
    });
  });

  // ===========================================================================
  // JSDoc/Types Template Content
  // ===========================================================================

  describe('Types Template Content', () => {
    it('jsdoc-entities.njk should have JSDoc typedefs', async () => {
      const templatePath = join(TEMPLATES_DIR, 'types/jsdoc-entities.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain('@file');
      expect(body).toContain('@module');
      expect(body).toContain('@typedef');
      expect(body).toContain('export const EntityNames');
      expect(body).toContain('export const ALL_ENTITY_NAMES');
    });

    it('jsdoc-requests.njk should define request types', async () => {
      const templatePath = join(TEMPLATES_DIR, 'types/jsdoc-requests.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain('@file');
      expect(body).toContain('@typedef');
      expect(body).toContain('PaginationRequest');
      expect(body).toContain('PaginatedResponse');
      expect(body).toContain('export const OperationNames');
    });

    it('jsdoc-guards.njk should export type guards', async () => {
      const templatePath = join(TEMPLATES_DIR, 'types/jsdoc-guards.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain("import { z } from 'zod';");
      expect(body).toContain('export const entitySchemas');
      expect(body).toContain('export function getSchema');
      expect(body).toContain('export function hasSchema');
    });

    it('jsdoc-index.njk should be a barrel export', async () => {
      const templatePath = join(TEMPLATES_DIR, 'types/jsdoc-index.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);

      expect(body).toContain("export * from './types/entities.mjs'");
      expect(body).toContain("export * from './types/requests.mjs'");
      expect(body).toContain("export * from './guards/entities.mjs'");
      expect(body).toContain('export const __generated__');
    });
  });

  // ===========================================================================
  // Nunjucks Syntax Verification
  // ===========================================================================

  describe('Nunjucks Syntax', () => {
    it('templates should have balanced Nunjucks blocks', async () => {
      for (const [category, templates] of Object.entries(TEMPLATES)) {
        for (const templateName of templates) {
          const templatePath = join(TEMPLATES_DIR, category, templateName);
          const content = await readFile(templatePath, 'utf-8');
          const { body } = extractFrontmatter(content);

          // Count block openers and closers
          const forOpens = (body.match(/\{%\s*for\s/g) || []).length;
          const forCloses = (body.match(/\{%\s*endfor\s*%\}/g) || []).length;
          expect(forOpens, `${templateName} should have balanced for loops`).toBe(forCloses);

          const ifOpens = (body.match(/\{%\s*if\s/g) || []).length;
          const ifCloses = (body.match(/\{%\s*endif\s*%\}/g) || []).length;
          expect(ifOpens, `${templateName} should have balanced if blocks`).toBe(ifCloses);

          const setOpens = (body.match(/\{%\s*set\s/g) || []).length;
          const macroOpens = (body.match(/\{%\s*macro\s/g) || []).length;
          const macroCloses = (body.match(/\{%\s*endmacro\s*%\}/g) || []).length;
          expect(macroOpens, `${templateName} should have balanced macros`).toBe(macroCloses);
        }
      }
    });

    it('templates should use valid variable syntax', async () => {
      for (const [category, templates] of Object.entries(TEMPLATES)) {
        for (const templateName of templates) {
          const templatePath = join(TEMPLATES_DIR, category, templateName);
          const content = await readFile(templatePath, 'utf-8');
          const { body } = extractFrontmatter(content);

          // Check for double curly braces
          const varMatches = body.match(/\{\{[^}]+\}\}/g) || [];
          for (const match of varMatches) {
            // Should not have unclosed braces
            expect(match.includes('{{{'), `${templateName} has invalid triple brace`).toBe(false);
          }
        }
      }
    });
  });

  // ===========================================================================
  // Simple Rendering Test
  // ===========================================================================

  describe('Basic Rendering', () => {
    it('openapi-info.njk should render without syntax errors', async () => {
      const templatePath = join(TEMPLATES_DIR, 'openapi/openapi-info.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);
      const env = createTestEnvironment();

      // Should not throw
      const rendered = env.renderString(body, SAMPLE_CONTEXT);
      expect(rendered).toContain('info:');
      expect(rendered).toContain('Test API');
    });

    it('zod-index.njk should render without syntax errors', async () => {
      const templatePath = join(TEMPLATES_DIR, 'zod/zod-index.njk');
      const content = await readFile(templatePath, 'utf-8');
      const { body } = extractFrontmatter(content);
      const env = createTestEnvironment();

      const rendered = env.renderString(body, SAMPLE_CONTEXT);
      expect(rendered).toContain("from './entities.mjs'");
      expect(rendered).toContain("export { z } from 'zod';");
    });
  });

  // ===========================================================================
  // Output Path Validation
  // ===========================================================================

  describe('Output Paths', () => {
    it('openapi templates should output to openapi directory', async () => {
      for (const templateName of TEMPLATES.openapi) {
        const templatePath = join(TEMPLATES_DIR, 'openapi', templateName);
        const content = await readFile(templatePath, 'utf-8');
        const { frontmatter } = extractFrontmatter(content);

        expect(frontmatter.to).toContain('openapi/');
        expect(frontmatter.to).toMatch(/\.(yaml|yml)$/);
      }
    });

    it('zod templates should output .mjs files', async () => {
      for (const templateName of TEMPLATES.zod) {
        const templatePath = join(TEMPLATES_DIR, 'zod', templateName);
        const content = await readFile(templatePath, 'utf-8');
        const { frontmatter } = extractFrontmatter(content);

        expect(frontmatter.to).toMatch(/\.mjs$/);
      }
    });

    it('types templates should output .mjs files', async () => {
      for (const templateName of TEMPLATES.types) {
        const templatePath = join(TEMPLATES_DIR, 'types', templateName);
        const content = await readFile(templatePath, 'utf-8');
        const { frontmatter } = extractFrontmatter(content);

        expect(frontmatter.to).toMatch(/\.mjs$/);
      }
    });
  });
});
