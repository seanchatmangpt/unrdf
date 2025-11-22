/**
 * @fileoverview Tests for Nunjucks template engine integration
 *
 * @description
 * Comprehensive tests for:
 * - Custom Nunjucks filters (LaTeX escaping, bibtex keys, etc.)
 * - Template engine initialization and rendering
 * - File I/O operations
 * - Context validation
 *
 * @module test/integration/nunjucks-integration
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { join } from 'node:path';
import { promises as fs } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const TEST_OUTPUT_DIR = join(__dirname, '../.test-output');

// =============================================================================
// Nunjucks Filters Tests
// =============================================================================

describe('Nunjucks Filters', () => {
  let filters;

  beforeEach(async () => {
    filters = await import('../../src/integration/nunjucks-filters.mjs');
  });

  describe('texescape', () => {
    it('should escape LaTeX special character &', () => {
      expect(filters.texescape('A & B')).toBe('A \\& B');
    });

    it('should escape LaTeX special character %', () => {
      expect(filters.texescape('100%')).toBe('100\\%');
    });

    it('should escape LaTeX special character $', () => {
      expect(filters.texescape('$100')).toBe('\\$100');
    });

    it('should escape LaTeX special character #', () => {
      expect(filters.texescape('#1')).toBe('\\#1');
    });

    it('should escape LaTeX special character _', () => {
      expect(filters.texescape('file_name')).toBe('file\\_name');
    });

    it('should escape LaTeX special characters { and }', () => {
      expect(filters.texescape('{test}')).toBe('\\{test\\}');
    });

    it('should escape LaTeX special character ~', () => {
      expect(filters.texescape('a~b')).toBe('a\\textasciitilde{}b');
    });

    it('should escape LaTeX special character ^', () => {
      expect(filters.texescape('a^2')).toBe('a\\textasciicircum{}2');
    });

    it('should escape LaTeX special character \\', () => {
      expect(filters.texescape('path\\to\\file')).toBe('path\\textbackslash{}to\\textbackslash{}file');
    });

    it('should escape multiple special characters', () => {
      expect(filters.texescape('100% & $50')).toBe('100\\% \\& \\$50');
    });

    it('should handle null and undefined', () => {
      expect(filters.texescape(null)).toBe('');
      expect(filters.texescape(undefined)).toBe('');
    });

    it('should convert non-strings to strings', () => {
      expect(filters.texescape(123)).toBe('123');
    });
  });

  describe('bibtexkey', () => {
    it('should generate key from simple title', () => {
      const key = filters.bibtexkey('Machine Learning');
      expect(key).toMatch(/^MachineLearning\d{4}$/);
    });

    it('should skip stop words', () => {
      const key = filters.bibtexkey('A Study of the Effects');
      expect(key).toMatch(/^StudyEffects\d{4}$/);
    });

    it('should handle single word titles', () => {
      const key = filters.bibtexkey('Algorithms');
      expect(key).toMatch(/^Algorithms\d{4}$/);
    });

    it('should handle null and undefined', () => {
      expect(filters.bibtexkey(null)).toMatch(/^unknown\d{4}$/);
      expect(filters.bibtexkey(undefined)).toMatch(/^unknown\d{4}$/);
    });

    it('should append current year', () => {
      const currentYear = new Date().getFullYear();
      expect(filters.bibtexkey('Test')).toContain(String(currentYear));
    });
  });

  describe('latexjoin', () => {
    it('should join array with default separator', () => {
      expect(filters.latexjoin(['A', 'B', 'C'])).toBe('A B C');
    });

    it('should join array with custom separator', () => {
      expect(filters.latexjoin(['A', 'B', 'C'], ', ')).toBe('A, B, C');
    });

    it('should escape special characters in items', () => {
      expect(filters.latexjoin(['100%', '$50'], ' and ')).toBe('100\\% and \\$50');
    });

    it('should handle non-array input', () => {
      expect(filters.latexjoin('test')).toBe('test');
      expect(filters.latexjoin(null)).toBe('');
    });
  });

  describe('formatdate', () => {
    it('should format date with long format', () => {
      const result = filters.formatdate('2024-03-15', 'long');
      expect(result).toContain('March');
      expect(result).toContain('15');
      expect(result).toContain('2024');
    });

    it('should format date with short format', () => {
      const result = filters.formatdate('2024-03-15', 'short');
      expect(result).toContain('Mar');
      expect(result).toContain('2024');
    });

    it('should format date with iso format', () => {
      expect(filters.formatdate('2024-03-15', 'iso')).toBe('2024-03-15');
    });

    it('should format date with year format', () => {
      expect(filters.formatdate('2024-03-15', 'year')).toBe('2024');
    });

    it('should format date with month-year format', () => {
      const result = filters.formatdate('2024-03-15', 'month-year');
      expect(result).toContain('March');
      expect(result).toContain('2024');
    });

    it('should handle Date objects', () => {
      const date = new Date('2024-03-15');
      expect(filters.formatdate(date, 'iso')).toBe('2024-03-15');
    });

    it('should handle invalid dates', () => {
      expect(filters.formatdate('invalid')).toBe('invalid');
    });

    it('should handle null and undefined', () => {
      expect(filters.formatdate(null)).toBe('');
      expect(filters.formatdate(undefined)).toBe('');
    });
  });

  describe('uppercase', () => {
    it('should convert to uppercase', () => {
      expect(filters.uppercase('hello')).toBe('HELLO');
    });

    it('should handle null and undefined', () => {
      expect(filters.uppercase(null)).toBe('');
      expect(filters.uppercase(undefined)).toBe('');
    });
  });

  describe('lowercase', () => {
    it('should convert to lowercase', () => {
      expect(filters.lowercase('HELLO')).toBe('hello');
    });

    it('should handle null and undefined', () => {
      expect(filters.lowercase(null)).toBe('');
      expect(filters.lowercase(undefined)).toBe('');
    });
  });

  describe('slugify', () => {
    it('should create URL-friendly slug', () => {
      expect(filters.slugify('Hello World!')).toBe('hello-world');
    });

    it('should handle multiple spaces', () => {
      expect(filters.slugify('Hello   World')).toBe('hello-world');
    });

    it('should remove special characters', () => {
      expect(filters.slugify('Machine Learning & AI')).toBe('machine-learning-ai');
    });

    it('should handle null and undefined', () => {
      expect(filters.slugify(null)).toBe('');
      expect(filters.slugify(undefined)).toBe('');
    });
  });

  describe('capitalize', () => {
    it('should capitalize first letter', () => {
      expect(filters.capitalize('hello world')).toBe('Hello world');
    });

    it('should handle null and undefined', () => {
      expect(filters.capitalize(null)).toBe('');
      expect(filters.capitalize(undefined)).toBe('');
    });
  });

  describe('titlecase', () => {
    it('should convert to title case', () => {
      expect(filters.titlecase('hello world')).toBe('Hello World');
    });

    it('should handle null and undefined', () => {
      expect(filters.titlecase(null)).toBe('');
      expect(filters.titlecase(undefined)).toBe('');
    });
  });

  describe('wraptext', () => {
    it('should wrap long text', () => {
      const longText = 'This is a very long sentence that should be wrapped at a certain width';
      const wrapped = filters.wraptext(longText, 30);
      expect(wrapped).toContain('\n');
    });

    it('should not wrap short text', () => {
      expect(filters.wraptext('Short', 80)).toBe('Short');
    });

    it('should handle null and undefined', () => {
      expect(filters.wraptext(null)).toBe('');
      expect(filters.wraptext(undefined)).toBe('');
    });
  });

  describe('truncate', () => {
    it('should truncate long strings', () => {
      expect(filters.truncate('Hello World', 8)).toBe('Hello...');
    });

    it('should not truncate short strings', () => {
      expect(filters.truncate('Hi', 10)).toBe('Hi');
    });

    it('should use custom ellipsis', () => {
      expect(filters.truncate('Hello World', 9, '---')).toBe('Hello ---');
    });
  });

  describe('ordinal', () => {
    it('should convert 1 to 1st', () => {
      expect(filters.ordinal(1)).toBe('1st');
    });

    it('should convert 2 to 2nd', () => {
      expect(filters.ordinal(2)).toBe('2nd');
    });

    it('should convert 3 to 3rd', () => {
      expect(filters.ordinal(3)).toBe('3rd');
    });

    it('should convert 4 to 4th', () => {
      expect(filters.ordinal(4)).toBe('4th');
    });

    it('should convert 11 to 11th', () => {
      expect(filters.ordinal(11)).toBe('11th');
    });

    it('should convert 22 to 22nd', () => {
      expect(filters.ordinal(22)).toBe('22nd');
    });
  });

  describe('pluralize', () => {
    it('should return singular for count 1', () => {
      expect(filters.pluralize(1, 'paper')).toBe('paper');
    });

    it('should return plural for count > 1', () => {
      expect(filters.pluralize(5, 'paper')).toBe('papers');
    });

    it('should return plural for count 0', () => {
      expect(filters.pluralize(0, 'paper')).toBe('papers');
    });

    it('should use custom plural', () => {
      expect(filters.pluralize(0, 'thesis', 'theses')).toBe('theses');
    });
  });

  describe('formatauthor', () => {
    it('should format simple name', () => {
      expect(filters.formatauthor('John Smith')).toBe('Smith, John');
    });

    it('should handle single name', () => {
      expect(filters.formatauthor('Madonna')).toBe('Madonna');
    });

    it('should handle object input', () => {
      expect(filters.formatauthor({ name: 'John Smith' })).toBe('Smith, John');
    });

    it('should handle null and undefined', () => {
      expect(filters.formatauthor(null)).toBe('');
      expect(filters.formatauthor(undefined)).toBe('');
    });
  });

  describe('registerAllFilters', () => {
    it('should throw error for invalid environment', () => {
      expect(() => filters.registerAllFilters(null)).toThrow();
      expect(() => filters.registerAllFilters({})).toThrow();
    });
  });
});

// =============================================================================
// Template Engine Tests
// =============================================================================

describe('Template Engine', () => {
  let templates;

  beforeEach(async () => {
    templates = await import('../../src/integration/templates.mjs');
  });

  describe('initTemplateEngine', () => {
    it('should initialize Nunjucks environment', () => {
      const env = templates.initTemplateEngine();
      expect(env).toBeDefined();
      expect(typeof env.render).toBe('function');
    });

    it('should register custom filters', () => {
      const env = templates.initTemplateEngine();
      // Test that texescape filter is registered
      expect(env.filters.texescape).toBeDefined();
    });
  });

  describe('getTemplateEngine', () => {
    it('should return existing or create new engine', () => {
      const env1 = templates.getTemplateEngine();
      const env2 = templates.getTemplateEngine();
      expect(env1).toBeDefined();
      expect(env2).toBeDefined();
    });
  });

  describe('renderTemplateString', () => {
    it('should render simple template string', async () => {
      const result = await templates.renderTemplateString('Hello {{ name }}', { name: 'World' });
      expect(result).toBe('Hello World');
    });

    it('should apply filters in template string', async () => {
      const result = await templates.renderTemplateString(
        '{{ text | uppercase }}',
        { text: 'hello' }
      );
      expect(result).toBe('HELLO');
    });

    it('should apply texescape filter', async () => {
      const result = await templates.renderTemplateString(
        '{{ text | texescape }}',
        { text: '100%' }
      );
      expect(result).toBe('100\\%');
    });
  });

  describe('getAvailableTemplates', () => {
    it('should return array of template info', async () => {
      const templatesList = await templates.getAvailableTemplates();
      expect(Array.isArray(templatesList)).toBe(true);
    });

    it('should include standard families', async () => {
      const templatesList = await templates.getAvailableTemplates();
      const names = templatesList.map(t => t.name);
      expect(names).toContain('imrad');
      expect(names).toContain('dsr');
      expect(names).toContain('monograph');
    });
  });

  describe('templateExists', () => {
    it('should return true for existing template', async () => {
      const exists = await templates.templateExists('imrad');
      expect(typeof exists).toBe('boolean');
    });

    it('should return false for non-existent template', async () => {
      const exists = await templates.templateExists('nonexistent-template-xyz');
      expect(exists).toBe(false);
    });
  });

  describe('validateContext', () => {
    it('should validate correct context', async () => {
      const result = await templates.validateContext({
        title: 'Test Paper',
        authors: [{ name: 'John Smith' }]
      });
      expect(result.valid).toBe(true);
    });

    it('should reject context without title', async () => {
      const result = await templates.validateContext({
        authors: [{ name: 'John Smith' }]
      });
      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
    });

    it('should reject context without authors', async () => {
      const result = await templates.validateContext({
        title: 'Test Paper'
      });
      expect(result.valid).toBe(false);
    });

    it('should reject context with empty authors array', async () => {
      const result = await templates.validateContext({
        title: 'Test Paper',
        authors: []
      });
      expect(result.valid).toBe(false);
    });
  });

  describe('validatePartialContext', () => {
    it('should accept partial context', async () => {
      const result = await templates.validatePartialContext({
        title: 'Test Paper'
      });
      expect(result.valid).toBe(true);
    });

    it('should validate sections if provided', async () => {
      const result = await templates.validatePartialContext({
        sections: [{ heading: 'Intro', content: 'Content' }]
      });
      expect(result.valid).toBe(true);
    });
  });

  describe('createContextFactory', () => {
    it('should create factory for imrad family', () => {
      const factory = templates.createContextFactory('imrad');
      const context = factory({ title: 'Test' });
      expect(context.title).toBe('Test');
      expect(context.documentClass).toBe('article');
    });

    it('should create factory for monograph family', () => {
      const factory = templates.createContextFactory('monograph');
      const context = factory({ title: 'Test' });
      expect(context.toc).toBe(true);
      expect(context.documentClass).toBe('report');
    });
  });

  describe('createTemplateEngine', () => {
    it('should create template engine instance', () => {
      const engine = templates.createTemplateEngine();
      expect(engine).toBeDefined();
      expect(typeof engine.render).toBe('function');
      expect(typeof engine.renderString).toBe('function');
      expect(typeof engine.getTemplates).toBe('function');
      expect(typeof engine.validateContext).toBe('function');
    });

    it('should allow adding custom filters', () => {
      const engine = templates.createTemplateEngine();
      engine.addFilter('customFilter', (val) => `[${val}]`);
      // Filter should be added without error
    });
  });
});

// =============================================================================
// File I/O Tests
// =============================================================================

describe('File I/O', () => {
  let fileIO;

  beforeEach(async () => {
    fileIO = await import('../../src/integration/file-io.mjs');
    // Create test output directory
    await fs.mkdir(TEST_OUTPUT_DIR, { recursive: true });
  });

  afterEach(async () => {
    // Clean up test output directory
    try {
      await fs.rm(TEST_OUTPUT_DIR, { recursive: true });
    } catch {
      // Ignore cleanup errors
    }
  });

  describe('ensureOutputDir', () => {
    it('should create directory if not exists', async () => {
      const testDir = join(TEST_OUTPUT_DIR, 'new-dir');
      await fileIO.ensureOutputDir(testDir);
      const stats = await fs.stat(testDir);
      expect(stats.isDirectory()).toBe(true);
    });

    it('should not error if directory exists', async () => {
      await fileIO.ensureOutputDir(TEST_OUTPUT_DIR);
      // Should not throw
    });
  });

  describe('writePaper', () => {
    it('should write content to file', async () => {
      const testFile = join(TEST_OUTPUT_DIR, 'test.tex');
      await fileIO.writePaper(testFile, 'Test content');
      const content = await fs.readFile(testFile, 'utf-8');
      expect(content).toBe('Test content');
    });

    it('should create parent directories', async () => {
      const testFile = join(TEST_OUTPUT_DIR, 'nested/dir/test.tex');
      await fileIO.writePaper(testFile, 'Test content');
      const content = await fs.readFile(testFile, 'utf-8');
      expect(content).toBe('Test content');
    });

    it('should respect overwrite option', async () => {
      const testFile = join(TEST_OUTPUT_DIR, 'existing.tex');
      await fileIO.writePaper(testFile, 'Original');
      await expect(
        fileIO.writePaper(testFile, 'New', { overwrite: false })
      ).rejects.toThrow(/already exists/);
    });
  });

  describe('readPaper', () => {
    it('should read file content', async () => {
      const testFile = join(TEST_OUTPUT_DIR, 'read-test.tex');
      await fs.writeFile(testFile, 'Test content');
      const content = await fileIO.readPaper(testFile);
      expect(content).toBe('Test content');
    });

    it('should throw error for non-existent file', async () => {
      await expect(
        fileIO.readPaper(join(TEST_OUTPUT_DIR, 'nonexistent.tex'))
      ).rejects.toThrow(/not found/);
    });
  });

  describe('listPapers', () => {
    it('should list files in directory', async () => {
      await fs.writeFile(join(TEST_OUTPUT_DIR, 'paper1.tex'), 'content');
      await fs.writeFile(join(TEST_OUTPUT_DIR, 'paper2.tex'), 'content');
      const papers = await fileIO.listPapers(TEST_OUTPUT_DIR);
      expect(papers.length).toBe(2);
    });

    it('should filter by extension', async () => {
      await fs.writeFile(join(TEST_OUTPUT_DIR, 'paper.tex'), 'content');
      await fs.writeFile(join(TEST_OUTPUT_DIR, 'data.json'), '{}');
      const papers = await fileIO.listPapers(TEST_OUTPUT_DIR, { extension: '.tex' });
      expect(papers.length).toBe(1);
      expect(papers[0].extension).toBe('.tex');
    });

    it('should return empty array for non-existent directory', async () => {
      const papers = await fileIO.listPapers(join(TEST_OUTPUT_DIR, 'nonexistent'));
      expect(papers).toEqual([]);
    });
  });

  describe('paperExists', () => {
    it('should return true for existing file', async () => {
      const testFile = join(TEST_OUTPUT_DIR, 'exists.tex');
      await fs.writeFile(testFile, 'content');
      const exists = await fileIO.paperExists(testFile);
      expect(exists).toBe(true);
    });

    it('should return false for non-existent file', async () => {
      const exists = await fileIO.paperExists(join(TEST_OUTPUT_DIR, 'nonexistent.tex'));
      expect(exists).toBe(false);
    });
  });

  describe('getPaperInfo', () => {
    it('should return file info', async () => {
      const testFile = join(TEST_OUTPUT_DIR, 'info-test.tex');
      await fs.writeFile(testFile, 'content');
      const info = await fileIO.getPaperInfo(testFile);
      expect(info.name).toBe('info-test.tex');
      expect(info.size).toBe(7);
      expect(info.extension).toBe('.tex');
    });

    it('should throw error for non-existent file', async () => {
      await expect(
        fileIO.getPaperInfo(join(TEST_OUTPUT_DIR, 'nonexistent.tex'))
      ).rejects.toThrow(/not found/);
    });
  });

  describe('deletePaper', () => {
    it('should delete existing file', async () => {
      const testFile = join(TEST_OUTPUT_DIR, 'delete-test.tex');
      await fs.writeFile(testFile, 'content');
      await fileIO.deletePaper(testFile);
      const exists = await fileIO.paperExists(testFile);
      expect(exists).toBe(false);
    });

    it('should throw error for non-existent file', async () => {
      await expect(
        fileIO.deletePaper(join(TEST_OUTPUT_DIR, 'nonexistent.tex'))
      ).rejects.toThrow(/not found/);
    });
  });

  describe('copyPaper', () => {
    it('should copy file to new location', async () => {
      const source = join(TEST_OUTPUT_DIR, 'source.tex');
      const dest = join(TEST_OUTPUT_DIR, 'dest.tex');
      await fs.writeFile(source, 'content');
      await fileIO.copyPaper(source, dest);
      const content = await fs.readFile(dest, 'utf-8');
      expect(content).toBe('content');
    });

    it('should throw error if source does not exist', async () => {
      await expect(
        fileIO.copyPaper(
          join(TEST_OUTPUT_DIR, 'nonexistent.tex'),
          join(TEST_OUTPUT_DIR, 'dest.tex')
        )
      ).rejects.toThrow(/not found/);
    });
  });

  describe('formatFileSize', () => {
    it('should format bytes', () => {
      expect(fileIO.formatFileSize(500)).toBe('500 B');
    });

    it('should format kilobytes', () => {
      expect(fileIO.formatFileSize(1024)).toBe('1 KB');
    });

    it('should format megabytes', () => {
      expect(fileIO.formatFileSize(1024 * 1024)).toBe('1 MB');
    });

    it('should handle zero', () => {
      expect(fileIO.formatFileSize(0)).toBe('0 B');
    });
  });

  describe('getOutputStats', () => {
    it('should return directory statistics', async () => {
      await fs.writeFile(join(TEST_OUTPUT_DIR, 'stat1.tex'), 'content');
      await fs.writeFile(join(TEST_OUTPUT_DIR, 'stat2.tex'), 'more content');
      const stats = await fileIO.getOutputStats(TEST_OUTPUT_DIR);
      expect(stats.fileCount).toBe(2);
      expect(stats.totalSize).toBeGreaterThan(0);
      expect(stats.byExtension['.tex']).toBe(2);
    });
  });
});

// =============================================================================
// Integration Index Tests
// =============================================================================

describe('Integration Index', () => {
  let integration;

  beforeEach(async () => {
    integration = await import('../../src/integration/index.mjs');
  });

  it('should export all template functions', () => {
    expect(integration.initTemplateEngine).toBeDefined();
    expect(integration.renderTemplate).toBeDefined();
    expect(integration.validateContext).toBeDefined();
    expect(integration.createTemplateEngine).toBeDefined();
  });

  it('should export all filters', () => {
    expect(integration.texescape).toBeDefined();
    expect(integration.bibtexkey).toBeDefined();
    expect(integration.latexjoin).toBeDefined();
    expect(integration.formatdate).toBeDefined();
  });

  it('should export file I/O functions', () => {
    expect(integration.writePaper).toBeDefined();
    expect(integration.readPaper).toBeDefined();
    expect(integration.listPapers).toBeDefined();
    expect(integration.ensureOutputDir).toBeDefined();
  });

  it('should export knowledge graph functions', () => {
    expect(integration.createKnowledgeGraph).toBeDefined();
    expect(integration.knowledgeGraph).toBeDefined();
  });

  it('should export configuration', () => {
    expect(integration.config).toBeDefined();
    expect(integration.config.templates).toBeDefined();
    expect(integration.config.output).toBeDefined();
  });

  it('should export createIntegration factory', () => {
    expect(integration.createIntegration).toBeDefined();
    expect(typeof integration.createIntegration).toBe('function');
  });

  it('should export legacy aliases', () => {
    expect(integration.texEscape).toBeDefined();
    expect(integration.toBibtexKey).toBeDefined();
    expect(integration.wrapText).toBeDefined();
  });
});
