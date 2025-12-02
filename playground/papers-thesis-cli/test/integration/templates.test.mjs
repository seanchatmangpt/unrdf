/**
 * @fileoverview Template engine integration tests
 *
 * @description
 * Integration tests for Nunjucks template engine and custom filters.
 */

import { describe, it, expect } from 'vitest';
import {
  createTemplateEngine,
  texEscape,
  toBibtexKey,
  slugify,
  formatDate,
  wrapText
} from '../../src/integration/templates.mjs';

describe('Template Engine', () => {
  describe('texEscape', () => {
    it('should escape LaTeX special characters', () => {
      expect(texEscape('&')).toBe('\\&');
      expect(texEscape('%')).toBe('\\%');
      expect(texEscape('$')).toBe('\\$');
      expect(texEscape('#')).toBe('\\#');
      expect(texEscape('_')).toBe('\\_');
      expect(texEscape('{')).toBe('\\{');
      expect(texEscape('}')).toBe('\\}');
    });

    it('should escape multiple special characters', () => {
      expect(texEscape('100% & $50')).toBe('100\\% \\& \\$50');
    });

    it('should handle empty strings', () => {
      expect(texEscape('')).toBe('');
    });

    it('should handle non-string input', () => {
      expect(texEscape(123)).toBe('123');
      expect(texEscape(null)).toBe('');
    });

    it('should escape backslash', () => {
      expect(texEscape('\\')).toBe('\\textbackslash{}');
    });

    it('should escape tilde and caret', () => {
      expect(texEscape('~')).toBe('\\textasciitilde{}');
      expect(texEscape('^')).toBe('\\textasciicircum{}');
    });
  });

  describe('slugify', () => {
    it('should convert to lowercase', () => {
      expect(slugify('UPPERCASE')).toBe('uppercase');
    });

    it('should replace spaces and special chars with hyphens', () => {
      expect(slugify('Hello World')).toBe('hello-world');
      expect(slugify('Test@#$Value')).toBe('testvalue');
    });

    it('should remove leading and trailing hyphens', () => {
      expect(slugify('--test--')).toBe('test');
    });

    it('should collapse multiple hyphens', () => {
      expect(slugify('a    b')).toBe('a-b');
    });

    it('should handle long strings', () => {
      const longString = 'a'.repeat(100);
      expect(slugify(longString).length).toBeGreaterThan(0);
    });
  });

  describe('formatDate', () => {
    it('should format date in long format', () => {
      const result = formatDate('2025-01-15', 'long');
      expect(result).toContain('2025');
      expect(result).toContain('January');
    });

    it('should format date in short format', () => {
      const result = formatDate('2025-01-15', 'short');
      expect(result).toContain('2025');
    });

    it('should format date in ISO format', () => {
      const result = formatDate('2025-01-15T10:30:00Z', 'iso');
      expect(result).toBe('2025-01-15');
    });

    it('should handle Date objects', () => {
      const date = new Date('2025-01-15');
      const result = formatDate(date, 'iso');
      expect(result).toBe('2025-01-15');
    });

    it('should handle invalid dates', () => {
      const result = formatDate('invalid', 'long');
      expect(result).toBe('invalid');
    });
  });

  describe('wrapText', () => {
    it('should wrap text at specified width', () => {
      const text = 'This is a long line of text that should be wrapped';
      const result = wrapText(text, 20);
      const lines = result.split('\n');

      for (const line of lines) {
        expect(line.length).toBeLessThanOrEqual(20);
      }
    });

    it('should preserve single words longer than width', () => {
      const text = 'superlongwordthatcannotbewrapped';
      const result = wrapText(text, 10);
      expect(result).toBe(text);
    });

    it('should handle empty strings', () => {
      expect(wrapText('')).toBe('');
    });
  });

  describe('createTemplateEngine', () => {
    it('should create a template engine instance', () => {
      const engine = createTemplateEngine();

      expect(engine).toBeDefined();
      expect(typeof engine.render).toBe('function');
      expect(typeof engine.renderString).toBe('function');
      expect(typeof engine.getTemplates).toBe('function');
    });

    it('should render template strings', async () => {
      const engine = createTemplateEngine();

      const result = await engine.renderString(
        'Hello {{ name | texescape }}!',
        { name: 'World & Co' }
      );

      expect(result).toBe('Hello World \\& Co!');
    });

    it('should support default filter', async () => {
      const engine = createTemplateEngine();

      const result = await engine.renderString(
        '{{ value | default("fallback") }}',
        { value: undefined }
      );

      expect(result).toBe('fallback');
    });

    it('should support latexjoin filter', async () => {
      const engine = createTemplateEngine();

      const result = await engine.renderString(
        '{{ items | latexjoin("; ") }}',
        { items: ['a & b', 'c $d'] }
      );

      expect(result).toBe('a \\& b; c \\$d');
    });

    it('should support capitalize filter', async () => {
      const engine = createTemplateEngine();

      const result = await engine.renderString(
        '{{ word | capitalize }}',
        { word: 'hello' }
      );

      expect(result).toBe('Hello');
    });

    it('should support titlecase filter', async () => {
      const engine = createTemplateEngine();

      const result = await engine.renderString(
        '{{ phrase | titlecase }}',
        { phrase: 'hello world' }
      );

      expect(result).toBe('Hello World');
    });

    it('should list available templates', async () => {
      const engine = createTemplateEngine();
      const templates = await engine.getTemplates();

      expect(templates).toContain('imrad');
      expect(templates).toContain('dsr');
      expect(templates).toContain('monograph');
    });

    it('should allow adding custom filters', async () => {
      const engine = createTemplateEngine();

      engine.addFilter('shout', (str) => str.toUpperCase() + '!');

      const result = await engine.renderString(
        '{{ message | shout }}',
        { message: 'hello' }
      );

      expect(result).toBe('HELLO!');
    });

    it('should allow adding global variables', async () => {
      const engine = createTemplateEngine();

      engine.addGlobal('version', '1.0.0');

      const result = await engine.renderString(
        'Version: {{ version }}',
        {}
      );

      expect(result).toBe('Version: 1.0.0');
    });
  });

  describe('Template rendering', () => {
    it('should render paper with title and author', async () => {
      const engine = createTemplateEngine();

      const template = `
\\title{ {{- paper.title | texescape -}} }
\\author{ {{- paper.authors[0].name | texescape -}} }
      `.trim();

      const result = await engine.renderString(template, {
        paper: {
          title: 'Test & Demo',
          authors: [{ name: 'Alice $Johnson' }]
        }
      });

      expect(result).toContain('\\title{Test \\& Demo}');
      expect(result).toContain('\\author{Alice \\$Johnson}');
    });

    it('should render sections in a loop', async () => {
      const engine = createTemplateEngine();

      const template = `
{% for section in paper.sections %}
\\section{ {{ section.heading }} }
{% endfor %}
      `.trim();

      const result = await engine.renderString(template, {
        paper: {
          sections: [
            { heading: 'Introduction' },
            { heading: 'Methods' },
            { heading: 'Results' }
          ]
        }
      });

      expect(result).toContain('\\section{ Introduction }');
      expect(result).toContain('\\section{ Methods }');
      expect(result).toContain('\\section{ Results }');
    });

    it('should handle conditional blocks', async () => {
      const engine = createTemplateEngine();

      const template = `
{% if paper.abstract %}
\\begin{abstract}
{{ paper.abstract }}
\\end{abstract}
{% endif %}
      `.trim();

      // With abstract
      const withAbstract = await engine.renderString(template, {
        paper: { abstract: 'Test abstract' }
      });
      expect(withAbstract).toContain('\\begin{abstract}');

      // Without abstract
      const withoutAbstract = await engine.renderString(template, {
        paper: { abstract: '' }
      });
      expect(withoutAbstract).not.toContain('\\begin{abstract}');
    });
  });
});
