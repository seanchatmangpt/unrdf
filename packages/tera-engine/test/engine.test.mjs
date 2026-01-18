/**
 * @file Core Engine Tests
 * @description Tests for Tera template engine
 */

import { describe, it, expect } from 'vitest';
import { TeraEngine, createTeraEngine, renderTemplate } from '../src/engine.mjs';

describe('TeraEngine', () => {
  describe('constructor', () => {
    it('should create engine with default config', () => {
      const engine = new TeraEngine();
      expect(engine).toBeDefined();
      expect(engine.filters).toEqual({});
    });

    it('should create engine with custom filters', () => {
      const upperFilter = (s) => s.toUpperCase();
      const engine = new TeraEngine({ filters: { upper: upperFilter } });
      expect(typeof engine.filters.upper).toBe('function');
      expect(engine.filters.upper('test')).toBe('TEST');
    });

    it('should validate config with Zod', () => {
      expect(() => new TeraEngine({ filters: 'invalid' })).toThrow();
    });
  });

  describe('variable interpolation', () => {
    it('should interpolate simple variables', () => {
      const engine = new TeraEngine();
      const result = engine.render('Hello {{ name }}!', { name: 'World' });
      expect(result).toBe('Hello World!');
    });

    it('should interpolate nested variables', () => {
      const engine = new TeraEngine();
      const result = engine.render('{{ user.name }}', {
        user: { name: 'Alice' },
      });
      expect(result).toBe('Alice');
    });

    it('should handle undefined variables gracefully', () => {
      const engine = new TeraEngine();
      const result = engine.render('{{ missing }}', {});
      expect(result).toBe('');
    });

    it('should throw on undefined variables in strict mode', () => {
      const engine = new TeraEngine();
      expect(() => {
        engine.render('{{ missing }}', {}, { strictVariables: true });
      }).toThrow('Undefined variable');
    });

    it('should auto-escape HTML by default', () => {
      const engine = new TeraEngine();
      const result = engine.render('{{ html }}', { html: '<script>alert("xss")</script>' });
      expect(result).toContain('&lt;script&gt;');
      expect(result).not.toContain('<script>');
    });

    it('should disable auto-escape when configured', () => {
      const engine = new TeraEngine();
      const result = engine.render('{{ html }}', { html: '<b>bold</b>' }, { autoescape: false });
      expect(result).toBe('<b>bold</b>');
    });
  });

  describe('filters', () => {
    it('should apply custom filters', () => {
      const engine = new TeraEngine({
        filters: {
          double: (n) => n * 2,
        },
      });
      const result = engine.render('{{ count | double }}', { count: 5 });
      expect(result).toBe('10');
    });

    it('should chain multiple filters', () => {
      const engine = new TeraEngine({
        filters: {
          double: (n) => n * 2,
          add: (n, x) => n + x,
        },
      });
      const result = engine.render('{{ count | double | add(3) }}', { count: 5 });
      expect(result).toBe('13');
    });

    it('should throw on unknown filter', () => {
      const engine = new TeraEngine();
      expect(() => {
        engine.render('{{ name | unknown }}', { name: 'test' }, { strictVariables: true });
      }).toThrow();
    });

    it('should register filter via method', () => {
      const engine = new TeraEngine();
      engine.registerFilter('reverse', (s) => s.split('').reverse().join(''));
      const result = engine.render('{{ word | reverse }}', { word: 'hello' });
      expect(result).toBe('olleh');
    });
  });

  describe('for loops', () => {
    it('should iterate over arrays', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% for item in items %}{{ item }}{% endfor %}',
        { items: ['a', 'b', 'c'] }
      );
      expect(result).toBe('abc');
    });

    it('should provide loop variables', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% for item in items %}{{ loop.index }}: {{ item }} {% endfor %}',
        { items: ['a', 'b', 'c'] }
      );
      expect(result).toBe('0: a 1: b 2: c ');
    });

    it.skip('should handle nested loops', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% for row in matrix %}{% for num in row %}{{ num }}{% endfor %};{% endfor %}',
        { matrix: [[1, 2], [3, 4]] }
      );
      expect(result).toBe('12;34;');
    });

    it('should handle empty arrays', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% for item in items %}{{ item }}{% endfor %}',
        { items: [] }
      );
      expect(result).toBe('');
    });

    it('should access loop.first and loop.last', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% for item in items %}{% if loop.first %}FIRST{% endif %}{{ item }}{% if loop.last %}LAST{% endif %}{% endfor %}',
        { items: ['a', 'b', 'c'] }
      );
      expect(result).toBe('FIRSTabcLAST');
    });
  });

  describe('if statements', () => {
    it('should render if branch when truthy', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% if show %}visible{% endif %}',
        { show: true }
      );
      expect(result).toBe('visible');
    });

    it('should skip if branch when falsy', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% if show %}visible{% endif %}',
        { show: false }
      );
      expect(result).toBe('');
    });

    it('should render else branch', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% if show %}yes{% else %}no{% endif %}',
        { show: false }
      );
      expect(result).toBe('no');
    });

    it('should handle nested if statements', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        '{% if a %}{% if b %}both{% endif %}{% endif %}',
        { a: true, b: true }
      );
      expect(result).toBe('both');
    });
  });

  describe('comments', () => {
    it('should remove comments', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        'Before {# this is a comment #} After',
        {}
      );
      expect(result).toBe('Before  After');
    });

    it('should handle multiline comments', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        'Before {# this is\na multiline\ncomment #} After',
        {}
      );
      expect(result).toBe('Before  After');
    });
  });

  describe('whitespace control', () => {
    it.skip('should trim blocks when enabled', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        'Line1{% if true %}\nLine2{% endif %}\nLine3',
        {},
        { trimBlocks: true }
      );
      expect(result).toBe('Line1Line2Line3');
    });

    it.skip('should lstrip blocks when enabled', () => {
      const engine = new TeraEngine();
      const result = engine.render(
        'Start\n  {% if true %}text{% endif %}',
        {},
        { lstripBlocks: true }
      );
      expect(result).toBe('Start\ntext');
    });
  });
});

describe('createTeraEngine', () => {
  it('should create engine instance', () => {
    const engine = createTeraEngine();
    expect(engine).toBeInstanceOf(TeraEngine);
  });

  it('should accept configuration', () => {
    const engine = createTeraEngine({
      filters: { test: () => 'test' },
    });
    expect(engine.filters.test).toBeDefined();
  });
});

describe('renderTemplate', () => {
  it('should render template with context', () => {
    const result = renderTemplate('Hello {{ name }}!', { name: 'World' });
    expect(result).toBe('Hello World!');
  });

  it('should accept options', () => {
    const result = renderTemplate(
      '{{ html }}',
      { html: '<b>test</b>' },
      { autoescape: false }
    );
    expect(result).toBe('<b>test</b>');
  });
});

describe('edge cases', () => {
  it('should handle empty template', () => {
    const engine = new TeraEngine();
    const result = engine.render('', {});
    expect(result).toBe('');
  });

  it('should handle empty context', () => {
    const engine = new TeraEngine();
    const result = engine.render('Static text', {});
    expect(result).toBe('Static text');
  });

  it('should handle special characters in variables', () => {
    const engine = new TeraEngine();
    const result = engine.render('{{ text }}', { text: 'Special: \n\t"quoted"' });
    expect(result).toContain('Special');
  });

  it('should handle numeric values', () => {
    const engine = new TeraEngine();
    const result = engine.render('{{ count }}', { count: 42 });
    expect(result).toBe('42');
  });

  it('should handle boolean values', () => {
    const engine = new TeraEngine();
    const result = engine.render('{{ flag }}', { flag: true });
    expect(result).toBe('true');
  });

  it('should handle null and undefined', () => {
    const engine = new TeraEngine();
    const result = engine.render('{{ value }}', { value: null });
    expect(result).toBe('');
  });
});
