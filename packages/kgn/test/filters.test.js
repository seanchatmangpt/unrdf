/**
 * KGEN Filters Test Suite
 * Validates all implemented filters for compatibility and deterministic behavior
 */

import { createCustomFilters } from '../src/filters/index.js';

describe('KGEN Filters', () => {
  let filters;
  let deterministicFilters;

  beforeEach(() => {
    filters = createCustomFilters();
    deterministicFilters = createCustomFilters({
      deterministicMode: true,
      staticBuildTime: '2024-01-01T00:00:00.000Z'
    });
  });

  describe('Text Filters', () => {
    test('upper/lower case conversion', () => {
      expect(filters.upper('hello')).toBe('HELLO');
      expect(filters.lower('WORLD')).toBe('world');
      expect(filters.upper(null)).toBe('');
      expect(filters.lower(undefined)).toBe('');
    });

    test('case conversions', () => {
      expect(filters.camelCase('hello-world')).toBe('helloWorld');
      expect(filters.pascalCase('hello_world')).toBe('HelloWorld');
      expect(filters.kebabCase('HelloWorld')).toBe('hello-world');
      expect(filters.snakeCase('HelloWorld')).toBe('hello_world');
      expect(filters.constantCase('hello world')).toBe('HELLO_WORLD');
    });

    test('string manipulation', () => {
      expect(filters.trim('  hello  ')).toBe('hello');
      expect(filters.replace('hello world', 'world', 'universe')).toBe('hello universe');
      expect(filters.truncate('very long text', 10)).toBe('very long ...');
      expect(filters.reverse('hello')).toBe('olleh');
    });

    test('new string filters', () => {
      expect(filters.quote('test')).toBe('"test"');
      expect(filters.quote('test', 'single')).toBe("'test'");
      expect(filters.unquote('"test"')).toBe('test');
      expect(filters.wrap('hello world test', 10)).toBe('hello\nworld test');
      expect(filters.extname('file.txt')).toBe('.txt');
    });
  });

  describe('Array Filters', () => {
    test('basic array operations', () => {
      expect(filters.join(['a', 'b', 'c'])).toBe('a,b,c');
      expect(filters.join(['a', 'b', 'c'], ' | ')).toBe('a | b | c');
      expect(filters.split('a,b,c')).toEqual(['a', 'b', 'c']);
      expect(filters.unique([1, 2, 2, 3])).toEqual([1, 2, 3]);
    });

    test('array analysis', () => {
      expect(filters.length([1, 2, 3])).toBe(3);
      expect(filters.first([1, 2, 3])).toBe(1);
      expect(filters.last([1, 2, 3])).toBe(3);
      expect(filters.isEmpty([])).toBe(true);
      expect(filters.contains([1, 2, 3], 2)).toBe(true);
    });

    test('advanced array operations', () => {
      expect(filters.zip([1, 2], [3, 4])).toEqual([[1, 3], [2, 4]]);
      expect(filters.unzip([[1, 3], [2, 4]])).toEqual([[1, 2], [3, 4]]);
      expect(filters.min([3, 1, 4, 1, 5])).toBe(1);
      expect(filters.max([3, 1, 4, 1, 5])).toBe(5);
      expect(filters.sum([1, 2, 3, 4])).toBe(10);
      expect(filters.avg([2, 4, 6])).toBe(4);
    });

    test('matrix operations', () => {
      const matrix = [[1, 2], [3, 4], [5, 6]];
      const transposed = [[1, 3, 5], [2, 4, 6]];
      expect(filters.transpose(matrix)).toEqual(transposed);
    });
  });

  describe('Data Transformation Filters', () => {
    test('JSON operations', () => {
      const obj = { name: 'test', value: 123 };
      const jsonStr = filters.json(obj);
      expect(jsonStr).toBe(JSON.stringify(obj, null, 2));
      expect(filters.parseJson(jsonStr)).toEqual(obj);
      expect(filters.parseJson('invalid json')).toBe(null);
    });

    test('type checking', () => {
      expect(filters.typeOf('string')).toBe('string');
      expect(filters.typeOf(123)).toBe('number');
      expect(filters.typeOf([])).toBe('array');
      expect(filters.typeOf({})).toBe('object');
      expect(filters.typeOf(null)).toBe('null');
    });

    test('value conversion', () => {
      expect(filters.toString(123)).toBe('123');
      expect(filters.toNumber('123')).toBe(123);
      expect(filters.toInt('123.45')).toBe(123);
      expect(filters.toFloat('123.45')).toBe(123.45);
      expect(filters.toBoolean('true')).toBe(true);
      expect(filters.toBoolean('false')).toBe(false);
    });

    test('numeric operations', () => {
      expect(filters.round(3.14159, 2)).toBe(3.14);
      expect(filters.ceil(3.14)).toBe(4);
      expect(filters.floor(3.14)).toBe(3);
      expect(filters.abs(-5)).toBe(5);
    });

    test('object manipulation', () => {
      const obj = { a: 1, b: 2, c: 3 };
      expect(filters.keys(obj)).toEqual(['a', 'b', 'c']);
      expect(filters.values(obj)).toEqual([1, 2, 3]);
      expect(filters.pick(obj, 'a', 'c')).toEqual({ a: 1, c: 3 });
      expect(filters.omit(obj, 'b')).toEqual({ a: 1, c: 3 });
    });
  });

  describe('Date/Time Filters', () => {
    test('date formatting', () => {
      const date = '2024-01-15T10:30:00.000Z';
      expect(filters.formatDate(date, 'YYYY-MM-DD')).toBe('2024-01-15');
      expect(filters.formatTime(date, 'HH:mm:ss')).toBe('10:30:00');
    });

    test('deterministic date behavior', () => {
      const staticDate = deterministicFilters.formatDate(new Date(), 'YYYY-MM-DD');
      expect(staticDate).toBe('2024-01-01');

      const staticTime = deterministicFilters.timestamp();
      expect(staticTime).toBe('2024-01-01T00:00:00.000Z');
    });

    test('date arithmetic', () => {
      const baseDate = '2024-01-01T00:00:00.000Z';
      const nextDay = filters.dateAdd(baseDate, 1, 'day');
      const prevDay = filters.dateSub(baseDate, 1, 'day');

      expect(nextDay).toBe('2024-01-02T00:00:00.000Z');
      expect(prevDay).toBe('2023-12-31T00:00:00.000Z');
    });
  });

  describe('Path Utilities', () => {
    test('file path operations', () => {
      expect(filters.filename('/path/to/file.txt')).toBe('file.txt');
      expect(filters.basename('/path/to/file.txt')).toBe('file');
      expect(filters.dirname('/path/to/file.txt')).toBe('/path/to');
      expect(filters.extname('/path/to/file.txt')).toBe('.txt');
    });

    test('path resolution', () => {
      expect(filters.resolve('/base', 'relative')).toBe('/base/relative');
      expect(filters.resolve('/base', '../up')).toBe('/up');
      expect(filters.relative('/base/path', '/base/other')).toBe('../other');
    });
  });

  describe('RDF/Semantic Filters', () => {
    test('CURIE expansion', () => {
      expect(filters.expand('rdf:type')).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      expect(filters.contract('http://www.w3.org/1999/02/22-rdf-syntax-ns#type')).toBe('rdf:type');
    });

    test('RDF literal formatting', () => {
      expect(filters.rdfLiteral('test')).toBe('"test"');
      expect(filters.rdfLiteral('test', 'en')).toBe('"test"@en');
      expect(filters.rdfLiteral('123', 'xsd:int')).toBe('"123"^^xsd:int');
    });

    test('SPARQL utilities', () => {
      expect(filters.sparqlVar('name')).toBe('?name');
      expect(filters.sparqlVar('?name')).toBe('?name');
      expect(filters.blankNode('test')).toBe('_:test');
    });
  });

  describe('Deterministic Behavior', () => {
    test('blocks non-deterministic operations', () => {
      expect(() => deterministicFilters.now()).toThrow('not allowed in deterministic mode');
      expect(() => deterministicFilters.random()).toThrow('not allowed in deterministic mode');
      expect(() => deterministicFilters.uuid()).toThrow('not allowed in deterministic mode');
    });

    test('provides consistent hash-based operations', () => {
      const content = 'test content';
      const hash1 = filters.hash(content);
      const hash2 = filters.hash(content);
      expect(hash1).toBe(hash2);

      const shortHash1 = filters.shortHash(content, 8);
      const shortHash2 = filters.shortHash(content, 8);
      expect(shortHash1).toBe(shortHash2);
      expect(shortHash1).toHaveLength(8);
    });

    test('deterministic sample selection', () => {
      const array = [1, 2, 3, 4, 5];
      const sample1 = filters.sample(array);
      const sample2 = filters.sample(array);
      expect(sample1).toBe(sample2); // Same array content = same sample
    });
  });

  describe('Data Export Filters', () => {
    test('CSV export', () => {
      const data = [
        { name: 'John', age: 30 },
        { name: 'Jane', age: 25 }
      ];

      const csv = filters.csv(data);
      const lines = csv.split('\n');
      expect(lines[0]).toBe('"name","age"');
      expect(lines[1]).toBe('John,30');
      expect(lines[2]).toBe('Jane,25');
    });

    test('Markdown table export', () => {
      const data = [
        { name: 'John', age: 30 },
        { name: 'Jane', age: 25 }
      ];

      const markdown = filters.markdown(data);
      const lines = markdown.split('\n');
      expect(lines[0]).toBe('| name | age |');
      expect(lines[1]).toBe('| --- | --- |');
      expect(lines[2]).toBe('| John | 30 |');
      expect(lines[3]).toBe('| Jane | 25 |');
    });
  });

  describe('Error Handling & Edge Cases', () => {
    test('handles null/undefined inputs gracefully', () => {
      expect(filters.upper(null)).toBe('');
      expect(filters.join(null)).toBe('');
      expect(filters.length(undefined)).toBe(0);
      expect(filters.default(null, 'fallback')).toBe('fallback');
    });

    test('handles invalid inputs gracefully', () => {
      expect(filters.toNumber('invalid', 42)).toBe(42);
      expect(filters.parseJson('invalid')).toBe(null);
      expect(filters.formatDate('invalid')).toBe('');
    });

    test('validation filters throw appropriately', () => {
      expect(() => filters.required('')).toThrow('Value is required');
      expect(filters.required('valid')).toBe('valid');
    });
  });

  describe('Performance & Memory', () => {
    test('handles large arrays efficiently', () => {
      const largeArray = Array.from({ length: 10000 }, (_, i) => i);
      const startTime = Date.now();

      const result = filters.join(largeArray, ',');
      const endTime = Date.now();

      expect(result.split(',').length).toBe(10000);
      expect(endTime - startTime).toBeLessThan(100); // Should be very fast
    });

    test('memory-efficient string operations', () => {
      const longString = 'x'.repeat(100000);
      const result = filters.upper(longString);
      expect(result.length).toBe(100000);
      expect(result.charAt(0)).toBe('X');
    });
  });
});