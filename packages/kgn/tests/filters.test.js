/**
 * KGEN Filter Tests - Comprehensive Test Suite
 *
 * Tests for all P0 filters with 100% coverage
 * Following London BDD approach with deterministic behavior
 */

import { describe, test, expect, beforeEach } from 'vitest';
import { textFilters } from '../src/filters/text.js';
import { arrayFilters } from '../src/filters/array.js';
import { dataFilters } from '../src/filters/data.js';
import { rdfFilters } from '../src/filters/rdf.js';
import { createCustomFilters } from '../src/filters/index.js';

describe('KGEN Filter System', () => {
  describe('Text Filters', () => {
    describe('Case Conversion', () => {
      test('upper converts to uppercase deterministically', () => {
        expect(textFilters.upper('hello')).toBe('HELLO');
        expect(textFilters.upper('HELLO')).toBe('HELLO');
        expect(textFilters.upper('')).toBe('');
        expect(textFilters.upper(null)).toBe('');
        expect(textFilters.upper(undefined)).toBe('');
        expect(textFilters.upper(123)).toBe('123');
      });

      test('lower converts to lowercase deterministically', () => {
        expect(textFilters.lower('HELLO')).toBe('hello');
        expect(textFilters.lower('hello')).toBe('hello');
        expect(textFilters.lower('')).toBe('');
        expect(textFilters.lower(null)).toBe('');
        expect(textFilters.lower(undefined)).toBe('');
        expect(textFilters.lower(123)).toBe('123');
      });

      test('camelCase converts properly', () => {
        expect(textFilters.camelCase('hello world')).toBe('helloWorld');
        expect(textFilters.camelCase('hello-world')).toBe('helloWorld');
        expect(textFilters.camelCase('hello_world')).toBe('helloWorld');
        expect(textFilters.camelCase('HELLO WORLD')).toBe('HELLOWORLD');
        expect(textFilters.camelCase('')).toBe('');
        expect(textFilters.camelCase(null)).toBe('');
      });

      test('pascalCase converts properly', () => {
        expect(textFilters.pascalCase('hello world')).toBe('HelloWorld');
        expect(textFilters.pascalCase('hello-world')).toBe('HelloWorld');
        expect(textFilters.pascalCase('hello_world')).toBe('HelloWorld');
        expect(textFilters.pascalCase('')).toBe('');
        expect(textFilters.pascalCase(null)).toBe('');
      });

      test('kebabCase converts properly', () => {
        expect(textFilters.kebabCase('helloWorld')).toBe('hello-world');
        expect(textFilters.kebabCase('hello world')).toBe('hello-world');
        expect(textFilters.kebabCase('hello_world')).toBe('hello-world');
        expect(textFilters.kebabCase('')).toBe('');
        expect(textFilters.kebabCase(null)).toBe('');
      });

      test('snakeCase converts properly', () => {
        expect(textFilters.snakeCase('helloWorld')).toBe('hello_world');
        expect(textFilters.snakeCase('hello world')).toBe('hello_world');
        expect(textFilters.snakeCase('hello-world')).toBe('hello_world');
        expect(textFilters.snakeCase('')).toBe('');
        expect(textFilters.snakeCase(null)).toBe('');
      });

      test('constantCase converts properly', () => {
        expect(textFilters.constantCase('helloWorld')).toBe('HELLO_WORLD');
        expect(textFilters.constantCase('hello world')).toBe('HELLO_WORLD');
        expect(textFilters.constantCase('')).toBe('');
      });
    });

    describe('Text Manipulation', () => {
      test('trim removes whitespace', () => {
        expect(textFilters.trim('  hello  ')).toBe('hello');
        expect(textFilters.trim('')).toBe('');
        expect(textFilters.trim(null)).toBe('');
        expect(textFilters.trim(undefined)).toBe('');
      });

      test('replace replaces all occurrences', () => {
        expect(textFilters.replace('hello world hello', 'hello', 'hi')).toBe('hi world hi');
        expect(textFilters.replace('hello', 'hello', 'hi')).toBe('hi');
        expect(textFilters.replace('hello', null, 'hi')).toBe('hello');
        expect(textFilters.replace(null, 'hello', 'hi')).toBe('');
      });

      test('replaceFirst replaces first occurrence only', () => {
        expect(textFilters.replaceFirst('hello world hello', 'hello', 'hi')).toBe('hi world hello');
        expect(textFilters.replaceFirst('hello', 'hello', 'hi')).toBe('hi');
        expect(textFilters.replaceFirst(null, 'hello', 'hi')).toBe('');
      });

      test('truncate limits text length', () => {
        expect(textFilters.truncate('hello world', 5)).toBe('hello...');
        expect(textFilters.truncate('hello', 10)).toBe('hello');
        expect(textFilters.truncate('', 5)).toBe('');
        expect(textFilters.truncate(null, 5)).toBe('');
      });

      test('slug creates URL-friendly strings', () => {
        expect(textFilters.slug('Hello World!')).toBe('hello-world');
        expect(textFilters.slug('  Hello   World  ')).toBe('hello-world');
        expect(textFilters.slug('Hello@#$World')).toBe('helloworld');
        expect(textFilters.slug('')).toBe('');
        expect(textFilters.slug(null)).toBe('');
      });
    });

    describe('Formatting', () => {
      test('title case works correctly', () => {
        expect(textFilters.title('hello world')).toBe('Hello World');
        expect(textFilters.title('HELLO WORLD')).toBe('Hello World');
        expect(textFilters.title('')).toBe('');
        expect(textFilters.title(null)).toBe('');
      });

      test('capitalize works correctly', () => {
        expect(textFilters.capitalize('hello')).toBe('Hello');
        expect(textFilters.capitalize('HELLO')).toBe('HELLO');
        expect(textFilters.capitalize('')).toBe('');
        expect(textFilters.capitalize(null)).toBe('');
      });

      test('escape handles HTML characters', () => {
        expect(textFilters.escape('<script>alert("xss")</script>'))
          .toBe('&lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;');
        expect(textFilters.escape('&')).toBe('&amp;');
        expect(textFilters.escape('')).toBe('');
        expect(textFilters.escape(null)).toBe('');
      });
    });

    describe('Utility Functions', () => {
      test('reverse reverses strings', () => {
        expect(textFilters.reverse('hello')).toBe('olleh');
        expect(textFilters.reverse('')).toBe('');
        expect(textFilters.reverse(null)).toBe('');
      });

      test('wordCount counts words', () => {
        expect(textFilters.wordCount('hello world')).toBe(2);
        expect(textFilters.wordCount('  hello   world  ')).toBe(2);
        expect(textFilters.wordCount('')).toBe(0);
        expect(textFilters.wordCount(null)).toBe(0);
      });

      test('length returns string length', () => {
        expect(textFilters.length('hello')).toBe(5);
        expect(textFilters.length('')).toBe(0);
        expect(textFilters.length(null)).toBe(0);
        expect(textFilters.length(undefined)).toBe(0);
      });
    });
  });

  describe('Array Filters', () => {
    describe('Basic Operations', () => {
      test('join combines array elements', () => {
        expect(arrayFilters.join(['a', 'b', 'c'])).toBe('a,b,c');
        expect(arrayFilters.join(['a', 'b', 'c'], '|')).toBe('a|b|c');
        expect(arrayFilters.join([])).toBe('');
        expect(arrayFilters.join(null)).toBe('');
        expect(arrayFilters.join('hello')).toBe('hello');
      });

      test('split creates arrays from strings', () => {
        expect(arrayFilters.split('a,b,c')).toEqual(['a', 'b', 'c']);
        expect(arrayFilters.split('a|b|c', '|')).toEqual(['a', 'b', 'c']);
        expect(arrayFilters.split('')).toEqual(['']);
        expect(arrayFilters.split(null)).toEqual([]);
        expect(arrayFilters.split(undefined)).toEqual([]);
      });

      test('unique removes duplicates deterministically', () => {
        expect(arrayFilters.unique([1, 2, 2, 3])).toEqual([1, 2, 3]);
        expect(arrayFilters.unique(['a', 'b', 'a'])).toEqual(['a', 'b']);
        expect(arrayFilters.unique([])).toEqual([]);
        expect(arrayFilters.unique(null)).toBe(null);
      });

      test('sort orders elements deterministically', () => {
        expect(arrayFilters.sort([3, 1, 2])).toEqual([1, 2, 3]); // Numbers stay as numbers after sort
        expect(arrayFilters.sort(['c', 'a', 'b'])).toEqual(['a', 'b', 'c']);
        expect(arrayFilters.sort([])).toEqual([]);
        expect(arrayFilters.sort(null)).toBe(null);
      });
    });

    describe('Array Manipulation', () => {
      test('reverse reverses arrays', () => {
        expect(arrayFilters.reverse([1, 2, 3])).toEqual([3, 2, 1]);
        expect(arrayFilters.reverse([])).toEqual([]);
        expect(arrayFilters.reverse(null)).toBe(null);
      });

      test('first gets first elements', () => {
        expect(arrayFilters.first([1, 2, 3])).toBe(1);
        expect(arrayFilters.first([1, 2, 3], 2)).toEqual([1, 2]);
        expect(arrayFilters.first([])).toBeUndefined();
        expect(arrayFilters.first(null)).toBe(null);
      });

      test('last gets last elements', () => {
        expect(arrayFilters.last([1, 2, 3])).toBe(3);
        expect(arrayFilters.last([1, 2, 3], 2)).toEqual([2, 3]);
        expect(arrayFilters.last([])).toBeUndefined();
        expect(arrayFilters.last(null)).toBe(null);
      });

      test('chunk splits arrays into groups', () => {
        expect(arrayFilters.chunk([1, 2, 3, 4, 5], 2)).toEqual([[1, 2], [3, 4], [5]]);
        expect(arrayFilters.chunk([1, 2, 3], 5)).toEqual([[1, 2, 3]]);
        expect(arrayFilters.chunk([], 2)).toEqual([]);
        expect(arrayFilters.chunk(null, 2)).toEqual([]);
      });

      test('flatten flattens nested arrays', () => {
        expect(arrayFilters.flatten([[1, 2], [3, 4]])).toEqual([1, 2, 3, 4]);
        expect(arrayFilters.flatten([1, [2, [3, 4]]])).toEqual([1, 2, 3, 4]);
        expect(arrayFilters.flatten([])).toEqual([]);
        expect(arrayFilters.flatten(null)).toBe(null);
      });
    });

    describe('Advanced Operations', () => {
      test('groupBy groups by property', () => {
        const items = [
          { category: 'A', value: 1 },
          { category: 'B', value: 2 },
          { category: 'A', value: 3 }
        ];
        const grouped = arrayFilters.groupBy(items, 'category');
        expect(grouped.A).toHaveLength(2);
        expect(grouped.B).toHaveLength(1);
        expect(arrayFilters.groupBy(null, 'key')).toEqual({});
      });

      test('sortBy sorts by property deterministically', () => {
        const items = [
          { name: 'Charlie', age: 30 },
          { name: 'Alice', age: 25 },
          { name: 'Bob', age: 35 }
        ];
        const sorted = arrayFilters.sortBy(items, 'name');
        expect(sorted[0].name).toBe('Alice');
        expect(sorted[1].name).toBe('Bob');
        expect(sorted[2].name).toBe('Charlie');
      });

      test('sample returns deterministic "random" element', () => {
        const arr = ['a', 'b', 'c'];
        const result1 = arrayFilters.sample(arr);
        const result2 = arrayFilters.sample(arr);
        expect(result1).toBe(result2); // Should be deterministic
        expect(arr).toContain(result1);
        expect(arrayFilters.sample([])).toBe(null);
        expect(arrayFilters.sample(null)).toBe(null);
      });
    });
  });

  describe('Data Filters', () => {
    describe('JSON Operations', () => {
      test('json converts to JSON string', () => {
        expect(dataFilters.json({ name: 'test' })).toBe('{\n  \"name\": \"test\"\n}');
        expect(dataFilters.json([1, 2, 3])).toBe('[\n  1,\n  2,\n  3\n]');
        expect(dataFilters.json('string')).toBe('\"string\"');
        expect(dataFilters.json(null)).toBe('null');
      });

      test('json handles complex objects', () => {
        const func = () => 'test';
        const obj = { func, symbol: Symbol('test'), bigint: BigInt(123) };
        const result = dataFilters.json(obj);
        expect(result).toContain('[Function]');
        expect(result).toContain('[Symbol]');
        expect(result).toContain('[BigInt: 123]');
      });

      test('parseJson parses JSON strings', () => {
        expect(dataFilters.parseJson('{"name":"test"}')).toEqual({ name: 'test' });
        expect(dataFilters.parseJson('[1,2,3]')).toEqual([1, 2, 3]);
        expect(dataFilters.parseJson('invalid')).toBe(null);
        expect(dataFilters.parseJson('')).toBe(null);
      });
    });

    describe('Default Values', () => {
      test('default provides fallback values', () => {
        expect(dataFilters.default('value', 'fallback')).toBe('value');
        expect(dataFilters.default(null, 'fallback')).toBe('fallback');
        expect(dataFilters.default(undefined, 'fallback')).toBe('fallback');
        expect(dataFilters.default('', 'fallback')).toBe('fallback');
        expect(dataFilters.default([], 'fallback')).toBe('fallback');
        expect(dataFilters.default({}, 'fallback')).toBe('fallback');
      });
    });

    describe('Type Checking', () => {
      test('typeOf returns correct types', () => {
        expect(dataFilters.typeOf(null)).toBe('null');
        expect(dataFilters.typeOf([])).toBe('array');
        expect(dataFilters.typeOf(new Date())).toBe('date');
        expect(dataFilters.typeOf(/regex/)).toBe('regexp');
        expect(dataFilters.typeOf('string')).toBe('string');
        expect(dataFilters.typeOf(123)).toBe('number');
        expect(dataFilters.typeOf({})).toBe('object');
      });

      test('isEmpty checks empty values', () => {
        expect(dataFilters.isEmpty(null)).toBe(true);
        expect(dataFilters.isEmpty(undefined)).toBe(true);
        expect(dataFilters.isEmpty('')).toBe(true);
        expect(dataFilters.isEmpty([])).toBe(true);
        expect(dataFilters.isEmpty({})).toBe(true);
        expect(dataFilters.isEmpty(NaN)).toBe(true);
        expect(dataFilters.isEmpty('value')).toBe(false);
        expect(dataFilters.isEmpty([1])).toBe(false);
        expect(dataFilters.isEmpty({ key: 'value' })).toBe(false);
      });

      test('type checking functions work correctly', () => {
        expect(dataFilters.isString('hello')).toBe(true);
        expect(dataFilters.isString(123)).toBe(false);
        expect(dataFilters.isNumber(123)).toBe(true);
        expect(dataFilters.isNumber(NaN)).toBe(false);
        expect(dataFilters.isArray([])).toBe(true);
        expect(dataFilters.isArray(null)).toBe(false);
        expect(dataFilters.isObject({})).toBe(true);
        expect(dataFilters.isObject([])).toBe(false);
        expect(dataFilters.isBoolean(true)).toBe(true);
        expect(dataFilters.isBoolean('true')).toBe(false);
      });
    });

    describe('Type Conversions', () => {
      test('toNumber converts values to numbers', () => {
        expect(dataFilters.toNumber('123')).toBe(123);
        expect(dataFilters.toNumber('123.45')).toBe(123.45);
        expect(dataFilters.toNumber('invalid')).toBe(0);
        expect(dataFilters.toNumber('invalid', 42)).toBe(42);
        expect(dataFilters.toNumber(null)).toBe(0);
      });

      test('toBoolean converts values to booleans', () => {
        expect(dataFilters.toBoolean(true)).toBe(true);
        expect(dataFilters.toBoolean('true')).toBe(true);
        expect(dataFilters.toBoolean('yes')).toBe(true);
        expect(dataFilters.toBoolean('1')).toBe(true);
        expect(dataFilters.toBoolean('false')).toBe(false);
        expect(dataFilters.toBoolean('')).toBe(false);
        expect(dataFilters.toBoolean(0)).toBe(false);
        expect(dataFilters.toBoolean([])).toBe(false);
        expect(dataFilters.toBoolean([1])).toBe(true);
      });
    });

    describe('Object Operations', () => {
      test('get retrieves nested properties', () => {
        const obj = { user: { profile: { name: 'John' } } };
        expect(dataFilters.get(obj, 'user.profile.name')).toBe('John');
        expect(dataFilters.get(obj, 'user.profile.age', 25)).toBe(25);
        expect(dataFilters.get(obj, 'invalid.path', 'default')).toBe('default');
        expect(dataFilters.get(null, 'path', 'default')).toBe('default');
      });

      test('has checks property existence', () => {
        const obj = { user: { profile: { name: 'John' } } };
        expect(dataFilters.has(obj, 'user.profile.name')).toBe(true);
        expect(dataFilters.has(obj, 'user.profile.age')).toBe(false);
        expect(dataFilters.has(null, 'path')).toBe(false);
      });
    });
  });

  describe('RDF Filters', () => {
    describe('URI Operations', () => {
      test('expand converts CURIEs to full URIs', () => {
        expect(rdfFilters.expand('rdf:type')).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
        expect(rdfFilters.expand('foaf:name')).toBe('http://xmlns.com/foaf/0.1/name');
        expect(rdfFilters.expand('unknown:term')).toBe('unknown:term');
        expect(rdfFilters.expand('not-a-curie')).toBe('not-a-curie');
        expect(rdfFilters.expand('')).toBe('');
        expect(rdfFilters.expand(null)).toBe('');
      });

      test('contract converts URIs to CURIEs', () => {
        expect(rdfFilters.contract('http://www.w3.org/1999/02/22-rdf-syntax-ns#type')).toBe('rdf:type');
        expect(rdfFilters.contract('http://xmlns.com/foaf/0.1/name')).toBe('foaf:name');
        expect(rdfFilters.contract('http://unknown.org/term')).toBe('http://unknown.org/term');
        expect(rdfFilters.contract('')).toBe('');
        expect(rdfFilters.contract(null)).toBe('');
      });
    });

    describe('RDF Literals and Resources', () => {
      test('rdfLiteral creates proper RDF literals', () => {
        expect(rdfFilters.rdfLiteral('hello')).toBe('\"hello\"');
        expect(rdfFilters.rdfLiteral('hello', 'en')).toBe('\"hello\"@en');
        expect(rdfFilters.rdfLiteral('123', 'xsd:integer')).toBe('\"123\"^^xsd:integer');
        expect(rdfFilters.rdfLiteral('with \"quotes\"')).toBe('\"with \\\"quotes\\\"\"');
        expect(rdfFilters.rdfLiteral(null)).toBe('\"\"');
      });

      test('rdfResource creates URI references', () => {
        expect(rdfFilters.rdfResource('http://example.org/test')).toBe('<http://example.org/test>');
        expect(rdfFilters.rdfResource('ex:test')).toBe('ex:test'); // CURIE unchanged
        expect(rdfFilters.rdfResource('<already-wrapped>')).toBe('<already-wrapped>');
        expect(rdfFilters.rdfResource('')).toBe('<>');
        expect(rdfFilters.rdfResource(null)).toBe('<>');
      });

      test('turtleEscape escapes special characters', () => {
        expect(rdfFilters.turtleEscape('hello\\world')).toBe('hello\\\\world');
        expect(rdfFilters.turtleEscape('hello\"world')).toBe('hello\\\"world');
        expect(rdfFilters.turtleEscape('hello\\nworld')).toBe('hello\\\\nworld');
        expect(rdfFilters.turtleEscape('')).toBe('');
        expect(rdfFilters.turtleEscape(null)).toBe('');
      });
    });

    describe('SPARQL Operations', () => {
      test('sparqlVar creates SPARQL variables', () => {
        expect(rdfFilters.sparqlVar('name')).toBe('?name');
        expect(rdfFilters.sparqlVar('?name')).toBe('?name'); // Already has prefix
        expect(rdfFilters.sparqlVar('$name')).toBe('?name'); // Convert $ to ?
        expect(rdfFilters.sparqlVar('invalid-chars!')).toBe('?invalid_chars_');
        expect(rdfFilters.sparqlVar('')).toBe('?var');
        expect(rdfFilters.sparqlVar(null)).toBe('?var');
      });

      test('sparql returns mock results deterministically', async () => {
        const result1 = await rdfFilters.sparql('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
        const result2 = await rdfFilters.sparql('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
        expect(result1).toEqual(result2); // Deterministic
        expect(Array.isArray(result1)).toBe(true);

        const emptyResult = await rdfFilters.sparql('');
        expect(emptyResult).toEqual([]);
      });
    });

    describe('Utility Functions', () => {
      test('rdfList creates RDF collections', () => {
        expect(rdfFilters.rdfList(['a', 'b', 'c'])).toBe('( a b c )');
        expect(rdfFilters.rdfList([])).toBe('rdf:nil');
        expect(rdfFilters.rdfList(null)).toBe('rdf:nil');
      });

      test('blankNode creates blank node identifiers', () => {
        expect(rdfFilters.blankNode('test')).toBe('_:test');
        expect(rdfFilters.blankNode('invalid-chars!')).toBe('_:invalid_chars_');
        const auto = rdfFilters.blankNode();
        expect(auto).toMatch(/^_:blank\d+$/);
      });

      test('rdfDatatype creates typed literals', () => {
        expect(rdfFilters.rdfDatatype('123', 'integer')).toBe('\"123\"^^xsd:integer');
        expect(rdfFilters.rdfDatatype('true', 'boolean')).toBe('\"true\"^^xsd:boolean');
        expect(rdfFilters.rdfDatatype('hello')).toBe('\"hello\"^^xsd:string');
        expect(rdfFilters.rdfDatatype(null)).toBe('\"\"');
      });
    });
  });

  describe('Integration and Deterministic Mode', () => {
    test('createCustomFilters merges all filter types', () => {
      const filters = createCustomFilters();

      // Check that all filter types are present
      expect(filters.upper).toBeDefined();
      expect(filters.join).toBeDefined();
      expect(filters.json).toBeDefined();
      expect(filters.expand).toBeDefined();
      expect(filters.hash).toBeDefined();
      expect(filters.timestamp).toBeDefined();
    });

    test('deterministic mode affects time-based filters', () => {
      const deterministicFilters = createCustomFilters({ deterministicMode: true });
      const normalFilters = createCustomFilters({ deterministicMode: false });

      // Deterministic filters should return static values
      expect(deterministicFilters.formatDate(new Date())).toBe('2024-01-01');
      expect(deterministicFilters.formatTime(new Date())).toBe('00:00:00');
      expect(deterministicFilters.timestamp()).toBe('2024-01-01T00:00:00.000Z');

      // Non-deterministic filters should throw in deterministic mode
      expect(() => deterministicFilters.now()).toThrow();
      expect(() => deterministicFilters.random()).toThrow();
      expect(() => deterministicFilters.uuid()).toThrow();
    });

    test('hash filter provides deterministic results', () => {
      const filters = createCustomFilters();
      const hash1 = filters.hash('test content');
      const hash2 = filters.hash('test content');
      const hash3 = filters.hash('different content');

      expect(hash1).toBe(hash2); // Same input = same hash
      expect(hash1).not.toBe(hash3); // Different input = different hash
      expect(hash1).toHaveLength(64); // SHA-256 = 64 chars
    });

    test('filters handle edge cases gracefully', () => {
      const filters = createCustomFilters();

      // All filters should handle null/undefined without throwing
      expect(() => filters.upper(null)).not.toThrow();
      expect(() => filters.join(null)).not.toThrow();
      expect(() => filters.json(null)).not.toThrow();
      expect(() => filters.expand(null)).not.toThrow();

      // All filters should return predictable types
      expect(typeof filters.upper(null)).toBe('string');
      expect(typeof filters.join(null)).toBe('string');
      expect(typeof filters.json(null)).toBe('string');
      expect(typeof filters.expand(null)).toBe('string');
    });
  });
});