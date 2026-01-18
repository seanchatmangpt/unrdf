/**
 * @file Filter Tests
 * @description Tests for standard and RDF filters
 */

import { describe, it, expect } from 'vitest';
import {
  upper,
  lower,
  capitalize,
  truncate,
  join,
  length,
  first,
  last,
  reverse,
  replace,
  localname,
  namespace,
  prefixedName,
  ntriples,
  turtle,
  subjects,
  predicates,
  objects,
  filterByPredicate,
  getStandardFilters,
  getRdfFilters,
  getAllFilters,
} from '../src/filters.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, blankNode } = dataFactory;

describe('Standard Filters', () => {
  describe('upper', () => {
    it('should convert to uppercase', () => {
      expect(upper('hello')).toBe('HELLO');
      expect(upper('World')).toBe('WORLD');
    });
  });

  describe('lower', () => {
    it('should convert to lowercase', () => {
      expect(lower('HELLO')).toBe('hello');
      expect(lower('World')).toBe('world');
    });
  });

  describe('capitalize', () => {
    it('should capitalize first letter', () => {
      expect(capitalize('hello')).toBe('Hello');
      expect(capitalize('WORLD')).toBe('World');
    });
  });

  describe('truncate', () => {
    it('should truncate long strings', () => {
      const long = 'a'.repeat(300);
      const result = truncate(long, 10);
      expect(result).toBe('aaaaaaaaaa...');
      expect(result.length).toBe(13);
    });

    it('should not truncate short strings', () => {
      expect(truncate('short', 10)).toBe('short');
    });

    it('should use default length', () => {
      const long = 'a'.repeat(300);
      const result = truncate(long);
      expect(result.length).toBe(258); // 255 + '...'
    });
  });

  describe('join', () => {
    it('should join array with separator', () => {
      expect(join(['a', 'b', 'c'], ', ')).toBe('a, b, c');
      expect(join(['x', 'y'], '-')).toBe('x-y');
    });

    it('should use default separator', () => {
      expect(join(['a', 'b', 'c'])).toBe('a, b, c');
    });

    it('should handle non-arrays', () => {
      expect(join('test')).toBe('test');
    });
  });

  describe('length', () => {
    it('should return array length', () => {
      expect(length([1, 2, 3])).toBe(3);
      expect(length([])).toBe(0);
    });

    it('should return string length', () => {
      expect(length('hello')).toBe(5);
      expect(length('')).toBe(0);
    });

    it('should return object key count', () => {
      expect(length({ a: 1, b: 2 })).toBe(2);
    });

    it('should return 0 for invalid input', () => {
      expect(length(null)).toBe(0);
      expect(length(undefined)).toBe(0);
      expect(length(42)).toBe(0);
    });
  });

  describe('first', () => {
    it('should return first element', () => {
      expect(first([1, 2, 3])).toBe(1);
      expect(first(['a'])).toBe('a');
    });

    it('should return undefined for empty array', () => {
      expect(first([])).toBeUndefined();
    });

    it('should return value for non-array', () => {
      expect(first('test')).toBe('test');
    });
  });

  describe('last', () => {
    it('should return last element', () => {
      expect(last([1, 2, 3])).toBe(3);
      expect(last(['a'])).toBe('a');
    });

    it('should return undefined for empty array', () => {
      expect(last([])).toBeUndefined();
    });
  });

  describe('reverse', () => {
    it('should reverse array', () => {
      expect(reverse([1, 2, 3])).toEqual([3, 2, 1]);
      expect(reverse(['a', 'b'])).toEqual(['b', 'a']);
    });

    it('should reverse string', () => {
      expect(reverse('hello')).toBe('olleh');
    });

    it('should not mutate original', () => {
      const original = [1, 2, 3];
      reverse(original);
      expect(original).toEqual([1, 2, 3]);
    });
  });

  describe('replace', () => {
    it('should replace all occurrences', () => {
      expect(replace('hello world', 'o', 'a')).toBe('hella warld');
    });

    it('should handle regex special chars', () => {
      expect(replace('a.b.c', '.', '-')).toBe('a-b-c');
    });

    it('should use empty string as default replacement', () => {
      expect(replace('hello', 'l')).toBe('heo');
    });
  });
});

describe('RDF Filters', () => {
  describe('localname', () => {
    it('should extract local name from URI with hash', () => {
      expect(localname('http://example.org/ns#Person')).toBe('Person');
    });

    it('should extract local name from URI with slash', () => {
      expect(localname('http://example.org/ns/Person')).toBe('Person');
    });

    it('should return original if no separator', () => {
      expect(localname('Person')).toBe('Person');
    });

    it('should prefer hash over slash', () => {
      expect(localname('http://example.org/ns/prefix#Person')).toBe('Person');
    });
  });

  describe('namespace', () => {
    it('should extract namespace from URI with hash', () => {
      expect(namespace('http://example.org/ns#Person')).toBe('http://example.org/ns#');
    });

    it('should extract namespace from URI with slash', () => {
      expect(namespace('http://example.org/ns/Person')).toBe('http://example.org/ns/');
    });

    it('should return empty string if no separator', () => {
      expect(namespace('Person')).toBe('');
    });
  });

  describe('prefixedName', () => {
    it('should convert URI to prefixed name', () => {
      const prefixes = {
        rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        ex: 'http://example.org/',
      };

      expect(prefixedName('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', prefixes))
        .toBe('rdf:type');
      expect(prefixedName('http://example.org/Person', prefixes))
        .toBe('ex:Person');
    });

    it('should return URI if no matching prefix', () => {
      expect(prefixedName('http://unknown.org/term', {}))
        .toBe('http://unknown.org/term');
    });
  });

  describe('ntriples', () => {
    it('should format NamedNode', () => {
      const node = namedNode('http://example.org/Person');
      expect(ntriples(node)).toBe('<http://example.org/Person>');
    });

    it('should format Literal', () => {
      const lit = literal('hello');
      expect(ntriples(lit)).toBe('"hello"');
    });

    it('should format Literal with language', () => {
      const lit = literal('hello', 'en');
      expect(ntriples(lit)).toBe('"hello"@en');
    });

    it('should format Literal with datatype', () => {
      const lit = literal('42', namedNode('http://www.w3.org/2001/XMLSchema#integer'));
      expect(ntriples(lit)).toBe('"42"^^<http://www.w3.org/2001/XMLSchema#integer>');
    });

    it('should format BlankNode', () => {
      const blank = blankNode('b1');
      expect(ntriples(blank)).toBe('_:b1');
    });

    it('should handle non-RDF terms', () => {
      expect(ntriples('test')).toBe('test');
      expect(ntriples(42)).toBe('42');
    });

    it('should escape quotes in literals', () => {
      const lit = literal('say "hello"');
      expect(ntriples(lit)).toBe('"say \\"hello\\""');
    });
  });

  describe('turtle', () => {
    it('should format NamedNode with prefix', () => {
      const node = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#' };
      expect(turtle(node, prefixes)).toBe('rdf:type');
    });

    it('should format Literal', () => {
      const lit = literal('test');
      expect(turtle(lit)).toBe('"test"');
    });
  });

  describe('subjects', () => {
    it('should extract unique subjects', () => {
      const triples = [
        {
          subject: namedNode('http://example.org/s1'),
          predicate: namedNode('http://example.org/p'),
          object: literal('o1'),
        },
        {
          subject: namedNode('http://example.org/s2'),
          predicate: namedNode('http://example.org/p'),
          object: literal('o2'),
        },
        {
          subject: namedNode('http://example.org/s1'),
          predicate: namedNode('http://example.org/p2'),
          object: literal('o3'),
        },
      ];

      const result = subjects(triples);
      expect(result).toHaveLength(2);
      expect(result).toContain('http://example.org/s1');
      expect(result).toContain('http://example.org/s2');
    });

    it('should handle empty array', () => {
      expect(subjects([])).toEqual([]);
    });

    it('should handle non-arrays', () => {
      expect(subjects('invalid')).toEqual([]);
    });
  });

  describe('predicates', () => {
    it('should extract unique predicates', () => {
      const triples = [
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p1'),
          object: literal('o1'),
        },
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p2'),
          object: literal('o2'),
        },
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p1'),
          object: literal('o3'),
        },
      ];

      const result = predicates(triples);
      expect(result).toHaveLength(2);
      expect(result).toContain('http://example.org/p1');
      expect(result).toContain('http://example.org/p2');
    });
  });

  describe('objects', () => {
    it('should extract unique objects', () => {
      const triples = [
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('o1'),
        },
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('o2'),
        },
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('o1'),
        },
      ];

      const result = objects(triples);
      expect(result).toHaveLength(2);
      expect(result).toContain('o1');
      expect(result).toContain('o2');
    });
  });

  describe('filterByPredicate', () => {
    it('should filter triples by predicate', () => {
      const triples = [
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p1'),
          object: literal('o1'),
        },
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p2'),
          object: literal('o2'),
        },
      ];

      const result = filterByPredicate(triples, 'http://example.org/p1');
      expect(result).toHaveLength(1);
      expect(result[0].object.value).toBe('o1');
    });

    it('should return empty array if no matches', () => {
      const triples = [
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p1'),
          object: literal('o1'),
        },
      ];

      const result = filterByPredicate(triples, 'http://example.org/p2');
      expect(result).toEqual([]);
    });
  });
});

describe('Filter Collections', () => {
  describe('getStandardFilters', () => {
    it('should return all standard filters', () => {
      const filters = getStandardFilters();
      expect(filters).toHaveProperty('upper');
      expect(filters).toHaveProperty('lower');
      expect(filters).toHaveProperty('capitalize');
      expect(filters).toHaveProperty('truncate');
      expect(Object.keys(filters)).toHaveLength(10);
    });
  });

  describe('getRdfFilters', () => {
    it('should return all RDF filters', () => {
      const filters = getRdfFilters();
      expect(filters).toHaveProperty('localname');
      expect(filters).toHaveProperty('namespace');
      expect(filters).toHaveProperty('ntriples');
      expect(filters).toHaveProperty('subjects');
      expect(Object.keys(filters)).toHaveLength(9);
    });
  });

  describe('getAllFilters', () => {
    it('should return all filters', () => {
      const filters = getAllFilters();
      expect(filters).toHaveProperty('upper');
      expect(filters).toHaveProperty('localname');
      expect(Object.keys(filters)).toHaveLength(19);
    });
  });
});
