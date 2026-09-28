import { describe, it, expect } from 'vitest';
import { bindSparqlVariables, toSparqlTerm, escapeLiteral } from './sparql-bind.mjs';

describe('bindSparqlVariables', () => {
  it('binds strings as escaped literals, so quotes cannot break out (injection regression)', () => {
    const q = 'SELECT ?s WHERE { ?s <http://xmlns.com/foaf/0.1/name> $name }';
    const evil = '" . ?s a <http://xmlns.com/foaf/0.1/Person> } #';
    const bound = bindSparqlVariables(q, { name: evil });
    // The old implementation (replaceAll) produced the raw text below, changing the query structure
    expect(bound).not.toContain('name> " . ?s a <http://xmlns.com/foaf/0.1/Person>');
    expect(bound).toContain('\\" . ?s a <http://xmlns.com/foaf/0.1/Person> } #');
    expect(bound.match(/(?<!\\)"/g)).toHaveLength(2); // only the delimiting quotes are unescaped
  });

  it('binds IRIs only through the explicit { iri } form and validates them', () => {
    expect(bindSparqlVariables('ASK { $s ?p ?o }', { s: { iri: 'http://example.org/a' } })).toBe(
      'ASK { <http://example.org/a> ?p ?o }'
    );
    expect(() => bindSparqlVariables('ASK { $s ?p ?o }', { s: { iri: 'http://x> ?y <http://z' } })).toThrow(
      /Invalid IRI/
    );
  });

  it('binds numbers and booleans as typed lexical forms and rejects other types', () => {
    expect(toSparqlTerm(42)).toBe('42');
    expect(toSparqlTerm(-1.5)).toBe('-1.5');
    expect(toSparqlTerm(true)).toBe('true');
    expect(() => toSparqlTerm(NaN)).toThrow();
    expect(() => toSparqlTerm(null)).toThrow();
    expect(() => toSparqlTerm([1])).toThrow();
  });

  it('matches whole placeholder names: $s does not corrupt $street', () => {
    const bound = bindSparqlVariables('$s $street', { s: 'a' });
    expect(bound).toBe('"a" $street');
  });

  it('does not interpret $-patterns inside substituted values', () => {
    expect(bindSparqlVariables('$x', { x: "$& $1 $$" })).toBe('"$& $1 $$"');
  });

  it('leaves unknown placeholders untouched and rejects invalid names', () => {
    expect(bindSparqlVariables('$known $unknown', { known: 1 })).toBe('1 $unknown');
    expect(() => bindSparqlVariables('x', { 'a b': 1 })).toThrow(/Invalid variable name/);
    expect(() => bindSparqlVariables(5, {})).toThrow(TypeError);
  });

  it('escapes control characters in literals', () => {
    expect(escapeLiteral('a\\b"c\nd\re\tf')).toBe('a\\\\b\\"c\\nd\\re\\tf');
  });
});
