/**
 * @file SPARQL injection regression tests for rdf-graphql
 * Rule: .claude/rules/scoped/p1-sparql-injection.md - never interpolate untrusted values.
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { createStore } from '@unrdf/oxigraph';
import { createAdapter } from '../src/adapter.mjs';
import { SPARQLQueryBuilder, buildSimpleQuery } from '../src/query-builder.mjs';
import { RDFResolverFactory, createRelationshipResolver } from '../src/resolver.mjs';

const EX = 'http://example.org/test#';

const ONTOLOGY = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <${EX}> .
ex:Person a rdfs:Class .
ex:name a rdf:Property ; rdfs:domain ex:Person ; rdfs:range xsd:string .
ex:age a rdf:Property ; rdfs:domain ex:Person ; rdfs:range xsd:integer .
`;

const DATA = `
@prefix ex: <${EX}> .
<http://example.org/people/alice> a ex:Person ; ex:name "Alice" ; ex:age 30 .
<http://example.org/people/bob> a ex:Person ; ex:name "Bob" ; ex:age 25 .
`;

const emptyInfo = { fieldNodes: [{ selectionSet: { selections: [] } }] };

function makeStore() {
  const store = createStore();
  store.load(DATA, { format: 'text/turtle' });
  return store;
}

describe('SPARQLQueryBuilder filter injection', () => {
  test('numeric comparison operands cannot smuggle SPARQL expressions', () => {
    const qb = new SPARQLQueryBuilder();
    // Without validation this yields FILTER(?age > 100 || true) which matches every row
    assert.throws(() => qb.buildFilters({ age: { gt: '100 || true' } }), /numeric|Invalid/i);
    assert.throws(() => qb.buildFilters({ age: { lt: '1) } # ' } }), /numeric|Invalid/i);
  });

  test('numeric operands still work for real numbers and numeric strings', () => {
    const qb = new SPARQLQueryBuilder();
    assert.match(qb.buildFilters({ age: { gt: 18 } }), /FILTER\(\?age > 18\)/);
    assert.match(qb.buildFilters({ age: { lt: '65.5' } }), /FILTER\(\?age < 65\.5\)/);
  });

  test('filter field names must be plain variable names', () => {
    const qb = new SPARQLQueryBuilder();
    assert.throws(() => qb.buildFilters({ 'age) || true || (?x': 1 }), /Invalid/i);
  });

  test('string filter values stay escaped inside the literal', () => {
    const qb = new SPARQLQueryBuilder();
    const out = qb.buildFilters({ name: 'x") || true || (?name = "' });
    assert.ok(out.includes('\\"'), 'quotes are escaped');
  });

  test('injected numeric filter does not widen results against a real store', () => {
    const store = makeStore();
    const qb = new SPARQLQueryBuilder({ namespaces: { ex: EX } });
    const info = {
      fieldNodes: [
        { selectionSet: { selections: [{ kind: 'Field', name: { value: 'age' }, arguments: [] }] } },
      ],
    };
    // Legit: nobody older than 100
    const ok = qb.buildFilteredQuery(info, `${EX}Person`, { age: { gt: 100 } });
    assert.equal([...store.query(ok)].length, 0);
    // Attack must be rejected rather than returning both people
    assert.throws(() => qb.buildFilteredQuery(info, `${EX}Person`, { age: { gt: '100 || true' } }));
  });
});

describe('SPARQLQueryBuilder IRI / paging injection', () => {
  test('resource IRI containing > is rejected', () => {
    const qb = new SPARQLQueryBuilder();
    const evil = 'http://example.org/people/alice> AS ?s) } UNION { ?s ?p ?o . BIND(<http://x';
    assert.throws(() => qb.buildQueryForResource(emptyInfo, evil, `${EX}Person`), /Invalid IRI/);
  });

  test('type IRI containing > is rejected', () => {
    const qb = new SPARQLQueryBuilder();
    assert.throws(
      () => qb.buildListQuery(emptyInfo, `${EX}Person> . ?s ?p ?o . <http://x`, {}),
      /Invalid IRI/
    );
  });

  test('limit/offset must be non-negative integers', () => {
    const qb = new SPARQLQueryBuilder();
    assert.throws(
      () => qb.buildListQuery(emptyInfo, `${EX}Person`, { limit: '1 } DROP ALL #' }),
      /integer/i
    );
    assert.throws(() => qb.buildListQuery(emptyInfo, `${EX}Person`, { offset: -1 }), /integer/i);
    assert.match(
      qb.buildListQuery(emptyInfo, `${EX}Person`, { limit: 5, offset: 2 }),
      /LIMIT 5\s+OFFSET 2/
    );
  });

  test('namespace declarations are validated', () => {
    const qb = new SPARQLQueryBuilder({ namespaces: { 'bad prefix': 'http://x/' } });
    assert.throws(() => qb.buildPrefixes(), /Invalid/i);
    const qb2 = new SPARQLQueryBuilder({ namespaces: { ex: 'http://x/> } #' } });
    assert.throws(() => qb2.buildPrefixes(), /Invalid IRI/);
  });

  test('buildSimpleQuery validates predicate, limit and escapes literal objects', () => {
    assert.throws(() => buildSimpleQuery({ subject: '?s', predicate: 'http://x> ?y <http://z' }));
    assert.throws(() => buildSimpleQuery({ subject: '?s', predicate: 'http://x/p', limit: '1; x' }));
    const q = buildSimpleQuery({ subject: '?s', predicate: 'http://x/p', object: 'a" . } #' });
    assert.ok(q.includes('\\"'));
    assert.match(buildSimpleQuery({ subject: '?s', predicate: 'http://x/p', limit: 3 }), /LIMIT 3/);
  });
});

describe('End-to-end via adapter', () => {
  test('malicious id in a GraphQL query does not escape the IRI', async () => {
    const adapter = createAdapter({
      namespaces: { ex: EX },
      typeMapping: { Person: `${EX}Person` },
    });
    await adapter.loadOntology(ONTOLOGY);
    await adapter.loadData(DATA);
    adapter.generateSchema();
    const q = 'query($id: ID!){ person(id:$id){ id name age } }';

    const good = await adapter.executeQuery(q, { id: 'http://example.org/people/alice' });
    assert.equal(good.data.person.name, 'Alice');

    const evil = await adapter.executeQuery(q, {
      id: 'http://example.org/people/alice> AS ?s) } UNION { ?s ?p ?o . BIND(<http://x',
    });
    assert.ok(evil.errors, 'injection attempt must surface as an error');
    assert.equal(evil.data.person, null);
  });

  test('relationship resolver rejects IRIs that break out of <>', async () => {
    const store = makeStore();
    const resolve = createRelationshipResolver(store, `${EX}name`);
    assert.deepEqual(await resolve({ id: 'http://example.org/people/alice' }, {}, {}, {}), 'Alice');
    await assert.rejects(
      resolve({ id: 'http://example.org/people/alice> ?p ?o . <http://x' }, {}, {}, {}),
      /Invalid IRI/
    );
  });
});

describe('Resolver cache bound', () => {
  test('cache never grows beyond maxCacheSize', () => {
    const factory = new RDFResolverFactory(makeStore(), { enableCache: true, maxCacheSize: 3 });
    for (let i = 0; i < 20; i++) factory.setCache(`k${i}`, i);
    assert.equal(factory.getCacheStats().size, 3);
    assert.equal(factory.cache.has('k19'), true);
    assert.equal(factory.cache.has('k0'), false);
  });
});
