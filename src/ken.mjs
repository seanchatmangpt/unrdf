/**
 * @file Knowledge Engine Bulk Example with Assertions
 */

import {
  parseTurtle,
  toTurtle,
  query,
  validateShacl,
  reason,
  canonicalize,
  isIsomorphic,
  TransactionManager
} from './knowledge-engine.mjs';

import { DataFactory, Store } from 'n3';
import { faker } from '@faker-js/faker';
import assert from 'assert';

const { namedNode, literal, quad } = DataFactory;

async function generatePeople(count) {
  const store = new Store();
  const ex = "http://example.org/";

  for (let i = 0; i < count; i++) {
    const id = faker.string.uuid();
    const iri = namedNode(`${ex}person-${id}`);
    const name = literal(faker.person.firstName());
    const age = literal(faker.number.int({ min: 18, max: 90 }).toString(), namedNode("http://www.w3.org/2001/XMLSchema#integer"));

    store.addQuad(iri, namedNode(`${ex}type`), namedNode(`${ex}Person`));
    store.addQuad(iri, namedNode(`${ex}name`), name);
    store.addQuad(iri, namedNode(`${ex}age`), age);

    if (i > 0 && Math.random() > 0.5) {
      const otherIndex = Math.floor(Math.random() * i);
      const otherIri = namedNode(`${ex}person-${otherIndex}`);
      store.addQuad(iri, namedNode(`${ex}knows`), otherIri);
    }
  }

  return store;
}

async function main() {
  const bulkStore = await generatePeople(50);
  console.log("Generated bulk store size:", bulkStore.size);

  // === Assertion 1: Store has expected number of quads ===
  assert(bulkStore.size >= 150, "Store should have at least 3 triples per person");

  // === SPARQL Query ===
  const rows = await query(bulkStore, `
    PREFIX ex: <http://example.org/>
    SELECT ?person ?name ?age WHERE {
      ?person ex:name ?name ;
              ex:age ?age .
    } LIMIT 5
  `);
  console.log("SPARQL query results:", rows);
  assert(rows.length > 0, "SPARQL query should return at least 1 result");
  rows.forEach(r => {
    console.log("Row:", r);
    assert(r.name && r.name !== '[object Object]', "Each row should have a name");
    assert(r.age && r.age !== '[object Object]', "Each row should have an age");
  });

  // === SHACL Validation ===
  const shapesTtl = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:datatype xsd:string ;
        sh:minCount 1
      ] ;
      sh:property [
        sh:path ex:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0
      ] .
  `;
  const report = validateShacl(bulkStore, shapesTtl);
  console.log("SHACL validation report:", report);
  // SHACL validation may return undefined for conforms, so we check that there are no results
  assert(report.conforms !== false, "SHACL validation should not have violations");
  assert(report.results.length === 0, "SHACL validation should have no validation results");

  // === Reasoning ===
  const rulesTtl = `
    @prefix ex: <http://example.org/> .
    { ?x ex:knows ?y } => { ?y ex:knows ?x } .
  `;
  const reasonedStore = await reason(bulkStore, rulesTtl);
  assert(reasonedStore.size >= bulkStore.size, "Reasoned store should be same size or larger");

  // === Canonicalization & Isomorphism ===
  const canonical = await canonicalize(bulkStore);
  assert(canonical.length > 0, "Canonicalization should produce non-empty output");

  const copy = new Store(bulkStore.getQuads());
  const iso = await isIsomorphic(bulkStore, copy);
  assert.strictEqual(iso, true, "Copy of store should be isomorphic");

  // === Transactions ===
  const tx = new TransactionManager();
  tx.addHook({
    id: "no-teenagers",
    mode: "pre",
    condition: async (_store, delta) => {
      return !delta.additions.some(q => q.predicate.value.endsWith("age") && parseInt(q.object.value) < 18);
    },
    effect: "veto"
  });

  const delta = {
    additions: [quad(namedNode("http://example.org/person-extra"), namedNode("http://example.org/age"), literal("15"))],
    removals: []
  };
  const { receipt } = await tx.apply(bulkStore, delta);
  assert.strictEqual(receipt.committed, false, "Transaction with underage person should be vetoed");

  console.log("✅ All assertions passed successfully.");
}

main().catch(err => {
  console.error("❌ Assertion failed:", err.message);
  process.exit(1);
});
