/**
 * @fileoverview Demo 2: SPARQL Pattern Matching
 * Standalone version - no external dependencies
 *
 * Proves: BEAM-style pattern matching on RDF triples
 */

const dataFactory = {
  namedNode: (value) => ({ termType: 'NamedNode', value }),
  literal: (value) => ({ termType: 'Literal', value }),
};

// Mock Store with Pattern Matching
class MockStore {
  constructor() {
    this.triples = [];
  }
  add(triple) {
    this.triples.push(triple);
  }
  match(subject, predicate, object) {
    return this.triples.filter(triple => {
      if (subject && triple.subject.value !== subject.value) return false;
      if (predicate && triple.predicate.value !== predicate.value) return false;
      if (object && triple.object.value !== object.value) return false;
      return true;
    });
  }
}

const FOAF = 'http://xmlns.com/foaf/0.1/';
const EX = 'http://example.org/';

console.log('╔════════════════════════════════════════════════════════════════╗');
console.log('║  Demo 2: SPARQL Pattern Matching                              ║');
console.log('╚════════════════════════════════════════════════════════════════╝');

const store = new MockStore();
const alice = dataFactory.namedNode(`${EX}Alice`);
const bob = dataFactory.namedNode(`${EX}Bob`);
const charlie = dataFactory.namedNode(`${EX}Charlie`);
const knows = dataFactory.namedNode(`${FOAF}knows`);
const name = dataFactory.namedNode(`${FOAF}name`);

store.add({ subject: alice, predicate: name, object: dataFactory.literal('Alice') });
store.add({ subject: bob, predicate: name, object: dataFactory.literal('Bob') });
store.add({ subject: charlie, predicate: name, object: dataFactory.literal('Charlie') });
store.add({ subject: alice, predicate: knows, object: bob });
store.add({ subject: bob, predicate: knows, object: charlie });
store.add({ subject: alice, predicate: knows, object: charlie });

console.log(`\nCreated graph with ${store.triples.length} triples`);

// Test 1: Find all people Alice knows
console.log('\n=== Pattern 1: Find all people Alice knows ===');
console.log('Pattern: <Alice> <knows> ?person');
const results1 = store.match(alice, knows, null);
console.log(`Found ${results1.length} results:`);
results1.forEach((triple, i) => {
  console.log(`  ${i + 1}. ${triple.object.value}`);
});
const test1 = results1.length === 2;
console.log(test1 ? '✅ SUCCESS: Found 2 matches (expected 2)' : `❌ FAIL: Found ${results1.length} matches (expected 2)`);

// Test 2: Find all names
console.log('\n=== Pattern 2: Find all names ===');
console.log('Pattern: ?person <name> ?value');
const results2 = store.match(null, name, null);
console.log(`Found ${results2.length} results:`);
results2.forEach((triple, i) => {
  console.log(`  ${i + 1}. ${triple.subject.value.split('/').pop()} has name "${triple.object.value}"`);
});
const test2 = results2.length === 3;
console.log(test2 ? '✅ SUCCESS: Found 3 names (expected 3)' : `❌ FAIL: Found ${results2.length} names (expected 3)`);

// Test 3: Find all relationships
console.log('\n=== Pattern 3: Find all "knows" relationships ===');
console.log('Pattern: ?person1 <knows> ?person2');
const results3 = store.match(null, knows, null);
console.log(`Found ${results3.length} results:`);
results3.forEach((triple, i) => {
  const person1 = triple.subject.value.split('/').pop();
  const person2 = triple.object.value.split('/').pop();
  console.log(`  ${i + 1}. ${person1} knows ${person2}`);
});
const test3 = results3.length === 3;
console.log(test3 ? '✅ SUCCESS: Found 3 relationships (expected 3)' : `❌ FAIL: Found ${results3.length} relationships (expected 3)`);

const allPassed = test1 && test2 && test3;
console.log('\n╔════════════════════════════════════════════════════════════════╗');
if (allPassed) {
  console.log('║  ✅ ALL TESTS PASSED - Pattern matching verified!             ║');
} else {
  console.log('║  ❌ SOME TESTS FAILED - See details above                      ║');
}
console.log('╚════════════════════════════════════════════════════════════════╝');
process.exit(allPassed ? 0 : 1);
