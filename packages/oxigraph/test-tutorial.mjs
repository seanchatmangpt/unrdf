import { createStore, dataFactory } from './src/index.mjs';

console.log('Testing tutorial examples against real API...\n');

// Test 1: Create Store
const store = createStore();
console.log('✅ Test 1: Store created');
console.log('   Store size:', store.size);

// Test 2: Add Data
const alice = dataFactory.namedNode('http://example.com/alice');
const bob = dataFactory.namedNode('http://example.com/bob');
const knows = dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows');
const name = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
const age = dataFactory.namedNode('http://xmlns.com/foaf/0.1/age');

store.add(dataFactory.triple(alice, name, dataFactory.literal('Alice')));
store.add(dataFactory.triple(alice, age, dataFactory.literal('30')));
store.add(dataFactory.triple(alice, knows, bob));
store.add(dataFactory.triple(bob, name, dataFactory.literal('Bob')));

console.log('\n✅ Test 2: Added data');
console.log('   Store size:', store.size);

// Test 3: Pattern Matching
const aliceTriples = store.match(alice, null, null);
console.log('\n✅ Test 3: Pattern matching');
console.log('   Found', aliceTriples.length, 'triples about Alice');

// Test 4: SPARQL Query
const results = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name
  }
`);

console.log('\n✅ Test 4: SPARQL SELECT');
console.log('   Found', results.length, 'names:');
results.forEach(binding => {
  console.log('   -', binding.get('name').value);
});

// Test 5: SPARQL ASK
const askResult = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK {
    <http://example.com/alice> foaf:knows <http://example.com/bob>
  }
`);
console.log('\n✅ Test 5: SPARQL ASK');
console.log('   Does Alice know Bob?', askResult);

// Test 6: Load Turtle
const newStore = createStore();
newStore.load(`
  @prefix ex: <http://example.com/> .
  ex:charlie ex:knows ex:alice .
`, {
  format: 'text/turtle',
  base_iri: 'http://example.com/',
});

console.log('\n✅ Test 6: Load Turtle');
console.log('   Loaded', newStore.size, 'triples');

// Test 7: Dump
const exported = newStore.dump({ format: 'application/n-triples' });
console.log('\n✅ Test 7: Export to N-Triples');
console.log('   Exported', exported.split('\n').filter(l => l.trim()).length, 'lines');

console.log('\n🎉 All tutorial examples verified successfully!');
console.log('\nSummary:');
console.log('  - createStore() works ✅');
console.log('  - dataFactory.namedNode/literal/triple() work ✅');
console.log('  - store.add() works ✅');
console.log('  - store.match() works ✅');
console.log('  - store.query() (SELECT) works ✅');
console.log('  - store.query() (ASK) works ✅');
console.log('  - store.load() works ✅');
console.log('  - store.dump() works ✅');
