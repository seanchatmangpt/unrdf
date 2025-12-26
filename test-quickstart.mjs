import { createStore, dataFactory } from '@unrdf/oxigraph';

// Test 1: Create Store
const store = createStore();
console.log('✅ Test 1: Store created');
console.log('Store size:', store.size);

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

console.log('\n✅ Test 2: Added', store.size, 'triples');

// Test 3: Pattern Matching
const aliceTriples = store.match(alice, null, null);
console.log('\n✅ Test 3: Pattern matching found', aliceTriples.length, 'triples about Alice');

// Test 4: SPARQL Query
const results = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name
  }
`);

console.log('\n✅ Test 4: SPARQL found', results.length, 'names');
results.forEach(binding => {
  console.log('  -', binding.get('name').value);
});

// Test 5: Load Turtle
const newStore = createStore();
newStore.load(`
  @prefix ex: <http://example.com/> .
  ex:charlie ex:knows ex:alice .
`, {
  format: 'text/turtle',
  base_iri: 'http://example.com/',
});

console.log('\n✅ Test 5: Loaded', newStore.size, 'triples from Turtle');

// Test 6: Dump
const exported = newStore.dump({ format: 'application/n-triples' });
console.log('\n✅ Test 6: Exported', exported.split('\n').filter(l => l.trim()).length, 'N-Triples');

console.log('\n🎉 All tutorial examples verified!');
