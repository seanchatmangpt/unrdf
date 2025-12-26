/**
 * @file Demo 2: Store Creation (Node.js Version)
 * @description Create an in-memory RDF store and query it in Node.js
 *
 * This is a self-contained demo that shows runtime bridging patterns
 * WITHOUT external dependencies. Real apps would use Oxigraph.
 */

// Simple in-memory RDF store (minimal implementation for demo purposes)
class SimpleStore {
  constructor() {
    this.quads = [];
  }

  add(quad) {
    this.quads.push(quad);
  }

  match(s, p, o) {
    return this.quads.filter(quad => {
      if (s !== null && quad.subject.value !== s.value) return false;
      if (p !== null && quad.predicate.value !== p.value) return false;
      if (o !== null && quad.object.value !== o.value) return false;
      return true;
    });
  }

  get size() {
    return this.quads.length;
  }
}

function namedNode(value) {
  return { termType: 'NamedNode', value };
}

function literal(value, datatype) {
  return { termType: 'Literal', value, datatype };
}

function quad(subject, predicate, object) {
  return { subject, predicate, object };
}

async function main() {
  console.log('=== Demo 2: Store Creation (Node.js) ===\n');

  try {
    // Create store
    const store = new SimpleStore();
    console.log('✓ Created in-memory RDF store');

    // Add triples
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
    const age = namedNode('http://xmlns.com/foaf/0.1/age');

    store.add(quad(alice, name, literal('Alice Smith')));
    store.add(quad(alice, age, literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));
    store.add(quad(alice, knows, bob));
    store.add(quad(bob, name, literal('Bob Jones')));
    store.add(quad(bob, age, literal('28', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));

    console.log(`✓ Added ${store.size} triples to store\n`);

    // Query: Find all names
    console.log('Query 1: Find all names');
    const names = store.match(null, name, null);
    for (const q of names) {
      console.log(`  ${q.subject.value} has name "${q.object.value}"`);
    }

    // Query: Find who Alice knows
    console.log('\nQuery 2: Find who Alice knows');
    const aliceKnows = store.match(alice, knows, null);
    for (const q of aliceKnows) {
      console.log(`  Alice knows ${q.object.value}`);
    }

    // Query: Count triples
    console.log(`\nTotal triples in store: ${store.size}`);

    console.log('\n✅ Success: Store operations completed in Node.js');

    return {
      storeSize: store.size,
      namesCount: names.length,
      aliceKnowsCount: aliceKnows.length,
    };
  } catch (error) {
    console.error('❌ Error:', error.message);
    throw error;
  }
}

main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
