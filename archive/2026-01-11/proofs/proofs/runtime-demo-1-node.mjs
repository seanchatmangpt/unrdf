/**
 * @file Demo 1: RDF Parsing (Node.js Version)
 * @description Parse Turtle RDF in Node.js using basic string parsing
 *
 * This is a self-contained demo that shows runtime bridging patterns
 * WITHOUT external dependencies. Real apps would use N3.js.
 */

// Simple Turtle parser (minimal implementation for demo purposes)
function parseTurtleSimple(input) {
  const quads = [];
  const lines = input.split('\n').filter(l => {
    const trimmed = l.trim();
    return trimmed && !trimmed.startsWith('@prefix') && !trimmed.startsWith('#');
  });

  let currentSubject = null;
  let currentPredicate = null;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed === '.') continue;

    // Handle subject predicate object pattern
    const match = trimmed.match(/^(\S+)\s+(\S+)\s+(.+?)\s*[;.]?\s*$/);
    if (match) {
      const [, s, p, o] = match;

      currentSubject = s.startsWith('<') ? s.slice(1, -1) :
                       s.startsWith('ex:') ? `http://example.org/${s.slice(3)}` : s;

      currentPredicate = p.startsWith('<') ? p.slice(1, -1) :
                        p.startsWith('foaf:') ? `http://xmlns.com/foaf/0.1/${p.slice(5)}` : p;

      let object = o.replace(/[;.]$/, '').trim();
      if (object.startsWith('"') && object.endsWith('"')) {
        object = object.slice(1, -1);
      } else if (object.startsWith('<') && object.endsWith('>')) {
        object = object.slice(1, -1);
      } else if (object.startsWith('ex:')) {
        object = `http://example.org/${object.slice(3)}`;
      }

      quads.push({
        subject: { value: currentSubject },
        predicate: { value: currentPredicate },
        object: { value: object },
      });
    }
  }

  return quads;
}

const turtleInput = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:name "Alice Smith" .
ex:alice foaf:age "30" .
ex:alice foaf:knows ex:bob .
ex:bob foaf:name "Bob Jones" .
ex:bob foaf:age "28" .
`;

async function main() {
  console.log('=== Demo 1: RDF Parsing (Node.js) ===\n');

  try {
    const quads = parseTurtleSimple(turtleInput);

    console.log(`Parsed ${quads.length} triples:`);
    console.log('');

    for (const quad of quads) {
      console.log(`Subject:   ${quad.subject.value}`);
      console.log(`Predicate: ${quad.predicate.value}`);
      console.log(`Object:    ${quad.object.value}`);
      console.log('---');
    }

    console.log('\n✅ Success: Parsed RDF in Node.js');
    console.log(`Total triples: ${quads.length}`);

    return quads;
  } catch (error) {
    console.error('❌ Error:', error.message);
    throw error;
  }
}

main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
