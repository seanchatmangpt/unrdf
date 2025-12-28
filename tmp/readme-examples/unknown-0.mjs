import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize the knowledge substrate with all features
const core = await createKnowledgeSubstrateCore();

// Parse RDF data (Turtle format)
const store = core.parseRdf(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice foaf:name "Alice Smith" ;
           foaf:knows ex:Bob .

  ex:Bob foaf:name "Bob Johnson" .
`);

// Query with SPARQL
const results = await core.query(
  store,
  `
  SELECT ?name WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
  }
`
);

// Access results
for (const binding of results) {
  console.log(binding.get('name')?.value);
}
// Output:
// Alice Smith
// Bob Johnson