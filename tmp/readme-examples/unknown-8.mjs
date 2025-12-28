import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

// Define blog data
const blogData = `
  @prefix blog: <http://example.org/blog/> .
  @prefix dct: <http://purl.org/dc/terms/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  blog:post1 a blog:BlogPost ;
            dct:title "Hello UNRDF" ;
            dct:creator blog:alice ;
            dct:date "2024-01-01" .

  blog:alice a foaf:Person ;
            foaf:name "Alice" ;
            foaf:mbox "alice@example.org" .
`;

const store = core.parseRdf(blogData);

// Query: Find all blog posts with their creators
const results = await core.query(
  store,
  `
  PREFIX blog: <http://example.org/blog/>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?title ?author WHERE {
    ?post a blog:BlogPost ;
          dct:title ?title ;
          dct:creator ?creator .
    ?creator foaf:name ?author .
  }
`
);

// Display results
for (const row of results) {
  console.log(`${row.get('title').value} by ${row.get('author').value}`);
}
// Output: Hello UNRDF by Alice