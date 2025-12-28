// Find similar documents by topic
const similar = await core.query(
  store,
  `
  SELECT ?doc1 ?doc2 WHERE {
    ?doc1 dct:subject ?topic .
    ?doc2 dct:subject ?topic .
    FILTER (?doc1 != ?doc2)
  }
`
);