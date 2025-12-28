// SPARQL with inference rules
const results = await core.query(
  store,
  `
  SELECT ?ancestor WHERE {
    ?person ex:parentOf+ ?ancestor .  # transitive closure
  }
`
);