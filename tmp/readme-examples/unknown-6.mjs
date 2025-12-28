// Query policy compliance
const violations = await core.query(
  store,
  `
  SELECT ?resource ?violation WHERE {
    ?resource a sec:ProtectedResource ;
              sec:policy ?policy .
    ?policy sec:requires ?requirement .
    FILTER NOT EXISTS { ?resource sec:has ?requirement }
  }
`
);