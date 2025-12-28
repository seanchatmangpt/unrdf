// Query experts by skill
const experts = await core.query(
  store,
  `
  SELECT ?expert ?skill WHERE {
    ?expert a ex:Employee ;
            ex:hasSkill ?skill ;
            ex:experienceLevel "expert" .
  }
`
);