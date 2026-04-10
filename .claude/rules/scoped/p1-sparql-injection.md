# Scoped Rule: SPARQL Injection Prevention

**Scope**: All SPARQL query construction and execution

## Parameterized Queries Only

Never concatenate strings into SPARQL. Use parameterized queries:

```javascript
// Correct - parameterized
const result = await store.query(`
  SELECT * WHERE {
    ?s a ?type .
    FILTER (?type = $type)
  }
`, { type: 'schema:Person' });

// Wrong - string concatenation (injection risk)
const type = 'schema:Person';
const result = await store.query(`
  SELECT * WHERE {
    ?s a ${type} .
  }
`);
```

## Template Literals for Structure

Template literals are OK for query STRUCTURE (not values):

```javascript
// Correct - structure is literal, values are parameters
const query = `
  CONSTRUCT {
    ?s a :ImprovedType .
    ?s :name ?name .
  }
  WHERE {
    ?s a $oldType .
    ?s :name ?name .
    FILTER (${includeInactive ? 'true' : 'false'})
  }
`;

// Wrong - values in template literal
const query = `
  SELECT * WHERE {
    ?s a '${userInput}' .
  }
`;
```

## RDF Term Escaping

When you MUST include values (rare), use proper RDF term escaping:

```javascript
// Correct - escape with DataFactory
const term = DataFactory.namedNode(userInput);
const query = `?s a ${term.value} .`;

// Use RDF Term library for literals
const literal = DataFactory.literal(userInput, language || '');
```

## Hooks Package Pattern

In `@unrdf/hooks`, use the SPARQL executor's parameter binding:

```javascript
// Correct
const bindings = {
  entity: DataFactory.namedNode(entityUri),
  type: DataFactory.namedNode('schema:Person')
};
await executeQuery(sparqlTemplate, bindings);
```

## Injection Test Vectors

Watch for these patterns (all indicate vulnerabilities):

- `+ variable` in SPARQL string
- `${}` with user input directly
- `.replace()` to insert values
- String concatenation with `+`

## Files Handling SPARQL

- `packages/cli/src/cli/commands/sync/sparql-executor.mjs`
- `packages/hooks/src/**/*`
- `packages/core/src/sparql/*`
