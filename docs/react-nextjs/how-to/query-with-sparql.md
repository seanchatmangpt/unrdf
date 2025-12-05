# How-to: Query with SPARQL

**Goal:** Execute SPARQL queries against your knowledge graph using React hooks.

**Time:** 5-10 minutes

---

## Basic SPARQL Query

Use the `useSPARQLQuery` hook to execute SELECT queries:

```jsx
import { useSPARQLQuery } from 'unrdf-react';

function PeopleList() {
  const { data, isLoading, error } = useSPARQLQuery(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?email
    WHERE {
      ?person a foaf:Person .
      ?person foaf:name ?name .
      OPTIONAL { ?person foaf:mbox ?email }
    }
    ORDER BY ?name
  `);

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <ul>
      {data?.map((row, idx) => (
        <li key={idx}>
          {row.name} {row.email && `(${row.email})`}
        </li>
      ))}
    </ul>
  );
}
```

**Result:** Fetches all people with their names and optional emails.

---

## Query with Variables

Pass variables to your SPARQL query:

```jsx
function PersonDetails({ personId }) {
  const { data } = useSPARQLQuery(
    `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT ?property ?value
      WHERE {
        $personId ?property ?value
      }
    `,
    {
      variables: {
        personId
      }
    }
  );

  return (
    <dl>
      {data?.map((row, idx) => (
        <div key={idx}>
          <dt>{row.property}</dt>
          <dd>{row.value}</dd>
        </div>
      ))}
    </dl>
  );
}
```

**Result:** Fetches all properties for a specific person.

---

## CONSTRUCT Queries

Use CONSTRUCT to build new RDF graphs:

```jsx
import { useSPARQLConstruct } from 'unrdf-react';

function SimplifiedView() {
  const { data: triples } = useSPARQLConstruct(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/>

    CONSTRUCT {
      ?person ex:displayName ?name .
      ?person ex:contact ?email .
    }
    WHERE {
      ?person a foaf:Person .
      ?person foaf:name ?name .
      OPTIONAL { ?person foaf:mbox ?email }
    }
  `);

  // triples is an array of { subject, predicate, object }
  return <div>{triples?.length} simplified triples</div>;
}
```

**Result:** Creates a simplified graph with renamed predicates.

---

## ASK Queries

Check if a pattern exists:

```jsx
import { useSPARQLAsk } from 'unrdf-react';

function FriendshipChecker({ person1, person2 }) {
  const { data: areFriends } = useSPARQLAsk(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    ASK {
      {
        $person1 foaf:knows $person2
      } UNION {
        $person2 foaf:knows $person1
      }
    }
  `, {
    variables: { person1, person2 }
  });

  return (
    <div>
      {areFriends ? '✅ They are friends' : '❌ Not friends'}
    </div>
  );
}
```

**Result:** Returns boolean indicating if pattern matches.

---

## Query with Pagination

Handle large result sets efficiently:

```jsx
import { useSPARQLQuery } from 'unrdf-react';
import { useState } from 'react';

function PaginatedResults() {
  const [page, setPage] = useState(0);
  const pageSize = 10;

  const { data, isLoading } = useSPARQLQuery(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?email
    WHERE {
      ?person a foaf:Person .
      ?person foaf:name ?name .
      OPTIONAL { ?person foaf:mbox ?email }
    }
    ORDER BY ?name
    LIMIT ${pageSize}
    OFFSET ${page * pageSize}
  `);

  return (
    <div>
      <ul>
        {data?.map((row, idx) => (
          <li key={idx}>{row.name}</li>
        ))}
      </ul>
      <button onClick={() => setPage(p => Math.max(0, p - 1))}>
        Previous
      </button>
      <span>Page {page + 1}</span>
      <button onClick={() => setPage(p => p + 1)}>
        Next
      </button>
    </div>
  );
}
```

**Result:** Loads 10 results at a time with pagination controls.

---

## Query with Filters

Use FILTER to refine results:

```jsx
function SearchPeople({ searchTerm }) {
  const { data } = useSPARQLQuery(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?email
    WHERE {
      ?person a foaf:Person .
      ?person foaf:name ?name .
      OPTIONAL { ?person foaf:mbox ?email }
      FILTER(CONTAINS(LCASE(?name), LCASE("${searchTerm}")))
    }
  `);

  return (
    <ul>
      {data?.map((row, idx) => (
        <li key={idx}>{row.name}</li>
      ))}
    </ul>
  );
}
```

**Result:** Filters people by name (case-insensitive substring match).

---

## Aggregation Queries

Count, sum, average, etc.:

```jsx
function Statistics() {
  const { data } = useSPARQLQuery(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT
      (COUNT(?person) as ?totalPeople)
      (COUNT(?email) as ?peopleWithEmail)
    WHERE {
      ?person a foaf:Person .
      OPTIONAL { ?person foaf:mbox ?email }
    }
  `);

  const stats = data?.[0];

  return (
    <div>
      <p>Total People: {stats?.totalPeople}</p>
      <p>With Email: {stats?.peopleWithEmail}</p>
    </div>
  );
}
```

**Result:** Computes aggregate statistics.

---

## Query with Auto-Refetch

Automatically re-query when data changes:

```jsx
function LiveResults() {
  const { data, refetch } = useSPARQLQuery(
    `SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100`,
    {
      // Re-fetch every 5 seconds
      refetchInterval: 5000,

      // Or refetch on window focus
      refetchOnFocus: true,

      // Or refetch when coming back online
      refetchOnReconnect: true
    }
  );

  return (
    <div>
      <button onClick={refetch}>Refresh Now</button>
      <p>{data?.length} results</p>
    </div>
  );
}
```

**Result:** Query stays up-to-date automatically.

---

## Error Handling

Handle query errors gracefully:

```jsx
function RobustQuery() {
  const { data, error, isLoading, refetch } = useSPARQLQuery(`
    SELECT ?invalid SYNTAX ERROR
  `);

  if (isLoading) {
    return <div>Loading...</div>;
  }

  if (error) {
    return (
      <div className="error">
        <p>Query failed: {error.message}</p>
        <button onClick={refetch}>Try Again</button>
      </div>
    );
  }

  return <div>{data?.length} results</div>;
}
```

**Result:** Shows error message with retry option.

---

## Performance Tips

1. **Use variables instead of string concatenation:**
   ```jsx
   // ✅ Good - prevents injection, enables caching
   useSPARQLQuery(query, { variables: { name: userInput } })

   // ❌ Bad - injection risk, no caching
   useSPARQLQuery(`SELECT ?x WHERE { ?x foaf:name "${userInput}" }`)
   ```

2. **Limit results when possible:**
   ```sparql
   SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100
   ```

3. **Use indexes with specific predicates:**
   ```sparql
   # ✅ Good - uses predicate index
   SELECT ?name WHERE { ?s foaf:name ?name }

   # ❌ Slow - scans all triples
   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
   ```

4. **Enable query caching:**
   ```jsx
   useSPARQLQuery(query, { cacheTime: 60000 }) // Cache for 1 minute
   ```

---

## Related

- [Reference: Query Hooks API](../reference/query-hooks.md)
- [How-to: Optimize Query Performance](./optimize-queries.md)
- [Explanation: SPARQL in UNRDF](../explanation/sparql-integration.md)
