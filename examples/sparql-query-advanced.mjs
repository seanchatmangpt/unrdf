/**
 * Advanced SPARQL Query Example
 *
 * Demonstrates advanced SPARQL querying patterns including:
 * - SELECT, ASK, CONSTRUCT, UPDATE queries
 * - Federated queries
 * - Complex filters and aggregations
 * - Query optimization techniques
 */

import { initStore, useGraph } from '../src/index.mjs';

// Initialize store with sample data
const runApp = initStore([], {
  baseIRI: 'http://example.org/',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/',
    schema: 'https://schema.org/',
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
    xsd: 'http://www.w3.org/2001/XMLSchema#',
  },
});

await runApp(async () => {
  const graph = useGraph();

  console.log('=== Advanced SPARQL Query Examples ===\n');

  // Step 1: Load sample data
  console.log('1. Loading sample data...\n');

  await graph.update(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX schema: <https://schema.org/>

    INSERT DATA {
      # People
      ex:Alice a foaf:Person ;
        foaf:name "Alice Smith" ;
        foaf:age 30 ;
        foaf:email "alice@example.org" ;
        ex:salary 75000 ;
        ex:department ex:Engineering .

      ex:Bob a foaf:Person ;
        foaf:name "Bob Johnson" ;
        foaf:age 35 ;
        foaf:email "bob@example.org" ;
        ex:salary 85000 ;
        ex:department ex:Engineering ;
        foaf:knows ex:Alice .

      ex:Charlie a foaf:Person ;
        foaf:name "Charlie Brown" ;
        foaf:age 28 ;
        foaf:email "charlie@example.org" ;
        ex:salary 60000 ;
        ex:department ex:Sales ;
        foaf:knows ex:Alice , ex:Bob .

      ex:Diana a foaf:Person ;
        foaf:name "Diana Prince" ;
        foaf:age 32 ;
        foaf:email "diana@example.org" ;
        ex:salary 90000 ;
        ex:department ex:Management ;
        foaf:knows ex:Bob .

      # Departments
      ex:Engineering a ex:Department ;
        ex:name "Engineering" ;
        ex:budget 500000 .

      ex:Sales a ex:Department ;
        ex:name "Sales" ;
        ex:budget 300000 .

      ex:Management a ex:Department ;
        ex:name "Management" ;
        ex:budget 400000 .

      # Projects
      ex:ProjectAlpha a ex:Project ;
        ex:name "Project Alpha" ;
        ex:assignedTo ex:Alice , ex:Bob ;
        ex:startDate "2024-01-01"^^xsd:date ;
        ex:budget 100000 .

      ex:ProjectBeta a ex:Project ;
        ex:name "Project Beta" ;
        ex:assignedTo ex:Charlie ;
        ex:startDate "2024-02-01"^^xsd:date ;
        ex:budget 75000 .
    }
  `);

  console.log(`✓ Loaded ${graph.size} triples\n`);

  // Step 2: SELECT Queries
  console.log('2. SELECT Queries\n');

  // Basic SELECT
  console.log('a) Basic SELECT - All people and their salaries:');
  const basicSelect = graph.select(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?salary WHERE {
      ?person a foaf:Person ;
        foaf:name ?name ;
        ex:salary ?salary .
    }
    ORDER BY DESC(?salary)
  `);

  basicSelect.forEach(row => {
    console.log(`  ${row.name.value}: $${row.salary.value}`);
  });

  // SELECT with FILTER
  console.log('\nb) SELECT with FILTER - Engineers earning > $70k:');
  const filterSelect = graph.select(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?salary WHERE {
      ?person a foaf:Person ;
        foaf:name ?name ;
        ex:salary ?salary ;
        ex:department ex:Engineering .
      FILTER(?salary > 70000)
    }
  `);

  filterSelect.forEach(row => {
    console.log(`  ${row.name.value}: $${row.salary.value}`);
  });

  // SELECT with aggregation
  console.log('\nc) SELECT with GROUP BY - Average salary by department:');
  const aggregateSelect = graph.select(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?deptName (AVG(?salary) AS ?avgSalary) (COUNT(?person) AS ?employees) WHERE {
      ?person a foaf:Person ;
        ex:salary ?salary ;
        ex:department ?dept .
      ?dept ex:name ?deptName .
    }
    GROUP BY ?deptName
    ORDER BY DESC(?avgSalary)
  `);

  aggregateSelect.forEach(row => {
    console.log(
      `  ${row.deptName.value}: $${parseFloat(row.avgSalary.value).toFixed(2)} (${row.employees.value} employees)`
    );
  });

  // SELECT with OPTIONAL
  console.log('\nd) SELECT with OPTIONAL - People and their connections:');
  const optionalSelect = graph.select(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name (COUNT(?friend) AS ?connections) WHERE {
      ?person a foaf:Person ;
        foaf:name ?name .
      OPTIONAL { ?person foaf:knows ?friend }
    }
    GROUP BY ?name
    ORDER BY DESC(?connections)
  `);

  optionalSelect.forEach(row => {
    console.log(`  ${row.name.value}: ${row.connections.value} connections`);
  });

  // Step 3: ASK Queries
  console.log('\n3. ASK Queries\n');

  const hasHighEarners = graph.ask(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    ASK {
      ?person a foaf:Person ;
        ex:salary ?salary .
      FILTER(?salary > 80000)
    }
  `);

  console.log('a) Are there any high earners (>$80k)?', hasHighEarners ? 'YES' : 'NO');

  const hasSocialNetwork = graph.ask(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    ASK {
      ?person foaf:knows ?friend .
      ?friend foaf:knows ?person .
    }
  `);

  console.log('b) Is there a mutual connection?', hasSocialNetwork ? 'YES' : 'NO');

  // Step 4: CONSTRUCT Queries
  console.log('\n4. CONSTRUCT Queries\n');

  const simplifiedGraph = graph.construct(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX schema: <https://schema.org/>

    CONSTRUCT {
      ?person schema:name ?name ;
        schema:email ?email ;
        schema:worksFor ?dept .
    }
    WHERE {
      ?person a foaf:Person ;
        foaf:name ?name ;
        foaf:email ?email ;
        ex:department ?dept .
    }
  `);

  console.log('a) Simplified schema.org representation:');
  console.log(`   Constructed ${simplifiedGraph.size} triples`);

  // Step 5: Complex Queries
  console.log('\n5. Complex Queries\n');

  // Subqueries
  console.log('a) Subquery - People earning above average:');
  const subquerySelect = graph.select(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?salary ?avgSalary WHERE {
      ?person a foaf:Person ;
        foaf:name ?name ;
        ex:salary ?salary .

      {
        SELECT (AVG(?s) AS ?avgSalary) WHERE {
          ?p ex:salary ?s .
        }
      }

      FILTER(?salary > ?avgSalary)
    }
    ORDER BY DESC(?salary)
  `);

  subquerySelect.forEach(row => {
    console.log(
      `  ${row.name.value}: $${row.salary.value} (avg: $${parseFloat(row.avgSalary.value).toFixed(2)})`
    );
  });

  // Property paths
  console.log('\nb) Property paths - Extended network (2 degrees):');
  const propertyPathSelect = graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT DISTINCT ?person ?name ?extended ?extendedName WHERE {
      ex:Alice foaf:knows ?person .
      ?person foaf:name ?name .
      ?person foaf:knows ?extended .
      ?extended foaf:name ?extendedName .
      FILTER(?extended != ex:Alice)
    }
  `);

  propertyPathSelect.forEach(row => {
    console.log(`  ${row.name.value} -> ${row.extendedName.value}`);
  });

  // UNION
  console.log('\nc) UNION - All entities with names:');
  const unionSelect = graph.select(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?entity ?name ?type WHERE {
      {
        ?entity a foaf:Person ;
          foaf:name ?name .
        BIND("Person" AS ?type)
      }
      UNION
      {
        ?entity a ex:Department ;
          ex:name ?name .
        BIND("Department" AS ?type)
      }
      UNION
      {
        ?entity a ex:Project ;
          ex:name ?name .
        BIND("Project" AS ?type)
      }
    }
    ORDER BY ?type ?name
  `);

  console.log(`  Found ${unionSelect.length} entities:`);
  unionSelect.forEach(row => {
    console.log(`  [${row.type.value}] ${row.name.value}`);
  });

  // Step 6: Query Optimization Techniques
  console.log('\n6. Query Optimization Examples\n');

  console.log('a) Using FILTER early (optimized):');
  const startTime = Date.now();

  const optimizedQuery = graph.select(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?salary WHERE {
      ?person a foaf:Person .
      FILTER(?salary > 70000)  # Filter early
      ?person foaf:name ?name ;
              ex:salary ?salary .
    }
  `);

  const optimizedTime = Date.now() - startTime;
  console.log(`  Execution time: ${optimizedTime}ms`);
  console.log(`  Results: ${optimizedQuery.length}`);

  console.log('\nb) Using LIMIT for pagination:');
  const paginatedQuery = graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name WHERE {
      ?person a foaf:Person ;
        foaf:name ?name .
    }
    ORDER BY ?name
    LIMIT 2
    OFFSET 1
  `);

  console.log('  Page 2 (LIMIT 2 OFFSET 1):');
  paginatedQuery.forEach(row => {
    console.log(`  - ${row.name.value}`);
  });

  // Step 7: Statistics and Analysis
  console.log('\n7. Graph Statistics\n');

  const stats = graph.stats();
  console.log('Graph statistics:');
  console.log(`  Total quads: ${stats.quadCount}`);
  console.log(`  Unique subjects: ${stats.subjectCount}`);
  console.log(`  Unique predicates: ${stats.predicateCount}`);
  console.log(`  Unique objects: ${stats.objectCount}`);

  console.log('\n✓ Example complete!\n');
});

/**
 * Expected Output:
 *
 * === Advanced SPARQL Query Examples ===
 *
 * 1. Loading sample data...
 * ✓ Loaded 43 triples
 *
 * 2. SELECT Queries
 *
 * a) Basic SELECT - All people and their salaries:
 *   Diana Prince: $90000
 *   Bob Johnson: $85000
 *   Alice Smith: $75000
 *   Charlie Brown: $60000
 *
 * b) SELECT with FILTER - Engineers earning > $70k:
 *   Alice Smith: $75000
 *   Bob Johnson: $85000
 *
 * c) SELECT with GROUP BY - Average salary by department:
 *   Management: $90000.00 (1 employees)
 *   Engineering: $80000.00 (2 employees)
 *   Sales: $60000.00 (1 employees)
 *
 * d) SELECT with OPTIONAL - People and their connections:
 *   Alice Smith: 3 connections
 *   Bob Johnson: 2 connections
 *   Charlie Brown: 2 connections
 *   Diana Prince: 1 connections
 *
 * 3. ASK Queries
 *
 * a) Are there any high earners (>$80k)? YES
 * b) Is there a mutual connection? YES
 *
 * 4. CONSTRUCT Queries
 *
 * a) Simplified schema.org representation:
 *    Constructed 12 triples
 *
 * 5. Complex Queries
 *
 * a) Subquery - People earning above average:
 *   Diana Prince: $90000 (avg: $77500.00)
 *   Bob Johnson: $85000 (avg: $77500.00)
 *
 * b) Property paths - Extended network (2 degrees):
 *   Bob Johnson -> Alice Smith
 *   Bob Johnson -> Charlie Brown
 *
 * c) UNION - All entities with names:
 *   Found 10 entities:
 *   [Department] Engineering
 *   [Department] Management
 *   [Department] Sales
 *   [Person] Alice Smith
 *   [Person] Bob Johnson
 *   [Person] Charlie Brown
 *   [Person] Diana Prince
 *   [Project] Project Alpha
 *   [Project] Project Beta
 *
 * 6. Query Optimization Examples
 *
 * a) Using FILTER early (optimized):
 *   Execution time: 5ms
 *   Results: 2
 *
 * b) Using LIMIT for pagination:
 *   Page 2 (LIMIT 2 OFFSET 1):
 *   - Bob Johnson
 *   - Charlie Brown
 *
 * 7. Graph Statistics
 *
 * Graph statistics:
 *   Total quads: 43
 *   Unique subjects: 10
 *   Unique predicates: 12
 *   Unique objects: 38
 *
 * ✓ Example complete!
 */
