/**
 * @file SPARQL Query Builder Demo
 * @module examples/rdf-kgn/03-sparql-builder-demo
 * @description Demonstrate dynamic SPARQL query generation using templates
 *
 * Time Estimate: 15-20 minutes
 * Difficulty: Intermediate
 * Prerequisites: SPARQL query language basics
 */

import { RdfTemplateEngine } from '@unrdf/kgn/rdf';
import { createStore, executeQuery, namedNode, literal, COMMON_PREFIXES } from '@unrdf/core';

/**
 * SPARQL SELECT query template
 */
const SPARQL_SELECT_TEMPLATE = `
{% if prefixes %}
{% for prefix, uri in prefixes %}
PREFIX {{ prefix }}: <{{ uri }}>
{% endfor %}
{% endif %}

SELECT {% if distinct %}DISTINCT {% endif %}{% if variables %}{{ variables | join(' ') }}{% else %}*{% endif %}
WHERE {
{% for pattern in where %}
  {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
{% endfor %}
{% if filter %}
  FILTER({{ filter }})
{% endif %}
}
{% if orderBy %}
ORDER BY {% for order in orderBy %}{{ order.direction }} ?{{ order.variable }}{% if not loop.last %} {% endif %}{% endfor %}
{% endif %}
{% if limit %}
LIMIT {{ limit }}
{% endif %}
{% if offset > 0 %}
OFFSET {{ offset }}
{% endif %}
`.trim();

/**
 * SPARQL INSERT DATA template
 */
const SPARQL_INSERT_TEMPLATE = `
{% if prefixes %}
{% for prefix, uri in prefixes %}
PREFIX {{ prefix }}: <{{ uri }}>
{% endfor %}
{% endif %}

INSERT DATA {
{% for triple in triples %}
  {{ triple.subject }} {{ triple.predicate }} {{ triple.object }} .
{% endfor %}
}
`.trim();

/**
 * Create sample dataset for demonstration
 * @param {Object} store - RDF store
 * @returns {Promise<void>}
 */
async function createSampleDataset(store) {
  const ex = (name) => namedNode(`http://example.org/${name}`);
  const schema = (name) => namedNode(`http://schema.org/${name}`);

  // Add people
  const people = [
    { id: 'alice', name: 'Alice Johnson', email: 'alice@example.com', age: 28, role: 'Developer' },
    { id: 'bob', name: 'Bob Smith', email: 'bob@example.com', age: 35, role: 'Manager' },
    { id: 'charlie', name: 'Charlie Brown', email: 'charlie@example.com', age: 42, role: 'Architect' },
    { id: 'diana', name: 'Diana Prince', email: 'diana@example.com', age: 31, role: 'Developer' },
  ];

  for (const person of people) {
    store.add(ex(person.id), schema('name'), literal(person.name));
    store.add(ex(person.id), schema('email'), literal(person.email));
    store.add(ex(person.id), schema('age'), literal(person.age));
    store.add(ex(person.id), schema('jobTitle'), literal(person.role));
    store.add(ex(person.id), namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), schema('Person'));
  }
}

/**
 * Demonstrate SPARQL query builder
 * @returns {Promise<void>}
 */
async function sparqlBuilderDemo() {
  console.log('=== SPARQL Query Builder Demo ===\n');

  // Create template engine
  const engine = new RdfTemplateEngine({
    prefixes: COMMON_PREFIXES,
  });

  // Create and populate store
  const store = createStore();
  await createSampleDataset(store);
  console.log('✓ Sample dataset created\n');

  // Example 1: Simple SELECT query
  console.log('--- Example 1: Basic SELECT Query ---');
  const basicQuery = engine.render(SPARQL_SELECT_TEMPLATE, {
    prefixes: {
      schema: 'http://schema.org/',
      ex: 'http://example.org/',
    },
    variables: ['?person', '?name', '?email'],
    where: [
      { subject: '?person', predicate: 'a', object: 'schema:Person' },
      { subject: '?person', predicate: 'schema:name', object: '?name' },
      { subject: '?person', predicate: 'schema:email', object: '?email' },
    ],
    distinct: false,
  });

  console.log('Generated Query:');
  console.log(basicQuery);
  console.log('\nResults:');
  const results1 = await executeQuery(store, basicQuery);
  results1.forEach((binding, i) => {
    console.log(`${i + 1}. ${binding.name.value} <${binding.email.value}>`);
  });

  // Example 2: Query with FILTER
  console.log('\n--- Example 2: Query with FILTER ---');
  const filterQuery = engine.render(SPARQL_SELECT_TEMPLATE, {
    prefixes: {
      schema: 'http://schema.org/',
      ex: 'http://example.org/',
    },
    variables: ['?person', '?name', '?age'],
    where: [
      { subject: '?person', predicate: 'a', object: 'schema:Person' },
      { subject: '?person', predicate: 'schema:name', object: '?name' },
      { subject: '?person', predicate: 'schema:age', object: '?age' },
    ],
    filter: '?age >= 30',
    distinct: true,
  });

  console.log('Generated Query:');
  console.log(filterQuery);
  console.log('\nResults:');
  const results2 = await executeQuery(store, filterQuery);
  results2.forEach((binding, i) => {
    console.log(`${i + 1}. ${binding.name.value} (age: ${binding.age.value})`);
  });

  // Example 3: Query with ORDER BY and LIMIT
  console.log('\n--- Example 3: Query with ORDER BY and LIMIT ---');
  const sortedQuery = engine.render(SPARQL_SELECT_TEMPLATE, {
    prefixes: {
      schema: 'http://schema.org/',
      ex: 'http://example.org/',
    },
    variables: ['?person', '?name', '?age'],
    where: [
      { subject: '?person', predicate: 'a', object: 'schema:Person' },
      { subject: '?person', predicate: 'schema:name', object: '?name' },
      { subject: '?person', predicate: 'schema:age', object: '?age' },
    ],
    orderBy: [
      { variable: 'age', direction: 'DESC' },
    ],
    limit: 2,
    distinct: true,
  });

  console.log('Generated Query:');
  console.log(sortedQuery);
  console.log('\nResults (top 2 oldest):');
  const results3 = await executeQuery(store, sortedQuery);
  results3.forEach((binding, i) => {
    console.log(`${i + 1}. ${binding.name.value} (age: ${binding.age.value})`);
  });

  // Example 4: Query for specific job role
  console.log('\n--- Example 4: Query by Job Role ---');
  const roleQuery = engine.render(SPARQL_SELECT_TEMPLATE, {
    prefixes: {
      schema: 'http://schema.org/',
      ex: 'http://example.org/',
    },
    variables: ['?person', '?name', '?role'],
    where: [
      { subject: '?person', predicate: 'a', object: 'schema:Person' },
      { subject: '?person', predicate: 'schema:name', object: '?name' },
      { subject: '?person', predicate: 'schema:jobTitle', object: '?role' },
    ],
    filter: '?role = "Developer"',
    distinct: true,
  });

  console.log('Generated Query:');
  console.log(roleQuery);
  console.log('\nResults (Developers only):');
  const results4 = await executeQuery(store, roleQuery);
  results4.forEach((binding, i) => {
    console.log(`${i + 1}. ${binding.name.value} - ${binding.role.value}`);
  });

  console.log(`\n✓ Executed ${4} different SPARQL queries successfully`);
}

// Execute demo
try {
  await sparqlBuilderDemo();
  console.log('\n✓ Example completed successfully');
  process.exit(0);
} catch (error) {
  console.error('\n✗ Example failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
