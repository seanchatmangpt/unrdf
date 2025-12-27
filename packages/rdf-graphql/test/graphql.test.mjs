/**
 * @file Comprehensive tests for RDF GraphQL Adapter
 * @module @unrdf/rdf-graphql/test
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { createAdapter } from '../src/adapter.mjs';
import { RDFSchemaGenerator } from '../src/schema-generator.mjs';
import { SPARQLQueryBuilder } from '../src/query-builder.mjs';
import { YAWL_ONTOLOGY as _YAWL_ONTOLOGY, WORKFLOW_INSTANCES as _WORKFLOW_INSTANCES, createWorkflowAdapter } from '../src/examples/workflow-schema.mjs';

// Test ontology (minimal)
const TEST_ONTOLOGY = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/test#> .

ex:Person a rdfs:Class ;
  rdfs:label "Person" ;
  rdfs:comment "A human being" .

ex:name a rdf:Property ;
  rdfs:domain ex:Person ;
  rdfs:range xsd:string ;
  rdfs:label "Name" .

ex:age a rdf:Property ;
  rdfs:domain ex:Person ;
  rdfs:range xsd:integer ;
  rdfs:label "Age" .
`;

const TEST_DATA = `
@prefix ex: <http://example.org/test#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://example.org/people/alice> a ex:Person ;
  ex:name "Alice Smith" ;
  ex:age 30 .

<http://example.org/people/bob> a ex:Person ;
  ex:name "Bob Johnson" ;
  ex:age 25 .
`;

describe('RDFSchemaGenerator', () => {
  test('should extract classes from ontology', async () => {
    const generator = new RDFSchemaGenerator({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await generator.loadOntology(TEST_ONTOLOGY);
    const classes = generator.extractClasses();

    assert.ok(classes.size >= 1, 'Should find at least one class');
    assert.ok(classes.has('http://example.org/test#Person'), 'Should find Person class');
  });

  test('should generate GraphQL schema from ontology', async () => {
    const generator = new RDFSchemaGenerator({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await generator.loadOntology(TEST_ONTOLOGY);
    const schema = generator.generateSchema();

    assert.ok(schema, 'Schema should be generated');

    const queryType = schema.getQueryType();
    assert.ok(queryType, 'Query type should exist');

    const fields = queryType.getFields();
    assert.ok(fields.person, 'Should have person query field');
    assert.ok(fields.persons, 'Should have persons list query field');
  });

  test('should include ID field in all types', async () => {
    const generator = new RDFSchemaGenerator({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await generator.loadOntology(TEST_ONTOLOGY);
    const schema = generator.generateSchema();

    const personType = schema.getType('Person');
    assert.ok(personType, 'Person type should exist');

    const fields = personType.getFields();
    assert.ok(fields.id, 'Should have id field');
    assert.ok(fields.name, 'Should have name field');
    assert.ok(fields.age, 'Should have age field');
  });

  test('should map XSD datatypes to GraphQL types', async () => {
    const generator = new RDFSchemaGenerator({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await generator.loadOntology(TEST_ONTOLOGY);
    const schema = generator.generateSchema();

    const personType = schema.getType('Person');
    const fields = personType.getFields();

    assert.equal(fields.name.type.toString(), 'String', 'name should be String');
    assert.equal(fields.age.type.toString(), 'Int', 'age should be Int');
  });
});

describe('SPARQLQueryBuilder', () => {
  test('should build prefixes', () => {
    const builder = new SPARQLQueryBuilder({
      namespaces: {
        ex: 'http://example.org/test#',
        rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
      },
    });

    const prefixes = builder.buildPrefixes();

    assert.ok(prefixes.includes('PREFIX ex:'), 'Should include ex prefix');
    assert.ok(prefixes.includes('PREFIX rdfs:'), 'Should include rdfs prefix');
  });

  test('should add custom namespace', () => {
    const builder = new SPARQLQueryBuilder();

    builder.addNamespace('custom', 'http://example.org/custom#');

    const prefixes = builder.buildPrefixes();
    assert.ok(prefixes.includes('PREFIX custom:'), 'Should include custom prefix');
  });

  test('should escape SPARQL strings', () => {
    const builder = new SPARQLQueryBuilder();

    const escaped = builder.escapeSPARQL('Hello "World"\nNew line');

    assert.ok(escaped.includes('\\"'), 'Should escape quotes');
    assert.ok(escaped.includes('\\n'), 'Should escape newlines');
  });

  test('should build simple query', () => {
    const builder = new SPARQLQueryBuilder();

    // Mock GraphQL info object
    const mockInfo = {
      fieldNodes: [{
        selectionSet: {
          selections: [
            { kind: 'Field', name: { value: 'id' } },
            { kind: 'Field', name: { value: 'name' } },
          ],
        },
      }],
    };

    const query = builder.buildQueryForResource(
      mockInfo,
      'http://example.org/people/alice',
      'http://example.org/test#Person'
    );

    assert.ok(query.includes('SELECT'), 'Should be SELECT query');
    assert.ok(query.includes('?s'), 'Should include subject variable');
    assert.ok(query.includes('BIND'), 'Should bind resource IRI');
  });
});

describe('RDFGraphQLAdapter - Basic Operations', () => {
  test('should create adapter instance', () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
    });

    assert.ok(adapter, 'Adapter should be created');
    assert.ok(adapter.getStore(), 'Should have store');
  });

  test('should load ontology and data', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    await adapter.loadData(TEST_DATA);

    const stats = await adapter.getStatistics();

    assert.ok(stats.tripleCount > 0, 'Should have loaded triples');
    assert.ok(stats.classCount >= 1, 'Should have classes');
  });

  test('should generate schema', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    const schema = adapter.generateSchema();

    assert.ok(schema, 'Schema should be generated');
    assert.ok(adapter.getSchema(), 'Should return schema via getter');
  });

  test('should execute SPARQL queries', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    await adapter.loadData(TEST_DATA);

    const results = adapter.executeSPARQL(`
      SELECT ?person WHERE {
        ?person a <http://example.org/test#Person> .
      }
    `);

    assert.ok(results.length >= 2, 'Should find at least 2 people');
  });

  test('should introspect classes', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);

    const classes = adapter.introspectClasses();

    assert.ok(classes.length >= 1, 'Should find classes');
    const personClass = classes.find(c => c.class === 'http://example.org/test#Person');
    assert.ok(personClass, 'Should find Person class');
  });

  test('should introspect properties', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);

    const properties = adapter.introspectProperties();

    assert.ok(properties.length >= 2, 'Should find properties');
    const nameProperty = properties.find(p => p.property === 'http://example.org/test#name');
    assert.ok(nameProperty, 'Should find name property');
  });
});

describe('RDFGraphQLAdapter - GraphQL Queries', () => {
  test('should execute GraphQL query for single item', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
      typeMapping: {
        Person: 'http://example.org/test#Person',
      },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    await adapter.loadData(TEST_DATA);
    adapter.generateSchema();

    const query = `
      query GetPerson($id: ID!) {
        person(id: $id) {
          id
          name
          age
        }
      }
    `;

    const result = await adapter.executeQuery(query, {
      id: 'http://example.org/people/alice',
    });

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data, 'Should have data');
  });

  test('should execute GraphQL query for list', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
      typeMapping: {
        Person: 'http://example.org/test#Person',
      },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    await adapter.loadData(TEST_DATA);
    adapter.generateSchema();

    const query = `
      query ListPeople {
        persons {
          id
          name
        }
      }
    `;

    const result = await adapter.executeQuery(query);

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data, 'Should have data');
  });

  test('should support pagination', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
      typeMapping: {
        Person: 'http://example.org/test#Person',
      },
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    await adapter.loadData(TEST_DATA);
    adapter.generateSchema();

    const query = `
      query ListPeople($limit: Int, $offset: Int) {
        persons(limit: $limit, offset: $offset) {
          id
        }
      }
    `;

    const result = await adapter.executeQuery(query, {
      limit: 1,
      offset: 0,
    });

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data, 'Should have data');
  });
});

describe('RDFGraphQLAdapter - YAWL Workflow Example', () => {
  test('should create workflow adapter', async () => {
    const { adapter, schema } = await createWorkflowAdapter();

    assert.ok(adapter, 'Adapter should be created');
    assert.ok(schema, 'Schema should be generated');
  });

  test('should query workflows', async () => {
    const { adapter } = await createWorkflowAdapter();

    const query = `
      query GetWorkflow($id: ID!) {
        workflow(id: $id) {
          id
          workflowId
          name
          description
        }
      }
    `;

    const result = await adapter.executeQuery(query, {
      id: 'http://example.org/workflows/order-fulfillment',
    });

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data?.workflow, 'Should return workflow');
    assert.equal(result.data.workflow.workflowId, 'wf-001', 'Should have correct ID');
  });

  test('should list cases', async () => {
    const { adapter } = await createWorkflowAdapter();

    const query = `
      query ListCases {
        cases {
          id
          caseId
          status
        }
      }
    `;

    const result = await adapter.executeQuery(query);

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data?.cases, 'Should return cases');
    assert.ok(result.data.cases.length >= 1, 'Should have at least one case');
  });

  test('should list tasks', async () => {
    const { adapter } = await createWorkflowAdapter();

    const query = `
      query ListTasks {
        tasks {
          id
          taskId
          priority
        }
      }
    `;

    const result = await adapter.executeQuery(query);

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data?.tasks, 'Should return tasks');
    assert.ok(result.data.tasks.length >= 2, 'Should have at least two tasks');
  });

  test('should list agents', async () => {
    const { adapter } = await createWorkflowAdapter();

    const query = `
      query ListAgents {
        agents {
          id
          agentId
          capabilities
        }
      }
    `;

    const result = await adapter.executeQuery(query);

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data?.agents, 'Should return agents');
    assert.ok(result.data.agents.length >= 2, 'Should have at least two agents');
  });

  test('should list receipts', async () => {
    const { adapter } = await createWorkflowAdapter();

    const query = `
      query ListReceipts {
        receipts {
          id
          receiptId
          timestamp
          proofHash
        }
      }
    `;

    const result = await adapter.executeQuery(query);

    assert.ok(!result.errors, 'Should not have errors');
    assert.ok(result.data?.receipts, 'Should return receipts');
    assert.ok(result.data.receipts.length >= 1, 'Should have at least one receipt');
  });

  test('should get workflow statistics', async () => {
    const { adapter } = await createWorkflowAdapter();

    const stats = await adapter.getStatistics();

    assert.ok(stats.tripleCount > 0, 'Should have triples');
    assert.ok(stats.classCount >= 5, 'Should have workflow classes');
    assert.ok(stats.instanceCount >= 8, 'Should have workflow instances');
  });
});

describe('RDFGraphQLAdapter - Caching', () => {
  test('should enable caching when configured', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
      enableCache: true,
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    await adapter.loadData(TEST_DATA);
    adapter.generateSchema();

    const stats = adapter.getCacheStats();

    assert.equal(stats.enabled, true, 'Cache should be enabled');
    assert.equal(stats.size, 0, 'Cache should be empty initially');
  });

  test('should clear cache', async () => {
    const adapter = createAdapter({
      namespaces: { ex: 'http://example.org/test#' },
      enableCache: true,
    });

    await adapter.loadOntology(TEST_ONTOLOGY);
    await adapter.loadData(TEST_DATA);
    adapter.generateSchema();

    adapter.clearCache();

    const stats = adapter.getCacheStats();
    assert.equal(stats.size, 0, 'Cache should be empty after clear');
  });
});
