/**
 * @file SHACL Validation Workflow Example
 * @module examples/rdf-kgn/04-shacl-validation-workflow
 * @description Complete SHACL shape generation and validation workflow
 *
 * Time Estimate: 20-25 minutes
 * Difficulty: Advanced
 * Prerequisites: Understanding of SHACL shapes and RDF validation
 */

import { RdfTemplateEngine } from '@unrdf/kgn/rdf';
import { createStore, validateWithShacl, namedNode, literal, COMMON_PREFIXES } from '@unrdf/core';

/**
 * SHACL shape template for data validation
 */
const SHACL_SHAPE_TEMPLATE = `
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix {{ prefix }}: <{{ namespace }}> .
@prefix {{ dataPrefix }}: <{{ dataNamespace }}> .

{{ dataPrefix }}:{{ shapeName }} a sh:NodeShape ;
    sh:targetClass {{ dataPrefix }}:{{ targetClass }} ;
    sh:closed {{ closed | lower }} ;
    sh:property [
{% for prop in properties %}
        sh:path {{ dataPrefix }}:{{ prop.path }} ;
        {% if prop.datatype %}sh:datatype {{ prop.datatype }} ;{% endif %}
        {% if prop.minCount %}sh:minCount {{ prop.minCount }} ;{% endif %}
        {% if prop.maxCount %}sh:maxCount {{ prop.maxCount }} ;{% endif %}
        {% if prop.pattern %}sh:pattern "{{ prop.pattern }}" ;{% endif %}
        {% if prop.minLength %}sh:minLength {{ prop.minLength }} ;{% endif %}
        {% if prop.maxLength %}sh:maxLength {{ prop.maxLength }} ;{% endif %}
        {% if prop.minInclusive %}sh:minInclusive {{ prop.minInclusive }} ;{% endif %}
        {% if prop.maxInclusive %}sh:maxInclusive {{ prop.maxInclusive }} ;{% endif %}
        {% if prop.nodeKind %}sh:nodeKind sh:{{ prop.nodeKind }} ;{% endif %}
        sh:message "{{ prop.message | default('Validation failed for ' + prop.path) }}"
    {% if not loop.last %}] , [{% else %}]{% endif %}
{% endfor %} .
`.trim();

/**
 * Create sample data for validation
 * @param {Object} store - RDF store
 * @returns {Promise<void>}
 */
async function createValidationTestData(store) {
  const ex = (name) => namedNode(`http://example.org/data/${name}`);
  const prop = (name) => namedNode(`http://example.org/schema/${name}`);
  const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

  // Valid person
  store.add(ex('person1'), rdfType, prop('Person'));
  store.add(ex('person1'), prop('name'), literal('Alice Johnson'));
  store.add(ex('person1'), prop('email'), literal('alice@example.com'));
  store.add(ex('person1'), prop('age'), literal(28));
  store.add(ex('person1'), prop('phone'), literal('+1-555-0101'));

  // Invalid person (missing required fields)
  store.add(ex('person2'), rdfType, prop('Person'));
  store.add(ex('person2'), prop('name'), literal('Bob'));
  // Missing email and age

  // Invalid person (invalid email format)
  store.add(ex('person3'), rdfType, prop('Person'));
  store.add(ex('person3'), prop('name'), literal('Charlie Brown'));
  store.add(ex('person3'), prop('email'), literal('not-an-email'));
  store.add(ex('person3'), prop('age'), literal(150)); // Age too high

  // Invalid person (age not a number)
  store.add(ex('person4'), rdfType, prop('Person'));
  store.add(ex('person4'), prop('name'), literal('Diana Prince'));
  store.add(ex('person4'), prop('email'), literal('diana@example.com'));
  store.add(ex('person4'), prop('age'), literal('thirty')); // Should be integer
}

/**
 * Demonstrate SHACL validation workflow
 * @returns {Promise<void>}
 */
async function shaclValidationDemo() {
  console.log('=== SHACL Validation Workflow ===\n');

  // Create template engine
  const engine = new RdfTemplateEngine({
    prefixes: COMMON_PREFIXES,
  });

  // Define SHACL shape for Person validation
  const shapeDefinition = {
    prefix: 'sh',
    namespace: 'http://www.w3.org/ns/shacl#',
    dataPrefix: 'ex',
    dataNamespace: 'http://example.org/schema/',
    shapeName: 'PersonShape',
    targetClass: 'Person',
    closed: false,
    properties: [
      {
        path: 'name',
        datatype: 'xsd:string',
        minCount: 1,
        maxCount: 1,
        minLength: 2,
        maxLength: 100,
        message: 'Person must have exactly one name (2-100 characters)',
      },
      {
        path: 'email',
        datatype: 'xsd:string',
        minCount: 1,
        maxCount: 1,
        pattern: '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
        message: 'Person must have exactly one valid email address',
      },
      {
        path: 'age',
        datatype: 'xsd:integer',
        minCount: 1,
        maxCount: 1,
        minInclusive: 0,
        maxInclusive: 120,
        message: 'Person must have exactly one age (0-120)',
      },
      {
        path: 'phone',
        datatype: 'xsd:string',
        minCount: 0,
        maxCount: 1,
        pattern: '^\\+?[1-9]\\d{1,14}$',
        message: 'Phone number must be in valid international format',
      },
    ],
  };

  // Generate SHACL shapes
  const shapesRdf = engine.render(SHACL_SHAPE_TEMPLATE, shapeDefinition);

  console.log('Generated SHACL Shapes:');
  console.log('='.repeat(70));
  console.log(shapesRdf);
  console.log('='.repeat(70));

  // Create shapes store
  const shapesStore = createStore();
  await shapesStore.load(shapesRdf, { format: 'turtle' });
  console.log('\n✓ SHACL shapes loaded into store\n');

  // Create data store and populate
  const dataStore = createStore();
  await createValidationTestData(dataStore);
  console.log('✓ Test data created (4 person records)\n');

  // Validate data against shapes
  console.log('--- Validation Results ---\n');
  const validationResult = await validateWithShacl(dataStore, shapesStore);

  if (validationResult.conforms) {
    console.log('✓ All data conforms to SHACL shapes');
  } else {
    console.log('✗ Validation found violations:\n');

    const violations = validationResult.results || [];
    violations.forEach((violation, index) => {
      console.log(`Violation ${index + 1}:`);
      console.log(`  Focus Node: ${violation.focusNode ? violation.focusNode.value : 'N/A'}`);
      console.log(`  Path: ${violation.path ? violation.path.value : 'N/A'}`);
      console.log(`  Message: ${violation.message ? violation.message[0].value : 'No message'}`);
      console.log(`  Severity: ${violation.severity ? violation.severity.value.split('#')[1] : 'N/A'}`);
      console.log('');
    });

    console.log(`Total violations: ${violations.length}`);
  }

  // Demonstrate validation statistics
  console.log('\n--- Validation Statistics ---');
  const stats = {
    totalRecords: 4,
    validRecords: validationResult.conforms ? 4 : 0,
    invalidRecords: validationResult.conforms ? 0 : violations.length,
    totalViolations: validationResult.conforms ? 0 : violations.length,
  };

  console.log(`Total Records: ${stats.totalRecords}`);
  console.log(`Valid Records: ${stats.validRecords}`);
  console.log(`Invalid Records: ${stats.invalidRecords}`);
  console.log(`Total Violations: ${stats.totalViolations}`);

  // Demonstrate fixing violations
  console.log('\n--- Fixing Violations ---');
  console.log('To fix the violations:');
  console.log('1. person2: Add required email and age fields');
  console.log('2. person3: Fix email format and ensure age is within 0-120 range');
  console.log('3. person4: Ensure age is an integer, not a string');

  console.log('\n✓ SHACL validation workflow completed');
}

// Execute demo
try {
  await shaclValidationDemo();
  console.log('\n✓ Example completed successfully');
  process.exit(0);
} catch (error) {
  console.error('\n✗ Example failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
