/**
 * @file OWL Ontology Generation Example
 * @module examples/rdf-kgn/02-ontology-generation
 * @description Build OWL ontologies programmatically using RDF-KGN patterns
 *
 * Time Estimate: 15-20 minutes
 * Difficulty: Intermediate
 * Prerequisites: Understanding of OWL ontologies and RDF schemas
 */

import { createStore, executeQuery, namedNode, literal, quad, COMMON_PREFIXES } from '../../packages/core/src/index.mjs';

/**
 * Generate ontology class definitions
 * @param {Object} classInfo - Class information
 * @returns {Array} Array of quads
 */
function generateClass(classInfo) {
  const { namespace, name, label, comment, subClassOf } = classInfo;
  const classUri = namedNode(`${namespace}${name}`);
  const owl = (n) => namedNode(`http://www.w3.org/2002/07/owl#${n}`);
  const rdfs = (n) => namedNode(`http://www.w3.org/2000/01/rdf-schema#${n}`);
  const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

  const quads = [
    quad(classUri, rdfType, owl('Class')),
    quad(classUri, rdfs('label'), literal(label)),
    quad(classUri, rdfs('comment'), literal(comment)),
  ];

  if (subClassOf) {
    quads.push(quad(classUri, rdfs('subClassOf'), namedNode(`${namespace}${subClassOf}`)));
  }

  return quads;
}

/**
 * Generate ontology property definitions
 * @param {Object} propInfo - Property information
 * @returns {Array} Array of quads
 */
function generateProperty(propInfo) {
  const { namespace, name, label, comment, type, domain, range, functional } = propInfo;
  const propUri = namedNode(`${namespace}${name}`);
  const owl = (n) => namedNode(`http://www.w3.org/2002/07/owl#${n}`);
  const rdfs = (n) => namedNode(`http://www.w3.org/2000/01/rdf-schema#${n}`);
  const xsd = (n) => namedNode(`http://www.w3.org/2001/XMLSchema#${n}`);
  const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

  const quads = [
    quad(propUri, rdfType, owl(type)),
    quad(propUri, rdfs('label'), literal(label)),
    quad(propUri, rdfs('comment'), literal(comment)),
    quad(propUri, rdfs('domain'), namedNode(`${namespace}${domain}`)),
  ];

  // Handle range (could be XSD type or class)
  if (range.startsWith('xsd:')) {
    quads.push(quad(propUri, rdfs('range'), xsd(range.substring(4))));
  } else {
    quads.push(quad(propUri, rdfs('range'), namedNode(`${namespace}${range}`)));
  }

  if (functional) {
    quads.push(quad(propUri, rdfType, owl('FunctionalProperty')));
  }

  return quads;
}

/**
 * Generate a domain ontology from structured data
 * @returns {Promise<void>}
 */
async function generateOntologyDemo() {
  console.log('=== OWL Ontology Generation Example ===\n');

  const namespace = 'http://example.org/library#';
  const store = createStore();

  // Generate ontology metadata
  const ontologyUri = namedNode(namespace);
  const owl = (n) => namedNode(`http://www.w3.org/2002/07/owl#${n}`);
  const rdfs = (n) => namedNode(`http://www.w3.org/2000/01/rdf-schema#${n}`);
  const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

  store.add(quad(ontologyUri, rdfType, owl('Ontology')));
  store.add(quad(ontologyUri, rdfs('label'), literal('Library Domain Ontology')));
  store.add(quad(ontologyUri, rdfs('comment'), literal('An ontology for modeling library resources and relationships')));
  store.add(quad(ontologyUri, owl('versionInfo'), literal('1.0.0')));

  // Define classes
  const classes = [
    { name: 'Resource', label: 'Library Resource', comment: 'Base class for all library resources' },
    { name: 'Book', label: 'Book', comment: 'A published book in the library collection', subClassOf: 'Resource' },
    { name: 'Journal', label: 'Journal', comment: 'A scholarly journal in the library collection', subClassOf: 'Resource' },
    { name: 'Author', label: 'Author', comment: 'A person who has authored library resources' },
    { name: 'Publisher', label: 'Publisher', comment: 'An organization that publishes resources' },
  ];

  for (const cls of classes) {
    const classQuads = generateClass({ namespace, ...cls });
    for (const q of classQuads) {
      store.add(q);
    }
  }

  // Define properties
  const properties = [
    { name: 'title', label: 'Title', comment: 'The title of a resource', type: 'DatatypeProperty', domain: 'Resource', range: 'xsd:string', functional: true },
    { name: 'isbn', label: 'ISBN', comment: 'International Standard Book Number', type: 'DatatypeProperty', domain: 'Book', range: 'xsd:string', functional: true },
    { name: 'publishedYear', label: 'Published Year', comment: 'Year of publication', type: 'DatatypeProperty', domain: 'Resource', range: 'xsd:gYear', functional: true },
    { name: 'author', label: 'Author', comment: 'Author of the resource', type: 'ObjectProperty', domain: 'Resource', range: 'Author', functional: false },
    { name: 'publisher', label: 'Publisher', comment: 'Publisher of the resource', type: 'ObjectProperty', domain: 'Resource', range: 'Publisher', functional: true },
    { name: 'pageCount', label: 'Page Count', comment: 'Number of pages in the resource', type: 'DatatypeProperty', domain: 'Resource', range: 'xsd:integer', functional: true },
  ];

  for (const prop of properties) {
    const propQuads = generateProperty({ namespace, ...prop });
    for (const q of propQuads) {
      store.add(q);
    }
  }

  console.log('Generated OWL Ontology:');
  console.log('='.repeat(60));

  // Query to verify ontology structure
  const classQuery = `
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?class ?label ?comment
    WHERE {
      ?class a owl:Class .
      OPTIONAL { ?class rdfs:label ?label }
      OPTIONAL { ?class rdfs:comment ?comment }
    }
    ORDER BY ?class
  `;

  console.log('=== Ontology Classes ===');
  const classResults = await executeQuery(store, classQuery);
  classResults.forEach((binding, index) => {
    console.log(`${index + 1}. ${binding.class.value.split('#')[1]}`);
    if (binding.label) console.log(`   Label: ${binding.label.value}`);
    if (binding.comment) console.log(`   Description: ${binding.comment.value}`);
  });

  // Query properties
  const propertyQuery = `
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?property ?type ?domain ?range
    WHERE {
      ?property a ?type .
      FILTER(?type IN (owl:DatatypeProperty, owl:ObjectProperty))
      OPTIONAL { ?property rdfs:domain ?domain }
      OPTIONAL { ?property rdfs:range ?range }
    }
    ORDER BY ?property
  `;

  console.log('\n=== Ontology Properties ===');
  const propertyResults = await executeQuery(store, propertyQuery);
  propertyResults.forEach((binding, index) => {
    const propName = binding.property.value.split('#')[1];
    const propType = binding.type.value.split('#')[1];
    console.log(`${index + 1}. ${propName} (${propType})`);
    if (binding.domain) {
      console.log(`   Domain: ${binding.domain.value.split('#')[1]}`);
    }
    if (binding.range) {
      const rangePart = binding.range.value.split('#')[1] || binding.range.value.split('/').pop();
      console.log(`   Range: ${rangePart}`);
    }
  });

  console.log(`\n✓ Generated ontology with ${classResults.length} classes and ${propertyResults.length} properties`);
}

// Execute demo
try {
  await generateOntologyDemo();
  console.log('\n✓ Example completed successfully');
  process.exit(0);
} catch (error) {
  console.error('\n✗ Example failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
