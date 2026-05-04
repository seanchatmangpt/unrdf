/**
 * @file RDF Integration Demonstration
 * @description Demonstrates working RDF-aware template system
 */

import {
  toTurtle,
  toSparql,
  rdfPrefix,
  blankNode,
  literal,
} from '../src/rdf/filters.js';
import { renderRdfTemplate } from '../src/rdf/index.js';

console.log('='.repeat(60));
console.log('RDF Template System Integration Demo');
console.log('='.repeat(60));

// Demo 1: toTurtle filter
console.log('\n1. Convert RDF quads to Turtle format:');
console.log('-'.repeat(60));
const quads = [
  {
    subject: 'http://example.org/alice',
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://xmlns.com/foaf/0.1/Person',
  },
  {
    subject: 'http://example.org/alice',
    predicate: 'http://xmlns.com/foaf/0.1/name',
    object: '"Alice Smith"',
  },
];

const turtle = toTurtle(quads);
console.log(turtle);

// Demo 2: toSparql filter
console.log('\n2. Generate SPARQL query from pattern:');
console.log('-'.repeat(60));
const pattern = {
  subject: '?person',
  predicate: 'rdf:type',
  object: 'foaf:Person',
};

const sparql = toSparql(pattern);
console.log(sparql);

// Demo 3: RDF filters
console.log('\n3. RDF filter functions:');
console.log('-'.repeat(60));
console.log('rdfPrefix:', rdfPrefix('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'rdf'));
console.log('blankNode:', blankNode('person1'));
console.log('literal (simple):', literal('Hello World'));
console.log('literal (lang):', literal('Bonjour', 'fr'));
console.log('literal (typed):', literal(42, null, 'xsd:integer'));

// Demo 4: Template rendering
console.log('\n4. RDF template rendering:');
console.log('-'.repeat(60));
const template = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

{% for person in people %}
ex:{{ person.id }} rdf:type foaf:Person .
ex:{{ person.id }} foaf:name {{ person.name | literal('en') }} .
{% if person.email %}
ex:{{ person.id }} foaf:mbox <mailto:{{ person.email }}> .
{% endif %}
{% endfor %}
`.trim();

const people = [
  { id: 'alice', name: 'Alice Smith', email: 'alice@example.org' },
  { id: 'bob', name: 'Bob Jones' },
];

const result = renderRdfTemplate(template, { people });
console.log(result);

// Demo 5: SPARQL query generation
console.log('\n5. SPARQL query from template:');
console.log('-'.repeat(60));
const queryTemplate = `
{{ patterns | toSparql({ type: 'select', limit: 100 }) }}
`.trim();

const patterns = [
  { subject: '?book', predicate: 'rdf:type', object: 'ex:Book' },
  { subject: '?book', predicate: 'ex:title', object: '?title' },
  { subject: '?book', predicate: 'ex:author', object: '?author' },
];

const queryResult = renderRdfTemplate(queryTemplate, { patterns });
console.log(queryResult);

console.log('\n' + '='.repeat(60));
console.log('All demonstrations completed successfully!');
console.log('='.repeat(60));
