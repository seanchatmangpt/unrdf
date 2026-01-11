/**
 * @file RDF Templates Integration Tests
 * @description Comprehensive tests for all RDF templates ensuring valid output
 */

import { describe, it, expect, beforeAll } from 'vitest';
import nunjucks from 'nunjucks';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { readFileSync } from 'fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

describe('RDF Templates Integration', () => {
  let env;

  beforeAll(() => {
    // Create Nunjucks environment with template path
    env = new nunjucks.Environment(
      new nunjucks.FileSystemLoader(join(__dirname, '../src/templates/rdf')),
      { autoescape: false }
    );

    // Add date filter for templates that use it
    env.addFilter('date', function(value, format) {
      if (value === undefined || value === null) {
        value = new Date();
      }
      // Simple date formatting - just return ISO date for YYYY-MM-DD
      if (format === 'YYYY-MM-DD') {
        return value.toISOString().split('T')[0];
      }
      return value.toString();
    });
  });

  describe('ontology.njk', () => {
    it('should generate valid OWL ontology with minimal data', () => {
      const data = {
        ontologyIRI: 'http://example.org/ontology/minimal',
        title: 'Minimal Ontology',
        now: new Date()
      };

      const result = env.render('ontology.njk', data);

      expect(result).toContain('@prefix owl:');
      expect(result).toContain('@prefix rdfs:');
      expect(result).toContain('<http://example.org/ontology/minimal> a owl:Ontology');
      expect(result).toContain('dc:title "Minimal Ontology"@en');
    });

    it('should generate complete OWL ontology with classes and properties', () => {
      const data = {
        ontologyIRI: 'http://example.org/ontology/library',
        version: '1.0.0',
        title: 'Library Ontology',
        description: 'An ontology for library systems',
        creator: 'http://example.org/people/librarian',
        created: '2026-01-11',
        license: 'http://creativecommons.org/licenses/by/4.0/',
        now: new Date('2026-01-11'),
        classes: [
          {
            iri: 'http://example.org/ontology/library#Book',
            label: 'Book',
            comment: 'Represents a book in the library'
          },
          {
            iri: 'http://example.org/ontology/library#Author',
            label: 'Author',
            comment: 'Represents an author',
            subClassOf: 'http://xmlns.com/foaf/0.1/Person'
          }
        ],
        objectProperties: [
          {
            iri: 'http://example.org/ontology/library#writtenBy',
            label: 'written by',
            comment: 'Links a book to its author',
            domain: 'http://example.org/ontology/library#Book',
            range: 'http://example.org/ontology/library#Author'
          }
        ],
        datatypeProperties: [
          {
            iri: 'http://example.org/ontology/library#isbn',
            label: 'ISBN',
            comment: 'The ISBN number',
            domain: 'http://example.org/ontology/library#Book',
            range: 'http://www.w3.org/2001/XMLSchema#string'
          }
        ]
      };

      const result = env.render('ontology.njk', data);

      // Check metadata
      expect(result).toContain('owl:versionInfo "1.0.0"');
      expect(result).toContain('dc:description "An ontology for library systems"@en');
      expect(result).toContain('dcterms:creator <http://example.org/people/librarian>');
      expect(result).toContain('dcterms:created "2026-01-11"^^xsd:date');
      expect(result).toContain('dcterms:license <http://creativecommons.org/licenses/by/4.0/>');

      // Check classes
      expect(result).toContain('# Classes');
      expect(result).toContain('<http://example.org/ontology/library#Book> a owl:Class');
      expect(result).toContain('rdfs:label "Book"@en');
      expect(result).toContain('<http://example.org/ontology/library#Author> a owl:Class');
      expect(result).toContain('rdfs:subClassOf <http://xmlns.com/foaf/0.1/Person>');

      // Check object properties
      expect(result).toContain('# Object Properties');
      expect(result).toContain('<http://example.org/ontology/library#writtenBy> a owl:ObjectProperty');
      expect(result).toContain('rdfs:domain <http://example.org/ontology/library#Book>');
      expect(result).toContain('rdfs:range <http://example.org/ontology/library#Author>');

      // Check datatype properties
      expect(result).toContain('# Datatype Properties');
      expect(result).toContain('<http://example.org/ontology/library#isbn> a owl:DatatypeProperty');
      expect(result).toContain('rdfs:range <http://www.w3.org/2001/XMLSchema#string>');
    });
  });

  describe('schema.njk', () => {
    it('should generate valid RDFS schema', () => {
      const data = {
        schemaIRI: 'http://example.org/schema/vocab',
        title: 'Document Vocabulary',
        description: 'A vocabulary for documents',
        version: '1.0.0',
        now: new Date(),
        classes: [
          {
            iri: 'http://example.org/schema/vocab#Document',
            label: 'Document',
            comment: 'A textual document'
          },
          {
            iri: 'http://example.org/schema/vocab#Article',
            label: 'Article',
            comment: 'A published article',
            subClassOf: 'http://example.org/schema/vocab#Document'
          }
        ],
        properties: [
          {
            iri: 'http://example.org/schema/vocab#title',
            label: 'title',
            comment: 'The title of a document',
            domain: 'http://example.org/schema/vocab#Document',
            range: 'http://www.w3.org/2001/XMLSchema#string'
          }
        ]
      };

      const result = env.render('schema.njk', data);

      // Check prefixes
      expect(result).toContain('@prefix rdfs:');
      expect(result).toContain('@prefix xsd:');

      // Check schema metadata
      expect(result).toContain('# Schema Metadata');
      expect(result).toContain('<http://example.org/schema/vocab> a rdfs:Resource');
      expect(result).toContain('rdfs:label "Document Vocabulary"@en');
      expect(result).toContain('rdfs:comment "A vocabulary for documents"@en');

      // Check classes
      expect(result).toContain('# Class Definitions');
      expect(result).toContain('<http://example.org/schema/vocab#Document> a rdfs:Class');
      expect(result).toContain('rdfs:subClassOf <http://example.org/schema/vocab#Document>');

      // Check properties
      expect(result).toContain('# Property Definitions');
      expect(result).toContain('<http://example.org/schema/vocab#title> a rdf:Property');
    });
  });

  describe('dataset.njk', () => {
    it('should generate valid DCAT dataset metadata', () => {
      const data = {
        datasetIRI: 'http://example.org/datasets/census-2020',
        title: 'Census Data 2020',
        description: 'Population census data for 2020',
        publisher: 'http://example.org/organizations/census-bureau',
        creator: 'http://example.org/people/statistician',
        issued: '2020-12-31',
        license: 'http://creativecommons.org/licenses/by/4.0/',
        now: new Date(),
        keywords: ['census', 'population', 'statistics', '2020'],
        theme: ['http://example.org/themes/demographics'],
        spatial: 'http://sws.geonames.org/6252001/',
        temporal: '2020-01-01/2020-12-31',
        distributions: [
          {
            iri: 'http://example.org/datasets/census-2020/csv',
            title: 'CSV Distribution',
            format: 'text/csv',
            accessURL: 'http://example.org/data/census-2020.csv',
            downloadURL: 'http://example.org/downloads/census-2020.csv'
          },
          {
            iri: 'http://example.org/datasets/census-2020/json',
            title: 'JSON Distribution',
            format: 'application/json',
            accessURL: 'http://example.org/data/census-2020.json'
          }
        ]
      };

      const result = env.render('dataset.njk', data);

      // Check prefixes
      expect(result).toContain('@prefix dcat:');
      expect(result).toContain('@prefix dcterms:');

      // Check dataset
      expect(result).toContain('# Dataset Description');
      expect(result).toContain('<http://example.org/datasets/census-2020> a dcat:Dataset');
      expect(result).toContain('dcterms:title "Census Data 2020"@en');
      expect(result).toContain('dcterms:description "Population census data for 2020"@en');
      expect(result).toContain('dcterms:publisher <http://example.org/organizations/census-bureau>');
      expect(result).toContain('dcterms:issued "2020-12-31"^^xsd:date');
      expect(result).toContain('dcterms:license <http://creativecommons.org/licenses/by/4.0/>');

      // Check keywords
      expect(result).toContain('dcat:keyword "census"@en');
      expect(result).toContain('dcat:keyword "population"@en');

      // Check distributions
      expect(result).toContain('# Distributions');
      expect(result).toContain('<http://example.org/datasets/census-2020/csv> a dcat:Distribution');
      expect(result).toContain('dcat:mediaType "text/csv"');
      expect(result).toContain('dcat:accessURL <http://example.org/data/census-2020.csv>');
      expect(result).toContain('dcat:downloadURL <http://example.org/downloads/census-2020.csv>');
    });
  });

  describe('vocabulary.njk', () => {
    it('should generate valid SKOS concept scheme', () => {
      const data = {
        schemeIRI: 'http://example.org/vocabularies/topics',
        title: 'Topic Vocabulary',
        description: 'A controlled vocabulary of topics',
        creator: 'http://example.org/people/taxonomist',
        created: '2026-01-11',
        now: new Date('2026-01-11'),
        concepts: [
          {
            iri: 'http://example.org/vocabularies/topics#Science',
            prefLabel: 'Science',
            altLabel: ['Natural Science', 'Sciences'],
            definition: 'Study of the natural world',
            notation: 'SCI',
            narrower: [
              'http://example.org/vocabularies/topics#Physics',
              'http://example.org/vocabularies/topics#Biology'
            ]
          },
          {
            iri: 'http://example.org/vocabularies/topics#Physics',
            prefLabel: 'Physics',
            definition: 'The study of matter and energy',
            notation: 'PHY',
            broader: 'http://example.org/vocabularies/topics#Science',
            related: ['http://example.org/vocabularies/topics#Mathematics']
          }
        ]
      };

      const result = env.render('vocabulary.njk', data);

      // Check prefixes
      expect(result).toContain('@prefix skos:');
      expect(result).toContain('@prefix dcterms:');

      // Check concept scheme
      expect(result).toContain('# Concept Scheme');
      expect(result).toContain('<http://example.org/vocabularies/topics> a skos:ConceptScheme');
      expect(result).toContain('dc:title "Topic Vocabulary"@en');
      expect(result).toContain('skos:hasTopConcept');

      // Check concepts
      expect(result).toContain('# Concepts');
      expect(result).toContain('<http://example.org/vocabularies/topics#Science> a skos:Concept');
      expect(result).toContain('skos:prefLabel "Science"@en');
      expect(result).toContain('skos:altLabel "Natural Science"@en');
      expect(result).toContain('skos:definition "Study of the natural world"@en');
      expect(result).toContain('skos:notation "SCI"');
      expect(result).toContain('skos:narrower <http://example.org/vocabularies/topics#Physics>');
      expect(result).toContain('skos:broader <http://example.org/vocabularies/topics#Science>');
      expect(result).toContain('skos:related <http://example.org/vocabularies/topics#Mathematics>');
    });
  });

  describe('shapes.njk', () => {
    it('should generate valid SHACL shapes', () => {
      const data = {
        shapesGraphIRI: 'http://example.org/shapes/person',
        title: 'Person Validation Shapes',
        description: 'SHACL shapes for validating person data',
        shapes: [
          {
            iri: 'http://example.org/shapes/person#PersonShape',
            targetClass: 'http://xmlns.com/foaf/0.1/Person',
            label: 'Person Shape',
            description: 'Validates person instances',
            closed: false,
            properties: [
              {
                path: 'http://xmlns.com/foaf/0.1/name',
                minCount: 1,
                maxCount: 1,
                datatype: 'http://www.w3.org/2001/XMLSchema#string',
                minLength: 1,
                maxLength: 200
              },
              {
                path: 'http://xmlns.com/foaf/0.1/mbox',
                minCount: 0,
                maxCount: 5,
                datatype: 'http://www.w3.org/2001/XMLSchema#string',
                pattern: '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$'
              },
              {
                path: 'http://xmlns.com/foaf/0.1/age',
                minCount: 0,
                maxCount: 1,
                datatype: 'http://www.w3.org/2001/XMLSchema#integer',
                minInclusive: 0,
                maxInclusive: 150
              },
              {
                path: 'http://xmlns.com/foaf/0.1/knows',
                minCount: 0,
                class: 'http://xmlns.com/foaf/0.1/Person'
              }
            ]
          }
        ]
      };

      const result = env.render('shapes.njk', data);

      // Check prefixes
      expect(result).toContain('@prefix sh:');
      expect(result).toContain('@prefix rdfs:');

      // Check shapes graph
      expect(result).toContain('# Shapes Graph Metadata');
      expect(result).toContain('<http://example.org/shapes/person> a sh:ShapesGraph');

      // Check shape
      expect(result).toContain('# Shape Definitions');
      expect(result).toContain('<http://example.org/shapes/person#PersonShape> a sh:NodeShape');
      expect(result).toContain('sh:targetClass <http://xmlns.com/foaf/0.1/Person>');
      expect(result).toContain('rdfs:label "Person Shape"@en');

      // Check property constraints
      expect(result).toContain('sh:property [');
      expect(result).toContain('sh:path <http://xmlns.com/foaf/0.1/name>');
      expect(result).toContain('sh:minCount 1');
      expect(result).toContain('sh:maxCount 1');
      expect(result).toContain('sh:datatype <http://www.w3.org/2001/XMLSchema#string>');
      expect(result).toContain('sh:minLength 1');
      expect(result).toContain('sh:maxLength 200');
      expect(result).toContain('sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"');
      expect(result).toContain('sh:minInclusive 0');
      expect(result).toContain('sh:maxInclusive 150');
      expect(result).toContain('sh:class <http://xmlns.com/foaf/0.1/Person>');
    });
  });

  describe('sparql-queries.njk', () => {
    it('should generate SPARQL query collection', () => {
      const data = {
        title: 'Person Queries',
        description: 'Common queries for person data',
        queries: [
          {
            name: 'getAllPersons',
            description: 'Retrieve all persons with their names',
            type: 'SELECT',
            prefixes: {
              foaf: 'http://xmlns.com/foaf/0.1/',
              rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
            },
            variables: ['person', 'name'],
            where: [
              '?person a foaf:Person',
              '?person foaf:name ?name'
            ],
            orderBy: ['name'],
            limit: 100
          },
          {
            name: 'constructPersonGraph',
            description: 'Construct a graph of person relationships',
            type: 'CONSTRUCT',
            prefixes: {
              foaf: 'http://xmlns.com/foaf/0.1/'
            },
            construct: [
              '?person foaf:name ?name',
              '?person foaf:knows ?friend'
            ],
            where: [
              '?person a foaf:Person',
              '?person foaf:name ?name',
              'OPTIONAL { ?person foaf:knows ?friend }'
            ]
          },
          {
            name: 'hasPersons',
            description: 'Check if any persons exist',
            type: 'ASK',
            prefixes: {
              foaf: 'http://xmlns.com/foaf/0.1/'
            },
            where: [
              '?person a foaf:Person'
            ]
          },
          {
            name: 'describePerson',
            description: 'Describe a specific person',
            type: 'DESCRIBE',
            prefixes: {
              ex: 'http://example.org/'
            },
            resources: ['ex:JohnDoe']
          }
        ]
      };

      const result = env.render('sparql-queries.njk', data);

      // Note: Nunjucks comments {# #} are stripped from output
      // We verify the actual SPARQL content instead

      // Check SELECT query
      expect(result).toContain('SELECT ?person ?name');
      expect(result).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
      expect(result).toContain('?person a foaf:Person');
      expect(result).toContain('ORDER BY ?name');
      expect(result).toContain('LIMIT 100');

      // Check CONSTRUCT query
      expect(result).toContain('CONSTRUCT {');
      expect(result).toContain('?person foaf:name ?name');
      expect(result).toContain('?person foaf:knows ?friend');

      // Check ASK query
      expect(result).toContain('ASK');

      // Check DESCRIBE query
      expect(result).toContain('DESCRIBE ex:JohnDoe');
    });
  });

  describe('jsonld-context.njk', () => {
    it('should generate valid JSON-LD context', () => {
      const data = {
        contextIRI: 'http://example.org/contexts/person',
        vocab: 'http://schema.org/',
        base: 'http://example.org/',
        prefixes: {
          foaf: 'http://xmlns.com/foaf/0.1/',
          xsd: 'http://www.w3.org/2001/XMLSchema#'
        },
        terms: {
          Person: 'foaf:Person',
          name: 'foaf:name',
          email: {
            id: 'foaf:mbox',
            type: '@id'
          },
          age: {
            id: 'foaf:age',
            type: 'xsd:integer'
          },
          knows: {
            id: 'foaf:knows',
            type: '@id',
            container: '@set'
          }
        }
      };

      const result = env.render('jsonld-context.njk', data);

      // Parse JSON to validate
      const context = JSON.parse(result);

      expect(context).toHaveProperty('@context');
      expect(context['@context']).toHaveProperty('@id', 'http://example.org/contexts/person');
      expect(context['@context']).toHaveProperty('@vocab', 'http://schema.org/');
      expect(context['@context']).toHaveProperty('@base', 'http://example.org/');
      expect(context['@context']).toHaveProperty('foaf', 'http://xmlns.com/foaf/0.1/');
      expect(context['@context']).toHaveProperty('xsd', 'http://www.w3.org/2001/XMLSchema#');

      // Check term mappings
      expect(context['@context'].Person).toBe('foaf:Person');
      expect(context['@context'].name).toBe('foaf:name');

      // Check complex term definitions
      expect(context['@context'].email).toEqual({
        '@id': 'foaf:mbox',
        '@type': '@id'
      });

      expect(context['@context'].age).toEqual({
        '@id': 'foaf:age',
        '@type': 'xsd:integer'
      });

      expect(context['@context'].knows).toEqual({
        '@id': 'foaf:knows',
        '@type': '@id',
        '@container': '@set'
      });
    });

    it('should generate minimal JSON-LD context', () => {
      const data = {
        terms: {
          name: 'http://schema.org/name',
          email: 'http://schema.org/email'
        }
      };

      const result = env.render('jsonld-context.njk', data);

      const context = JSON.parse(result);

      expect(context).toHaveProperty('@context');
      expect(context['@context']).toHaveProperty('name', 'http://schema.org/name');
      expect(context['@context']).toHaveProperty('email', 'http://schema.org/email');
      expect(context['@context']).not.toHaveProperty('@id');
      expect(context['@context']).not.toHaveProperty('@vocab');
    });
  });

  describe('Template Integration', () => {
    it('should generate all templates without errors', () => {
      const templates = [
        { name: 'ontology.njk', data: { ontologyIRI: 'http://test.org/o', title: 'Test', now: new Date() } },
        { name: 'schema.njk', data: { schemaIRI: 'http://test.org/s', title: 'Test', now: new Date() } },
        { name: 'dataset.njk', data: { datasetIRI: 'http://test.org/d', title: 'Test', description: 'Test', now: new Date() } },
        { name: 'vocabulary.njk', data: { schemeIRI: 'http://test.org/v', title: 'Test', now: new Date() } },
        { name: 'shapes.njk', data: { shapesGraphIRI: 'http://test.org/sh', shapes: [] } },
        { name: 'sparql-queries.njk', data: { title: 'Test', queries: [] } },
        { name: 'jsonld-context.njk', data: { terms: { name: 'http://schema.org/name' } } }
      ];

      for (const { name, data } of templates) {
        const result = env.render(name, data);
        expect(result).toBeTruthy();
        expect(result.length).toBeGreaterThan(0);
      }
    });
  });
});
