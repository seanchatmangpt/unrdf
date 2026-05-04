/**
 * @file Schema Generator - Generates GraphQL schemas from RDF ontologies
 * @module @unrdf/rdf-graphql/schema
 */

import {
  GraphQLObjectType,
  GraphQLString,
  GraphQLInt,
  GraphQLFloat,
  GraphQLBoolean,
  GraphQLList,
  GraphQLNonNull,
  GraphQLSchema,
  GraphQLID,
} from 'graphql';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

const { namedNode, literal } = dataFactory;

// Zod schemas for validation
const OntologyConfigSchema = z.object({
  namespaces: z.record(z.string()),
  excludeClasses: z.array(z.string()).optional(),
  includeInferred: z.boolean().optional(),
});

/**
 * Maps XSD datatypes to GraphQL scalar types
 * @type {Map<string, GraphQLScalarType>}
 */
const XSD_TO_GRAPHQL = new Map([
  ['http://www.w3.org/2001/XMLSchema#string', GraphQLString],
  ['http://www.w3.org/2001/XMLSchema#integer', GraphQLInt],
  ['http://www.w3.org/2001/XMLSchema#int', GraphQLInt],
  ['http://www.w3.org/2001/XMLSchema#long', GraphQLInt],
  ['http://www.w3.org/2001/XMLSchema#decimal', GraphQLFloat],
  ['http://www.w3.org/2001/XMLSchema#double', GraphQLFloat],
  ['http://www.w3.org/2001/XMLSchema#float', GraphQLFloat],
  ['http://www.w3.org/2001/XMLSchema#boolean', GraphQLBoolean],
  ['http://www.w3.org/2001/XMLSchema#dateTime', GraphQLString],
  ['http://www.w3.org/2001/XMLSchema#anyURI', GraphQLString],
]);

/**
 * RDF Schema Generator - Creates GraphQL schemas from RDF ontologies
 */
export class RDFSchemaGenerator {
  /**
   * @param {object} config - Configuration options
   * @param {Record<string, string>} config.namespaces - Namespace prefixes
   * @param {string[]} [config.excludeClasses] - Classes to exclude from schema
   * @param {boolean} [config.includeInferred] - Include inferred types
   */
  constructor(config) {
    this.config = OntologyConfigSchema.parse(config);
    this.store = createStore();
    this.typeCache = new Map();
    this.propertyCache = new Map();
  }

  /**
   * Load RDF ontology into store
   * @param {string} rdfData - RDF data in Turtle/N-Triples format
   * @param {string} [baseIRI] - Base IRI for relative URIs
   * @returns {Promise<void>}
   */
  async loadOntology(rdfData, baseIRI = 'http://example.org/') {
    await this.store.load(rdfData, 'text/turtle', baseIRI, null);
  }

  /**
   * Generate GraphQL schema from loaded ontology
   * @param {object} options - Generation options
   * @param {string} [options.queryTypeName] - Name for root Query type
   * @returns {GraphQLSchema}
   */
  generateSchema(options = {}) {
    const { queryTypeName = 'Query' } = options;

    // Extract all rdfs:Class and owl:Class definitions
    const classes = this.extractClasses();

    // Build GraphQL types for each class
    const graphqlTypes = new Map();
    for (const classIRI of classes) {
      const typeName = this.iriToTypeName(classIRI);
      if (!typeName || this.config.excludeClasses?.includes(typeName)) {
        continue;
      }

      const objectType = this.buildObjectType(classIRI, typeName, graphqlTypes);
      graphqlTypes.set(classIRI, objectType);
      this.typeCache.set(classIRI, objectType);
    }

    // Build root Query type with finders for each class
    const queryFields = {};
    for (const [classIRI, objectType] of graphqlTypes) {
      const typeName = objectType.name;

      // Single item query: workflow(id: ID!): Workflow
      queryFields[this.lcFirst(typeName)] = {
        type: objectType,
        args: {
          id: { type: new GraphQLNonNull(GraphQLID) },
        },
        description: `Find ${typeName} by ID`,
      };

      // List query: workflows(limit: Int, offset: Int): [Workflow]
      queryFields[this.lcFirst(typeName) + 's'] = {
        type: new GraphQLList(objectType),
        args: {
          limit: { type: GraphQLInt, defaultValue: 10 },
          offset: { type: GraphQLInt, defaultValue: 0 },
        },
        description: `List all ${typeName} instances`,
      };
    }

    const queryType = new GraphQLObjectType({
      name: queryTypeName,
      fields: queryFields,
    });

    return new GraphQLSchema({
      query: queryType,
      types: Array.from(graphqlTypes.values()),
    });
  }

  /**
   * Extract all classes from the ontology
   * @returns {Set<string>} Set of class IRIs
   * @private
   */
  extractClasses() {
    const classes = new Set();

    // Find rdfs:Class instances
    const rdfsClassQuery = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>

      SELECT DISTINCT ?class WHERE {
        {
          ?class a rdfs:Class .
        } UNION {
          ?class a owl:Class .
        }
        FILTER(!isBlank(?class))
      }
    `;

    const results = this.store.query(rdfsClassQuery);
    for (const binding of results) {
      classes.add(binding.get('class').value);
    }

    return classes;
  }

  /**
   * Build GraphQL ObjectType for an RDF class
   * @param {string} classIRI - Class IRI
   * @param {string} typeName - GraphQL type name
   * @param {Map<string, GraphQLObjectType>} typeMap - Map of existing types
   * @returns {GraphQLObjectType}
   * @private
   */
  buildObjectType(classIRI, typeName, typeMap) {
    const fields = {};

    // Add ID field (always present for RDF resources)
    fields.id = {
      type: new GraphQLNonNull(GraphQLID),
      description: 'Resource IRI',
    };

    // Extract properties with this class as domain
    const properties = this.extractProperties(classIRI);

    for (const propIRI of properties) {
      const fieldName = this.iriToFieldName(propIRI);
      if (!fieldName) continue;

      const range = this.getPropertyRange(propIRI);
      const fieldType = this.resolveFieldType(range, typeMap);

      fields[fieldName] = {
        type: fieldType,
        description: this.getPropertyComment(propIRI),
        args: {}, // Can add filtering args here
      };
    }

    return new GraphQLObjectType({
      name: typeName,
      description: this.getClassComment(classIRI),
      fields,
    });
  }

  /**
   * Extract properties for a class
   * @param {string} classIRI - Class IRI
   * @returns {Set<string>} Property IRIs
   * @private
   */
  extractProperties(classIRI) {
    const properties = new Set();

    const query = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

      SELECT DISTINCT ?property WHERE {
        ?property rdfs:domain <${classIRI}> .
        FILTER(!isBlank(?property))
      }
    `;

    const results = this.store.query(query);
    for (const binding of results) {
      properties.add(binding.get('property').value);
    }

    return properties;
  }

  /**
   * Get property range (return type)
   * @param {string} propertyIRI - Property IRI
   * @returns {string|null} Range IRI
   * @private
   */
  getPropertyRange(propertyIRI) {
    const query = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

      SELECT ?range WHERE {
        <${propertyIRI}> rdfs:range ?range .
      }
      LIMIT 1
    `;

    const results = this.store.query(query);
    for (const binding of results) {
      return binding.get('range').value;
    }
    return null;
  }

  /**
   * Resolve field type from RDF range
   * @param {string|null} rangeIRI - Range IRI
   * @param {Map<string, GraphQLObjectType>} typeMap - Available types
   * @returns {GraphQLOutputType}
   * @private
   */
  resolveFieldType(rangeIRI, typeMap) {
    if (!rangeIRI) {
      return GraphQLString; // Default to string
    }

    // Check if it's an XSD datatype
    if (XSD_TO_GRAPHQL.has(rangeIRI)) {
      return XSD_TO_GRAPHQL.get(rangeIRI);
    }

    // Check if it's a known class (object reference)
    if (typeMap.has(rangeIRI)) {
      return typeMap.get(rangeIRI);
    }

    // Check cache
    if (this.typeCache.has(rangeIRI)) {
      return this.typeCache.get(rangeIRI);
    }

    return GraphQLString; // Fallback
  }

  /**
   * Get class comment/description
   * @param {string} classIRI - Class IRI
   * @returns {string|undefined}
   * @private
   */
  getClassComment(classIRI) {
    const query = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

      SELECT ?comment WHERE {
        <${classIRI}> rdfs:comment ?comment .
      }
      LIMIT 1
    `;

    const results = this.store.query(query);
    for (const binding of results) {
      return binding.get('comment').value;
    }
    return undefined;
  }

  /**
   * Get property comment/description
   * @param {string} propertyIRI - Property IRI
   * @returns {string|undefined}
   * @private
   */
  getPropertyComment(propertyIRI) {
    const query = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

      SELECT ?comment WHERE {
        <${propertyIRI}> rdfs:comment ?comment .
      }
      LIMIT 1
    `;

    const results = this.store.query(query);
    for (const binding of results) {
      return binding.get('comment').value;
    }
    return undefined;
  }

  /**
   * Convert IRI to GraphQL type name
   * @param {string} iri - Full IRI
   * @returns {string|null}
   * @private
   */
  iriToTypeName(iri) {
    // Extract local name from IRI
    const match = iri.match(/[#/]([^#/]+)$/);
    if (!match) return null;

    const localName = match[1];
    // Capitalize first letter
    return localName.charAt(0).toUpperCase() + localName.slice(1);
  }

  /**
   * Convert IRI to GraphQL field name
   * @param {string} iri - Full IRI
   * @returns {string|null}
   * @private
   */
  iriToFieldName(iri) {
    const match = iri.match(/[#/]([^#/]+)$/);
    if (!match) return null;

    const localName = match[1];
    // Lowercase first letter, camelCase
    return localName.charAt(0).toLowerCase() + localName.slice(1);
  }

  /**
   * Lowercase first character
   * @param {string} str - Input string
   * @returns {string}
   * @private
   */
  lcFirst(str) {
    return str.charAt(0).toLowerCase() + str.slice(1);
  }

  /**
   * Get the store instance
   * @returns {object} Oxigraph store
   */
  getStore() {
    return this.store;
  }
}

/**
 * Convenience function to generate schema from RDF
 * @param {string} rdfData - RDF ontology data
 * @param {object} config - Configuration
 * @returns {Promise<GraphQLSchema>}
 */
export async function generateSchemaFromRDF(rdfData, config = {}) {
  const generator = new RDFSchemaGenerator({
    namespaces: config.namespaces || {},
    excludeClasses: config.excludeClasses,
    includeInferred: config.includeInferred,
  });

  await generator.loadOntology(rdfData);
  return generator.generateSchema(config);
}
