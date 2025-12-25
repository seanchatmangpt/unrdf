/**
 * @file RDF GraphQL Adapter - Main entry point for type-safe GraphQL interface
 * @module @unrdf/rdf-graphql
 */

import { graphql } from 'graphql';
import { createStore } from '@unrdf/oxigraph';
import { RDFSchemaGenerator } from './schema-generator.mjs';
import { RDFResolverFactory } from './resolver.mjs';
import { SPARQLQueryBuilder } from './query-builder.mjs';
import { z } from 'zod';

// Zod schemas for validation
const AdapterConfigSchema = z.object({
  namespaces: z.record(z.string()).optional(),
  excludeClasses: z.array(z.string()).optional(),
  includeInferred: z.boolean().optional(),
  enableCache: z.boolean().optional(),
  typeMapping: z.record(z.string()).optional(),
});

/**
 * RDF GraphQL Adapter - Generates GraphQL schemas from RDF ontologies
 * and provides type-safe query execution
 */
export class RDFGraphQLAdapter {
  /**
   * @param {object} [config] - Adapter configuration
   * @param {Record<string, string>} [config.namespaces] - Namespace prefixes
   * @param {string[]} [config.excludeClasses] - Classes to exclude from schema
   * @param {boolean} [config.includeInferred] - Include inferred types
   * @param {boolean} [config.enableCache] - Enable query result caching
   * @param {Record<string, string>} [config.typeMapping] - GraphQL type to RDF class mapping
   */
  constructor(config = {}) {
    this.config = AdapterConfigSchema.parse(config);
    this.store = createStore();
    this.schema = null;
    this.resolvers = null;
    this.schemaGenerator = new RDFSchemaGenerator({
      namespaces: this.config.namespaces || {},
      excludeClasses: this.config.excludeClasses,
      includeInferred: this.config.includeInferred,
    });
    this.resolverFactory = null;
  }

  /**
   * Load RDF ontology into the store
   * @param {string} rdfData - RDF data in Turtle/N-Triples format
   * @param {string} [format='text/turtle'] - RDF format
   * @param {string} [baseIRI] - Base IRI for relative URIs
   * @returns {Promise<void>}
   */
  async loadOntology(rdfData, format = 'text/turtle', baseIRI = 'http://example.org/') {
    await this.schemaGenerator.loadOntology(rdfData, baseIRI);
    await this.store.load(rdfData, format, baseIRI, null);
  }

  /**
   * Load RDF instance data into the store
   * @param {string} rdfData - RDF instance data
   * @param {string} [format='text/turtle'] - RDF format
   * @param {string} [baseIRI] - Base IRI
   * @returns {Promise<void>}
   */
  async loadData(rdfData, format = 'text/turtle', baseIRI = 'http://example.org/') {
    await this.store.load(rdfData, format, baseIRI, null);
  }

  /**
   * Generate GraphQL schema from loaded ontology
   * @param {object} [options] - Schema generation options
   * @param {string} [options.queryTypeName='Query'] - Name for root Query type
   * @returns {GraphQLSchema}
   */
  generateSchema(options = {}) {
    this.schema = this.schemaGenerator.generateSchema(options);

    // Create resolvers
    this.resolverFactory = new RDFResolverFactory(this.store, {
      namespaces: this.config.namespaces,
      typeMapping: this.config.typeMapping,
      enableCache: this.config.enableCache,
    });

    this.resolvers = this.resolverFactory.createResolvers(this.schema);

    return this.schema;
  }

  /**
   * Execute GraphQL query
   * @param {string} query - GraphQL query string
   * @param {object} [variables] - Query variables
   * @param {object} [context] - Execution context
   * @returns {Promise<object>} Query result
   */
  async executeQuery(query, variables = {}, context = {}) {
    if (!this.schema) {
      throw new Error('Schema not generated. Call generateSchema() first.');
    }

    const result = await graphql({
      schema: this.schema,
      source: query,
      variableValues: variables,
      contextValue: context,
      rootValue: this.resolvers.Query,
    });

    return result;
  }

  /**
   * Execute SPARQL query directly
   * @param {string} query - SPARQL query
   * @returns {Array<object>} Query results
   */
  executeSPARQL(query) {
    const results = this.store.query(query);
    const bindings = [];

    for (const binding of results) {
      const obj = {};
      for (const [key, value] of binding.entries()) {
        obj[key] = value.value;
      }
      bindings.push(obj);
    }

    return bindings;
  }

  /**
   * Get the GraphQL schema
   * @returns {GraphQLSchema|null}
   */
  getSchema() {
    return this.schema;
  }

  /**
   * Get the Oxigraph store instance
   * @returns {object}
   */
  getStore() {
    return this.store;
  }

  /**
   * Get schema SDL (Schema Definition Language)
   * @returns {Promise<string|null>}
   */
  async getSchemaSDL() {
    if (!this.schema) return null;

    // Import printSchema at runtime to avoid circular dependencies
    const { printSchema } = await import('graphql');
    return printSchema(this.schema);
  }

  /**
   * Clear query result cache
   */
  clearCache() {
    this.resolverFactory?.clearCache();
  }

  /**
   * Get cache statistics
   * @returns {{size: number, enabled: boolean}}
   */
  getCacheStats() {
    return this.resolverFactory?.getCacheStats() || { enabled: false, size: 0 };
  }

  /**
   * Introspect ontology classes
   * @returns {Array<{iri: string, label: string, comment: string}>}
   */
  introspectClasses() {
    const query = `
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ?class ?label ?comment WHERE {
  {
    ?class a rdfs:Class .
  } UNION {
    ?class a owl:Class .
  }
  OPTIONAL { ?class rdfs:label ?label . }
  OPTIONAL { ?class rdfs:comment ?comment . }
  FILTER(!isBlank(?class))
}
    `.trim();

    return this.executeSPARQL(query);
  }

  /**
   * Introspect ontology properties
   * @returns {Array<{iri: string, domain: string, range: string, label: string}>}
   */
  introspectProperties() {
    const query = `
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ?property ?domain ?range ?label WHERE {
  {
    ?property a rdf:Property .
  } UNION {
    ?property a owl:ObjectProperty .
  } UNION {
    ?property a owl:DatatypeProperty .
  }
  OPTIONAL { ?property rdfs:domain ?domain . }
  OPTIONAL { ?property rdfs:range ?range . }
  OPTIONAL { ?property rdfs:label ?label . }
  FILTER(!isBlank(?property))
}
    `.trim();

    return this.executeSPARQL(query);
  }

  /**
   * Get statistics about the RDF store
   * @returns {{tripleCount: number, classCount: number, instanceCount: number}}
   */
  async getStatistics() {
    const tripleCountQuery = 'SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o . }';
    const classCountQuery = `
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
SELECT (COUNT(DISTINCT ?class) AS ?count) WHERE {
  { ?class a rdfs:Class . } UNION { ?class a owl:Class . }
}
    `.trim();

    const instanceCountQuery = 'SELECT (COUNT(DISTINCT ?s) AS ?count) WHERE { ?s a ?type . }';

    const tripleCount = parseInt(this.executeSPARQL(tripleCountQuery)[0]?.count || '0', 10);
    const classCount = parseInt(this.executeSPARQL(classCountQuery)[0]?.count || '0', 10);
    const instanceCount = parseInt(this.executeSPARQL(instanceCountQuery)[0]?.count || '0', 10);

    return {
      tripleCount,
      classCount,
      instanceCount,
    };
  }
}

/**
 * Create RDF GraphQL Adapter with fluent API
 * @param {object} [config] - Adapter configuration
 * @returns {RDFGraphQLAdapter}
 */
export function createAdapter(config = {}) {
  return new RDFGraphQLAdapter(config);
}

// Re-export key components
export { RDFSchemaGenerator } from './schema-generator.mjs';
export { SPARQLQueryBuilder } from './query-builder.mjs';
export { RDFResolverFactory } from './resolver.mjs';
