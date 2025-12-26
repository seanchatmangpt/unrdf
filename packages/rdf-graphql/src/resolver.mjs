/**
 * @file Resolver - GraphQL resolvers using Oxigraph SPARQL engine
 * @module @unrdf/rdf-graphql/resolver
 */

import { SPARQLQueryBuilder } from './query-builder.mjs';
import { z } from 'zod';

// Zod schemas for validation
const ResolverConfigSchema = z.object({
  namespaces: z.record(z.string()).optional(),
  typeMapping: z.record(z.string()).optional(),
  enableCache: z.boolean().optional(),
});

/**
 * RDF GraphQL Resolver Factory
 */
export class RDFResolverFactory {
  /**
   * @param {object} store - Oxigraph store instance
   * @param {object} config - Resolver configuration
   * @param {Record<string, string>} [config.namespaces] - Namespace prefixes
   * @param {Record<string, string>} [config.typeMapping] - GraphQL type to RDF class mapping
   * @param {boolean} [config.enableCache] - Enable query result caching
   */
  constructor(store, config = {}) {
    this.store = store;
    this.config = ResolverConfigSchema.parse(config);
    this.queryBuilder = new SPARQLQueryBuilder({
      namespaces: config.namespaces,
    });
    this.typeMapping = config.typeMapping || new Map();
    this.cache = config.enableCache ? new Map() : null;
  }

  /**
   * Create resolvers for a GraphQL schema
   * @param {GraphQLSchema} schema - GraphQL schema
   * @returns {Record<string, any>} Resolver map
   */
  createResolvers(schema) {
    const resolvers = {
      Query: {},
    };

    const queryType = schema.getQueryType();
    if (!queryType) return resolvers;

    const fields = queryType.getFields();

    for (const [fieldName, field] of Object.entries(fields)) {
      // Determine if it's a single item or list query
      const isList = fieldName.endsWith('s');
      const typeName = isList
        ? fieldName.charAt(0).toUpperCase() + fieldName.slice(1, -1)
        : fieldName.charAt(0).toUpperCase() + fieldName.slice(1);

      const typeIRI = this.typeMapping[typeName] || this.typeNameToIRI(typeName);

      if (isList) {
        resolvers.Query[fieldName] = this.createListResolver(typeIRI);
      } else {
        resolvers.Query[fieldName] = this.createItemResolver(typeIRI);
      }
    }

    return resolvers;
  }

  /**
   * Create resolver for single item queries
   * @param {string} typeIRI - RDF class IRI
   * @returns {Function} Resolver function
   * @private
   */
  createItemResolver(typeIRI) {
    return async (parent, args, context, info) => {
      const { id } = args;
      const resourceIRI = this.resolveIRI(id);

      // Check cache
      const cacheKey = `item:${resourceIRI}:${this.hashInfo(info)}`;
      if (this.cache?.has(cacheKey)) {
        return this.cache.get(cacheKey);
      }

      // Build SPARQL query
      const sparqlQuery = this.queryBuilder.buildQueryForResource(
        info,
        resourceIRI,
        typeIRI
      );

      // Execute query
      const bindings = this.executeSPARQL(sparqlQuery);

      // Convert SPARQL results to GraphQL object
      const result = this.bindingsToObject(bindings, resourceIRI);

      // Cache result
      if (this.cache) {
        this.cache.set(cacheKey, result);
      }

      return result;
    };
  }

  /**
   * Create resolver for list queries
   * @param {string} typeIRI - RDF class IRI
   * @returns {Function} Resolver function
   * @private
   */
  createListResolver(typeIRI) {
    return async (parent, args, context, info) => {
      const { limit = 10, offset = 0, ...filters } = args;

      // Check cache
      const cacheKey = `list:${typeIRI}:${limit}:${offset}:${JSON.stringify(filters)}:${this.hashInfo(info)}`;
      if (this.cache?.has(cacheKey)) {
        return this.cache.get(cacheKey);
      }

      // Build SPARQL query
      const sparqlQuery = filters && Object.keys(filters).length > 0
        ? this.queryBuilder.buildFilteredQuery(info, typeIRI, filters)
        : this.queryBuilder.buildListQuery(info, typeIRI, { limit, offset });

      // Execute query
      const bindings = this.executeSPARQL(sparqlQuery);

      // Group bindings by subject and convert to objects
      const results = this.bindingsToObjects(bindings);

      // Cache results
      if (this.cache) {
        this.cache.set(cacheKey, results);
      }

      return results;
    };
  }

  /**
   * Execute SPARQL query against store
   * @param {string} query - SPARQL query
   * @returns {Array<Map<string, any>>} Query bindings
   * @private
   */
  executeSPARQL(query) {
    try {
      const results = this.store.query(query);
      const bindings = [];

      for (const binding of results) {
        bindings.push(binding);
      }

      return bindings;
    } catch (error) {
      console.error('SPARQL query error:', error);
      console.error('Query:', query);
      throw new Error(`SPARQL query failed: ${error.message}`);
    }
  }

  /**
   * Convert SPARQL bindings to GraphQL object (single result)
   * @param {Array<Map<string, any>>} bindings - SPARQL bindings
   * @param {string} resourceIRI - Resource IRI
   * @returns {object|null} GraphQL object
   * @private
   */
  bindingsToObject(bindings, resourceIRI) {
    if (bindings.length === 0) return null;

    const result = { id: resourceIRI };
    const binding = bindings[0];

    for (const [varName, value] of binding.entries()) {
      if (varName === 's') continue; // Skip subject variable

      result[varName] = this.rdfTermToValue(value);
    }

    return result;
  }

  /**
   * Convert SPARQL bindings to GraphQL objects (multiple results)
   * @param {Array<Map<string, any>>} bindings - SPARQL bindings
   * @returns {Array<object>} GraphQL objects
   * @private
   */
  bindingsToObjects(bindings) {
    const subjectMap = new Map();

    for (const binding of bindings) {
      const subject = binding.get('s');
      if (!subject) continue;

      const subjectIRI = subject.value;

      if (!subjectMap.has(subjectIRI)) {
        subjectMap.set(subjectIRI, { id: subjectIRI });
      }

      const obj = subjectMap.get(subjectIRI);

      for (const [varName, value] of binding.entries()) {
        if (varName === 's') continue;

        // Handle multi-valued properties (keep as array or single value)
        const convertedValue = this.rdfTermToValue(value);

        if (obj[varName] !== undefined && obj[varName] !== convertedValue) {
          // Multiple values - convert to array
          if (Array.isArray(obj[varName])) {
            if (!obj[varName].includes(convertedValue)) {
              obj[varName].push(convertedValue);
            }
          } else {
            obj[varName] = [obj[varName], convertedValue];
          }
        } else {
          obj[varName] = convertedValue;
        }
      }
    }

    return Array.from(subjectMap.values());
  }

  /**
   * Convert RDF term to JavaScript value
   * @param {object} term - RDF term
   * @returns {any} JavaScript value
   * @private
   */
  rdfTermToValue(term) {
    if (!term) return null;

    if (term.termType === 'NamedNode') {
      return term.value; // Return IRI as string
    }

    if (term.termType === 'Literal') {
      const datatype = term.datatype?.value;

      // Type-specific conversions
      if (datatype === 'http://www.w3.org/2001/XMLSchema#integer' ||
          datatype === 'http://www.w3.org/2001/XMLSchema#int' ||
          datatype === 'http://www.w3.org/2001/XMLSchema#long') {
        return parseInt(term.value, 10);
      }

      if (datatype === 'http://www.w3.org/2001/XMLSchema#decimal' ||
          datatype === 'http://www.w3.org/2001/XMLSchema#double' ||
          datatype === 'http://www.w3.org/2001/XMLSchema#float') {
        return parseFloat(term.value);
      }

      if (datatype === 'http://www.w3.org/2001/XMLSchema#boolean') {
        return term.value === 'true' || term.value === '1';
      }

      // Default to string value
      return term.value;
    }

    return term.value;
  }

  /**
   * Resolve GraphQL ID to full IRI
   * @param {string} id - GraphQL ID (could be short form or full IRI)
   * @returns {string} Full IRI
   * @private
   */
  resolveIRI(id) {
    // If already a full IRI, return as-is
    if (id.startsWith('http://') || id.startsWith('https://') || id.startsWith('urn:')) {
      return id;
    }

    // Otherwise, construct IRI using default namespace
    const defaultNs = this.config.namespaces?.[''] || 'http://example.org/';
    return defaultNs + id;
  }

  /**
   * Convert GraphQL type name to RDF class IRI
   * @param {string} typeName - GraphQL type name
   * @returns {string} RDF class IRI
   * @private
   */
  typeNameToIRI(typeName) {
    const defaultNs = this.config.namespaces?.[''] || 'http://example.org/';
    return defaultNs + typeName;
  }

  /**
   * Create simple hash of GraphQL info for caching
   * @param {object} info - GraphQL resolve info
   * @returns {string} Hash
   * @private
   */
  hashInfo(info) {
    // Simple hash based on field selections
    const fieldNames = info.fieldNodes?.[0]?.selectionSet?.selections
      ?.map(s => s.name.value)
      ?.sort()
      ?.join(',') || '';

    return fieldNames;
  }

  /**
   * Clear resolver cache
   */
  clearCache() {
    if (this.cache) {
      this.cache.clear();
    }
  }

  /**
   * Get cache statistics
   * @returns {{size: number, enabled: boolean}}
   */
  getCacheStats() {
    return {
      enabled: !!this.cache,
      size: this.cache?.size || 0,
    };
  }
}

/**
 * Create field resolver for nested relationships
 * @param {object} store - Oxigraph store
 * @param {string} propertyIRI - Property IRI to follow
 * @returns {Function} Field resolver
 */
export function createRelationshipResolver(store, propertyIRI) {
  return async (parent, args, context, info) => {
    const subjectIRI = parent.id;

    const query = `
SELECT ?o WHERE {
  <${subjectIRI}> <${propertyIRI}> ?o .
}
    `.trim();

    const results = store.query(query);
    const values = [];

    for (const binding of results) {
      const value = binding.get('o');
      if (value) {
        values.push(value.value);
      }
    }

    return values.length === 1 ? values[0] : values;
  };
}
