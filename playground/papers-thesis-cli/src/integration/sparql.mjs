/**
 * @fileoverview SPARQL Query Execution Layer for UNRDF Papers-Thesis CLI
 *
 * @description
 * Provides SPARQL query execution against the papers-thesis knowledge graph.
 * Integrates with UNRDF composables (useGraph, useTurtle) and supports:
 * - SELECT, ASK, CONSTRUCT query types
 * - Named query registry with parameterization
 * - Query result caching
 * - Performance metrics
 *
 * @module integration/sparql
 * @version 1.0.0
 * @license MIT
 */

import { join, dirname } from 'node:path';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { useGraph, initStore, RdfEngine, Store } from 'unrdf';
import { LRUCache } from 'lru-cache';

const __dirname = dirname(fileURLToPath(import.meta.url));

// =============================================================================
// Constants and Configuration
// =============================================================================

/**
 * Default paths for ontology files
 * @type {Object}
 */
export const ONTOLOGY_PATHS = {
  papersThesis: join(__dirname, '../../ontologies/papers-thesis.ttl'),
  examples: join(__dirname, '../../ontologies/examples.ttl')
};

/**
 * RDF namespace prefixes used in queries
 * @type {Object<string, string>}
 */
export const PREFIXES = {
  pt: 'http://papers-thesis.org/ontology#',
  ex: 'http://papers-thesis.org/examples#',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  owl: 'http://www.w3.org/2002/07/owl#',
  foaf: 'http://xmlns.com/foaf/0.1/',
  dc: 'http://purl.org/dc/terms/',
  skos: 'http://www.w3.org/2004/02/skos/core#'
};

/**
 * Generate SPARQL prefix declarations
 * @returns {string} SPARQL PREFIX declarations
 */
export function createPrefixDeclarations() {
  return Object.entries(PREFIXES)
    .map(([prefix, uri]) => `PREFIX ${prefix}: <${uri}>`)
    .join('\n');
}

// =============================================================================
// Named Queries Registry
// =============================================================================

/**
 * Named SPARQL queries for common operations
 * Each query can have $param placeholders for parameterization
 * @type {Object<string, {query: string, description: string, params: string[]}>}
 */
export const NAMED_QUERIES = {
  // Paper Queries
  findPapersByAuthor: {
    query: `
      SELECT ?paper ?title ?authorName
      WHERE {
        ?paper a pt:Paper ;
               pt:hasTitle ?title ;
               pt:hasAuthor ?author .
        ?author pt:authorName ?authorName .
        FILTER(CONTAINS(LCASE(?authorName), LCASE("$authorName")))
      }
      ORDER BY ?title
    `,
    description: 'Find all papers written by a specific author',
    params: ['authorName']
  },

  findPapersByFamily: {
    query: `
      SELECT ?paper ?title ?family ?abstract
      WHERE {
        ?paper a pt:Paper ;
               pt:hasTitle ?title ;
               pt:paperFamily ?family .
        OPTIONAL { ?paper pt:hasAbstract ?abstract }
        FILTER(?family = "$family")
      }
      ORDER BY ?title
    `,
    description: 'Find all papers of a specific structural family',
    params: ['family']
  },

  listPaperSections: {
    query: `
      SELECT ?section ?heading ?order ?level ?content
      WHERE {
        <$paperUri> pt:hasSection ?section .
        ?section pt:sectionHeading ?heading ;
                 pt:sectionOrder ?order .
        OPTIONAL { ?section pt:sectionLevel ?level }
        OPTIONAL { ?section pt:sectionContent ?content }
      }
      ORDER BY ?order
    `,
    description: 'List all sections of a paper in order',
    params: ['paperUri']
  },

  searchPapersByKeyword: {
    query: `
      SELECT DISTINCT ?paper ?title ?abstract
      WHERE {
        ?paper a pt:Paper ;
               pt:hasTitle ?title .
        OPTIONAL { ?paper pt:hasAbstract ?abstract }
        OPTIONAL { ?paper pt:hasKeyword ?keyword }
        FILTER(
          CONTAINS(LCASE(?title), LCASE("$keyword")) ||
          CONTAINS(LCASE(?abstract), LCASE("$keyword")) ||
          CONTAINS(LCASE(?keyword), LCASE("$keyword"))
        )
      }
      ORDER BY ?title
    `,
    description: 'Search papers by keyword in title, abstract, or keywords',
    params: ['keyword']
  },

  getPaperDetails: {
    query: `
      SELECT ?paper ?title ?abstract ?family ?venue ?doi
             (GROUP_CONCAT(DISTINCT ?authorName; separator=", ") AS ?authors)
             (GROUP_CONCAT(DISTINCT ?keyword; separator=", ") AS ?keywords)
             (COUNT(DISTINCT ?section) AS ?sectionCount)
      WHERE {
        <$paperUri> a pt:Paper ;
                    pt:hasTitle ?title .
        OPTIONAL { <$paperUri> pt:hasAbstract ?abstract }
        OPTIONAL { <$paperUri> pt:paperFamily ?family }
        OPTIONAL { <$paperUri> pt:venue ?venue }
        OPTIONAL { <$paperUri> pt:doi ?doi }
        OPTIONAL {
          <$paperUri> pt:hasAuthor ?author .
          ?author pt:authorName ?authorName
        }
        OPTIONAL { <$paperUri> pt:hasKeyword ?keyword }
        OPTIONAL { <$paperUri> pt:hasSection ?section }
        BIND(<$paperUri> AS ?paper)
      }
      GROUP BY ?paper ?title ?abstract ?family ?venue ?doi
    `,
    description: 'Get comprehensive details about a paper',
    params: ['paperUri']
  },

  // Thesis Queries
  getThesisByType: {
    query: `
      SELECT ?thesis ?title ?type ?institution ?degree ?defenseDate
      WHERE {
        ?thesis a pt:Thesis ;
                pt:hasTitle ?title ;
                pt:thesisType ?type .
        OPTIONAL { ?thesis pt:institution ?institution }
        OPTIONAL { ?thesis pt:degree ?degree }
        OPTIONAL {
          ?thesis pt:hasSchedule ?schedule .
          ?schedule pt:defenseDate ?defenseDate .
        }
        FILTER(?type = "$type")
      }
      ORDER BY ?defenseDate
    `,
    description: 'List all theses of a specific type',
    params: ['type']
  },

  listAllTheses: {
    query: `
      SELECT ?thesis ?title ?type ?institution ?degree ?defenseDate
      WHERE {
        ?thesis a pt:Thesis ;
                pt:hasTitle ?title ;
                pt:thesisType ?type .
        OPTIONAL { ?thesis pt:institution ?institution }
        OPTIONAL { ?thesis pt:degree ?degree }
        OPTIONAL {
          ?thesis pt:hasSchedule ?schedule .
          ?schedule pt:defenseDate ?defenseDate .
        }
      }
      ORDER BY ?defenseDate
    `,
    description: 'List all theses with schedule information',
    params: []
  },

  findUpcomingDefenses: {
    query: `
      SELECT ?thesis ?title ?defenseDate ?studentName
      WHERE {
        ?thesis a pt:Thesis ;
                pt:hasTitle ?title ;
                pt:hasSchedule ?schedule ;
                pt:hasAuthor ?author .
        ?schedule pt:defenseDate ?defenseDate .
        ?author pt:authorName ?studentName .
        FILTER(?defenseDate > NOW())
      }
      ORDER BY ?defenseDate
    `,
    description: 'Find thesis defenses scheduled in the future',
    params: []
  },

  getThesisProgress: {
    query: `
      SELECT ?thesis ?title
             (COUNT(?milestone) AS ?totalMilestones)
             (COUNT(?completedMilestone) AS ?completedCount)
      WHERE {
        <$thesisUri> a pt:Thesis ;
                     pt:hasTitle ?title ;
                     pt:hasSchedule ?schedule .
        ?schedule pt:hasMilestone ?milestone .
        OPTIONAL {
          ?schedule pt:hasMilestone ?completedMilestone .
          ?completedMilestone pt:milestoneStatus "completed" .
        }
        BIND(<$thesisUri> AS ?thesis)
      }
      GROUP BY ?thesis ?title
    `,
    description: 'Calculate thesis completion progress based on milestones',
    params: ['thesisUri']
  },

  findMilestonesByStatus: {
    query: `
      SELECT ?thesis ?thesisTitle ?milestoneName ?milestoneDate ?status
      WHERE {
        ?thesis a pt:Thesis ;
                pt:hasTitle ?thesisTitle ;
                pt:hasSchedule ?schedule .
        ?schedule pt:hasMilestone ?milestone .
        ?milestone pt:milestoneName ?milestoneName ;
                   pt:milestoneDate ?milestoneDate ;
                   pt:milestoneStatus ?status .
        FILTER(?status = "$status")
      }
      ORDER BY ?milestoneDate
    `,
    description: 'Find all milestones with a specific status',
    params: ['status']
  },

  getOverdueMilestones: {
    query: `
      SELECT ?thesis ?thesisTitle ?milestoneName ?milestoneDate ?status
      WHERE {
        ?thesis a pt:Thesis ;
                pt:hasTitle ?thesisTitle ;
                pt:hasSchedule ?schedule .
        ?schedule pt:hasMilestone ?milestone .
        ?milestone pt:milestoneName ?milestoneName ;
                   pt:milestoneDate ?milestoneDate ;
                   pt:milestoneStatus ?status .
        FILTER(?milestoneDate < NOW() && ?status != "completed")
      }
      ORDER BY ?milestoneDate
    `,
    description: 'Find milestones that are past due but not completed',
    params: []
  },

  // Author and Statistics Queries
  getAuthorStatistics: {
    query: `
      SELECT ?authorName ?affiliation (COUNT(?work) AS ?workCount)
      WHERE {
        ?work pt:hasAuthor ?author .
        ?author pt:authorName ?authorName .
        OPTIONAL { ?author pt:authorAffiliation ?affiliation }
      }
      GROUP BY ?authorName ?affiliation
      ORDER BY DESC(?workCount)
    `,
    description: 'Get statistics on papers per author',
    params: []
  },

  findCoAuthors: {
    query: `
      SELECT DISTINCT ?author1Name ?author2Name (COUNT(DISTINCT ?paper) AS ?collaborations)
      WHERE {
        ?paper pt:hasAuthor ?author1, ?author2 .
        ?author1 pt:authorName ?author1Name .
        ?author2 pt:authorName ?author2Name .
        FILTER(STR(?author1) < STR(?author2))
      }
      GROUP BY ?author1Name ?author2Name
      HAVING(COUNT(DISTINCT ?paper) > 0)
      ORDER BY DESC(?collaborations)
    `,
    description: 'Find pairs of authors who have co-authored papers',
    params: []
  },

  // Ontology Introspection Queries
  listOntologyClasses: {
    query: `
      SELECT ?class ?label ?comment ?superClass
      WHERE {
        ?class a owl:Class .
        OPTIONAL { ?class rdfs:label ?label }
        OPTIONAL { ?class rdfs:comment ?comment }
        OPTIONAL { ?class rdfs:subClassOf ?superClass }
        FILTER(STRSTARTS(STR(?class), STR(pt:)))
      }
      ORDER BY ?label
    `,
    description: 'List all classes defined in the ontology',
    params: []
  },

  listOntologyProperties: {
    query: `
      SELECT ?property ?label ?domain ?range ?type
      WHERE {
        ?property a ?propType .
        FILTER(?propType IN (rdf:Property, owl:ObjectProperty, owl:DatatypeProperty))
        OPTIONAL { ?property rdfs:label ?label }
        OPTIONAL { ?property rdfs:domain ?domain }
        OPTIONAL { ?property rdfs:range ?range }
        BIND(IF(?propType = owl:ObjectProperty, "Object",
             IF(?propType = owl:DatatypeProperty, "Datatype", "General")) AS ?type)
        FILTER(STRSTARTS(STR(?property), STR(pt:)))
      }
      ORDER BY ?type ?label
    `,
    description: 'List all properties with their domains and ranges',
    params: []
  },

  ontologyIntrospection: {
    query: `
      SELECT ?entity ?type ?label ?comment
      WHERE {
        {
          ?entity a rdfs:Class ;
                  rdfs:label ?label .
          OPTIONAL { ?entity rdfs:comment ?comment }
          BIND("Class" AS ?type)
        }
        UNION
        {
          ?entity a rdf:Property ;
                  rdfs:label ?label .
          OPTIONAL { ?entity rdfs:comment ?comment }
          BIND("Property" AS ?type)
        }
      }
      ORDER BY ?type ?label
    `,
    description: 'List all classes and properties in the ontology',
    params: []
  },

  // Citation and Relationship Queries
  getCitationNetwork: {
    query: `
      SELECT ?citingWork ?citingTitle ?citedWork ?citedTitle
      WHERE {
        ?citingWork pt:cites ?citedWork ;
                    pt:hasTitle ?citingTitle .
        ?citedWork pt:hasTitle ?citedTitle .
      }
      ORDER BY ?citingTitle
    `,
    description: 'Get the citation network between academic works',
    params: []
  },

  getSectionHierarchy: {
    query: `
      SELECT ?section ?heading ?level ?order ?parentHeading
      WHERE {
        <$paperUri> pt:hasSection ?section .
        ?section pt:sectionHeading ?heading ;
                 pt:sectionOrder ?order .
        OPTIONAL { ?section pt:sectionLevel ?level }
        OPTIONAL {
          ?section pt:parentSection ?parent .
          ?parent pt:sectionHeading ?parentHeading
        }
      }
      ORDER BY ?level ?order
    `,
    description: 'Get the complete section hierarchy for a paper',
    params: ['paperUri']
  },

  // Validation Queries (ASK)
  validateIMRADStructure: {
    query: `
      ASK {
        <$paperUri> a pt:IMRADPaper ;
                    pt:hasSection ?intro, ?methods, ?results, ?discussion .
        ?intro pt:sectionHeading ?introHeading .
        ?methods pt:sectionHeading ?methodsHeading .
        ?results pt:sectionHeading ?resultsHeading .
        ?discussion pt:sectionHeading ?discussionHeading .
        FILTER(
          CONTAINS(LCASE(?introHeading), "introduction") &&
          (CONTAINS(LCASE(?methodsHeading), "method") || CONTAINS(LCASE(?methodsHeading), "methodology")) &&
          CONTAINS(LCASE(?resultsHeading), "result") &&
          CONTAINS(LCASE(?discussionHeading), "discussion")
        )
      }
    `,
    description: 'Check if a paper has valid IMRAD structure',
    params: ['paperUri']
  },

  checkPaperHasAuthor: {
    query: `
      ASK {
        <$paperUri> pt:hasAuthor ?author .
        ?author pt:authorName ?name .
      }
    `,
    description: 'Check if a paper has at least one author',
    params: ['paperUri']
  },

  // CONSTRUCT Queries
  constructPaperSummary: {
    query: `
      CONSTRUCT {
        ?paper dc:title ?title ;
               dc:creator ?authorName ;
               dc:abstract ?abstract ;
               dc:subject ?keyword .
      }
      WHERE {
        <$paperUri> pt:hasTitle ?title ;
                    pt:hasAuthor ?author .
        ?author pt:authorName ?authorName .
        OPTIONAL { <$paperUri> pt:hasAbstract ?abstract }
        OPTIONAL { <$paperUri> pt:hasKeyword ?keyword }
        BIND(<$paperUri> AS ?paper)
      }
    `,
    description: 'Construct an RDF summary of a paper using Dublin Core',
    params: ['paperUri']
  }
};

// =============================================================================
// Query Result Cache
// =============================================================================

/**
 * LRU Cache for query results
 * @type {LRUCache}
 */
const queryCache = new LRUCache({
  max: 100,
  ttl: 1000 * 60 * 5 // 5 minutes TTL
});

/**
 * Clear the query cache
 */
export function clearQueryCache() {
  queryCache.clear();
}

/**
 * Get cache statistics
 * @returns {{size: number, max: number, hitRate: number}}
 */
export function getCacheStats() {
  return {
    size: queryCache.size,
    max: queryCache.max,
    // @ts-ignore - calculatedSize might not be available
    calculatedSize: queryCache.calculatedSize || 0
  };
}

// =============================================================================
// Knowledge Graph State
// =============================================================================

/**
 * @typedef {Object} KnowledgeGraphState
 * @property {Object|null} store - RDF store instance
 * @property {Object|null} graph - Graph instance
 * @property {Object|null} turtle - Turtle parser/serializer
 * @property {boolean} initialized - Whether the graph is initialized
 * @property {string[]} loadedOntologies - Paths of loaded ontologies
 * @property {Object} metrics - Performance metrics
 */

/**
 * Internal state for the knowledge graph
 * @type {KnowledgeGraphState}
 */
let graphState = {
  runWithStore: null,
  engine: null,
  store: null,
  graph: null,
  initialized: false,
  loadedOntologies: [],
  metrics: {
    queriesExecuted: 0,
    totalQueryTime: 0,
    cacheHits: 0,
    cacheMisses: 0
  }
};

// =============================================================================
// Core Functions
// =============================================================================

/**
 * Initialize UNRDF knowledge graph with playground ontologies
 * @param {Object} [options] - Configuration options
 * @param {boolean} [options.loadExamples=false] - Whether to load examples.ttl
 * @param {string} [options.ontologyPath] - Custom path to main ontology
 * @returns {Promise<{success: boolean, message: string, ontologies: string[]}>}
 */
export async function initKnowledgeGraph(options = {}) {
  const startTime = Date.now();

  try {
    // Initialize RdfEngine for Turtle parsing/serialization
    graphState.engine = new RdfEngine();
    graphState.store = graphState.engine.store;

    // Initialize store context for useGraph
    graphState.runWithStore = initStore([]);

    // Initialize graph within store context
    await graphState.runWithStore(async () => {
      graphState.graph = useGraph();
    });

    // Load main ontology
    const ontologyPath = options.ontologyPath || ONTOLOGY_PATHS.papersThesis;
    await loadOntology(ontologyPath);

    // Optionally load examples
    if (options.loadExamples) {
      await loadOntology(ONTOLOGY_PATHS.examples);
    }

    graphState.initialized = true;

    const duration = Date.now() - startTime;

    return {
      success: true,
      message: `Knowledge graph initialized in ${duration}ms`,
      ontologies: [...graphState.loadedOntologies],
      duration
    };
  } catch (error) {
    graphState.initialized = false;
    throw new Error(`Failed to initialize knowledge graph: ${error.message}`);
  }
}

/**
 * Load ontology from Turtle file
 * @param {string} filePath - Path to .ttl file
 * @returns {Promise<{success: boolean, path: string, tripleCount: number}>}
 */
export async function loadOntology(filePath) {
  ensureInitialized(true); // Allow loading without full init

  try {
    const turtleContent = await readFile(filePath, 'utf-8');

    // Parse Turtle and add quads to the store
    const parsedStore = graphState.engine.parseTurtle(turtleContent);
    const tripleCount = parsedStore.size;

    // Add all quads from parsed store to the main store
    for (const quad of parsedStore) {
      graphState.store.add(quad);
    }

    graphState.loadedOntologies.push(filePath);

    return {
      success: true,
      path: filePath,
      tripleCount
    };
  } catch (error) {
    throw new Error(`Failed to load ontology from ${filePath}: ${error.message}`);
  }
}

/**
 * Execute SPARQL SELECT query
 * @param {string} query - SPARQL SELECT query
 * @param {Object} [options] - Query options
 * @param {boolean} [options.useCache=true] - Whether to use query cache
 * @param {number} [options.limit] - Limit results
 * @returns {Promise<Array<Object>>} Result bindings as array of objects
 */
export async function executeSparqlSelect(query, options = {}) {
  ensureInitialized();
  const { useCache = true, limit } = options;

  // Add prefixes if not present
  const fullQuery = prepareQuery(query, limit);

  // Check cache
  const cacheKey = `select:${fullQuery}`;
  if (useCache && queryCache.has(cacheKey)) {
    graphState.metrics.cacheHits++;
    return queryCache.get(cacheKey);
  }

  graphState.metrics.cacheMisses++;
  const startTime = Date.now();

  try {
    // Execute SPARQL query using comunicaEngine directly
    const resultStream = await graphState.engine.comunicaEngine.queryBindings(
      fullQuery,
      { sources: [graphState.store] }
    );

    const bindings = [];
    for await (const binding of resultStream) {
      const row = {};
      // Access internal entries structure of immutable map
      if (binding.entries && binding.entries._root && binding.entries._root.entries) {
        for (const [key, term] of binding.entries._root.entries) {
          row[key] = termToValue(term);
        }
      }
      bindings.push(row);
    }

    // Update metrics
    graphState.metrics.queriesExecuted++;
    graphState.metrics.totalQueryTime += Date.now() - startTime;

    // Cache results
    if (useCache) {
      queryCache.set(cacheKey, bindings);
    }

    return bindings;
  } catch (error) {
    throw new SparqlQueryError(error.message, fullQuery);
  }
}

/**
 * Execute SPARQL ASK query
 * @param {string} query - SPARQL ASK query
 * @returns {Promise<boolean>}
 */
export async function executeSparqlAsk(query) {
  ensureInitialized();

  const fullQuery = prepareQuery(query);
  const startTime = Date.now();

  try {
    // Execute ASK query using comunicaEngine
    const result = await graphState.engine.comunicaEngine.queryBoolean(
      fullQuery,
      { sources: [graphState.store] }
    );

    graphState.metrics.queriesExecuted++;
    graphState.metrics.totalQueryTime += Date.now() - startTime;

    return Boolean(result);
  } catch (error) {
    throw new SparqlQueryError(error.message, fullQuery);
  }
}

/**
 * Execute SPARQL CONSTRUCT query
 * @param {string} query - SPARQL CONSTRUCT query
 * @returns {Promise<string>} RDF/Turtle result
 */
export async function executeSparqlConstruct(query) {
  ensureInitialized();

  const fullQuery = prepareQuery(query);
  const startTime = Date.now();

  try {
    // Execute CONSTRUCT query using comunicaEngine
    const resultStream = await graphState.engine.comunicaEngine.queryQuads(
      fullQuery,
      { sources: [graphState.store] }
    );

    const tempStore = new Store();
    for await (const quad of resultStream) {
      tempStore.add(quad);
    }

    // Serialize to Turtle using RdfEngine
    const turtle = graphState.engine.serializeTurtle(tempStore);

    graphState.metrics.queriesExecuted++;
    graphState.metrics.totalQueryTime += Date.now() - startTime;

    return turtle || '';
  } catch (error) {
    throw new SparqlQueryError(error.message, fullQuery);
  }
}

/**
 * Execute a named query by name with parameters
 * @param {string} queryName - Name of the query from NAMED_QUERIES
 * @param {Object} [params={}] - Query parameters to substitute
 * @returns {Promise<Array<Object>|boolean|string>} Query results
 */
export async function executeNamedQuery(queryName, params = {}) {
  const queryDef = NAMED_QUERIES[queryName];

  if (!queryDef) {
    const available = Object.keys(NAMED_QUERIES).join(', ');
    throw new Error(`Unknown named query: ${queryName}. Available: ${available}`);
  }

  // Validate required parameters
  for (const param of queryDef.params) {
    if (!(param in params)) {
      throw new Error(`Missing required parameter '${param}' for query '${queryName}'`);
    }
  }

  // Substitute parameters
  let query = queryDef.query;
  for (const [key, value] of Object.entries(params)) {
    query = query.replaceAll(`$${key}`, String(value));
  }

  // Determine query type and execute
  const trimmedQuery = query.trim().toUpperCase();
  if (trimmedQuery.startsWith('ASK')) {
    return executeSparqlAsk(query);
  } else if (trimmedQuery.startsWith('CONSTRUCT')) {
    return executeSparqlConstruct(query);
  } else {
    return executeSparqlSelect(query);
  }
}

/**
 * List all available named queries
 * @returns {Array<{name: string, description: string, params: string[]}>}
 */
export function listNamedQueries() {
  return Object.entries(NAMED_QUERIES).map(([name, def]) => ({
    name,
    description: def.description,
    params: def.params
  }));
}

/**
 * Insert RDF data into knowledge graph
 * @param {string} turtleData - Turtle format data
 * @returns {Promise<{success: boolean, tripleCount: number}>}
 */
export async function insertTriples(turtleData) {
  ensureInitialized();

  try {
    // Parse Turtle using RdfEngine
    const parsedStore = graphState.engine.parseTurtle(turtleData);
    const tripleCount = parsedStore.size;

    // Add all quads to the main store
    for (const quad of parsedStore) {
      graphState.store.add(quad);
    }

    // Clear cache on data modification
    clearQueryCache();

    return {
      success: true,
      tripleCount
    };
  } catch (error) {
    throw new Error(`Failed to insert triples: ${error.message}`);
  }
}

/**
 * Export knowledge graph as Turtle
 * @returns {Promise<string>} Turtle format
 */
export async function exportAsTurtle() {
  ensureInitialized();

  try {
    // Serialize the main store using RdfEngine
    const turtle = graphState.engine.serializeTurtle(graphState.store);
    return turtle;
  } catch (error) {
    throw new Error(`Failed to export as Turtle: ${error.message}`);
  }
}

/**
 * Query ontology classes
 * @returns {Promise<Array<{class: string, label: string, comment: string, superClass: string}>>}
 */
export async function getOntologyClasses() {
  return executeNamedQuery('listOntologyClasses');
}

/**
 * Query ontology properties
 * @returns {Promise<Array<{property: string, label: string, domain: string, range: string, type: string}>>}
 */
export async function getOntologyProperties() {
  return executeNamedQuery('listOntologyProperties');
}

/**
 * Get query execution metrics
 * @returns {{queriesExecuted: number, totalQueryTime: number, avgQueryTime: number, cacheHits: number, cacheMisses: number, hitRate: number}}
 */
export function getQueryMetrics() {
  const { queriesExecuted, totalQueryTime, cacheHits, cacheMisses } = graphState.metrics;
  const totalCacheAccess = cacheHits + cacheMisses;

  return {
    queriesExecuted,
    totalQueryTime,
    avgQueryTime: queriesExecuted > 0 ? Math.round(totalQueryTime / queriesExecuted) : 0,
    cacheHits,
    cacheMisses,
    hitRate: totalCacheAccess > 0 ? Math.round((cacheHits / totalCacheAccess) * 100) : 0
  };
}

/**
 * Reset query metrics
 */
export function resetQueryMetrics() {
  graphState.metrics = {
    queriesExecuted: 0,
    totalQueryTime: 0,
    cacheHits: 0,
    cacheMisses: 0
  };
}

/**
 * Check if knowledge graph is initialized
 * @returns {boolean}
 */
export function isInitialized() {
  return graphState.initialized;
}

/**
 * Get loaded ontology paths
 * @returns {string[]}
 */
export function getLoadedOntologies() {
  return [...graphState.loadedOntologies];
}

/**
 * Get graph statistics
 * @returns {Promise<{tripleCount: number, initialized: boolean, loadedOntologies: string[]}>}
 */
export async function getGraphStats() {
  if (!graphState.initialized) {
    return {
      tripleCount: 0,
      initialized: false,
      loadedOntologies: []
    };
  }

  try {
    // Get triple count directly from the store
    const tripleCount = graphState.store ? graphState.store.size : 0;
    return {
      tripleCount,
      initialized: graphState.initialized,
      loadedOntologies: [...graphState.loadedOntologies]
    };
  } catch {
    return {
      tripleCount: 0,
      initialized: graphState.initialized,
      loadedOntologies: [...graphState.loadedOntologies]
    };
  }
}

/**
 * Shutdown and cleanup the knowledge graph
 */
export function shutdown() {
  graphState = {
    runWithStore: null,
    engine: null,
    store: null,
    graph: null,
    initialized: false,
    loadedOntologies: [],
    metrics: {
      queriesExecuted: 0,
      totalQueryTime: 0,
      cacheHits: 0,
      cacheMisses: 0
    }
  };
  clearQueryCache();
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Ensure knowledge graph is initialized
 * @param {boolean} [allowPartial=false] - Allow partial initialization
 * @throws {Error} If not initialized
 */
function ensureInitialized(allowPartial = false) {
  if (allowPartial) {
    // Initialize engine and store context if not present
    if (!graphState.engine) {
      graphState.engine = new RdfEngine();
      graphState.store = graphState.engine.store;
      graphState.runWithStore = initStore([]);
      graphState.runWithStore(() => {
        graphState.graph = useGraph();
      });
    }
    return;
  }

  if (!graphState.initialized) {
    throw new Error('Knowledge graph not initialized. Call initKnowledgeGraph() first.');
  }
}

/**
 * Prepare query with prefixes and optional limit
 * @param {string} query - SPARQL query
 * @param {number} [limit] - Optional limit
 * @returns {string} Prepared query
 */
function prepareQuery(query, limit) {
  let fullQuery = query.includes('PREFIX')
    ? query
    : `${createPrefixDeclarations()}\n\n${query}`;

  // Add limit if specified and not already present
  if (limit && !query.toUpperCase().includes('LIMIT')) {
    fullQuery = `${fullQuery}\nLIMIT ${limit}`;
  }

  return fullQuery;
}

/**
 * Convert RDF term to JavaScript value
 * @param {Object} term - RDF term
 * @returns {string|number|boolean|Object}
 */
function termToValue(term) {
  if (!term) return null;

  switch (term.termType) {
    case 'Literal':
      // Handle typed literals
      if (term.datatype) {
        const dt = term.datatype.value;
        if (dt.includes('integer') || dt.includes('int')) {
          return parseInt(term.value, 10);
        }
        if (dt.includes('decimal') || dt.includes('double') || dt.includes('float')) {
          return parseFloat(term.value);
        }
        if (dt.includes('boolean')) {
          return term.value === 'true';
        }
        if (dt.includes('date')) {
          return term.value; // Keep as string for dates
        }
      }
      return term.value;
    case 'NamedNode':
      return term.value;
    case 'BlankNode':
      return `_:${term.value}`;
    default:
      return term.value;
  }
}

// =============================================================================
// Error Classes
// =============================================================================

/**
 * SPARQL Query Error
 */
export class SparqlQueryError extends Error {
  /**
   * @param {string} message - Error message
   * @param {string} query - The SPARQL query that caused the error
   */
  constructor(message, query) {
    super(message);
    this.name = 'SparqlQueryError';
    this.query = query;
  }
}

// =============================================================================
// Default Export
// =============================================================================

export default {
  // Initialization
  initKnowledgeGraph,
  loadOntology,
  shutdown,
  isInitialized,

  // Query Execution
  executeSparqlSelect,
  executeSparqlAsk,
  executeSparqlConstruct,
  executeNamedQuery,

  // Data Operations
  insertTriples,
  exportAsTurtle,

  // Introspection
  getOntologyClasses,
  getOntologyProperties,
  listNamedQueries,

  // Metrics and Stats
  getQueryMetrics,
  resetQueryMetrics,
  getGraphStats,
  getLoadedOntologies,

  // Cache
  clearQueryCache,
  getCacheStats,

  // Constants
  PREFIXES,
  NAMED_QUERIES,
  ONTOLOGY_PATHS,
  createPrefixDeclarations
};
