/**
 * @fileoverview UNRDF Knowledge Graph integration
 *
 * @description
 * Provides integration with UNRDF knowledge engine for storing and querying
 * papers and thesis documents as RDF data. This module wraps the SPARQL layer
 * and provides a high-level API for domain operations.
 *
 * @module integration/knowledge-graph
 * @version 2.0.0
 * @license MIT
 */

import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

// Import SPARQL layer
import {
  initKnowledgeGraph as initSparql,
  loadOntology as loadSparqlOntology,
  executeSparqlSelect,
  executeSparqlAsk,
  executeSparqlConstruct,
  executeNamedQuery,
  insertTriples,
  exportAsTurtle,
  getOntologyClasses,
  getOntologyProperties,
  getQueryMetrics,
  getGraphStats,
  isInitialized,
  shutdown as shutdownSparql,
  clearQueryCache,
  PREFIXES,
  NAMED_QUERIES as SPARQL_NAMED_QUERIES,
  createPrefixDeclarations,
} from './sparql.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const DEFAULT_ONTOLOGY_PATH = join(__dirname, '../../ontologies/papers-thesis.ttl');
const EXAMPLES_PATH = join(__dirname, '../../ontologies/examples.ttl');

// =============================================================================
// Re-export SPARQL layer constants
// =============================================================================

export { PREFIXES, createPrefixDeclarations };

/**
 * Named SPARQL queries for common operations (re-export with additional domain queries)
 * @type {Object}
 */
export const NAMED_QUERIES = {
  ...SPARQL_NAMED_QUERIES,

  // Additional domain-specific queries
  findPapersByFamily: `
    ${createPrefixDeclarations()}

    SELECT ?paper ?title ?family
    WHERE {
      ?paper a pt:Paper ;
             pt:hasTitle ?title ;
             pt:paperFamily ?family .
      FILTER(?family = $family)
    }
  `,

  listAllTheses: `
    ${createPrefixDeclarations()}

    SELECT ?thesis ?title ?type ?defenseDate
    WHERE {
      ?thesis a pt:Thesis ;
              pt:hasTitle ?title ;
              pt:thesisType ?type .
      OPTIONAL {
        ?thesis pt:hasSchedule ?schedule .
        ?schedule pt:defenseDate ?defenseDate .
      }
    }
    ORDER BY ?defenseDate
  `,

  ontologyIntrospection: `
    ${createPrefixDeclarations()}

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

  authorStatistics: `
    ${createPrefixDeclarations()}

    SELECT ?authorName (COUNT(?work) AS ?workCount)
    WHERE {
      ?work pt:hasAuthor ?author .
      ?author pt:authorName ?authorName .
    }
    GROUP BY ?authorName
    ORDER BY DESC(?workCount)
  `,
};

// =============================================================================
// Knowledge Graph Factory
// =============================================================================

/**
 * Create a knowledge graph instance for papers-thesis domain
 * @param {Object} options - Configuration options
 * @param {string} [options.ontologyPath] - Path to ontology file
 * @param {boolean} [options.loadExamples=false] - Whether to load example data
 * @returns {Object} Knowledge graph interface
 */
export function createKnowledgeGraph(options = {}) {
  const ontologyPath = options.ontologyPath || DEFAULT_ONTOLOGY_PATH;
  const loadExamples = options.loadExamples || false;

  // In-memory store for domain objects (supplements RDF store)
  let domainStore = {
    papers: new Map(),
    theses: new Map(),
  };

  let initialized = false;

  return {
    /**
     * Load the ontology into the knowledge graph
     * @returns {Promise<{success: boolean, path: string, message: string}>}
     */
    async loadOntology() {
      try {
        // Initialize SPARQL layer if not already done
        if (!isInitialized()) {
          await initSparql({
            ontologyPath,
            loadExamples,
          });
        } else {
          // Just load the ontology file
          await loadSparqlOntology(ontologyPath);
        }

        initialized = true;

        return {
          success: true,
          path: ontologyPath,
          message: 'Ontology loaded successfully',
        };
      } catch (error) {
        throw new Error(`Failed to load ontology: ${error.message}`);
      }
    },

    /**
     * Add a paper to the knowledge graph
     * @param {Object} paper - Paper object
     * @returns {Promise<{success: boolean, uri: string, id: string}>}
     */
    async addPaper(paper) {
      const uri = `${PREFIXES.ex}${paper.id}`;

      // Generate RDF/Turtle for the paper
      const turtleData = paperToTurtle(paper);

      try {
        // Insert into RDF store
        if (isInitialized()) {
          await insertTriples(turtleData);
        }

        // Also keep in domain store for quick access
        domainStore.papers.set(paper.id, paper);

        return {
          success: true,
          uri,
          id: paper.id,
        };
      } catch (error) {
        throw new Error(`Failed to add paper: ${error.message}`);
      }
    },

    /**
     * Add a thesis to the knowledge graph
     * @param {Object} thesis - Thesis object
     * @returns {Promise<{success: boolean, uri: string, id: string}>}
     */
    async addThesis(thesis) {
      const uri = `${PREFIXES.ex}${thesis.id}`;

      // Generate RDF/Turtle for the thesis
      const turtleData = thesisToTurtle(thesis);

      try {
        // Insert into RDF store
        if (isInitialized()) {
          await insertTriples(turtleData);
        }

        // Also keep in domain store
        domainStore.theses.set(thesis.id, thesis);

        return {
          success: true,
          uri,
          id: thesis.id,
        };
      } catch (error) {
        throw new Error(`Failed to add thesis: ${error.message}`);
      }
    },

    /**
     * Execute a SPARQL query
     * @param {string} sparql - SPARQL query string
     * @returns {Promise<{success: boolean, query: string, bindings: Array, duration: number}>}
     */
    async query(sparql) {
      const startTime = Date.now();

      // Add prefixes if not present
      const fullQuery = sparql.includes('PREFIX')
        ? sparql
        : `${createPrefixDeclarations()}\n\n${sparql}`;

      try {
        if (!isInitialized()) {
          // Return mock results if not initialized
          console.warn('Knowledge graph not initialized. Returning empty results.');
          return {
            success: true,
            query: fullQuery,
            bindings: [],
            duration: Date.now() - startTime,
            message: 'Query executed (not initialized - empty results)',
          };
        }

        // Determine query type
        const queryType = detectQueryType(sparql);

        let result;
        switch (queryType) {
          case 'ASK':
            result = await executeSparqlAsk(fullQuery);
            return {
              success: true,
              query: fullQuery,
              result,
              duration: Date.now() - startTime,
            };
          case 'CONSTRUCT':
            result = await executeSparqlConstruct(fullQuery);
            return {
              success: true,
              query: fullQuery,
              turtle: result,
              duration: Date.now() - startTime,
            };
          default:
            result = await executeSparqlSelect(fullQuery);
            return {
              success: true,
              query: fullQuery,
              bindings: result,
              duration: Date.now() - startTime,
            };
        }
      } catch (error) {
        return {
          success: false,
          query: fullQuery,
          error: error.message,
          duration: Date.now() - startTime,
        };
      }
    },

    /**
     * Execute a named query
     * @param {string} queryName - Name of the query
     * @param {Object} [params={}] - Query parameters
     * @returns {Promise<Array|boolean|string>}
     */
    async executeNamedQuery(queryName, params = {}) {
      if (!isInitialized()) {
        console.warn('Knowledge graph not initialized.');
        return [];
      }
      return executeNamedQuery(queryName, params);
    },

    /**
     * Find papers by family
     * @param {string} family - Paper family (imrad, dsr, etc.)
     * @returns {Promise<Array>} Matching papers
     */
    async findPapersByFamily(family) {
      if (!isInitialized()) {
        // Return from in-memory store
        return Array.from(domainStore.papers.values()).filter(p => p.family === family);
      }

      try {
        return await executeNamedQuery('findPapersByFamily', { family });
      } catch {
        // Fallback to in-memory
        return Array.from(domainStore.papers.values()).filter(p => p.family === family);
      }
    },

    /**
     * Find theses by type
     * @param {string} type - Thesis type (monograph, narrative, etc.)
     * @returns {Promise<Array>} Matching theses
     */
    async findThesesByType(type) {
      if (!isInitialized()) {
        return Array.from(domainStore.theses.values()).filter(t => t.type === type);
      }

      try {
        return await executeNamedQuery('getThesisByType', { type });
      } catch {
        return Array.from(domainStore.theses.values()).filter(t => t.type === type);
      }
    },

    /**
     * List all ontology classes
     * @returns {Promise<Array<{uri: string, label: string, comment: string}>>}
     */
    async listClasses() {
      if (!isInitialized()) {
        // Return mock class list
        return getDefaultClasses();
      }

      try {
        const results = await getOntologyClasses();
        return results.map(r => ({
          uri: r.class,
          label: r.label || extractLocalName(r.class),
          comment: r.comment || '',
        }));
      } catch {
        return getDefaultClasses();
      }
    },

    /**
     * List all ontology properties
     * @returns {Promise<Array<{uri: string, label: string, domain: string, range: string}>>}
     */
    async listProperties() {
      if (!isInitialized()) {
        return getDefaultProperties();
      }

      try {
        const results = await getOntologyProperties();
        return results.map(r => ({
          uri: r.property,
          label: r.label || extractLocalName(r.property),
          domain: r.domain ? extractLocalName(r.domain) : 'unknown',
          range: r.range ? extractLocalName(r.range) : 'unknown',
          type: r.type || 'General',
        }));
      } catch {
        return getDefaultProperties();
      }
    },

    /**
     * Export knowledge graph in specified format
     * @param {string} format - Export format (turtle, jsonld)
     * @returns {Promise<string>} Serialized graph
     */
    async export(format = 'turtle') {
      if (!isInitialized()) {
        // Export domain store
        const papers = Array.from(domainStore.papers.values());
        const theses = Array.from(domainStore.theses.values());

        if (format === 'jsonld') {
          return JSON.stringify(
            {
              '@context': PREFIXES,
              '@graph': [...papers, ...theses],
            },
            null,
            2
          );
        }

        return `# Papers: ${papers.length}\n# Theses: ${theses.length}\n# Format: ${format}`;
      }

      try {
        if (format === 'turtle') {
          return await exportAsTurtle();
        }

        // For JSON-LD, get turtle and note that conversion would be needed
        const turtle = await exportAsTurtle();
        if (format === 'jsonld') {
          return JSON.stringify(
            {
              '@context': PREFIXES,
              turtle: turtle.substring(0, 500) + '...',
              note: 'Full JSON-LD conversion requires additional processing',
            },
            null,
            2
          );
        }

        return turtle;
      } catch (error) {
        throw new Error(`Failed to export: ${error.message}`);
      }
    },

    /**
     * Get statistics about the knowledge graph
     * @returns {Promise<Object>} Statistics
     */
    async getStats() {
      const baseStats = {
        paperCount: domainStore.papers.size,
        thesisCount: domainStore.theses.size,
        totalDocuments: domainStore.papers.size + domainStore.theses.size,
      };

      if (!isInitialized()) {
        return {
          ...baseStats,
          ontologyLoaded: false,
          tripleCount: 0,
        };
      }

      try {
        const graphStats = await getGraphStats();
        const queryMetrics = getQueryMetrics();

        return {
          ...baseStats,
          ontologyLoaded: true,
          tripleCount: graphStats.tripleCount,
          loadedOntologies: graphStats.loadedOntologies,
          queryMetrics,
        };
      } catch {
        return {
          ...baseStats,
          ontologyLoaded: true,
          tripleCount: 0,
        };
      }
    },

    /**
     * Clear the knowledge graph (for testing)
     * @returns {Promise<void>}
     */
    async clear() {
      domainStore = {
        papers: new Map(),
        theses: new Map(),
      };

      if (isInitialized()) {
        shutdownSparql();
        initialized = false;
      }

      clearQueryCache();
    },

    /**
     * Get prefix declarations
     * @returns {Object} Prefix map
     */
    getPrefixes() {
      return { ...PREFIXES };
    },

    /**
     * Check if initialized
     * @returns {boolean}
     */
    isInitialized() {
      return initialized && isInitialized();
    },

    /**
     * Get query metrics
     * @returns {Object}
     */
    getQueryMetrics() {
      return getQueryMetrics();
    },
  };
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Detect SPARQL query type
 * @param {string} query - SPARQL query
 * @returns {'SELECT'|'ASK'|'CONSTRUCT'|'DESCRIBE'}
 */
function detectQueryType(query) {
  const normalized = query.trim().toUpperCase();
  if (normalized.includes('ASK')) return 'ASK';
  if (normalized.includes('CONSTRUCT')) return 'CONSTRUCT';
  if (normalized.includes('DESCRIBE')) return 'DESCRIBE';
  return 'SELECT';
}

/**
 * Extract local name from URI
 * @param {string} uri - Full URI
 * @returns {string} Local name
 */
function extractLocalName(uri) {
  if (!uri) return 'unknown';
  const hashIndex = uri.lastIndexOf('#');
  const slashIndex = uri.lastIndexOf('/');
  const index = Math.max(hashIndex, slashIndex);
  return index >= 0 ? uri.substring(index + 1) : uri;
}

/**
 * Convert paper object to Turtle
 * @param {Object} paper - Paper object
 * @returns {string} Turtle format
 */
function paperToTurtle(paper) {
  const uri = `ex:${paper.id}`;
  const lines = [
    `@prefix pt: <${PREFIXES.pt}> .`,
    `@prefix ex: <${PREFIXES.ex}> .`,
    `@prefix xsd: <${PREFIXES.xsd}> .`,
    '',
    `${uri} a pt:Paper ;`,
  ];

  if (paper.title) {
    lines.push(`    pt:hasTitle "${escapeTurtle(paper.title)}" ;`);
  }
  if (paper.family) {
    lines.push(`    pt:paperFamily "${paper.family}" ;`);
  }
  if (paper.abstract) {
    lines.push(`    pt:hasAbstract "${escapeTurtle(paper.abstract)}" ;`);
  }
  if (paper.createdAt) {
    lines.push(`    pt:createdAt "${paper.createdAt}"^^xsd:dateTime ;`);
  }

  // Fix trailing semicolon
  const lastLine = lines[lines.length - 1];
  lines[lines.length - 1] = lastLine.replace(/ ;$/, ' .');

  return lines.join('\n');
}

/**
 * Convert thesis object to Turtle
 * @param {Object} thesis - Thesis object
 * @returns {string} Turtle format
 */
function thesisToTurtle(thesis) {
  const uri = `ex:${thesis.id}`;
  const lines = [
    `@prefix pt: <${PREFIXES.pt}> .`,
    `@prefix ex: <${PREFIXES.ex}> .`,
    `@prefix xsd: <${PREFIXES.xsd}> .`,
    '',
    `${uri} a pt:Thesis ;`,
  ];

  if (thesis.title) {
    lines.push(`    pt:hasTitle "${escapeTurtle(thesis.title)}" ;`);
  }
  if (thesis.type) {
    lines.push(`    pt:thesisType "${thesis.type}" ;`);
  }
  if (thesis.institution) {
    lines.push(`    pt:institution "${escapeTurtle(thesis.institution)}" ;`);
  }
  if (thesis.department) {
    lines.push(`    pt:department "${escapeTurtle(thesis.department)}" ;`);
  }
  if (thesis.degree) {
    lines.push(`    pt:degree "${thesis.degree}" ;`);
  }

  const lastLine = lines[lines.length - 1];
  lines[lines.length - 1] = lastLine.replace(/ ;$/, ' .');

  return lines.join('\n');
}

/**
 * Escape string for Turtle format
 * @param {string} str - Input string
 * @returns {string} Escaped string
 */
function escapeTurtle(str) {
  if (!str) return '';
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

/**
 * Get default class list (mock data)
 * @returns {Array}
 */
function getDefaultClasses() {
  return [
    {
      uri: `${PREFIXES.pt}AcademicWork`,
      label: 'Academic Work',
      comment: 'Base class for all academic documents',
    },
    { uri: `${PREFIXES.pt}Paper`, label: 'Research Paper', comment: 'A research paper' },
    { uri: `${PREFIXES.pt}Thesis`, label: 'Thesis', comment: 'A thesis document' },
    {
      uri: `${PREFIXES.pt}IMRADPaper`,
      label: 'IMRAD Paper',
      comment: 'Paper with IMRAD structure',
    },
    { uri: `${PREFIXES.pt}DSRPaper`, label: 'DSR Paper', comment: 'Design Science Research paper' },
    {
      uri: `${PREFIXES.pt}ArgumentPaper`,
      label: 'Argument Paper',
      comment: 'Argument-based paper',
    },
    {
      uri: `${PREFIXES.pt}ContributionPaper`,
      label: 'Contribution Paper',
      comment: 'Contribution-focused paper',
    },
    {
      uri: `${PREFIXES.pt}Monograph`,
      label: 'Monograph Thesis',
      comment: 'Traditional monograph thesis',
    },
    {
      uri: `${PREFIXES.pt}NarrativeThesis`,
      label: 'Narrative Thesis',
      comment: 'Narrative-style thesis',
    },
    {
      uri: `${PREFIXES.pt}ContributionThesis`,
      label: 'Contribution Thesis',
      comment: 'Publication-based thesis',
    },
    {
      uri: `${PREFIXES.pt}Section`,
      label: 'Document Section',
      comment: 'A section within a document',
    },
    { uri: `${PREFIXES.pt}Author`, label: 'Author', comment: 'Document author' },
    { uri: `${PREFIXES.pt}Schedule`, label: 'Schedule', comment: 'Thesis schedule' },
    { uri: `${PREFIXES.pt}Milestone`, label: 'Milestone', comment: 'Schedule milestone' },
  ];
}

/**
 * Get default property list (mock data)
 * @returns {Array}
 */
function getDefaultProperties() {
  return [
    {
      uri: `${PREFIXES.pt}hasTitle`,
      label: 'has title',
      domain: 'AcademicWork',
      range: 'xsd:string',
      type: 'Datatype',
    },
    {
      uri: `${PREFIXES.pt}hasAbstract`,
      label: 'has abstract',
      domain: 'AcademicWork',
      range: 'xsd:string',
      type: 'Datatype',
    },
    {
      uri: `${PREFIXES.pt}hasAuthor`,
      label: 'has author',
      domain: 'AcademicWork',
      range: 'Author',
      type: 'Object',
    },
    {
      uri: `${PREFIXES.pt}hasSection`,
      label: 'has section',
      domain: 'AcademicWork',
      range: 'Section',
      type: 'Object',
    },
    {
      uri: `${PREFIXES.pt}paperFamily`,
      label: 'paper family',
      domain: 'Paper',
      range: 'xsd:string',
      type: 'Datatype',
    },
    {
      uri: `${PREFIXES.pt}thesisType`,
      label: 'thesis type',
      domain: 'Thesis',
      range: 'xsd:string',
      type: 'Datatype',
    },
    {
      uri: `${PREFIXES.pt}hasSchedule`,
      label: 'has schedule',
      domain: 'Thesis',
      range: 'Schedule',
      type: 'Object',
    },
    {
      uri: `${PREFIXES.pt}institution`,
      label: 'institution',
      domain: 'Thesis',
      range: 'xsd:string',
      type: 'Datatype',
    },
    {
      uri: `${PREFIXES.pt}degree`,
      label: 'degree',
      domain: 'Thesis',
      range: 'xsd:string',
      type: 'Datatype',
    },
    {
      uri: `${PREFIXES.pt}defenseDate`,
      label: 'defense date',
      domain: 'Schedule',
      range: 'xsd:date',
      type: 'Datatype',
    },
  ];
}

// =============================================================================
// Default Instance
// =============================================================================

/**
 * Default knowledge graph instance
 * @type {ReturnType<typeof createKnowledgeGraph>}
 */
export const knowledgeGraph = createKnowledgeGraph();
