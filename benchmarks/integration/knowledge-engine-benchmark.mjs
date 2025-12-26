/**
 * @file Knowledge Engine Performance Benchmarks
 * @module benchmarks/integration/knowledge-engine-benchmark
 *
 * @description
 * Benchmarks for knowledge graph operations:
 * - Triple insertion speed
 * - Query performance on large graphs
 * - Inference execution time
 * - Graph traversal performance
 */

import { suite, randomString } from '../framework.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Generate knowledge graph triple
 * @param {number} entityId - Entity ID
 * @returns {object} Quad object
 */
function generateKnowledgeTriple(entityId) {
  const subject = namedNode(`http://kg.example.org/entity/${entityId}`);
  const predicates = [
    'http://schema.org/name',
    'http://schema.org/description',
    'http://schema.org/category',
    'http://schema.org/relatedTo'
  ];
  const predicate = namedNode(predicates[entityId % predicates.length]);
  const object = literal(`Value for entity ${entityId}`);

  return quad(subject, predicate, object);
}

/**
 * Build knowledge graph
 * @param {object} store - Oxigraph store
 * @param {number} entityCount - Number of entities
 * @param {number} triplesPerEntity - Triples per entity
 */
async function buildKnowledgeGraph(store, entityCount, triplesPerEntity = 4) {
  for (let i = 0; i < entityCount; i++) {
    for (let j = 0; j < triplesPerEntity; j++) {
      const triple = generateKnowledgeTriple(i * triplesPerEntity + j);
      await store.add(triple);
    }
  }
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const knowledgeEngineBenchmarks = suite('Knowledge Engine Performance', {
  'insert entity (4 triples)': {
    setup: () => {
      const store = createStore();
      return { store, entityId: 0 };
    },
    fn: async function() {
      for (let j = 0; j < 4; j++) {
        const triple = generateKnowledgeTriple(this.entityId * 4 + j);
        await this.store.add(triple);
      }
      this.entityId++;
    },
    iterations: 5000,
    warmup: 500
  },

  'query entity by ID (100 entities)': {
    setup: async () => {
      const store = createStore();
      await buildKnowledgeGraph(store, 100, 4);
      return { store };
    },
    fn: async function() {
      const entityId = Math.floor(Math.random() * 100);
      const query = `
        SELECT ?p ?o WHERE {
          <http://kg.example.org/entity/${entityId}> ?p ?o
        }
      `;
      return await this.store.query(query);
    },
    iterations: 10000,
    warmup: 1000
  },

  'query entity by ID (1000 entities)': {
    setup: async () => {
      const store = createStore();
      await buildKnowledgeGraph(store, 1000, 4);
      return { store };
    },
    fn: async function() {
      const entityId = Math.floor(Math.random() * 1000);
      const query = `
        SELECT ?p ?o WHERE {
          <http://kg.example.org/entity/${entityId}> ?p ?o
        }
      `;
      return await this.store.query(query);
    },
    iterations: 5000,
    warmup: 500
  },

  'find entities by property': {
    setup: async () => {
      const store = createStore();
      await buildKnowledgeGraph(store, 500, 4);
      return { store };
    },
    fn: async function() {
      const query = `
        SELECT ?s ?o WHERE {
          ?s <http://schema.org/name> ?o
        }
        LIMIT 50
      `;
      return await this.store.query(query);
    },
    iterations: 3000,
    warmup: 300
  },

  'graph traversal (2 hops)': {
    setup: async () => {
      const store = createStore();
      await buildKnowledgeGraph(store, 200, 4);

      // Add some relatedTo links
      for (let i = 0; i < 200; i += 2) {
        const subject = namedNode(`http://kg.example.org/entity/${i}`);
        const predicate = namedNode('http://schema.org/relatedTo');
        const object = namedNode(`http://kg.example.org/entity/${i + 1}`);
        await store.add(quad(subject, predicate, object));
      }

      return { store };
    },
    fn: async function() {
      const query = `
        SELECT ?entity2 ?name WHERE {
          <http://kg.example.org/entity/0> <http://schema.org/relatedTo> ?entity1 .
          ?entity1 <http://schema.org/relatedTo> ?entity2 .
          ?entity2 <http://schema.org/name> ?name
        }
        LIMIT 20
      `;
      return await this.store.query(query);
    },
    iterations: 2000,
    warmup: 200
  },

  'aggregate query (count by category)': {
    setup: async () => {
      const store = createStore();
      await buildKnowledgeGraph(store, 300, 4);
      return { store };
    },
    fn: async function() {
      const query = `
        SELECT ?category (COUNT(?entity) as ?count) WHERE {
          ?entity <http://schema.org/category> ?category
        }
        GROUP BY ?category
      `;
      return await this.store.query(query);
    },
    iterations: 2000,
    warmup: 200
  },

  'full-text search simulation': {
    setup: async () => {
      const store = createStore();
      await buildKnowledgeGraph(store, 500, 4);
      return { store };
    },
    fn: async function() {
      const query = `
        SELECT ?s ?name WHERE {
          ?s <http://schema.org/name> ?name .
          FILTER(CONTAINS(?name, "entity"))
        }
        LIMIT 50
      `;
      return await this.store.query(query);
    },
    iterations: 1000,
    warmup: 100
  },

  'bulk load (1000 triples)': {
    setup: () => {
      const store = createStore();
      return { store };
    },
    fn: async function() {
      for (let i = 0; i < 1000; i++) {
        const triple = generateKnowledgeTriple(i);
        await this.store.add(triple);
      }
    },
    iterations: 100,
    warmup: 10
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await knowledgeEngineBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
