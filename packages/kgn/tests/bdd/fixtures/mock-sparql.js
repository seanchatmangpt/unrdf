/**
 * Mock SPARQL Query Engine for BDD Testing
 * Provides realistic RDF query responses for template testing
 */
import { vi } from 'vitest';

export function createMockSPARQL() {
  // Mock RDF data store
  const rdfStore = {
    classes: [
      {
        class: 'http://example.org/Person',
        label: 'Person',
        comment: 'A person entity'
      },
      {
        class: 'http://example.org/Organization',
        label: 'Organization',
        comment: 'An organizational entity'
      },
      {
        class: 'http://example.org/API',
        label: 'API',
        comment: 'Application Programming Interface'
      }
    ],
    properties: [
      {
        prop: 'http://example.org/name',
        domain: 'http://example.org/Person',
        range: 'http://www.w3.org/2001/XMLSchema#string',
        label: 'name'
      },
      {
        prop: 'http://example.org/age',
        domain: 'http://example.org/Person',
        range: 'http://www.w3.org/2001/XMLSchema#integer',
        label: 'age'
      },
      {
        prop: 'http://example.org/endpoint',
        domain: 'http://example.org/API',
        range: 'http://example.org/Endpoint',
        label: 'endpoint'
      }
    ],
    endpoints: [
      {
        endpoint: 'http://example.org/endpoints/users',
        path: '/users',
        method: 'GET',
        api: 'http://example.org/UserAPI'
      },
      {
        endpoint: 'http://example.org/endpoints/users-post',
        path: '/users',
        method: 'POST',
        api: 'http://example.org/UserAPI'
      }
    ],
    instances: [
      {
        instance: 'http://example.org/john',
        type: 'http://example.org/Person',
        name: 'John Doe',
        age: 30
      }
    ]
  };

  // Query response patterns
  const queryPatterns = [
    {
      pattern: /SELECT.*\?class.*\?label.*owl:Class/i,
      response: () => rdfStore.classes.map(c => ({ class: c.class, label: c.label }))
    },
    {
      pattern: /SELECT.*\?prop.*\?domain.*\?range.*DatatypeProperty/i,
      response: () => rdfStore.properties.map(p => ({
        prop: p.prop,
        domain: p.domain,
        range: p.range
      }))
    },
    {
      pattern: /SELECT.*\?endpoint.*\?path.*\?method/i,
      response: () => rdfStore.endpoints.map(e => ({
        endpoint: e.endpoint,
        path: e.path,
        method: e.method
      }))
    },
    {
      pattern: /SELECT.*\?instance.*\?type.*\?name/i,
      response: () => rdfStore.instances.map(i => ({
        instance: i.instance,
        type: i.type,
        name: i.name
      }))
    }
  ];

  const mockSPARQL = {
    // Core query execution
    query: vi.fn(async (sparqlQuery, options = {}) => {
      // Simulate query execution time
      await new Promise(resolve => setTimeout(resolve, Math.random() * 50 + 10));

      // Find matching pattern
      const matchedPattern = queryPatterns.find(p => p.pattern.test(sparqlQuery));

      if (matchedPattern) {
        const results = matchedPattern.response();

        return {
          results: {
            bindings: results.map(result => {
              const binding = {};
              Object.entries(result).forEach(([key, value]) => {
                binding[key] = { type: 'uri', value };
              });
              return binding;
            })
          },
          head: {
            vars: Object.keys(results[0] || {})
          },
          queryTime: Math.random() * 100 + 20,
          success: true
        };
      }

      // Default empty response for unmatched queries
      return {
        results: { bindings: [] },
        head: { vars: [] },
        queryTime: Math.random() * 50 + 10,
        success: true
      };
    }),

    // Batch query execution
    batchQuery: vi.fn(async (queries) => {
      const results = {};

      for (const [queryId, query] of Object.entries(queries)) {
        results[queryId] = await mockSPARQL.query(query);
      }

      return results;
    }),

    // Update operations
    update: vi.fn(async (sparqlUpdate) => {
      // Mock update operation
      await new Promise(resolve => setTimeout(resolve, Math.random() * 30 + 5));

      return {
        success: true,
        modified: Math.floor(Math.random() * 10) + 1,
        updateTime: Math.random() * 100 + 10
      };
    }),

    // Graph operations
    loadGraph: vi.fn(async (graphUri, rdfData) => {
      return {
        success: true,
        graphUri,
        triplesLoaded: Math.floor(Math.random() * 1000) + 100,
        loadTime: Math.random() * 500 + 100
      };
    }),

    clearGraph: vi.fn(async (graphUri) => {
      return {
        success: true,
        graphUri,
        triplesRemoved: Math.floor(Math.random() * 1000) + 100,
        clearTime: Math.random() * 200 + 50
      };
    }),

    // Custom query helpers for templates
    getClasses: vi.fn(async () => {
      return rdfStore.classes;
    }),

    getProperties: vi.fn(async (classUri) => {
      return rdfStore.properties.filter(p =>
        !classUri || p.domain === classUri
      );
    }),

    getInstances: vi.fn(async (classUri) => {
      return rdfStore.instances.filter(i =>
        !classUri || i.type === classUri
      );
    }),

    // Template integration helpers
    executeTemplateQuery: vi.fn(async (templateQuery, bindings = {}) => {
      // Replace template variables in query
      let processedQuery = templateQuery;

      Object.entries(bindings).forEach(([key, value]) => {
        processedQuery = processedQuery.replace(
          new RegExp(`\\{\\{\\s*${key}\\s*\\}\\}`, 'g'),
          value
        );
      });

      return mockSPARQL.query(processedQuery);
    }),

    // Inference simulation
    infer: vi.fn(async (rules = []) => {
      // Mock inference results
      const inferredTriples = Math.floor(Math.random() * 50) + 10;

      return {
        success: true,
        rulesApplied: rules.length,
        inferredTriples,
        inferenceTime: Math.random() * 1000 + 200
      };
    }),

    // Performance metrics
    getMetrics: vi.fn(() => ({
      queriesExecuted: Math.floor(Math.random() * 1000) + 100,
      averageQueryTime: Math.random() * 100 + 50,
      cacheHitRate: Math.random() * 0.4 + 0.6,
      totalTriples: rdfStore.classes.length + rdfStore.properties.length + rdfStore.instances.length
    })),

    // Test utilities
    __addMockData: (type, data) => {
      if (rdfStore[type]) {
        rdfStore[type].push(...(Array.isArray(data) ? data : [data]));
      }
    },

    __addQueryPattern: (pattern, responseFunction) => {
      queryPatterns.push({ pattern, response: responseFunction });
    },

    __reset: () => {
      // Reset to original mock data
      rdfStore.classes.length = 3;
      rdfStore.properties.length = 3;
      rdfStore.endpoints.length = 2;
      rdfStore.instances.length = 1;

      // Clear custom patterns
      queryPatterns.length = 4;

      // Reset mock call history
      Object.values(mockSPARQL).forEach(fn => {
        if (vi.isMockFunction(fn)) {
          fn.mockClear();
        }
      });
    },

    __getRdfStore: () => ({ ...rdfStore }),

    // Simulate query optimization
    __optimizeQuery: vi.fn((query) => {
      return {
        originalQuery: query,
        optimizedQuery: query.replace(/\s+/g, ' ').trim(),
        estimatedCost: Math.random() * 1000,
        optimizations: ['removed-redundant-joins', 'reordered-clauses']
      };
    }),

    // Simulate connection health
    healthCheck: vi.fn(async () => {
      return {
        status: 'healthy',
        responseTime: Math.random() * 50 + 10,
        availableGraphs: ['default', 'ontology', 'instances'],
        version: '1.0.0-mock'
      };
    })
  };

  return mockSPARQL;
}