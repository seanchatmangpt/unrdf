import { describe, it, expect, beforeEach } from "vitest";
import { useKnowledgeHooks, defineHook, evaluateHook } from "../../src/composables/use-knowledge-hooks.mjs";
import { initStore, useStoreContext } from "../../src/context/index.mjs";
import { DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useKnowledgeHooks - Comprehensive Implementation", () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore();
  });

  it("provides knowledge hooks interface", async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      
      expect(hooks).toBeDefined();
      expect(typeof hooks.defineHook).toBe("function");
      expect(typeof hooks.evaluateHook).toBe("function");
      expect(typeof hooks.evaluateHooks).toBe("function");
      expect(typeof hooks.getStats).toBe("function");
      expect(hooks.engine).toBeDefined();
    });
  });

  it("defines hooks with SPARQL queries", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'test-sparql',
        query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
        predicates: [
          { kind: 'COUNT', spec: { operator: '>', value: 2 } }
        ]
      });

      expect(hook.id).toBe('test-sparql');
      expect(hook.query).toBe('SELECT ?s WHERE { ?s ex:type ex:Error }');
      expect(hook.predicates).toHaveLength(1);
      expect(hook.predicates[0].kind).toBe('COUNT');
      expect(hook.combine).toBe('AND');
    });
  });

  it("defines hooks with THRESHOLD predicates", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'test-threshold',
        query: 'SELECT ?service ?latency WHERE { ?service ex:latency ?latency }',
        predicates: [
          { kind: 'THRESHOLD', spec: { variable: 'latency', operator: '>', value: 1000 } }
        ]
      });

      expect(hook.id).toBe('test-threshold');
      expect(hook.predicates[0].kind).toBe('THRESHOLD');
      expect(hook.predicates[0].spec.variable).toBe('latency');
      expect(hook.predicates[0].spec.operator).toBe('>');
      expect(hook.predicates[0].spec.value).toBe(1000);
    });
  });

  it("defines hooks with SHACL predicates", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'test-shacl',
        query: 'SELECT ?person WHERE { ?person rdf:type ex:Person }',
        predicates: [
          { kind: 'SHACL', spec: { shape: 'ex:PersonShape', strict: true } }
        ]
      });

      expect(hook.id).toBe('test-shacl');
      expect(hook.predicates[0].kind).toBe('SHACL');
      expect(hook.predicates[0].spec.shape).toBe('ex:PersonShape');
      expect(hook.predicates[0].spec.strict).toBe(true);
    });
  });

  it("defines hooks with OWL predicates", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'test-owl',
        query: 'SELECT ?person WHERE { ?person rdf:type ex:Person }',
        predicates: [
          { kind: 'OWL', spec: { rules: '@prefix ex: <http://example.org/> . { ?x ex:hasParent ?y } => { ?x ex:isChildOf ?y } .' } }
        ]
      });

      expect(hook.id).toBe('test-owl');
      expect(hook.predicates[0].kind).toBe('OWL');
      expect(hook.predicates[0].spec.rules).toContain('ex:hasParent');
    });
  });

  it("defines hooks with ASK predicates", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'test-ask',
        query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
        predicates: [
          { kind: 'ASK', spec: { query: 'ASK WHERE { ?s ex:type ex:Error }', expected: true } }
        ]
      });

      expect(hook.id).toBe('test-ask');
      expect(hook.predicates[0].kind).toBe('ASK');
      expect(hook.predicates[0].spec.query).toContain('ASK WHERE');
      expect(hook.predicates[0].spec.expected).toBe(true);
    });
  });

  it("supports AND/OR combination", async () => {
    await runApp(async () => {
      const andHook = defineHook({
        id: 'and-hook',
        query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
        predicates: [
          { kind: 'COUNT', spec: { operator: '>', value: 0 } },
          { kind: 'COUNT', spec: { operator: '<', value: 10 } }
        ],
        combine: 'AND'
      });

      const orHook = defineHook({
        id: 'or-hook',
        query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
        predicates: [
          { kind: 'COUNT', spec: { operator: '>', value: 5 } },
          { kind: 'COUNT', spec: { operator: '<', value: 2 } }
        ],
        combine: 'OR'
      });

      expect(andHook.combine).toBe('AND');
      expect(orHook.combine).toBe('OR');
    });
  });

  it("validates hook configuration", async () => {
    await runApp(async () => {
      // Missing id
      expect(() => defineHook({ 
        query: 'SELECT ?s WHERE { ?s ?p ?o }'
      })).toThrow('Hook id must be a non-empty string');
      
      // Missing query
      expect(() => defineHook({ 
        id: 'test'
      })).toThrow('Hook query must be a non-empty string');
      
      // Invalid predicates
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: 'not-an-array'
      })).toThrow('Hook predicates must be an array');
      
      // Invalid combine
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        combine: 'INVALID'
      })).toThrow('Hook combine must be "AND" or "OR"');
      
      // Invalid predicate kind
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'INVALID', spec: {} }]
      })).toThrow('Unknown predicate kind: INVALID');
      
      // Missing predicate spec
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'COUNT' }]
      })).toThrow('Predicate must have a spec object');
      
      // Invalid THRESHOLD predicate
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'THRESHOLD', spec: { operator: '>', value: 5 } }]
      })).toThrow('THRESHOLD predicate requires a variable');
      
      // Invalid COUNT predicate
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'COUNT', spec: { operator: 'INVALID', value: 5 } }]
      })).toThrow('COUNT predicate requires a valid operator');
      
      // Invalid SHACL predicate
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'SHACL', spec: {} }]
      })).toThrow('SHACL predicate requires a shape IRI');
      
      // Invalid OWL predicate
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'OWL', spec: {} }]
      })).toThrow('OWL predicate requires rules');
      
      // Invalid ASK predicate
      expect(() => defineHook({ 
        id: 'test',
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'ASK', spec: {} }]
      })).toThrow('ASK predicate requires a query');
    });
  });

  it("evaluates COUNT predicates", async () => {
    await runApp(async () => {
      const storeContext = useStoreContext();
      
      // Add test data
      storeContext.add(quad(namedNode('http://example.org/error1'), namedNode('http://example.org/type'), namedNode('http://example.org/Error')));
      storeContext.add(quad(namedNode('http://example.org/error2'), namedNode('http://example.org/type'), namedNode('http://example.org/Error')));
      storeContext.add(quad(namedNode('http://example.org/error3'), namedNode('http://example.org/type'), namedNode('http://example.org/Error')));
      
      const hook = defineHook({
        id: 'error-count',
        query: `
          PREFIX ex: <http://example.org/>
          SELECT ?s WHERE { ?s ex:type ex:Error }
        `,
        predicates: [
          { kind: 'COUNT', spec: { operator: '>', value: 2 } }
        ]
      });

      const result = await evaluateHook(hook);

      expect(result.hookId).toBe('error-count');
      expect(result.fired).toBe(true);
      expect(result.data.count).toBe(3);
      expect(result.data.predicateResults).toHaveLength(1);
      expect(result.data.predicateResults[0].kind).toBe('COUNT');
      expect(result.data.predicateResults[0].fired).toBe(true);
      expect(result.duration).toBeGreaterThan(0);
      expect(result.timestamp).toBeDefined();
    });
  });

  it("evaluates THRESHOLD predicates", async () => {
    await runApp(async () => {
      const storeContext = useStoreContext();
      
      // Add test data with latency values
      storeContext.add(quad(namedNode('http://example.org/service1'), namedNode('http://example.org/latency'), literal('1500')));
      storeContext.add(quad(namedNode('http://example.org/service2'), namedNode('http://example.org/latency'), literal('2500')));
      storeContext.add(quad(namedNode('http://example.org/service3'), namedNode('http://example.org/latency'), literal('800')));
      
      const hook = defineHook({
        id: 'latency-threshold',
        query: `
          PREFIX ex: <http://example.org/>
          SELECT ?service ?latency WHERE { ?service ex:latency ?latency }
        `,
        predicates: [
          { kind: 'THRESHOLD', spec: { variable: 'latency', operator: '>', value: 2000 } }
        ]
      });

      const result = await evaluateHook(hook);

      expect(result.hookId).toBe('latency-threshold');
      expect(result.fired).toBe(false); // Average latency (1600) is not > 2000
      expect(result.data.count).toBe(3);
      expect(result.data.predicateResults).toHaveLength(1);
      expect(result.data.predicateResults[0].kind).toBe('THRESHOLD');
      expect(result.data.predicateResults[0].fired).toBe(false);
      expect(result.data.predicateResults[0].data.averageValue).toBeCloseTo(1600, 0);
    });
  });

  it("handles empty query results", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'empty-results',
        query: 'SELECT ?s WHERE { ?s ex:nonexistent ?o }',
        predicates: [
          { kind: 'COUNT', spec: { operator: '>', value: 0 } }
        ]
      });

      const result = await evaluateHook(hook);

      expect(result.hookId).toBe('empty-results');
      expect(result.fired).toBe(false);
      expect(result.data.count).toBe(0);
      expect(result.data.predicateResults[0].fired).toBe(false);
    });
  });

  it("handles query errors gracefully", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'invalid-query',
        query: 'INVALID SPARQL QUERY',
        predicates: [
          { kind: 'COUNT', spec: { operator: '>', value: 0 } }
        ]
      });

      const result = await evaluateHook(hook);

      expect(result.hookId).toBe('invalid-query');
      expect(result.fired).toBe(false);
      expect(result.error).toBeDefined();
      expect(result.duration).toBeGreaterThan(0);
    });
  });

  it("supports different comparison operators", async () => {
    await runApp(async () => {
      const storeContext = useStoreContext();
      storeContext.add(quad(namedNode('http://example.org/item1'), namedNode('http://example.org/type'), namedNode('http://example.org/Item')));
      storeContext.add(quad(namedNode('http://example.org/item2'), namedNode('http://example.org/type'), namedNode('http://example.org/Item')));

      const operators = ['>', '<', '>=', '<=', '==', '!='];
      const expectedResults = [true, false, true, true, false, true]; // count=2, value=2

      for (let i = 0; i < operators.length; i++) {
        const hook = defineHook({
          id: `test-${operators[i]}`,
          query: `
            PREFIX ex: <http://example.org/>
            SELECT ?s WHERE { ?s ex:type ex:Item }
          `,
          predicates: [
            { kind: 'COUNT', spec: { operator: operators[i], value: 2 } }
          ]
        });

        const result = await evaluateHook(hook);
        expect(result.fired).toBe(expectedResults[i]);
      }
    });
  });

  it("evaluates multiple hooks", async () => {
    await runApp(async () => {
      const storeContext = useStoreContext();
      
      // Add test data
      storeContext.add(quad(namedNode('http://example.org/error1'), namedNode('http://example.org/type'), namedNode('http://example.org/Error')));
      storeContext.add(quad(namedNode('http://example.org/service1'), namedNode('http://example.org/latency'), literal('3000')));
      
      const hooks = [
        defineHook({
          id: 'error-count',
          query: `
            PREFIX ex: <http://example.org/>
            SELECT ?s WHERE { ?s ex:type ex:Error }
          `,
          predicates: [
            { kind: 'COUNT', spec: { operator: '>', value: 0 } }
          ]
        }),
        defineHook({
          id: 'latency-check',
          query: `
            PREFIX ex: <http://example.org/>
            SELECT ?service ?latency WHERE { ?service ex:latency ?latency }
          `,
          predicates: [
            { kind: 'THRESHOLD', spec: { variable: 'latency', operator: '>', value: 2000 } }
          ]
        })
      ];

      const hooksComposable = useKnowledgeHooks();
      const results = await hooksComposable.evaluateHooks(hooks);

      expect(results).toHaveLength(2);
      expect(results[0].hookId).toBe('error-count');
      expect(results[0].fired).toBe(true);
      expect(results[1].hookId).toBe('latency-check');
      expect(results[1].fired).toBe(true);
    });
  });

  it("provides comprehensive statistics", async () => {
    await runApp(async () => {
      const results = [
        { 
          hookId: 'hook1', 
          fired: true, 
          duration: 100,
          data: {
            predicateResults: [
              { kind: 'COUNT', fired: true },
              { kind: 'THRESHOLD', fired: false }
            ]
          }
        },
        { 
          hookId: 'hook2', 
          fired: false, 
          duration: 200,
          data: {
            predicateResults: [
              { kind: 'SHACL', fired: false }
            ]
          }
        },
        { 
          hookId: 'hook3', 
          fired: true, 
          duration: 150,
          data: {
            predicateResults: [
              { kind: 'OWL', fired: true }
            ]
          }
        },
        { 
          hookId: 'hook4', 
          fired: false, 
          error: 'Query failed', 
          duration: 50
        }
      ];

      const hooksComposable = useKnowledgeHooks();
      const stats = hooksComposable.getStats(results);

      expect(stats.total).toBe(4);
      expect(stats.fired).toBe(2);
      expect(stats.fireRate).toBe(0.5);
      expect(stats.avgDuration).toBe(125);
      expect(stats.errors).toBe(1);
      expect(stats.errorRate).toBe(0.25);
      expect(stats.predicates.COUNT).toBe(1);
      expect(stats.predicates.THRESHOLD).toBe(1);
      expect(stats.predicates.SHACL).toBe(1);
      expect(stats.predicates.OWL).toBe(1);
    });
  });

  it("handles THRESHOLD with non-numeric values", async () => {
    await runApp(async () => {
      const storeContext = useStoreContext();
      
      // Add test data with mixed types
      storeContext.add(quad(namedNode('http://example.org/service1'), namedNode('http://example.org/latency'), literal('not-a-number')));
      storeContext.add(quad(namedNode('http://example.org/service2'), namedNode('http://example.org/latency'), literal('1500')));
      
      const hook = defineHook({
        id: 'mixed-types',
        query: `
          PREFIX ex: <http://example.org/>
          SELECT ?service ?latency WHERE { ?service ex:latency ?latency }
        `,
        predicates: [
          { kind: 'THRESHOLD', spec: { variable: 'latency', operator: '>', value: 1000 } }
        ]
      });

      const result = await evaluateHook(hook);

      expect(result.hookId).toBe('mixed-types');
      expect(result.fired).toBe(true); // Only numeric value (1500) > 1000
      expect(result.data.predicateResults[0].data.averageValue).toBe(1500); // Only one valid numeric value
    });
  });

  it("uses default values when not provided", async () => {
    await runApp(async () => {
      const hook = defineHook({
        id: 'default-values',
        query: 'SELECT ?s WHERE { ?s ex:type ex:Item }'
      });

      expect(hook.predicates).toEqual([]);
      expect(hook.combine).toBe('AND');
    });
  });
});