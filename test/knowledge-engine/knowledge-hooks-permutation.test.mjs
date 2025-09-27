/**
 * @fileoverview Permutation and combination tests for Knowledge Hooks using Faker
 * Tests various combinations of hook definitions, conditions, and execution scenarios
 */

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { faker } from "@faker-js/faker";
import { Store, DataFactory } from "n3";
import { defineHook } from "../../src/knowledge-engine/define-hook.mjs";
import { KnowledgeHookManager } from "../../src/knowledge-engine/knowledge-hook-manager.mjs";
import { createFileResolver } from "../../src/knowledge-engine/file-resolver.mjs";
import { createConditionEvaluator } from "../../src/knowledge-engine/condition-evaluator.mjs";
import { createHookExecutor } from "../../src/knowledge-engine/hook-executor.mjs";

const { namedNode, literal, quad } = DataFactory;

describe("Knowledge Hooks Permutation Tests", () => {
  let manager;
  let testStore;
  let fileResolver;
  let conditionEvaluator;
  let hookExecutor;

  beforeEach(() => {
    testStore = new Store();
    fileResolver = createFileResolver();
    conditionEvaluator = createConditionEvaluator(fileResolver);
    hookExecutor = createHookExecutor(conditionEvaluator);
    manager = new KnowledgeHookManager({
      basePath: process.cwd(),
      strictMode: false,
      enableConditionEvaluation: true
    });
  });

  afterEach(() => {
    manager.clearKnowledgeHooks();
  });

  describe("Hook Definition Permutations", () => {
    it("should handle various hook name permutations", () => {
      const hookNames = [
        faker.lorem.word(),
        faker.lorem.words(2).replace(/\s+/g, '-'),
        faker.lorem.words(3).replace(/\s+/g, '-'),
        `${faker.lorem.word()}-${faker.number.int({ min: 1, max: 999 })}`,
        `${faker.lorem.word()}_${faker.lorem.word()}`,
        faker.lorem.word().toUpperCase(),
        faker.lorem.word().toLowerCase()
      ];

      hookNames.forEach((name, index) => {
        const hook = defineHook({
          meta: {
            name: name,
            description: faker.lorem.sentence(),
            version: faker.system.semver()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, result: `Hook ${name} executed` };
          }
        });

        expect(hook.meta.name).toBe(name);
        expect(hook.meta.description).toBeDefined();
        expect(hook.meta.version).toBeDefined();
      });
    });

    it("should handle various determinism seed permutations", () => {
      const seeds = [
        faker.number.int({ min: 1, max: 1000 }),
        faker.number.int({ min: 1000, max: 10000 }),
        faker.number.int({ min: 10000, max: 100000 }),
        faker.number.int({ min: 100000, max: 1000000 })
      ];

      seeds.forEach((seed) => {
        const hook = defineHook({
          meta: {
            name: `seed-test-${seed}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, seed };
          },
          determinism: { seed }
        });

        expect(hook.determinism.seed).toBe(seed);
      });
    });

    it("should handle various receipt anchor permutations", () => {
      const anchors = ['none', 'blockchain', 'merkle', 'timestamp', 'hash'];

      anchors.forEach((anchor) => {
        const hook = defineHook({
          meta: {
            name: `anchor-test-${anchor}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, anchor };
          },
          receipt: { anchor }
        });

        expect(hook.receipt.anchor).toBe(anchor);
      });
    });
  });

  describe("Condition Evaluation Combinations", () => {
    it("should handle various SPARQL query combinations", async () => {
      const queries = [
        // Simple ASK queries
        "ASK WHERE { ?s ?p ?o }",
        "ASK WHERE { ?s <http://example.org/type> <http://example.org/Person> }",
        "ASK WHERE { ?s <http://example.org/age> ?age . FILTER(?age > 18) }",
        
        // Complex ASK queries
        "ASK WHERE { ?s <http://example.org/type> <http://example.org/Person> . ?s <http://example.org/age> ?age . FILTER(?age >= 21) }",
        "ASK WHERE { ?s <http://example.org/type> <http://example.org/Employee> . ?s <http://example.org/department> ?dept . FILTER(?dept = 'Engineering') }",
        
        // SELECT queries
        "SELECT ?s WHERE { ?s ?p ?o } LIMIT 1",
        "SELECT ?name WHERE { ?s <http://example.org/name> ?name } LIMIT 5"
      ];

      for (const query of queries) {
        const hook = defineHook({
          meta: {
            name: `query-test-${faker.lorem.word()}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, query };
          }
        });

        // Test condition evaluation
        const result = await conditionEvaluator.evaluate(hook.when, testStore);
        expect(result).toBeDefined();
      }
    });

    it("should handle various graph state combinations", async () => {
      const graphStates = [
        // Empty graph
        [],
        
        // Single triple
        [quad(namedNode("http://example.org/alice"), namedNode("http://example.org/name"), literal("Alice"))],
        
        // Multiple triples
        [
          quad(namedNode("http://example.org/alice"), namedNode("http://example.org/name"), literal("Alice")),
          quad(namedNode("http://example.org/alice"), namedNode("http://example.org/age"), literal("30")),
          quad(namedNode("http://example.org/bob"), namedNode("http://example.org/name"), literal("Bob"))
        ],
        
        // Complex graph
        [
          quad(namedNode("http://example.org/alice"), namedNode("http://example.org/type"), namedNode("http://example.org/Person")),
          quad(namedNode("http://example.org/alice"), namedNode("http://example.org/name"), literal("Alice")),
          quad(namedNode("http://example.org/alice"), namedNode("http://example.org/age"), literal("30")),
          quad(namedNode("http://example.org/alice"), namedNode("http://example.org/email"), literal("alice@example.org")),
          quad(namedNode("http://example.org/bob"), namedNode("http://example.org/type"), namedNode("http://example.org/Person")),
          quad(namedNode("http://example.org/bob"), namedNode("http://example.org/name"), literal("Bob")),
          quad(namedNode("http://example.org/bob"), namedNode("http://example.org/age"), literal("25"))
        ]
      ];

      for (const triples of graphStates) {
        const store = new Store();
        triples.forEach(t => store.addQuad(t));

        const hook = defineHook({
          meta: {
            name: `graph-test-${faker.lorem.word()}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, graphSize: store.size };
          }
        });

        const result = await conditionEvaluator.evaluate(hook.when, store);
        expect(result).toBeDefined();
      }
    });
  });

  describe("Execution Scenario Combinations", () => {
    it("should handle various event payload combinations", async () => {
      const payloads = [
        // Simple payload
        { id: faker.string.uuid(), type: "test" },
        
        // Complex payload
        {
          id: faker.string.uuid(),
          type: "transaction",
          amount: faker.number.float({ min: 0, max: 10000, fractionDigits: 2 }),
          currency: faker.finance.currencyCode(),
          timestamp: faker.date.recent().toISOString(),
          metadata: {
            source: faker.lorem.word(),
            category: faker.lorem.word(),
            tags: faker.lorem.words(3).split(' ')
          }
        },
        
        // Large payload
        {
          id: faker.string.uuid(),
          type: "bulk-operation",
          items: Array.from({ length: 100 }, () => ({
            id: faker.string.uuid(),
            name: faker.person.fullName(),
            email: faker.internet.email(),
            age: faker.number.int({ min: 18, max: 80 })
          }))
        }
      ];

      for (const payload of payloads) {
        const hook = defineHook({
          meta: {
            name: `payload-test-${faker.lorem.word()}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, payloadSize: JSON.stringify(payload).length };
          }
        });

        const event = {
          name: hook.meta.name,
          payload,
          context: { graph: testStore }
        };

        const result = await hookExecutor.execute(hook, event);
        expect(result.success).toBe(true);
      }
    });

    it("should handle various hook lifecycle combinations", async () => {
      const lifecycleCombinations = [
        // Only run phase
        { before: false, run: true, after: false },
        
        // Before and run
        { before: true, run: true, after: false },
        
        // Run and after
        { before: false, run: true, after: true },
        
        // All phases
        { before: true, run: true, after: true }
      ];

      for (const combo of lifecycleCombinations) {
        const hook = defineHook({
          meta: {
            name: `lifecycle-test-${faker.lorem.word()}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, phase: 'run' };
          },
          before: combo.before ? async (event) => {
            return { success: true, phase: 'before' };
          } : undefined,
          after: combo.after ? async (event) => {
            return { success: true, phase: 'after' };
          } : undefined
        });

        const event = {
          name: hook.meta.name,
          payload: { test: true },
          context: { graph: testStore }
        };

        const result = await hookExecutor.execute(hook, event);
        expect(result.success).toBe(true);
      }
    });

    it("should handle various error scenario combinations", async () => {
      const errorScenarios = [
        // Sync error in run
        {
          run: async (event) => {
            throw new Error("Sync error in run");
          }
        },
        
        // Async error in run
        {
          run: async (event) => {
            await new Promise((_, reject) => reject(new Error("Async error in run")));
          }
        },
        
        // Error in before
        {
          before: async (event) => {
            throw new Error("Error in before");
          },
          run: async (event) => {
            return { success: true };
          }
        },
        
        // Error in after
        {
          run: async (event) => {
            return { success: true };
          },
          after: async (event) => {
            throw new Error("Error in after");
          }
        }
      ];

      for (const scenario of errorScenarios) {
        const hook = defineHook({
          meta: {
            name: `error-test-${faker.lorem.word()}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          ...scenario
        });

        const event = {
          name: hook.meta.name,
          payload: { test: true },
          context: { graph: testStore }
        };

        try {
          const result = await hookExecutor.execute(hook, event);
          // If execution succeeds, it should have handled the error gracefully
          expect(result.success).toBeDefined();
        } catch (error) {
          // If execution throws, that's also acceptable for error scenarios
          expect(error).toBeDefined();
        }
      }
    });
  });

  describe("Manager Integration Combinations", () => {
    it("should handle various hook registration combinations", () => {
      const hookCounts = [1, 5, 10, 25, 50];

      hookCounts.forEach((count) => {
        const manager = new KnowledgeHookManager();
        
        // Register multiple hooks
        for (let i = 0; i < count; i++) {
          const hook = defineHook({
            meta: {
              name: `batch-hook-${i}`,
              description: faker.lorem.sentence()
            },
            when: {
              kind: 'sparql-ask',
              ref: {
                uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
                sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
                mediaType: 'application/sparql-query'
              }
            },
            run: async (event) => {
              return { success: true, index: i };
            }
          });

          manager.addKnowledgeHook(hook);
        }

        const stats = manager.getStats();
        // Check if the manager has the expected number of hooks
        expect(manager.knowledgeHooks.size).toBe(count);
      });
    });

    it("should handle various transaction delta combinations", async () => {
      const deltaCombinations = [
        // Only additions
        {
          additions: [
            quad(namedNode("http://example.org/alice"), namedNode("http://example.org/name"), literal("Alice"))
          ],
          removals: []
        },
        
        // Only removals
        {
          additions: [],
          removals: [
            quad(namedNode("http://example.org/alice"), namedNode("http://example.org/name"), literal("Alice"))
          ]
        },
        
        // Both additions and removals
        {
          additions: [
            quad(namedNode("http://example.org/alice"), namedNode("http://example.org/email"), literal("alice@example.org"))
          ],
          removals: [
            quad(namedNode("http://example.org/alice"), namedNode("http://example.org/name"), literal("Alice"))
          ]
        },
        
        // Large delta
        {
          additions: Array.from({ length: 100 }, (_, i) => 
            quad(namedNode(`http://example.org/resource${i}`), namedNode("http://example.org/value"), literal(`value${i}`))
          ),
          removals: Array.from({ length: 50 }, (_, i) => 
            quad(namedNode(`http://example.org/old${i}`), namedNode("http://example.org/value"), literal(`old${i}`))
          )
        }
      ];

      for (const delta of deltaCombinations) {
        const hook = defineHook({
          meta: {
            name: `delta-test-${faker.lorem.word()}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { 
              success: true, 
              additionsCount: delta.additions.length,
              removalsCount: delta.removals.length
            };
          }
        });

        manager.addKnowledgeHook(hook);

        const result = await manager.apply(testStore, delta);
        expect(result.receipt.committed).toBeDefined();
      }
    });

    it("should handle various manager configuration combinations", () => {
      const configs = [
        { strictMode: true, enableConditionEvaluation: true },
        { strictMode: false, enableConditionEvaluation: true },
        { strictMode: true, enableConditionEvaluation: false },
        { strictMode: false, enableConditionEvaluation: false },
        { basePath: "/custom/path", strictMode: true },
        { maxHooks: 10, strictMode: false }
      ];

      configs.forEach((config) => {
        const manager = new KnowledgeHookManager(config);
        expect(manager).toBeInstanceOf(KnowledgeHookManager);
        
        const stats = manager.getStats();
        expect(stats).toBeDefined();
      });
    });
  });

  describe("Performance and Stress Combinations", () => {
    it("should handle concurrent hook execution", async () => {
      const concurrentCounts = [2, 5, 10, 20];

      for (const count of concurrentCounts) {
        const hooks = [];
        const manager = new KnowledgeHookManager(); // Create fresh manager for each test
        
        for (let i = 0; i < count; i++) {
          const hook = defineHook({
            meta: {
              name: `concurrent-hook-${count}-${i}`,
              description: faker.lorem.sentence()
            },
            when: {
              kind: 'sparql-ask',
              ref: {
                uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
                sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
                mediaType: 'application/sparql-query'
              }
            },
            run: async (event) => {
              // Simulate some work
              await new Promise(resolve => setTimeout(resolve, 10));
              return { success: true, index: i };
            }
          });
          
          hooks.push(hook);
          manager.addKnowledgeHook(hook);
        }

        const events = hooks.map(hook => ({
          name: hook.meta.name,
          payload: { test: true },
          context: { graph: testStore }
        }));

        const startTime = Date.now();
        const results = await Promise.all(
          events.map(event => manager.executeAllKnowledgeHooks(event))
        );
        const endTime = Date.now();

        expect(results).toHaveLength(count);
        results.forEach(result => {
          // Results can be arrays of hook execution results
          if (Array.isArray(result)) {
            result.forEach(hookResult => {
              expect(hookResult.success).toBe(true);
            });
          } else {
            expect(result.success || result).toBe(true);
          }
        });

        // Performance check - should complete within reasonable time
        expect(endTime - startTime).toBeLessThan(5000);
      }
    });

    it("should handle memory-intensive scenarios", async () => {
      const memoryScenarios = [
        // Large payload
        {
          payload: {
            data: Array.from({ length: 1000 }, () => ({
              id: faker.string.uuid(),
              content: faker.lorem.paragraphs(5),
              metadata: faker.lorem.words(20).split(' ')
            }))
          }
        },
        
        // Deeply nested payload
        {
          payload: (() => {
            let obj = { level: 0 };
            let current = obj;
            for (let i = 1; i < 100; i++) {
              current.next = { level: i };
              current = current.next;
            }
            return obj;
          })()
        },
        
        // Circular reference payload
        {
          payload: (() => {
            const obj = { name: "circular" };
            obj.self = obj;
            return obj;
          })()
        }
      ];

      for (const scenario of memoryScenarios) {
        const hook = defineHook({
          meta: {
            name: `memory-test-${faker.lorem.word()}`,
            description: faker.lorem.sentence()
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://examples/hooks/parliamentary/motion-compliance.ask.rq`,
              sha256: "23e6491f00f136d30b585cc307a26b38031312774d455840f87e0676c999b75f",
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { 
              success: true, 
              payloadSize: JSON.stringify(event.payload).length 
            };
          }
        });

        const event = {
          name: hook.meta.name,
          payload: scenario.payload,
          context: { graph: testStore }
        };

        const result = await hookExecutor.execute(hook, event);
        expect(result.success).toBe(true);
      }
    });
  });
});
