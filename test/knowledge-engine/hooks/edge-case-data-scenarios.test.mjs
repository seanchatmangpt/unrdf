/**
 * @file Edge Case Data Scenarios Tests
 * @module edge-case-data-scenarios
 * 
 * @description
 * Tests for edge case data scenarios including empty graphs, circular references,
 * unicode normalization, timezone handling, and floating-point precision in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink, mkdir } from 'fs/promises';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store, DataFactory, Parser } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

const { namedNode, literal, quad } = DataFactory;

// Mock condition evaluator at module level
vi.mock('../../../src/knowledge-engine/condition-evaluator.mjs', () => ({
  evaluateCondition: vi.fn().mockResolvedValue(true),
  createConditionEvaluator: vi.fn().mockReturnValue({
    evaluate: vi.fn().mockResolvedValue(true),
    isSatisfied: vi.fn().mockResolvedValue(true)
  }),
  validateCondition: vi.fn().mockReturnValue({ valid: true })
}));

describe('Edge Case Data Scenarios', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-edge-case-test-${Date.now()}`);
    await require('fs/promises').mkdir(tempDir, { recursive: true });
    manager = new KnowledgeHookManager({ basePath: tempDir });
    testStore = new Store();
  });

  afterEach(async () => {
    try {
      await require('fs/promises').rm(tempDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('Empty Graphs', () => {
    it('should handle completely empty RDF graphs', async () => {
      const query = join(tempDir, 'empty-graph.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const emptyStore = new Store();
      
      const hook = defineHook({
        meta: { name: 'empty-graph-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || emptyStore;
          
          // Analyze empty graph characteristics
          const analysis = {
            quadCount: graph.size,
            isEmpty: graph.size === 0,
            subjects: new Set(),
            predicates: new Set(),
            objects: new Set()
          };
          
          // Try to iterate over empty graph
          const quads = graph.getQuads();
          for (const quad of quads) {
            analysis.subjects.add(quad.subject.value);
            analysis.predicates.add(quad.predicate.value);
            analysis.objects.add(quad.object.value);
          }
          
          analysis.uniqueSubjects = analysis.subjects.size;
          analysis.uniquePredicates = analysis.predicates.size;
          analysis.uniqueObjects = analysis.objects.size;
          
          // Test operations on empty graph
          const operations = {
            getQuadsWithSubject: graph.getQuads(namedNode('http://example.org/test')).length,
            getQuadsWithPredicate: graph.getQuads(null, namedNode('http://example.org/prop')).length,
            getQuadsWithObject: graph.getQuads(null, null, literal('test')).length,
            addQuadToEmpty: false,
            removeQuadFromEmpty: false
          };
          
          // Try adding and removing from empty graph
          try {
            const testQuad = quad(
              namedNode('http://example.org/s'),
              namedNode('http://example.org/p'),
              literal('test')
            );
            
            graph.addQuad(testQuad);
            operations.addQuadToEmpty = graph.size === 1;
            
            graph.removeQuad(testQuad);
            operations.removeQuadFromEmpty = graph.size === 0;
          } catch (error) {
            operations.error = error.message;
          }
          
          return { 
            success: true, 
            analysis,
            operations,
            handlesEmptyGraph: analysis.isEmpty && operations.addQuadToEmpty && operations.removeQuadFromEmpty
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: emptyStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle empty graphs correctly
      expect(results[0].analysis.isEmpty).toBe(true);
      expect(results[0].analysis.quadCount).toBe(0);
      expect(results[0].analysis.uniqueSubjects).toBe(0);
      expect(results[0].handlesEmptyGraph).toBe(true);
    });

    it('should handle graphs with only blank nodes', async () => {
      const query = join(tempDir, 'blank-nodes.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const blankNodeStore = new Store();
      
      // Add quads with only blank nodes
      const blankNode1 = DataFactory.blankNode('b1');
      const blankNode2 = DataFactory.blankNode('b2');
      
      blankNodeStore.addQuad(quad(
        blankNode1,
        namedNode('http://example.org/prop'),
        blankNode2
      ));
      
      blankNodeStore.addQuad(quad(
        blankNode2,
        namedNode('http://example.org/prop'),
        literal('value')
      ));
      
      const hook = defineHook({
        meta: { name: 'blank-nodes-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || blankNodeStore;
          
          const analysis = {
            quadCount: graph.size,
            blankNodeCount: 0,
            namedNodeCount: 0,
            literalCount: 0
          };
          
          // Analyze node types
          const quads = graph.getQuads();
          const blankNodes = new Set();
          
          for (const q of quads) {
            [q.subject, q.predicate, q.object].forEach(term => {
              if (term.termType === 'BlankNode') {
                analysis.blankNodeCount++;
                blankNodes.add(term.value);
              } else if (term.termType === 'NamedNode') {
                analysis.namedNodeCount++;
              } else if (term.termType === 'Literal') {
                analysis.literalCount++;
              }
            });
          }
          
          analysis.uniqueBlankNodes = blankNodes.size;
          
          // Test blank node operations
          const operations = {
            canQueryBlankNodes: false,
            canCreateBlankNodes: false,
            blankNodesAreUnique: false
          };
          
          try {
            // Test querying with blank nodes
            const queryResult = graph.getQuads(blankNode1);
            operations.canQueryBlankNodes = queryResult.length > 0;
            
            // Test creating new blank nodes
            const newBlankNode = DataFactory.blankNode();
            operations.canCreateBlankNodes = newBlankNode.termType === 'BlankNode';
            
            // Test blank node uniqueness
            const anotherBlankNode = DataFactory.blankNode();
            operations.blankNodesAreUnique = newBlankNode.value !== anotherBlankNode.value;
            
          } catch (error) {
            operations.error = error.message;
          }
          
          return { 
            success: true, 
            analysis,
            operations,
            hasOnlyBlankNodes: analysis.namedNodeCount === 1 && analysis.blankNodeCount > 0 // 1 predicate is named
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: blankNodeStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle blank nodes correctly
      expect(results[0].analysis.quadCount).toBe(2);
      expect(results[0].analysis.uniqueBlankNodes).toBe(2);
      expect(results[0].operations.canQueryBlankNodes).toBe(true);
      expect(results[0].operations.blankNodesAreUnique).toBe(true);
    });

    it('should handle graphs with only literals', async () => {
      const query = join(tempDir, 'only-literals.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const literalStore = new Store();
      
      // Create a graph where objects are all literals
      const subject = namedNode('http://example.org/subject');
      
      literalStore.addQuad(quad(
        subject,
        namedNode('http://example.org/stringProp'),
        literal('string value')
      ));
      
      literalStore.addQuad(quad(
        subject,
        namedNode('http://example.org/intProp'),
        literal('42', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ));
      
      literalStore.addQuad(quad(
        subject,
        namedNode('http://example.org/boolProp'),
        literal('true', namedNode('http://www.w3.org/2001/XMLSchema#boolean'))
      ));
      
      literalStore.addQuad(quad(
        subject,
        namedNode('http://example.org/dateProp'),
        literal('2023-12-01T10:30:00Z', namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
      ));
      
      const hook = defineHook({
        meta: { name: 'only-literals-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || literalStore;
          
          const analysis = {
            quadCount: graph.size,
            literalTypes: {},
            languages: new Set(),
            datatypes: new Set()
          };
          
          // Analyze literal types
          const quads = graph.getQuads();
          for (const q of quads) {
            if (q.object.termType === 'Literal') {
              const literalValue = q.object.value;
              const datatype = q.object.datatype?.value;
              const language = q.object.language;
              
              if (datatype) {
                analysis.datatypes.add(datatype);
                const shortType = datatype.split('#')[1] || datatype;
                analysis.literalTypes[shortType] = (analysis.literalTypes[shortType] || 0) + 1;
              }
              
              if (language) {
                analysis.languages.add(language);
              }
              
              if (!datatype && !language) {
                analysis.literalTypes['string'] = (analysis.literalTypes['string'] || 0) + 1;
              }
            }
          }
          
          analysis.uniqueDatatypes = analysis.datatypes.size;
          analysis.uniqueLanguages = analysis.languages.size;
          
          // Test literal operations
          const operations = {
            canParseInteger: false,
            canParseBoolean: false,
            canParseDateTime: false,
            handlesTypedLiterals: false
          };
          
          try {
            const intQuads = graph.getQuads(null, namedNode('http://example.org/intProp'));
            if (intQuads.length > 0) {
              const intValue = parseInt(intQuads[0].object.value);
              operations.canParseInteger = !isNaN(intValue) && intValue === 42;
            }
            
            const boolQuads = graph.getQuads(null, namedNode('http://example.org/boolProp'));
            if (boolQuads.length > 0) {
              operations.canParseBoolean = boolQuads[0].object.value === 'true';
            }
            
            const dateQuads = graph.getQuads(null, namedNode('http://example.org/dateProp'));
            if (dateQuads.length > 0) {
              const dateValue = new Date(dateQuads[0].object.value);
              operations.canParseDateTime = !isNaN(dateValue.getTime());
            }
            
            operations.handlesTypedLiterals = operations.canParseInteger && 
                                            operations.canParseBoolean && 
                                            operations.canParseDateTime;
            
          } catch (error) {
            operations.error = error.message;
          }
          
          return { 
            success: true, 
            analysis,
            operations,
            hasVariousLiteralTypes: analysis.uniqueDatatypes >= 3
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: literalStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle various literal types correctly
      expect(results[0].analysis.quadCount).toBe(4);
      expect(results[0].analysis.uniqueDatatypes).toBe(3);
      expect(results[0].operations.handlesTypedLiterals).toBe(true);
      expect(results[0].hasVariousLiteralTypes).toBe(true);
    });
  });

  describe('Circular References in Data', () => {
    it('should handle simple circular references', async () => {
      const query = join(tempDir, 'circular-simple.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const circularStore = new Store();
      
      // Create simple circular reference: A -> B -> A
      const nodeA = namedNode('http://example.org/A');
      const nodeB = namedNode('http://example.org/B');
      const prop = namedNode('http://example.org/references');
      
      circularStore.addQuad(quad(nodeA, prop, nodeB));
      circularStore.addQuad(quad(nodeB, prop, nodeA));
      
      const hook = defineHook({
        meta: { name: 'circular-simple-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || circularStore;
          
          // Detect circular references
          const visited = new Set();
          const recursionStack = new Set();
          const cycles = [];
          
          const detectCycles = (node, path = []) => {
            if (recursionStack.has(node)) {
              // Found a cycle
              const cycleStart = path.indexOf(node);
              cycles.push(path.slice(cycleStart).concat([node]));
              return;
            }
            
            if (visited.has(node)) {
              return;
            }
            
            visited.add(node);
            recursionStack.add(node);
            
            // Follow outgoing edges
            const outgoing = graph.getQuads(namedNode(node));
            for (const q of outgoing) {
              if (q.object.termType === 'NamedNode') {
                detectCycles(q.object.value, [...path, node]);
              }
            }
            
            recursionStack.delete(node);
          };
          
          // Start cycle detection from all nodes
          const allNodes = new Set();
          const quads = graph.getQuads();
          for (const q of quads) {
            if (q.subject.termType === 'NamedNode') allNodes.add(q.subject.value);
            if (q.object.termType === 'NamedNode') allNodes.add(q.object.value);
          }
          
          for (const node of allNodes) {
            if (!visited.has(node)) {
              detectCycles(node);
            }
          }
          
          const analysis = {
            quadCount: graph.size,
            nodeCount: allNodes.size,
            cycleCount: cycles.length,
            cycles: cycles.map(cycle => cycle.map(node => node.split('/').pop())),
            hasCycles: cycles.length > 0,
            maxCycleLength: cycles.length > 0 ? Math.max(...cycles.map(c => c.length)) : 0
          };
          
          return { 
            success: true, 
            analysis,
            detectedSimpleCycle: analysis.hasCycles && analysis.cycles.some(c => c.length === 3)
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: circularStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should detect simple circular reference
      expect(results[0].analysis.hasCycles).toBe(true);
      expect(results[0].analysis.cycleCount).toBe(1);
      expect(results[0].detectedSimpleCycle).toBe(true);
    });

    it('should handle complex circular reference chains', async () => {
      const query = join(tempDir, 'circular-complex.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const complexCircularStore = new Store();
      
      // Create complex circular reference: A -> B -> C -> D -> B (cycle), E -> F -> G -> E (separate cycle)
      const nodes = ['A', 'B', 'C', 'D', 'E', 'F', 'G'].map(id => 
        namedNode(`http://example.org/${id}`)
      );
      const [A, B, C, D, E, F, G] = nodes;
      const prop = namedNode('http://example.org/references');
      
      // First cycle: A -> B -> C -> D -> B
      complexCircularStore.addQuad(quad(A, prop, B));
      complexCircularStore.addQuad(quad(B, prop, C));
      complexCircularStore.addQuad(quad(C, prop, D));
      complexCircularStore.addQuad(quad(D, prop, B)); // Back to B, creating cycle
      
      // Second cycle: E -> F -> G -> E
      complexCircularStore.addQuad(quad(E, prop, F));
      complexCircularStore.addQuad(quad(F, prop, G));
      complexCircularStore.addQuad(quad(G, prop, E)); // Back to E, creating cycle
      
      // Connection between cycles
      complexCircularStore.addQuad(quad(A, prop, E));
      
      const hook = defineHook({
        meta: { name: 'circular-complex-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || complexCircularStore;
          
          // Advanced cycle detection using Tarjan's algorithm
          let index = 0;
          const stack = [];
          const indices = new Map();
          const lowLinks = new Map();
          const onStack = new Set();
          const stronglyConnectedComponents = [];
          
          const strongConnect = (node) => {
            indices.set(node, index);
            lowLinks.set(node, index);
            index++;
            stack.push(node);
            onStack.add(node);
            
            // Follow outgoing edges
            const outgoing = graph.getQuads(namedNode(node));
            for (const q of outgoing) {
              if (q.object.termType === 'NamedNode') {
                const successor = q.object.value;
                
                if (!indices.has(successor)) {
                  strongConnect(successor);
                  lowLinks.set(node, Math.min(lowLinks.get(node), lowLinks.get(successor)));
                } else if (onStack.has(successor)) {
                  lowLinks.set(node, Math.min(lowLinks.get(node), indices.get(successor)));
                }
              }
            }
            
            // If node is a root node, pop the stack and create an SCC
            if (lowLinks.get(node) === indices.get(node)) {
              const component = [];
              let w;
              do {
                w = stack.pop();
                onStack.delete(w);
                component.push(w);
              } while (w !== node);
              
              if (component.length > 1) {
                stronglyConnectedComponents.push(component);
              }
            }
          };
          
          // Get all nodes
          const allNodes = new Set();
          const quads = graph.getQuads();
          for (const q of quads) {
            if (q.subject.termType === 'NamedNode') allNodes.add(q.subject.value);
            if (q.object.termType === 'NamedNode') allNodes.add(q.object.value);
          }
          
          // Run Tarjan's algorithm
          for (const node of allNodes) {
            if (!indices.has(node)) {
              strongConnect(node);
            }
          }
          
          const analysis = {
            quadCount: graph.size,
            nodeCount: allNodes.size,
            stronglyConnectedComponents: stronglyConnectedComponents.length,
            componentSizes: stronglyConnectedComponents.map(c => c.length),
            largestComponent: stronglyConnectedComponents.length > 0 ? 
              Math.max(...stronglyConnectedComponents.map(c => c.length)) : 0,
            components: stronglyConnectedComponents.map(component => 
              component.map(node => node.split('/').pop())
            )
          };
          
          return { 
            success: true, 
            analysis,
            hasMultipleCycles: analysis.stronglyConnectedComponents > 1,
            hasComplexCycles: analysis.largestComponent > 2
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: complexCircularStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should detect multiple complex cycles
      expect(results[0].analysis.stronglyConnectedComponents).toBe(2);
      expect(results[0].hasMultipleCycles).toBe(true);
      expect(results[0].hasComplexCycles).toBe(true);
      expect(results[0].analysis.largestComponent).toBe(3);
    });

    it('should handle self-referencing nodes', async () => {
      const query = join(tempDir, 'self-reference.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const selfRefStore = new Store();
      
      // Create self-referencing nodes
      const nodeA = namedNode('http://example.org/A');
      const nodeB = namedNode('http://example.org/B');
      const selfProp = namedNode('http://example.org/self');
      const otherProp = namedNode('http://example.org/other');
      
      // A references itself
      selfRefStore.addQuad(quad(nodeA, selfProp, nodeA));
      
      // B references itself and A
      selfRefStore.addQuad(quad(nodeB, selfProp, nodeB));
      selfRefStore.addQuad(quad(nodeB, otherProp, nodeA));
      
      // A references B (creating additional complexity)
      selfRefStore.addQuad(quad(nodeA, otherProp, nodeB));
      
      const hook = defineHook({
        meta: { name: 'self-reference-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || selfRefStore;
          
          const analysis = {
            quadCount: graph.size,
            selfReferences: [],
            mutualReferences: [],
            totalNodes: new Set()
          };
          
          // Detect self-references and mutual references
          const quads = graph.getQuads();
          for (const q of quads) {
            analysis.totalNodes.add(q.subject.value);
            if (q.object.termType === 'NamedNode') {
              analysis.totalNodes.add(q.object.value);
              
              // Check for self-reference
              if (q.subject.value === q.object.value) {
                analysis.selfReferences.push({
                  node: q.subject.value.split('/').pop(),
                  property: q.predicate.value.split('/').pop()
                });
              }
            }
          }
          
          // Check for mutual references (A -> B and B -> A)
          const nodeList = Array.from(analysis.totalNodes);
          for (let i = 0; i < nodeList.length; i++) {
            for (let j = i + 1; j < nodeList.length; j++) {
              const nodeA = nodeList[i];
              const nodeB = nodeList[j];
              
              const aToB = graph.getQuads(namedNode(nodeA), null, namedNode(nodeB));
              const bToA = graph.getQuads(namedNode(nodeB), null, namedNode(nodeA));
              
              if (aToB.length > 0 && bToA.length > 0) {
                analysis.mutualReferences.push({
                  nodeA: nodeA.split('/').pop(),
                  nodeB: nodeB.split('/').pop(),
                  aToB: aToB.map(q => q.predicate.value.split('/').pop()),
                  bToA: bToA.map(q => q.predicate.value.split('/').pop())
                });
              }
            }
          }
          
          analysis.nodeCount = analysis.totalNodes.size;
          analysis.hasSelfReferences = analysis.selfReferences.length > 0;
          analysis.hasMutualReferences = analysis.mutualReferences.length > 0;
          
          return { 
            success: true, 
            analysis,
            handlesSelfReferences: analysis.hasSelfReferences && analysis.selfReferences.length === 2,
            handlesMutualReferences: analysis.hasMutualReferences
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: selfRefStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle self-references and mutual references
      expect(results[0].analysis.hasSelfReferences).toBe(true);
      expect(results[0].analysis.selfReferences).toHaveLength(2);
      expect(results[0].analysis.hasMutualReferences).toBe(true);
      expect(results[0].handlesSelfReferences).toBe(true);
      expect(results[0].handlesMutualReferences).toBe(true);
    });
  });

  describe('Unicode Normalization Issues', () => {
    it('should handle different Unicode normalization forms', async () => {
      const query = join(tempDir, 'unicode-normalization.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Create strings that look the same but have different Unicode representations
      const unicodeStore = new Store();
      const subject = namedNode('http://example.org/unicode-test');
      const prop = namedNode('http://example.org/name');
      
      // Same logical string in different normalization forms
      const nfc = 'café'; // NFC (Canonical Composition)
      const nfd = 'cafe\u0301'; // NFD (Canonical Decomposition) - e with combining acute accent
      const nfkc = 'café'; // NFKC (Compatibility Composition)
      const nfkd = 'cafe\u0301'; // NFKD (Compatibility Decomposition)
      
      unicodeStore.addQuad(quad(subject, prop, literal(nfc, 'en')));
      unicodeStore.addQuad(quad(subject, prop, literal(nfd, 'en')));
      unicodeStore.addQuad(quad(subject, prop, literal(nfkc, 'fr')));
      unicodeStore.addQuad(quad(subject, prop, literal(nfkd, 'fr')));
      
      // Add some other Unicode edge cases
      unicodeStore.addQuad(quad(
        subject, 
        namedNode('http://example.org/emoji'), 
        literal('👨‍👩‍👧‍👦') // Family emoji with ZWJ sequences
      ));
      
      unicodeStore.addQuad(quad(
        subject, 
        namedNode('http://example.org/rtl'), 
        literal('مرحبا بالعالم') // Arabic (RTL text)
      ));
      
      unicodeStore.addQuad(quad(
        subject, 
        namedNode('http://example.org/mixed'), 
        literal('Hello 世界 🌍') // Mixed scripts and emoji
      ));
      
      const hook = defineHook({
        meta: { name: 'unicode-normalization-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || unicodeStore;
          
          const analysis = {
            quadCount: graph.size,
            unicodeStrings: [],
            normalizationIssues: [],
            emojiHandling: false,
            rtlHandling: false,
            mixedScriptHandling: false
          };
          
          // Analyze Unicode strings
          const quads = graph.getQuads();
          for (const q of quads) {
            if (q.object.termType === 'Literal') {
              const value = q.object.value;
              const property = q.predicate.value.split('/').pop();
              
              analysis.unicodeStrings.push({
                property,
                value,
                length: value.length,
                byteLength: new TextEncoder().encode(value).length,
                codePointLength: Array.from(value).length,
                normalized: {
                  nfc: value.normalize('NFC'),
                  nfd: value.normalize('NFD'),
                  nfkc: value.normalize('NFKC'),
                  nfkd: value.normalize('NFKD')
                }
              });
              
              // Check for normalization issues
              const nfcForm = value.normalize('NFC');
              const nfdForm = value.normalize('NFD');
              
              if (value !== nfcForm || value !== nfdForm) {
                analysis.normalizationIssues.push({
                  property,
                  original: value,
                  nfc: nfcForm,
                  nfd: nfdForm,
                  different: value !== nfcForm
                });
              }
              
              // Check specific Unicode features
              if (property === 'emoji') {
                analysis.emojiHandling = value.includes('👨‍👩‍👧‍👦');
              } else if (property === 'rtl') {
                analysis.rtlHandling = /[\u0600-\u06FF]/.test(value); // Arabic range
              } else if (property === 'mixed') {
                analysis.mixedScriptHandling = /[^\x00-\x7F]/.test(value) && /[a-zA-Z]/.test(value);
              }
            }
          }
          
          // Test string equality with different normalizations
          const nameQuads = graph.getQuads(null, prop);
          const nameValues = nameQuads.map(q => q.object.value);
          
          const equalityTests = {
            rawEquality: nameValues[0] === nameValues[1],
            nfcEquality: nameValues[0].normalize('NFC') === nameValues[1].normalize('NFC'),
            nfdEquality: nameValues[0].normalize('NFD') === nameValues[1].normalize('NFD'),
            logicallyEqual: nameValues[0].normalize('NFC') === nameValues[1].normalize('NFC')
          };
          
          return { 
            success: true, 
            analysis,
            equalityTests,
            handlesUnicodeNormalization: !equalityTests.rawEquality && equalityTests.logicallyEqual,
            handlesComplexUnicode: analysis.emojiHandling && analysis.rtlHandling && analysis.mixedScriptHandling
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: unicodeStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle Unicode normalization correctly
      expect(results[0].analysis.quadCount).toBe(7);
      expect(results[0].analysis.normalizationIssues.length).toBeGreaterThan(0);
      expect(results[0].equalityTests.rawEquality).toBe(false);
      expect(results[0].equalityTests.logicallyEqual).toBe(true);
      expect(results[0].handlesUnicodeNormalization).toBe(true);
      expect(results[0].handlesComplexUnicode).toBe(true);
    });

    it('should handle Unicode edge cases in IRIs', async () => {
      const query = join(tempDir, 'unicode-iris.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const iriStore = new Store();
      
      // Create IRIs with Unicode characters (should be percent-encoded)
      const unicodeIRIs = [
        'http://example.org/café', // Non-ASCII in IRI
        'http://example.org/用户', // Chinese characters
        'http://example.org/مستخدم', // Arabic characters
        'http://example.org/🏠', // Emoji in IRI
        'http://example.org/spac e' // Space (should be encoded)
      ];
      
      const hook = defineHook({
        meta: { name: 'unicode-iris-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const analysis = {
            unicodeIRIs: [],
            validationResults: [],
            encodingTests: []
          };
          
          for (const iri of unicodeIRIs) {
            try {
              // Test IRI creation and validation
              const namedNodeResult = namedNode(iri);
              
              analysis.unicodeIRIs.push({
                original: iri,
                created: true,
                value: namedNodeResult.value,
                isValid: true
              });
              
              // Test encoding
              const encoded = encodeURI(iri);
              const decoded = decodeURI(encoded);
              
              analysis.encodingTests.push({
                original: iri,
                encoded,
                decoded,
                roundTrip: decoded === iri
              });
              
            } catch (error) {
              analysis.unicodeIRIs.push({
                original: iri,
                created: false,
                error: error.message,
                isValid: false
              });
              
              analysis.validationResults.push({
                iri,
                valid: false,
                error: error.message
              });
            }
          }
          
          const validIRIs = analysis.unicodeIRIs.filter(i => i.isValid);
          const invalidIRIs = analysis.unicodeIRIs.filter(i => !i.isValid);
          
          return { 
            success: true, 
            analysis,
            validIRICount: validIRIs.length,
            invalidIRICount: invalidIRIs.length,
            handlesUnicodeIRIs: validIRIs.length > 0,
            properEncoding: analysis.encodingTests.every(t => t.roundTrip)
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: iriStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle Unicode IRIs (most should be valid due to N3.js handling)
      expect(results[0].validIRICount).toBeGreaterThan(0);
      expect(results[0].handlesUnicodeIRIs).toBe(true);
    });
  });

  describe('Timezone Handling Problems', () => {
    it('should handle different timezone representations', async () => {
      const query = join(tempDir, 'timezone-handling.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const timezoneStore = new Store();
      const subject = namedNode('http://example.org/event');
      const timeProp = namedNode('http://example.org/timestamp');
      
      // Different timezone representations of the same moment
      const timestamps = [
        '2023-12-01T15:30:00Z', // UTC
        '2023-12-01T10:30:00-05:00', // EST
        '2023-12-01T16:30:00+01:00', // CET
        '2023-12-01T15:30:00+00:00', // UTC with explicit offset
        '2023-12-01T23:30:00+08:00', // CST (China)
        '2023-12-01T15:30:00' // No timezone (local)
      ];
      
      timestamps.forEach((timestamp, index) => {
        timezoneStore.addQuad(quad(
          namedNode(`http://example.org/event${index}`),
          timeProp,
          literal(timestamp, namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
        ));
      });
      
      const hook = defineHook({
        meta: { name: 'timezone-handling-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || timezoneStore;
          
          const analysis = {
            timestampCount: 0,
            timezoneFormats: [],
            utcConversions: [],
            comparisonResults: []
          };
          
          // Analyze timestamps
          const timeQuads = graph.getQuads(null, timeProp);
          analysis.timestampCount = timeQuads.length;
          
          const parsedTimes = [];
          
          for (const quad of timeQuads) {
            const timestamp = quad.object.value;
            
            try {
              const date = new Date(timestamp);
              const utcTime = date.getTime();
              
              analysis.timezoneFormats.push({
                original: timestamp,
                parsed: date.toISOString(),
                utcTimestamp: utcTime,
                hasTimezone: timestamp.includes('Z') || timestamp.includes('+') || timestamp.includes('-', 10),
                isValid: !isNaN(utcTime)
              });
              
              analysis.utcConversions.push({
                original: timestamp,
                utc: date.toISOString(),
                localString: date.toString(),
                utcTimestamp: utcTime
              });
              
              parsedTimes.push({ timestamp, utcTime, date });
              
            } catch (error) {
              analysis.timezoneFormats.push({
                original: timestamp,
                error: error.message,
                isValid: false
              });
            }
          }
          
          // Compare timestamps to see if they represent the same moment
          const baseTime = parsedTimes.length > 0 ? parsedTimes[0].utcTime : null;
          
          for (let i = 1; i < parsedTimes.length; i++) {
            const comparison = {
              timestamp1: parsedTimes[0].timestamp,
              timestamp2: parsedTimes[i].timestamp,
              utcTime1: parsedTimes[0].utcTime,
              utcTime2: parsedTimes[i].utcTime,
              sameInstant: Math.abs(parsedTimes[0].utcTime - parsedTimes[i].utcTime) < 1000, // Within 1 second
              timeDifference: parsedTimes[i].utcTime - parsedTimes[0].utcTime
            };
            
            analysis.comparisonResults.push(comparison);
          }
          
          const validTimestamps = analysis.timezoneFormats.filter(t => t.isValid);
          const sameInstantCount = analysis.comparisonResults.filter(c => c.sameInstant).length;
          
          return { 
            success: true, 
            analysis,
            validTimestampCount: validTimestamps.length,
            handlesTimezones: validTimestamps.some(t => t.hasTimezone),
            recognizesSameInstant: sameInstantCount >= 4 // Most should be the same instant
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: timezoneStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle timezone conversions correctly
      expect(results[0].analysis.timestampCount).toBe(6);
      expect(results[0].validTimestampCount).toBe(6);
      expect(results[0].handlesTimezones).toBe(true);
      expect(results[0].recognizesSameInstant).toBe(true);
    });

    it('should handle daylight saving time transitions', async () => {
      const query = join(tempDir, 'dst-transitions.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const dstStore = new Store();
      const subject = namedNode('http://example.org/dst-test');
      const timeProp = namedNode('http://example.org/time');
      
      // DST transition times (Spring forward and Fall back in US Eastern time)
      const dstTransitions = [
        '2023-03-12T06:59:59Z', // Just before spring forward (1:59:59 EST)
        '2023-03-12T07:00:00Z', // Just after spring forward (3:00:00 EDT)
        '2023-11-05T05:59:59Z', // Just before fall back (1:59:59 EDT)
        '2023-11-05T06:00:00Z', // Just after fall back (1:00:00 EST)
        '2023-11-05T06:59:59Z', // The "repeated" hour (1:59:59 EST)
      ];
      
      dstTransitions.forEach((timestamp, index) => {
        dstStore.addQuad(quad(
          namedNode(`http://example.org/dst-event${index}`),
          timeProp,
          literal(timestamp, namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
        ));
      });
      
      const hook = defineHook({
        meta: { name: 'dst-transitions-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || dstStore;
          
          const analysis = {
            dstTransitions: [],
            timeGaps: [],
            timeOverlaps: []
          };
          
          // Analyze DST transitions
          const timeQuads = graph.getQuads(null, timeProp);
          const sortedTimes = timeQuads
            .map(q => ({
              timestamp: q.object.value,
              date: new Date(q.object.value),
              utcTime: new Date(q.object.value).getTime()
            }))
            .sort((a, b) => a.utcTime - b.utcTime);
          
          // Detect time gaps and overlaps
          for (let i = 1; i < sortedTimes.length; i++) {
            const prev = sortedTimes[i - 1];
            const current = sortedTimes[i];
            const timeDiff = current.utcTime - prev.utcTime;
            
            analysis.dstTransitions.push({
              from: prev.timestamp,
              to: current.timestamp,
              utcDifference: timeDiff,
              hoursDifference: timeDiff / (1000 * 60 * 60),
              isSpringForward: timeDiff === 3600000, // 1 hour gap
              isFallBack: timeDiff === 0, // Same UTC time (overlap)
              isNormal: timeDiff === 3600000 // 1 hour normal progression
            });
            
            // Detect gaps (spring forward)
            if (timeDiff > 3600000) {
              analysis.timeGaps.push({
                before: prev.timestamp,
                after: current.timestamp,
                gapDuration: timeDiff - 3600000
              });
            }
            
            // Detect overlaps (fall back)
            if (timeDiff < 3600000 && timeDiff > 0) {
              analysis.timeOverlaps.push({
                start: prev.timestamp,
                end: current.timestamp,
                overlapDuration: 3600000 - timeDiff
              });
            }
          }
          
          // Test DST-aware operations
          const dstTests = {
            canDetectSpringForward: analysis.timeGaps.length > 0,
            canDetectFallBack: analysis.timeOverlaps.length > 0,
            handlesAmbiguousTime: analysis.dstTransitions.some(t => t.isFallBack),
            maintainsUtcConsistency: sortedTimes.every((time, index) => 
              index === 0 || time.utcTime >= sortedTimes[index - 1].utcTime
            )
          };
          
          return { 
            success: true, 
            analysis,
            dstTests,
            handlesDSTTransitions: dstTests.canDetectSpringForward || dstTests.canDetectFallBack
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: dstStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle DST transitions
      expect(results[0].analysis.dstTransitions.length).toBe(4);
      expect(results[0].dstTests.maintainsUtcConsistency).toBe(true);
      // Note: Actual DST detection depends on the system's timezone handling
    });
  });

  describe('Floating-Point Precision Issues', () => {
    it('should handle floating-point precision in numeric literals', async () => {
      const query = join(tempDir, 'float-precision.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const floatStore = new Store();
      const subject = namedNode('http://example.org/precision-test');
      
      // Floating-point numbers that demonstrate precision issues
      const precisionTests = [
        { name: 'simple-decimal', value: '0.1', type: 'decimal' },
        { name: 'repeating-decimal', value: '0.333333333333333', type: 'decimal' },
        { name: 'large-float', value: '1.7976931348623157e+308', type: 'double' },
        { name: 'small-float', value: '2.2250738585072014e-308', type: 'double' },
        { name: 'pi', value: '3.141592653589793', type: 'double' },
        { name: 'euler', value: '2.718281828459045', type: 'double' },
        { name: 'problematic-sum', value: '0.30000000000000004', type: 'double' }, // 0.1 + 0.2
        { name: 'zero-positive', value: '0.0', type: 'double' },
        { name: 'zero-negative', value: '-0.0', type: 'double' },
        { name: 'infinity', value: 'INF', type: 'double' },
        { name: 'negative-infinity', value: '-INF', type: 'double' },
        { name: 'not-a-number', value: 'NaN', type: 'double' }
      ];
      
      precisionTests.forEach(test => {
        const datatype = test.type === 'decimal' ? 
          'http://www.w3.org/2001/XMLSchema#decimal' :
          'http://www.w3.org/2001/XMLSchema#double';
          
        floatStore.addQuad(quad(
          subject,
          namedNode(`http://example.org/${test.name}`),
          literal(test.value, namedNode(datatype))
        ));
      });
      
      const hook = defineHook({
        meta: { name: 'float-precision-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const graph = event.context?.graph || floatStore;
          
          const analysis = {
            numericLiterals: [],
            precisionTests: [],
            specialValues: []
          };
          
          // Analyze numeric literals
          const quads = graph.getQuads();
          for (const q of quads) {
            if (q.object.termType === 'Literal') {
              const value = q.object.value;
              const datatype = q.object.datatype?.value;
              const property = q.predicate.value.split('/').pop();
              
              if (datatype?.includes('double') || datatype?.includes('decimal') || datatype?.includes('float')) {
                const numericAnalysis = {
                  property,
                  stringValue: value,
                  datatype: datatype.split('#')[1],
                  parsedValue: null,
                  isFinite: false,
                  isNaN: false,
                  isInfinity: false,
                  precisionLoss: false
                };
                
                try {
                  if (value === 'INF' || value === '-INF') {
                    numericAnalysis.parsedValue = value === 'INF' ? Infinity : -Infinity;
                    numericAnalysis.isInfinity = true;
                  } else if (value === 'NaN') {
                    numericAnalysis.parsedValue = NaN;
                    numericAnalysis.isNaN = true;
                  } else {
                    numericAnalysis.parsedValue = parseFloat(value);
                    numericAnalysis.isFinite = isFinite(numericAnalysis.parsedValue);
                    numericAnalysis.isNaN = isNaN(numericAnalysis.parsedValue);
                  }
                  
                  // Test precision loss
                  if (numericAnalysis.isFinite && !numericAnalysis.isNaN) {
                    const roundTrip = numericAnalysis.parsedValue.toString();
                    numericAnalysis.precisionLoss = roundTrip !== value && 
                      !roundTrip.startsWith(value) && 
                      !value.startsWith(roundTrip);
                  }
                  
                } catch (error) {
                  numericAnalysis.error = error.message;
                }
                
                analysis.numericLiterals.push(numericAnalysis);
                
                // Categorize special values
                if (numericAnalysis.isInfinity || numericAnalysis.isNaN) {
                  analysis.specialValues.push({
                    property,
                    value,
                    type: numericAnalysis.isInfinity ? 'infinity' : 'nan'
                  });
                }
              }
            }
          }
          
          // Test arithmetic precision
          const arithmeticTests = [
            {
              name: 'decimal-addition',
              operation: () => 0.1 + 0.2,
              expected: 0.3,
              tolerance: 1e-15
            },
            {
              name: 'large-number-precision',
              operation: () => 9007199254740992 + 1, // 2^53 + 1
              expected: 9007199254740993,
              tolerance: 0
            },
            {
              name: 'division-precision',
              operation: () => 1 / 3,
              expected: 0.3333333333333333,
              tolerance: 1e-15
            }
          ];
          
          for (const test of arithmeticTests) {
            try {
              const result = test.operation();
              const withinTolerance = Math.abs(result - test.expected) <= test.tolerance;
              
              analysis.precisionTests.push({
                name: test.name,
                result,
                expected: test.expected,
                difference: Math.abs(result - test.expected),
                withinTolerance,
                hasIssue: !withinTolerance && test.tolerance > 0
              });
            } catch (error) {
              analysis.precisionTests.push({
                name: test.name,
                error: error.message
              });
            }
          }
          
          const hasSpecialValues = analysis.specialValues.length > 0;
          const hasPrecisionIssues = analysis.precisionTests.some(t => t.hasIssue);
          
          return { 
            success: true, 
            analysis,
            numericLiteralCount: analysis.numericLiterals.length,
            specialValueCount: analysis.specialValues.length,
            handlesSpecialValues: hasSpecialValues,
            detectsPrecisionIssues: hasPrecisionIssues
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: floatStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle floating-point precision issues
      expect(results[0].numericLiteralCount).toBe(12);
      expect(results[0].specialValueCount).toBe(3); // INF, -INF, NaN
      expect(results[0].handlesSpecialValues).toBe(true);
      expect(results[0].analysis.precisionTests.some(t => t.name === 'decimal-addition')).toBe(true);
    });
  });
});
