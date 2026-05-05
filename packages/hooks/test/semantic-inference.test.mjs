/**
 * @fileoverview End-to-end tests for Semantic Inference Hooks using Open Ontologies
 */

import { describe, it, expect } from 'vitest';
import { namedNode, literal, quad } from '../../test-utils/src/index.mjs';
import { KnowledgeHookManager } from '../src/index.mjs';
import path from 'path';
import { SEMANTIC_INFERENCE } from '../src/define.mjs';

describe('Semantic Inference Hooks (Vision 2030)', () => {
  it('should evaluate a condition using Open Ontologies inference', async () => {
    const manager = new KnowledgeHookManager();
    
    // Register the Semantic Pizza Hook
    const hook = {
      id: 'test-semantic-hook',
      name: 'Test Semantic Hook',
      version: '[VERSION]',
      trigger: 'after-add',
      enabled: true,
      description: 'Test autonomic trigger based on inference',
      condition: {
        kind: SEMANTIC_INFERENCE,
        query: `
          PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          PREFIX ex: <http://example.org/pizza#>
          SELECT ?subclass WHERE {
            ?subclass rdfs:subClassOf ex:Pizza .
            ?subclass rdfs:label "Margherita" .
          }
        `,
        ontologyFiles: [path.join(process.cwd(), '../../pizza.ttl')]
      },
      effect: {
        kind: 'function',
        inline: async (context) => {
          return { status: 'success', semanticHit: true };
        }
      }
    };

    manager.registerHook(hook);

    // Mock a trigger event.
    // The graph state won't matter as much here since Open Ontologies 
    // will reason over the provided ontology files AND the graph.
    const testQuad = quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('value')
    );

    const result = await manager.executeByTrigger('after-add', testQuad);
    
    expect(result.valid).toBe(true);
    expect(result.results).toBeDefined();
    
    console.log(JSON.stringify(result.results, null, 2));
    
    const semanticHit = result.results.find(r => r.hookName === 'Test Semantic Hook');
    expect(semanticHit).toBeDefined();
    expect(semanticHit.valid).toBe(true);
  });
});
