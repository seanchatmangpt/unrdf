import { KnowledgeHookManager } from '../src/hooks/knowledge-hook-manager.mjs';
import { executeSemanticQuery, createSemanticCondition } from '@unrdf/core/utils/semantic-bridge';
import path from 'path';

// Initialize the UNRDF Knowledge Hook Manager
const manager = new KnowledgeHookManager();

// Define a semantic condition: 
// Trigger ONLY IF the Open Ontologies reasoner/query engine confirms the semantic state.
// In this PoC, we query the pizza.ttl ontology we created earlier.
const sparqlQuery = `
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX ex: <http://example.org/pizza#>
  SELECT ?subclass WHERE {
    ?subclass rdfs:subClassOf ex:Pizza .
    ?subclass rdfs:label "Margherita" .
  }
`;

const ontologyFiles = [
  path.join(process.cwd(), 'pizza.ttl') // Path to the generated pizza.ttl
];

const semanticCondition = createSemanticCondition(sparqlQuery, ontologyFiles);

// Register the Autonomous Hook
manager.registerHook({
  id: 'semantic-pizza-detector',
  name: 'Semantic Pizza Detector',
  version: '[VERSION]',
  trigger: 'after-add',
  enabled: true,
  description: 'Triggers autonomically when the Open Ontologies graph proves Margherita exists',
  validate: async (context) => {
    console.log('🧠 Evaluating Semantic Condition via Open Ontologies...');
    const result = await semanticCondition(context);

    if (result) {
      console.log('🚨 SEMANTIC INFERENCE TRIGGERED: The graph formally proved the existence of Margherita.');
      console.log('🤖 Executing autonomic remediation/action based on logic, not hardcoded metrics!');
      return true;
    } else {
      console.log('No semantic conditions met.');
      return true; // Return true to allow the quad through, but log that it wasn't our condition
    }
  }
});

// Simulate the system triggering an event (like a data ingest)
async function runDemo() {
  console.log('--- Vision 2030: Semantic Knowledge Hooks Demo ---');
  console.log('Triggering data:ingest event...\n');
  
  const mockQuad = {
    subject: { termType: 'NamedNode', value: 'http://example.org/subject', equals: () => false },
    predicate: { termType: 'NamedNode', value: 'http://example.org/predicate', equals: () => false },
    object: { termType: 'Literal', value: 'event-triggered', equals: () => false }
  };
  
  const results = await manager.executeByTrigger('after-add', mockQuad);
  
  console.log('\n--- Hook Execution Results ---');
  console.log(JSON.stringify(results, null, 2));
}

runDemo().catch(console.error);