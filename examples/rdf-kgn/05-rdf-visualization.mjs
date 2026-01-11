/**
 * @file RDF Graph Visualization Demo
 * @module examples/rdf-kgn/05-rdf-visualization
 * @description Generate RDF graph visualization formats (DOT, Mermaid, JSON)
 *
 * Time Estimate: 15-20 minutes
 * Difficulty: Intermediate
 * Prerequisites: Basic graph theory and visualization concepts
 */

import { RdfTemplateEngine } from '@unrdf/kgn/rdf';
import { createStore, executeQuery, namedNode, literal, COMMON_PREFIXES } from '@unrdf/core';

/**
 * GraphViz DOT format template
 */
const DOT_TEMPLATE = `
digraph RDFGraph {
  rankdir={{ direction | default('LR') }};
  node [shape={{ nodeShape | default('box') }}, style=filled, fillcolor=lightblue];
  edge [color=gray];

  {% for node in nodes %}
  "{{ node.id }}" [label="{{ node.label }}", fillcolor={{ node.color | default('lightblue') }}];
  {% endfor %}

  {% for edge in edges %}
  "{{ edge.from }}" -> "{{ edge.to }}" [label="{{ edge.label }}"];
  {% endfor %}
}
`.trim();

/**
 * Mermaid diagram template
 */
const MERMAID_TEMPLATE = `
graph {{ direction | default('LR') }}
{% for node in nodes %}
  {{ node.id }}["{{ node.label }}"]
{% endfor %}

{% for edge in edges %}
  {{ edge.from }} -->|{{ edge.label }}| {{ edge.to }}
{% endfor %}
`.trim();

/**
 * Create sample knowledge graph
 * @param {Object} store - RDF store
 * @returns {Promise<void>}
 */
async function createKnowledgeGraph(store) {
  const ex = (name) => namedNode(`http://example.org/${name}`);
  const schema = (name) => namedNode(`http://schema.org/${name}`);
  const knows = namedNode('http://xmlns.com/foaf/0.1/knows');

  // Create organization
  store.add(ex('acme-corp'), schema('name'), literal('Acme Corporation'));
  store.add(ex('acme-corp'), schema('url'), literal('https://acme.example.com'));

  // Create people
  const people = [
    { id: 'alice', name: 'Alice Johnson', role: 'CEO', dept: 'executive' },
    { id: 'bob', name: 'Bob Smith', role: 'CTO', dept: 'engineering' },
    { id: 'charlie', name: 'Charlie Brown', role: 'Developer', dept: 'engineering' },
    { id: 'diana', name: 'Diana Prince', role: 'Designer', dept: 'design' },
  ];

  for (const person of people) {
    store.add(ex(person.id), schema('name'), literal(person.name));
    store.add(ex(person.id), schema('jobTitle'), literal(person.role));
    store.add(ex(person.id), schema('worksFor'), ex('acme-corp'));
    store.add(ex(person.id), ex('department'), literal(person.dept));
  }

  // Create relationships
  store.add(ex('alice'), knows, ex('bob'));
  store.add(ex('bob'), knows, ex('charlie'));
  store.add(ex('bob'), knows, ex('diana'));
  store.add(ex('charlie'), knows, ex('diana'));
  store.add(ex('alice'), ex('manages'), ex('bob'));
  store.add(ex('bob'), ex('manages'), ex('charlie'));

  // Create projects
  store.add(ex('project-alpha'), schema('name'), literal('Project Alpha'));
  store.add(ex('project-beta'), schema('name'), literal('Project Beta'));
  store.add(ex('charlie'), ex('worksOn'), ex('project-alpha'));
  store.add(ex('diana'), ex('worksOn'), ex('project-alpha'));
  store.add(ex('charlie'), ex('worksOn'), ex('project-beta'));
}

/**
 * Extract graph structure from RDF store
 * @param {Object} store - RDF store
 * @returns {Promise<Object>} Graph structure
 */
async function extractGraphStructure(store) {
  // Query for all subjects
  const nodeQuery = `
    PREFIX schema: <http://schema.org/>
    PREFIX ex: <http://example.org/>

    SELECT DISTINCT ?node ?name ?type
    WHERE {
      ?node ?p ?o .
      OPTIONAL { ?node schema:name ?name }
      OPTIONAL { ?node schema:jobTitle ?type }
    }
  `;

  const nodeResults = await executeQuery(store, nodeQuery);
  const nodes = nodeResults.map(binding => {
    const nodeUri = binding.node.value;
    const nodeId = nodeUri.split('/').pop();
    const label = binding.name ? binding.name.value : nodeId;
    const type = binding.type ? binding.type.value : '';

    return {
      id: nodeId,
      label: type ? `${label}\\n(${type})` : label,
      uri: nodeUri,
      color: type ? 'lightgreen' : 'lightblue',
    };
  });

  // Query for all relationships
  const edgeQuery = `
    PREFIX schema: <http://schema.org/>
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?from ?to ?predicate
    WHERE {
      ?from ?predicate ?to .
      FILTER(isIRI(?to))
      FILTER(?predicate NOT IN (schema:name, schema:jobTitle, schema:url, ex:department))
    }
  `;

  const edgeResults = await executeQuery(store, edgeQuery);
  const edges = edgeResults.map(binding => {
    const fromUri = binding.from.value;
    const toUri = binding.to.value;
    const predicate = binding.predicate.value;

    const fromId = fromUri.split('/').pop();
    const toId = toUri.split('/').pop();
    const label = predicate.split('/').pop();

    return {
      from: fromId,
      to: toId,
      label: label,
    };
  });

  return { nodes, edges };
}

/**
 * Generate visualization formats
 * @returns {Promise<void>}
 */
async function visualizationDemo() {
  console.log('=== RDF Graph Visualization Demo ===\n');

  // Create template engine
  const engine = new RdfTemplateEngine({
    prefixes: COMMON_PREFIXES,
  });

  // Create and populate knowledge graph
  const store = createStore();
  await createKnowledgeGraph(store);
  console.log('✓ Knowledge graph created\n');

  // Extract graph structure
  const graphData = await extractGraphStructure(store);
  console.log(`✓ Extracted ${graphData.nodes.length} nodes and ${graphData.edges.length} edges\n`);

  // Generate DOT format (GraphViz)
  console.log('--- GraphViz DOT Format ---');
  const dotGraph = engine.render(DOT_TEMPLATE, {
    direction: 'TB',
    nodeShape: 'box',
    nodes: graphData.nodes,
    edges: graphData.edges,
  });

  console.log(dotGraph);
  console.log('\nTo visualize: Save as graph.dot and run "dot -Tpng graph.dot -o graph.png"\n');

  // Generate Mermaid format
  console.log('--- Mermaid Diagram Format ---');
  const mermaidGraph = engine.render(MERMAID_TEMPLATE, {
    direction: 'TD',
    nodes: graphData.nodes,
    edges: graphData.edges,
  });

  console.log(mermaidGraph);
  console.log('\nTo visualize: Paste into Mermaid Live Editor (https://mermaid.live)\n');

  // Generate JSON format for D3.js or other libraries
  console.log('--- JSON Format (for D3.js, Cytoscape.js, etc.) ---');
  const jsonGraph = {
    nodes: graphData.nodes.map(n => ({
      id: n.id,
      label: n.label.replace('\\n', ' '),
      uri: n.uri,
    })),
    edges: graphData.edges.map(e => ({
      source: e.from,
      target: e.to,
      label: e.label,
    })),
  };

  console.log(JSON.stringify(jsonGraph, null, 2));

  // Generate ASCII art visualization
  console.log('\n--- ASCII Graph Visualization ---');
  console.log('Organization Structure:');
  console.log('┌─────────────────────┐');
  console.log('│  Acme Corporation   │');
  console.log('└──────────┬──────────┘');
  console.log('           │');
  console.log('     ┌─────┴─────────────────┬─────────┐');
  console.log('     │                       │         │');
  console.log('┌────▼─────┐          ┌──────▼──┐  ┌──▼──────┐');
  console.log('│  Alice   │          │   Bob   │  │  Diana  │');
  console.log('│  (CEO)   │◄─manages─┤  (CTO)  │  │(Designer)│');
  console.log('└──────────┘          └────┬────┘  └────┬────┘');
  console.log('                           │            │');
  console.log('                      manages           │');
  console.log('                           │            │');
  console.log('                      ┌────▼────┐       │');
  console.log('                      │ Charlie │       │');
  console.log('                      │  (Dev)  │◄──────┘');
  console.log('                      └─────────┘  knows');

  console.log('\n--- Graph Statistics ---');
  console.log(`Total Nodes: ${graphData.nodes.length}`);
  console.log(`Total Edges: ${graphData.edges.length}`);
  console.log(`Graph Density: ${(graphData.edges.length / (graphData.nodes.length * (graphData.nodes.length - 1))).toFixed(2)}`);

  console.log('\n✓ Generated visualizations in multiple formats');
}

// Execute demo
try {
  await visualizationDemo();
  console.log('\n✓ Example completed successfully');
  process.exit(0);
} catch (error) {
  console.error('\n✗ Example failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
