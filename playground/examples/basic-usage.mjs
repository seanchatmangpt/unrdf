#!/usr/bin/env node

/**
 * Basic UNRDF Usage Example
 * 
 * This example demonstrates the core functionality of UNRDF:
 * - Creating a knowledge graph
 * - Adding triples
 * - Querying with SPARQL
 * - Working with different RDF formats
 */

import { useStore, useGraph, useTurtle, useZod } from 'unrdf';

console.log('🚀 UNRDF Basic Usage Example\n');

async function main() {
  try {
    // Initialize the RDF store
    const store = useStore();
    console.log('✅ RDF Store initialized');

    // Create a simple knowledge graph
    const graph = useGraph(store);
    console.log('✅ Knowledge graph created');

    // Add some sample triples about a person
    const personData = `
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      @prefix ex: <http://example.org/> .
      
      ex:john a foaf:Person ;
        foaf:name "John Doe" ;
        foaf:age 30 ;
        foaf:knows ex:jane .
      
      ex:jane a foaf:Person ;
        foaf:name "Jane Smith" ;
        foaf:age 28 .
    `;

    // Parse and add Turtle data
    const turtle = useTurtle();
    const quads = await turtle.parse(personData);
    await graph.addQuads(quads);
    console.log('✅ Sample data added to graph');

    // Query the data with SPARQL
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?name ?age WHERE {
        ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age .
      }
    `;

    const results = await graph.query(query);
    console.log('\n📊 Query Results:');
    for await (const binding of results) {
      console.log(`  - ${binding.get('name').value} (age: ${binding.get('age').value})`);
    }

    // Serialize back to Turtle
    const serialized = await turtle.serialize(graph.getQuads());
    console.log('\n📝 Serialized Turtle:');
    console.log(serialized);

    // Demonstrate Zod integration for validation
    const PersonSchema = useZod().createSchema({
      name: 'string',
      age: 'number',
      knows: 'array'
    });

    console.log('\n🔍 Zod Schema created for validation');
    console.log('Schema structure:', PersonSchema.shape);

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
  }
}

main();

