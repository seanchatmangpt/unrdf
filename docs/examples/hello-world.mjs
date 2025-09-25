#!/usr/bin/env node

/**
 * Example: Hello World
 * 
 * This example demonstrates basic unrdf usage including:
 * - Creating a store
 * - Adding data
 * - Querying with SPARQL
 * - Serializing to Turtle
 */

import { useStore, useGraph, useTurtle, useValidator, useReasoner, useCanon, useZod } from 'unrdf';

async function main() {
  try {
    console.log('🚀 Starting unrdf Hello World example...\n');

    // 1. Create a store and add some data
    console.log('1. Creating store and adding data...');
    const store = useStoreContext();
    
    // Create some sample data about Alice
    const alice = store.namedNode('http://example.org/alice');
    const type = store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const person = store.namedNode('http://xmlns.com/foaf/0.1/Person');
    const name = store.namedNode('http://xmlns.com/foaf/0.1/name');
    const age = store.namedNode('http://example.org/age');
    
    store.add(
      store.quad(alice, type, person),
      store.quad(alice, name, store.literal('Alice')),
      store.quad(alice, age, store.literal('30', 'http://www.w3.org/2001/XMLSchema#integer'))
    );
    
    console.log(`   ✅ Added ${store.size} quads to store`);

    // 2. Query the data with SPARQL
    console.log('\n2. Querying data with SPARQL...');
    const graph = useGraph(store);
    
    const results = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?person ?name ?age WHERE {
        ?person a foaf:Person ;
                foaf:name ?name ;
                ex:age ?age .
      }
    `);
    
    console.log('   📊 Query results:');
    for (const row of results.results) {
      console.log(`      - ${row.name} (age: ${row.age})`);
    }

    // 3. Serialize to Turtle
    console.log('\n3. Serializing to Turtle...');
    const turtle = store.serialize();
    console.log('   📝 Turtle output:');
    console.log(turtle);

    // 4. Validate with SHACL (if shapes are available)
    console.log('\n4. SHACL validation...');
    try {
      const validator = useValidator();
      
      // Create simple shapes
      const shapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix ex: <http://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass foaf:Person ;
          sh:property [
            sh:path foaf:name ;
            sh:datatype xsd:string ;
            sh:minCount 1 ;
          ] .
      `;
      
      const report = await validator.validate(store, shapes);
      
      if (report.conforms) {
        console.log('   ✅ SHACL validation passed!');
      } else {
        console.log('   ❌ SHACL validation failed:');
        for (const result of report.results) {
          console.log(`      - ${result.message}`);
        }
      }
    } catch (error) {
      console.log('   ⚠️  SHACL validation skipped:', error.message);
    }

    // 5. N3 Reasoning (if rules are available)
    console.log('\n5. N3 reasoning...');
    try {
      const reasoner = useReasoner();
      
      // Create simple rules
      const rules = `
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix ex: <http://example.org/> .
        
        { ?person a foaf:Person } => { ?person ex:isPerson true } .
      `;
      
      const inferred = await reasoner.reason(store, rules);
      const newTriples = reasoner.getNewTriples(store, inferred);
      
      console.log(`   🧠 Reasoning completed: ${newTriples.size} new triples inferred`);
      
      if (newTriples.size > 0) {
        console.log('   📝 Inferred triples:');
        const inferredTurtle = newTriples.serialize();
        console.log(inferredTurtle);
      }
    } catch (error) {
      console.log('   ⚠️  N3 reasoning skipped:', error.message);
    }

    // 6. Canonicalization
    console.log('\n6. Canonicalization...');
    try {
      const canon = useCanon();
      const canonical = await canon.canonicalize(store);
      
      console.log('   🔄 Canonicalization completed');
      console.log('   📝 Canonical N-Quads:');
      console.log(canonical);
    } catch (error) {
      console.log('   ⚠️  Canonicalization skipped:', error.message);
    }

    // 7. Zod validation
    console.log('\n7. Zod validation...');
    try {
      const zod = useZod();
      
      // Define schema for our query results
      const personSchema = zod.z.object({
        person: zod.z.string(),
        name: zod.z.string(),
        age: zod.z.string()
      });
      
      const validation = zod.validateResults(results.results, personSchema);
      
      if (validation.validated) {
        console.log('   ✅ Zod validation passed!');
        console.log('   📊 Validated data:');
        for (const person of validation.data) {
          console.log(`      - ${person.name} (${person.age})`);
        }
      } else {
        console.log('   ❌ Zod validation failed:');
        for (const error of validation.errors) {
          console.log(`      - ${error.message}`);
        }
      }
    } catch (error) {
      console.log('   ⚠️  Zod validation skipped:', error.message);
    }

    // 8. Store statistics
    console.log('\n8. Store statistics...');
    const stats = store.stats();
    console.log('   📈 Store statistics:');
    console.log(`      - Quads: ${stats.quads}`);
    console.log(`      - Subjects: ${stats.subjects}`);
    console.log(`      - Predicates: ${stats.predicates}`);
    console.log(`      - Objects: ${stats.objects}`);
    console.log(`      - Graphs: ${stats.graphs}`);

    console.log('\n🎉 Hello World example completed successfully!');
    
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run the example
main();
