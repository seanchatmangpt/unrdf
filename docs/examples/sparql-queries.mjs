#!/usr/bin/env node

/**
 * Example: SPARQL Queries
 * 
 * This example demonstrates comprehensive SPARQL query usage including:
 * - SELECT queries
 * - ASK queries
 * - CONSTRUCT queries
 * - UPDATE queries
 * - Query options and error handling
 */

import { useStore, useGraph } from 'unrdf';

async function main() {
  try {
    console.log('🚀 Starting SPARQL Queries example...\n');

    // 1. Create a store with sample data
    console.log('1. Creating store with sample data...');
    const store = useStore();
    
    // Create sample data about people and their relationships
    const alice = store.namedNode('http://example.org/alice');
    const bob = store.namedNode('http://example.org/bob');
    const charlie = store.namedNode('http://example.org/charlie');
    
    const type = store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const person = store.namedNode('http://xmlns.com/foaf/0.1/Person');
    const name = store.namedNode('http://xmlns.com/foaf/0.1/name');
    const age = store.namedNode('http://example.org/age');
    const knows = store.namedNode('http://xmlns.com/foaf/0.1/knows');
    const worksFor = store.namedNode('http://example.org/worksFor');
    const company = store.namedNode('http://example.org/company');
    
    const acme = store.namedNode('http://example.org/acme');
    const techCorp = store.namedNode('http://example.org/techCorp');
    
    // Add data
    store.add(
      // Alice
      store.quad(alice, type, person),
      store.quad(alice, name, store.literal('Alice')),
      store.quad(alice, age, store.literal('30', 'http://www.w3.org/2001/XMLSchema#integer')),
      store.quad(alice, knows, bob),
      store.quad(alice, worksFor, acme),
      
      // Bob
      store.quad(bob, type, person),
      store.quad(bob, name, store.literal('Bob')),
      store.quad(bob, age, store.literal('25', 'http://www.w3.org/2001/XMLSchema#integer')),
      store.quad(bob, knows, charlie),
      store.quad(bob, worksFor, techCorp),
      
      // Charlie
      store.quad(charlie, type, person),
      store.quad(charlie, name, store.literal('Charlie')),
      store.quad(charlie, age, store.literal('35', 'http://www.w3.org/2001/XMLSchema#integer')),
      store.quad(charlie, worksFor, acme),
      
      // Companies
      store.quad(acme, type, company),
      store.quad(techCorp, type, company)
    );
    
    console.log(`   ✅ Added ${store.size} quads to store`);

    // 2. SELECT queries
    console.log('\n2. SELECT queries...');
    const graph = useGraph(store);
    
    // Basic SELECT
    console.log('   📊 Basic SELECT query:');
    const basicResults = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?person ?name ?age WHERE {
        ?person a foaf:Person ;
                foaf:name ?name ;
                ex:age ?age .
      }
    `);
    
    basicResults.results.forEach(row => {
      console.log(`      - ${row.name} (age: ${row.age})`);
    });

    // SELECT with FILTER
    console.log('\n   📊 SELECT with FILTER (age > 25):');
    const filteredResults = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?person ?name ?age WHERE {
        ?person a foaf:Person ;
                foaf:name ?name ;
                ex:age ?age .
        FILTER(?age > 25)
      }
    `);
    
    filteredResults.results.forEach(row => {
      console.log(`      - ${row.name} (age: ${row.age})`);
    });

    // SELECT with OPTIONAL
    console.log('\n   📊 SELECT with OPTIONAL (who knows whom):');
    const optionalResults = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      
      SELECT ?person ?name ?knowsName WHERE {
        ?person a foaf:Person ;
                foaf:name ?name .
        OPTIONAL {
          ?person foaf:knows ?knows .
          ?knows foaf:name ?knowsName .
        }
      }
    `);
    
    optionalResults.results.forEach(row => {
      const knows = row.knowsName ? ` (knows ${row.knowsName})` : '';
      console.log(`      - ${row.name}${knows}`);
    });

    // SELECT with LIMIT and OFFSET
    console.log('\n   📊 SELECT with LIMIT and OFFSET:');
    const limitedResults = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?person ?name ?age WHERE {
        ?person a foaf:Person ;
                foaf:name ?name ;
                ex:age ?age .
      }
      ORDER BY ?age
      LIMIT 2
    `);
    
    limitedResults.results.forEach(row => {
      console.log(`      - ${row.name} (age: ${row.age})`);
    });

    // 3. ASK queries
    console.log('\n3. ASK queries...');
    
    // Basic ASK
    console.log('   ❓ ASK: Are there any people?');
    const hasPeople = await graph.ask(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      
      ASK WHERE {
        ?person a foaf:Person .
      }
    `);
    console.log(`      Answer: ${hasPeople}`);

    // ASK with conditions
    console.log('\n   ❓ ASK: Is there anyone over 30?');
    const hasOver30 = await graph.ask(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      ASK WHERE {
        ?person a foaf:Person ;
                ex:age ?age .
        FILTER(?age > 30)
      }
    `);
    console.log(`      Answer: ${hasOver30}`);

    // 4. CONSTRUCT queries
    console.log('\n4. CONSTRUCT queries...');
    
    // Basic CONSTRUCT
    console.log('   🔨 CONSTRUCT: Create a simplified view');
    const constructed = await graph.construct(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      CONSTRUCT {
        ?person ex:name ?name .
        ?person ex:age ?age .
      } WHERE {
        ?person a foaf:Person ;
                foaf:name ?name ;
                ex:age ?age .
      }
    `);
    
    console.log(`      Constructed ${constructed.quads.length} quads`);
    const constructedTurtle = constructed.store.serialize();
    console.log('      Constructed data:');
    console.log(constructedTurtle);

    // 5. UPDATE queries
    console.log('\n5. UPDATE queries...');
    
    // INSERT DATA
    console.log('   ➕ INSERT DATA: Adding new person');
    await graph.update(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      INSERT DATA {
        ex:david a foaf:Person .
        ex:david foaf:name "David" .
        ex:david ex:age 28 .
      }
    `);
    
    console.log(`      Store size after INSERT: ${store.size}`);

    // DELETE DATA
    console.log('\n   ➖ DELETE DATA: Removing age information');
    await graph.update(`
      PREFIX ex: <http://example.org/>
      
      DELETE DATA {
        ex:alice ex:age 30 .
      }
    `);
    
    console.log(`      Store size after DELETE: ${store.size}`);

    // 6. Query with options
    console.log('\n6. Query with options...');
    
    // Query with limit
    console.log('   ⚙️  Query with limit:');
    const limitedQuery = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      
      SELECT ?person ?name WHERE {
        ?person a foaf:Person ;
                foaf:name ?name .
      }
    `, { limit: 2 });
    
    console.log(`      Limited to ${limitedQuery.results.length} results`);

    // 7. Error handling
    console.log('\n7. Error handling...');
    
    try {
      await graph.select('INVALID SPARQL SYNTAX');
    } catch (error) {
      console.log('   ❌ Caught expected error:', error.message);
    }

    // 8. Complex queries
    console.log('\n8. Complex queries...');
    
    // Find people who work at the same company
    console.log('   🔍 Complex query: People who work at the same company');
    const complexResults = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?person1 ?name1 ?person2 ?name2 ?company WHERE {
        ?person1 a foaf:Person ;
                 foaf:name ?name1 ;
                 ex:worksFor ?company .
        ?person2 a foaf:Person ;
                 foaf:name ?name2 ;
                 ex:worksFor ?company .
        FILTER(?person1 != ?person2)
      }
    `);
    
    complexResults.results.forEach(row => {
      console.log(`      - ${row.name1} and ${row.name2} both work at ${row.company}`);
    });

    // 9. Query statistics
    console.log('\n9. Query statistics...');
    const stats = store.stats();
    console.log('   📈 Final store statistics:');
    console.log(`      - Quads: ${stats.quads}`);
    console.log(`      - Subjects: ${stats.subjects}`);
    console.log(`      - Predicates: ${stats.predicates}`);
    console.log(`      - Objects: ${stats.objects}`);
    console.log(`      - Graphs: ${stats.graphs}`);

    console.log('\n🎉 SPARQL Queries example completed successfully!');
    
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run the example
main();
