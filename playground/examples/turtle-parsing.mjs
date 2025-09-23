#!/usr/bin/env node

/**
 * Turtle Parsing Example
 * 
 * This example demonstrates Turtle RDF format handling:
 * - Parsing Turtle strings
 * - Serializing to Turtle
 * - Handling different Turtle syntax features
 * - Working with prefixes and namespaces
 */

import { useStore, useGraph, useTurtle } from 'unrdf';

console.log('🐢 UNRDF Turtle Parsing Example\n');

async function main() {
  try {
    // Initialize components
    const store = useStore();
    const graph = useGraph(store);
    const turtle = useTurtle();

    // Example 1: Basic Turtle with prefixes
    console.log('📝 Example 1: Basic Turtle with prefixes');
    const basicTurtle = `
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

    const basicQuads = await turtle.parse(basicTurtle);
    await graph.addQuads(basicQuads);
    console.log(`✅ Parsed ${basicQuads.length} triples from basic Turtle`);

    // Example 2: Turtle with collections and blank nodes
    console.log('\n📝 Example 2: Collections and blank nodes');
    const collectionTurtle = `
      @prefix ex: <http://example.org/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      
      ex:team a ex:Team ;
        ex:name "Development Team" ;
        ex:members ( ex:alice ex:bob ex:charlie ) .
      
      ex:alice a foaf:Person ;
        foaf:name "Alice" .
      
      ex:bob a foaf:Person ;
        foaf:name "Bob" .
      
      ex:charlie a foaf:Person ;
        foaf:name "Charlie" .
    `;

    const collectionQuads = await turtle.parse(collectionTurtle);
    await graph.addQuads(collectionQuads);
    console.log(`✅ Parsed ${collectionQuads.length} triples from collection Turtle`);

    // Example 3: Turtle with datatypes and language tags
    console.log('\n📝 Example 3: Datatypes and language tags');
    const datatypeTurtle = `
      @prefix ex: <http://example.org/> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
      
      ex:product1 a ex:Product ;
        ex:name "Laptop"@en ;
        ex:name "Ordinateur portable"@fr ;
        ex:price "999.99"^^xsd:decimal ;
        ex:weight "2.5"^^xsd:float ;
        ex:inStock "true"^^xsd:boolean ;
        ex:releaseDate "2023-01-15"^^xsd:date .
    `;

    const datatypeQuads = await turtle.parse(datatypeTurtle);
    await graph.addQuads(datatypeQuads);
    console.log(`✅ Parsed ${datatypeQuads.length} triples from datatype Turtle`);

    // Example 4: Turtle with nested blank nodes
    console.log('\n📝 Example 4: Nested blank nodes');
    const nestedTurtle = `
      @prefix ex: <http://example.org/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      
      ex:company a ex:Company ;
        ex:name "Tech Corp" ;
        ex:address [
          ex:street "123 Main St" ;
          ex:city "San Francisco" ;
          ex:state "CA" ;
          ex:zipCode "94105"
        ] ;
        ex:ceo [
          a foaf:Person ;
          foaf:name "CEO Name" ;
          foaf:email "ceo@techcorp.com"
        ] .
    `;

    const nestedQuads = await turtle.parse(nestedTurtle);
    await graph.addQuads(nestedQuads);
    console.log(`✅ Parsed ${nestedQuads.length} triples from nested blank node Turtle`);

    // Serialize back to Turtle
    console.log('\n🔄 Serializing graph back to Turtle...');
    const allQuads = graph.getQuads();
    const serializedTurtle = await turtle.serialize(allQuads);
    
    console.log('\n📄 Serialized Turtle Output:');
    console.log(serializedTurtle);

    // Demonstrate different serialization options
    console.log('\n⚙️  Serialization Options:');

    // Serialize with custom prefixes
    const customPrefixes = {
      'foaf': 'http://xmlns.com/foaf/0.1/',
      'ex': 'http://example.org/',
      'xsd': 'http://www.w3.org/2001/XMLSchema#'
    };

    const customSerialized = await turtle.serialize(allQuads, { prefixes: customPrefixes });
    console.log('\n📄 Custom Prefix Serialization:');
    console.log(customSerialized);

    // Query for specific patterns
    console.log('\n🔍 Querying parsed data:');

    // Query for all persons
    const personQuery = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?person ?name ?age WHERE {
        ?person a foaf:Person ;
          foaf:name ?name .
        OPTIONAL { ?person foaf:age ?age }
      }
    `;

    const personResults = await graph.query(personQuery);
    console.log('\n👥 All Persons:');
    for await (const binding of personResults) {
      const age = binding.get('age') ? ` (age: ${binding.get('age').value})` : '';
      console.log(`  - ${binding.get('name').value}${age}`);
    }

    // Query for products with their properties
    const productQuery = `
      PREFIX ex: <http://example.org/>
      
      SELECT ?product ?name ?price ?inStock WHERE {
        ?product a ex:Product ;
          ex:name ?name .
        OPTIONAL { ?product ex:price ?price }
        OPTIONAL { ?product ex:inStock ?inStock }
      }
    `;

    const productResults = await graph.query(productQuery);
    console.log('\n🛍️  All Products:');
    for await (const binding of productResults) {
      const price = binding.get('price') ? ` - $${binding.get('price').value}` : '';
      const stock = binding.get('inStock') ? ` (in stock: ${binding.get('inStock').value})` : '';
      console.log(`  - ${binding.get('name').value}${price}${stock}`);
    }

    // Demonstrate error handling
    console.log('\n⚠️  Error Handling Example:');
    try {
      const invalidTurtle = `
        @prefix ex: <http://example.org/> .
        
        ex:invalid [ a ex:Person ;  # Missing closing bracket
      `;
      
      await turtle.parse(invalidTurtle);
    } catch (error) {
      console.log(`  ✅ Caught parsing error: ${error.message}`);
    }

    // Show statistics
    console.log(`\n📊 Parsing Statistics:`);
    console.log(`  - Total triples parsed: ${allQuads.length}`);
    console.log(`  - Unique subjects: ${new Set(allQuads.map(q => q.subject.value)).size}`);
    console.log(`  - Unique predicates: ${new Set(allQuads.map(q => q.predicate.value)).size}`);
    console.log(`  - Unique objects: ${new Set(allQuads.map(q => q.object.value)).size}`);

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
  }
}

main();
