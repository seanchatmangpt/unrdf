#!/usr/bin/env node

/**
 * SPARQL Queries Example
 * 
 * This example demonstrates various SPARQL query patterns:
 * - SELECT queries
 * - CONSTRUCT queries
 * - ASK queries
 * - DESCRIBE queries
 */

import { useStore, useGraph, useTurtle } from 'unrdf';

console.log('🔍 UNRDF SPARQL Queries Example\n');

async function main() {
  try {
    // Initialize components
    const store = useStore();
    const graph = useGraph(store);
    const turtle = useTurtle();

    // Load sample data about books and authors
    const bookData = `
      @prefix ex: <http://example.org/> .
      @prefix dc: <http://purl.org/dc/elements/1.1/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      
      ex:book1 a ex:Book ;
        dc:title "The Great Gatsby" ;
        dc:creator ex:author1 ;
        ex:publishedYear 1925 .
      
      ex:book2 a ex:Book ;
        dc:title "To Kill a Mockingbird" ;
        dc:creator ex:author2 ;
        ex:publishedYear 1960 .
      
      ex:book3 a ex:Book ;
        dc:title "1984" ;
        dc:creator ex:author3 ;
        ex:publishedYear 1949 .
      
      ex:author1 a foaf:Person ;
        foaf:name "F. Scott Fitzgerald" .
      
      ex:author2 a foaf:Person ;
        foaf:name "Harper Lee" .
      
      ex:author3 a foaf:Person ;
        foaf:name "George Orwell" .
    `;

    const quads = await turtle.parse(bookData);
    await graph.addQuads(quads);
    console.log('✅ Sample book data loaded');

    // 1. SELECT Query - Get all books and their authors
    console.log('\n📚 SELECT Query - Books and Authors:');
    const selectQuery = `
      PREFIX ex: <http://example.org/>
      PREFIX dc: <http://purl.org/dc/elements/1.1/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      
      SELECT ?title ?authorName WHERE {
        ?book a ex:Book ;
          dc:title ?title ;
          dc:creator ?author .
        ?author foaf:name ?authorName .
      }
    `;

    const selectResults = await graph.query(selectQuery);
    for await (const binding of selectResults) {
      console.log(`  - "${binding.get('title').value}" by ${binding.get('authorName').value}`);
    }

    // 2. CONSTRUCT Query - Create a new graph structure
    console.log('\n🏗️  CONSTRUCT Query - Book Summary:');
    const constructQuery = `
      PREFIX ex: <http://example.org/>
      PREFIX dc: <http://purl.org/dc/elements/1.1/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      
      CONSTRUCT {
        ?book ex:hasAuthor ?authorName .
        ?book ex:hasTitle ?title .
      } WHERE {
        ?book a ex:Book ;
          dc:title ?title ;
          dc:creator ?author .
        ?author foaf:name ?authorName .
      }
    `;

    const constructResults = await graph.query(constructQuery);
    const constructedQuads = [];
    for await (const quad of constructResults) {
      constructedQuads.push(quad);
    }
    
    const constructedTurtle = await turtle.serialize(constructedQuads);
    console.log(constructedTurtle);

    // 3. ASK Query - Check if data exists
    console.log('\n❓ ASK Query - Does "1984" exist?');
    const askQuery = `
      PREFIX ex: <http://example.org/>
      PREFIX dc: <http://purl.org/dc/elements/1.1/>
      
      ASK {
        ?book dc:title "1984" .
      }
    `;

    const askResult = await graph.query(askQuery);
    console.log(`  Result: ${askResult}`);

    // 4. DESCRIBE Query - Get all information about a resource
    console.log('\n📖 DESCRIBE Query - Information about "The Great Gatsby":');
    const describeQuery = `
      PREFIX ex: <http://example.org/>
      PREFIX dc: <http://purl.org/dc/elements/1.1/>
      
      DESCRIBE ?book WHERE {
        ?book dc:title "The Great Gatsby" .
      }
    `;

    const describeResults = await graph.query(describeQuery);
    const describeQuads = [];
    for await (const quad of describeResults) {
      describeQuads.push(quad);
    }
    
    const describeTurtle = await turtle.serialize(describeQuads);
    console.log(describeTurtle);

    // 5. Filtered Query - Books published after 1950
    console.log('\n📅 Filtered Query - Books published after 1950:');
    const filterQuery = `
      PREFIX ex: <http://example.org/>
      PREFIX dc: <http://purl.org/dc/elements/1.1/>
      
      SELECT ?title ?year WHERE {
        ?book a ex:Book ;
          dc:title ?title ;
          ex:publishedYear ?year .
        FILTER(?year > 1950)
      }
    `;

    const filterResults = await graph.query(filterQuery);
    for await (const binding of filterResults) {
      console.log(`  - "${binding.get('title').value}" (${binding.get('year').value})`);
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
  }
}

main();

