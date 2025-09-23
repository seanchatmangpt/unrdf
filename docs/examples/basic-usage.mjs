#!/usr/bin/env node

/**
 * Basic Usage Examples for unrdf
 * 
 * This file demonstrates fundamental unrdf operations including:
 * - Store creation and management
 * - Term creation and manipulation
 * - Prefix management
 * - Basic SPARQL queries
 * - Data serialization
 */

import { 
  useStore, 
  useTerms, 
  usePrefixes, 
  useGraph, 
  useTurtle,
  useNQuads 
} from 'unrdf';

console.log('🚀 unrdf Basic Usage Examples\n');

// ============================================================================
// 1. Store Creation and Management
// ============================================================================

console.log('1. Store Creation and Management');
console.log('================================');

const store = useStore();
console.log('✅ Created empty store');
console.log(`   Store size: ${store.size()}`);

// ============================================================================
// 2. Term Creation
// ============================================================================

console.log('\n2. Term Creation');
console.log('================');

const terms = useTerms({
  baseIRI: "http://example.org/",
  defaultDatatype: "http://www.w3.org/2001/XMLSchema#string"
});

// Create different types of terms
const person = terms.iri("person1");
const name = terms.lit("John Doe");
const age = terms.lit(30, "http://www.w3.org/2001/XMLSchema#integer");
const email = terms.lit("john@example.org");
const bnode = terms.bnode("person1");

console.log('✅ Created terms:');
console.log(`   Person IRI: ${person.value}`);
console.log(`   Name literal: ${name.value}`);
console.log(`   Age literal: ${age.value} (${age.datatype.value})`);
console.log(`   Email literal: ${email.value}`);
console.log(`   Blank node: ${bnode.value}`);

// ============================================================================
// 3. Quad Creation and Storage
// ============================================================================

console.log('\n3. Quad Creation and Storage');
console.log('============================');

// Create quads
const nameQuad = terms.quad(person, terms.iri("name"), name);
const ageQuad = terms.quad(person, terms.iri("age"), age);
const emailQuad = terms.quad(person, terms.iri("email"), email);

// Add to store
store.add(nameQuad);
store.add(ageQuad);
store.add(emailQuad);

console.log('✅ Created and added quads:');
console.log(`   Store size: ${store.size()}`);

// Get store statistics
const stats = store.stats();
console.log('   Store statistics:');
console.log(`     - Quads: ${stats.quads}`);
console.log(`     - Subjects: ${stats.subjects}`);
console.log(`     - Predicates: ${stats.predicates}`);
console.log(`     - Objects: ${stats.objects}`);

// ============================================================================
// 4. Prefix Management
// ============================================================================

console.log('\n4. Prefix Management');
console.log('====================');

const prefixes = usePrefixes({
  "ex": "http://example.org/",
  "foaf": "http://xmlns.com/foaf/0.1/"
});

// Register additional prefixes
prefixes.register({
  "dc": "http://purl.org/dc/terms/",
  "schema": "https://schema.org/"
});

console.log('✅ Registered prefixes:');
const allPrefixes = prefixes.list();
Object.entries(allPrefixes).forEach(([prefix, uri]) => {
  console.log(`   ${prefix}: ${uri}`);
});

// Expand CURIEs
const expandedIRI = prefixes.expand("ex:person1");
console.log(`\n✅ Expanded CURIE: ex:person1 → ${expandedIRI}`);

// Shrink IRIs
const shrunkCURIE = prefixes.shrink("http://example.org/person1");
console.log(`✅ Shrunk IRI: ${expandedIRI} → ${shrunkCURIE}`);

// ============================================================================
// 5. SPARQL Queries
// ============================================================================

console.log('\n5. SPARQL Queries');
console.log('=================');

const graph = useGraph(store.store);

// Simple SELECT query
const selectResults = await graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?name ?age ?email WHERE {
    ?person ex:name ?name ;
            ex:age ?age ;
            ex:email ?email .
  }
`);

console.log('✅ SELECT query results:');
selectResults.forEach((result, index) => {
  console.log(`   Result ${index + 1}:`);
  console.log(`     Name: ${result.name.value}`);
  console.log(`     Age: ${result.age.value}`);
  console.log(`     Email: ${result.email.value}`);
});

// ASK query
const askResult = await graph.ask(`
  PREFIX ex: <http://example.org/>
  ASK WHERE {
    ?person ex:name "John Doe" .
  }
`);

console.log(`\n✅ ASK query result: ${askResult}`);

// ============================================================================
// 6. Data Serialization
// ============================================================================

console.log('\n6. Data Serialization');
console.log('======================');

// Serialize to Turtle
const turtle = useTurtle();
const turtleString = await turtle.serialize(store.store);
console.log('✅ Turtle serialization:');
console.log(turtleString);

// Serialize to N-Quads
const nquads = useNQuads();
const nquadsString = await nquads.serialize(store.store);
console.log('\n✅ N-Quads serialization:');
console.log(nquadsString);

// ============================================================================
// 7. Data Parsing
// ============================================================================

console.log('\n7. Data Parsing');
console.log('===============');

// Parse Turtle string
const turtleData = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:person2 a foaf:Person ;
    foaf:name "Jane Smith" ;
    foaf:age 28 ;
    foaf:mbox "jane@example.org" .
`;

const parsedStore = await turtle.parse(turtleData);
console.log('✅ Parsed Turtle data:');
console.log(`   Parsed store size: ${parsedStore.size()}`);

// Query the parsed data
const parsedGraph = useGraph(parsedStore);
const parsedResults = await parsedGraph.select(`
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?age ?email WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age ;
            foaf:mbox ?email .
  }
`);

console.log('   Parsed data results:');
parsedResults.forEach((result, index) => {
  console.log(`     Person ${index + 1}:`);
  console.log(`       Name: ${result.name.value}`);
  console.log(`       Age: ${result.age.value}`);
  console.log(`       Email: ${result.email.value}`);
});

// ============================================================================
// 8. Error Handling
// ============================================================================

console.log('\n8. Error Handling');
console.log('==================');

try {
  // This will throw an error
  const invalidIRI = terms.iri(123);
} catch (error) {
  console.log('✅ Caught expected error:');
  console.log(`   ${error.message}`);
}

try {
  // This will also throw an error
  const invalidLiteral = terms.lit(null);
} catch (error) {
  console.log('✅ Caught expected error:');
  console.log(`   ${error.message}`);
}

// ============================================================================
// 9. Store Operations
// ============================================================================

console.log('\n9. Store Operations');
console.log('===================');

// Check if store has specific quads
const hasNameQuad = store.has(nameQuad);
const hasAgeQuad = store.has(ageQuad);

console.log('✅ Store operations:');
console.log(`   Has name quad: ${hasNameQuad}`);
console.log(`   Has age quad: ${hasAgeQuad}`);

// Remove a quad
store.remove(emailQuad);
console.log(`   After removing email quad: ${store.size()} quads`);

// Check if email quad still exists
const hasEmailQuad = store.has(emailQuad);
console.log(`   Has email quad after removal: ${hasEmailQuad}`);

// Clear the store
store.clear();
console.log(`   After clearing store: ${store.size()} quads`);

// ============================================================================
// 10. Advanced Term Operations
// ============================================================================

console.log('\n10. Advanced Term Operations');
console.log('============================');

// Create a new store for advanced operations
const advancedStore = useStore();
const advancedTerms = useTerms({ baseIRI: "http://advanced.example.org/" });

// Create complex data structure
const company = advancedTerms.iri("company1");
const employee1 = advancedTerms.iri("employee1");
const employee2 = advancedTerms.iri("employee2");

// Add company information
advancedStore.add(advancedTerms.quad(
  company,
  advancedTerms.iri("name"),
  advancedTerms.lit("Acme Corp")
));

advancedStore.add(advancedTerms.quad(
  company,
  advancedTerms.iri("founded"),
  advancedTerms.lit("2020-01-01", "http://www.w3.org/2001/XMLSchema#date")
));

// Add employee information
advancedStore.add(advancedTerms.quad(
  employee1,
  advancedTerms.iri("name"),
  advancedTerms.lit("Alice Johnson")
));

advancedStore.add(advancedTerms.quad(
  employee1,
  advancedTerms.iri("worksFor"),
  company
));

advancedStore.add(advancedTerms.quad(
  employee2,
  advancedTerms.iri("name"),
  advancedTerms.lit("Bob Wilson")
));

advancedStore.add(advancedTerms.quad(
  employee2,
  advancedTerms.iri("worksFor"),
  company
));

console.log('✅ Created advanced data structure:');
console.log(`   Advanced store size: ${advancedStore.size()}`);

// Query for employees of the company
const advancedGraph = useGraph(advancedStore);
const employeeResults = await advancedGraph.select(`
  PREFIX ex: <http://advanced.example.org/>
  SELECT ?employeeName WHERE {
    ?employee ex:worksFor ex:company1 ;
              ex:name ?employeeName .
  }
`);

console.log('   Employees of Acme Corp:');
employeeResults.forEach((result, index) => {
  console.log(`     ${index + 1}. ${result.employeeName.value}`);
});

// ============================================================================
// Summary
// ============================================================================

console.log('\n🎉 Basic Usage Examples Complete!');
console.log('==================================');
console.log('✅ Store creation and management');
console.log('✅ Term creation and manipulation');
console.log('✅ Quad creation and storage');
console.log('✅ Prefix management');
console.log('✅ SPARQL queries (SELECT, ASK)');
console.log('✅ Data serialization (Turtle, N-Quads)');
console.log('✅ Data parsing');
console.log('✅ Error handling');
console.log('✅ Store operations');
console.log('✅ Advanced term operations');
console.log('\n🚀 Ready for more advanced examples!');
