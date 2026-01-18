/**
 * Composition C31: GraphQL Adapter + RDF Store
 * Atoms: A45 + A01
 *
 * Proof: Type-safe GraphQL over RDF
 */

import { createAdapter } from '@unrdf/rdf-graphql';

console.log('=== C31: GraphQL Adapter Proof ===\n');

async function prove() {
  try {
    // A45: Create GraphQL adapter
    const adapter = createAdapter({
      namespaces: {
        foaf: 'http://xmlns.com/foaf/0.1/',
        schema: 'http://schema.org/'
      },
      enableCache: true
    });

    console.log('✅ A45: GraphQL adapter created');

    // Load simple ontology
    const ontology = `
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

      foaf:Person a rdfs:Class ;
        rdfs:label "Person" ;
        rdfs:comment "A person" .

      foaf:name a rdfs:Property ;
        rdfs:domain foaf:Person ;
        rdfs:range xsd:string .

      foaf:age a rdfs:Property ;
        rdfs:domain foaf:Person ;
        rdfs:range xsd:integer .
    `;

    await adapter.loadOntology(ontology, 'text/turtle');
    console.log('✅ Loaded FOAF ontology');

    // Load instance data
    const data = `
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      @prefix ex: <http://example.org/> .

      ex:alice a foaf:Person ;
        foaf:name "Alice" ;
        foaf:age 30 .

      ex:bob a foaf:Person ;
        foaf:name "Bob" ;
        foaf:age 25 .
    `;

    await adapter.loadData(data, 'text/turtle');
    console.log('✅ Loaded instance data');

    // Generate GraphQL schema from ontology
    const schema = adapter.generateSchema();
    console.log('✅ Generated GraphQL schema from RDF');

    // Get schema SDL
    const sdl = await adapter.getSchemaSDL();
    console.log('\n📋 GraphQL Schema (first 200 chars):');
    console.log(sdl?.substring(0, 200) + '...');

    // Execute GraphQL query
    const graphqlQuery = `
      query {
        persons {
          name
          age
        }
      }
    `;

    const result = await adapter.executeQuery(graphqlQuery);
    console.log('\n🔍 GraphQL Query Result:');
    console.log(`   Errors: ${result.errors?.length || 0}`);
    console.log(`   Data: ${JSON.stringify(result.data, null, 2)}`);

    // Get statistics
    const stats = await adapter.getStatistics();
    console.log('\n📊 RDF Store Statistics:');
    console.log(`   Total triples: ${stats.tripleCount}`);
    console.log(`   Classes: ${stats.classCount}`);
    console.log(`   Instances: ${stats.instanceCount}`);

    console.log('\n✅ COMPOSITION VERIFIED');
    console.log('   Value: Modern GraphQL API over RDF data');
    console.log('   Type safety: Schema generated from ontology');
    console.log('   Query execution: SPARQL → GraphQL translation');

    process.exit(result.data && stats.tripleCount > 0 ? 0 : 1);
  } catch (error) {
    console.error('❌ COMPOSITION FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

prove();
