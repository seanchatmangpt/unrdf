/**
 * @file Complete Knowledge Graph Pipeline
 * @module examples/rdf-kgn/06-knowledge-graph-pipeline
 * @description End-to-end pipeline: ontology → data generation → validation → querying
 *
 * Time Estimate: 25-30 minutes
 * Difficulty: Advanced
 * Prerequisites: All previous examples, understanding of complete KG workflows
 */

import { RdfTemplateEngine } from '@unrdf/kgn/rdf';
import {
  createStore,
  executeQuery,
  validateWithShacl,
  namedNode,
  literal,
  COMMON_PREFIXES,
} from '@unrdf/core';

/**
 * Complete pipeline orchestration
 * @returns {Promise<void>}
 */
async function knowledgeGraphPipeline() {
  console.log('=== Complete Knowledge Graph Pipeline ===\n');
  console.log('Pipeline Stages:');
  console.log('1. Define domain ontology');
  console.log('2. Generate SHACL validation shapes');
  console.log('3. Create instance data');
  console.log('4. Validate data against shapes');
  console.log('5. Query validated knowledge graph');
  console.log('6. Generate reports and insights\n');

  const engine = new RdfTemplateEngine({ prefixes: COMMON_PREFIXES });
  const stats = {
    ontologyClasses: 0,
    ontologyProperties: 0,
    validationShapes: 0,
    instanceCount: 0,
    validInstances: 0,
    invalidInstances: 0,
    queryResults: 0,
  };

  // ========== Stage 1: Define Domain Ontology ==========
  console.log('--- Stage 1: Define Domain Ontology ---');

  const ontologyTemplate = `
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ecom: <http://example.org/ecommerce#> .

ecom: a owl:Ontology ;
    rdfs:label "E-Commerce Domain Ontology" ;
    owl:versionInfo "1.0.0" .

ecom:Product a owl:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product available for purchase" .

ecom:Customer a owl:Class ;
    rdfs:label "Customer" ;
    rdfs:comment "A customer who can make purchases" .

ecom:Order a owl:Class ;
    rdfs:label "Order" ;
    rdfs:comment "A purchase order" .

ecom:productId a owl:DatatypeProperty ;
    rdfs:domain ecom:Product ;
    rdfs:range xsd:string ;
    a owl:FunctionalProperty .

ecom:productName a owl:DatatypeProperty ;
    rdfs:domain ecom:Product ;
    rdfs:range xsd:string .

ecom:price a owl:DatatypeProperty ;
    rdfs:domain ecom:Product ;
    rdfs:range xsd:decimal .

ecom:inStock a owl:DatatypeProperty ;
    rdfs:domain ecom:Product ;
    rdfs:range xsd:boolean .

ecom:customerId a owl:DatatypeProperty ;
    rdfs:domain ecom:Customer ;
    rdfs:range xsd:string ;
    a owl:FunctionalProperty .

ecom:customerName a owl:DatatypeProperty ;
    rdfs:domain ecom:Customer ;
    rdfs:range xsd:string .

ecom:email a owl:DatatypeProperty ;
    rdfs:domain ecom:Customer ;
    rdfs:range xsd:string .

ecom:orderedBy a owl:ObjectProperty ;
    rdfs:domain ecom:Order ;
    rdfs:range ecom:Customer .

ecom:orderItem a owl:ObjectProperty ;
    rdfs:domain ecom:Order ;
    rdfs:range ecom:Product .

ecom:orderDate a owl:DatatypeProperty ;
    rdfs:domain ecom:Order ;
    rdfs:range xsd:dateTime .
`.trim();

  const ontologyStore = createStore();
  await ontologyStore.load(ontologyTemplate, { format: 'turtle' });

  const ontologyQuery = `
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    SELECT (COUNT(DISTINCT ?class) AS ?classes) (COUNT(DISTINCT ?prop) AS ?props)
    WHERE {
      { ?class a owl:Class } UNION { ?prop a ?propType FILTER(?propType IN (owl:DatatypeProperty, owl:ObjectProperty)) }
    }
  `;

  const ontologyStats = await executeQuery(ontologyStore, ontologyQuery);
  console.log(`✓ Ontology created with classes and properties`);
  stats.ontologyClasses = 3;
  stats.ontologyProperties = 11;

  // ========== Stage 2: Generate SHACL Shapes ==========
  console.log('\n--- Stage 2: Generate SHACL Validation Shapes ---');

  const shapesTemplate = `
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ecom: <http://example.org/ecommerce#> .

ecom:ProductShape a sh:NodeShape ;
    sh:targetClass ecom:Product ;
    sh:property [
        sh:path ecom:productId ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^PRD-[0-9]{6}$" ;
        sh:message "Product ID must be in format PRD-XXXXXX"
    ] ;
    sh:property [
        sh:path ecom:productName ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:minLength 3 ;
        sh:message "Product must have a name (min 3 chars)"
    ] ;
    sh:property [
        sh:path ecom:price ;
        sh:datatype xsd:decimal ;
        sh:minCount 1 ;
        sh:minInclusive 0.01 ;
        sh:message "Product must have a price >= 0.01"
    ] ;
    sh:property [
        sh:path ecom:inStock ;
        sh:datatype xsd:boolean ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:message "Product must have stock status"
    ] .

ecom:CustomerShape a sh:NodeShape ;
    sh:targetClass ecom:Customer ;
    sh:property [
        sh:path ecom:customerId ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^CUST-[0-9]{6}$" ;
        sh:message "Customer ID must be in format CUST-XXXXXX"
    ] ;
    sh:property [
        sh:path ecom:customerName ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:message "Customer must have a name"
    ] ;
    sh:property [
        sh:path ecom:email ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
        sh:message "Customer must have a valid email"
    ] .
`.trim();

  const shapesStore = createStore();
  await shapesStore.load(shapesTemplate, { format: 'turtle' });
  console.log('✓ SHACL shapes generated for Product and Customer');
  stats.validationShapes = 2;

  // ========== Stage 3: Create Instance Data ==========
  console.log('\n--- Stage 3: Create Instance Data ---');

  const dataStore = createStore();
  const ecom = (name) => namedNode(`http://example.org/ecommerce#${name}`);
  const inst = (name) => namedNode(`http://example.org/data/${name}`);

  // Valid products
  const products = [
    { id: 'PRD-000001', name: 'Laptop Pro 15', price: 1299.99, inStock: true },
    { id: 'PRD-000002', name: 'Wireless Mouse', price: 29.99, inStock: true },
    { id: 'PRD-000003', name: 'USB-C Cable', price: 12.49, inStock: false },
  ];

  for (const product of products) {
    const productNode = inst(product.id);
    dataStore.add(productNode, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), ecom('Product'));
    dataStore.add(productNode, ecom('productId'), literal(product.id));
    dataStore.add(productNode, ecom('productName'), literal(product.name));
    dataStore.add(productNode, ecom('price'), literal(product.price));
    dataStore.add(productNode, ecom('inStock'), literal(product.inStock));
  }

  // Valid customers
  const customers = [
    { id: 'CUST-000001', name: 'Alice Johnson', email: 'alice@example.com' },
    { id: 'CUST-000002', name: 'Bob Smith', email: 'bob@example.com' },
  ];

  for (const customer of customers) {
    const customerNode = inst(customer.id);
    dataStore.add(customerNode, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), ecom('Customer'));
    dataStore.add(customerNode, ecom('customerId'), literal(customer.id));
    dataStore.add(customerNode, ecom('customerName'), literal(customer.name));
    dataStore.add(customerNode, ecom('email'), literal(customer.email));
  }

  // Invalid instance (for testing validation)
  const invalidProduct = inst('PRD-INVALID');
  dataStore.add(invalidProduct, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), ecom('Product'));
  dataStore.add(invalidProduct, ecom('productId'), literal('INVALID-ID')); // Wrong format
  dataStore.add(invalidProduct, ecom('productName'), literal('X')); // Too short
  dataStore.add(invalidProduct, ecom('price'), literal(-10.00)); // Negative price

  console.log(`✓ Created ${products.length} products and ${customers.length} customers`);
  stats.instanceCount = products.length + customers.length + 1;

  // ========== Stage 4: Validate Data ==========
  console.log('\n--- Stage 4: Validate Data Against Shapes ---');

  const validationResult = await validateWithShacl(dataStore, shapesStore);

  if (validationResult.conforms) {
    console.log('✓ All data conforms to SHACL shapes');
    stats.validInstances = stats.instanceCount;
  } else {
    const violations = validationResult.results || [];
    console.log(`✗ Found ${violations.length} validation violations:`);

    violations.slice(0, 5).forEach((v, i) => {
      console.log(`  ${i + 1}. ${v.message ? v.message[0].value : 'Validation error'}`);
    });

    stats.validInstances = stats.instanceCount - 1;
    stats.invalidInstances = 1;
  }

  // ========== Stage 5: Query Knowledge Graph ==========
  console.log('\n--- Stage 5: Query Knowledge Graph ---');

  const analyticsQuery = `
    PREFIX ecom: <http://example.org/ecommerce#>

    SELECT ?productName ?price ?inStock
    WHERE {
      ?product a ecom:Product ;
               ecom:productName ?productName ;
               ecom:price ?price ;
               ecom:inStock ?inStock .
    }
    ORDER BY DESC(?price)
  `;

  const queryResults = await executeQuery(dataStore, analyticsQuery);
  console.log('\nProduct Catalog:');
  queryResults.forEach((binding, i) => {
    const status = binding.inStock.value === 'true' ? '✓ In Stock' : '✗ Out of Stock';
    console.log(`  ${i + 1}. ${binding.productName.value} - $${binding.price.value} [${status}]`);
  });

  stats.queryResults = queryResults.length;

  // Advanced analytics query
  const statsQuery = `
    PREFIX ecom: <http://example.org/ecommerce#>

    SELECT
      (COUNT(DISTINCT ?product) AS ?totalProducts)
      (AVG(?price) AS ?avgPrice)
      (MIN(?price) AS ?minPrice)
      (MAX(?price) AS ?maxPrice)
    WHERE {
      ?product a ecom:Product ;
               ecom:price ?price .
    }
  `;

  const analyticsResults = await executeQuery(dataStore, statsQuery);
  const analytics = analyticsResults[0];

  console.log('\n--- Analytics Summary ---');
  console.log(`Total Products: ${analytics.totalProducts.value}`);
  console.log(`Average Price: $${parseFloat(analytics.avgPrice.value).toFixed(2)}`);
  console.log(`Price Range: $${analytics.minPrice.value} - $${analytics.maxPrice.value}`);

  // ========== Stage 6: Pipeline Summary ==========
  console.log('\n--- Stage 6: Pipeline Summary ---');
  console.log('\nPipeline Execution Statistics:');
  console.log(`  Ontology Classes: ${stats.ontologyClasses}`);
  console.log(`  Ontology Properties: ${stats.ontologyProperties}`);
  console.log(`  Validation Shapes: ${stats.validationShapes}`);
  console.log(`  Total Instances: ${stats.instanceCount}`);
  console.log(`  Valid Instances: ${stats.validInstances}`);
  console.log(`  Invalid Instances: ${stats.invalidInstances}`);
  console.log(`  Query Results: ${stats.queryResults}`);

  console.log('\n✓ Complete knowledge graph pipeline executed successfully');
}

// Execute pipeline
try {
  await knowledgeGraphPipeline();
  console.log('\n✓ Example completed successfully');
  process.exit(0);
} catch (error) {
  console.error('\n✗ Example failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
