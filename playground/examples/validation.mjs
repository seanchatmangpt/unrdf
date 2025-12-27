#!/usr/bin/env node

/**
 * Validation Example
 * 
 * This example demonstrates RDF validation using SHACL:
 * - Creating SHACL shapes
 * - Validating RDF data against shapes
 * - Handling validation results
 */

import { useStore, useGraph, useTurtle, useValidator } from 'unrdf';

console.log('✅ UNRDF Validation Example\n');

async function main() {
  try {
    // Initialize components
    const store = useStore();
    const graph = useGraph(store);
    const turtle = useTurtle();
    const validator = useValidator();

    // Define SHACL shape for validating person data
    const shaclShape = `
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      @prefix ex: <http://example.org/> .
      
      ex:PersonShape a sh:NodeShape ;
        sh:targetClass foaf:Person ;
        sh:property [
          sh:path foaf:name ;
          sh:datatype xsd:string ;
          sh:minCount 1 ;
          sh:maxCount 1 ;
        ] ;
        sh:property [
          sh:path foaf:age ;
          sh:datatype xsd:integer ;
          sh:minInclusive 0 ;
          sh:maxInclusive 150 ;
        ] ;
        sh:property [
          sh:path foaf:email ;
          sh:datatype xsd:string ;
          sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
        ] .
    `;

    // Parse and add the SHACL shape
    const shapeQuads = await turtle.parse(shaclShape);
    await graph.addQuads(shapeQuads);
    console.log('✅ SHACL shape loaded');

    // Create some test data - some valid, some invalid
    const testData = `
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      @prefix ex: <http://example.org/> .
      
      # Valid person
      ex:person1 a foaf:Person ;
        foaf:name "John Doe" ;
        foaf:age 30 ;
        foaf:email "john@example.com" .
      
      # Invalid person - missing required name
      ex:person2 a foaf:Person ;
        foaf:age 25 ;
        foaf:email "jane@example.com" .
      
      # Invalid person - invalid email format
      ex:person3 a foaf:Person ;
        foaf:name "Bob Smith" ;
        foaf:age 35 ;
        foaf:email "invalid-email" .
      
      # Invalid person - age out of range
      ex:person4 a foaf:Person ;
        foaf:name "Alice Johnson" ;
        foaf:age 200 ;
        foaf:email "alice@example.com" .
    `;

    const dataQuads = await turtle.parse(testData);
    await graph.addQuads(dataQuads);
    console.log('✅ Test data loaded');

    // Validate the data against the SHACL shape
    console.log('\n🔍 Validating data against SHACL shape...');
    const validationResults = await validator.validate(graph.getQuads(), shapeQuads);
    
    console.log(`\n📊 Validation Results:`);
    console.log(`  - Conforms: ${validationResults.conforms}`);
    console.log(`  - Total violations: ${validationResults.results.length}`);

    if (validationResults.results.length > 0) {
      console.log('\n❌ Validation Violations:');
      for (const result of validationResults.results) {
        console.log(`  - Focus Node: ${result.focusNode.value}`);
        console.log(`    Severity: ${result.severity.value}`);
        console.log(`    Message: ${result.resultMessage[0].value}`);
        console.log(`    Path: ${result.resultPath ? result.resultPath.value : 'N/A'}`);
        console.log('');
      }
    } else {
      console.log('\n✅ All data conforms to the SHACL shape!');
    }

    // Demonstrate validation with custom constraints
    console.log('\n🔧 Custom Validation Example:');
    
    // Create a custom validation function
    const customValidator = (quads) => {
      const violations = [];
      const personQuads = quads.filter(q => 
        q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
        q.object.value === 'http://xmlns.com/foaf/0.1/Person'
      );

      for (const personQuad of personQuads) {
        const personSubject = personQuad.subject;
        const nameQuads = quads.filter(q => 
          q.subject.equals(personSubject) && 
          q.predicate.value === 'http://xmlns.com/foaf/0.1/name'
        );

        if (nameQuads.length === 0) {
          violations.push({
            focusNode: personSubject.value,
            message: 'Person must have a name',
            severity: 'http://www.w3.org/ns/shacl#Violation'
          });
        }
      }

      return {
        conforms: violations.length === 0,
        results: violations
      };
    };

    const customResults = customValidator(graph.getQuads());
    console.log(`  - Custom validation conforms: ${customResults.conforms}`);
    if (customResults.results.length > 0) {
      console.log('  - Custom validation violations:');
      for (const violation of customResults.results) {
        console.log(`    * ${violation.focusNode}: ${violation.message}`);
      }
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
  }
}

main();

