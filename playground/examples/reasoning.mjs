#!/usr/bin/env node

/**
 * Reasoning Example
 * 
 * This example demonstrates RDF reasoning capabilities:
 * - OWL reasoning with EyeReasoner
 * - Rule-based inference
 * - Materializing inferred triples
 */

import { useStore, useGraph, useTurtle, useReasoner } from 'unrdf';

console.log('🧠 UNRDF Reasoning Example\n');

async function main() {
  try {
    // Initialize components
    const store = useStore();
    const graph = useGraph(store);
    const turtle = useTurtle();
    const reasoner = useReasoner();

    // Define OWL ontology with classes and properties
    const ontology = `
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix ex: <http://example.org/> .
      
      # Class definitions
      ex:Person a owl:Class .
      ex:Student a owl:Class ;
        rdfs:subClassOf ex:Person .
      ex:Teacher a owl:Class ;
        rdfs:subClassOf ex:Person .
      ex:Course a owl:Class .
      
      # Property definitions
      ex:teaches a owl:ObjectProperty ;
        rdfs:domain ex:Teacher ;
        rdfs:range ex:Course .
      
      ex:enrolledIn a owl:ObjectProperty ;
        rdfs:domain ex:Student ;
        rdfs:range ex:Course .
      
      # Inverse property
      ex:taughtBy a owl:ObjectProperty ;
        owl:inverseOf ex:teaches .
      
      # Transitive property
      ex:supervisorOf a owl:ObjectProperty ;
        rdf:type owl:TransitiveProperty .
    `;

    // Parse and add ontology
    const ontologyQuads = await turtle.parse(ontology);
    await graph.addQuads(ontologyQuads);
    console.log('✅ OWL ontology loaded');

    // Add instance data
    const instances = `
      @prefix ex: <http://example.org/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      
      # Teachers
      ex:alice a ex:Teacher ;
        foaf:name "Alice Johnson" .
      
      ex:bob a ex:Teacher ;
        foaf:name "Bob Smith" .
      
      # Students
      ex:charlie a ex:Student ;
        foaf:name "Charlie Brown" .
      
      ex:diana a ex:Student ;
        foaf:name "Diana Prince" .
      
      # Courses
      ex:math101 a ex:Course ;
        rdfs:label "Mathematics 101" .
      
      ex:physics201 a ex:Course ;
        rdfs:label "Physics 201" .
      
      # Teaching relationships
      ex:alice ex:teaches ex:math101 .
      ex:bob ex:teaches ex:physics201 .
      
      # Enrollment relationships
      ex:charlie ex:enrolledIn ex:math101 .
      ex:diana ex:enrolledIn ex:physics201 .
      
      # Supervision hierarchy
      ex:alice ex:supervisorOf ex:bob .
    `;

    const instanceQuads = await turtle.parse(instances);
    await graph.addQuads(instanceQuads);
    console.log('✅ Instance data loaded');

    // Perform reasoning
    console.log('\n🔍 Performing OWL reasoning...');
    const reasonedQuads = await reasoner.reason(graph.getQuads());
    
    // Add inferred triples to the graph
    await graph.addQuads(reasonedQuads);
    console.log(`✅ Reasoning completed - ${reasonedQuads.length} new triples inferred`);

    // Query for inferred relationships
    console.log('\n📊 Querying inferred relationships:');

    // 1. Check for inverse property relationships
    const inverseQuery = `
      PREFIX ex: <http://example.org/>
      
      SELECT ?course ?teacher WHERE {
        ?course ex:taughtBy ?teacher .
      }
    `;

    console.log('\n🔄 Inverse Property Results (taughtBy):');
    const inverseResults = await graph.query(inverseQuery);
    for await (const binding of inverseResults) {
      console.log(`  - ${binding.get('course').value} is taught by ${binding.get('teacher').value}`);
    }

    // 2. Check for transitive property relationships
    const transitiveQuery = `
      PREFIX ex: <http://example.org/>
      
      SELECT ?supervisor ?subordinate WHERE {
        ?supervisor ex:supervisorOf ?subordinate .
      }
    `;

    console.log('\n🔗 Transitive Property Results (supervisorOf):');
    const transitiveResults = await graph.query(transitiveQuery);
    for await (const binding of transitiveResults) {
      console.log(`  - ${binding.get('supervisor').value} supervises ${binding.get('subordinate').value}`);
    }

    // 3. Check for subclass relationships
    const subclassQuery = `
      PREFIX ex: <http://example.org/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      
      SELECT ?person ?type WHERE {
        ?person a ?type .
        ?type rdfs:subClassOf ex:Person .
      }
    `;

    console.log('\n👥 Subclass Results (Person hierarchy):');
    const subclassResults = await graph.query(subclassQuery);
    for await (const binding of subclassResults) {
      console.log(`  - ${binding.get('person').value} is a ${binding.get('type').value}`);
    }

    // Demonstrate custom rule-based reasoning
    console.log('\n⚙️  Custom Rule-based Reasoning:');
    
    const customRules = [
      {
        name: 'Student-Teacher Relationship',
        condition: (quads) => {
          const studentCoursePairs = [];
          const teacherCoursePairs = [];
          
          for (const quad of quads) {
            if (quad.predicate.value === 'http://example.org/enrolledIn') {
              studentCoursePairs.push({
                student: quad.subject.value,
                course: quad.object.value
              });
            }
            if (quad.predicate.value === 'http://example.org/teaches') {
              teacherCoursePairs.push({
                teacher: quad.subject.value,
                course: quad.object.value
              });
            }
          }
          
          return { studentCoursePairs, teacherCoursePairs };
        },
        inference: (data) => {
          const inferred = [];
          const { studentCoursePairs, teacherCoursePairs } = data;
          
          for (const studentPair of studentCoursePairs) {
            for (const teacherPair of teacherCoursePairs) {
              if (studentPair.course === teacherPair.course) {
                // Create a new quad for student-teacher relationship
                const studentTerm = { value: studentPair.student, termType: 'NamedNode' };
                const teacherTerm = { value: teacherPair.teacher, termType: 'NamedNode' };
                const predicateTerm = { value: 'http://example.org/taughtBy', termType: 'NamedNode' };
                
                inferred.push({
                  subject: studentTerm,
                  predicate: predicateTerm,
                  object: teacherTerm,
                  graph: { value: 'http://example.org/inferred', termType: 'NamedNode' }
                });
              }
            }
          }
          
          return inferred;
        }
      }
    ];

    // Apply custom rules
    for (const rule of customRules) {
      console.log(`\n  Applying rule: ${rule.name}`);
      const conditionData = rule.condition(graph.getQuads());
      const inferredTriples = rule.inference(conditionData);
      
      if (inferredTriples.length > 0) {
        await graph.addQuads(inferredTriples);
        console.log(`    ✅ Inferred ${inferredTriples.length} new relationships`);
        
        // Query the new relationships
        const newRelQuery = `
          PREFIX ex: <http://example.org/>
          
          SELECT ?student ?teacher WHERE {
            ?student ex:taughtBy ?teacher .
          }
        `;
        
        const newRelResults = await graph.query(newRelQuery);
        for await (const binding of newRelResults) {
          console.log(`    - ${binding.get('student').value} is taught by ${binding.get('teacher').value}`);
        }
      } else {
        console.log('    ℹ️  No new relationships inferred');
      }
    }

    // Show final graph statistics
    const allQuads = graph.getQuads();
    console.log(`\n📈 Final Graph Statistics:`);
    console.log(`  - Total triples: ${allQuads.length}`);
    console.log(`  - Original triples: ${ontologyQuads.length + instanceQuads.length}`);
    console.log(`  - Inferred triples: ${allQuads.length - ontologyQuads.length - instanceQuads.length}`);

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
  }
}

main();
