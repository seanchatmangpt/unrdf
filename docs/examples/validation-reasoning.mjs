#!/usr/bin/env node

/**
 * Validation and Reasoning Examples for unrdf
 * 
 * This file demonstrates advanced unrdf features including:
 * - SHACL validation
 * - EYE reasoning
 * - Zod runtime validation
 * - Canonicalization
 * - Data quality assessment
 */

import { 
  useStore, 
  useTerms, 
  useValidator, 
  useReasoner, 
  useZod,
  useCanon,
  useTurtle
} from 'unrdf';
import { z } from 'zod';

console.log('🔍 unrdf Validation and Reasoning Examples\n');

// ============================================================================
// 1. SHACL Validation
// ============================================================================

console.log('1. SHACL Validation');
console.log('===================');

const turtle = useTurtle();

// Define SHACL shapes
const shapesTurtle = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
  
  ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:datatype xsd:string ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:message "Person must have exactly one name"
    ] ;
    sh:property [
      sh:path foaf:age ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0 ;
      sh:maxInclusive 150 ;
      sh:message "Age must be between 0 and 150"
    ] ;
    sh:property [
      sh:path foaf:mbox ;
      sh:datatype xsd:string ;
      sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
      sh:message "Email must be valid"
    ] .
`;

// Create data to validate
const dataTurtle = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  
  ex:person1 a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 ;
    foaf:mbox "john@example.org" .
    
  ex:person2 a foaf:Person ;
    foaf:name "Jane Smith" ;
    foaf:age 25 ;
    foaf:mbox "jane@example.org" .
    
  ex:person3 a foaf:Person ;
    foaf:name "Bob Wilson" ;
    foaf:age 200 ;  # Invalid: age > 150
    foaf:mbox "invalid-email" .  # Invalid: not a valid email
`;

// Parse shapes and data
const shapesStore = await turtle.parse(shapesTurtle);
const dataStore = await turtle.parse(dataTurtle);

console.log('✅ Created SHACL shapes and test data');
console.log(`   Shapes store size: ${shapesStore.size()}`);
console.log(`   Data store size: ${dataStore.size()}`);

// Validate data against shapes
const validator = useValidator();
const validationReport = await validator.validate(dataStore, shapesStore);

console.log('\n✅ SHACL Validation Results:');
console.log(`   Conforms: ${validationReport.conforms}`);

if (!validationReport.conforms) {
  console.log('   Violations:');
  validationReport.results.forEach((violation, index) => {
    console.log(`     ${index + 1}. ${violation.message[0].value}`);
    console.log(`        Focus node: ${violation.focusNode.value}`);
    console.log(`        Path: ${violation.resultPath?.value || 'N/A'}`);
  });
}

// ============================================================================
// 2. EYE Reasoning
// ============================================================================

console.log('\n2. EYE Reasoning');
console.log('================');

// Create data for reasoning
const reasoningDataTurtle = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  
  ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    foaf:age 25 .
    
  ex:bob a foaf:Person ;
    foaf:name "Bob" ;
    foaf:age 30 .
    
  ex:charlie a foaf:Person ;
    foaf:name "Charlie" ;
    foaf:age 35 .
    
  ex:alice foaf:knows ex:bob .
  ex:bob foaf:knows ex:charlie .
`;

// Define reasoning rules
const rulesTurtle = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  # Rule 1: If someone is over 30, they are an adult
  { ?person foaf:age ?age . ?age > 30 } => { ?person ex:isAdult true } .
  
  # Rule 2: If A knows B and B knows C, then A knows C (transitive)
  { ?a foaf:knows ?b . ?b foaf:knows ?c } => { ?a foaf:knows ?c } .
  
  # Rule 3: If someone is an adult, they can vote
  { ?person ex:isAdult true } => { ?person ex:canVote true } .
`;

// Parse data and rules
const reasoningDataStore = await turtle.parse(reasoningDataTurtle);
const rulesStore = await turtle.parse(rulesTurtle);

console.log('✅ Created reasoning data and rules');
console.log(`   Data store size: ${reasoningDataStore.size()}`);
console.log(`   Rules store size: ${rulesStore.size()}`);

// Perform reasoning
const reasoner = useReasoner();
const inferredStore = await reasoner.reason(reasoningDataStore, rulesStore);

console.log('\n✅ Reasoning Results:');
console.log(`   Original quads: ${reasoningDataStore.size()}`);
console.log(`   Inferred quads: ${inferredStore.size()}`);
console.log(`   Total quads: ${inferredStore.size()}`);

// Query for inferred facts
const { useGraph } = await import('unrdf');
const reasoningGraph = useGraph(inferredStore);

const adultResults = await reasoningGraph.select(`
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person ex:isAdult true ;
            foaf:name ?name .
  }
`);

console.log('\n   Inferred adults:');
adultResults.forEach((result, index) => {
  console.log(`     ${index + 1}. ${result.name.value} (${result.person.value})`);
});

const votingResults = await reasoningGraph.select(`
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person ex:canVote true ;
            foaf:name ?name .
  }
`);

console.log('\n   People who can vote:');
votingResults.forEach((result, index) => {
  console.log(`     ${index + 1}. ${result.name.value} (${result.person.value})`);
});

const transitiveResults = await reasoningGraph.select(`
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person1 ?person2 WHERE {
    ?person1 foaf:knows ?person2 .
  }
`);

console.log('\n   All known relationships (including inferred):');
transitiveResults.forEach((result, index) => {
  console.log(`     ${index + 1}. ${result.person1.value} knows ${result.person2.value}`);
});

// ============================================================================
// 3. Zod Runtime Validation
// ============================================================================

console.log('\n3. Zod Runtime Validation');
console.log('==========================');

// Define Zod schema
const PersonSchema = z.object({
  name: z.string().min(1, "Name is required"),
  age: z.number().min(0, "Age must be non-negative").max(150, "Age must be <= 150"),
  email: z.string().email("Invalid email format").optional(),
  isAdult: z.boolean().optional(),
  canVote: z.boolean().optional()
});

console.log('✅ Created Zod schema for Person validation');

// Query data for validation
const validationResults = await reasoningGraph.select(`
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name ?age ?email ?isAdult ?canVote WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
    OPTIONAL { ?person foaf:mbox ?email }
    OPTIONAL { ?person ex:isAdult ?isAdult }
    OPTIONAL { ?person ex:canVote ?canVote }
  }
`);

console.log(`   Retrieved ${validationResults.length} person records for validation`);

// Validate with Zod
const zod = useZod();
const zodValidation = await zod.validateResults(validationResults, PersonSchema);

console.log('\n✅ Zod Validation Results:');
console.log(`   Valid: ${zodValidation.valid}`);
console.log(`   Validated records: ${zodValidation.validated.length}`);

if (zodValidation.valid) {
  console.log('\n   Validated data:');
  zodValidation.validated.forEach((person, index) => {
    console.log(`     ${index + 1}. ${person.name} (age: ${person.age})`);
    if (person.email) console.log(`        Email: ${person.email}`);
    if (person.isAdult) console.log(`        Adult: ${person.isAdult}`);
    if (person.canVote) console.log(`        Can vote: ${person.canVote}`);
  });
} else {
  console.log('\n   Validation errors:');
  zodValidation.errors.forEach((error, index) => {
    console.log(`     ${index + 1}. ${error.message}`);
  });
}

// ============================================================================
// 4. Canonicalization
// ============================================================================

console.log('\n4. Canonicalization');
console.log('===================');

// Create two equivalent stores with different order
const store1 = useStore();
const store2 = useStore();
const terms = useTerms({ baseIRI: "http://example.org/" });

// Add quads in different order
const quad1 = terms.quad(
  terms.iri("person1"),
  terms.iri("name"),
  terms.lit("John")
);

const quad2 = terms.quad(
  terms.iri("person1"),
  terms.iri("age"),
  terms.lit(30)
);

const quad3 = terms.quad(
  terms.iri("person2"),
  terms.iri("name"),
  terms.lit("Jane")
);

// Store 1: quad1, quad2, quad3
store1.add(quad1);
store1.add(quad2);
store1.add(quad3);

// Store 2: quad3, quad1, quad2 (different order)
store2.add(quad3);
store2.add(quad1);
store2.add(quad2);

console.log('✅ Created two equivalent stores with different quad order');
console.log(`   Store 1 size: ${store1.size()}`);
console.log(`   Store 2 size: ${store2.size()}`);

// Canonicalize both stores
const canon = useCanon();
const canonical1 = await canon.canonicalize(store1);
const canonical2 = await canon.canonicalize(store2);

console.log('\n✅ Canonicalization Results:');
console.log(`   Store 1 canonical: ${canonical1.substring(0, 100)}...`);
console.log(`   Store 2 canonical: ${canonical2.substring(0, 100)}...`);

// Check if stores are isomorphic
const isIsomorphic = await canon.isIsomorphic(store1, store2);
console.log(`   Stores are isomorphic: ${isIsomorphic}`);

// Check if canonical forms are identical
const canonicalFormsIdentical = canonical1 === canonical2;
console.log(`   Canonical forms identical: ${canonicalFormsIdentical}`);

// ============================================================================
// 5. Data Quality Assessment
// ============================================================================

console.log('\n5. Data Quality Assessment');
console.log('===========================');

// Create a store with various data quality issues
const qualityDataTurtle = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  
  ex:person1 a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 ;
    foaf:mbox "john@example.org" .
    
  ex:person2 a foaf:Person ;
    foaf:name "Jane" ;  # Missing last name
    foaf:age 25 .
    # Missing email
    
  ex:person3 a foaf:Person ;
    foaf:name "Bob Wilson" ;
    foaf:age 200 ;  # Invalid age
    foaf:mbox "invalid-email" .  # Invalid email format
    
  ex:person4 a foaf:Person ;
    foaf:name "" ;  # Empty name
    foaf:age -5 ;  # Negative age
    foaf:mbox "bob@example.org" .
`;

const qualityStore = await turtle.parse(qualityDataTurtle);
const qualityGraph = useGraph(qualityStore);

console.log('✅ Created data with quality issues');
console.log(`   Quality store size: ${qualityStore.size()}`);

// Assess data quality
function assessDataQuality(store) {
  const stats = store.stats();
  const issues = [];
  
  // Check for missing names
  const persons = getSubjectsByType(store, "foaf:Person");
  const personsWithNames = persons.filter(p => {
    const name = getOne(store, p, "foaf:name");
    return name && name.value && name.value.trim() !== "";
  });
  
  if (personsWithNames.length < persons.length) {
    issues.push({
      type: "missing_name",
      count: persons.length - personsWithNames.length,
      message: "Persons missing or empty names"
    });
  }
  
  // Check for missing emails
  const personsWithEmails = persons.filter(p => {
    const email = getOne(store, p, "foaf:mbox");
    return email && email.value;
  });
  
  if (personsWithEmails.length < persons.length) {
    issues.push({
      type: "missing_email",
      count: persons.length - personsWithEmails.length,
      message: "Persons missing email addresses"
    });
  }
  
  // Check for invalid ages
  const personsWithInvalidAges = persons.filter(p => {
    const age = getOne(store, p, "foaf:age");
    if (!age || age.termType !== "Literal") return true;
    const ageValue = parseInt(age.value);
    return isNaN(ageValue) || ageValue < 0 || ageValue > 150;
  });
  
  if (personsWithInvalidAges.length > 0) {
    issues.push({
      type: "invalid_age",
      count: personsWithInvalidAges.length,
      message: "Persons with invalid ages"
    });
  }
  
  // Check for invalid emails
  const emailPattern = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
  const personsWithInvalidEmails = persons.filter(p => {
    const email = getOne(store, p, "foaf:mbox");
    if (!email || email.termType !== "Literal") return false;
    return !emailPattern.test(email.value);
  });
  
  if (personsWithInvalidEmails.length > 0) {
    issues.push({
      type: "invalid_email",
      count: personsWithInvalidEmails.length,
      message: "Persons with invalid email formats"
    });
  }
  
  return {
    totalPersons: persons.length,
    totalQuads: stats.quads,
    completeness: {
      names: personsWithNames.length / persons.length,
      emails: personsWithEmails.length / persons.length
    },
    issues
  };
}

// Helper functions (simplified versions)
function getSubjectsByType(store, type) {
  const results = [];
  for (const quad of store) {
    if (quad.predicate.value === "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" && 
        quad.object.value === type) {
      results.push(quad.subject);
    }
  }
  return results;
}

function getOne(store, subject, predicate) {
  for (const quad of store) {
    if (quad.subject.value === subject.value && quad.predicate.value === predicate) {
      return quad.object;
    }
  }
  return null;
}

const qualityAssessment = assessDataQuality(qualityStore);

console.log('\n✅ Data Quality Assessment:');
console.log(`   Total persons: ${qualityAssessment.totalPersons}`);
console.log(`   Total quads: ${qualityAssessment.totalQuads}`);
console.log(`   Name completeness: ${(qualityAssessment.completeness.names * 100).toFixed(1)}%`);
console.log(`   Email completeness: ${(qualityAssessment.completeness.emails * 100).toFixed(1)}%`);

console.log('\n   Quality issues:');
qualityAssessment.issues.forEach((issue, index) => {
  console.log(`     ${index + 1}. ${issue.message} (${issue.count} instances)`);
});

// ============================================================================
// 6. Combined Validation Pipeline
// ============================================================================

console.log('\n6. Combined Validation Pipeline');
console.log('=================================');

async function comprehensiveValidation(store) {
  const results = {
    shacl: null,
    zod: null,
    quality: null,
    overall: { valid: false, score: 0 }
  };
  
  try {
    // SHACL validation
    const shaclReport = await validator.validate(store, shapesStore);
    results.shacl = {
      conforms: shaclReport.conforms,
      violations: shaclReport.results?.length || 0
    };
  } catch (error) {
    results.shacl = { error: error.message };
  }
  
  try {
    // Zod validation
    const queryResults = await qualityGraph.select(`
      PREFIX ex: <http://example.org/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?person ?name ?age ?email WHERE {
        ?person foaf:name ?name ;
                foaf:age ?age .
        OPTIONAL { ?person foaf:mbox ?email }
      }
    `);
    
    const zodResult = await zod.validateResults(queryResults, PersonSchema);
    results.zod = {
      valid: zodResult.valid,
      errors: zodResult.errors?.length || 0
    };
  } catch (error) {
    results.zod = { error: error.message };
  }
  
  // Quality assessment
  results.quality = assessDataQuality(store);
  
  // Calculate overall score
  let score = 0;
  let maxScore = 0;
  
  // SHACL score (40% weight)
  maxScore += 40;
  if (results.shacl.conforms) score += 40;
  
  // Zod score (30% weight)
  maxScore += 30;
  if (results.zod.valid) score += 30;
  
  // Quality score (30% weight)
  maxScore += 30;
  const qualityScore = (results.quality.completeness.names + results.quality.completeness.emails) / 2 * 30;
  score += qualityScore;
  
  results.overall = {
    valid: results.shacl.conforms && results.zod.valid && results.quality.issues.length === 0,
    score: Math.round((score / maxScore) * 100)
  };
  
  return results;
}

const comprehensiveResults = await comprehensiveValidation(qualityStore);

console.log('✅ Comprehensive Validation Results:');
console.log(`   Overall valid: ${comprehensiveResults.overall.valid}`);
console.log(`   Overall score: ${comprehensiveResults.overall.score}%`);

console.log('\n   SHACL validation:');
if (comprehensiveResults.shacl.error) {
  console.log(`     Error: ${comprehensiveResults.shacl.error}`);
} else {
  console.log(`     Conforms: ${comprehensiveResults.shacl.conforms}`);
  console.log(`     Violations: ${comprehensiveResults.shacl.violations}`);
}

console.log('\n   Zod validation:');
if (comprehensiveResults.zod.error) {
  console.log(`     Error: ${comprehensiveResults.zod.error}`);
} else {
  console.log(`     Valid: ${comprehensiveResults.zod.valid}`);
  console.log(`     Errors: ${comprehensiveResults.zod.errors}`);
}

console.log('\n   Quality assessment:');
console.log(`     Name completeness: ${(comprehensiveResults.quality.completeness.names * 100).toFixed(1)}%`);
console.log(`     Email completeness: ${(comprehensiveResults.quality.completeness.emails * 100).toFixed(1)}%`);
console.log(`     Issues found: ${comprehensiveResults.quality.issues.length}`);

// ============================================================================
// Summary
// ============================================================================

console.log('\n🎉 Validation and Reasoning Examples Complete!');
console.log('==============================================');
console.log('✅ SHACL validation with custom shapes');
console.log('✅ EYE reasoning with custom rules');
console.log('✅ Zod runtime validation');
console.log('✅ Canonicalization and isomorphism checking');
console.log('✅ Data quality assessment');
console.log('✅ Comprehensive validation pipeline');
console.log('\n🚀 Ready for production use!');
