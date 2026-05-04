/**
 * @file SHACL-to-SPARQL-to-BEAM Demo
 */

import { createSHACLCompiler } from '../src/shacl-compiler.mjs';

async function runDemo() {
  console.log('🚀 Starting SHACL-to-SPARQL-to-BEAM Demo\n');

  const compiler = createSHACLCompiler();

  // Define a sample SHACL shape (simplified JS representation)
  const personShape = {
    id: 'http://example.org/PersonShape',
    targetClass: 'http://xmlns.com/foaf/0.1/Person',
    properties: [
      {
        path: 'http://xmlns.com/foaf/0.1/name',
        datatype: 'http://www.w3.org/2001/XMLSchema#string',
        minCount: 1
      },
      {
        path: 'http://xmlns.com/foaf/0.1/age',
        datatype: 'http://www.w3.org/2001/XMLSchema#integer'
      }
    ]
  };

  console.log('1️⃣  SHACL Shape (Abstract):');
  console.log(JSON.stringify(personShape, null, 2));
  console.log();

  const sparql = compiler.transpileToSPARQL(personShape);
  console.log('2️⃣  Transpiled to SPARQL:');
  console.log(sparql);
  console.log();

  const beam = compiler.compile(personShape);
  console.log('3️⃣  Compiled to BEAM List Comprehension:');
  console.log(beam);
  console.log();

  const erlangModule = compiler.generateErlangModule('person_validator', personShape);
  console.log('4️⃣  Generated Erlang Module:');
  console.log('----------------------------------------');
  console.log(erlangModule);
  console.log('----------------------------------------');

  console.log('\n✅ Demo Complete!');
}

runDemo().catch(console.error);
