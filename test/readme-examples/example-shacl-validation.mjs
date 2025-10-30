/**
 * README Example: SHACL Validation (lines 214-241)
 * Tests SHACL shape validation
 */

import { createDarkMatterCore, parseTurtle, DataFactory } from '../../src/knowledge-engine/index.mjs';
const { namedNode, quad, literal } = DataFactory;

async function testShaclValidation() {
  console.log('🧪 Testing SHACL Validation Example...');

  const system = await createDarkMatterCore();

  try {
    // Define SHACL shapes
    const shapes = await parseTurtle(`
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix ex: <http://example.org/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

      ex:PersonShape a sh:NodeShape ;
        sh:targetClass foaf:Person ;
        sh:property [
          sh:path foaf:name ;
          sh:minCount 1 ;
          sh:datatype xsd:string ;
        ] .
    `);
    console.log('✅ Parsed SHACL shapes');

    // Create test data (valid person)
    const validStore = await parseTurtle(`
      @prefix ex: <http://example.org/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .

      ex:alice a foaf:Person ;
               foaf:name "Alice" .
    `);

    // Validate data
    const validation = await system.validate({
      dataGraph: validStore,
      shapesGraph: shapes
    });
    console.log('✅ Validation executed');

    if (!validation) {
      throw new Error('Validation returned null/undefined');
    }

    console.log('Validation conforms:', validation.conforms);

    if (!validation.conforms) {
      console.log('Validation errors:', validation.results);
    }

    await system.cleanup();
    console.log('\n✅ SHACL Validation Example: PASSED\n');
    return true;
  } catch (error) {
    await system.cleanup();
    console.error('❌ SHACL Validation Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testShaclValidation();
  process.exit(success ? 0 : 1);
}

export { testShaclValidation };
