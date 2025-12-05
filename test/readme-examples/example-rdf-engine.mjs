/**
 * README Example: RDF Knowledge Engine (lines 114-131)
 * Tests parsing and serialization
 */

import { parseTurtle, toJsonLd, toNQuads } from '../../packages/knowledge-engine/index.mjs';

/**
 *
 */
async function testRdfEngine() {
  console.log('🧪 Testing RDF Engine Example...');

  try {
    // Parse Turtle
    const store = await parseTurtle(`
      @prefix ex: <http://example.org/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .

      ex:alice foaf:name "Alice" ;
               foaf:knows ex:bob .
    `);
    console.log('✅ Parsed Turtle');

    // Validate store has quads
    const quadCount = store.size;
    if (quadCount === 0) {
      throw new Error('Store is empty after parsing');
    }
    console.log(`✅ Store has ${quadCount} quads`);

    // Convert to JSON-LD
    const jsonld = await toJsonLd(store);
    console.log('✅ Converted to JSON-LD');

    if (!jsonld || typeof jsonld !== 'string') {
      throw new Error('JSON-LD conversion failed');
    }

    // Convert to N-Quads
    const nquads = await toNQuads(store);
    console.log('✅ Converted to N-Quads');

    if (!nquads || typeof nquads !== 'string') {
      throw new Error('N-Quads conversion failed');
    }

    // Validate content
    if (!nquads.includes('Alice')) {
      throw new Error('N-Quads missing expected content');
    }

    console.log('\n✅ RDF Engine Example: PASSED\n');
    return true;
  } catch (error) {
    console.error('❌ RDF Engine Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testRdfEngine();
  process.exit(success ? 0 : 1);
}

export { testRdfEngine };
