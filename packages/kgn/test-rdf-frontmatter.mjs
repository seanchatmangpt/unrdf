#!/usr/bin/env node
/**
 * Test RDF data in frontmatter
 * Adversarial PM: PROVE it works with EVIDENCE
 */

import { renderTemplate } from './src/utils/template-utils.js';
import { FrontmatterParser } from './src/parser/frontmatter.js';
import nunjucks from 'nunjucks';
import { expand, contract, rdfLiteral, sparql } from './src/filters/rdf.js';

// Test 1: Template with RDF data in frontmatter
const rdfTemplate = `---
name: "RDF Entity Generator"
prefixes:
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  foaf: "http://xmlns.com/foaf/0.1/"
  ex: "http://example.org/"
entities:
  - uri: "ex:Person1"
    type: "foaf:Person"
    name: "John Doe"
    email: "john@example.org"
  - uri: "ex:Person2"
    type: "foaf:Person"
    name: "Jane Smith"
    email: "jane@example.org"
---
# RDF Entities Generated from Frontmatter

{% for entity in entities %}
## Entity: {{ entity.name }}
- URI: {{ entity.uri | expand(prefixes) }}
- Type: {{ entity.type | expand(prefixes) }}
- Name: {{ entity.name | rdfLiteral }}
- Email: {{ entity.email | rdfLiteral }}
{% endfor %}

Total entities: {{ entities | length }}
`;

// Test 2: Template using RDF expansion
const rdfExpansionTemplate = `---
name: "RDF Expansion Demo"
prefixes:
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
  foaf: "http://xmlns.com/foaf/0.1/"
terms:
  - "rdf:type"
  - "rdfs:label"
  - "foaf:Person"
  - "foaf:name"
---
# RDF Term Expansion

Testing prefix expansion with frontmatter:

{% for term in terms %}
- {{ term }} → {{ term | expand(prefixes) }}
{% endfor %}

Total terms expanded: {{ terms | length }}
`;

async function runTests() {
  console.log('🧪 Testing RDF Frontmatter Integration\n');
  console.log('=' .repeat(60));

  try {
    // Set up nunjucks environment with RDF filters
    const env = new nunjucks.Environment(null, { autoescape: false });
    env.addFilter('expand', expand);
    env.addFilter('contract', contract);
    env.addFilter('rdfLiteral', rdfLiteral);
    env.addFilter('sparql', sparql);

    // Parse frontmatter
    const parser = new FrontmatterParser();

    // Test 1: RDF frontmatter parsing
    console.log('\n📋 Test 1: RDF Data in Frontmatter');
    console.log('-'.repeat(60));

    const parsed1 = parser.parse(rdfTemplate);
    console.log('✅ Frontmatter parsed successfully');
    console.log('📦 Frontmatter data:', JSON.stringify(parsed1.frontmatter, null, 2));

    const result1 = env.renderString(parsed1.content, parsed1.frontmatter);
    console.log('\n✅ Template rendered successfully\n');
    console.log(result1);

    // Verification checks
    const checks1 = [
      { test: 'Contains expanded URIs', pass: result1.includes('http://example.org/') },
      { test: 'Contains FOAF namespace', pass: result1.includes('http://xmlns.com/foaf/') },
      { test: 'Contains RDF literals', pass: result1.includes('"John Doe"') },
      { test: 'Shows entity count', pass: result1.includes('Total entities: 2') }
    ];

    console.log('\n🔍 Verification Checks:');
    checks1.forEach(({ test, pass }) => {
      console.log(`  ${pass ? '✅' : '❌'} ${test}`);
    });

    // Test 2: RDF prefix expansion with frontmatter
    console.log('\n\n📋 Test 2: RDF Prefix Expansion with Frontmatter');
    console.log('-'.repeat(60));

    const parsed2 = parser.parse(rdfExpansionTemplate);
    console.log('✅ Frontmatter parsed successfully');
    console.log('📦 Frontmatter data:', JSON.stringify(parsed2.frontmatter, null, 2));

    const result2 = env.renderString(parsed2.content, parsed2.frontmatter);
    console.log('\n✅ Template rendered successfully\n');
    console.log(result2);

    // Verification checks
    const checks2 = [
      { test: 'RDF prefixes from frontmatter', pass: result2.includes('rdf:type →') },
      { test: 'Terms expanded to full URIs', pass: result2.includes('http://www.w3.org/1999/02/22-rdf-syntax-ns#') },
      { test: 'FOAF namespace expanded', pass: result2.includes('http://xmlns.com/foaf/') },
      { test: 'Term count shown', pass: result2.includes('Total terms expanded: 4') }
    ];

    console.log('\n🔍 Verification Checks:');
    checks2.forEach(({ test, pass }) => {
      console.log(`  ${pass ? '✅' : '❌'} ${test}`);
    });

    // Overall verdict
    const allPassed = [...checks1, ...checks2].every(c => c.pass);

    console.log('\n' + '='.repeat(60));
    console.log(allPassed
      ? '✅ ALL TESTS PASSED - RDF frontmatter works!'
      : '❌ SOME TESTS FAILED - See details above'
    );
    console.log('='.repeat(60));

    process.exit(allPassed ? 0 : 1);

  } catch (error) {
    console.error('\n❌ TEST FAILED WITH ERROR:');
    console.error(error.message);
    console.error('\nStack trace:');
    console.error(error.stack);
    process.exit(1);
  }
}

runTests();
