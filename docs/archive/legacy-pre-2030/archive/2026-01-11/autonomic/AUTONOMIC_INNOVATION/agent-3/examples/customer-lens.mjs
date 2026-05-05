/**
 * @fileoverview Customer domain lens demonstration
 */

import { defineLens, compileLens, executeLensToGraph, executeLensFromGraph } from '../src/index.mjs';
import { createStore } from '@unrdf/oxigraph';

/**
 * Create a customer lens for transforming customer API payloads to RDF
 */
export function createCustomerLens() {
  const profile = {
    namespace: 'https://example.org/',
    prefixes: {
      schema: 'http://schema.org/',
      xsd: 'http://www.w3.org/2001/XMLSchema#',
      foaf: 'http://xmlns.com/foaf/0.1/'
    },
    conventions: {
      idField: 'id',
      case: 'camelCase'
    }
  };

  const mappings = {
    Customer: {
      subject: {
        pattern: '{namespace}Customer/{id}',
        keys: ['id']
      },
      type: 'schema:Customer',
      predicates: {
        name: {
          iri: 'schema:name',
          datatype: 'xsd:string',
          required: true
        },
        email: {
          iri: 'schema:email',
          datatype: 'xsd:string',
          required: true
        },
        telephone: {
          iri: 'schema:telephone',
          datatype: 'xsd:string',
          required: false
        },
        birthDate: {
          iri: 'schema:birthDate',
          datatype: 'xsd:date',
          required: false
        },
        address: {
          iri: 'schema:address',
          datatype: 'xsd:string',
          required: false
        }
      }
    },
    Address: {
      subject: {
        pattern: '{namespace}Address/{id}',
        keys: ['id'],
        skolemPattern: 'urn:skolem:address:{hash}'
      },
      type: 'schema:PostalAddress',
      predicates: {
        streetAddress: {
          iri: 'schema:streetAddress',
          datatype: 'xsd:string',
          required: true
        },
        addressLocality: {
          iri: 'schema:addressLocality',
          datatype: 'xsd:string',
          required: true
        },
        addressRegion: {
          iri: 'schema:addressRegion',
          datatype: 'xsd:string',
          required: false
        },
        postalCode: {
          iri: 'schema:postalCode',
          datatype: 'xsd:string',
          required: false
        },
        addressCountry: {
          iri: 'schema:addressCountry',
          datatype: 'xsd:string',
          required: false
        }
      }
    }
  };

  return defineLens('customer-v1', profile, mappings);
}

/**
 * Example usage demonstration
 */
export function demonstrateCustomerLens() {
  console.log('=== Customer Lens Demonstration ===\n');

  // 1. Define and compile lens
  console.log('1. Creating customer lens...');
  const lens = createCustomerLens();
  console.log('   ✓ Lens defined:', lens.id);

  console.log('\n2. Compiling lens...');
  const compiled = compileLens(lens);
  console.log('   ✓ Lens compiled');
  console.log('   ✓ Canonical hash:', compiled.canonicalHash);

  // 2. Create example payload
  console.log('\n3. Example customer payload:');
  const customer = {
    id: 'cust-12345',
    name: 'Alice Johnson',
    email: 'alice.johnson@example.com',
    telephone: '+1-555-0100',
    birthDate: '1990-05-15',
    address: '123 Main St, Springfield, IL 62701'
  };
  console.log('   ', JSON.stringify(customer, null, 2));

  // 3. Transform to RDF
  console.log('\n4. Transforming to RDF graph...');
  const store = createStore();
  const quads = executeLensToGraph(customer, compiled, store, 'Customer');
  console.log(`   ✓ Generated ${quads.length} quads`);

  console.log('\n5. Generated triples (N-Triples format):');
  for (const quad of quads) {
    const subject = `<${quad.subject.value}>`;
    const predicate = `<${quad.predicate.value}>`;
    let object;
    if (quad.object.termType === 'Literal') {
      const dt = quad.object.datatype ? `^^<${quad.object.datatype.value}>` : '';
      object = `"${quad.object.value}"${dt}`;
    } else {
      object = `<${quad.object.value}>`;
    }
    console.log(`   ${subject} ${predicate} ${object} .`);
  }

  // 4. Transform back to payload
  console.log('\n6. Transforming back to payload...');
  const reconstructed = executeLensFromGraph(
    quads[0].subject.value,
    compiled,
    store,
    'Customer'
  );
  console.log('   ✓ Payload reconstructed');
  console.log('   ', JSON.stringify(reconstructed, null, 2));

  // 5. Verify round-trip
  console.log('\n7. Verifying round-trip...');
  const isLossless = JSON.stringify(customer) === JSON.stringify(reconstructed);
  console.log(`   ${isLossless ? '✓' : '✗'} Round-trip is ${isLossless ? 'LOSSLESS' : 'LOSSY'}`);

  // 6. Demonstrate determinism
  console.log('\n8. Testing determinism (10 iterations)...');
  const hashes = new Set();
  const iris = new Set();

  for (let i = 0; i < 10; i++) {
    const c = compileLens(lens);
    hashes.add(c.canonicalHash);

    const s = createStore();
    const q = executeLensToGraph(customer, c, s, 'Customer');
    iris.add(q[0].subject.value);
  }

  console.log(`   ✓ Unique hashes: ${hashes.size} (expected: 1)`);
  console.log(`   ✓ Unique IRIs: ${iris.size} (expected: 1)`);
  console.log(`   ${hashes.size === 1 && iris.size === 1 ? '✓' : '✗'} Determinism verified`);

  console.log('\n=== Demonstration Complete ===\n');

  return {
    lens,
    compiled,
    customer,
    quads,
    reconstructed,
    isDeterministic: hashes.size === 1 && iris.size === 1,
    isLossless
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  demonstrateCustomerLens();
}
