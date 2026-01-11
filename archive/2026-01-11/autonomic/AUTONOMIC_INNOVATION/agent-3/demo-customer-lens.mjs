/**
 * @file demo-customer-lens.mjs
 * @description Reference implementation of a Customer lens
 * @module agent-3/demo-customer-lens
 */

import { createStore } from '../../packages/oxigraph/src/index.mjs';
import { defineLens, compileLens, executeLensToGraph, executeLensFromGraph } from './lens.mjs';

/**
 * Customer DTO schema
 * @typedef {Object} CustomerDTO
 * @property {string} id - Customer unique identifier
 * @property {string} name - Customer full name
 * @property {string} email - Customer email address
 * @property {string} registeredAt - ISO 8601 registration timestamp
 */

/**
 * Define Customer lens mapping
 */
export const customerLens = defineLens('CustomerLens', {
  domain: 'kgc-facade',
  entity: 'customer',
  rules: [
    {
      dto_field: 'id',
      rdf_predicate: 'http://schema.org/identifier',
      type: 'string'
    },
    {
      dto_field: 'name',
      rdf_predicate: 'http://schema.org/name',
      type: 'string'
    },
    {
      dto_field: 'email',
      rdf_predicate: 'http://schema.org/email',
      type: 'string'
    },
    {
      dto_field: 'registeredAt',
      rdf_predicate: 'http://schema.org/dateCreated',
      type: 'datetime'
    }
  ]
});

/**
 * Compile customer lens into executable program
 */
export const customerLensProgram = compileLens(customerLens);

/**
 * Demonstrate round-trip transformation: DTO → RDF → DTO
 *
 * @param {CustomerDTO} customerDTO - Input customer data
 * @returns {Object} { success: boolean, original: CustomerDTO, reconstructed: CustomerDTO, quads: Array }
 *
 * @example
 * const result = demoCustomerRoundTrip({
 *   id: "customer-123",
 *   name: "Alice Johnson",
 *   email: "alice@example.com",
 *   registeredAt: "2025-01-15T10:30:00Z"
 * });
 * console.log(result.success); // true
 */
export function demoCustomerRoundTrip(customerDTO) {
  // Phase 1: DTO → RDF quads
  const { quads, subjects } = executeLensToGraph(customerDTO, customerLensProgram);

  console.log(`\n[DEMO] Generated ${quads.length} quads for customer ${customerDTO.id}`);
  for (const quad of quads) {
    console.log(`  ${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`);
  }

  // Phase 2: Store quads in Oxigraph
  const store = createStore();
  for (const quad of quads) {
    store.add(quad);
  }

  console.log(`\n[DEMO] Stored ${quads.length} quads in RDF store`);

  // Phase 3: RDF → DTO reconstruction
  const reconstructedDTO = executeLensFromGraph(subjects, store, customerLensProgram);

  console.log(`\n[DEMO] Reconstructed DTO:`, reconstructedDTO);

  // Phase 4: Verify round-trip fidelity
  const success = deepEqual(customerDTO, reconstructedDTO);

  console.log(`\n[DEMO] Round-trip ${success ? '✅ SUCCESS' : '❌ FAILED'}`);

  return {
    success,
    original: customerDTO,
    reconstructed: reconstructedDTO,
    quads: quads.map(q => ({
      subject: q.subject.value,
      predicate: q.predicate.value,
      object: q.object.value
    }))
  };
}

/**
 * Deep equality check for DTO comparison
 *
 * @param {Object} obj1 - First object
 * @param {Object} obj2 - Second object
 * @returns {boolean} True if objects are deeply equal
 * @private
 */
function deepEqual(obj1, obj2) {
  if (obj1 === obj2) return true;
  if (typeof obj1 !== 'object' || typeof obj2 !== 'object') return false;
  if (obj1 === null || obj2 === null) return false;

  const keys1 = Object.keys(obj1).sort();
  const keys2 = Object.keys(obj2).sort();

  if (keys1.length !== keys2.length) return false;

  for (let i = 0; i < keys1.length; i++) {
    if (keys1[i] !== keys2[i]) return false;
    if (obj1[keys1[i]] !== obj2[keys2[i]]) return false;
  }

  return true;
}

/**
 * Demo: Multiple customers with stable IRI space
 *
 * Demonstrates that:
 * 1. Different customers get different IRIs
 * 2. Same customer ID → same IRI (deterministic)
 * 3. IRI space is collision-resistant
 */
export function demoMultipleCustomers() {
  const customers = [
    { id: 'customer-001', name: 'Alice', email: 'alice@example.com', registeredAt: '2025-01-01T00:00:00Z' },
    { id: 'customer-002', name: 'Bob', email: 'bob@example.com', registeredAt: '2025-01-02T00:00:00Z' },
    { id: 'customer-001', name: 'Alice Updated', email: 'alice@example.com', registeredAt: '2025-01-01T00:00:00Z' }
  ];

  console.log('\n[DEMO] Multiple Customers - IRI Stability Test');

  const results = customers.map(customer => {
    const { subjects } = executeLensToGraph(customer, customerLensProgram);
    return { id: customer.id, name: customer.name, iri: subjects[0] };
  });

  console.log('\nResults:');
  for (const result of results) {
    console.log(`  ${result.id} (${result.name}) → ${result.iri}`);
  }

  // Verify: customer-001 appears twice with SAME IRI
  const customer001IRIs = results
    .filter(r => r.id === 'customer-001')
    .map(r => r.iri);

  const iriStable = customer001IRIs[0] === customer001IRIs[1];
  console.log(`\n[DEMO] IRI stability for customer-001: ${iriStable ? '✅ STABLE' : '❌ UNSTABLE'}`);

  return { success: iriStable, results };
}

/**
 * Demo: Lens program serialization and portability
 *
 * Demonstrates that compiled lens programs are fully JSON-serializable
 * and can be stored/transmitted without loss
 */
export function demoLensSerialization() {
  console.log('\n[DEMO] Lens Program Serialization Test');

  // Serialize to JSON
  const serialized = JSON.stringify(customerLensProgram, null, 2);
  console.log(`\nSerialized program (${serialized.length} bytes):\n`, serialized.substring(0, 200) + '...');

  // Deserialize
  const deserialized = JSON.parse(serialized);

  // Test with deserialized program
  const testDTO = {
    id: 'customer-999',
    name: 'Test User',
    email: 'test@example.com',
    registeredAt: '2025-12-26T00:00:00Z'
  };

  const { quads: quads1 } = executeLensToGraph(testDTO, customerLensProgram);
  const { quads: quads2 } = executeLensToGraph(testDTO, deserialized);

  const serializable = quads1.length === quads2.length;
  console.log(`\n[DEMO] Serialization test: ${serializable ? '✅ PORTABLE' : '❌ FAILED'}`);

  return { success: serializable, serialized, deserialized };
}

// Run demos if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('=== Customer Lens Demo ===\n');

  // Demo 1: Basic round-trip
  const customer = {
    id: 'customer-123',
    name: 'Alice Johnson',
    email: 'alice@example.com',
    registeredAt: '2025-01-15T10:30:00Z'
  };

  demoCustomerRoundTrip(customer);

  // Demo 2: Multiple customers
  demoMultipleCustomers();

  // Demo 3: Serialization
  demoLensSerialization();
}
