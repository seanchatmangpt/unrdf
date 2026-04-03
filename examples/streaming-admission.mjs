/**
 * @file Streaming Admission Example
 * @description Demonstrates streaming RDF delta validation with receipt chaining
 *
 * This example shows:
 * 1. Creating a streaming admission handler
 * 2. Admitting deltas with automatic receipt generation
 * 3. Verifying receipt chain integrity
 * 4. Using delta conditions to validate changes
 */

import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';
import { createStreamingAdmission } from '../packages/streaming/src/stream-admit.mjs';

const { namedNode, literal } = dataFactory;

/**
 * Helper to create RDF quads from simple notation
 */
function quad(s, p, o) {
  return {
    subject: namedNode(s),
    predicate: namedNode(p),
    object: (o && o.startsWith('http')) ? namedNode(o) : literal(o),
  };
}

/**
 * Example 1: Basic streaming admission with no conditions
 */
async function example1BasicAdmission() {
  console.log('\n=== Example 1: Basic Streaming Admission ===\n');

  const store = createStore();
  const admin = createStreamingAdmission(store, {
    nodeId: 'example-node-1',
  });

  // Define some deltas
  const delta1 = {
    additions: [
      quad('https://example.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice'),
    ],
    removals: [],
  };

  const delta2 = {
    additions: [
      quad('https://example.org/bob', 'http://xmlns.com/foaf/0.1/name', 'Bob'),
    ],
    removals: [],
  };

  // Admit deltas
  console.log('Admitting delta 1...');
  const { receipt: receipt1, admitted: admitted1 } = await admin.admit(delta1);
  console.log(`✓ Delta 1 admitted: ${admitted1}`);
  console.log(`  Receipt ID: ${receipt1.id}`);
  console.log(`  Input hash: ${receipt1.inputHash.slice(0, 16)}...`);
  console.log(`  Output hash: ${receipt1.outputHash.slice(0, 16)}...`);
  console.log(`  Quad count: ${receipt1.quadCountBefore} → ${receipt1.quadCountAfter}`);

  console.log('\nAdmitting delta 2...');
  const { receipt: receipt2, admitted: admitted2 } = await admin.admit(delta2);
  console.log(`✓ Delta 2 admitted: ${admitted2}`);
  console.log(`  Receipt ID: ${receipt2.id}`);
  console.log(`  Previous receipt hash: ${receipt2.previousReceiptHash.slice(0, 16)}...`);
  console.log(`  Receipt hash: ${receipt2.receiptHash.slice(0, 16)}...`);

  // Verify chain
  const verification = admin.verifyChain(true);
  console.log(`\n✓ Chain verification: ${verification.valid}`);
  console.log(`  Chain length: ${verification.chain.length}`);
  console.log(`  Mismatches: ${verification.mismatches.length}`);

  // Show stats
  const stats = admin.getStats();
  console.log(`\nAdmission statistics:`);
  console.log(`  Total: ${stats.total}`);
  console.log(`  Admitted: ${stats.admitted}`);
  console.log(`  Rejected: ${stats.rejected}`);
  console.log(`  Current head hash: ${stats.currentHeadHash.slice(0, 16)}...`);
}

/**
 * Example 2: Determinism verification - same delta produces same hashes
 */
async function example2Determinism() {
  console.log('\n=== Example 2: Determinism Verification ===\n');

  const delta = {
    additions: [
      quad('https://example.org/subject', 'http://example.org/property', 'value'),
    ],
    removals: [],
  };

  console.log('Processing same delta 5 times...\n');

  const hashes = [];
  for (let i = 0; i < 5; i++) {
    const store = createStore();
    const admin = createStreamingAdmission(store);
    const { receipt } = await admin.admit(delta);

    hashes.push({
      iteration: i + 1,
      deltaHash: receipt.deltaHash,
      outputHash: receipt.outputHash,
      inputHash: receipt.inputHash,
    });

    console.log(`Iteration ${i + 1}:`);
    console.log(`  Delta hash:  ${receipt.deltaHash.slice(0, 16)}...`);
    console.log(`  Input hash:  ${receipt.inputHash.slice(0, 16)}...`);
    console.log(`  Output hash: ${receipt.outputHash.slice(0, 16)}...`);
  }

  // Check determinism
  const allSameDelta = hashes.every(h => h.deltaHash === hashes[0].deltaHash);
  const allSameInput = hashes.every(h => h.inputHash === hashes[0].inputHash);
  const allSameOutput = hashes.every(h => h.outputHash === hashes[0].outputHash);

  console.log(`\n✓ Determinism check:`);
  console.log(`  Delta hashes identical: ${allSameDelta}`);
  console.log(`  Input hashes identical: ${allSameInput}`);
  console.log(`  Output hashes identical: ${allSameOutput}`);
}

/**
 * Example 3: Receipt chaining with multiple deltas
 */
async function example3ReceiptChaining() {
  console.log('\n=== Example 3: Receipt Chaining ===\n');

  const store = createStore();
  const admin = createStreamingAdmission(store);

  console.log('Building receipt chain with 3 deltas...\n');

  const deltas = [
    {
      additions: [
        quad('https://example.org/dataset1', 'http://www.w3.org/2000/01/rdf-schema#label', 'Dataset 1'),
      ],
      removals: [],
    },
    {
      additions: [
        quad('https://example.org/dataset1', 'http://www.w3.org/2000/01/rdf-schema#comment', 'First dataset'),
      ],
      removals: [],
    },
    {
      additions: [
        quad('https://example.org/dataset2', 'http://www.w3.org/2000/01/rdf-schema#label', 'Dataset 2'),
      ],
      removals: [],
    },
  ];

  const receipts = [];
  for (let i = 0; i < deltas.length; i++) {
    const { receipt } = await admin.admit(deltas[i]);
    receipts.push(receipt);

    console.log(`Delta ${i + 1}:`);
    console.log(`  Receipt ID: ${receipt.id}`);
    console.log(`  Status: ${receipt.status}`);
    if (receipt.previousReceiptHash) {
      console.log(`  Links to: ${receipt.previousReceiptHash.slice(0, 16)}...`);
    } else {
      console.log(`  (Genesis receipt)`);
    }
    console.log(`  Hash: ${receipt.receiptHash.slice(0, 16)}...`);
    console.log();
  }

  // Verify chain integrity
  const verification = admin.verifyChain(false);
  console.log(`✓ Chain integrity verification: ${verification.valid}`);
  console.log(`  Total receipts: ${verification.chain}`);

  // Trace the chain
  console.log(`\nChain lineage (head to genesis):`);
  for (let i = receipts.length - 1; i >= 0; i--) {
    const receipt = receipts[i];
    console.log(`  ${receipt.id}`);
    if (i > 0) {
      console.log(`    ↑ links to`);
    }
  }
}

/**
 * Example 4: Conditional admission with hash validation
 */
async function example4ConditionalAdmission() {
  console.log('\n=== Example 4: Conditional Admission ===\n');

  // First, compute the expected output hash
  const store1 = createStore();
  const admin1 = createStreamingAdmission(store1);

  const delta = {
    additions: [
      quad('https://example.org/product', 'http://www.w3.org/2000/01/rdf-schema#label', 'Product A'),
    ],
    removals: [],
  };

  const { receipt: expectedReceipt } = await admin1.admit(delta);
  console.log(`Expected output hash: ${expectedReceipt.outputHash}\n`);

  // Now try to admit with condition that validates the hash
  console.log('Attempting admission with correct hash condition...');
  const store2 = createStore();
  const admin2 = createStreamingAdmission(store2, {
    condition: {
      kind: 'delta',
      hash: expectedReceipt.outputHash,
    },
  });

  const { admitted: admitted1, receipt: receipt1 } = await admin2.admit(delta);
  console.log(`✓ Admission result: ${admitted1}`);
  console.log(`  Status: ${receipt1.status}`);
  console.log(`  Output hash: ${receipt1.outputHash.slice(0, 16)}...\n`);

  // Now try with wrong hash
  console.log('Attempting admission with incorrect hash condition...');
  const store3 = createStore();
  const admin3 = createStreamingAdmission(store3, {
    condition: {
      kind: 'delta',
      hash: 'badbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadb',
    },
  });

  const { admitted: admitted2, receipt: receipt2 } = await admin3.admit(delta);
  console.log(`✓ Admission result: ${admitted2}`);
  console.log(`  Status: ${receipt2.status}`);
  console.log(`  Rejection reason: ${receipt2.rejectionReason}`);
}

/**
 * Example 5: Stream processing multiple deltas
 */
async function example5StreamProcessing() {
  console.log('\n=== Example 5: Stream Processing ===\n');

  const store = createStore();
  const admin = createStreamingAdmission(store, {
    nodeId: 'stream-processor',
  });

  const deltas = [
    {
      additions: [
        quad('https://example.org/article1', 'http://purl.org/dc/terms/title', 'Article 1'),
      ],
      removals: [],
    },
    {
      additions: [
        quad('https://example.org/article2', 'http://purl.org/dc/terms/title', 'Article 2'),
      ],
      removals: [],
    },
    {
      additions: [
        quad('https://example.org/article3', 'http://purl.org/dc/terms/title', 'Article 3'),
      ],
      removals: [],
    },
  ];

  console.log(`Processing stream of ${deltas.length} deltas...\n`);

  const results = await admin.admitStream(deltas);

  console.log('Results:');
  results.forEach((result, idx) => {
    console.log(`  Delta ${idx + 1}: ${result.admitted ? '✓ Admitted' : '✗ Rejected'}`);
    console.log(`    Receipt: ${result.receipt.id}`);
  });

  const stats = admin.getStats();
  console.log(`\nStream statistics:`);
  console.log(`  Total processed: ${stats.total}`);
  console.log(`  Admitted: ${stats.admitted}`);
  console.log(`  Rejected: ${stats.rejected}`);
}

/**
 * Main execution
 */
async function main() {
  try {
    await example1BasicAdmission();
    await example2Determinism();
    await example3ReceiptChaining();
    await example4ConditionalAdmission();
    await example5StreamProcessing();

    console.log('\n=== All examples completed successfully ===\n');
  } catch (error) {
    console.error('Error running examples:', error.message);
    process.exit(1);
  }
}

main();
