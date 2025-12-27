#!/usr/bin/env node
/**
 * KGC Documentation Proof Appendix - Integration Example
 *
 * Demonstrates end-to-end usage of the proof appendix generator:
 * 1. Create receipts for document operations
 * 2. Build merkle tree from receipts
 * 3. Generate proof appendix
 * 4. Insert into markdown document
 * 5. Export as JSON for verification
 *
 * Usage:
 *   node examples/proof-appendix-demo.mjs
 *
 * @module @unrdf/fusion/examples/proof-appendix-demo
 */

import {
  createReceipt,
  chainReceipts,
  generateProofAppendix,
  insertProofAppendix,
  renderProofAsJSON,
  extractProofFromDocument,
  validateProofIntegrity,
} from '@unrdf/fusion';

// =============================================================================
// Example Document Generation Workflow
// =============================================================================

console.log('📝 KGC Documentation Proof Appendix Demo\n');

// Step 1: Simulate document rendering workflow
console.log('Step 1: Creating receipts for document operations...');

const receipts = [];

// Receipt 1: Query KGC for content
receipts.push(await createReceipt('query', {
  query: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
  resultCount: 42,
  executionTime: '125ms',
}, {
  receiptType: 'kgc',
}));

// Receipt 2: Extract data from universe
receipts.push(await createReceipt('extract', {
  entities: ['Entity:1', 'Entity:2', 'Entity:3'],
  relationships: ['rel:connects', 'rel:contains'],
}, {
  receiptType: 'kgc',
}));

// Receipt 3: Transform to markdown
receipts.push(await createReceipt('render', {
  format: 'markdown',
  sections: 5,
  codeBlocks: 3,
}, {
  receiptType: 'kgc',
}));

// Receipt 4: Validate output
receipts.push(await createReceipt('validation', {
  valid: true,
  errors: [],
  warnings: [],
}, {
  receiptType: 'kgc',
}));

console.log(`✅ Created ${receipts.length} receipts\n`);

// Step 2: Chain receipts into merkle tree
console.log('Step 2: Building merkle tree from receipts...');

const chainResult = await chainReceipts(receipts);

console.log(`✅ Merkle root: ${chainResult.root.slice(0, 16)}...`);
console.log(`✅ Tree valid: ${chainResult.valid}`);
console.log(`✅ Proof count: ${chainResult.proofs.length}\n`);

// Step 3: Generate proof appendix
console.log('Step 3: Generating proof appendix...');

// Simulate universe hash (would come from KGC freeze)
const o_hash = '0xabc123def456789abcdef0123456789abcdef0123456789abcdef0123456789';

// Convert proofs to expected format
const merkleProofs = {};
for (const proof of chainResult.proofs) {
  merkleProofs[proof.receiptId] = proof.merkleProof.proof;
}

const appendix = generateProofAppendix(
  receipts,
  chainResult.root,
  o_hash,
  {
    merkleProofs,
    frontmatter: {
      title: 'Example KGC Documentation',
      version: '1.0.0',
    },
  }
);

console.log(`✅ Generated proof appendix (${appendix.length} chars)\n`);

// Step 4: Insert into document
console.log('Step 4: Inserting proof appendix into document...');

const originalMarkdown = `# Example KGC Documentation

## Introduction

This is an example document generated from KGC universe data.

## Entities

- Entity:1 - Description
- Entity:2 - Description
- Entity:3 - Description

## Relationships

The entities are connected via relationships:
- \`rel:connects\`: Links entities together
- \`rel:contains\`: Hierarchical containment

## References

1. KGC-4D Documentation
2. UNRDF Specification
`;

const documentWithProof = insertProofAppendix(
  originalMarkdown,
  receipts,
  chainResult.root,
  o_hash,
  {
    merkleProofs,
    frontmatter: {
      title: 'Example KGC Documentation',
      version: '1.0.0',
    },
  }
);

console.log(`✅ Document size: ${originalMarkdown.length} → ${documentWithProof.length} chars\n`);

// Step 5: Export as JSON for verification
console.log('Step 5: Exporting proof as JSON...');

const proofJSON = renderProofAsJSON(
  receipts,
  chainResult.root,
  o_hash,
  {
    merkleProofs,
    frontmatter: {
      title: 'Example KGC Documentation',
      version: '1.0.0',
    },
  }
);

console.log('✅ JSON proof structure:');
console.log(`   - Version: ${proofJSON.version}`);
console.log(`   - Type: ${proofJSON.type}`);
console.log(`   - Receipts: ${proofJSON.receipts.length}`);
console.log(`   - Tree depth: ${proofJSON.metadata.treeDepth}`);
console.log(`   - Document: ${proofJSON.metadata.documentTitle}\n`);

// Step 6: Extract and validate proof
console.log('Step 6: Extracting and validating proof...');

const extractedProof = extractProofFromDocument(documentWithProof);

if (extractedProof) {
  console.log('✅ Successfully extracted proof from document');
  console.log(`   - Universe hash: ${extractedProof.o_hash.slice(0, 16)}...`);
  console.log(`   - Merkle root: ${extractedProof.merkleRoot.slice(0, 16)}...`);
  console.log(`   - Receipt count: ${extractedProof.receiptIds.length}`);
} else {
  console.log('❌ Failed to extract proof');
}

const validation = validateProofIntegrity(proofJSON);
console.log(`\n✅ Proof validation: ${validation.valid ? 'PASSED' : 'FAILED'}`);
if (!validation.valid) {
  console.log(`   Errors: ${validation.errors.join(', ')}`);
}

// Step 7: Display sample proof appendix
console.log('\n' + '='.repeat(80));
console.log('SAMPLE PROOF APPENDIX OUTPUT');
console.log('='.repeat(80));
console.log(appendix.split('\n').slice(0, 40).join('\n'));
console.log('\n... (truncated for demo) ...\n');

// Step 8: Summary
console.log('='.repeat(80));
console.log('SUMMARY');
console.log('='.repeat(80));
console.log(`
✅ Generated ${receipts.length} receipts for document operations
✅ Built merkle tree with root: ${chainResult.root.slice(0, 16)}...
✅ Created verification appendix (${appendix.length} chars)
✅ Inserted into document (${documentWithProof.length} total chars)
✅ Exported JSON proof for programmatic verification
✅ Validated proof integrity: ${validation.valid ? 'PASSED' : 'FAILED'}

🎯 Next Steps:
   1. Integrate with kgc-docs.mjs build pipeline
   2. Add to /kgc:prove command
   3. Enable automated periodic re-verification
   4. Publish proofs to IPFS/blockchain for permanence

📚 Documentation:
   - See packages/fusion/src/kgc-docs-proof-appendix.mjs for API
   - See packages/fusion/test/kgc-docs-proof-appendix.test.mjs for examples
`);

console.log('✅ Demo complete!\n');
