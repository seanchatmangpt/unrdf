/**
 * Tests for KGC Documentation Proof Appendix Generator
 *
 * Validates:
 * - Proof appendix generation
 * - Markdown formatting correctness
 * - Receipt table generation
 * - Merkle tree visualization
 * - Verification instructions
 * - Hash value formatting
 * - JSON export/import
 * - Document insertion logic
 *
 * @module @unrdf/fusion/test/kgc-docs-proof-appendix
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert/strict';
import {
  generateProofAppendix,
  formatReceiptTable,
  formatMerkleTree,
  formatVerificationInstructions,
  formatHashValues,
  insertProofAppendix,
  renderProofAsJSON,
  updateProofTimestamp,
  extractProofFromDocument,
  computeDocumentHash,
  validateProofIntegrity,
} from '../src/kgc-docs-proof-appendix.mjs';

// =============================================================================
// Test Fixtures
// =============================================================================

/**
 * Create mock receipt
 */
function createMockReceipt(id, eventType, timestamp = '1000000000') {
  return {
    id: `receipt-${eventType}-${id}`,
    hash: `abc123def456${id}`.padEnd(64, '0'),
    timestamp,
    timestamp_iso: '2025-12-26T12:00:00.000Z',
    eventType,
    payload: {
      value: 42,
      decision: 'allow',
    },
    receiptType: 'kgc',
  };
}

/**
 * Create mock merkle proofs
 */
function createMockMerkleProofs(receiptCount) {
  const proofs = {};
  for (let i = 1; i <= receiptCount; i++) {
    const receiptId = `receipt-test-${i}`;
    proofs[receiptId] = [
      '0x1111111111111111111111111111111111111111111111111111111111111111',
      '0x2222222222222222222222222222222222222222222222222222222222222222',
    ];
  }
  return proofs;
}

// =============================================================================
// Tests: generateProofAppendix
// =============================================================================

describe('generateProofAppendix', () => {
  it('should generate complete proof appendix', () => {
    const receipts = [
      createMockReceipt(1, 'query', '1000000000'),
      createMockReceipt(2, 'proof', '2000000000'),
      createMockReceipt(3, 'extract', '3000000000'),
    ];
    const merkleRoot = '0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890';
    const o_hash = '0x9876543210fedcba9876543210fedcba9876543210fedcba9876543210fedcba';

    const appendix = generateProofAppendix(receipts, merkleRoot, o_hash);

    // Verify structure
    assert.ok(appendix.includes('## Verification Appendix'));
    assert.ok(appendix.includes('### Receipt Log'));
    assert.ok(appendix.includes('### Merkle Tree Structure'));
    assert.ok(appendix.includes('### Hash Values'));
    assert.ok(appendix.includes('### Verification Instructions'));

    // Verify content
    assert.ok(appendix.includes('receipt-query-1'));
    assert.ok(appendix.includes('receipt-proof-2'));
    assert.ok(appendix.includes('receipt-extract-3'));
    assert.ok(appendix.includes(merkleRoot));
    assert.ok(appendix.includes(o_hash));
  });

  it('should throw on invalid inputs', () => {
    const merkleRoot = '0xabc123';
    const o_hash = '0xdef456';

    assert.throws(() => {
      generateProofAppendix([], merkleRoot, o_hash);
    }, /receipts must be a non-empty array/);

    assert.throws(() => {
      generateProofAppendix([createMockReceipt(1, 'test')], '', o_hash);
    }, /merkleRoot must be a string/);

    assert.throws(() => {
      generateProofAppendix([createMockReceipt(1, 'test')], merkleRoot, '');
    }, /o_hash must be a string/);
  });

  it('should sort receipts by timestamp', () => {
    const receipts = [
      createMockReceipt(1, 'third', '3000000000'),
      createMockReceipt(2, 'first', '1000000000'),
      createMockReceipt(3, 'second', '2000000000'),
    ];
    const merkleRoot = '0xabc123';
    const o_hash = '0xdef456';

    const appendix = generateProofAppendix(receipts, merkleRoot, o_hash);

    // Extract receipt order from table
    const lines = appendix.split('\n');
    const tableLines = lines.filter(l => l.includes('receipt-'));

    // Verify chronological order
    assert.ok(tableLines[0].includes('first'));
    assert.ok(tableLines[1].includes('second'));
    assert.ok(tableLines[2].includes('third'));
  });
});

// =============================================================================
// Tests: formatReceiptTable
// =============================================================================

describe('formatReceiptTable', () => {
  it('should format valid receipt table', () => {
    const receipts = [
      createMockReceipt(1, 'query'),
      createMockReceipt(2, 'proof'),
    ];

    const table = formatReceiptTable(receipts);

    // Verify table structure
    assert.ok(table.includes('| Receipt ID | Timestamp | Type | Decision | Output Hash |'));
    assert.ok(table.includes('|------------|-----------|------|----------|-------------|'));

    // Verify rows
    assert.ok(table.includes('receipt-q'));
    assert.ok(table.includes('query'));
    assert.ok(table.includes('proof'));
    assert.ok(table.includes('allow'));
  });

  it('should handle empty receipts array', () => {
    const table = formatReceiptTable([]);
    assert.strictEqual(table, '*No receipts available*');
  });

  it('should abbreviate receipt IDs and hashes', () => {
    const receipt = createMockReceipt(1, 'test');
    const table = formatReceiptTable([receipt]);

    // ID abbreviated to 8 chars (receipt- prefix + alphanumeric)
    const idMatch = table.match(/\[([a-z0-9-]{8})\]/);
    assert.ok(idMatch, 'Should find abbreviated receipt ID');
    assert.strictEqual(idMatch[1].length, 8);

    // Hash abbreviated to 8 chars
    const hashMatch = table.match(/`([a-f0-9]{8})`/);
    assert.ok(hashMatch, 'Should find abbreviated hash');
    assert.strictEqual(hashMatch[1].length, 8);
  });

  it('should handle receipts without decision', () => {
    const receipt = {
      ...createMockReceipt(1, 'test'),
      payload: {}, // No decision field
    };

    const table = formatReceiptTable([receipt]);
    assert.ok(table.includes('| - |')); // Default decision
  });
});

// =============================================================================
// Tests: formatMerkleTree
// =============================================================================

describe('formatMerkleTree', () => {
  it('should format small tree (≤8 receipts) as ASCII', () => {
    const proofs = createMockMerkleProofs(4);
    const tree = formatMerkleTree(proofs);

    // Verify ASCII structure
    assert.ok(tree.includes('[root]'));
    assert.ok(tree.includes('r1'));
    assert.ok(tree.includes('r4'));
    assert.ok(tree.includes('```'));
  });

  it('should format large tree (>8 receipts) as nested list', () => {
    const proofs = createMockMerkleProofs(20);
    const tree = formatMerkleTree(proofs);

    // Verify nested structure
    assert.ok(tree.includes('root/'));
    assert.ok(tree.includes('left-subtree/'));
    assert.ok(tree.includes('right-subtree/'));
    assert.ok(tree.includes('receipts 1-10'));
    assert.ok(tree.includes('receipts 11-20'));
  });

  it('should include tree properties', () => {
    const proofs = createMockMerkleProofs(8);
    const tree = formatMerkleTree(proofs);

    assert.ok(tree.includes('**Tree Properties:**'));
    assert.ok(tree.includes('- Total Receipts: 8'));
    assert.ok(tree.includes('- Tree Depth:'));
    assert.ok(tree.includes('- Proof Size:'));
  });

  it('should handle empty proofs', () => {
    const tree = formatMerkleTree({});
    assert.strictEqual(tree, '*No merkle proofs available*');
  });

  it('should handle null/undefined proofs', () => {
    assert.strictEqual(formatMerkleTree(null), '*Merkle tree structure not available*');
    assert.strictEqual(formatMerkleTree(undefined), '*Merkle tree structure not available*');
  });
});

// =============================================================================
// Tests: formatVerificationInstructions
// =============================================================================

describe('formatVerificationInstructions', () => {
  it('should generate complete verification steps', () => {
    const o_hash = '0xabc123';
    const merkleRoot = '0xdef456';

    const instructions = formatVerificationInstructions(o_hash, merkleRoot);

    // Verify all steps present
    assert.ok(instructions.includes('**Step 1: Fetch Published Receipts**'));
    assert.ok(instructions.includes('**Step 2: Reconstruct Merkle Tree**'));
    assert.ok(instructions.includes('**Step 3: Compare Root Hash**'));
    assert.ok(instructions.includes('**Step 4: Verify Universe Hash (Optional)**'));

    // Verify includes bash commands
    assert.ok(instructions.includes('```bash'));
    assert.ok(instructions.includes('curl'));
    assert.ok(instructions.includes('node'));

    // Verify interpretation section
    assert.ok(instructions.includes('**Interpretation:**'));
    assert.ok(instructions.includes('✅ **Match**'));
    assert.ok(instructions.includes('❌ **Mismatch**'));
  });

  it('should include automated verification command', () => {
    const instructions = formatVerificationInstructions('0xabc', '0xdef');
    assert.ok(instructions.includes('pnpm fusion verify-doc'));
  });
});

// =============================================================================
// Tests: formatHashValues
// =============================================================================

describe('formatHashValues', () => {
  it('should format comprehensive hash reference', () => {
    const frontmatter = {
      title: 'Test Document',
      version: '1.0.0',
    };
    const receipts = [
      createMockReceipt(1, 'query'),
      createMockReceipt(2, 'proof'),
    ];
    const merkleRoot = '0xabc123';
    const o_hash = '0xdef456';

    const hashes = formatHashValues(frontmatter, receipts, merkleRoot, o_hash);

    // Verify YAML structure
    assert.ok(hashes.includes('```yaml'));
    assert.ok(hashes.includes('# Universe State'));
    assert.ok(hashes.includes('o_hash: 0xdef456'));
    assert.ok(hashes.includes('# Merkle Tree Root'));
    assert.ok(hashes.includes('merkle_root: 0xabc123'));

    // Verify receipt chain
    assert.ok(hashes.includes('# Receipt Chain'));
    assert.ok(hashes.includes('receipts:'));
    assert.ok(hashes.includes('- id: receipt-query-1'));
    assert.ok(hashes.includes('- id: receipt-proof-2'));

    // Verify metadata
    assert.ok(hashes.includes('verified_at:'));
    assert.ok(hashes.includes('document_title: Test Document'));
    assert.ok(hashes.includes('document_version: 1.0.0'));
  });

  it('should handle missing frontmatter fields', () => {
    const receipts = [createMockReceipt(1, 'test')];
    const hashes = formatHashValues({}, receipts, '0xabc', '0xdef');

    // Should not include document_title/version
    assert.ok(!hashes.includes('document_title:'));
    assert.ok(!hashes.includes('document_version:'));

    // But should still have core fields
    assert.ok(hashes.includes('o_hash:'));
    assert.ok(hashes.includes('merkle_root:'));
  });
});

// =============================================================================
// Tests: insertProofAppendix
// =============================================================================

describe('insertProofAppendix', () => {
  it('should append proof to end of document', () => {
    const markdown = '# Test Document\n\nSome content here.';
    const receipts = [createMockReceipt(1, 'test')];
    const merkleRoot = '0xabc123';
    const o_hash = '0xdef456';

    const result = insertProofAppendix(markdown, receipts, merkleRoot, o_hash);

    // Verify original content preserved
    assert.ok(result.includes('# Test Document'));
    assert.ok(result.includes('Some content here.'));

    // Verify appendix added
    assert.ok(result.includes('## Verification Appendix'));

    // Verify appendix is at the end
    const appendixIndex = result.indexOf('## Verification Appendix');
    assert.ok(appendixIndex > result.indexOf('Some content here.'));
  });

  it('should insert before References section', () => {
    const markdown = '# Test\n\nContent.\n\n## References\n\n1. Source';
    const receipts = [createMockReceipt(1, 'test')];
    const merkleRoot = '0xabc123';
    const o_hash = '0xdef456';

    const result = insertProofAppendix(markdown, receipts, merkleRoot, o_hash);

    // Verify appendix before references
    const appendixIndex = result.indexOf('## Verification Appendix');
    const referencesIndex = result.indexOf('## References');
    assert.ok(appendixIndex < referencesIndex);
  });

  it('should replace existing verification appendix', () => {
    const markdown = `# Test

Content here.

## Verification Appendix

Old proof data.`;

    const receipts = [createMockReceipt(1, 'new')];
    const merkleRoot = '0xnewhash';
    const o_hash = '0xnewohash';

    const result = insertProofAppendix(markdown, receipts, merkleRoot, o_hash);

    // Verify old data removed
    assert.ok(!result.includes('Old proof data'));

    // Verify new data present
    assert.ok(result.includes('0xnewhash'));
    assert.ok(result.includes('0xnewohash'));

    // Verify only one appendix
    const matches = result.match(/## Verification Appendix/g);
    assert.strictEqual(matches.length, 1);
  });

  it('should throw on invalid markdown input', () => {
    assert.throws(() => {
      insertProofAppendix(null, [createMockReceipt(1, 'test')], '0xabc', '0xdef');
    }, /markdown must be a string/);
  });
});

// =============================================================================
// Tests: renderProofAsJSON
// =============================================================================

describe('renderProofAsJSON', () => {
  it('should export proof as JSON', () => {
    const receipts = [
      createMockReceipt(1, 'query'),
      createMockReceipt(2, 'proof'),
    ];
    const merkleRoot = '0xabc123';
    const o_hash = '0xdef456';
    const merkleProofs = createMockMerkleProofs(2);

    const json = renderProofAsJSON(receipts, merkleRoot, o_hash, { merkleProofs });

    // Verify structure
    assert.strictEqual(json.version, '1.0.0');
    assert.strictEqual(json.type, 'kgc-document-proof');
    assert.strictEqual(json.o_hash, o_hash);
    assert.strictEqual(json.merkleRoot, merkleRoot);
    assert.ok(json.verified_at);

    // Verify receipts
    assert.strictEqual(json.receipts.length, 2);
    assert.strictEqual(json.receipts[0].id, 'receipt-query-1');
    assert.strictEqual(json.receipts[1].id, 'receipt-proof-2');

    // Verify proofs
    assert.ok(json.proofs);

    // Verify metadata
    assert.strictEqual(json.metadata.receiptCount, 2);
    assert.ok(json.metadata.treeDepth >= 0);
  });

  it('should include frontmatter in metadata', () => {
    const receipts = [createMockReceipt(1, 'test')];
    const frontmatter = {
      title: 'Test Doc',
      version: '2.0.0',
    };

    const json = renderProofAsJSON(receipts, '0xabc', '0xdef', { frontmatter });

    assert.strictEqual(json.metadata.documentTitle, 'Test Doc');
    assert.strictEqual(json.metadata.documentVersion, '2.0.0');
  });

  it('should throw on invalid inputs', () => {
    assert.throws(() => {
      renderProofAsJSON('not-array', '0xabc', '0xdef');
    }, /receipts must be an array/);

    assert.throws(() => {
      renderProofAsJSON([], 123, '0xdef');
    }, /merkleRoot must be a string/);

    assert.throws(() => {
      renderProofAsJSON([], '0xabc', null);
    }, /o_hash must be a string/);
  });
});

// =============================================================================
// Tests: updateProofTimestamp
// =============================================================================

describe('updateProofTimestamp', () => {
  it('should update timestamp to current time', () => {
    const proof = {
      o_hash: '0xabc',
      merkleRoot: '0xdef',
      verified_at: '2025-01-01T00:00:00.000Z',
      receipts: [],
    };

    const updated = updateProofTimestamp(proof);

    // Verify timestamp changed
    assert.notStrictEqual(updated.verified_at, proof.verified_at);

    // Verify other fields preserved
    assert.strictEqual(updated.o_hash, proof.o_hash);
    assert.strictEqual(updated.merkleRoot, proof.merkleRoot);
  });

  it('should accept custom timestamp', () => {
    const proof = {
      o_hash: '0xabc',
      verified_at: '2025-01-01T00:00:00.000Z',
    };
    const customTime = '2025-12-26T15:30:00.000Z';

    const updated = updateProofTimestamp(proof, customTime);

    assert.strictEqual(updated.verified_at, customTime);
  });

  it('should throw on invalid proof', () => {
    assert.throws(() => {
      updateProofTimestamp(null);
    }, /proof must be an object/);
  });
});

// =============================================================================
// Tests: extractProofFromDocument
// =============================================================================

describe('extractProofFromDocument', () => {
  it('should extract proof from document with verification appendix', () => {
    const markdown = `# Test Document

Content here.

## Verification Appendix

| Receipt ID | Timestamp | Type | Decision | Output Hash |
|------------|-----------|------|----------|-------------|
| [abc12345](receipts/admits/receipt-test-1.json) | 2025-12-26 | query | allow | \`fedcba98\` |
| [def67890](receipts/admits/receipt-test-2.json) | 2025-12-26 | proof | allow | \`12345678\` |

\`\`\`yaml
o_hash: 0xabc123def456
merkle_root: 0xfedcba987654
verified_at: 2025-12-26T12:00:00.000Z
\`\`\`
`;

    const proof = extractProofFromDocument(markdown);

    assert.ok(proof);
    assert.strictEqual(proof.o_hash, '0xabc123def456');
    assert.strictEqual(proof.merkleRoot, '0xfedcba987654');
    assert.strictEqual(proof.verified_at, '2025-12-26T12:00:00.000Z');
    assert.strictEqual(proof.receiptIds.length, 2);
    assert.ok(proof.receiptIds.includes('receipt-test-1'));
    assert.ok(proof.receiptIds.includes('receipt-test-2'));
  });

  it('should return null for document without appendix', () => {
    const markdown = '# Test\n\nNo appendix here.';
    const proof = extractProofFromDocument(markdown);
    assert.strictEqual(proof, null);
  });

  it('should return null for invalid input', () => {
    assert.strictEqual(extractProofFromDocument(null), null);
    assert.strictEqual(extractProofFromDocument(123), null);
  });
});

// =============================================================================
// Tests: computeDocumentHash
// =============================================================================

describe('computeDocumentHash', () => {
  it('should compute SHA256 hash of document', () => {
    const markdown = '# Test\n\nContent';
    const hash = computeDocumentHash(markdown);

    // Verify hash format
    assert.strictEqual(typeof hash, 'string');
    assert.strictEqual(hash.length, 64); // SHA256 = 64 hex chars
    assert.ok(/^[0-9a-f]+$/i.test(hash));
  });

  it('should exclude verification appendix from hash', () => {
    const content = '# Test\n\nContent';
    const withAppendix = content + '\n\n## Verification Appendix\n\nProof data';

    const hash1 = computeDocumentHash(content);
    const hash2 = computeDocumentHash(withAppendix);

    // Hashes should match (appendix excluded)
    assert.strictEqual(hash1, hash2);
  });

  it('should be deterministic', () => {
    const markdown = '# Test\n\nSame content';
    const hash1 = computeDocumentHash(markdown);
    const hash2 = computeDocumentHash(markdown);

    assert.strictEqual(hash1, hash2);
  });

  it('should throw on invalid input', () => {
    assert.throws(() => {
      computeDocumentHash(null);
    }, /markdown must be a string/);
  });
});

// =============================================================================
// Tests: validateProofIntegrity
// =============================================================================

describe('validateProofIntegrity', () => {
  it('should validate well-formed proof', () => {
    const proof = {
      o_hash: '0xabc123',
      merkleRoot: '0xdef456',
      verified_at: '2025-12-26T12:00:00.000Z',
      receipts: [
        {
          id: 'receipt-1',
          hash: 'abc123',
          timestamp: '1000000000',
        },
      ],
      proofs: {},
    };

    const result = validateProofIntegrity(proof);

    assert.strictEqual(result.valid, true);
    assert.strictEqual(result.errors.length, 0);
  });

  it('should detect missing required fields', () => {
    const proof = {
      o_hash: '0xabc123',
      // Missing merkleRoot, verified_at, receipts
    };

    const result = validateProofIntegrity(proof);

    assert.strictEqual(result.valid, false);
    assert.ok(result.errors.some(e => e.includes('merkleRoot')));
    assert.ok(result.errors.some(e => e.includes('verified_at')));
    assert.ok(result.errors.some(e => e.includes('receipts')));
  });

  it('should detect invalid receipt structure', () => {
    const proof = {
      o_hash: '0xabc123',
      merkleRoot: '0xdef456',
      verified_at: '2025-12-26T12:00:00.000Z',
      receipts: [
        {
          id: 'receipt-1',
          // Missing hash, timestamp
        },
      ],
    };

    const result = validateProofIntegrity(proof);

    assert.strictEqual(result.valid, false);
    assert.ok(result.errors.some(e => e.includes('hash')));
    assert.ok(result.errors.some(e => e.includes('timestamp')));
  });

  it('should detect invalid hash formats', () => {
    const proof = {
      o_hash: 'not-a-hex-hash',
      merkleRoot: '0xdef456',
      verified_at: '2025-12-26T12:00:00.000Z',
      receipts: [createMockReceipt(1, 'test')],
    };

    const result = validateProofIntegrity(proof);

    assert.strictEqual(result.valid, false);
    assert.ok(result.errors.some(e => e.includes('o_hash')));
  });

  it('should detect empty receipts array', () => {
    const proof = {
      o_hash: '0xabc123',
      merkleRoot: '0xdef456',
      verified_at: '2025-12-26T12:00:00.000Z',
      receipts: [],
    };

    const result = validateProofIntegrity(proof);

    assert.strictEqual(result.valid, false);
    assert.ok(result.errors.some(e => e.includes('empty')));
  });

  it('should return invalid for non-object proof', () => {
    const result = validateProofIntegrity(null);
    assert.strictEqual(result.valid, false);
    assert.ok(result.errors.some(e => e.includes('must be an object')));
  });
});
