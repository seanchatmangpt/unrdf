#!/usr/bin/env node
/**
 * @file Standalone test runner for KGC Docs
 * @module kgc-docs/test/runner
 *
 * Simple test runner without vitest dependency
 */

import {
  parseKGCMarkdown,
  parseFrontmatter,
  parseFencedBlock,
  buildAST,
} from '../src/parser.mjs';
import {
  renderDiataxisView,
  renderTutorial,
  renderHowTo,
  renderReference,
  renderExplanation,
  generateProofAppendix,
} from '../src/renderer.mjs';
import {
  createMerkleTree,
  generateProofTree,
  verifyProof,
  linkReceiptHash,
} from '../src/proof.mjs';

let passed = 0;
let failed = 0;

function assert(condition, message) {
  if (condition) {
    passed++;
    console.log(`✓ ${message}`);
  } else {
    failed++;
    console.error(`✗ ${message}`);
  }
}

function assertEqual(actual, expected, message) {
  const condition = actual === expected;
  if (!condition) {
    console.error(
      `  Expected: ${JSON.stringify(expected)}, Got: ${JSON.stringify(actual)}`
    );
  }
  assert(condition, message);
}

console.log('Running KGC Markdown Tests...\n');

// Test 1: Parse frontmatter
console.log('Parser Tests:');
try {
  const markdown = `---
o_hash: test123
policy_id: policy-001
bounds: []
receipts: [receipt-001, receipt-002]
views: [tutorial, reference]
sources: [source-001]
---
# Content`;

  const result = parseFrontmatter(markdown);
  assertEqual(result.o_hash, 'test123', 'Parse o_hash from frontmatter');
  assertEqual(
    result.policy_id,
    'policy-001',
    'Parse policy_id from frontmatter'
  );
  assert(result.receipts.length === 2, 'Parse receipts array');
  assert(result.views.includes('tutorial'), 'Parse views array');
} catch (error) {
  failed++;
  console.error(`✗ Frontmatter parsing failed: ${error.message}`);
}

// Test 2: Parse fenced blocks
try {
  const markdown = `
\`\`\`kgc:query
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
\`\`\`
`;

  const result = parseFencedBlock(markdown);
  assertEqual(result.type, 'query', 'Parse kgc:query block type');
  assert(result.content.includes('SELECT'), 'Parse query content');
} catch (error) {
  failed++;
  console.error(`✗ Fenced block parsing failed: ${error.message}`);
}

// Test 3: Build AST
try {
  const markdown = `---
o_hash: ast-test
policy_id: test
bounds: []
receipts: []
views: [tutorial]
sources: []
---
# Test Heading

\`\`\`kgc:query
SELECT * WHERE { ?s ?p ?o }
\`\`\`
`;

  const ast = buildAST(markdown);
  assert(ast.frontmatter !== undefined, 'AST has frontmatter');
  assertEqual(ast.frontmatter.o_hash, 'ast-test', 'AST frontmatter o_hash');
  assert(ast.blocks.length > 0, 'AST has blocks');
  assert(
    ast.blocks.some((b) => b.type === 'heading'),
    'AST contains heading block'
  );
  assert(
    ast.blocks.some((b) => b.type === 'query'),
    'AST contains query block'
  );
} catch (error) {
  failed++;
  console.error(`✗ AST building failed: ${error.message}`);
}

// Test 4: Render views
console.log('\nRenderer Tests:');
const sampleAST = {
  frontmatter: {
    o_hash: 'render-test',
    policy_id: 'policy-001',
    bounds: [],
    receipts: ['receipt-001'],
    views: ['tutorial', 'how-to', 'reference', 'explanation'],
    sources: [],
  },
  blocks: [
    { type: 'heading', level: 1, content: 'Test Doc' },
    { type: 'query', content: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }' },
  ],
};

try {
  const tutorial = renderTutorial(sampleAST);
  assert(tutorial.includes('Tutorial'), 'Tutorial view renders');
  assert(
    tutorial.includes('Getting Started'),
    'Tutorial contains getting started'
  );
} catch (error) {
  failed++;
  console.error(`✗ Tutorial rendering failed: ${error.message}`);
}

try {
  const howTo = renderHowTo(sampleAST);
  assert(howTo.includes('How to'), 'How-to view renders');
} catch (error) {
  failed++;
  console.error(`✗ How-to rendering failed: ${error.message}`);
}

try {
  const reference = renderReference(sampleAST);
  assert(reference.includes('Reference'), 'Reference view renders');
  assert(reference.includes(sampleAST.frontmatter.o_hash), 'Reference includes o_hash');
} catch (error) {
  failed++;
  console.error(`✗ Reference rendering failed: ${error.message}`);
}

try {
  const explanation = renderExplanation(sampleAST);
  assert(explanation.includes('Understanding'), 'Explanation view renders');
} catch (error) {
  failed++;
  console.error(`✗ Explanation rendering failed: ${error.message}`);
}

try {
  const proof = generateProofAppendix(sampleAST);
  assert(proof.includes('Proof Appendix'), 'Proof appendix renders');
  assert(proof.includes('merkle_root'), 'Proof includes merkle_root');
  assert(
    proof.includes(sampleAST.frontmatter.o_hash),
    'Proof includes o_hash'
  );
} catch (error) {
  failed++;
  console.error(`✗ Proof appendix generation failed: ${error.message}`);
}

// Test 5: Diataxis views
try {
  const views = ['tutorial', 'how-to', 'reference', 'explanation'];
  for (const view of views) {
    const result = renderDiataxisView(sampleAST, view);
    assert(result.length > 0, `Render ${view} view`);
    assert(result.includes('Proof Appendix'), `${view} includes proof`);
  }
} catch (error) {
  failed++;
  console.error(`✗ Diataxis view rendering failed: ${error.message}`);
}

// Test 6: Merkle tree
console.log('\nProof Tests:');
try {
  const receipts = ['receipt-001', 'receipt-002', 'receipt-003'];
  const tree = createMerkleTree(receipts);
  assert(tree.root !== undefined, 'Merkle tree has root');
  assertEqual(tree.root.length, 64, 'Merkle root is SHA256 hash');
  assertEqual(tree.leaves.length, 3, 'Merkle tree has correct leaves');
} catch (error) {
  failed++;
  console.error(`✗ Merkle tree creation failed: ${error.message}`);
}

// Test 7: Determinism
try {
  const receipts = ['r1', 'r2'];
  const tree1 = createMerkleTree(receipts);
  const tree2 = createMerkleTree(receipts);
  assertEqual(tree1.root, tree2.root, 'Merkle tree is deterministic');
} catch (error) {
  failed++;
  console.error(`✗ Determinism test failed: ${error.message}`);
}

// Test 8: Proof generation
try {
  const ast = {
    frontmatter: {
      o_hash: 'proof-test',
      receipts: ['receipt-001', 'receipt-002'],
    },
  };
  const proof = generateProofTree(ast);
  assert(proof.merkle_root !== undefined, 'Proof has merkle_root');
  assertEqual(proof.o_hash, 'proof-test', 'Proof has o_hash');
  assertEqual(proof.receipt_count, 2, 'Proof has correct receipt count');
  assert(proof.timestamp !== undefined, 'Proof has timestamp');
} catch (error) {
  failed++;
  console.error(`✗ Proof generation failed: ${error.message}`);
}

// Test 9: Proof verification
try {
  const ast = {
    frontmatter: {
      o_hash: 'verify-test',
      receipts: ['receipt-001'],
    },
  };
  const proof = generateProofTree(ast);
  const isValid = verifyProof(proof, ast);
  assert(isValid, 'Valid proof verifies successfully');
} catch (error) {
  failed++;
  console.error(`✗ Proof verification failed: ${error.message}`);
}

// Test 10: Receipt linkage
try {
  const linkage = linkReceiptHash('receipt123', 'root456');
  assertEqual(linkage.receipt_hash, 'receipt123', 'Linkage has receipt_hash');
  assertEqual(linkage.o_hash, 'root456', 'Linkage has o_hash');
  assert(linkage.combined_hash !== undefined, 'Linkage has combined_hash');
  assertEqual(linkage.combined_hash.length, 64, 'Combined hash is SHA256');
} catch (error) {
  failed++;
  console.error(`✗ Receipt linkage failed: ${error.message}`);
}

// Summary
console.log('\n' + '='.repeat(50));
console.log(`Tests Passed: ${passed}`);
console.log(`Tests Failed: ${failed}`);
console.log('='.repeat(50));

process.exit(failed > 0 ? 1 : 0);
