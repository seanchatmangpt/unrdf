/**
 * Comprehensive Test Suite for KGC Markdown Hooks
 *
 * Tests all 4 hooks with happy path and error cases.
 *
 * @module hooks-test
 */

import { describe, it, beforeEach, afterEach } from 'node:test';
import assert from 'node:assert/strict';
import { writeFile, mkdir, rm, access } from 'node:fs/promises';
import { join } from 'node:path';

// Import hooks
import onWriteDocs from './on-write-docs-hook.mjs';
import onEditDocs from './on-edit-docs-hook.mjs';
import onBoundsExceeded from './on-bounds-exceeded-hook.mjs';
import onNonDeterminism from './on-non-determinism-hook.mjs';

// Import shared utilities
import {
  extractFrontmatter,
  validateFrontmatter,
  computeContentHash,
  DENIALS_DIR,
} from './hooks-shared.mjs';

// =============================================================================
// Test Fixtures
// =============================================================================

const TEST_DIR = '/tmp/kgc-hooks-test';
const TEST_DOCS_DIR = join(TEST_DIR, 'docs');
const TEST_RECEIPTS_DIR = join(TEST_DIR, 'receipts');

const VALID_FRONTMATTER = `---
o_hash: 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
policy_id: test-policy-v1
receipts: []
maxQueries: 50
maxRuntime: 3000
maxFileScans: 100
---`;

const VALID_DOCUMENT = `${VALID_FRONTMATTER}

# Test Document

This is a test document with valid frontmatter.

## Section 1

Some content here.
`;

const INVALID_FRONTMATTER_MISSING_FIELDS = `---
title: Test Document
---`;

const INVALID_FRONTMATTER_BAD_HASH = `---
o_hash: invalid-hash
policy_id: test-policy
receipts: []
---`;

// =============================================================================
// Setup & Teardown
// =============================================================================

async function setupTestEnvironment() {
  await mkdir(TEST_DIR, { recursive: true });
  await mkdir(TEST_DOCS_DIR, { recursive: true });
  await mkdir(TEST_RECEIPTS_DIR, { recursive: true });
}

async function teardownTestEnvironment() {
  try {
    await rm(TEST_DIR, { recursive: true, force: true });
  } catch {
    // Ignore errors
  }
}

// =============================================================================
// Test Suite: On-Write-Docs Hook
// =============================================================================

describe('on-write-docs-hook', () => {
  beforeEach(setupTestEnvironment);
  afterEach(teardownTestEnvironment);

  it('should allow write with valid frontmatter', async () => {
    const testPath = join(TEST_DOCS_DIR, 'valid.md');

    const result = await onWriteDocs.validate({
      filePath: testPath,
      content: VALID_DOCUMENT,
      dryRun: true,
    });

    assert.equal(result.allowed, true);
    assert.equal(result.errors, undefined);
  });

  it('should deny write with missing frontmatter', async () => {
    const testPath = join(TEST_DOCS_DIR, 'no-frontmatter.md');
    const content = '# Document\n\nNo frontmatter here.';

    const result = await onWriteDocs.validate({
      filePath: testPath,
      content,
      dryRun: true,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.errors.length > 0);
    assert.ok(result.errors.some(e => e.includes('frontmatter')));
  });

  it('should deny write with invalid frontmatter (missing o_hash)', async () => {
    const testPath = join(TEST_DOCS_DIR, 'invalid.md');
    const content = `${INVALID_FRONTMATTER_MISSING_FIELDS}\n\nContent`;

    const result = await onWriteDocs.validate({
      filePath: testPath,
      content,
      dryRun: true,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.errors.some(e => e.includes('o_hash')));
  });

  it('should deny write with invalid o_hash format', async () => {
    const testPath = join(TEST_DOCS_DIR, 'bad-hash.md');
    const content = `${INVALID_FRONTMATTER_BAD_HASH}\n\nContent`;

    const result = await onWriteDocs.validate({
      filePath: testPath,
      content,
      dryRun: true,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.errors.some(e => e.includes('64-character hex')));
  });

  it('should deny write with missing receipt references', async () => {
    const testPath = join(TEST_DOCS_DIR, 'missing-receipt.md');
    const content = `---
o_hash: 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
policy_id: test-policy
receipts: ["non-existent-receipt-id"]
---

Content`;

    const result = await onWriteDocs.validate({
      filePath: testPath,
      content,
      dryRun: true,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.errors.some(e => e.includes('receipt')));
  });

  it('should skip validation for non-docs paths', async () => {
    const testPath = '/tmp/other-dir/file.md';

    const result = await onWriteDocs.validate({
      filePath: testPath,
      content: 'No frontmatter needed',
      dryRun: true,
    });

    assert.equal(result.allowed, true);
  });

  it('should generate denial receipt when not in dry run', async () => {
    const testPath = join(TEST_DOCS_DIR, 'denied.md');
    const content = '# No Frontmatter';

    // Override DENIALS_DIR temporarily
    const originalDenialsDir = process.env.DENIALS_DIR;
    process.env.DENIALS_DIR = join(TEST_DIR, 'denials');

    await mkdir(process.env.DENIALS_DIR, { recursive: true });

    const result = await onWriteDocs.validate({
      filePath: testPath,
      content,
      dryRun: false,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.receipt);
    assert.equal(result.receipt.operation, 'write');
    assert.ok(result.receipt.remediation);

    // Restore
    process.env.DENIALS_DIR = originalDenialsDir;
  });
});

// =============================================================================
// Test Suite: On-Edit-Docs Hook
// =============================================================================

describe('on-edit-docs-hook', () => {
  beforeEach(setupTestEnvironment);
  afterEach(teardownTestEnvironment);

  it('should allow edit to frontmatter only', async () => {
    const testPath = join(TEST_DOCS_DIR, 'edit-frontmatter.md');
    await writeFile(testPath, VALID_DOCUMENT);

    const result = await onEditDocs.validate({
      filePath: testPath,
      oldString: 'maxQueries: 50',
      newString: 'maxQueries: 100',
      dryRun: true,
    });

    assert.equal(result.allowed, true);
  });

  it('should deny edit to body without o_hash change', async () => {
    const testPath = join(TEST_DOCS_DIR, 'edit-body.md');
    await writeFile(testPath, VALID_DOCUMENT);

    const result = await onEditDocs.validate({
      filePath: testPath,
      oldString: 'Some content here',
      newString: 'Different content',
      dryRun: true,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.errors.some(e => e.includes('o_hash unchanged')));
  });

  it('should allow edit to body with o_hash change', async () => {
    const testPath = join(TEST_DOCS_DIR, 'edit-with-hash.md');
    await writeFile(testPath, VALID_DOCUMENT);

    // Edit both content and o_hash
    const oldString = `${VALID_FRONTMATTER}

# Test Document

This is a test document with valid frontmatter.`;

    const newString = `---
o_hash: fedcba0987654321fedcba0987654321fedcba0987654321fedcba0987654321
policy_id: test-policy-v1
receipts: []
maxQueries: 50
maxRuntime: 3000
maxFileScans: 100
---

# Test Document

This is MODIFIED content with new hash.`;

    const result = await onEditDocs.validate({
      filePath: testPath,
      oldString,
      newString,
      dryRun: true,
    });

    assert.equal(result.allowed, true);
  });

  it('should skip validation for files without frontmatter', async () => {
    const testPath = join(TEST_DOCS_DIR, 'no-frontmatter.md');
    await writeFile(testPath, '# Simple Doc\n\nNo frontmatter.');

    const result = await onEditDocs.validate({
      filePath: testPath,
      oldString: 'Simple',
      newString: 'Complex',
      dryRun: true,
    });

    assert.equal(result.allowed, true);
    assert.ok(result.warnings);
  });

  it('should skip validation for non-existent files', async () => {
    const testPath = join(TEST_DOCS_DIR, 'non-existent.md');

    const result = await onEditDocs.validate({
      filePath: testPath,
      oldString: 'old',
      newString: 'new',
      dryRun: true,
    });

    assert.equal(result.allowed, true);
  });
});

// =============================================================================
// Test Suite: On-Bounds-Exceeded Hook
// =============================================================================

describe('on-bounds-exceeded-hook', () => {
  beforeEach(setupTestEnvironment);
  afterEach(teardownTestEnvironment);

  it('should allow query within bounds', async () => {
    const testPath = join(TEST_DOCS_DIR, 'bounded.md');
    await writeFile(testPath, VALID_DOCUMENT);

    const result = await onBoundsExceeded.validate({
      filePath: testPath,
      operation: 'query',
      sparqlQuery: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
      operationParams: {},
      dryRun: true,
    });

    assert.equal(result.allowed, true);
    assert.ok(result.cost);
  });

  it('should deny query exceeding maxRuntime', async () => {
    const testPath = join(TEST_DOCS_DIR, 'bounded.md');
    const lowBoundsDoc = `---
o_hash: 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
policy_id: test-policy
receipts: []
maxQueries: 100
maxRuntime: 10
maxFileScans: 100
---

Content`;

    await writeFile(testPath, lowBoundsDoc);

    // Complex query that exceeds 10ms estimate
    const complexQuery = `
      SELECT * WHERE {
        ?s1 ?p1 ?o1 .
        ?s2 ?p2 ?o2 .
        ?s3 ?p3 ?o3 .
        FILTER(regex(?o1, "pattern"))
        FILTER(regex(?o2, "pattern"))
      }
    `;

    const result = await onBoundsExceeded.validate({
      filePath: testPath,
      operation: 'query',
      sparqlQuery: complexQuery,
      operationParams: {},
      dryRun: true,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.errors.some(e => e.includes('maxRuntime')));
  });

  it('should deny operation exceeding maxFileScans', async () => {
    const testPath = join(TEST_DOCS_DIR, 'bounded.md');
    const lowBoundsDoc = `---
o_hash: 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
policy_id: test-policy
receipts: []
maxQueries: 100
maxRuntime: 5000
maxFileScans: 5
---

Content`;

    await writeFile(testPath, lowBoundsDoc);

    const result = await onBoundsExceeded.validate({
      filePath: testPath,
      operation: 'proof',
      operationParams: {
        receiptChainDepth: 10,  // 10 * 2 = 20 files > 5 limit
      },
      dryRun: true,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.errors.some(e => e.includes('maxFileScans')));
  });

  it('should use default bounds if not specified', async () => {
    const testPath = join(TEST_DOCS_DIR, 'no-bounds.md');
    const noBoundsDoc = `---
o_hash: 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
policy_id: test-policy
receipts: []
---

Content`;

    await writeFile(testPath, noBoundsDoc);

    const result = await onBoundsExceeded.validate({
      filePath: testPath,
      operation: 'query',
      sparqlQuery: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
      operationParams: {},
      dryRun: true,
    });

    assert.equal(result.allowed, true);
    assert.ok(result.cost);
  });

  it('should estimate query cardinality correctly', () => {
    const askQuery = 'ASK { ?s ?p ?o }';
    const limitQuery = 'SELECT * WHERE { ?s ?p ?o } LIMIT 50';
    const countQuery = 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }';

    assert.equal(onBoundsExceeded.estimateQueryCardinality(askQuery), 1);
    assert.equal(onBoundsExceeded.estimateQueryCardinality(limitQuery), 50);
    assert.equal(onBoundsExceeded.estimateQueryCardinality(countQuery), 1);
  });
});

// =============================================================================
// Test Suite: On-Non-Determinism Hook
// =============================================================================

describe('on-non-determinism-hook', () => {
  beforeEach(setupTestEnvironment);
  afterEach(teardownTestEnvironment);

  it('should allow deterministic output', async () => {
    const testPath = join(TEST_DOCS_DIR, 'deterministic.md');
    await writeFile(testPath, VALID_DOCUMENT);

    const deterministicOutput = `
Result: Success
Count: 42
Items: [item1, item2, item3]
`;

    const result = await onNonDeterminism.validate({
      filePath: testPath,
      operation: 'query',
      output: deterministicOutput,
      dryRun: true,
      warnOnly: true,
    });

    assert.equal(result.allowed, true);
  });

  it('should warn on timestamp in output', async () => {
    const testPath = join(TEST_DOCS_DIR, 'timestamp.md');
    await writeFile(testPath, VALID_DOCUMENT);

    const outputWithTimestamp = `
Generated at: 2025-12-26T10:30:45Z
Result: Success
`;

    const result = await onNonDeterminism.validate({
      filePath: testPath,
      operation: 'query',
      output: outputWithTimestamp,
      dryRun: true,
      warnOnly: true,
    });

    assert.equal(result.allowed, true);
    assert.ok(result.warnings);
    assert.ok(result.warnings.some(w => w.includes('non-deterministic')));
  });

  it('should warn on random values in output', async () => {
    const testPath = join(TEST_DOCS_DIR, 'random.md');
    await writeFile(testPath, VALID_DOCUMENT);

    const outputWithRandom = `
const id = Math.random();
Result: Success
`;

    const result = await onNonDeterminism.validate({
      filePath: testPath,
      operation: 'query',
      output: outputWithRandom,
      dryRun: true,
      warnOnly: true,
    });

    assert.equal(result.allowed, true);
    assert.ok(result.warnings);
  });

  it('should detect hash mismatch', async () => {
    const testPath = join(TEST_DOCS_DIR, 'hash-check.md');
    await writeFile(testPath, VALID_DOCUMENT);

    const output = 'Deterministic output';
    const expectedHash = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';

    const result = await onNonDeterminism.validate({
      filePath: testPath,
      operation: 'query',
      output,
      expectedHash,
      dryRun: true,
      warnOnly: true,
    });

    assert.equal(result.allowed, true);
    assert.ok(result.warnings);
    assert.ok(result.warnings.some(w => w.includes('hash mismatch')));
  });

  it('should normalize output before hashing', async () => {
    const output1 = '  { "a": 1, "b": 2 }  \n\n';
    const output2 = '{"a":1,"b":2}';

    const normalized1 = onNonDeterminism.normalizeOutput(output1);
    const normalized2 = onNonDeterminism.normalizeOutput(output2);

    // Both should normalize to same JSON structure
    assert.equal(normalized1, normalized2);
  });

  it('should check determinism patterns', () => {
    const deterministicOutput = 'Result: 42\nStatus: OK';
    const nonDeterministicOutput = 'Timestamp: 2025-12-26T10:30:45Z';

    const check1 = onNonDeterminism.checkDeterminism(deterministicOutput);
    const check2 = onNonDeterminism.checkDeterminism(nonDeterministicOutput);

    assert.equal(check1.isDeterministic, true);
    assert.equal(check2.isDeterministic, false);
    assert.ok(check2.violations.length > 0);
  });
});

// =============================================================================
// Test Suite: Shared Utilities
// =============================================================================

describe('hooks-shared utilities', () => {
  it('should extract frontmatter correctly', () => {
    const { frontmatter, content } = extractFrontmatter(VALID_DOCUMENT);

    assert.ok(frontmatter);
    assert.equal(frontmatter.o_hash, '1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef');
    assert.equal(frontmatter.policy_id, 'test-policy-v1');
    assert.ok(Array.isArray(frontmatter.receipts));
    assert.ok(content.includes('# Test Document'));
  });

  it('should validate frontmatter schema', () => {
    const valid = validateFrontmatter({
      o_hash: '1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
      policy_id: 'test',
      receipts: [],
    });

    const invalid = validateFrontmatter({
      title: 'Missing required fields',
    });

    assert.equal(valid.valid, true);
    assert.equal(invalid.valid, false);
  });

  it('should compute content hash deterministically', async () => {
    const content = 'Test content';

    const hash1 = await computeContentHash(content);
    const hash2 = await computeContentHash(content);

    assert.equal(hash1, hash2);
    assert.equal(hash1.length, 64); // BLAKE3 hex length
  });
});

// =============================================================================
// Run Tests
// =============================================================================

console.log('Running KGC Markdown Hooks Test Suite...\n');
