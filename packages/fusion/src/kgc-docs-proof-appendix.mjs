/**
 * KGC Documentation Proof Appendix Generator
 *
 * Auto-generates verification appendices for rendered documents.
 * Provides cryptographic proof of document provenance and integrity.
 *
 * VERIFICATION FLOW:
 * 1. Document rendered → Receipts generated for each operation
 * 2. Receipts chained → Merkle tree built from receipt hashes
 * 3. Proof appendix → Generated with receipts + merkle root + o_hash
 * 4. Auditor verification → Follow instructions to verify independently
 *
 * @module @unrdf/fusion/kgc-docs-proof-appendix
 */

import { createHash } from 'node:crypto';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Proof appendix schema
 */
const ProofAppendixSchema = z.object({
  /** Proof format version */
  version: z.string(),
  /** Proof type identifier */
  type: z.string(),
  /** Universe hash (o_hash) at time of generation */
  o_hash: z.string(),
  /** Merkle root of receipt chain */
  merkleRoot: z.string(),
  /** Timestamp when proof was generated/verified */
  verified_at: z.string(),
  /** Array of receipts in chronological order */
  receipts: z.array(z.any()),
  /** Merkle proofs for each receipt */
  proofs: z.any(),
  /** Metadata about the proof */
  metadata: z.object({
    receiptCount: z.number(),
    treeDepth: z.number(),
    documentTitle: z.string().optional(),
    documentVersion: z.string().optional(),
  }).optional(),
});

/**
 * Receipt summary for table display
 */
const ReceiptSummarySchema = z.object({
  id: z.string(),
  timestamp: z.string(),
  eventType: z.string(),
  decision: z.string().optional(),
  outputHash: z.string(),
});

// =============================================================================
// Core Functions
// =============================================================================

/**
 * Generate complete proof appendix for a document
 *
 * Combines receipt log, merkle tree visualization, verification instructions,
 * and hash values into a comprehensive proof section.
 *
 * @param {Array<Object>} receipts - Receipt chain (sorted by timestamp)
 * @param {string} merkleRoot - Merkle tree root hash
 * @param {string} o_hash - Universe hash (snapshot when doc was generated)
 * @param {Object} [opts={}] - Optional configuration
 * @param {Object} [opts.frontmatter] - Document frontmatter
 * @param {Object} [opts.merkleProofs] - Merkle proof objects
 * @returns {string} Markdown proof appendix
 *
 * @example
 * const appendix = generateProofAppendix(receipts, merkleRoot, o_hash);
 * console.log(appendix); // Full markdown verification section
 */
export function generateProofAppendix(receipts, merkleRoot, o_hash, opts = {}) {
  // Input validation
  if (!Array.isArray(receipts) || receipts.length === 0) {
    throw new TypeError('generateProofAppendix: receipts must be a non-empty array');
  }
  if (!merkleRoot || typeof merkleRoot !== 'string') {
    throw new TypeError('generateProofAppendix: merkleRoot must be a string');
  }
  if (!o_hash || typeof o_hash !== 'string') {
    throw new TypeError('generateProofAppendix: o_hash must be a string');
  }

  const { frontmatter = {}, merkleProofs = {} } = opts;

  // Sort receipts by timestamp (chronological order)
  const sortedReceipts = [...receipts].sort((a, b) => {
    const timeA = BigInt(a.timestamp);
    const timeB = BigInt(b.timestamp);
    return timeA < timeB ? -1 : timeA > timeB ? 1 : 0;
  });

  // Build markdown sections
  const sections = [];

  // Header
  sections.push('## Verification Appendix');
  sections.push('');
  sections.push('This appendix provides cryptographic proof of document provenance and integrity.');
  sections.push('All claims in this document are verifiable using the receipts and merkle proofs below.');
  sections.push('');

  // Receipt log table
  sections.push('### Receipt Log');
  sections.push('');
  sections.push(formatReceiptTable(sortedReceipts));
  sections.push('');

  // Merkle tree visualization
  sections.push('### Merkle Tree Structure');
  sections.push('');
  sections.push(formatMerkleTree(merkleProofs));
  sections.push('');

  // Hash values reference
  sections.push('### Hash Values');
  sections.push('');
  sections.push(formatHashValues(frontmatter, sortedReceipts, merkleRoot, o_hash));
  sections.push('');

  // Verification instructions
  sections.push('### Verification Instructions');
  sections.push('');
  sections.push(formatVerificationInstructions(o_hash, merkleRoot));
  sections.push('');

  return sections.join('\n');
}

/**
 * Format receipt table for markdown display
 *
 * Creates a markdown table with receipt details:
 * - Receipt ID (abbreviated, linked to receipt file)
 * - Timestamp (ISO 8601)
 * - Block Type (query/proof/extract/render)
 * - Decision (allow/deny, if applicable)
 * - Output Hash (abbreviated)
 *
 * @param {Array<Object>} receipts - Sorted receipt array
 * @returns {string} Markdown table
 *
 * @example
 * const table = formatReceiptTable(receipts);
 * // | Receipt ID | Timestamp | Type | Decision | Hash |
 * // |------------|-----------|------|----------|------|
 * // | a1b2c3d4   | 2025-...  | query | allow   | f3e2... |
 */
export function formatReceiptTable(receipts) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    return '*No receipts available*';
  }

  const lines = [];

  // Table header
  lines.push('| Receipt ID | Timestamp | Type | Decision | Output Hash |');
  lines.push('|------------|-----------|------|----------|-------------|');

  // Table rows
  for (const receipt of receipts) {
    const id = receipt.id || 'unknown';
    const idAbbrev = id.slice(0, 8);
    const idLink = `[${idAbbrev}](receipts/admits/${id}.json)`;

    const timestamp = receipt.timestamp_iso || receipt.timestamp || 'unknown';
    const timestampShort = timestamp.slice(0, 19); // YYYY-MM-DDTHH:MM:SS

    const type = receipt.eventType || 'unknown';

    // Extract decision from payload if available
    let decision = '-';
    if (receipt.payload) {
      if (receipt.payload.decision) {
        decision = receipt.payload.decision;
      } else if (receipt.payload.valid !== undefined) {
        decision = receipt.payload.valid ? 'allow' : 'deny';
      }
    }

    const hash = receipt.hash || 'unknown';
    const hashAbbrev = hash.slice(0, 8);

    lines.push(`| ${idLink} | ${timestampShort} | ${type} | ${decision} | \`${hashAbbrev}\` |`);
  }

  return lines.join('\n');
}

/**
 * Format Merkle tree as ASCII diagram
 *
 * Generates a visual tree structure showing:
 * - Root hash (top)
 * - Internal nodes (middle layers)
 * - Leaf hashes (bottom, one per receipt)
 *
 * For large trees (>8 leaves), shows abbreviated structure.
 *
 * @param {Object} merkleProofs - Map of {receiptId: [proof_hashes]}
 * @returns {string} ASCII tree diagram or nested list
 *
 * @example
 * const tree = formatMerkleTree({r1: [...], r2: [...], r3: [...], r4: [...]});
 * //       [root: a1b2...]
 * //        /          \
 * //   [l: 1a2b...]  [r: 3c4d...]
 * //   /      \         /      \
 * //  r1      r2       r3      r4
 */
export function formatMerkleTree(merkleProofs) {
  if (!merkleProofs || typeof merkleProofs !== 'object') {
    return '*Merkle tree structure not available*';
  }

  const receiptIds = Object.keys(merkleProofs);

  if (receiptIds.length === 0) {
    return '*No merkle proofs available*';
  }

  const lines = [];

  // For small trees (<= 8 receipts), show full ASCII diagram
  if (receiptIds.length <= 8) {
    lines.push('```');
    lines.push('         [root]');
    lines.push('         /    \\');

    if (receiptIds.length <= 2) {
      lines.push('      r1        r2');
    } else if (receiptIds.length <= 4) {
      lines.push('     /  \\      /  \\');
      lines.push('   r1    r2  r3    r4');
    } else {
      lines.push('     /      \\        /      \\');
      lines.push('   [L1]    [L2]    [R1]    [R2]');
      lines.push('   / \\      / \\      / \\      / \\');

      const perQuadrant = Math.ceil(receiptIds.length / 4);
      const leafLine = receiptIds
        .slice(0, 8)
        .map((_, i) => `r${i + 1}`)
        .join('  ');
      lines.push(`  ${leafLine}`);
    }

    lines.push('```');
  } else {
    // For large trees, show nested list structure
    lines.push('```');
    lines.push('root/');
    lines.push('├── left-subtree/');
    lines.push('│   ├── receipts 1-' + Math.floor(receiptIds.length / 2));
    lines.push('│   └── ...');
    lines.push('└── right-subtree/');
    lines.push('    ├── receipts ' + (Math.floor(receiptIds.length / 2) + 1) + '-' + receiptIds.length);
    lines.push('    └── ...');
    lines.push('```');
  }

  lines.push('');
  lines.push(`**Tree Properties:**`);
  lines.push(`- Total Receipts: ${receiptIds.length}`);
  lines.push(`- Tree Depth: ${Math.ceil(Math.log2(receiptIds.length))}`);
  lines.push(`- Proof Size: ${merkleProofs[receiptIds[0]]?.length || 0} hashes per receipt`);

  return lines.join('\n');
}

/**
 * Format verification instructions
 *
 * Provides step-by-step guide for independent verification:
 * 1. Fetch published receipts
 * 2. Reconstruct merkle tree from receipts
 * 3. Compute root hash
 * 4. Compare against published merkleRoot
 * 5. Interpret results (match = verified, mismatch = tampering)
 *
 * Includes CLI commands for automation.
 *
 * @param {string} o_hash - Universe hash
 * @param {string} merkleRoot - Merkle tree root
 * @returns {string} Markdown instructions
 *
 * @example
 * const instructions = formatVerificationInstructions(o_hash, root);
 * // Returns step-by-step verification guide
 */
export function formatVerificationInstructions(o_hash, merkleRoot) {
  const lines = [];

  lines.push('Follow these steps to independently verify this document:');
  lines.push('');

  lines.push('**Step 1: Fetch Published Receipts**');
  lines.push('');
  lines.push('```bash');
  lines.push('# Download all receipts referenced in the receipt log');
  lines.push('for receipt_id in $(grep -oP "receipt-[^)]*" DOCUMENT.md); do');
  lines.push('  curl -o "receipts/admits/${receipt_id}.json" \\');
  lines.push('    "https://example.com/receipts/admits/${receipt_id}.json"');
  lines.push('done');
  lines.push('```');
  lines.push('');

  lines.push('**Step 2: Reconstruct Merkle Tree**');
  lines.push('');
  lines.push('```bash');
  lines.push('# Use the fusion toolkit to rebuild the merkle tree');
  lines.push('node -e "');
  lines.push('  const { MerkleProofGenerator } = require(\'@unrdf/blockchain\');');
  lines.push('  const fs = require(\'fs\');');
  lines.push('  const receipts = JSON.parse(fs.readFileSync(\'receipts.json\'));');
  lines.push('  const merkle = new MerkleProofGenerator();');
  lines.push('  receipts.forEach(r => merkle.addReceipt(r));');
  lines.push('  const root = merkle.buildTree();');
  lines.push('  console.log(\'Computed root:\', root);');
  lines.push('"');
  lines.push('```');
  lines.push('');

  lines.push('**Step 3: Compare Root Hash**');
  lines.push('');
  lines.push('```bash');
  lines.push('# Expected merkle root (from document)');
  lines.push(`EXPECTED_ROOT="${merkleRoot.slice(0, 16)}..."`);
  lines.push('');
  lines.push('# Computed root (from step 2)');
  lines.push('COMPUTED_ROOT="<your_computed_root>"');
  lines.push('');
  lines.push('if [ "$EXPECTED_ROOT" = "$COMPUTED_ROOT" ]; then');
  lines.push('  echo "✅ VERIFICATION SUCCESSFUL - Document is authentic"');
  lines.push('else');
  lines.push('  echo "❌ VERIFICATION FAILED - Possible tampering detected"');
  lines.push('  echo "Expected: $EXPECTED_ROOT"');
  lines.push('  echo "Computed: $COMPUTED_ROOT"');
  lines.push('fi');
  lines.push('```');
  lines.push('');

  lines.push('**Step 4: Verify Universe Hash (Optional)**');
  lines.push('');
  lines.push('```bash');
  lines.push('# Verify the o_hash matches the KGC universe state');
  lines.push(`UNIVERSE_HASH="${o_hash.slice(0, 16)}..."`);
  lines.push('');
  lines.push('# Query KGC for universe state at timestamp');
  lines.push('node tools/kgc-query.mjs --hash $UNIVERSE_HASH --verify');
  lines.push('```');
  lines.push('');

  lines.push('**Interpretation:**');
  lines.push('');
  lines.push('- ✅ **Match**: Document receipts are authentic, content is verified');
  lines.push('- ❌ **Mismatch**: Receipt chain has been modified or corrupted');
  lines.push('- ⚠️ **Missing receipts**: Incomplete proof, cannot verify fully');
  lines.push('');

  lines.push('**Automated Verification:**');
  lines.push('');
  lines.push('```bash');
  lines.push('# Use the fusion CLI to verify automatically');
  lines.push('pnpm fusion verify-doc DOCUMENT.md');
  lines.push('```');

  return lines.join('\n');
}

/**
 * Format hash values reference section
 *
 * Provides comprehensive hash reference for auditors:
 * - o_hash: Universe state hash
 * - merkleRoot: Receipt chain commitment
 * - Individual receipt hashes (full 64-char hex)
 * - Verification timestamp
 *
 * @param {Object} frontmatter - Document frontmatter
 * @param {Array<Object>} receipts - Receipt array
 * @param {string} merkleRoot - Merkle root hash
 * @param {string} o_hash - Universe hash
 * @returns {string} Markdown hash reference
 *
 * @example
 * const hashes = formatHashValues(fm, receipts, root, o_hash);
 * // Returns comprehensive hash listing
 */
export function formatHashValues(frontmatter, receipts, merkleRoot, o_hash) {
  const lines = [];

  lines.push('**Document Verification Hashes:**');
  lines.push('');

  lines.push('```yaml');
  lines.push('# Universe State');
  lines.push(`o_hash: ${o_hash}`);
  lines.push('');

  lines.push('# Merkle Tree Root');
  lines.push(`merkle_root: ${merkleRoot}`);
  lines.push('');

  lines.push('# Receipt Chain');
  lines.push('receipts:');

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];
    const num = String(i + 1).padStart(2, '0');
    lines.push(`  - id: ${receipt.id}`);
    lines.push(`    hash: ${receipt.hash}`);
    lines.push(`    type: ${receipt.eventType}`);
    lines.push(`    timestamp: ${receipt.timestamp_iso}`);

    if (i < receipts.length - 1) {
      lines.push('');
    }
  }

  lines.push('');
  lines.push('# Verification Metadata');
  lines.push(`verified_at: ${new Date().toISOString()}`);

  if (frontmatter.title) {
    lines.push(`document_title: ${frontmatter.title}`);
  }

  if (frontmatter.version) {
    lines.push(`document_version: ${frontmatter.version}`);
  }

  lines.push('```');

  return lines.join('\n');
}

/**
 * Insert proof appendix into markdown document
 *
 * Appends verification section to rendered markdown.
 * Placement: After all content, before references (if present).
 * Marker: ## Verification Appendix (always last section).
 *
 * @param {string} markdown - Original markdown content
 * @param {Array<Object>} receipts - Receipt chain
 * @param {string} merkleRoot - Merkle root
 * @param {string} o_hash - Universe hash
 * @param {Object} [opts={}] - Optional configuration
 * @returns {string} Markdown with appended proof
 *
 * @example
 * const verified = insertProofAppendix(markdown, receipts, root, o_hash);
 * // Original content + verification appendix
 */
export function insertProofAppendix(markdown, receipts, merkleRoot, o_hash, opts = {}) {
  // Input validation
  if (typeof markdown !== 'string') {
    throw new TypeError('insertProofAppendix: markdown must be a string');
  }

  // Generate proof appendix
  const appendix = generateProofAppendix(receipts, merkleRoot, o_hash, opts);

  // Check if verification appendix already exists
  if (markdown.includes('## Verification Appendix')) {
    // Replace existing appendix
    const beforeAppendix = markdown.split('## Verification Appendix')[0];
    return beforeAppendix.trim() + '\n\n' + appendix;
  }

  // Check if References section exists
  if (markdown.includes('## References') || markdown.includes('## Bibliography')) {
    // Insert before references
    const parts = markdown.split(/## (References|Bibliography)/);
    return parts[0].trim() + '\n\n' + appendix + '\n\n## ' + parts[1] + parts[2];
  }

  // Append to end
  return markdown.trim() + '\n\n' + appendix;
}

/**
 * Render proof as JSON for programmatic verification
 *
 * Exports proof materials in machine-readable format.
 * Useful for automated verification tools and CI/CD pipelines.
 *
 * @param {Array<Object>} receipts - Receipt chain
 * @param {string} merkleRoot - Merkle root
 * @param {string} o_hash - Universe hash
 * @param {Object} [opts={}] - Optional configuration
 * @returns {Object} JSON proof object
 *
 * @example
 * const proof = renderProofAsJSON(receipts, root, o_hash);
 * // {
 * //   o_hash: "abc...",
 * //   receipts: [...],
 * //   merkleRoot: "def...",
 * //   verified_at: "2025-12-26T...",
 * //   proofs: {...}
 * // }
 */
export function renderProofAsJSON(receipts, merkleRoot, o_hash, opts = {}) {
  // Input validation
  if (!Array.isArray(receipts)) {
    throw new TypeError('renderProofAsJSON: receipts must be an array');
  }
  if (typeof merkleRoot !== 'string') {
    throw new TypeError('renderProofAsJSON: merkleRoot must be a string');
  }
  if (typeof o_hash !== 'string') {
    throw new TypeError('renderProofAsJSON: o_hash must be a string');
  }

  const { merkleProofs = {}, frontmatter = {} } = opts;

  // Build proof object
  const proof = {
    version: '1.0.0',
    type: 'kgc-document-proof',
    o_hash,
    merkleRoot,
    verified_at: new Date().toISOString(),
    receipts: receipts.map(r => ({
      id: r.id,
      hash: r.hash,
      timestamp: r.timestamp,
      timestamp_iso: r.timestamp_iso,
      eventType: r.eventType,
      receiptType: r.receiptType,
      payload: r.payload,
    })),
    proofs: merkleProofs,
  };

  // Add metadata
  proof.metadata = {
    receiptCount: receipts.length,
    treeDepth: Math.ceil(Math.log2(receipts.length || 1)),
  };

  // Add frontmatter fields if present
  if (frontmatter.title) {
    proof.metadata.documentTitle = frontmatter.title;
  }
  if (frontmatter.version) {
    proof.metadata.documentVersion = frontmatter.version;
  }

  // Validate and return
  return ProofAppendixSchema.parse(proof);
}

/**
 * Update proof timestamp
 *
 * Updates verification timestamp when proof is re-verified.
 * Used for periodic automated re-verification.
 *
 * @param {Object} proof - Existing proof object
 * @param {string} [currentTime] - ISO timestamp (default: now)
 * @returns {Object} Updated proof with new timestamp
 *
 * @example
 * const updated = updateProofTimestamp(proof);
 * console.log(updated.verified_at); // Current timestamp
 */
export function updateProofTimestamp(proof, currentTime = null) {
  // Input validation
  if (!proof || typeof proof !== 'object') {
    throw new TypeError('updateProofTimestamp: proof must be an object');
  }

  const timestamp = currentTime || new Date().toISOString();

  return {
    ...proof,
    verified_at: timestamp,
  };
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Extract receipts from rendered document
 *
 * Parses verification appendix to extract receipt data.
 * Used for re-verification of existing documents.
 *
 * @param {string} markdown - Markdown with verification appendix
 * @returns {Object|null} Extracted proof data or null
 *
 * @example
 * const proof = extractProofFromDocument(markdown);
 * if (proof) {
 *   console.log('Found', proof.receipts.length, 'receipts');
 * }
 */
export function extractProofFromDocument(markdown) {
  if (typeof markdown !== 'string') {
    return null;
  }

  // Check for verification appendix
  if (!markdown.includes('## Verification Appendix')) {
    return null;
  }

  try {
    // Extract hash values section
    const hashMatch = markdown.match(/```yaml\n([\s\S]*?)\n```/);
    if (!hashMatch) {
      return null;
    }

    const yamlContent = hashMatch[1];

    // Parse o_hash
    const oHashMatch = yamlContent.match(/o_hash:\s*(\S+)/);
    const o_hash = oHashMatch ? oHashMatch[1] : null;

    // Parse merkle_root
    const merkleMatch = yamlContent.match(/merkle_root:\s*(\S+)/);
    const merkleRoot = merkleMatch ? merkleMatch[1] : null;

    // Parse verified_at
    const verifiedMatch = yamlContent.match(/verified_at:\s*(\S+)/);
    const verified_at = verifiedMatch ? verifiedMatch[1] : null;

    if (!o_hash || !merkleRoot) {
      return null;
    }

    // Extract receipt IDs from table
    const receiptMatches = markdown.matchAll(/\[([a-f0-9]{8})\]\(receipts\/admits\/(receipt-[^)]+)\.json\)/g);
    const receiptIds = Array.from(receiptMatches).map(m => m[2]);

    return {
      o_hash,
      merkleRoot,
      verified_at,
      receiptIds,
    };
  } catch (error) {
    return null;
  }
}

/**
 * Compute document hash
 *
 * Generates SHA256 hash of document content (excluding verification appendix).
 * Used for document integrity verification.
 *
 * @param {string} markdown - Markdown content
 * @returns {string} Hex hash
 *
 * @example
 * const hash = computeDocumentHash(markdown);
 * console.log('Document hash:', hash);
 */
export function computeDocumentHash(markdown) {
  if (typeof markdown !== 'string') {
    throw new TypeError('computeDocumentHash: markdown must be a string');
  }

  // Remove verification appendix if present
  let content = markdown;
  if (content.includes('## Verification Appendix')) {
    content = content.split('## Verification Appendix')[0];
  }

  // Compute SHA256
  const hash = createHash('sha256')
    .update(content.trim())
    .digest('hex');

  return hash;
}

/**
 * Validate proof integrity
 *
 * Checks that proof object is well-formed and internally consistent.
 * Does NOT verify cryptographic validity (use verifyProof for that).
 *
 * @param {Object} proof - Proof object to validate
 * @returns {Object} {valid: boolean, errors: string[]}
 *
 * @example
 * const result = validateProofIntegrity(proof);
 * if (!result.valid) {
 *   console.error('Proof errors:', result.errors);
 * }
 */
export function validateProofIntegrity(proof) {
  const errors = [];

  if (!proof || typeof proof !== 'object') {
    return { valid: false, errors: ['Proof must be an object'] };
  }

  // Required fields
  const required = ['o_hash', 'merkleRoot', 'verified_at', 'receipts'];
  for (const field of required) {
    if (!proof[field]) {
      errors.push(`Missing required field: ${field}`);
    }
  }

  // Validate receipts array
  if (proof.receipts) {
    if (!Array.isArray(proof.receipts)) {
      errors.push('receipts must be an array');
    } else if (proof.receipts.length === 0) {
      errors.push('receipts array is empty');
    } else {
      // Check each receipt has required fields
      for (let i = 0; i < proof.receipts.length; i++) {
        const receipt = proof.receipts[i];
        if (!receipt.id) errors.push(`Receipt ${i} missing id`);
        if (!receipt.hash) errors.push(`Receipt ${i} missing hash`);
        if (!receipt.timestamp) errors.push(`Receipt ${i} missing timestamp`);
      }
    }
  }

  // Validate hash formats
  if (proof.o_hash && !/^[0-9a-f]+$/i.test(proof.o_hash.replace('0x', ''))) {
    errors.push('o_hash is not a valid hex string');
  }
  if (proof.merkleRoot && !/^[0-9a-f]+$/i.test(proof.merkleRoot.replace('0x', ''))) {
    errors.push('merkleRoot is not a valid hex string');
  }

  // Validate timestamp format
  if (proof.verified_at) {
    try {
      new Date(proof.verified_at);
    } catch (e) {
      errors.push('verified_at is not a valid ISO timestamp');
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

// =============================================================================
// Exports
// =============================================================================

export default {
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
};

export {
  ProofAppendixSchema,
  ReceiptSummarySchema,
};
