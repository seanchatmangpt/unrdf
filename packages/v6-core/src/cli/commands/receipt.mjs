/**
 * @fileoverview Receipt noun commands
 *
 * Implements: kgc receipt <verb>
 * - verify <id> - Verify receipt
 * - chain <id> - Show receipt chain
 * - anchor <root> - Anchor merkle root to chain
 * - export <id> - Export receipt
 */

import { z } from 'zod';
import { buildMerkleTree, getProofPath } from '../../receipts/merkle/tree.mjs';
import { anchorToChain } from '../../receipts/merkle/anchor.mjs';
import { verifyChain } from '../../receipts/merkle/proofchain.mjs';

/**
 * Argument schemas for receipt commands.
 */
const VerifyArgsSchema = z.object({
  file: z.string().describe('JSON file containing receipt or receipt chain'),
  id: z.string().optional().describe('Specific receipt ID to verify')
});

const ChainArgsSchema = z.object({
  file: z.string().describe('JSON file containing receipt chain'),
  output: z.string().optional().describe('Output file for chain visualization')
});

const AnchorArgsSchema = z.object({
  root: z.string().describe('Merkle root hash to anchor'),
  network: z.string().optional().default('localhost').describe('Blockchain network'),
  output: z.string().optional().describe('Output file for anchor receipt')
});

const ExportArgsSchema = z.object({
  file: z.string().describe('JSON file containing receipt'),
  format: z.enum(['json', 'yaml', 'xml']).optional().default('json'),
  output: z.string().optional().describe('Output file path')
});

/**
 * Verify receipt or receipt chain.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Verification result
 */
async function verifyReceipt(args) {
  const { readFile } = await import('fs/promises');

  const data = await readFile(args.file, 'utf8');
  const receipts = JSON.parse(data);

  // Check if it's a single receipt or chain
  const isArray = Array.isArray(receipts);

  if (isArray) {
    // Verify chain
    const result = await verifyChain(receipts);

    return {
      verified: result.valid,
      chainLength: receipts.length,
      tamperedReceipts: result.tamperedReceipts.length,
      details: result
    };
  } else {
    // Verify single receipt
    const receiptId = args.id || receipts.id;

    // Basic receipt validation
    const isValid = receipts.id && receipts.timestamp && receipts.merkleProof;

    return {
      verified: isValid,
      receiptId,
      receipt: receipts
    };
  }
}

/**
 * Show receipt chain.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Chain visualization
 */
async function chainReceipt(args) {
  const { readFile, writeFile } = await import('fs/promises');

  const data = await readFile(args.file, 'utf8');
  const receipts = JSON.parse(data);

  if (!Array.isArray(receipts)) {
    throw new Error('File must contain an array of receipts');
  }

  // Build chain visualization
  const chain = receipts.map((r, i) => ({
    index: i,
    id: r.id,
    timestamp: r.timestamp,
    previousHash: r.previousHash || null,
    merkleRoot: r.merkleRoot || null
  }));

  if (args.output) {
    await writeFile(args.output, JSON.stringify(chain, null, 2));
  }

  return {
    chainLength: chain.length,
    firstReceipt: chain[0]?.timestamp,
    lastReceipt: chain[chain.length - 1]?.timestamp,
    chain
  };
}

/**
 * Anchor merkle root to blockchain.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Anchor result
 */
async function anchorReceipt(args) {
  const anchorResult = await anchorToChain(args.root, {
    network: args.network
  });

  if (args.output) {
    const { writeFile } = await import('fs/promises');
    await writeFile(args.output, JSON.stringify(anchorResult, null, 2));
  }

  return {
    anchored: true,
    txHash: anchorResult.txHash,
    network: args.network,
    timestamp: anchorResult.timestamp
  };
}

/**
 * Export receipt to format.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Export result
 */
async function exportReceipt(args) {
  const { readFile, writeFile } = await import('fs/promises');

  const data = await readFile(args.file, 'utf8');
  const receipt = JSON.parse(data);

  let exportData = receipt;

  if (args.format === 'yaml') {
    // Convert to YAML-friendly format
    exportData = {
      ...receipt,
      _format: 'yaml'
    };
  } else if (args.format === 'xml') {
    // Convert to XML-friendly format
    exportData = {
      _format: 'xml',
      receipt
    };
  }

  const outputPath = args.output || `receipt.${args.format}`;
  await writeFile(outputPath, JSON.stringify(exportData, null, 2));

  return {
    exported: true,
    format: args.format,
    file: outputPath
  };
}

/**
 * Receipt extension for V6 CLI.
 */
export const receiptExtension = {
  id: '@unrdf/v6-core/receipt',
  nouns: {
    receipt: {
      description: 'Receipt anchoring and verification commands',
      verbs: {
        verify: {
          description: 'Verify receipt or receipt chain',
          handler: verifyReceipt,
          argsSchema: VerifyArgsSchema,
          meta: {}
        },
        chain: {
          description: 'Show receipt chain lineage',
          handler: chainReceipt,
          argsSchema: ChainArgsSchema,
          meta: {}
        },
        anchor: {
          description: 'Anchor merkle root to blockchain',
          handler: anchorReceipt,
          argsSchema: AnchorArgsSchema,
          meta: {}
        },
        export: {
          description: 'Export receipt to format',
          handler: exportReceipt,
          argsSchema: ExportArgsSchema,
          meta: {}
        }
      }
    }
  },
  priority: 100
};

export default receiptExtension;
