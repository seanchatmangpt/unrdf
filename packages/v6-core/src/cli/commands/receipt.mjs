/**
 * V6 Receipt CLI Commands
 *
 * Commands for receipt anchoring, verification, and proof generation.
 *
 * @module @unrdf/v6-core/cli/commands/receipt
 */

import { Command } from 'commander';
import { buildMerkleTree, getProofPath } from '../../receipts/merkle/tree.mjs';
import { anchorToChain } from '../../receipts/merkle/anchor.mjs';
import { verifyChain, reconstructChainState } from '../../receipts/merkle/proofchain.mjs';
import fs from 'fs/promises';

export function createReceiptCommand() {
  const receipt = new Command('receipt')
    .description('Receipt anchoring and verification commands');

  receipt
    .command('anchor')
    .description('Anchor merkle root to blockchain')
    .requiredOption('--root <hash>', 'Merkle root hash to anchor')
    .option('--network <network>', 'Blockchain network', 'localhost')
    .option('--output <file>', 'Output file for anchor receipt')
    .action(async (opts) => {
      try {
        console.log(`Anchoring merkle root: ${opts.root}`);
        const anchorReceipt = await anchorToChain(opts.root, { network: opts.network });
        console.log('Transaction Hash:', anchorReceipt.txHash);
        if (opts.output) {
          await fs.writeFile(opts.output, JSON.stringify(anchorReceipt, null, 2));
        }
      } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
      }
    });

  receipt
    .command('verify-chain')
    .description('Verify receipt chain integrity')
    .requiredOption('--file <path>', 'JSON file containing receipt chain')
    .action(async (opts) => {
      try {
        const data = await fs.readFile(opts.file, 'utf8');
        const receipts = JSON.parse(data);
        const result = await verifyChain(receipts);
        console.log('Valid:', result.valid);
        console.log('Tampered Receipts:', result.tamperedReceipts.length);
        process.exit(result.valid ? 0 : 1);
      } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
      }
    });

  receipt
    .command('prove')
    .description('Generate merkle inclusion proof for a receipt')
    .requiredOption('--receipt <id>', 'Receipt ID to prove')
    .requiredOption('--file <path>', 'JSON file containing receipt chain')
    .option('--output <file>', 'Output file for proof')
    .action(async (opts) => {
      try {
        const data = await fs.readFile(opts.file, 'utf8');
        const receipts = JSON.parse(data);
        const tree = await buildMerkleTree(receipts);
        const proof = await getProofPath(tree, opts.receipt, receipts);
        console.log('Proof generated:', proof);
        if (opts.output) {
          await fs.writeFile(opts.output, JSON.stringify(proof, null, 2));
        }
      } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
      }
    });

  return receipt;
}

export default createReceiptCommand;
