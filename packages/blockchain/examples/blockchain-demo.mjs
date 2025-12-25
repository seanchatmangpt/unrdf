#!/usr/bin/env node
/**
 * Blockchain Integration Demo - Comprehensive demonstration
 *
 * Demonstrates:
 * 1. Contract deployment
 * 2. Single receipt anchoring
 * 3. Batch anchoring
 * 4. Merkle tree anchoring
 * 5. Verification
 * 6. Gas cost analysis
 *
 * Usage:
 *   node examples/blockchain-demo.mjs
 *
 * Prerequisites:
 *   - Local Ethereum node (Hardhat/Ganache) at http://localhost:8545
 *   - Or use test network (Sepolia, etc.)
 */

import { ReceiptAnchorer } from '../src/anchoring/receipt-anchorer.mjs';
import { WorkflowVerifier, estimateGasCosts } from '../src/contracts/workflow-verifier.mjs';
import { MerkleProofGenerator, calculateGasSavings } from '../src/merkle/merkle-proof-generator.mjs';
import { ethers } from 'ethers';

// =============================================================================
// Demo Configuration
// =============================================================================

const CONFIG = {
  provider: process.env.ETH_PROVIDER || 'http://localhost:8545',
  // Default Hardhat test account
  privateKey:
    process.env.PRIVATE_KEY ||
    '0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80',
};

// =============================================================================
// Mock Receipt Generation
// =============================================================================

/**
 * Generate mock YAWL receipt for demo
 */
function generateMockReceipt(id) {
  const receipt = {
    id: `receipt-${id}`,
    timestamp: Date.now(),
    event: 'TASK_COMPLETED',
    caseId: `case-${Math.floor(id / 10)}`,
    taskId: `task-${id}`,
    payload: {
      decision: 'APPROVE',
      actor: 'demo-user',
    },
  };

  // Compute simple hash
  const data = JSON.stringify(receipt);
  const encoder = new TextEncoder();
  const hashBytes = ethers.keccak256(encoder.encode(data));

  receipt.hash = hashBytes;

  return receipt;
}

// =============================================================================
// Demo Functions
// =============================================================================

/**
 * Step 1: Deploy WorkflowVerifier contract
 */
async function deployContract() {
  console.log('\n=== STEP 1: Deploy Contract ===\n');

  const verifier = new WorkflowVerifier(CONFIG);

  console.log('Deploying WorkflowVerifier contract...');
  const address = await verifier.deploy();

  console.log(`✅ Contract deployed at: ${address}`);

  return { verifier, address };
}

/**
 * Step 2: Single receipt anchoring
 */
async function anchorSingleReceipt(contractAddress) {
  console.log('\n=== STEP 2: Single Receipt Anchoring ===\n');

  const anchorer = new ReceiptAnchorer({
    ...CONFIG,
    contractAddress,
  });

  const receipt = generateMockReceipt(1);
  console.log(`Anchoring receipt: ${receipt.id}`);
  console.log(`Receipt hash: ${receipt.hash}`);

  const result = await anchorer.anchorReceipt(receipt);

  console.log(`✅ Anchored at block: ${result.blockNumber}`);
  console.log(`   Transaction: ${result.txHash}`);
  console.log(`   Gas used: ${result.gasUsed.toString()}`);
  console.log(`   Cost: ${result.costETH} ETH`);

  return { receipt, result };
}

/**
 * Step 3: Batch anchoring
 */
async function anchorBatch(contractAddress) {
  console.log('\n=== STEP 3: Batch Anchoring ===\n');

  const anchorer = new ReceiptAnchorer({
    ...CONFIG,
    contractAddress,
  });

  // Generate 5 receipts
  const receipts = Array.from({ length: 5 }, (_, i) => generateMockReceipt(10 + i));

  console.log(`Anchoring batch of ${receipts.length} receipts...`);

  const result = await anchorer.anchorBatch(receipts);

  console.log(`✅ Batch anchored at block: ${result.blockNumber}`);
  console.log(`   Transaction: ${result.txHash}`);
  console.log(`   Gas used: ${result.gasUsed.toString()}`);
  console.log(`   Cost: ${result.costETH} ETH`);
  console.log(`   Receipts: ${result.receiptCount}`);

  return { receipts, result };
}

/**
 * Step 4: Merkle tree anchoring
 */
async function anchorMerkleTree(contractAddress) {
  console.log('\n=== STEP 4: Merkle Tree Anchoring ===\n');

  // Generate larger batch
  const receipts = Array.from({ length: 100 }, (_, i) => generateMockReceipt(100 + i));

  console.log(`Building Merkle tree for ${receipts.length} receipts...`);

  // Build Merkle tree
  const generator = new MerkleProofGenerator();
  generator.addReceipts(receipts);
  const root = generator.buildTree();

  console.log(`✅ Merkle root: ${root}`);

  const treeInfo = generator.getTreeInfo();
  console.log(`   Tree depth: ${treeInfo.depth}`);
  console.log(`   Leaf count: ${treeInfo.leafCount}`);

  // Anchor root
  const anchorer = new ReceiptAnchorer({
    ...CONFIG,
    contractAddress,
  });

  console.log('\nAnchoring Merkle root...');
  const result = await anchorer.anchorMerkleRoot(root, receipts.length);

  console.log(`✅ Merkle root anchored at block: ${result.blockNumber}`);
  console.log(`   Transaction: ${result.txHash}`);
  console.log(`   Gas used: ${result.gasUsed.toString()}`);
  console.log(`   Cost: ${result.costETH} ETH`);

  // Generate and verify proof for first receipt
  console.log('\nGenerating proof for first receipt...');
  const proof = generator.generateProof(receipts[0]);
  const isValid = generator.verifyProof(proof);

  console.log(`✅ Proof generated (${proof.proof.length} steps)`);
  console.log(`   Valid: ${isValid}`);

  return { receipts, root, generator, result };
}

/**
 * Step 5: Verification
 */
async function verifyReceipts(contractAddress, receipt) {
  console.log('\n=== STEP 5: On-Chain Verification ===\n');

  const anchorer = new ReceiptAnchorer({
    ...CONFIG,
    contractAddress,
  });

  console.log(`Verifying receipt: ${receipt.id}`);

  const verification = await anchorer.verifyReceipt(receipt);

  console.log(`✅ Verification result:`);
  console.log(`   Is anchored: ${verification.isAnchored}`);
  if (verification.isAnchored) {
    console.log(`   Block number: ${verification.blockNumber}`);
  }

  return verification;
}

/**
 * Step 6: Gas cost analysis
 */
async function analyzeGasCosts(singleResult, batchResult, merkleResult) {
  console.log('\n=== STEP 6: Gas Cost Analysis ===\n');

  const provider = new ethers.JsonRpcProvider(CONFIG.provider);
  const feeData = await provider.getFeeData();
  const gasPrice = feeData.gasPrice;

  console.log('Individual Anchoring:');
  console.log(`  Gas per receipt: ~50,000`);
  console.log(`  Cost: ${singleResult.costETH} ETH`);

  console.log('\nBatch Anchoring (5 receipts):');
  console.log(`  Total gas: ${batchResult.gasUsed.toString()}`);
  console.log(`  Gas per receipt: ~${Number(batchResult.gasUsed) / batchResult.receiptCount}`);
  console.log(`  Cost: ${batchResult.costETH} ETH`);

  console.log('\nMerkle Anchoring (100 receipts):');
  console.log(`  Total gas: ${merkleResult.gasUsed.toString()}`);
  console.log(`  Gas per receipt: ~${Number(merkleResult.gasUsed) / merkleResult.receiptCount}`);
  console.log(`  Cost: ${merkleResult.costETH} ETH`);

  // Calculate savings
  const savings = calculateGasSavings(100, gasPrice);
  console.log('\nMerkle Savings (vs individual):');
  console.log(`  Saved gas: ${savings.savedGas}`);
  console.log(`  Savings: ${savings.savingsPercentage.toFixed(2)}%`);

  console.log('\nRecommendations:');
  console.log('  - Use individual anchoring for < 3 receipts');
  console.log('  - Use batch anchoring for 3-50 receipts');
  console.log('  - Use Merkle anchoring for > 50 receipts');
}

/**
 * Step 7: Contract statistics
 */
async function showContractStats(verifier) {
  console.log('\n=== STEP 7: Contract Statistics ===\n');

  const stats = await verifier.getStats();

  console.log('Contract Statistics:');
  console.log(`  Total receipts anchored: ${stats.totalAnchored}`);
  console.log(`  Total Merkle roots: ${stats.totalMerkleRoots}`);
  console.log(`  Owner: ${stats.owner}`);
}

// =============================================================================
// Main Demo
// =============================================================================

async function main() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║   UNRDF Blockchain Integration - Comprehensive Demo       ║');
  console.log('╚════════════════════════════════════════════════════════════╝');

  try {
    // Check provider
    const provider = new ethers.JsonRpcProvider(CONFIG.provider);
    const network = await provider.getNetwork();
    console.log(`\n✅ Connected to network: ${network.name} (chainId: ${network.chainId})`);

    // Run demo steps
    const { verifier, address } = await deployContract();
    const { receipt, result: singleResult } = await anchorSingleReceipt(address);
    const { result: batchResult } = await anchorBatch(address);
    const { result: merkleResult } = await anchorMerkleTree(address);
    await verifyReceipts(address, receipt);
    await analyzeGasCosts(singleResult, batchResult, merkleResult);
    await showContractStats(verifier);

    console.log('\n╔════════════════════════════════════════════════════════════╗');
    console.log('║   Demo completed successfully! ✅                         ║');
    console.log('╚════════════════════════════════════════════════════════════╝');
  } catch (error) {
    console.error('\n❌ Demo failed:', error.message);
    console.error('\nTroubleshooting:');
    console.error('  1. Ensure Ethereum node is running at', CONFIG.provider);
    console.error('  2. Try: npx hardhat node (in separate terminal)');
    console.error('  3. Check private key has funds');
    process.exit(1);
  }
}

// Run demo
main();
