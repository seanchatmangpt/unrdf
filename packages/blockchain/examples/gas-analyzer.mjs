#!/usr/bin/env node
/**
 * Gas Cost Analyzer - Detailed gas cost analysis for blockchain operations
 *
 * Analyzes and compares gas costs for different anchoring strategies
 * to help choose optimal approach based on batch size.
 */

import { ethers } from 'ethers';
import { calculateGasSavings } from '../src/merkle/merkle-proof-generator.mjs';

// =============================================================================
// Configuration
// =============================================================================

const PROVIDER = process.env.ETH_PROVIDER || 'http://localhost:8545';

// Gas estimates based on contract analysis
const GAS_ESTIMATES = {
  deployment: 500000n,
  singleAnchor: 50000n,
  batchBase: 30000n,
  batchPerReceipt: 20000n,
  merkleAnchor: 60000n,
};

// =============================================================================
// Analysis Functions
// =============================================================================

/**
 * Get current gas prices from network
 */
async function getGasPrices() {
  const provider = new ethers.JsonRpcProvider(PROVIDER);
  const feeData = await provider.getFeeData();

  return {
    gasPrice: feeData.gasPrice,
    maxFeePerGas: feeData.maxFeePerGas,
    maxPriorityFeePerGas: feeData.maxPriorityFeePerGas,
    network: await provider.getNetwork(),
  };
}

/**
 * Calculate cost for individual anchoring
 */
function calculateIndividualCost(count, gasPrice) {
  const totalGas = GAS_ESTIMATES.singleAnchor * BigInt(count);
  const costWei = totalGas * gasPrice;
  const costETH = ethers.formatEther(costWei);

  return {
    method: 'Individual',
    count,
    gasPerReceipt: GAS_ESTIMATES.singleAnchor.toString(),
    totalGas: totalGas.toString(),
    costWei: costWei.toString(),
    costETH,
  };
}

/**
 * Calculate cost for batch anchoring
 */
function calculateBatchCost(count, gasPrice) {
  const totalGas = GAS_ESTIMATES.batchBase + GAS_ESTIMATES.batchPerReceipt * BigInt(count);
  const costWei = totalGas * gasPrice;
  const costETH = ethers.formatEther(costWei);

  return {
    method: 'Batch',
    count,
    gasPerReceipt: (Number(totalGas) / count).toFixed(0),
    totalGas: totalGas.toString(),
    costWei: costWei.toString(),
    costETH,
  };
}

/**
 * Calculate cost for Merkle anchoring
 */
function calculateMerkleCost(count, gasPrice) {
  const totalGas = GAS_ESTIMATES.merkleAnchor;
  const costWei = totalGas * gasPrice;
  const costETH = ethers.formatEther(costWei);

  return {
    method: 'Merkle',
    count,
    gasPerReceipt: (Number(totalGas) / count).toFixed(0),
    totalGas: totalGas.toString(),
    costWei: costWei.toString(),
    costETH,
  };
}

/**
 * Compare all methods for a given count
 */
function compareAll(count, gasPrice) {
  return {
    individual: calculateIndividualCost(count, gasPrice),
    batch: calculateBatchCost(count, gasPrice),
    merkle: calculateMerkleCost(count, gasPrice),
  };
}

/**
 * Find optimal strategy for given count
 */
function findOptimalStrategy(count, gasPrice) {
  const costs = compareAll(count, gasPrice);

  const sorted = [
    { name: 'Individual', ...costs.individual },
    { name: 'Batch', ...costs.batch },
    { name: 'Merkle', ...costs.merkle },
  ].sort((a, b) => BigInt(a.costWei) - BigInt(b.costWei));

  return sorted[0];
}

/**
 * Generate cost breakdown table
 */
function generateCostTable(batchSizes, gasPrice) {
  const table = [];

  for (const size of batchSizes) {
    const individual = calculateIndividualCost(size, gasPrice);
    const batch = calculateBatchCost(size, gasPrice);
    const merkle = calculateMerkleCost(size, gasPrice);

    const optimal = findOptimalStrategy(size, gasPrice);

    table.push({
      size,
      individual: individual.costETH,
      batch: batch.costETH,
      merkle: merkle.costETH,
      optimal: optimal.name,
      savings: (
        ((BigInt(individual.costWei) - BigInt(optimal.costWei)) * 100n) /
        BigInt(individual.costWei)
      ).toString(),
    });
  }

  return table;
}

// =============================================================================
// Display Functions
// =============================================================================

/**
 * Print analysis header
 */
function printHeader(gasPrices) {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║     Blockchain Anchoring - Gas Cost Analysis             ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  console.log('Network Information:');
  console.log(`  Chain ID: ${gasPrices.network.chainId}`);
  console.log(`  Network: ${gasPrices.network.name}`);
  console.log(`  Gas Price: ${ethers.formatUnits(gasPrices.gasPrice, 'gwei')} Gwei`);
  if (gasPrices.maxFeePerGas) {
    console.log(`  Max Fee: ${ethers.formatUnits(gasPrices.maxFeePerGas, 'gwei')} Gwei`);
  }
  console.log();
}

/**
 * Print cost comparison table
 */
function printCostTable(table) {
  console.log('Cost Comparison by Batch Size:\n');
  console.log('┌──────────┬──────────────┬──────────────┬──────────────┬──────────────┬─────────────┐');
  console.log('│ Receipts │ Individual   │ Batch        │ Merkle       │ Optimal      │ Savings     │');
  console.log('├──────────┼──────────────┼──────────────┼──────────────┼──────────────┼─────────────┤');

  for (const row of table) {
    const size = row.size.toString().padStart(8);
    const individual = parseFloat(row.individual).toFixed(6).padStart(12);
    const batch = parseFloat(row.batch).toFixed(6).padStart(12);
    const merkle = parseFloat(row.merkle).toFixed(6).padStart(12);
    const optimal = row.optimal.padEnd(12);
    const savings = `${row.savings}%`.padStart(11);

    console.log(`│ ${size} │ ${individual} │ ${batch} │ ${merkle} │ ${optimal} │ ${savings} │`);
  }

  console.log('└──────────┴──────────────┴──────────────┴──────────────┴──────────────┴─────────────┘\n');
}

/**
 * Print recommendations
 */
function printRecommendations(gasPrice) {
  console.log('Recommendations:\n');

  // Find crossover points
  let batchCrossover = 1;
  while (
    calculateIndividualCost(batchCrossover, gasPrice).costWei >
    calculateBatchCost(batchCrossover, gasPrice).costWei
  ) {
    batchCrossover++;
    if (batchCrossover > 100) break;
  }

  let merkleCrossover = 1;
  while (
    calculateBatchCost(merkleCrossover, gasPrice).costWei >
    calculateMerkleCost(merkleCrossover, gasPrice).costWei
  ) {
    merkleCrossover++;
    if (merkleCrossover > 200) break;
  }

  console.log(`  ✓ Use INDIVIDUAL anchoring for < ${batchCrossover} receipts`);
  console.log(`  ✓ Use BATCH anchoring for ${batchCrossover}-${merkleCrossover} receipts`);
  console.log(`  ✓ Use MERKLE anchoring for > ${merkleCrossover} receipts`);
  console.log();
}

/**
 * Print gas savings analysis
 */
function printGasSavings(gasPrice) {
  console.log('Merkle Tree Savings Analysis:\n');

  const testSizes = [50, 100, 500, 1000, 5000, 10000];

  console.log('┌──────────┬──────────────┬──────────────┬─────────────┐');
  console.log('│ Receipts │ Individual   │ Merkle       │ Savings     │');
  console.log('├──────────┼──────────────┼──────────────┼─────────────┤');

  for (const size of testSizes) {
    const individual = calculateIndividualCost(size, gasPrice);
    const merkle = calculateMerkleCost(size, gasPrice);
    const savings = calculateGasSavings(size, gasPrice);

    const sizeStr = size.toString().padStart(8);
    const individualETH = parseFloat(individual.costETH).toFixed(6).padStart(12);
    const merkleETH = parseFloat(merkle.costETH).toFixed(6).padStart(12);
    const savingsStr = `${savings.savingsPercentage.toFixed(2)}%`.padStart(11);

    console.log(`│ ${sizeStr} │ ${individualETH} │ ${merkleETH} │ ${savingsStr} │`);
  }

  console.log('└──────────┴──────────────┴──────────────┴─────────────┘\n');
}

// =============================================================================
// Main
// =============================================================================

async function main() {
  try {
    // Get current gas prices
    const gasPrices = await getGasPrices();

    // Print header
    printHeader(gasPrices);

    // Generate cost table
    const batchSizes = [1, 3, 5, 10, 25, 50, 100, 500, 1000];
    const table = generateCostTable(batchSizes, gasPrices.gasPrice);

    // Print analysis
    printCostTable(table);
    printRecommendations(gasPrices.gasPrice);
    printGasSavings(gasPrices.gasPrice);

    console.log('Note: Costs are in ETH. Actual costs may vary based on:');
    console.log('  - Network congestion');
    console.log('  - Contract storage patterns');
    console.log('  - Transaction data size');
    console.log('  - EIP-1559 priority fees\n');
  } catch (error) {
    console.error('❌ Analysis failed:', error.message);
    console.error('\nEnsure Ethereum node is running at:', PROVIDER);
    process.exit(1);
  }
}

main();
