/**
 * @file Pattern mining using Apriori algorithm
 * @module @unrdf/temporal-discovery/pattern-miner
 * @description Frequent pattern mining for temporal RDF data
 */

import { PatternMiningOptionsSchema, PatternSchema } from './schemas.mjs';

/**
 * Generate candidate itemsets of size k from frequent itemsets of size k-1
 * @param {Set<string>[]} frequentItemsets - Frequent itemsets from previous iteration
 * @param {number} k - Size of candidate itemsets
 * @returns {Set<string>[]} Candidate itemsets
 */
function generateCandidates(frequentItemsets, k) {
  const candidates = [];
  const n = frequentItemsets.length;

  for (let i = 0; i < n; i++) {
    for (let j = i + 1; j < n; j++) {
      const set1 = Array.from(frequentItemsets[i]).sort();
      const set2 = Array.from(frequentItemsets[j]).sort();

      let canMerge = true;
      for (let idx = 0; idx < k - 2; idx++) {
        if (set1[idx] !== set2[idx]) {
          canMerge = false;
          break;
        }
      }

      if (canMerge && set1[k - 2] !== set2[k - 2]) {
        const candidate = new Set([...set1, set2[k - 2]]);
        if (candidate.size === k) {
          candidates.push(candidate);
        }
      }
    }
  }

  return candidates;
}

/**
 * Calculate support for itemsets
 * @param {Set<string>[]} itemsets - Itemsets to calculate support for
 * @param {Set<string>[]} transactions - All transactions
 * @returns {Map<string, number>} Support counts
 */
function calculateSupport(itemsets, transactions) {
  const supportCounts = new Map();

  for (const itemset of itemsets) {
    let count = 0;
    for (const transaction of transactions) {
      const allItemsPresent = Array.from(itemset).every((item) =>
        transaction.has(item)
      );
      if (allItemsPresent) {
        count++;
      }
    }

    const key = Array.from(itemset).sort().join(',');
    supportCounts.set(key, count);
  }

  return supportCounts;
}

/**
 * Apriori algorithm for frequent pattern mining
 * @param {Set<string>[]} transactions - Array of transaction sets
 * @param {Object} options - Mining options
 * @param {number} [options.minSupport=0.1] - Minimum support threshold (0-1)
 * @param {number} [options.minConfidence=0.5] - Minimum confidence for rules
 * @param {number} [options.maxPatternLength=5] - Maximum pattern length
 * @returns {Object[]} Discovered patterns with support and confidence
 * @throws {Error} If options are invalid
 * @example
 * const transactions = [
 *   new Set(['A', 'B', 'C']),
 *   new Set(['A', 'B']),
 *   new Set(['A', 'C']),
 * ];
 * const patterns = apriori(transactions, { minSupport: 0.5 });
 */
export function apriori(transactions, options = {}) {
  const validatedOptions = PatternMiningOptionsSchema.parse(options);
  const { minSupport, maxPatternLength } = validatedOptions;

  if (transactions.length === 0) {
    return [];
  }

  const minSupportCount = Math.ceil(minSupport * transactions.length);
  const allPatterns = [];

  const allItems = new Set();
  for (const transaction of transactions) {
    for (const item of transaction) {
      allItems.add(item);
    }
  }

  let frequentItemsets = Array.from(allItems).map((item) => new Set([item]));
  let k = 1;

  while (frequentItemsets.length > 0 && k <= maxPatternLength) {
    const supportCounts = calculateSupport(frequentItemsets, transactions);

    const newFrequentItemsets = [];
    for (const [itemsetKey, count] of supportCounts.entries()) {
      if (count >= minSupportCount) {
        const items = itemsetKey.split(',');
        const support = count / transactions.length;

        allPatterns.push(
          PatternSchema.parse({
            items,
            support,
            occurrences: count,
          })
        );

        newFrequentItemsets.push(new Set(items));
      }
    }

    frequentItemsets = newFrequentItemsets;

    if (k < maxPatternLength) {
      frequentItemsets = generateCandidates(frequentItemsets, k + 1);
    }

    k++;
  }

  allPatterns.sort((a, b) => b.support - a.support);

  return allPatterns;
}

/**
 * Convert temporal receipts to transaction format
 * @param {Object[]} receipts - Array of KGC receipts with timestamps
 * @returns {Set<string>[]} Transaction sets for pattern mining
 * @example
 * const receipts = [
 *   { timestamp: 1000, operation: 'insert', entityType: 'Triple' },
 *   { timestamp: 2000, operation: 'delete', entityType: 'Triple' },
 * ];
 * const transactions = receiptsToTransactions(receipts);
 */
export function receiptsToTransactions(receipts) {
  return receipts.map((receipt) => {
    const items = new Set();

    if (receipt.operation) items.add(`op:${receipt.operation}`);
    if (receipt.entityType) items.add(`type:${receipt.entityType}`);
    if (receipt.author) items.add(`author:${receipt.author}`);
    if (receipt.metadata) {
      for (const [key, value] of Object.entries(receipt.metadata)) {
        items.add(`${key}:${value}`);
      }
    }

    return items;
  });
}

/**
 * Mine patterns from temporal receipts
 * @param {Object[]} receipts - KGC receipts to mine
 * @param {Object} options - Mining options
 * @returns {Object[]} Discovered patterns
 * @example
 * const patterns = mineReceiptPatterns(receipts, { minSupport: 0.2 });
 */
export function mineReceiptPatterns(receipts, options = {}) {
  const transactions = receiptsToTransactions(receipts);
  return apriori(transactions, options);
}
