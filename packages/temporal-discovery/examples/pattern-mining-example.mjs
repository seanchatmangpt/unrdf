/**
 * @file Pattern mining example
 * @description Demonstrates Apriori algorithm for frequent pattern mining
 */

import { mineReceiptPatterns } from '../src/pattern-miner.mjs';

console.log('=== Pattern Mining Example ===\n');

const receipts = Array.from({ length: 100 }, (_, i) => ({
  timestamp: 1000 + i * 1000,
  operation: i % 3 === 0 ? 'insert' : i % 3 === 1 ? 'delete' : 'update',
  entityType: i % 2 === 0 ? 'Triple' : 'Graph',
  author: i % 5 === 0 ? 'alice' : i % 5 === 1 ? 'bob' : 'charlie',
  metadata: {
    source: i % 4 === 0 ? 'api' : 'ui',
  },
}));

console.log(`Mining patterns from ${receipts.length} receipts...\n`);

const patterns = mineReceiptPatterns(receipts, {
  minSupport: 0.2,
  maxPatternLength: 3,
});

console.log(`Found ${patterns.length} frequent patterns:\n`);

patterns.slice(0, 10).forEach((pattern, i) => {
  console.log(
    `${i + 1}. ${pattern.items.join(', ')} - Support: ${(pattern.support * 100).toFixed(1)}% (${pattern.occurrences} occurrences)`
  );
});

console.log('\n=== Top Patterns by Support ===\n');
const topPatterns = patterns.filter((p) => p.items.length >= 2).slice(0, 5);

topPatterns.forEach((pattern, i) => {
  console.log(`${i + 1}. Pattern: [${pattern.items.join(', ')}]`);
  console.log(`   Support: ${(pattern.support * 100).toFixed(1)}%`);
  console.log(`   Occurrences: ${pattern.occurrences}\n`);
});
