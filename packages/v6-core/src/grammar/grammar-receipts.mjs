/**
 * Grammar Parsing with L5 Receipt Support
 *
 * Wraps grammar parsing with version tracking and receipts
 *
 * @module @unrdf/v6-core/grammar/grammar-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  blake3Hash,
  canonicalize,
} from '../receipt-pattern.mjs';

/**
 * Grammar Schema
 */
export const GrammarSchema = z.object({
  source: z.string().min(1),
  version: z.string().default('1.0.0'),
  rules: z.array(z.string()).optional(),
});

/**
 * Parsed Grammar Schema
 */
export const ParsedGrammarSchema = z.object({
  ast: z.any(),
  version: z.string(),
  versionHash: z.string().length(64),
  ruleCount: z.number().int().nonnegative(),
});

/**
 * Pure function: Parse grammar
 *
 * @param {Object} grammar - Grammar source
 * @returns {Promise<Object>} Parsed grammar with version hash
 */
async function parseGrammarImpl(grammar) {
  const validated = GrammarSchema.parse(grammar);

  // Simulate grammar parsing (deterministic)
  const ast = {
    type: 'Grammar',
    rules: validated.rules || [],
  };

  // Generate version hash from grammar source using BLAKE3
  const versionHash = await blake3Hash(canonicalize({ source: validated.source, version: validated.version }));

  return {
    ast,
    version: validated.version,
    versionHash,
    ruleCount: (validated.rules || []).length,
  };
}

/**
 * Wrapped: Parse grammar with receipt
 */
export const parseGrammar = withReceipt(parseGrammarImpl, {
  operation: 'parseGrammar',
  profile: 'parse',
  inputSchema: z.tuple([GrammarSchema]),
  outputSchema: ParsedGrammarSchema,
});

/**
 * L5 Determinism Test
 */
export async function testGrammarDeterminism(context, iterations = 100) {
  const grammar = {
    source: 'rule1 := value1; rule2 := value2;',
    version: '1.0.0',
    rules: ['rule1', 'rule2'],
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await parseGrammar(context, grammar);
    receipts.push(receipt);
    hashes.add(receipt.receiptHash);
  }

  return {
    iterations,
    uniqueHashes: hashes.size,
    deterministic: hashes.size === 1,
    expectedHash: receipts[0].receiptHash,
  };
}
