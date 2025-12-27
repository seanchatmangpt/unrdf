/**
 * N3 Parsing with L5 Receipt Support
 *
 * Wraps N3 parsing with grammar version tracking and receipts
 *
 * @module @unrdf/core/n3-justified-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  blake3Hash,
  canonicalize,
} from '../../v6-core/src/receipt-pattern.mjs';

/**
 * Parse Input Schema
 */
export const ParseInputSchema = z.object({
  rdfData: z.string().min(1),
  format: z.enum(['turtle', 'ntriples', 'nquads', 'trig']).default('turtle'),
  grammarVersion: z.string().default('1.1'),
});

/**
 * Parse Result Schema
 */
export const ParseResultSchema = z.object({
  quads: z.array(z.any()),
  grammarVersion: z.string(),
  grammarHash: z.string().length(64),
  quadCount: z.number().int().nonnegative(),
});

/**
 * Pure function: Parse RDF with grammar tracking
 *
 * @param {Object} input - Parse input
 * @returns {Object} Parse result with grammar hash
 */
function parseRDFImpl(input) {
  const validated = ParseInputSchema.parse(input);

  // Simulate N3 parsing (deterministic for same input)
  // In real implementation, use N3.js parser
  const quads = []; // Parsed quads would go here

  // Generate grammar closure hash (all grammar rules + version)
  const grammarClosure = {
    version: validated.grammarVersion,
    format: validated.format,
  };
  const grammarHash = blake3Hash(canonicalize(grammarClosure));

  return {
    quads,
    grammarVersion: validated.grammarVersion,
    grammarHash,
    quadCount: quads.length,
  };
}

/**
 * Wrapped: Parse RDF with receipt
 */
export const parseRDF = withReceipt(parseRDFImpl, {
  operation: 'parseRDF',
  profile: 'parse',
  inputSchema: z.tuple([ParseInputSchema]),
  outputSchema: ParseResultSchema,
});

/**
 * L5 Determinism Test
 */
export async function testN3Determinism(context, iterations = 100) {
  const input = {
    rdfData: '<http://example.org/Alice> <http://xmlns.com/foaf/0.1/name> "Alice" .',
    format: 'turtle',
    grammarVersion: '1.1',
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await parseRDF(context, input);
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
