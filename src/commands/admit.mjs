/**
 * @file Admit Command - Admission Gate Logic
 * @module commands/admit
 *
 * @description
 * Runs admissibility checks on delta capsules
 * Emits admission receipts with decision reasoning
 */

import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { resolve, join } from 'node:path';
import { createHash, randomUUID } from 'node:crypto';

/**
 * Admit options
 * @typedef {Object} AdmitOptions
 * @property {string} delta - Path to delta TTL file
 * @property {string} out - Output directory for receipts
 * @property {boolean} [json] - Output JSON format
 */

/**
 * Admit result
 * @typedef {Object} AdmitResult
 * @property {string} decision - 'allow' or 'deny'
 * @property {Object} receipt - Receipt information
 * @property {Object} reasoning - Decision reasoning
 */

/**
 * Invariant check result
 * @typedef {Object} InvariantCheck
 * @property {string} name - Invariant name
 * @property {string} description - Invariant description
 * @property {boolean} satisfied - Whether invariant is satisfied
 * @property {string} [reason] - Reason if not satisfied
 */

/**
 * Run invariant checks on delta
 * @param {string} deltaContent - Delta content
 * @returns {Array<InvariantCheck>} Invariant check results
 */
function runInvariantChecks(deltaContent) {
  const invariants = [];

  // Invariant 1: Well-formed RDF syntax
  const wellFormedCheck = {
    name: 'Well-Formed RDF',
    description: 'Delta must contain valid RDF syntax',
    satisfied: true,
    reason: null,
  };

  // Check for basic RDF structure
  const hasTriples = deltaContent.includes('.') || deltaContent.includes(';');
  const hasPrefixes = deltaContent.includes('@prefix');

  if (!hasTriples) {
    wellFormedCheck.satisfied = false;
    wellFormedCheck.reason = 'No RDF triples found';
  }

  invariants.push(wellFormedCheck);

  // Invariant 2: Non-empty delta
  const nonEmptyCheck = {
    name: 'Non-Empty Delta',
    description: 'Delta must contain at least one statement',
    satisfied: true,
    reason: null,
  };

  const statements = deltaContent
    .split('\n')
    .filter(line => {
      const trimmed = line.trim();
      return trimmed && !trimmed.startsWith('#') && !trimmed.startsWith('@');
    });

  if (statements.length === 0) {
    nonEmptyCheck.satisfied = false;
    nonEmptyCheck.reason = 'Delta contains no statements';
  }

  invariants.push(nonEmptyCheck);

  // Invariant 3: No conflicting deletions
  const noConflictsCheck = {
    name: 'No Conflicts',
    description: 'Delta must not contain conflicting operations',
    satisfied: true,
    reason: null,
  };

  // Simple check: ensure we don't add and delete the same triple
  const addStatements = new Set();
  const deleteStatements = new Set();

  statements.forEach(stmt => {
    if (stmt.includes('DELETE')) {
      deleteStatements.add(stmt.replace('DELETE', '').trim());
    } else {
      addStatements.add(stmt.trim());
    }
  });

  for (const addStmt of addStatements) {
    if (deleteStatements.has(addStmt)) {
      noConflictsCheck.satisfied = false;
      noConflictsCheck.reason = 'Conflicting add/delete operations found';
      break;
    }
  }

  invariants.push(noConflictsCheck);

  // Invariant 4: Valid URIs
  const validUrisCheck = {
    name: 'Valid URIs',
    description: 'All URIs must be well-formed',
    satisfied: true,
    reason: null,
  };

  const uriRegex = /<([^>]+)>/g;
  const uris = [...deltaContent.matchAll(uriRegex)];

  for (const [, uri] of uris) {
    // Basic URI validation
    if (!uri.includes(':') && !uri.startsWith('_:')) {
      validUrisCheck.satisfied = false;
      validUrisCheck.reason = `Invalid URI: ${uri}`;
      break;
    }
  }

  invariants.push(validUrisCheck);

  // Invariant 5: Namespace consistency
  const namespaceCheck = {
    name: 'Namespace Consistency',
    description: 'All prefixes must be declared',
    satisfied: true,
    reason: null,
  };

  // Extract used prefixes
  const usedPrefixes = new Set();
  const declaredPrefixes = new Set();

  // Find declared prefixes
  const prefixDecls = [...deltaContent.matchAll(/@prefix\s+(\w+):/g)];
  prefixDecls.forEach(([, prefix]) => declaredPrefixes.add(prefix));

  // Find used prefixes (but not inside quoted strings or URIs)
  // Remove quoted strings and URIs first
  const cleanedContent = deltaContent
    .replace(/"[^"]*"/g, '') // Remove quoted strings
    .replace(/<[^>]*>/g, '') // Remove URIs
    .replace(/\^\^xsd:\w+/g, ''); // Remove datatype suffixes

  const prefixUses = [...cleanedContent.matchAll(/\b(\w+):\w+/g)];
  prefixUses.forEach(([, prefix]) => {
    if (prefix !== '@prefix' && prefix !== '@base') {
      usedPrefixes.add(prefix);
    }
  });

  // Check if all used prefixes are declared
  // Common standard prefixes that don't need explicit declaration
  const standardPrefixes = ['rdf', 'rdfs', 'xsd', 'owl'];

  for (const prefix of usedPrefixes) {
    if (!declaredPrefixes.has(prefix) && !standardPrefixes.includes(prefix)) {
      namespaceCheck.satisfied = false;
      namespaceCheck.reason = `Undeclared prefix: ${prefix}`;
      break;
    }
  }

  invariants.push(namespaceCheck);

  return invariants;
}

/**
 * Make admission decision
 * @param {Array<InvariantCheck>} invariants - Invariant check results
 * @returns {string} 'allow' or 'deny'
 */
function makeDecision(invariants) {
  const allSatisfied = invariants.every(inv => inv.satisfied);
  return allSatisfied ? 'allow' : 'deny';
}

/**
 * Generate admission receipt
 * @param {string} decision - Admission decision
 * @param {string} deltaPath - Delta file path
 * @param {string} deltaHash - Delta content hash
 * @param {Array<InvariantCheck>} invariants - Invariant checks
 * @returns {Object} Receipt data
 */
function generateReceipt(decision, deltaPath, deltaHash, invariants) {
  const receiptId = randomUUID();
  const timestamp = new Date().toISOString();

  return {
    id: receiptId,
    type: 'admission-receipt',
    timestamp,
    decision,
    delta: {
      path: deltaPath,
      hash: deltaHash,
    },
    invariants: invariants.map(inv => ({
      name: inv.name,
      description: inv.description,
      satisfied: inv.satisfied,
      reason: inv.reason || 'Invariant satisfied',
    })),
    metadata: {
      version: '1.0.0',
      generator: 'governance-substrate-cli',
    },
  };
}

/**
 * Compute hash of content
 * @param {string} content - Content to hash
 * @returns {string} SHA256 hex hash
 */
function computeHash(content) {
  return createHash('sha256').update(content).digest('hex');
}

/**
 * Admit command implementation
 * @param {AdmitOptions} options - Command options
 * @returns {Promise<AdmitResult>} Admit result
 */
export async function admitCommand(options) {
  // Validate required options
  if (!options.delta) {
    throw new Error('Missing required option: --delta <path>');
  }

  if (!options.out) {
    throw new Error('Missing required option: --out <path>');
  }

  // Resolve paths
  const deltaPath = resolve(options.delta);
  const outDir = resolve(options.out);

  // Read delta file
  let deltaContent;
  try {
    deltaContent = await readFile(deltaPath, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to read delta file: ${error.message}`);
  }

  // Compute delta hash
  const deltaHash = computeHash(deltaContent);

  // Run invariant checks
  const invariants = runInvariantChecks(deltaContent);

  // Make decision
  const decision = makeDecision(invariants);

  // Generate receipt
  const receipt = generateReceipt(decision, deltaPath, deltaHash, invariants);

  // Compute receipt hash
  const receiptContent = JSON.stringify(receipt, null, 2);
  const receiptHash = computeHash(receiptContent);

  // Ensure output directory exists
  try {
    await mkdir(outDir, { recursive: true });
  } catch (error) {
    throw new Error(`Failed to create output directory: ${error.message}`);
  }

  // Write receipt to file
  const receiptFilename = `admission-${receipt.id}.json`;
  const receiptPath = join(outDir, receiptFilename);

  try {
    await writeFile(receiptPath, receiptContent, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to write receipt file: ${error.message}`);
  }

  return {
    decision,
    receipt: {
      path: receiptPath,
      hash: receiptHash,
      id: receipt.id,
    },
    reasoning: {
      invariants: invariants.map(inv => ({
        name: inv.name,
        satisfied: inv.satisfied,
        reason: inv.reason,
      })),
      summary: decision === 'allow'
        ? 'All invariants satisfied'
        : `Failed: ${invariants.filter(i => !i.satisfied).map(i => i.name).join(', ')}`,
    },
  };
}
