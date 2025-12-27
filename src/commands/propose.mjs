/**
 * @file Propose Command - Delta Capsule Proposal
 * @module commands/propose
 *
 * @description
 * Proposes a Δ (delta) capsule containing RDF additions/deletions
 * Generates capsule ID, hash, and preview of changes
 */

import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createHash, randomUUID } from 'node:crypto';

/**
 * Propose options
 * @typedef {Object} ProposeOptions
 * @property {string} delta - Path to delta TTL file
 * @property {boolean} [json] - Output JSON format
 */

/**
 * Propose result
 * @typedef {Object} ProposeResult
 * @property {string} capsuleId - UUID for capsule
 * @property {string} hash - SHA256 hash of delta content
 * @property {Array<string>} preview - Preview of additions
 * @property {Object} [metadata] - Additional metadata
 */

/**
 * Extract RDF statements from TTL content
 * @param {string} content - TTL content
 * @returns {Array<string>} Array of RDF statements
 */
function extractStatements(content) {
  const statements = [];

  const lines = content.split('\n');
  let currentStatement = '';

  for (const line of lines) {
    const trimmed = line.trim();

    // Skip comments and empty lines
    if (!trimmed || trimmed.startsWith('#')) {
      continue;
    }

    // Skip prefix declarations (but track them separately)
    if (trimmed.startsWith('@prefix') || trimmed.startsWith('@base')) {
      continue;
    }

    // Accumulate statement
    currentStatement += ' ' + trimmed;

    // Check for statement terminator
    if (trimmed.endsWith('.')) {
      const statement = currentStatement.trim();
      if (statement) {
        statements.push(statement);
      }
      currentStatement = '';
    }
  }

  return statements;
}

/**
 * Compute deterministic hash of content
 * @param {string} content - Content to hash
 * @returns {string} SHA256 hex hash
 */
function computeHash(content) {
  // Normalize content for deterministic hashing
  const normalized = content
    .split('\n')
    .map(line => line.trim())
    .filter(line => line && !line.startsWith('#'))
    .sort()
    .join('\n');

  return createHash('sha256').update(normalized).digest('hex');
}

/**
 * Analyze delta content
 * @param {string} content - Delta TTL content
 * @returns {Object} Analysis result
 */
function analyzeDelta(content) {
  const statements = extractStatements(content);

  // Count additions and deletions (simple heuristic)
  const additions = statements.filter(s => !s.includes('DELETE'));
  const deletions = statements.filter(s => s.includes('DELETE'));

  // Extract subject URIs for preview
  const subjectRegex = /<([^>]+)>/g;
  const subjects = new Set();

  for (const statement of statements) {
    const matches = [...statement.matchAll(subjectRegex)];
    if (matches.length > 0) {
      subjects.add(matches[0][1]);
    }
  }

  return {
    totalStatements: statements.length,
    additions: additions.length,
    deletions: deletions.length,
    subjects: Array.from(subjects),
    statements,
  };
}

/**
 * Generate capsule preview
 * @param {Object} analysis - Delta analysis
 * @returns {Array<string>} Preview items
 */
function generatePreview(analysis) {
  const preview = [];

  // Summary
  preview.push(`Total statements: ${analysis.totalStatements}`);
  preview.push(`Additions: ${analysis.additions}`);
  preview.push(`Deletions: ${analysis.deletions}`);

  // Subject preview (first 5)
  if (analysis.subjects.length > 0) {
    preview.push(`Affected subjects (${analysis.subjects.length}):`);
    const displaySubjects = analysis.subjects.slice(0, 5);
    displaySubjects.forEach(subject => {
      const shortUri = subject.split('/').pop() || subject;
      preview.push(`  - ${shortUri}`);
    });
    if (analysis.subjects.length > 5) {
      preview.push(`  ... and ${analysis.subjects.length - 5} more`);
    }
  }

  return preview;
}

/**
 * Propose command implementation
 * @param {ProposeOptions} options - Command options
 * @returns {Promise<ProposeResult>} Propose result
 */
export async function proposeCommand(options) {
  // Validate required options
  if (!options.delta) {
    throw new Error('Missing required option: --delta <path>');
  }

  // Resolve path
  const deltaPath = resolve(options.delta);

  // Read delta file
  let deltaContent;
  try {
    deltaContent = await readFile(deltaPath, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to read delta file: ${error.message}`);
  }

  // Validate delta content
  if (!deltaContent || deltaContent.trim().length === 0) {
    throw new Error('Delta file is empty');
  }

  // Generate capsule ID
  const capsuleId = randomUUID();

  // Compute hash
  const hash = computeHash(deltaContent);

  // Analyze delta
  const analysis = analyzeDelta(deltaContent);

  // Generate preview
  const preview = generatePreview(analysis);

  return {
    capsuleId,
    hash,
    preview,
    metadata: {
      deltaPath,
      timestamp: Date.now(),
      contentSize: deltaContent.length,
      analysis: {
        statements: analysis.totalStatements,
        additions: analysis.additions,
        deletions: analysis.deletions,
        subjects: analysis.subjects.length,
      },
    },
  };
}
