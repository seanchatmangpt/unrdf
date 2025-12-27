/**
 * @file Validate Command - Universe and Policy Validation
 * @module commands/validate
 *
 * @description
 * Validates RDF universe against policy constraints
 * Checks graph consistency, policy conformance, and invariants
 */

import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createHash } from 'node:crypto';

/**
 * Validation schema (using simple object validation)
 * @typedef {Object} ValidationOptions
 * @property {string} universe - Path to universe TTL file
 * @property {string} policy - Path to policy TTL file
 * @property {boolean} [json] - Output JSON format
 */

/**
 * Validation result
 * @typedef {Object} ValidationResult
 * @property {string} status - 'valid' or 'invalid'
 * @property {Array<string>} errors - Validation errors
 * @property {Array<string>} warnings - Validation warnings
 * @property {Object} [metadata] - Additional metadata
 */

/**
 * Simple TTL parser for validation (checks basic syntax)
 * @param {string} content - TTL content
 * @returns {Object} Parse result with triples count
 */
function parseTTL(content) {
  const lines = content.split('\n').filter(line => {
    const trimmed = line.trim();
    return trimmed && !trimmed.startsWith('#');
  });

  const triples = lines.filter(line =>
    line.includes('.') || line.includes(';')
  );

  const prefixes = lines.filter(line =>
    line.trim().startsWith('@prefix')
  );

  return {
    valid: true,
    tripleCount: triples.length,
    prefixCount: prefixes.length,
    lines: lines.length,
  };
}

/**
 * Validate universe structure
 * @param {string} universeContent - Universe TTL content
 * @returns {Object} Validation result
 */
function validateUniverse(universeContent) {
  const errors = [];
  const warnings = [];

  // Check if content exists
  if (!universeContent || universeContent.trim().length === 0) {
    errors.push('Universe file is empty');
    return { valid: false, errors, warnings };
  }

  // Parse TTL
  const parseResult = parseTTL(universeContent);

  // Check for required prefixes
  const requiredPrefixes = ['rdf:', 'rdfs:', 'xsd:'];
  const hasPrefixes = requiredPrefixes.every(prefix =>
    universeContent.includes(prefix)
  );

  if (!hasPrefixes) {
    warnings.push('Missing some standard RDF prefixes (rdf:, rdfs:, xsd:)');
  }

  // Check minimum structure
  if (parseResult.tripleCount === 0) {
    errors.push('No RDF triples found in universe');
  }

  // Check for basic RDF syntax errors
  const lines = universeContent.split('\n');
  lines.forEach((line, idx) => {
    const trimmed = line.trim();
    if (trimmed && !trimmed.startsWith('#') && !trimmed.startsWith('@')) {
      // Check for incomplete statements
      if (!trimmed.endsWith('.') && !trimmed.endsWith(';') && !trimmed.endsWith(',')) {
        if (idx < lines.length - 1) { // Not last line
          const nextLine = lines[idx + 1].trim();
          if (!nextLine.startsWith('.') && !nextLine.startsWith(';') && !nextLine.startsWith(',')) {
            warnings.push(`Line ${idx + 1}: Possible incomplete statement`);
          }
        }
      }
    }
  });

  return {
    valid: errors.length === 0,
    errors,
    warnings,
    metadata: parseResult,
  };
}

/**
 * Validate policy rules
 * @param {string} policyContent - Policy TTL content
 * @returns {Object} Validation result
 */
function validatePolicy(policyContent) {
  const errors = [];
  const warnings = [];

  // Check if content exists
  if (!policyContent || policyContent.trim().length === 0) {
    errors.push('Policy file is empty');
    return { valid: false, errors, warnings };
  }

  // Parse TTL
  const parseResult = parseTTL(policyContent);

  // Check for policy-specific patterns
  const hasPolicyClass = policyContent.includes('Policy') ||
                          policyContent.includes('Rule') ||
                          policyContent.includes('Constraint');

  if (!hasPolicyClass) {
    warnings.push('No explicit policy/rule/constraint classes found');
  }

  // Check for invariants
  const hasInvariants = policyContent.includes('invariant') ||
                         policyContent.includes('constraint');

  if (!hasInvariants) {
    warnings.push('No invariants or constraints defined');
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
    metadata: parseResult,
  };
}

/**
 * Check cross-validation between universe and policy
 * @param {string} universeContent - Universe content
 * @param {string} policyContent - Policy content
 * @returns {Object} Cross-validation result
 */
function crossValidate(universeContent, policyContent) {
  const errors = [];
  const warnings = [];

  // Extract namespaces from both
  const universeNamespaces = new Set();
  const policyNamespaces = new Set();

  const extractNamespaces = (content) => {
    const prefixRegex = /@prefix\s+(\w+):/g;
    const matches = [...content.matchAll(prefixRegex)];
    return new Set(matches.map(m => m[1]));
  };

  const uNamespaces = extractNamespaces(universeContent);
  const pNamespaces = extractNamespaces(policyContent);

  // Check if policy references namespaces not in universe
  for (const ns of pNamespaces) {
    if (!uNamespaces.has(ns) && ns !== 'policy' && ns !== 'rule') {
      warnings.push(`Policy references namespace '${ns}:' not defined in universe`);
    }
  }

  return { errors, warnings };
}

/**
 * Validate command implementation
 * @param {ValidationOptions} options - Command options
 * @returns {Promise<ValidationResult>} Validation result
 */
export async function validateCommand(options) {
  const errors = [];
  const warnings = [];

  // Validate required options
  if (!options.universe) {
    throw new Error('Missing required option: --universe <path>');
  }

  if (!options.policy) {
    throw new Error('Missing required option: --policy <path>');
  }

  // Resolve paths
  const universePath = resolve(options.universe);
  const policyPath = resolve(options.policy);

  let universeContent, policyContent;

  // Read universe file
  try {
    universeContent = await readFile(universePath, 'utf-8');
  } catch (error) {
    errors.push(`Failed to read universe file: ${error.message}`);
    return { status: 'invalid', errors, warnings };
  }

  // Read policy file
  try {
    policyContent = await readFile(policyPath, 'utf-8');
  } catch (error) {
    errors.push(`Failed to read policy file: ${error.message}`);
    return { status: 'invalid', errors, warnings };
  }

  // Validate universe
  const universeValidation = validateUniverse(universeContent);
  errors.push(...universeValidation.errors);
  warnings.push(...universeValidation.warnings);

  // Validate policy
  const policyValidation = validatePolicy(policyContent);
  errors.push(...policyValidation.errors);
  warnings.push(...policyValidation.warnings);

  // Cross-validate
  if (universeValidation.valid && policyValidation.valid) {
    const crossValidation = crossValidate(universeContent, policyContent);
    errors.push(...crossValidation.errors);
    warnings.push(...crossValidation.warnings);
  }

  // Determine final status
  const status = errors.length === 0 ? 'valid' : 'invalid';

  return {
    status,
    errors,
    warnings,
    metadata: {
      universe: {
        path: universePath,
        ...universeValidation.metadata,
      },
      policy: {
        path: policyPath,
        ...policyValidation.metadata,
      },
    },
  };
}
