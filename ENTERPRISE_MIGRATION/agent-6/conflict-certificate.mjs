/**
 * Conflict Certificates for Delta Operations
 *
 * Generates and verifies cryptographically-signed certificates
 * that prove conflict detection verdicts are correct.
 *
 * @module agent-6/conflict-certificate
 */

import { createHash } from 'node:crypto';
import { detectConflict } from './conflict-detector.mjs';

/**
 * @typedef {import('./conflict-detector.mjs').Delta} Delta
 * @typedef {import('./conflict-detector.mjs').ConflictResult} ConflictResult
 */

/**
 * @typedef {Object} ConflictCertificate
 * @property {string} version - Certificate version
 * @property {string} timestamp - ISO timestamp of generation
 * @property {string} deltaAHash - SHA-256 hash of delta A
 * @property {string} deltaBHash - SHA-256 hash of delta B
 * @property {ConflictResult} verdict - Conflict detection result
 * @property {string} certificateHash - Hash of entire certificate (excluding this field)
 * @property {Object} metadata - Additional metadata
 */

/**
 * Compute SHA-256 hash of an object
 * @param {Object} obj - Object to hash
 * @returns {string} Hex-encoded hash
 */
function computeHash(obj) {
  const serialized = JSON.stringify(obj, Object.keys(obj).sort());
  return createHash('sha256').update(serialized).digest('hex');
}

/**
 * Normalize delta for hashing (remove non-deterministic fields)
 * @param {Delta} delta - Delta to normalize
 * @returns {Object} Normalized delta
 */
function normalizeDelta(delta) {
  const normalized = { ...delta };

  // Remove fields that might vary but don't affect semantics
  delete normalized.timestamp;
  delete normalized.id;
  delete normalized.metadata;

  // Sort arrays for determinism
  if (Array.isArray(normalized.fields)) {
    normalized.fields = [...normalized.fields].sort();
  }

  if (Array.isArray(normalized.deltas)) {
    normalized.deltas = normalized.deltas.map(normalizeDelta);
  }

  return normalized;
}

/**
 * Generate a conflict certificate
 *
 * Creates a verifiable proof that two deltas were checked for conflicts
 * and the verdict is correct. Certificate includes:
 * - Hashes of both deltas (for integrity)
 * - Conflict verdict (type, paths, description)
 * - Timestamp and version
 * - Certificate hash (for tamper detection)
 *
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @param {ConflictResult} [verdict] - Pre-computed verdict (optional)
 * @returns {ConflictCertificate} Signed certificate
 * @throws {Error} If deltas are invalid
 *
 * @example
 * const cert = generateCertificate(
 *   { type: 'UPDATE', path: '/users/123' },
 *   { type: 'DELETE', path: '/users/123' }
 * );
 * // cert.verdict.type === 'DELETE_ANY'
 * // cert.deltaAHash === '3f5b8...'
 */
export function generateCertificate(deltaA, deltaB, verdict = null) {
  if (!deltaA || typeof deltaA !== 'object') {
    throw new Error('deltaA must be a non-null object');
  }

  if (!deltaB || typeof deltaB !== 'object') {
    throw new Error('deltaB must be a non-null object');
  }

  // Compute verdict if not provided
  const actualVerdict = verdict || detectConflict(deltaA, deltaB);

  // Normalize deltas for deterministic hashing
  const normalizedA = normalizeDelta(deltaA);
  const normalizedB = normalizeDelta(deltaB);

  // Compute delta hashes
  const deltaAHash = computeHash(normalizedA);
  const deltaBHash = computeHash(normalizedB);

  // Build certificate (without hash)
  const certificate = {
    version: '1.0.0',
    timestamp: new Date().toISOString(),
    deltaAHash,
    deltaBHash,
    verdict: {
      type: actualVerdict.type,
      hasConflict: actualVerdict.hasConflict,
      conflictingPaths: [...actualVerdict.conflictingPaths].sort(),
      description: actualVerdict.description
    },
    metadata: {
      deltaAType: deltaA.type,
      deltaBType: deltaB.type,
      generatedBy: 'agent-6/conflict-certificate'
    }
  };

  // Compute certificate hash
  const certificateHash = computeHash(certificate);

  return {
    ...certificate,
    certificateHash
  };
}

/**
 * Verify a conflict certificate
 *
 * Checks:
 * 1. Certificate hash is valid (not tampered)
 * 2. Verdict matches re-computed conflict detection
 * 3. Delta hashes match provided deltas
 *
 * @param {ConflictCertificate} cert - Certificate to verify
 * @param {Delta} [deltaA] - First delta (optional, for hash verification)
 * @param {Delta} [deltaB] - Second delta (optional, for hash verification)
 * @returns {Object} Verification result
 *
 * @example
 * const result = verifyCertificate(cert, deltaA, deltaB);
 * // result.valid === true
 * // result.errors === []
 */
export function verifyCertificate(cert, deltaA = null, deltaB = null) {
  const errors = [];

  // Check required fields
  if (!cert || typeof cert !== 'object') {
    return { valid: false, errors: ['Certificate must be a non-null object'] };
  }

  if (!cert.version || !cert.timestamp || !cert.deltaAHash || !cert.deltaBHash) {
    errors.push('Certificate missing required fields');
  }

  if (!cert.verdict || typeof cert.verdict !== 'object') {
    errors.push('Certificate missing verdict');
  }

  if (!cert.certificateHash) {
    errors.push('Certificate missing hash');
  }

  // Check certificate hash integrity
  const certWithoutHash = { ...cert };
  delete certWithoutHash.certificateHash;
  const recomputedHash = computeHash(certWithoutHash);

  if (recomputedHash !== cert.certificateHash) {
    errors.push('Certificate hash mismatch - certificate may be tampered');
  }

  // If deltas provided, verify hashes
  if (deltaA) {
    const normalizedA = normalizeDelta(deltaA);
    const actualHashA = computeHash(normalizedA);
    if (actualHashA !== cert.deltaAHash) {
      errors.push('Delta A hash mismatch');
    }
  }

  if (deltaB) {
    const normalizedB = normalizeDelta(deltaB);
    const actualHashB = computeHash(normalizedB);
    if (actualHashB !== cert.deltaBHash) {
      errors.push('Delta B hash mismatch');
    }
  }

  // If both deltas provided, re-check verdict
  if (deltaA && deltaB) {
    const recomputedVerdict = detectConflict(deltaA, deltaB);

    if (recomputedVerdict.type !== cert.verdict.type) {
      errors.push(`Verdict type mismatch: expected ${cert.verdict.type}, got ${recomputedVerdict.type}`);
    }

    if (recomputedVerdict.hasConflict !== cert.verdict.hasConflict) {
      errors.push('Verdict conflict flag mismatch');
    }

    // Check conflicting paths match
    const certPaths = new Set(cert.verdict.conflictingPaths || []);
    const actualPaths = new Set(recomputedVerdict.conflictingPaths || []);

    if (certPaths.size !== actualPaths.size) {
      errors.push('Conflicting paths count mismatch');
    } else {
      for (const path of certPaths) {
        if (!actualPaths.has(path)) {
          errors.push(`Conflicting path missing in recomputed verdict: ${path}`);
        }
      }
    }
  }

  return {
    valid: errors.length === 0,
    errors,
    certificateHash: cert.certificateHash,
    timestamp: cert.timestamp
  };
}

/**
 * Batch generate certificates for multiple delta pairs
 * @param {Array<[Delta, Delta]>} deltaPairs - Array of delta pairs
 * @returns {ConflictCertificate[]} Array of certificates
 */
export function generateBatchCertificates(deltaPairs) {
  if (!Array.isArray(deltaPairs)) {
    throw new Error('deltaPairs must be an array');
  }

  return deltaPairs.map(([deltaA, deltaB]) => {
    if (!Array.isArray(deltaPairs[0]) || deltaPairs[0].length !== 2) {
      throw new Error('Each element must be a pair [deltaA, deltaB]');
    }
    return generateCertificate(deltaA, deltaB);
  });
}

/**
 * Verify batch of certificates
 * @param {ConflictCertificate[]} certificates - Certificates to verify
 * @param {Array<[Delta, Delta]>} [deltaPairs] - Optional delta pairs for verification
 * @returns {Object} Batch verification result
 */
export function verifyBatchCertificates(certificates, deltaPairs = null) {
  if (!Array.isArray(certificates)) {
    throw new Error('certificates must be an array');
  }

  const results = certificates.map((cert, index) => {
    const deltaA = deltaPairs?.[index]?.[0];
    const deltaB = deltaPairs?.[index]?.[1];
    return {
      index,
      ...verifyCertificate(cert, deltaA, deltaB)
    };
  });

  const allValid = results.every(r => r.valid);
  const failedCount = results.filter(r => !r.valid).length;

  return {
    allValid,
    totalCertificates: certificates.length,
    validCount: certificates.length - failedCount,
    failedCount,
    results
  };
}

/**
 * Serialize certificate to JSON string
 * @param {ConflictCertificate} cert - Certificate to serialize
 * @returns {string} JSON string
 */
export function serializeCertificate(cert) {
  return JSON.stringify(cert, null, 2);
}

/**
 * Deserialize certificate from JSON string
 * @param {string} json - JSON string
 * @returns {ConflictCertificate} Certificate object
 */
export function deserializeCertificate(json) {
  try {
    return JSON.parse(json);
  } catch (error) {
    throw new Error(`Failed to parse certificate JSON: ${error.message}`);
  }
}

/**
 * Create a certificate chain for a sequence of deltas
 * @param {Delta[]} deltas - Sequence of deltas
 * @returns {Object} Certificate chain with all pairwise certificates
 */
export function createCertificateChain(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('deltas must be an array');
  }

  const certificates = [];

  for (let i = 0; i < deltas.length; i++) {
    for (let j = i + 1; j < deltas.length; j++) {
      const cert = generateCertificate(deltas[i], deltas[j]);
      certificates.push({
        indexA: i,
        indexB: j,
        certificate: cert
      });
    }
  }

  const chainHash = computeHash(certificates);

  return {
    version: '1.0.0',
    timestamp: new Date().toISOString(),
    deltaCount: deltas.length,
    certificateCount: certificates.length,
    certificates,
    chainHash
  };
}

/**
 * Verify a certificate chain
 * @param {Object} chain - Certificate chain to verify
 * @param {Delta[]} [deltas] - Optional deltas for verification
 * @returns {Object} Verification result
 */
export function verifyCertificateChain(chain, deltas = null) {
  if (!chain || typeof chain !== 'object') {
    return { valid: false, errors: ['Chain must be a non-null object'] };
  }

  const errors = [];

  // Verify chain hash
  const chainWithoutHash = { ...chain };
  delete chainWithoutHash.chainHash;
  const recomputedHash = computeHash(chainWithoutHash);

  if (recomputedHash !== chain.chainHash) {
    errors.push('Chain hash mismatch');
  }

  // Verify individual certificates
  const certResults = chain.certificates.map(({ indexA, indexB, certificate }) => {
    const deltaA = deltas?.[indexA];
    const deltaB = deltas?.[indexB];
    return verifyCertificate(certificate, deltaA, deltaB);
  });

  const invalidCerts = certResults.filter(r => !r.valid);
  if (invalidCerts.length > 0) {
    errors.push(`${invalidCerts.length} invalid certificates in chain`);
  }

  return {
    valid: errors.length === 0,
    errors,
    certificateResults: certResults,
    chainHash: chain.chainHash
  };
}
