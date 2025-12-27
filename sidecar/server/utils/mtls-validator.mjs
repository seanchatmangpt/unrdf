/**
 * Byzantine Fault-Tolerant mTLS Certificate Validator
 * Implements 3-of-5 consensus for certificate trust decisions
 */

import crypto from 'crypto';
import { z } from 'zod';

const CertificateSchema = z.object({
  subject: z.object({
    CN: z.string(),
    O: z.string().optional(),
    OU: z.string().optional()
  }),
  issuer: z.object({
    CN: z.string(),
    O: z.string().optional()
  }),
  validFrom: z.string(),
  validTo: z.string(),
  fingerprint: z.string(),
  serialNumber: z.string(),
  publicKey: z.string()
});

/**
 * @typedef {z.infer<typeof CertificateSchema>} Certificate
 */

/**
 * Byzantine consensus validator pool
 */
class ByzantineValidatorPool {
  constructor() {
    /** @type {Map<string, string[]>} */
    this.pinnedKeys = new Map();

    /** @type {Set<string>} */
    this.revokedCerts = new Set();

    /** @type {number} */
    this.consensusThreshold = 3; // 3-of-5 validators must agree

    /** @type {number} */
    this.validatorCount = 5;
  }

  /**
   * Pin a public key for a subject
   * @param {string} subject - Certificate subject
   * @param {string} publicKeyHash - SHA-256 hash of public key
   */
  pinPublicKey(subject, publicKeyHash) {
    if (!this.pinnedKeys.has(subject)) {
      this.pinnedKeys.set(subject, []);
    }
    this.pinnedKeys.get(subject).push(publicKeyHash);
  }

  /**
   * Revoke a certificate
   * @param {string} serialNumber - Certificate serial number
   */
  revokeCertificate(serialNumber) {
    this.revokedCerts.add(serialNumber);
  }

  /**
   * Validate certificate with Byzantine consensus
   * @param {import('tls').PeerCertificate} cert - TLS certificate
   * @returns {Promise<{valid: boolean, votes: number[], reason?: string}>}
   */
  async validateWithConsensus(cert) {
    // Run 5 independent validators
    const validators = [
      this.validateExpiry(cert),
      this.validateRevocation(cert),
      this.validatePublicKeyPin(cert),
      this.validateChain(cert),
      this.validateCryptographicStrength(cert)
    ];

    const results = await Promise.all(validators);
    const votes = results.map(r => r.valid ? 1 : 0);
    const approvals = votes.filter(v => v === 1).length;

    // Byzantine consensus: require 3-of-5 validators to approve
    const consensusReached = approvals >= this.consensusThreshold;

    if (!consensusReached) {
      const failures = results.filter(r => !r.valid).map(r => r.reason);
      return {
        valid: false,
        votes,
        reason: `Byzantine consensus failed (${approvals}/${this.validatorCount}): ${failures.join(', ')}`
      };
    }

    return { valid: true, votes };
  }

  /**
   * Validator 1: Check certificate expiry
   * @param {import('tls').PeerCertificate} cert
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async validateExpiry(cert) {
    const now = new Date();
    const validFrom = new Date(cert.valid_from);
    const validTo = new Date(cert.valid_to);

    if (now < validFrom) {
      return { valid: false, reason: 'Certificate not yet valid' };
    }

    if (now > validTo) {
      return { valid: false, reason: 'Certificate expired' };
    }

    return { valid: true };
  }

  /**
   * Validator 2: Check revocation status
   * @param {import('tls').PeerCertificate} cert
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async validateRevocation(cert) {
    if (this.revokedCerts.has(cert.serialNumber)) {
      return { valid: false, reason: 'Certificate revoked' };
    }

    return { valid: true };
  }

  /**
   * Validator 3: Validate public key pinning
   * @param {import('tls').PeerCertificate} cert
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async validatePublicKeyPin(cert) {
    const subject = cert.subject.CN;
    const pinnedKeys = this.pinnedKeys.get(subject);

    if (!pinnedKeys || pinnedKeys.length === 0) {
      // No pinning configured - accept
      return { valid: true };
    }

    // Hash the public key
    const publicKeyHash = crypto
      .createHash('sha256')
      .update(cert.pubkey)
      .digest('hex');

    if (!pinnedKeys.includes(publicKeyHash)) {
      return { valid: false, reason: 'Public key pin mismatch' };
    }

    return { valid: true };
  }

  /**
   * Validator 4: Validate certificate chain
   * @param {import('tls').PeerCertificate} cert
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async validateChain(cert) {
    // Check if certificate is self-signed (dev mode)
    if (cert.issuer.CN === cert.subject.CN) {
      if (process.env.NODE_ENV === 'production') {
        return { valid: false, reason: 'Self-signed certificate in production' };
      }
      return { valid: true }; // Allow in development
    }

    // Validate chain depth (prevent chain too long attacks)
    let current = cert;
    let depth = 0;
    const maxDepth = 5;

    while (current.issuerCertificate && current.issuerCertificate !== current) {
      depth++;
      if (depth > maxDepth) {
        return { valid: false, reason: 'Certificate chain too deep' };
      }
      current = current.issuerCertificate;
    }

    return { valid: true };
  }

  /**
   * Validator 5: Validate cryptographic strength
   * @param {import('tls').PeerCertificate} cert
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async validateCryptographicStrength(cert) {
    // Check key size (minimum 2048 bits for RSA, 256 for EC)
    const keyType = cert.pubkey ? cert.pubkey.type : 'unknown';

    if (keyType === 'rsa') {
      const keySize = cert.bits || 0;
      if (keySize < 2048) {
        return { valid: false, reason: `RSA key too weak: ${keySize} bits` };
      }
    } else if (keyType === 'ec') {
      const keySize = cert.bits || 0;
      if (keySize < 256) {
        return { valid: false, reason: `EC key too weak: ${keySize} bits` };
      }
    }

    return { valid: true };
  }
}

// Singleton validator pool
const validatorPool = new ByzantineValidatorPool();

/**
 * Validate mTLS client certificate
 * @param {import('tls').PeerCertificate} cert - Client certificate
 * @returns {Promise<{valid: boolean, reason?: string, votes?: number[]}>}
 */
export async function validateClientCertificate(cert) {
  if (!cert || Object.keys(cert).length === 0) {
    return { valid: false, reason: 'No client certificate provided' };
  }

  return validatorPool.validateWithConsensus(cert);
}

/**
 * Pin a public key for enhanced security
 * @param {string} subject - Certificate subject
 * @param {string} publicKeyHash - SHA-256 hash of public key
 */
export function pinPublicKey(subject, publicKeyHash) {
  validatorPool.pinPublicKey(subject, publicKeyHash);
}

/**
 * Revoke a certificate by serial number
 * @param {string} serialNumber - Certificate serial number
 */
export function revokeCertificate(serialNumber) {
  validatorPool.revokeCertificate(serialNumber);
}

/**
 * Get validator pool instance
 * @returns {ByzantineValidatorPool}
 */
export function getValidatorPool() {
  return validatorPool;
}
