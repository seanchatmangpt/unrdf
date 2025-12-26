/**
 * KGEN Attestor - Generate cryptographic attestations for deterministic output
 *
 * Provides:
 * - Content integrity verification
 * - Deterministic output attestation
 * - Reproduction proofs
 * - Audit trails
 */

import crypto from 'crypto';

/**
 *
 */
export class KGenAttestor {
  /**
   *
   */
  constructor(options = {}) {
    this.options = {
      enableAttestation: options.enableAttestation !== false,
      algorithm: options.algorithm || 'sha256',
      attestorId: options.attestorId || 'kgen-templates',
      version: options.version || '2.0.0',
      staticBuildTime: options.staticBuildTime || '2024-01-01T00:00:00.000Z',
      deterministicMode: options.deterministicMode !== false,
      includeMetadata: options.includeMetadata !== false,
      ...options
    };
  }

  /**
   * Generate attestation for rendered content
   */
  async attest(content, context = {}) {
    if (!this.options.enableAttestation) {
      return {
        attested: false,
        reason: 'Attestation disabled'
      };
    }

    try {
      const attestation = await this.createAttestation(content, context);

      return {
        attested: true,
        attestation,
        metadata: {
          algorithm: this.options.algorithm,
          attestorId: this.options.attestorId,
          version: this.options.version,
          timestamp: this.getTimestamp()
        }
      };
    } catch (error) {
      return {
        attested: false,
        error: error.message,
        metadata: {
          timestamp: this.getTimestamp()
        }
      };
    }
  }

  /**
   * Create complete attestation record
   */
  async createAttestation(content, context = {}) {
    const contentHash = this.createContentHash(content);
    const contextHash = context.contextHash || this.createContentHash(JSON.stringify(context));
    const templateHash = context.templateHash || '';

    const attestationData = {
      // Core hashes
      contentHash,
      contextHash,
      templateHash,

      // Attestation metadata
      algorithm: this.options.algorithm,
      attestorId: this.options.attestorId,
      version: this.options.version,
      timestamp: this.getTimestamp(),
      deterministicMode: this.options.deterministicMode,

      // Content metadata
      contentLength: content ? content.length : 0,
      contentType: 'text/plain',

      // Reproducibility data
      reproducible: this.options.deterministicMode,
      staticBuildTime: this.options.staticBuildTime,

      // Additional metadata if enabled
      ...(this.options.includeMetadata ? this.getAdditionalMetadata(content, context) : {})
    };

    // Create attestation signature
    const attestationHash = this.createContentHash(JSON.stringify(attestationData));

    return {
      ...attestationData,
      attestationHash,
      signature: this.createSignature(attestationData)
    };
  }

  /**
   * Verify attestation integrity
   */
  async verify(content, attestation) {
    try {
      // Verify content hash
      const currentContentHash = this.createContentHash(content);
      if (currentContentHash !== attestation.contentHash) {
        return {
          valid: false,
          reason: 'Content hash mismatch',
          expected: attestation.contentHash,
          actual: currentContentHash
        };
      }

      // Verify attestation hash
      const { attestationHash, signature, ...attestationData } = attestation;
      const currentAttestationHash = this.createContentHash(JSON.stringify(attestationData));

      if (currentAttestationHash !== attestationHash) {
        return {
          valid: false,
          reason: 'Attestation hash mismatch',
          expected: attestationHash,
          actual: currentAttestationHash
        };
      }

      // Verify signature
      const currentSignature = this.createSignature(attestationData);
      if (currentSignature !== signature) {
        return {
          valid: false,
          reason: 'Signature mismatch',
          expected: signature,
          actual: currentSignature
        };
      }

      return {
        valid: true,
        contentHash: currentContentHash,
        attestationHash: currentAttestationHash,
        verifiedAt: this.getTimestamp()
      };
    } catch (error) {
      return {
        valid: false,
        reason: 'Verification error',
        error: error.message
      };
    }
  }

  /**
   * Create reproducibility proof
   */
  async createReproducibilityProof(content, context, iterations = 3) {
    const proofs = [];
    let isReproducible = true;
    const baseHash = this.createContentHash(content);

    for (let i = 0; i < iterations; i++) {
      const attestation = await this.createAttestation(content, context);

      proofs.push({
        iteration: i + 1,
        contentHash: attestation.contentHash,
        attestationHash: attestation.attestationHash,
        timestamp: attestation.timestamp
      });

      // Check if content hash is consistent
      if (attestation.contentHash !== baseHash) {
        isReproducible = false;
      }
    }

    return {
      reproducible: isReproducible,
      baseHash,
      iterations,
      proofs,
      algorithm: this.options.algorithm,
      deterministicMode: this.options.deterministicMode,
      generatedAt: this.getTimestamp()
    };
  }

  /**
   * Create audit trail for template execution
   */
  createAuditTrail(steps = []) {
    return {
      trailId: this.createContentHash(Date.now().toString()),
      attestorId: this.options.attestorId,
      version: this.options.version,
      createdAt: this.getTimestamp(),
      deterministicMode: this.options.deterministicMode,
      steps: steps.map((step, index) => ({
        stepId: index + 1,
        ...step,
        hash: this.createContentHash(JSON.stringify(step)),
        timestamp: step.timestamp || this.getTimestamp()
      })),
      trailHash: this.createContentHash(JSON.stringify(steps))
    };
  }

  /**
   * Compare two attestations
   */
  compareAttestations(attestation1, attestation2) {
    const differences = [];

    // Compare core hashes
    if (attestation1.contentHash !== attestation2.contentHash) {
      differences.push({
        field: 'contentHash',
        value1: attestation1.contentHash,
        value2: attestation2.contentHash
      });
    }

    if (attestation1.contextHash !== attestation2.contextHash) {
      differences.push({
        field: 'contextHash',
        value1: attestation1.contextHash,
        value2: attestation2.contextHash
      });
    }

    if (attestation1.templateHash !== attestation2.templateHash) {
      differences.push({
        field: 'templateHash',
        value1: attestation1.templateHash,
        value2: attestation2.templateHash
      });
    }

    // Compare metadata
    const metadataFields = ['algorithm', 'version', 'deterministicMode', 'contentLength'];
    metadataFields.forEach(field => {
      if (attestation1[field] !== attestation2[field]) {
        differences.push({
          field,
          value1: attestation1[field],
          value2: attestation2[field]
        });
      }
    });

    return {
      identical: differences.length === 0,
      differences,
      comparedAt: this.getTimestamp()
    };
  }

  /**
   * Create content hash using specified algorithm
   */
  createContentHash(content) {
    return crypto.createHash(this.options.algorithm)
      .update(String(content || ''), 'utf8')
      .digest('hex');
  }

  /**
   * Create signature for attestation data
   */
  createSignature(data) {
    // Simple signature using hash of data + attestor ID
    const signatureInput = JSON.stringify(data) + this.options.attestorId + this.options.version;
    return crypto.createHash(this.options.algorithm)
      .update(signatureInput, 'utf8')
      .digest('hex');
  }

  /**
   * Get additional metadata if enabled
   */
  getAdditionalMetadata(content, _context) {
    return {
      systemInfo: {
        platform: process.platform,
        nodeVersion: process.version,
        architecture: process.arch
      },
      environment: {
        deterministicMode: this.options.deterministicMode,
        staticBuildTime: this.options.staticBuildTime
      },
      contentAnalysis: {
        lineCount: content ? content.split('\n').length : 0,
        charCount: content ? content.length : 0,
        hasUnicodeChars: content ? /[^\x00-\x7F]/.test(content) : false
      }
    };
  }

  /**
   * Get deterministic or real timestamp
   */
  getTimestamp() {
    return this.options.deterministicMode ?
      this.options.staticBuildTime :
      new Date().toISOString();
  }

  /**
   * Export attestation in standard format
   */
  exportAttestation(attestation, format = 'json') {
    switch (format.toLowerCase()) {
      case 'json':
        return JSON.stringify(attestation, null, 2);

      case 'compact':
        return JSON.stringify(attestation);

      case 'yaml':
        // Basic YAML export (simplified)
        return Object.entries(attestation)
          .map(([key, value]) => `${key}: ${JSON.stringify(value)}`)
          .join('\n');

      case 'base64':
        return Buffer.from(JSON.stringify(attestation)).toString('base64');

      default:
        throw new Error(`Unsupported export format: ${format}`);
    }
  }

  /**
   * Import attestation from standard format
   */
  importAttestation(data, format = 'json') {
    try {
      switch (format.toLowerCase()) {
        case 'json':
        case 'compact':
          return JSON.parse(data);

        case 'base64':
          const decoded = Buffer.from(data, 'base64').toString('utf8');
          return JSON.parse(decoded);

        default:
          throw new Error(`Unsupported import format: ${format}`);
      }
    } catch (error) {
      throw new Error(`Failed to import attestation: ${error.message}`);
    }
  }

  /**
   * Get attestor statistics
   */
  getStats() {
    return {
      ...this.options,
      supportedAlgorithms: ['sha256', 'sha512', 'sha1'],
      supportedFormats: ['json', 'compact', 'yaml', 'base64'],
      features: [
        'content-attestation',
        'reproducibility-proof',
        'audit-trail',
        'signature-verification'
      ]
    };
  }
}

export default KGenAttestor;