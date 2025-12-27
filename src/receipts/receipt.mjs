/**
 * @fileoverview Receipt - Cryptographic audit trail for admissibility decisions
 *
 * **Purpose**: Immutable, deterministic receipts proving:
 * - Which ontology releases were used (input hashes)
 * - What Δ capsule was evaluated (delta capsule hash)
 * - What decision was made (allow/deny)
 * - When it occurred (epoch τ)
 * - What universe state resulted (output hash)
 * - What toolchain was used (reproducibility)
 *
 * **Properties**:
 * - Deterministic: Same inputs → Same receipt hash
 * - Immutable: Object.freeze() prevents modification
 * - Chainable: beforeHash links to previous epoch
 * - Batchable: merkleRoot for grouping receipts
 * - Reproducible: Toolchain versions recorded
 *
 * @module receipts/receipt
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Receipt input hashes schema
 */
const InputHashesSchema = z.object({
  ontologyReleases: z.array(z.string()),
  deltaCapsule: z.string(),
});

/**
 * Toolchain version schema
 */
const ToolchainVersionSchema = z.object({
  node: z.string(),
  packages: z.record(z.string()),
});

/**
 * Receipt schema (Zod validation)
 */
const ReceiptSchema = z.object({
  inputHashes: InputHashesSchema,
  decision: z.enum(['allow', 'deny']),
  epoch: z.string().regex(/^τ_\d{4}_\d{2}_\d{2}_\d{4}_\d{3}$/),
  outputHash: z.string(),
  toolchainVersion: ToolchainVersionSchema,
  generatedAtTime: z.string().datetime(),
  beforeHash: z.string().nullable(),
  merkleRoot: z.string().nullable(),
  receiptHash: z.string().optional(),
});

/**
 * Receipt - Immutable cryptographic proof of admissibility decision
 *
 * @example
 * const receipt = await Receipt.create({
 *   inputHashes: {
 *     ontologyReleases: ['hash1', 'hash2'],
 *     deltaCapsule: 'hash3'
 *   },
 *   decision: 'allow',
 *   outputHash: 'hash4',
 *   toolchainVersion: {
 *     node: '18.19.0',
 *     packages: { '@unrdf/core': '5.0.1' }
 *   },
 *   beforeHash: 'hash0'
 * });
 */
export class Receipt {
  /**
   * Create a new receipt (use Receipt.create() instead)
   * @private
   */
  constructor(data) {
    // Validate with Zod
    const validated = ReceiptSchema.parse(data);

    // Copy all fields
    Object.assign(this, validated);

    // Freeze to make immutable
    Object.freeze(this);
  }

  /**
   * Create a new receipt with automatic epoch generation
   *
   * @param {object} options - Receipt options
   * @param {object} options.inputHashes - Input hashes {ontologyReleases: [], deltaCapsule: string}
   * @param {'allow'|'deny'} options.decision - Admissibility decision
   * @param {string} options.outputHash - Resulting universe state hash
   * @param {object} options.toolchainVersion - Toolchain versions {node: string, packages: {}}
   * @param {string|null} [options.beforeHash=null] - Previous epoch hash (for chaining)
   * @param {string|null} [options.merkleRoot=null] - Merkle root (for batching)
   * @param {Date} [options.timestamp=new Date()] - Generation timestamp
   * @returns {Promise<Receipt>} Immutable receipt with computed hash
   */
  static async create(options) {
    const timestamp = options.timestamp || new Date();
    const epoch = Receipt.generateEpoch(timestamp);

    // Build receipt data (excluding hash)
    const receiptData = {
      inputHashes: options.inputHashes,
      decision: options.decision,
      epoch,
      outputHash: options.outputHash,
      toolchainVersion: options.toolchainVersion,
      generatedAtTime: timestamp.toISOString(),
      beforeHash: options.beforeHash || null,
      merkleRoot: options.merkleRoot || null,
    };

    // Compute deterministic hash
    const receiptHash = await Receipt.computeHash(receiptData);

    // Create immutable receipt
    return new Receipt({
      ...receiptData,
      receiptHash,
    });
  }

  /**
   * Generate deterministic epoch τ from timestamp
   *
   * Format: τ_YYYY_MM_DD_HHMM_nnn
   * - YYYY: Year
   * - MM: Month (01-12)
   * - DD: Day (01-31)
   * - HHMM: Hour/minute (0000-2359)
   * - nnn: Milliseconds (000-999)
   *
   * @param {Date} timestamp - Timestamp
   * @returns {string} Epoch string
   */
  static generateEpoch(timestamp) {
    const year = timestamp.getUTCFullYear();
    const month = String(timestamp.getUTCMonth() + 1).padStart(2, '0');
    const day = String(timestamp.getUTCDate()).padStart(2, '0');
    const hour = String(timestamp.getUTCHours()).padStart(2, '0');
    const minute = String(timestamp.getUTCMinutes()).padStart(2, '0');
    const ms = String(timestamp.getUTCMilliseconds()).padStart(3, '0');

    return `τ_${year}_${month}_${day}_${hour}${minute}_${ms}`;
  }

  /**
   * Compute deterministic hash of receipt
   *
   * **Determinism guarantee**: Same receipt data → Same hash
   * - Uses canonical JSON serialization (sorted keys)
   * - BLAKE3 for cryptographic strength
   *
   * @param {object} receiptData - Receipt data (without receiptHash)
   * @returns {Promise<string>} SHA256 hash
   */
  static async computeHash(receiptData) {
    // Canonical serialization (sorted keys for determinism)
    const canonical = JSON.stringify(receiptData, Object.keys(receiptData).sort());

    // BLAKE3 hash
    return blake3(canonical);
  }

  /**
   * Serialize receipt to JSON-LD format
   *
   * @returns {object} JSON-LD representation
   */
  toJSONLD() {
    return {
      '@context': {
        unrdf: 'https://unrdf.org/vocab#',
        xsd: 'http://www.w3.org/2001/XMLSchema#',
        prov: 'http://www.w3.org/ns/prov#',
      },
      '@type': 'unrdf:Receipt',
      '@id': `urn:receipt:${this.receiptHash}`,
      'unrdf:inputHashes': {
        'unrdf:ontologyReleases': this.inputHashes.ontologyReleases,
        'unrdf:deltaCapsule': this.inputHashes.deltaCapsule,
      },
      'unrdf:decision': this.decision,
      'unrdf:epoch': this.epoch,
      'unrdf:outputHash': this.outputHash,
      'unrdf:toolchainVersion': {
        'unrdf:node': this.toolchainVersion.node,
        'unrdf:packages': this.toolchainVersion.packages,
      },
      'prov:generatedAtTime': {
        '@type': 'xsd:dateTime',
        '@value': this.generatedAtTime,
      },
      'unrdf:beforeHash': this.beforeHash,
      'unrdf:merkleRoot': this.merkleRoot,
      'unrdf:receiptHash': this.receiptHash,
    };
  }

  /**
   * Serialize receipt to Turtle (TTL) format
   *
   * @returns {string} Turtle serialization
   */
  toTurtle() {
    const jsonld = this.toJSONLD();
    const ns = {
      unrdf: 'https://unrdf.org/vocab#',
      xsd: 'http://www.w3.org/2001/XMLSchema#',
      prov: 'http://www.w3.org/ns/prov#',
    };

    const lines = [
      '@prefix unrdf: <https://unrdf.org/vocab#> .',
      '@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .',
      '@prefix prov: <http://www.w3.org/ns/prov#> .',
      '',
      `<urn:receipt:${this.receiptHash}> a unrdf:Receipt ;`,
      `  unrdf:decision "${this.decision}" ;`,
      `  unrdf:epoch "${this.epoch}" ;`,
      `  unrdf:outputHash "${this.outputHash}" ;`,
      `  prov:generatedAtTime "${this.generatedAtTime}"^^xsd:dateTime ;`,
    ];

    // Add ontology releases
    this.inputHashes.ontologyReleases.forEach((hash, idx) => {
      lines.push(`  unrdf:ontologyRelease "${hash}" ;`);
    });

    lines.push(`  unrdf:deltaCapsule "${this.inputHashes.deltaCapsule}" ;`);

    // Add toolchain
    lines.push(`  unrdf:nodeVersion "${this.toolchainVersion.node}" ;`);

    // Add optional fields
    if (this.beforeHash) {
      lines.push(`  unrdf:beforeHash "${this.beforeHash}" ;`);
    }
    if (this.merkleRoot) {
      lines.push(`  unrdf:merkleRoot "${this.merkleRoot}" ;`);
    }

    lines.push(`  unrdf:receiptHash "${this.receiptHash}" .`);

    return lines.join('\n');
  }

  /**
   * Verify receipt hash matches current data
   *
   * @returns {Promise<boolean>} True if hash is valid
   */
  async verify() {
    const { receiptHash: _hash, ...dataWithoutHash } = this;
    const recomputedHash = await Receipt.computeHash(dataWithoutHash);
    return recomputedHash === this.receiptHash;
  }

  /**
   * Create a receipt from JSON-LD
   *
   * @param {object} jsonld - JSON-LD object
   * @returns {Receipt} Receipt instance
   */
  static fromJSONLD(jsonld) {
    return new Receipt({
      inputHashes: {
        ontologyReleases: jsonld['unrdf:inputHashes']['unrdf:ontologyReleases'],
        deltaCapsule: jsonld['unrdf:inputHashes']['unrdf:deltaCapsule'],
      },
      decision: jsonld['unrdf:decision'],
      epoch: jsonld['unrdf:epoch'],
      outputHash: jsonld['unrdf:outputHash'],
      toolchainVersion: {
        node: jsonld['unrdf:toolchainVersion']['unrdf:node'],
        packages: jsonld['unrdf:toolchainVersion']['unrdf:packages'],
      },
      generatedAtTime: jsonld['prov:generatedAtTime']['@value'],
      beforeHash: jsonld['unrdf:beforeHash'],
      merkleRoot: jsonld['unrdf:merkleRoot'],
      receiptHash: jsonld['unrdf:receiptHash'],
    });
  }
}
