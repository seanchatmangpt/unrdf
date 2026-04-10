/**
 * @file Artifact Module
 * @module manufacturing/artifact
 * @description Artifact creation with SHA-256 receipt chaining for deterministic A=μ(O) output
 */

import { createHash } from 'crypto';

/**
 * The 15 manufacturable artifact kinds.
 * Matches ostar's /api/v1/artifact-kinds endpoint.
 */
export const ARTIFACT_KINDS = [
  'powl',
  'sparql',
  'shacl',
  'mcp-tool',
  'documentation',
  'python',
  'typescript',
  'elixir',
  'go',
  'rust',
  'java',
  'yaml',
  'json',
  'ttl',
  'rq',
];

/**
 * A manufactured artifact with cryptographic receipt.
 * The receipt is a SHA-256 hash of the content, providing:
 * - Determinism: same content → same receipt
 * - Integrity: receipt detects content tampering
 */
export class Artifact {
  /**
   * @param {string} type - Artifact kind (must be in ARTIFACT_KINDS)
   * @param {string|object} content - Artifact content
   * @throws {Error} if type is not a known artifact kind
   */
  constructor(type, content) {
    if (!ARTIFACT_KINDS.includes(type)) {
      throw new Error(
        `Unknown artifact kind: "${type}". Known kinds: ${ARTIFACT_KINDS.join(', ')}`,
      );
    }
    this.type = type;
    this.content = content;
    this.receipt = computeReceipt(content);
    this.timestamp = new Date().toISOString();
  }

  /**
   * Serialize artifact metadata (excludes raw content for size efficiency).
   * @returns {{ type: string, receipt: string, timestamp: string, contentLength: number }}
   */
  toJSON() {
    const str = typeof this.content === 'string' ? this.content : JSON.stringify(this.content);
    return {
      type: this.type,
      receipt: this.receipt,
      timestamp: this.timestamp,
      contentLength: str.length,
    };
  }
}

/**
 * Factory function for creating artifacts.
 * @param {string} type - Artifact kind
 * @param {string|object} content - Artifact content
 * @returns {Artifact}
 */
export function createArtifact(type, content) {
  return new Artifact(type, content);
}

/**
 * Compute a deterministic SHA-256 receipt for content.
 * @param {string|object} content
 * @returns {string} 64-character hex digest
 */
function computeReceipt(content) {
  const str = typeof content === 'string' ? content : JSON.stringify(content);
  return createHash('sha256').update(str, 'utf8').digest('hex');
}
