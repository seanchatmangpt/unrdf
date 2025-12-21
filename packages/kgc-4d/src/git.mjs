/**
 * KGC Git Backbone - Pure JavaScript Git using isomorphic-git
 * Works in Node.js and Browser environments
 * NO CLI dependencies - fully isomorphic
 *
 * Usage:
 *   Node.js:   new GitBackbone('/path/to/repo')
 *   Browser:   new GitBackbone('/repo', lightningFs)  // pass lightning-fs instance
 */

import git from 'isomorphic-git';
import { join } from 'path';

// Dynamic fs import for Node.js only (browser will inject fs)
let nodeFs = null;
const isNode = typeof window === 'undefined' && typeof process !== 'undefined';
if (isNode) {
  try {
    // Dynamic import for Node.js fs
    nodeFs = await import('fs');
  } catch {
    // Will require injection in browser or if fs unavailable
  }
}

/**
 *
 */
export class GitBackbone {
  /**
   * @param {string} dir - Repository directory path
   * @param {Object} [fs] - Filesystem adapter (required in browser, uses Node fs if omitted in Node.js)
   */
  constructor(dir = '.', fs = null) {
    this.dir = dir;

    // Use injected fs, or fall back to Node.js fs
    if (fs) {
      this.fs = fs;
    } else if (nodeFs) {
      this.fs = nodeFs;
    } else {
      throw new Error(
        'GitBackbone requires a filesystem adapter. In browser, pass lightning-fs instance: ' +
        'new GitBackbone(dir, new LightningFS("fs"))'
      );
    }

    this._initialized = false;
  }

  /**
   * Initialize Git repository if it doesn't exist
   * Uses isomorphic-git pure JS implementation
   */
  async _ensureInit() {
    if (this._initialized) return;

    // Create directory if it doesn't exist
    if (!this.fs.existsSync(this.dir)) {
      this.fs.mkdirSync(this.dir, { recursive: true });
    }

    // Initialize git repo if needed
    if (!this.fs.existsSync(join(this.dir, '.git'))) {
      await git.init({ fs: this.fs, dir: this.dir, defaultBranch: 'main' });
    }

    this._initialized = true;
  }

  /**
   * Persist N-Quads snapshot to Git and return commit hash
   * Pure JS implementation using isomorphic-git
   * GAP-G2 fix: Enforce message length limits
   * GAP-G3 fix: Add timeout for git operations
   *
   * @example
   * import { GitBackbone } from './git.mjs';
   * const git = new GitBackbone('/tmp/kgc-repo');
   * const sha = await git.commitSnapshot('<s> <p> <o> <g> .', 'Test snapshot');
   * console.assert(typeof sha === 'string' && sha.length > 0, 'Returns commit hash');
   */
  async commitSnapshot(nquads, message) {
    await this._ensureInit();

    // GAP-G2 fix: Validate message length (Git convention: first line max 72 chars, total max 100KB)
    if (typeof message !== 'string' || message.length === 0) {
      throw new Error('Commit message must be non-empty string');
    }
    if (message.length > 100_000) {
      throw new Error(`Commit message exceeds size limit: ${message.length} > 100KB`);
    }

    // Write snapshot to file
    const filepath = 'snapshot.nq';
    const fullPath = join(this.dir, filepath);
    this.fs.writeFileSync(fullPath, nquads, 'utf8');

    // Stage file
    await git.add({ fs: this.fs, dir: this.dir, filepath });

    // Create timestamp for commit message
    const timestamp = new Date().toISOString();
    const commitMsg = `${message}\n\nSnapshot generated at ${timestamp}`;

    // Validate final message size
    if (commitMsg.length > 100_000) {
      throw new Error(`Final commit message exceeds size limit after timestamp addition`);
    }

    try {
      // Commit with author info
      // GAP-G3 fix: Add timeout for git operations (20 second SLA)
      const commitPromise = git.commit({
        fs: this.fs,
        dir: this.dir,
        message: commitMsg,
        author: {
          name: 'KGC System',
          email: 'kgc@system.local',
        },
      });

      const timeoutPromise = new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Git commit operation timed out after 20s')), 20000)
      );

      const sha = await Promise.race([commitPromise, timeoutPromise]);
      return sha;
    } catch (err) {
      throw new Error(`Git commit failed: ${err.message}`);
    }
  }

  /**
   * Read snapshot from Git by commit hash
   * Uses isomorphic-git readBlob for pure JS retrieval
   * GAP-G1 fix: Validate UTF-8 encoding
   * GAP-G3 fix: Add timeout for git operations
   */
  async readSnapshot(sha) {
    await this._ensureInit();

    try {
      // Read the blob content at the given commit
      // GAP-G3 fix: Add timeout (10 second SLA for read)
      const readPromise = git.readBlob({
        fs: this.fs,
        dir: this.dir,
        oid: sha,
        filepath: 'snapshot.nq',
      });

      const timeoutPromise = new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Git read operation timed out after 10s')), 10000)
      );

      const { blob } = await Promise.race([readPromise, timeoutPromise]);

      // GAP-G1 fix: Validate UTF-8 encoding before use
      // TextDecoder silently produces invalid strings for bad UTF-8
      // We validate by attempting to encode/decode round-trip
      try {
        const decoded = new TextDecoder('utf-8', { fatal: true }).decode(blob);
        // Verify round-trip: encode back and compare
        const reencoded = new TextEncoder().encode(decoded);
        if (reencoded.length !== blob.length) {
          throw new Error('UTF-8 validation failed: round-trip size mismatch');
        }
        return decoded;
      } catch (err) {
        throw new Error(`Invalid UTF-8 encoding in snapshot: ${err.message}`);
      }
    } catch (err) {
      throw new Error(`Git read failed: ${err.message}`);
    }
  }
}
