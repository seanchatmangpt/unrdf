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
   */
  async commitSnapshot(nquads, message) {
    await this._ensureInit();

    // Write snapshot to file
    const filepath = 'snapshot.nq';
    const fullPath = join(this.dir, filepath);
    this.fs.writeFileSync(fullPath, nquads, 'utf8');

    // Stage file
    await git.add({ fs: this.fs, dir: this.dir, filepath });

    // Create timestamp for commit message
    const timestamp = new Date().toISOString();
    const commitMsg = `${message}\n\nSnapshot generated at ${timestamp}`;

    // Commit with author info
    const sha = await git.commit({
      fs: this.fs,
      dir: this.dir,
      message: commitMsg,
      author: {
        name: 'KGC System',
        email: 'kgc@system.local',
      },
    });

    return sha;
  }

  /**
   * Read snapshot from Git by commit hash
   * Uses isomorphic-git readBlob for pure JS retrieval
   */
  async readSnapshot(sha) {
    await this._ensureInit();

    // Read the blob content at the given commit
    const { blob } = await git.readBlob({
      fs: this.fs,
      dir: this.dir,
      oid: sha,
      filepath: 'snapshot.nq',
    });

    // Convert Uint8Array to string
    return new TextDecoder().decode(blob);
  }
}
