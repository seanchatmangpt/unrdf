/**
 * KGC Git Backbone - Pure JavaScript Git using isomorphic-git
 * Works in Node.js and Browser environments
 * NO CLI dependencies - fully isomorphic
 */

import git from 'isomorphic-git';
import * as fs from 'fs';
import { join } from 'path';

export class GitBackbone {
  constructor(dir = '.') {
    this.dir = dir;
    this.fs = fs;
    this._initialized = false;
  }

  /**
   * Initialize Git repository if it doesn't exist
   * Uses isomorphic-git pure JS implementation
   */
  async _ensureInit() {
    if (this._initialized) return;

    // Create directory if it doesn't exist
    if (!fs.existsSync(this.dir)) {
      fs.mkdirSync(this.dir, { recursive: true });
    }

    // Initialize git repo if needed
    if (!fs.existsSync(join(this.dir, '.git'))) {
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
    fs.writeFileSync(fullPath, nquads, 'utf8');

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
