/**
 * KGC Git Backbone - Content-addressable file system for snapshots and code
 * Uses Git CLI in Node.js and isomorphic-git in Browser
 */

import { execSync } from 'child_process';

export class GitBackbone {
  constructor(dir = '.') {
    this.dir = dir;
    this.isNode = typeof process !== 'undefined' && typeof execSync === 'function';
  }

  /**
   * Persist N-Quads snapshot to Git and return commit hash
   */
  async commitSnapshot(nquads, message) {
    if (this.isNode) {
      return this._commitSnapshotNode(nquads, message);
    }
    return this._commitSnapshotBrowser(nquads, message);
  }

  /**
   * Read snapshot from Git by commit hash
   */
  async readSnapshot(hash) {
    if (this.isNode) {
      return this._readSnapshotNode(hash);
    }
    return this._readSnapshotBrowser(hash);
  }

  // ===== Node.js Implementation =====

  /**
   * Node.js: Use Git CLI for fast, simple operation
   */
  _commitSnapshotNode(nquads, message) {
    try {
      const fs = require('fs');
      const path = require('path');

      // Write snapshot to file
      const snapshotPath = path.join(this.dir, 'snapshot.nq');
      fs.writeFileSync(snapshotPath, nquads, 'utf8');

      // Add to git
      execSync('git add snapshot.nq', { cwd: this.dir });

      // Commit
      const timestamp = new Date().toISOString();
      const commitMsg = `${message}\n\nSnapshot generated at ${timestamp}`;
      const result = execSync(`git commit -m "${commitMsg.replace(/"/g, '\\"')}"`, {
        cwd: this.dir,
        encoding: 'utf8',
      });

      // Extract commit hash from output
      // Output format: [main abc123f] message
      const match = result.match(/\[[\w\s]+\s+([a-f0-9]+)\]/);
      const hash = match ? match[1] : null;

      if (!hash) {
        throw new Error('Failed to extract commit hash from git output');
      }

      return hash;
    } catch (error) {
      throw new Error(`Git commit failed: ${error.message}`);
    }
  }

  /**
   * Node.js: Read snapshot from Git by commit hash
   */
  _readSnapshotNode(hash) {
    try {
      const result = execSync(`git show ${hash}:snapshot.nq`, {
        cwd: this.dir,
        encoding: 'utf8',
      });
      return result;
    } catch (error) {
      throw new Error(`Failed to read snapshot ${hash}: ${error.message}`);
    }
  }

  // ===== Browser Implementation =====

  /**
   * Browser: Use isomorphic-git with lightning-fs
   */
  async _commitSnapshotBrowser(nquads, message) {
    const git = (await import('isomorphic-git')).default;
    const fs = (await import('lightning-fs')).default;

    const lfs = new fs('kgc');

    try {
      // Write snapshot
      await lfs.promises.writeFile('/snapshot.nq', nquads, 'utf8');

      // Add to git
      await git.add({ fs: lfs, dir: '/', filepath: 'snapshot.nq' });

      // Commit
      const sha = await git.commit({
        fs: lfs,
        dir: '/',
        message,
        author: { name: 'KGC System', email: 'system@kgc.io' },
      });

      return sha;
    } catch (error) {
      throw new Error(`Browser Git commit failed: ${error.message}`);
    }
  }

  /**
   * Browser: Read snapshot from Git by commit hash
   */
  async _readSnapshotBrowser(hash) {
    const git = (await import('isomorphic-git')).default;
    const fs = (await import('lightning-fs')).default;

    const lfs = new fs('kgc');

    try {
      const { object } = await git.readObject({
        fs: lfs,
        dir: '/',
        oid: hash,
      });

      return new TextDecoder().decode(object);
    } catch (error) {
      throw new Error(`Browser Git read failed: ${error.message}`);
    }
  }
}
