/**
 * KGC Git Backbone - Content-addressable file system for snapshots and code
 * Uses Git CLI in Node.js for fast, simple operations
 *
 * Note: Browser support (isomorphic-git) can be added later as needed
 */

import { execSync } from 'child_process';
import { writeFileSync, mkdirSync, existsSync } from 'fs';
import { join } from 'path';

export class GitBackbone {
  constructor(dir = '.') {
    this.dir = dir;
    this._initializeGit();
  }

  /**
   * Initialize Git repository if it doesn't exist
   */
  _initializeGit() {
    try {
      // Create directory if it doesn't exist
      if (!existsSync(this.dir)) {
        mkdirSync(this.dir, { recursive: true });
      }

      // Check if .git exists
      if (!existsSync(join(this.dir, '.git'))) {
        // Initialize git repo
        execSync('git init', { cwd: this.dir, stdio: 'pipe' });
        // Set user config (required for commits)
        execSync('git config user.email "kgc@system.local"', { cwd: this.dir, stdio: 'pipe' });
        execSync('git config user.name "KGC System"', { cwd: this.dir, stdio: 'pipe' });
      }
    } catch (error) {
      throw new Error(`Failed to initialize Git repository at ${this.dir}: ${error.message}`);
    }
  }

  /**
   * Persist N-Quads snapshot to Git and return commit hash
   */
  async commitSnapshot(nquads, message) {
    try {

      // Write snapshot to file
      const snapshotPath = join(this.dir, 'snapshot.nq');
      writeFileSync(snapshotPath, nquads, 'utf8');

      // Add to git
      execSync('git add snapshot.nq', { cwd: this.dir, stdio: 'pipe' });

      // Check if there are staged changes
      let hasChanges = true;
      try {
        execSync('git diff --cached --quiet', { cwd: this.dir, stdio: 'pipe' });
        hasChanges = false; // If command succeeds, no changes
      } catch {
        hasChanges = true; // If command fails, there are changes
      }

      const timestamp = new Date().toISOString();
      const commitMsg = `${message}\n\nSnapshot generated at ${timestamp}`;

      if (hasChanges) {
        // Commit normally
        execSync(`git commit -m "${commitMsg.replace(/"/g, '\\"')}"`, {
          cwd: this.dir,
          encoding: 'utf8',
          stdio: 'pipe',
        });
      } else {
        // Allow empty commit when content unchanged (idempotent freezes)
        execSync(`git commit --allow-empty -m "${commitMsg.replace(/"/g, '\\"')}"`, {
          cwd: this.dir,
          encoding: 'utf8',
          stdio: 'pipe',
        });
      }

      // Use git rev-parse to get the most recent commit hash
      const hash = execSync('git rev-parse HEAD', {
        cwd: this.dir,
        encoding: 'utf8',
      }).trim();

      if (!hash || hash.length === 0) {
        throw new Error('Failed to get commit hash from git rev-parse');
      }

      return hash;
    } catch (error) {
      throw new Error(`Git commit failed: ${error.message}`);
    }
  }

  /**
   * Read snapshot from Git by commit hash
   */
  async readSnapshot(hash) {
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
}
