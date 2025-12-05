/**
 * KGC Git Backbone - Content-addressable file system for snapshots and code
 * Uses Git CLI in Node.js for fast, simple operations
 *
 * Note: Browser support (isomorphic-git) can be added later as needed
 */

import { execSync } from 'child_process';
import { writeFileSync } from 'fs';
import { join } from 'path';

export class GitBackbone {
  constructor(dir = '.') {
    this.dir = dir;
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

      let result;
      const timestamp = new Date().toISOString();
      const commitMsg = `${message}\n\nSnapshot generated at ${timestamp}`;

      if (hasChanges) {
        // Commit normally
        result = execSync(`git commit -m "${commitMsg.replace(/"/g, '\\"')}"`, {
          cwd: this.dir,
          encoding: 'utf8',
        });
      } else {
        // Allow empty commit when content unchanged (idempotent freezes)
        result = execSync(`git commit --allow-empty -m "${commitMsg.replace(/"/g, '\\"')}"`, {
          cwd: this.dir,
          encoding: 'utf8',
        });
      }

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
