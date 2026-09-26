/**
 * @file Collect immutable repository facts directly from Git.
 * @module manufacturing/git-repository-facts
 */

import crypto from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { OwnershipKind } from './repository-fact-accounting.mjs';

const SHA40 = /^[0-9a-f]{40}$/;

function git(cwd, args) {
  return execFileSync('git', args, {
    cwd,
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'],
  }).trim();
}

function sha256(data) {
  return 'sha256:' + crypto.createHash('sha256').update(data).digest('hex');
}

function countLoc(data) {
  if (data.length === 0) return 0;
  const text = data.toString('utf8');
  if (text.includes('\u0000')) return 0;
  return text.split(/\r?\n/).length - (text.endsWith('\n') ? 1 : 0);
}

/**
 * Observe exact tracked-file repository facts.
 *
 * ownershipManifest is keyed by repository-relative path. Missing entries are
 * emitted as UNKNOWN so the accounting layer charges them as handwritten.
 *
 * @param {{
 *   cwd: string,
 *   repository: string,
 *   commit?: string,
 *   ownershipManifest?: Record<string, object>
 * }} input
 */
export function collectGitRepositoryFacts(input) {
  const cwd = path.resolve(input.cwd);
  const observedCommit = git(cwd, ['rev-parse', 'HEAD']);
  const commit = input.commit ?? observedCommit;

  if (!SHA40.test(commit)) {
    throw new Error('REFUSED:MUTABLE_OR_INVALID_GIT_SUBJECT');
  }
  if (observedCommit !== commit) {
    throw new Error(`REFUSED:GIT_SUBJECT_MISMATCH:expected=${commit}:observed=${observedCommit}`);
  }

  const status = git(cwd, ['status', '--porcelain=v1', '--untracked-files=no']);
  if (status) {
    throw new Error('REFUSED:DIRTY_TRACKED_WORKTREE');
  }

  const listed = execFileSync('git', ['ls-files', '-s', '-z'], {
    cwd,
    encoding: 'utf8',
  });
  const records = listed.split('\u0000').filter(Boolean);
  const manifest = input.ownershipManifest ?? {};

  const facts = records.map((record) => {
    const match = record.match(/^(\d+)\s+([0-9a-f]+)\s+(\d+)\t(.+)$/s);
    if (!match) throw new Error(`REFUSED:MALFORMED_GIT_INDEX_RECORD:${record}`);

    const [, mode, blobSha, stage, relPath] = match;
    if (stage !== '0') {
      throw new Error(`REFUSED:UNMERGED_INDEX_ENTRY:${relPath}:stage=${stage}`);
    }

    const bytes = fs.readFileSync(path.join(cwd, relPath));
    const ownership = manifest[relPath] ?? {};
    return {
      path: relPath,
      loc: countLoc(bytes),
      bytes: bytes.length,
      blobSha,
      contentDigest: sha256(bytes),
      ownership: ownership.ownership ?? OwnershipKind.UNKNOWN,
      sourceOwner: ownership.sourceOwner,
      generator: ownership.generator,
      generatorDigest: ownership.generatorDigest,
      sourceDigest: ownership.sourceDigest,
      materializationReceipt: ownership.materializationReceipt,
      gitMode: mode,
    };
  }).sort((a, b) => a.path.localeCompare(b.path));

  return {
    subject: {
      repository: input.repository,
      commit,
    },
    facts,
  };
}
