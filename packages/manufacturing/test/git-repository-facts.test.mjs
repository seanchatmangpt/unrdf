import { describe, expect, it } from 'vitest';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { collectGitRepositoryFacts } from '../src/git-repository-facts.mjs';
import { accountRepositoryFacts } from '../src/repository-fact-accounting.mjs';

function git(cwd, args) {
  return execFileSync('git', args, {
    cwd,
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'],
  }).trim();
}

function fixture() {
  const cwd = fs.mkdtempSync(path.join(os.tmpdir(), 'unrdf-errc-'));
  git(cwd, ['init', '-q']);
  git(cwd, ['config', 'user.name', 'ERRC Court']);
  git(cwd, ['config', 'user.email', 'errc@example.test']);
  fs.mkdirSync(path.join(cwd, 'src'), { recursive: true });
  fs.writeFileSync(path.join(cwd, 'src', 'hand.mjs'), 'a\nb\nc\n');
  fs.writeFileSync(path.join(cwd, 'src', 'generated.mjs'), 'x\ny\n');
  git(cwd, ['add', '.']);
  git(cwd, ['commit', '-qm', 'fixture']);
  return cwd;
}

describe('Git repository-fact collector', () => {
  it('binds facts to exact HEAD and defaults unknown ownership to handwritten accounting', () => {
    const cwd = fixture();
    const commit = git(cwd, ['rev-parse', 'HEAD']);
    const input = collectGitRepositoryFacts({
      cwd,
      repository: 'example/repo',
      ownershipManifest: {
        'src/generated.mjs': {
          ownership: 'generated',
          generator: 'ggen',
          generatorDigest: 'sha256:' + 'a'.repeat(64),
        },
      },
    });
    expect(input.subject.commit).toBe(commit);
    const receipt = accountRepositoryFacts(input);
    expect(receipt.totals.totalLoc).toBe(5);
    expect(receipt.totals.handwrittenLoc).toBe(3);
    expect(receipt.totals.generatedLoc).toBe(2);
  });

  it('refuses a caller-supplied commit different from observed HEAD', () => {
    const cwd = fixture();
    expect(() => collectGitRepositoryFacts({
      cwd,
      repository: 'example/repo',
      commit: '0'.repeat(40),
    })).toThrow('REFUSED:GIT_SUBJECT_MISMATCH');
  });

  it('refuses dirty tracked files instead of receipting unseen bytes', () => {
    const cwd = fixture();
    fs.appendFileSync(path.join(cwd, 'src', 'hand.mjs'), 'dirty\n');
    expect(() => collectGitRepositoryFacts({
      cwd,
      repository: 'example/repo',
    })).toThrow('REFUSED:DIRTY_TRACKED_WORKTREE');
  });

  it('does not let untracked files silently enter the repository subject', () => {
    const cwd = fixture();
    fs.writeFileSync(path.join(cwd, 'untracked.mjs'), 'not part of exact subject\n');
    const input = collectGitRepositoryFacts({ cwd, repository: 'example/repo' });
    expect(input.facts.some((fact) => fact.path === 'untracked.mjs')).toBe(false);
  });
});
