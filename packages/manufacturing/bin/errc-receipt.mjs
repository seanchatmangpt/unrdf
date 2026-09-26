#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import {
  accountRepositoryFacts,
  serializeRepositoryFactReceipt,
} from '../src/repository-fact-accounting.mjs';
import { collectGitRepositoryFacts } from '../src/git-repository-facts.mjs';

function parseArgs(argv) {
  const out = {};
  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i];
    if (!token.startsWith('--')) throw new Error(`REFUSED:UNKNOWN_ARGUMENT:${token}`);
    const key = token.slice(2);
    const value = argv[i + 1];
    if (!value || value.startsWith('--')) throw new Error(`REFUSED:MISSING_VALUE:${key}`);
    out[key] = value;
    i += 1;
  }
  return out;
}

try {
  const args = parseArgs(process.argv.slice(2));
  if (!args.repository) throw new Error('REFUSED:REPOSITORY_REQUIRED');
  const cwd = path.resolve(args.cwd ?? process.cwd());
  const ownershipManifest = args.ownership
    ? JSON.parse(fs.readFileSync(path.resolve(args.ownership), 'utf8'))
    : {};

  const facts = collectGitRepositoryFacts({
    cwd,
    repository: args.repository,
    commit: args.commit,
    ownershipManifest,
  });
  const receipt = accountRepositoryFacts(facts);
  const payload = serializeRepositoryFactReceipt(receipt);

  if (args.output) {
    fs.writeFileSync(path.resolve(args.output), payload);
  } else {
    process.stdout.write(payload);
  }
} catch (error) {
  process.stderr.write(String(error?.message ?? error) + '\n');
  process.exitCode = 2;
}
