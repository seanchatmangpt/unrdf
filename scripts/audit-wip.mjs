#!/usr/bin/env node

import { createHash } from 'node:crypto';
import { readdirSync, readFileSync, statSync } from 'node:fs';
import { basename, dirname, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import process from 'node:process';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const SELF = 'scripts/audit-wip.mjs';
const DEFAULT_SCOPES = ['packages', 'scripts'];
const SOURCE_EXTENSIONS = new Set(['.mjs', '.js', '.cjs', '.ts', '.tsx']);
const EXCLUDED_SEGMENTS = new Set([
  '.git', '.next', '.artifacts', 'archive', 'coverage', 'dist', 'historical',
  'node_modules', 'tmp', 'unrdf-archive', 'vendors',
]);
const INTENTIONAL = new Map([
  ['packages/manufacturing/src/operators/base.mjs', 'ABSTRACT_OPERATOR_CONTRACT'],
]);

function parseArgs(argv) {
  const scopes = [];
  let json = false;
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--json') json = true;
    else if (arg === '--scope') {
      const value = argv[index + 1];
      if (!value) throw new Error('--scope requires a repository-relative path');
      scopes.push(value);
      index += 1;
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }
  return { json, scopes: scopes.length > 0 ? scopes : DEFAULT_SCOPES };
}

function walk(path, files = []) {
  const stat = statSync(path);
  if (stat.isFile()) {
    files.push(path);
    return files;
  }

  for (const entry of readdirSync(path, { withFileTypes: true })) {
    if (entry.isDirectory() && EXCLUDED_SEGMENTS.has(entry.name)) continue;
    walk(resolve(path, entry.name), files);
  }
  return files;
}

function extension(path) {
  const match = path.match(/\.[^.]+$/);
  return match ? match[0] : '';
}

function lineOf(content, offset) {
  return content.slice(0, offset).split('\n').length;
}

function stripNonCode(content) {
  let state = 'code';
  let quote = null;
  let result = '';

  for (let index = 0; index < content.length; index += 1) {
    const char = content[index];
    const next = content[index + 1];

    if (state === 'line-comment') {
      if (char === '\n') {
        state = 'code';
        result += '\n';
      } else result += ' ';
      continue;
    }

    if (state === 'block-comment') {
      if (char === '*' && next === '/') {
        result += '  ';
        index += 1;
        state = 'code';
      } else result += char === '\n' ? '\n' : ' ';
      continue;
    }

    if (state === 'string') {
      if (char === '\\') {
        result += ' ';
        if (index + 1 < content.length) {
          result += content[index + 1] === '\n' ? '\n' : ' ';
          index += 1;
        }
      } else if (char === quote) {
        result += ' ';
        state = 'code';
        quote = null;
      } else result += char === '\n' ? '\n' : ' ';
      continue;
    }

    if (char === '/' && next === '/') {
      result += '  ';
      index += 1;
      state = 'line-comment';
    } else if (char === '/' && next === '*') {
      result += '  ';
      index += 1;
      state = 'block-comment';
    } else if (char === '"' || char === "'" || char === '`') {
      result += ' ';
      state = 'string';
      quote = char;
    } else result += char;
  }

  return result;
}

function extractComments(content) {
  return [...content.matchAll(/\/\*[\s\S]*?\*\/|\/\/[^\n]*/g)];
}

function classify(path, content, rule) {
  if (content.includes('@generated') || content.includes('DO NOT EDIT')) {
    return { classification: 'GENERATED', reason: 'GENERATED_PROJECTION' };
  }
  if (INTENTIONAL.has(path)) {
    return { classification: 'INTENTIONAL', reason: INTENTIONAL.get(path) };
  }
  if (rule === 'BACKUP_ARTIFACT' || rule === 'PLACEHOLDER_FILE' || rule === 'SKIPPED_TEST' || rule === 'DEFERRED_RUNTIME' || rule === 'COMMENT_MARKER') {
    return { classification: 'ACTIONABLE', reason: rule };
  }
  return { classification: 'REVIEWED', reason: rule };
}

function addFinding(findings, path, content, rule, offset, excerpt) {
  const verdict = classify(path, content, rule);
  findings.push({
    path,
    line: lineOf(content, offset),
    rule,
    ...verdict,
    excerpt: excerpt.trim().slice(0, 220),
  });
}

function inspectFile(absolutePath, findings) {
  const path = relative(REPO_ROOT, absolutePath).replaceAll('\\', '/');
  if (path === SELF) return;

  const name = basename(path);
  const content = readFileSync(absolutePath, 'utf8');

  if (/\.(?:bak|orig|rej)$/i.test(name)) {
    addFinding(findings, path, content, 'BACKUP_ARTIFACT', 0, name);
  }
  if (/^placeholder\./i.test(name)) {
    addFinding(findings, path, content, 'PLACEHOLDER_FILE', 0, name);
  }

  if (!SOURCE_EXTENSIONS.has(extension(path))) return;

  const code = stripNonCode(content);
  const skippedPattern = /\b(?:describe|it|test)\.skip\s*\(|\b(?:xit|xdescribe)\s*\(/g;
  for (const match of code.matchAll(skippedPattern)) {
    addFinding(findings, path, content, 'SKIPPED_TEST', match.index, match[0]);
  }

  const markerPattern = /\b(?:TODO|FIXME|HACK|XXX)\b\s*:/gi;
  for (const comment of extractComments(content)) {
    for (const marker of comment[0].matchAll(markerPattern)) {
      addFinding(findings, path, content, 'COMMENT_MARKER', comment.index + marker.index, marker[0]);
    }
  }

  const deferredPattern = /(?:not\s+(?:yet\s+)?implemented|implementation\s+pending)/gi;
  for (const match of content.matchAll(deferredPattern)) {
    const lineStart = content.lastIndexOf('\n', match.index) + 1;
    const lineEnd = content.indexOf('\n', match.index);
    const line = content.slice(lineStart, lineEnd === -1 ? undefined : lineEnd);
    if (/throw\s+new\s+Error|message\s*:|reason\s*:|return\s+/.test(line)) {
      addFinding(findings, path, content, 'DEFERRED_RUNTIME', match.index, line);
    }
  }
}

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map(key => [key, canonical(value[key])]));
  }
  return value;
}

function main() {
  const { json, scopes } = parseArgs(process.argv.slice(2));
  const files = scopes.flatMap(scope => walk(resolve(REPO_ROOT, scope))).filter((value, index, all) => all.indexOf(value) === index);
  const findings = [];
  for (const file of files.sort()) inspectFile(file, findings);

  findings.sort((a, b) => a.path.localeCompare(b.path) || a.line - b.line || a.rule.localeCompare(b.rule));
  const actionable = findings.filter(item => item.classification === 'ACTIONABLE');
  const receiptWithoutDigest = {
    schema: 'urn:unrdf:wip-audit-receipt:v1',
    subject: {
      scopes,
      scannedFiles: files.length,
    },
    standing: actionable.length === 0 ? 'ALIVE' : 'PARTIAL_ALIVE',
    summary: {
      actionable: actionable.length,
      generated: findings.filter(item => item.classification === 'GENERATED').length,
      intentional: findings.filter(item => item.classification === 'INTENTIONAL').length,
      reviewed: findings.filter(item => item.classification === 'REVIEWED').length,
    },
    findings,
  };
  const digest = createHash('sha256').update(JSON.stringify(canonical(receiptWithoutDigest))).digest('hex');
  const receipt = { ...receiptWithoutDigest, digest };

  if (json) process.stdout.write(`${JSON.stringify(receipt, null, 2)}\n`);
  else {
    process.stdout.write(`WIP audit: ${receipt.standing}\n`);
    process.stdout.write(`Scanned: ${files.length} files; actionable: ${actionable.length}\n`);
    for (const finding of findings) {
      process.stdout.write(`${finding.classification.padEnd(11)} ${finding.path}:${finding.line} ${finding.rule} ${finding.excerpt}\n`);
    }
    process.stdout.write(`Receipt: ${digest}\n`);
  }

  process.exitCode = actionable.length === 0 ? 0 : 1;
}

main();
