#!/usr/bin/env node
import { readdir, readFile, writeFile } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../test/', import.meta.url));
const packageRoot = fileURLToPath(new URL('../', import.meta.url));
const forbidden = [
  ['VITEST_MOCK', /\bvi\.(?:fn|spyOn|mock|useFakeTimers)\b/],
  ['JEST_MOCK', /\bjest\.(?:fn|spyOn|mock|useFakeTimers)\b/],
  ['SINON_TEST_DOUBLE', /\bsinon\b/],
  ['INTERACTION_ASSERTION', /\btoHaveBeenCalled(?:Times|With)?\b/],
  ['MOCK_CONFIGURATION', /\bmock(?:Implementation|ReturnValue|ResolvedValue|RejectedValue)\b/],
  ['FAKE_TIMER', /\b(?:fakeTimers|useFakeTimers|advanceTimersByTime)\b/],
  ['INJECTED_PROCESS_RUNNER', /\brunner\s*:/],
];

async function files(directory) {
  const entries = await readdir(directory, { withFileTypes: true });
  const output = [];
  for (const entry of entries) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) output.push(...await files(path));
    else if (/\.(?:test|spec)\.mjs$/.test(entry.name)) output.push(path);
  }
  return output;
}

const violations = [];
for (const path of await files(root)) {
  const content = await readFile(path, 'utf8');
  const lines = content.split('\n');
  lines.forEach((line, index) => {
    for (const [code, pattern] of forbidden) {
      if (pattern.test(line)) {
        violations.push({ code, path: relative(packageRoot, path), line: index + 1, text: line.trim() });
      }
    }
  });
}

const report = {
  methodology: 'Chicago/Detroit TDD',
  inspectedTests: (await files(root)).length,
  forbiddenCategories: forbidden.map(([code]) => code),
  violations,
  status: violations.length === 0 ? 'ALIVE' : 'BLOCKED',
};
const output = process.env.CHICAGO_TDD_REPORT;
if (output) await writeFile(output, `${JSON.stringify(report, null, 2)}\n`);
console.log(JSON.stringify(report));
if (violations.length > 0) process.exit(1);
