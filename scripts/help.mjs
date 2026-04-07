#!/usr/bin/env node
/**
 * @file Script discovery helper — prints categorized list of all package.json scripts
 * Usage: pnpm help
 */

import { readFileSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const pkg = JSON.parse(readFileSync(resolve(__dirname, '../package.json'), 'utf8'));
const scripts = pkg.scripts ?? {};

const BOLD = '\x1b[1m';
const CYAN = '\x1b[36m';
const GRAY = '\x1b[90m';
const RESET = '\x1b[0m';
const isColor = process.stdout.isTTY;
const b = s => (isColor ? BOLD + s + RESET : s);
const c = s => (isColor ? CYAN + s + RESET : s);
const g = s => (isColor ? GRAY + s + RESET : s);

// Group by prefix (part before first colon)
const CATEGORIES = {
  test: 'Testing',
  bench: 'Benchmarks',
  benchmark: 'Benchmarks',
  profile: 'Profiling',
  lint: 'Quality',
  format: 'Quality',
  quality: 'Quality',
  build: 'Build',
  clean: 'Build',
  dev: 'Development',
  validate: 'Validation',
  mcp: 'MCP / Sync',
  unrdf: 'UNRDF Tools',
  check: 'Health Checks',
  debug: 'Debugging',
  trace: 'Debugging',
  watch: 'Development',
  analyze: 'Debugging',
  new: 'Scaffolding',
  update: 'Maintenance',
  list: 'Info',
  docs: 'Documentation',
};

const SKIP = new Set(['prepare', 'precommit']);

const groups = new Map();
const ungrouped = [];

for (const [name, cmd] of Object.entries(scripts)) {
  if (SKIP.has(name)) continue;
  const prefix = name.split(':')[0];
  const category = CATEGORIES[prefix] ?? null;
  if (category) {
    if (!groups.has(category)) groups.set(category, []);
    groups.get(category).push({ name, cmd });
  } else {
    ungrouped.push({ name, cmd });
  }
}

// Sort categories in priority order
const ORDER = [
  'Testing', 'Development', 'Quality', 'Build', 'Benchmarks',
  'Profiling', 'Validation', 'MCP / Sync', 'UNRDF Tools',
  'Health Checks', 'Debugging', 'Scaffolding', 'Maintenance',
  'Documentation', 'Info',
];

console.log(`\n${b('UNRDF Scripts')} ${g('— pnpm <script> [args]')}\n`);

for (const category of ORDER) {
  const items = groups.get(category);
  if (!items) continue;
  console.log(b(category));
  for (const { name } of items) {
    const padded = name.padEnd(30);
    console.log(`  ${c(padded)}  ${g(scripts[name].slice(0, 60))}`);
  }
  console.log('');
}

if (ungrouped.length > 0) {
  console.log(b('Other'));
  for (const { name } of ungrouped) {
    console.log(`  ${c(name.padEnd(30))}  ${g(scripts[name].slice(0, 60))}`);
  }
  console.log('');
}

console.log(g(`Total: ${Object.keys(scripts).length - SKIP.size} scripts\n`));
