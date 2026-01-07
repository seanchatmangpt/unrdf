#!/usr/bin/env node

/**
 * @file prepare-evidence.mjs
 * Convert evidence from evidenceClass string to boolean mappings for validator
 */

import { readFileSync, writeFileSync } from 'fs';

const rawEvidence = JSON.parse(readFileSync('./evidence-phase-1.json', 'utf-8'));

const prepared = rawEvidence.map(item => ({
  ...item,
  // Convert evidenceClass string to boolean mappings
  classA: item.evidenceClass === 'A',
  classB: item.evidenceClass === 'B',
  classC: item.evidenceClass === 'C',
  // Ensure numeric fields
  primaryPercent: item.primarySourcePercent || 0,
  quantitativeRigor: item.quantitativeRigor || 0,
  // Keep other fields as-is
}));

writeFileSync('./evidence-phase-1-prepared.json', JSON.stringify(prepared, null, 2));
console.log(`✓ Converted ${prepared.length} evidence items`);
console.log('Sample item:');
console.log(JSON.stringify(prepared[0], null, 2).split('\n').slice(0, 20).join('\n'));
