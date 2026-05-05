#!/usr/bin/env node
/**
 * Verify extension structure and generate manifest entries
 */

import core from './packages/kgc-cli/src/extensions/core.mjs';
import domain from './packages/kgc-cli/src/extensions/domain.mjs';
import validation from './packages/kgc-cli/src/extensions/validation.mjs';
import consensus from './packages/kgc-cli/src/extensions/consensus.mjs';
import composables from './packages/kgc-cli/src/extensions/composables.mjs';

const extensions = [core, domain, validation, consensus, composables];

console.log('=== EXTENSION VERIFICATION ===\n');

extensions.forEach((ext, idx) => {
  console.log(`${idx + 1}. Extension: ${ext.id}`);
  console.log(`   Description: ${ext.description}`);
  console.log(`   Priority/LoadOrder: ${ext.priority}`);
  console.log(`   Nouns: ${Object.keys(ext.nouns).join(', ')}`);

  // Show verbs for each noun
  Object.entries(ext.nouns).forEach(([noun, nounDef]) => {
    const verbs = Object.keys(nounDef.verbs);
    console.log(`   - ${noun}: ${verbs.join(', ')}`);
  });
  console.log('');
});

console.log('\n=== MANIFEST ENTRIES ===\n');

const manifest = extensions.map(ext => ({
  id: ext.id,
  loadOrder: ext.priority,
  nouns: Object.keys(ext.nouns),
  verbs: Object.entries(ext.nouns).reduce((acc, [noun, nounDef]) => {
    acc[noun] = Object.keys(nounDef.verbs);
    return acc;
  }, {})
}));

console.log(JSON.stringify(manifest, null, 2));

console.log('\n=== VALIDATION SUMMARY ===\n');
console.log(`Total extensions: ${extensions.length}`);
console.log(`Total nouns: ${extensions.reduce((sum, ext) => sum + Object.keys(ext.nouns).length, 0)}`);
console.log(`Total verbs: ${extensions.reduce((sum, ext) =>
  sum + Object.values(ext.nouns).reduce((s, n) => s + Object.keys(n.verbs).length, 0), 0)}`);
console.log('\n✅ All extensions loaded successfully');
