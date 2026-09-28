#!/usr/bin/env node

/**
 * @file Working example - Composable Hooks Marketplace
 * @module examples/hooks-marketplace
 * @description
 * Demonstrates the Composable Hooks Marketplace with:
 * 1. Hook normalization to RDF via SPARQL CONSTRUCT
 * 2. Dependency resolution and circular cycle detection via N3
 * 3. SHACL validation in soft-fail (annotate) mode with RDF audit trail
 * 4. Marketplace queries over admitted hooks
 *
 * Run: node examples/hooks-marketplace.mjs
 */

import { HooksMarketplace, HOOK_NS } from '../src/lib/admit-hook.mjs';

/**
 * Helper: Pretty print JSON with 2-space indent
 */
function pretty(obj) {
  return JSON.stringify(obj, null, 2);
}

/**
 * Helper: Create hook definition
 */
function createHook(id, name, version, deps = []) {
  return {
    id,
    name,
    version,
    description: `The ${name} hook performs important transformations`,
    conditions: [
      {
        kind: 'sparql-ask',
        query: `ASK { ?s a <${HOOK_NS.hook}${id}> }`,
      },
    ],
    effects: [
      {
        kind: 'sparql-construct',
        query: `CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }`,
      },
    ],
    dependsOn: deps,
    priority: Math.floor(Math.random() * 100),
  };
}

async function main() {
  console.log('╔════════════════════════════════════════════════════════╗');
  console.log('║  O* Innovation 5: Composable Hooks Marketplace         ║');
  console.log('║  RDF-based hook composition with SPARQL + N3 + SHACL  ║');
  console.log('╚════════════════════════════════════════════════════════╝\n');

  // Initialize marketplace
  const marketplace = new HooksMarketplace();
  console.log('✓ Marketplace initialized\n');

  // ═══════════════════════════════════════════════════════════════════
  // DEMO 1: Single Hook Normalization
  // ═══════════════════════════════════════════════════════════════════
  console.log('┌─ DEMO 1: Hook Normalization to RDF ─────────────────┐');

  const hookId1 = '550e8400-e29b-41d4-a716-446655440001';
  const singleHook = createHook(
    hookId1,
    'Data Enrichment Hook',
    '1.0.0',
    [], // No dependencies
  );

  console.log('Input hook definition:');
  console.log(pretty(singleHook));
  console.log('\nNormalizing to RDF...');

  const normalized = marketplace.normalizeHookToRDF(singleHook);
  console.log(`\nGenerated hook URI: ${normalized.hookUri}`);
  console.log(`Total RDF triples: ${normalized.triples.length}`);
  console.log(`Conditions: ${normalized.conditions.length}`);
  console.log(`Effects: ${normalized.effects.length}`);

  // Sample triples
  console.log('\nSample RDF triples (first 5):');
  normalized.triples.slice(0, 5).forEach((t, idx) => {
    console.log(`  ${idx + 1}. ${t.subject.value}`);
    console.log(`     --[${t.predicate.value.split('/').pop()}]-->`);
    console.log(`     ${t.object.value || t.object.datatype}`);
  });

  console.log('└───────────────────────────────────────────────────────┘\n');

  // ═══════════════════════════════════════════════════════════════════
  // DEMO 2: Hook Admission with SHACL Soft-Fail
  // ═══════════════════════════════════════════════════════════════════
  console.log('┌─ DEMO 2: Hook Admission with SHACL Soft-Fail ────────┐');

  const admissionResult = marketplace.admitHook(singleHook);
  console.log(`Hook admission result:`);
  console.log(`  Admitted: ${admissionResult.admitted}`);
  console.log(`  Hook URI: ${admissionResult.hookUri}`);
  console.log(`  Violations: ${admissionResult.violationCount}`);

  if (admissionResult.violationCount > 0) {
    console.log(`  Violation details:`);
    admissionResult.violations.forEach(v => {
      console.log(`    - ${v.message}`);
    });
  }

  console.log('└───────────────────────────────────────────────────────┘\n');

  // ═══════════════════════════════════════════════════════════════════
  // DEMO 3: Valid Dependency Chain
  // ═══════════════════════════════════════════════════════════════════
  console.log('┌─ DEMO 3: Valid Dependency Chain A→B→C ────────────────┐');

  const hookId2 = '550e8400-e29b-41d4-a716-446655440002';
  const hookId3 = '550e8400-e29b-41d4-a716-446655440003';

  const hookA = createHook(hookId1, 'Hook A (Base)', '1.0.0', []);
  const hookB = createHook(hookId2, 'Hook B (depends on A)', '1.0.0', [hookId1]);
  const hookC = createHook(hookId3, 'Hook C (depends on B)', '1.0.0', [hookId2]);

  console.log('Admitting hooks A→B→C (valid chain)...\n');

  const chainResult = marketplace.admitHooksWithDependencies([hookA, hookB, hookC]);

  console.log(`Results:`);
  console.log(`  Admitted: ${chainResult.admittedCount}`);
  console.log(`  Rejected: ${chainResult.rejectedCount}`);
  console.log(`  Cycles detected: ${chainResult.hadCycles}`);

  console.log(`\nDependency graph:`);
  console.log(pretty(chainResult.dependencyGraph).split('\n').slice(0, 15).join('\n'));

  console.log('└───────────────────────────────────────────────────────┘\n');

  // ═══════════════════════════════════════════════════════════════════
  // DEMO 4: Circular Dependency Detection
  // ═══════════════════════════════════════════════════════════════════
  console.log('┌─ DEMO 4: Circular Dependency Detection (A↔B) ────────┐');

  const hookId4 = '550e8400-e29b-41d4-a716-446655440004';
  const hookId5 = '550e8400-e29b-41d4-a716-446655440005';

  const hookX = createHook(hookId4, 'Hook X', '1.0.0', [hookId5]);
  const hookY = createHook(hookId5, 'Hook Y', '1.0.0', [hookId4]);

  console.log('Admitting hooks X↔Y (circular dependency)...\n');

  const cycleResult = marketplace.admitHooksWithDependencies([hookX, hookY]);

  console.log(`Results:`);
  console.log(`  Admitted: ${cycleResult.admittedCount}`);
  console.log(`  Rejected: ${cycleResult.rejectedCount}`);
  console.log(`  Cycles detected: ${cycleResult.hadCycles}`);

  if (cycleResult.cycles.length > 0) {
    console.log(`  Hooks involved in cycles:`);
    cycleResult.cycles.slice(0, 3).forEach(cycle => {
      console.log(`    - ${cycle}`);
    });
  }

  console.log('└───────────────────────────────────────────────────────┘\n');

  // ═══════════════════════════════════════════════════════════════════
  // DEMO 5: Marketplace Queries
  // ═══════════════════════════════════════════════════════════════════
  console.log('┌─ DEMO 5: RDF Marketplace Queries ─────────────────────┐');

  // Re-create marketplace with valid hooks
  const queryMarketplace = new HooksMarketplace();

  const demoHooks = [
    createHook('550e8400-e29b-41d4-a716-446655440010', 'Hook Alpha', '1.0.0', []),
    createHook('550e8400-e29b-41d4-a716-446655440011', 'Hook Beta', '2.0.0', []),
    createHook('550e8400-e29b-41d4-a716-446655440012', 'Hook Gamma', '1.5.0', []),
  ];

  demoHooks.forEach(h => queryMarketplace.admitHook(h));

  console.log('Admitted hooks to marketplace:');
  const admitted = queryMarketplace.getAdmittedHooks();
  admitted.forEach((h, idx) => {
    console.log(`  ${idx + 1}. ${h.name} (v${h.version}) - priority: ${h.priority}`);
  });

  // Query: Count all hooks
  const countQuery = `
    PREFIX hook: <http://ostar.org/hook/>
    SELECT (COUNT(?hook) AS ?count) WHERE {
      ?hook a hook:Hook .
    }
  `;

  try {
    const countResults = queryMarketplace.query(countQuery);
    console.log(`\nSPARQL query result (COUNT all hooks):`);
    console.log(`  Total hooks: ${countResults.length > 0 ? countResults[0].count : 0}`);
  } catch (err) {
    console.log(`Query note: ${err.message}`);
  }

  console.log('└───────────────────────────────────────────────────────┘\n');

  // ═══════════════════════════════════════════════════════════════════
  // DEMO 6: Soft-Fail Admission with SHACL Violations
  // ═══════════════════════════════════════════════════════════════════
  console.log('┌─ DEMO 6: Soft-Fail Admission (SHACL Annotate) ────────┐');

  const softFailMarketplace = new HooksMarketplace();

  // All hooks that pass Zod schema validation are admitted (soft-fail)
  // This demonstrates the separation of concerns:
  // - Zod: syntactic validation (hard-fail)
  // - SHACL: semantic validation (soft-fail with audit trail RDF)
  const validHook = createHook(
    '550e8400-e29b-41d4-a716-446655440020',
    'Analytics Hook',
    '1.0.0',
    [],
  );

  const softFailResult = softFailMarketplace.admitHook(validHook);

  console.log('Admitting valid hook (all constraints satisfied)...\n');
  console.log(`Admission result:`);
  console.log(`  Admitted: ${softFailResult.admitted} (soft-fail mode)`);
  console.log(`  Violations recorded: ${softFailResult.violationCount}`);
  console.log(`  Hook URI: ${softFailResult.hookUri}`);

  console.log(`\nDesign Notes:`);
  console.log(`  • Zod schema validation: Hard-fail (syntactic)`);
  console.log(`  • SHACL validation: Soft-fail (semantic, RDF audit trail)`);
  console.log(`  • Violated hooks recorded as RDF triples for compliance audits`);
  console.log(`  • Hooks always admitted if they pass syntax validation`);

  console.log('└───────────────────────────────────────────────────────┘\n');

  // ═══════════════════════════════════════════════════════════════════
  // Summary
  // ═══════════════════════════════════════════════════════════════════
  console.log('╔════════════════════════════════════════════════════════╗');
  console.log('║  Summary                                               ║');
  console.log('╠════════════════════════════════════════════════════════╣');
  console.log('║ ✓ Hook normalization to RDF (SPARQL CONSTRUCT)       ║');
  console.log('║ ✓ Dependency composition (N3 forward-chaining)        ║');
  console.log('║ ✓ Circular cycle detection                            ║');
  console.log('║ ✓ SHACL validation (annotate/soft-fail mode)         ║');
  console.log('║ ✓ RDF audit trail for violations                      ║');
  console.log('║ ✓ Marketplace queries via SPARQL                      ║');
  console.log('╚════════════════════════════════════════════════════════╝\n');
}

main().catch(err => {
  console.error('Error:', err.message);
  process.exit(1);
});
