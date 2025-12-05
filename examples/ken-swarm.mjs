/**
 * @file AI Agent Swarm Governance Example
 * @description
 * Demonstrates multiple agents (Research, Planner, Coder) collaborating
 * in a shared RDF knowledge graph, with Knowledge Hooks providing
 * governance, validation, and audit receipts.
 */

import { TransactionManager } from '../packages/knowledge-engine.mjs';

import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
const { namedNode, literal, quad } = DataFactory;

// ---------------------------------------------------------------------------
// Shared Knowledge Store + Transaction Manager
// ---------------------------------------------------------------------------
const store = createStore();
const tx = new TransactionManager();

// ---------------------------------------------------------------------------
// Governance Hooks
// ---------------------------------------------------------------------------

// 1. Prevent conflicting deadlines
tx.addHook({
  id: 'no-deadline-conflicts',
  mode: 'pre',
  condition: async (store, delta) => {
    return !delta.additions.some(
      add =>
        add.predicate.value.endsWith('deadline') &&
        store.getQuads(add.subject, namedNode('http://example.org/deadline'), null).length > 0
    );
  },
  effect: 'veto',
});

// 2. Trigger fact-checking whenever a claim is added
tx.addHook({
  id: 'fact-check',
  mode: 'post',
  condition: async (_store, delta) =>
    delta.additions.some(a => a.predicate.value.endsWith('claims')),
  effect: async (_store, delta) => {
    const claims = delta.additions.filter(a => a.predicate.value.endsWith('claims'));
    console.log('🔎 Fact-check agent triggered for claims:');
    claims.forEach(c => console.log(`   - ${c.object.value}`));
  },
});

// 3. Log all successful commits
tx.addHook({
  id: 'audit-log',
  mode: 'post',
  condition: async () => true,
  effect: async (_store, delta) => {
    console.log('🪵 Audit: Commit additions:');
    delta.additions.forEach(a => {
      console.log(`   ${a.subject.value} ${a.predicate.value} ${a.object.value}`);
    });
  },
});

// ---------------------------------------------------------------------------
// Agent Simulation
// ---------------------------------------------------------------------------

async function agentAction(agent, description, additions) {
  console.log(`\n🤖 ${agent} proposes: ${description}`);

  const delta = { additions, removals: [] };
  const { receipt } = await tx.apply(store, delta);

  if (receipt.committed) {
    console.log(`✅ ${agent}'s proposal accepted.`);
  } else {
    console.log(`🚫 ${agent}'s proposal rejected.`);
  }

  // Show detailed receipt
  console.log('🧾 Receipt:');
  console.log('- Committed:', receipt.committed);
  console.log(
    '- Additions:',
    receipt.delta.additions.map(q => q.object.value)
  );
  console.log('- Hook results:');
  receipt.hookResults.forEach(r => {
    console.log(`   • Hook '${r.hookId}' => ${r.result}`);
  });
}

// ---------------------------------------------------------------------------
// Main Demo
// ---------------------------------------------------------------------------
async function main() {
  console.log('🚀 Starting AI Agent Swarm Governance Demo...\n');

  // ResearchAgent adds a scientific claim
  await agentAction('ResearchAgent', `New claim: "AI improves protein folding accuracy"`, [
    quad(
      namedNode('http://example.org/claim1'),
      namedNode('http://example.org/claims'),
      literal('AI improves protein folding accuracy')
    ),
  ]);

  // PlannerAgent sets a deadline for task1
  await agentAction('PlannerAgent', `Set deadline for task1 = 2025-12-01`, [
    quad(
      namedNode('http://example.org/task1'),
      namedNode('http://example.org/deadline'),
      literal('2025-12-01')
    ),
  ]);

  // CoderAgent tries to set a conflicting deadline (should be vetoed)
  await agentAction('CoderAgent', `Conflicting deadline for task1 = 2025-10-01`, [
    quad(
      namedNode('http://example.org/task1'),
      namedNode('http://example.org/deadline'),
      literal('2025-10-01')
    ),
  ]);

  console.log('\n✅ Swarm governance demo complete.');
}

main().catch(err => {
  console.error('❌ Demo failed:', err.message);
  process.exit(1);
});
