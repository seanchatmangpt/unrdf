/**
 * @file ken-parliament-debug.mjs
 * @description Debugging version of Parliamentary Swarm Demo with Robert's Rules
 */

import { DataFactory, Store } from 'n3';
import { TransactionManager, printReceipt } from './knowledge-engine.mjs';

const { namedNode, literal, quad } = DataFactory;

async function main() {
  console.log("🏛️ Starting Parliamentary Swarm Demo (Debug Mode)...\n");

  const store = new Store();
  const tx = new TransactionManager();
  const ex = "http://example.org/";

  // === Hooks ===
  tx.addHook({
    id: "motion-must-be-seconded",
    mode: "pre",
    condition: async (store, delta) => {
      const adoptions = delta.additions.filter(q =>
        q.predicate.value === `${ex}adoptMotion`
      );
      for (const adoption of adoptions) {
        const motion = adoption.subject;
        const seconds = store.getQuads(motion, namedNode(`${ex}secondedBy`), null, null);
        console.log(`🔎 Hook check: ${motion.value} has ${seconds.length} seconds in store.`);
        if (seconds.length === 0) return false;
      }
      return true;
    },
    effect: "veto"
  });

  tx.addHook({
    id: "vote-before-adoption",
    mode: "pre",
    condition: async (store, delta) => {
      const adoptions = delta.additions.filter(q =>
        q.predicate.value === `${ex}adoptMotion`
      );
      for (const adoption of adoptions) {
        const motion = adoption.subject;
        const votes = store.getQuads(motion, namedNode(`${ex}votedBy`), null, null);
        console.log(`🔎 Hook check: ${motion.value} has ${votes.length} votes in store.`);
        if (votes.length === 0) return false;
      }
      return true;
    },
    effect: "veto"
  });

  tx.addHook({
    id: "audit-log",
    mode: "post",
    condition: async () => true,
    effect: async (_store, delta) => {
      delta.additions.forEach(q => {
        console.log(`🪵 Audit: ${q.subject.value} ${q.predicate.value} ${q.object.value}`);
      });
    }
  });

  // === Helper to apply commits ===
  async function commit(description, additions, actor) {
    console.log(`\n🤖 ${description}`);
    const delta = { additions, removals: [] };
    const { receipt, store: updatedStore } = await tx.apply(store, delta, { actor });

    // Dump motion state after each commit
    const motion1Quads = updatedStore.getQuads(namedNode(`${ex}motion1`), null, null, null);
    console.log("📊 Current motion1 state:");
    motion1Quads.forEach(q =>
      console.log(`   • ${q.subject.value} ${q.predicate.value} ${q.object.value}`)
    );

    // Use the new printReceipt helper
    printReceipt(receipt, { verbose: true });
  }

  // === Scenario ===
  await commit(
    "PlannerAgent proposes: Introduce motion1: fund AI project",
    [quad(namedNode(`${ex}motion1`), namedNode(`${ex}introducedBy`), literal("PlannerAgent"))],
    "PlannerAgent"
  );

  await commit(
    "ResearchAgent proposes: Second motion1",
    [quad(namedNode(`${ex}motion1`), namedNode(`${ex}secondedBy`), literal("ResearchAgent"))],
    "ResearchAgent"
  );

  await commit(
    "CoderAgent proposes: Vote YES on motion1",
    [quad(namedNode(`${ex}motion1`), namedNode(`${ex}votedBy`), literal("CoderAgent"))],
    "CoderAgent"
  );

  await commit(
    "PlannerAgent proposes: Adopt motion1",
    [quad(namedNode(`${ex}motion1`), namedNode(`${ex}adoptMotion`), literal("PlannerAgent"))],
    "PlannerAgent"
  );

  await commit(
    "AdversarialAgent proposes: Adopt motion2 (no process)",
    [quad(namedNode(`${ex}motion2`), namedNode(`${ex}adoptMotion`), literal("AdversarialAgent"))],
    "AdversarialAgent"
  );

  console.log("\n✅ Debug demo complete.");
}

main().catch(err => {
  console.error("❌ Demo failed:", err);
  process.exit(1);
});
