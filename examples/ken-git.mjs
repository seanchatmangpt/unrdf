/**
 * @file Git Governance Example with Knowledge Engine
 * @description
 * Demonstrates how the Knowledge Engine can govern source code changes
 * (e.g., package.json modifications) using declarative rules.
 *
 * Clear console narration is provided so security officers (CISO/CTO)
 * can see what is happening step by step.
 */

import {
  parseTurtle,
  toTurtle,
  query,
  validateShacl,
  reason,
  canonicalize,
  isIsomorphic,
  TransactionManager,
} from "../src/knowledge-engine.mjs";

import { DataFactory, Store } from "n3";
import assert from "assert";
const { namedNode, literal, quad } = DataFactory;

// ---------------------------------------------------------------------------
// Helper: benchmark wrapper
// ---------------------------------------------------------------------------
async function benchmark(label, fn) {
  const start = process.hrtime.bigint();
  const result = await fn();
  const end = process.hrtime.bigint();
  const ms = Number(end - start) / 1_000_000;
  console.log(`⏱️  ${label} completed in ${ms.toFixed(2)} ms\n`);
  return result;
}

// ---------------------------------------------------------------------------
// Governance Example
// ---------------------------------------------------------------------------
async function main() {
  console.log("🚀 Starting Git Governance Example...\n");

  // STEP 1: Initialize empty RDF store representing commits
  const store = new Store();
  console.log("📦 Initialized RDF knowledge store for commits.\n");

  // STEP 2: Define approved library list
  const approvedLibs = ["lodash", "express", "faker"];
  console.log("⚖️  Governance rule: Only approved libraries allowed.");
  console.log("   Approved libraries =", approvedLibs, "\n");

  // STEP 3: Create a TransactionManager with governance hooks
  const tx = new TransactionManager();

  tx.addHook({
    id: "approved-libraries-only",
    mode: "pre",
    condition: async (_store, delta) => {
      const addedLibs = delta.additions
        .filter((q) => q.predicate.value.endsWith("addsLibrary"))
        .map((q) => q.object.value);

      const unapproved = addedLibs.filter((lib) => !approvedLibs.includes(lib));
      if (unapproved.length > 0) {
        console.log("❌ Found unapproved libraries:", unapproved);
        return false; // veto
      }
      console.log("✅ All libraries approved.");
      return true;
    },
    effect: "veto",
  });

  tx.addHook({
    id: "audit-log",
    mode: "post",
    condition: async () => true,
    effect: async (_store, delta) => {
      const libs = delta.additions
        .filter((q) => q.predicate.value.endsWith("addsLibrary"))
        .map((q) => q.object.value);
      if (libs.length > 0) {
        console.log(`🪵 Audit: Commit added libraries: ${libs.join(", ")}`);
      }
    },
  });

  // STEP 4: Mock commits and apply them under governance
  const commits = [
    { id: "commit-1", author: "alice", addsLibrary: "lodash" }, // approved
    { id: "commit-2", author: "bob", addsLibrary: "malware-lib" }, // unapproved
    { id: "commit-3", author: "carol", addsLibrary: "express" }, // approved
    { id: "commit-4", author: "dave", addsLibrary: "crypto-miner-lib" }, // unapproved
  ];

  for (const commit of commits) {
    console.log(
      `📝 Processing ${commit.id}: ${commit.author} adds '${commit.addsLibrary}'`,
    );
    await benchmark("Governance check", async () => {
      const delta = {
        additions: [
          quad(
            namedNode(`http://example.org/${commit.id}`),
            namedNode("http://example.org/author"),
            literal(commit.author),
          ),
          quad(
            namedNode(`http://example.org/${commit.id}`),
            namedNode("http://example.org/addsLibrary"),
            literal(commit.addsLibrary),
          ),
        ],
        removals: [],
      };

      const { store: newStore, receipt } = await tx.apply(store, delta);

      if (receipt.committed) {
        console.log("✅ Commit accepted.");
      } else {
        console.log("🚫 Commit rejected.");
      }

      // Detailed receipt log for audit
      console.log("🧾 Receipt:");
      console.log("- Committed:", receipt.committed);
      console.log(
        "- Additions:",
        receipt.delta.additions.map((q) => q.object.value),
      );
      console.log(
        "- Removals:",
        receipt.delta.removals.map((q) => q.object.value),
      );
      console.log("- Hook results:");
      receipt.hookResults.forEach((r) => {
        console.log(`   • Hook '${r.hookId}' => ${r.result}`);
      });
      console.log();
    });
  }

  console.log("✅ Governance demo complete.");
  console.log(
    "This process could run automatically as part of Git lifecycle hooks (pre-commit or pre-push).",
  );
}

main().catch((err) => {
  console.error("❌ Example failed:", err.message);
  process.exit(1);
});
