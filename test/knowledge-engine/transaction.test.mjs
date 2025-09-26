/**
 * @fileoverview Unit tests for knowledge-engine/transaction.mjs
 * Tests transaction management with hooks and receipts
 */

import { describe, it, expect, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";
import { TransactionManager } from "../../src/knowledge-engine/transaction.mjs";

const { namedNode, literal, quad } = DataFactory;

describe("transaction.mjs", () => {
  let testStore;
  let tx;

  beforeEach(() => {
    testStore = new Store();
    // Add initial test data
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/name"),
      literal("Alice")
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/age"),
      literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
    );

    tx = new TransactionManager();
  });

  describe("TransactionManager", () => {
    it("should create a transaction manager", () => {
      expect(tx).toBeInstanceOf(TransactionManager);
      expect(typeof tx.addHook).toBe("function");
      expect(typeof tx.removeHook).toBe("function");
      expect(typeof tx.apply).toBe("function");
      expect(typeof tx.getStats).toBe("function");
    });

    it("should create with options", () => {
      const txWithOptions = new TransactionManager({
        strictMode: true,
        maxHooks: 50
      });
      
      expect(txWithOptions).toBeInstanceOf(TransactionManager);
    });
  });

  describe("addHook", () => {
    it("should add a pre-hook", () => {
      const hook = {
        id: "test-pre-hook",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };

      tx.addHook(hook);
      
      const stats = tx.getStats();
      expect(stats.totalHooks).toBe(1);
      expect(stats.preHooks).toBe(1);
      expect(stats.postHooks).toBe(0);
    });

    it("should add a post-hook", () => {
      const hook = {
        id: "test-post-hook",
        mode: "post",
        condition: async (store, delta) => true,
        effect: async (store, delta) => {
          console.log("Post-hook executed");
        }
      };

      tx.addHook(hook);
      
      const stats = tx.getStats();
      expect(stats.totalHooks).toBe(1);
      expect(stats.preHooks).toBe(0);
      expect(stats.postHooks).toBe(1);
    });

    it("should add multiple hooks", () => {
      const hook1 = {
        id: "hook1",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };
      const hook2 = {
        id: "hook2",
        mode: "post",
        condition: async (store, delta) => true,
        effect: async (store, delta) => {}
      };

      tx.addHook(hook1);
      tx.addHook(hook2);
      
      const stats = tx.getStats();
      expect(stats.totalHooks).toBe(2);
      expect(stats.preHooks).toBe(1);
      expect(stats.postHooks).toBe(1);
    });

    it("should throw error for invalid hook", () => {
      expect(() => {
        tx.addHook(null);
      }).toThrow("addHook: hook must be an object");

      expect(() => {
        tx.addHook({});
      }).toThrow("addHook: hook must have id, mode, condition, and effect");
    });

    it("should throw error for duplicate hook id", () => {
      const hook = {
        id: "duplicate-hook",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };

      tx.addHook(hook);
      
      expect(() => {
        tx.addHook(hook);
      }).toThrow("addHook: hook with id 'duplicate-hook' already exists");
    });

    it("should enforce max hooks limit", () => {
      const txWithLimit = new TransactionManager({ maxHooks: 2 });
      
      const hook1 = {
        id: "hook1",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };
      const hook2 = {
        id: "hook2",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };
      const hook3 = {
        id: "hook3",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };

      txWithLimit.addHook(hook1);
      txWithLimit.addHook(hook2);
      
      expect(() => {
        txWithLimit.addHook(hook3);
      }).toThrow("addHook: maximum number of hooks (2) exceeded");
    });
  });

  describe("removeHook", () => {
    it("should remove a hook", () => {
      const hook = {
        id: "removable-hook",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };

      tx.addHook(hook);
      expect(tx.getStats().totalHooks).toBe(1);

      tx.removeHook("removable-hook");
      expect(tx.getStats().totalHooks).toBe(0);
    });

    it("should handle removing non-existent hook", () => {
      expect(() => {
        tx.removeHook("non-existent-hook");
      }).toThrow("removeHook: hook with id 'non-existent-hook' not found");
    });

    it("should throw error for invalid hook id", () => {
      expect(() => {
        tx.removeHook(null);
      }).toThrow("removeHook: hookId must be a string");
    });
  });

  describe("apply", () => {
    it("should apply a simple transaction", async () => {
      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result).toHaveProperty("store");
      expect(result).toHaveProperty("receipt");
      expect(result.receipt.committed).toBe(true);
      expect(result.receipt.delta).toEqual(delta);
      expect(result.store.size).toBe(testStore.size + 1);
    });

    it("should apply transaction with removals", async () => {
      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/age"),
            literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
          )
        ]
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(result.store.size).toBe(testStore.size); // +1 addition, -1 removal
    });

    it("should veto transaction with pre-hook", async () => {
      const hook = {
        id: "veto-hook",
        mode: "pre",
        condition: async (store, delta) => {
          // Veto if adding email
          return !delta.additions.some(q => 
            q.predicate.value.includes("email")
          );
        },
        effect: "veto"
      };

      tx.addHook(hook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(false);
      expect(result.store.size).toBe(testStore.size); // No changes
      expect(result.receipt.hookResults).toHaveLength(1);
      expect(result.receipt.hookResults[0].result).toBe(false);
    });

    it("should execute post-hook on successful transaction", async () => {
      let postHookExecuted = false;
      
      const hook = {
        id: "post-hook",
        mode: "post",
        condition: async (store, delta) => true,
        effect: async (store, delta) => {
          postHookExecuted = true;
        }
      };

      tx.addHook(hook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(postHookExecuted).toBe(true);
      expect(result.receipt.hookResults).toHaveLength(1);
      expect(result.receipt.hookResults[0].result).toBe(true);
    });

    it("should not execute post-hook on vetoed transaction", async () => {
      let postHookExecuted = false;
      
      const preHook = {
        id: "pre-hook",
        mode: "pre",
        condition: async (store, delta) => false, // Always veto
        effect: "veto"
      };
      
      const postHook = {
        id: "post-hook",
        mode: "post",
        condition: async (store, delta) => true,
        effect: async (store, delta) => {
          postHookExecuted = true;
        }
      };

      tx.addHook(preHook);
      tx.addHook(postHook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(false);
      expect(postHookExecuted).toBe(false);
      expect(result.receipt.hookResults).toHaveLength(1); // Only pre-hook executed
    });

    it("should handle multiple hooks", async () => {
      const hook1 = {
        id: "hook1",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };
      
      const hook2 = {
        id: "hook2",
        mode: "pre",
        condition: async (store, delta) => true,
        effect: "veto"
      };
      
      const hook3 = {
        id: "hook3",
        mode: "post",
        condition: async (store, delta) => true,
        effect: async (store, delta) => {}
      };

      tx.addHook(hook1);
      tx.addHook(hook2);
      tx.addHook(hook3);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(result.receipt.hookResults).toHaveLength(3);
    });

    it("should handle hook errors gracefully", async () => {
      const hook = {
        id: "error-hook",
        mode: "pre",
        condition: async (store, delta) => {
          throw new Error("Hook condition error");
        },
        effect: "veto"
      };

      tx.addHook(hook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(false);
      expect(result.receipt.hookResults).toHaveLength(1);
      expect(result.receipt.hookResults[0].error).toContain("Hook condition error");
    });

    it("should generate proper receipt", async () => {
      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      const receipt = result.receipt;
      
      expect(receipt).toHaveProperty("delta");
      expect(receipt).toHaveProperty("committed");
      expect(receipt).toHaveProperty("hookResults");
      expect(receipt).toHaveProperty("beforeHash");
      expect(receipt).toHaveProperty("afterHash");
      expect(receipt).toHaveProperty("timestamp");
      
      expect(receipt.beforeHash).toHaveProperty("sha3");
      expect(receipt.beforeHash).toHaveProperty("blake3");
      expect(receipt.afterHash).toHaveProperty("sha3");
      expect(receipt.afterHash).toHaveProperty("blake3");
      
      expect(receipt.beforeHash.sha3).not.toBe(receipt.afterHash.sha3);
      expect(receipt.beforeHash.blake3).not.toBe(receipt.afterHash.blake3);
    });

    it("should throw error for invalid store", async () => {
      const delta = { additions: [], removals: [] };
      
      await expect(tx.apply(null, delta)).rejects.toThrow("apply: store must be a valid Store instance");
      await expect(tx.apply("invalid", delta)).rejects.toThrow("apply: store must be a valid Store instance");
    });

    it("should throw error for invalid delta", async () => {
      await expect(tx.apply(testStore, null)).rejects.toThrow("apply: delta must be an object");
      await expect(tx.apply(testStore, "invalid")).rejects.toThrow("apply: delta must be an object");
    });

    it("should throw error for invalid delta structure", async () => {
      await expect(tx.apply(testStore, {})).rejects.toThrow("apply: delta must have additions and removals arrays");
      await expect(tx.apply(testStore, { additions: [] })).rejects.toThrow("apply: delta must have additions and removals arrays");
    });
  });

  describe("getStats", () => {
    it("should return transaction statistics", () => {
      const stats = tx.getStats();
      
      expect(stats).toHaveProperty("totalHooks");
      expect(stats).toHaveProperty("preHooks");
      expect(stats).toHaveProperty("postHooks");
      expect(stats).toHaveProperty("totalTransactions");
      expect(stats).toHaveProperty("committedTransactions");
      expect(stats).toHaveProperty("vetoedTransactions");
      
      expect(typeof stats.totalHooks).toBe("number");
      expect(typeof stats.preHooks).toBe("number");
      expect(typeof stats.postHooks).toBe("number");
      expect(typeof stats.totalTransactions).toBe("number");
      expect(typeof stats.committedTransactions).toBe("number");
      expect(typeof stats.vetoedTransactions).toBe("number");
    });

    it("should update statistics after transactions", async () => {
      const initialStats = tx.getStats();
      expect(initialStats.totalTransactions).toBe(0);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      await tx.apply(testStore, delta);
      
      const updatedStats = tx.getStats();
      expect(updatedStats.totalTransactions).toBe(1);
      expect(updatedStats.committedTransactions).toBe(1);
      expect(updatedStats.vetoedTransactions).toBe(0);
    });

    it("should update statistics for vetoed transactions", async () => {
      const hook = {
        id: "veto-hook",
        mode: "pre",
        condition: async (store, delta) => false, // Always veto
        effect: "veto"
      };

      tx.addHook(hook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      await tx.apply(testStore, delta);
      
      const stats = tx.getStats();
      expect(stats.totalTransactions).toBe(1);
      expect(stats.committedTransactions).toBe(0);
      expect(stats.vetoedTransactions).toBe(1);
    });
  });

  describe("edge cases", () => {
    it("should handle empty delta", async () => {
      const delta = { additions: [], removals: [] };
      
      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(result.store.size).toBe(testStore.size);
    });

    it("should handle large transactions", async () => {
      const additions = [];
      for (let i = 0; i < 1000; i++) {
        additions.push(
          quad(
            namedNode(`http://example.org/resource${i}`),
            namedNode("http://example.org/hasValue"),
            literal(`value${i}`)
          )
        );
      }

      const delta = { additions, removals: [] };
      
      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(result.store.size).toBe(testStore.size + 1000);
    });

    it("should handle concurrent transactions", async () => {
      const delta1 = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const delta2 = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/phone"),
            literal("555-1234")
          )
        ],
        removals: []
      };

      const promises = [
        tx.apply(testStore, delta1),
        tx.apply(testStore, delta2)
      ];

      const results = await Promise.all(promises);
      
      expect(results).toHaveLength(2);
      results.forEach(result => {
        expect(result.receipt.committed).toBe(true);
      });
    });

    it("should handle hooks with async conditions", async () => {
      const hook = {
        id: "async-hook",
        mode: "pre",
        condition: async (store, delta) => {
          // Simulate async operation
          await new Promise(resolve => setTimeout(resolve, 10));
          return true;
        },
        effect: "veto"
      };

      tx.addHook(hook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(result.receipt.hookResults).toHaveLength(1);
      expect(result.receipt.hookResults[0].result).toBe(true);
    });

    it("should handle hooks with complex conditions", async () => {
      const hook = {
        id: "complex-hook",
        mode: "pre",
        condition: async (store, delta) => {
          // Check if store has specific data
          const hasAlice = store.getQuads().some(q => 
            q.subject.value.includes("alice")
          );
          
          // Check if delta adds specific data
          const addsEmail = delta.additions.some(q => 
            q.predicate.value.includes("email")
          );
          
          return hasAlice && addsEmail;
        },
        effect: "veto"
      };

      tx.addHook(hook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(result.receipt.hookResults).toHaveLength(1);
      expect(result.receipt.hookResults[0].result).toBe(true);
    });

    it("should handle post-hooks with errors", async () => {
      const hook = {
        id: "error-post-hook",
        mode: "post",
        condition: async (store, delta) => true,
        effect: async (store, delta) => {
          throw new Error("Post-hook error");
        }
      };

      tx.addHook(hook);

      const delta = {
        additions: [
          quad(
            namedNode("http://example.org/alice"),
            namedNode("http://example.org/email"),
            literal("alice@example.org")
          )
        ],
        removals: []
      };

      const result = await tx.apply(testStore, delta);
      
      expect(result.receipt.committed).toBe(true); // Transaction still commits
      expect(result.receipt.hookResults).toHaveLength(1);
      expect(result.receipt.hookResults[0].error).toContain("Post-hook error");
    });

    it("should handle strict mode", () => {
      const strictTx = new TransactionManager({ strictMode: true });
      
      expect(strictTx).toBeInstanceOf(TransactionManager);
      // Strict mode behavior would be tested based on implementation details
    });
  });
});
