/**
 * @fileoverview Tests for useDelta convenience layer
 * Tests graph diff and patch operations
 */

import { describe, it, expect, beforeEach } from "vitest";
import { useDelta } from "../../src/composables/use-delta.mjs";
import { initStore, useStoreContext } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useDelta convenience layer", () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore();
  });

  it("provides delta operations interface", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      expect(delta).toBeDefined();
      expect(typeof delta.compareWith).toBe("function");
      expect(typeof delta.syncWith).toBe("function");
      expect(typeof delta.apply).toBe("function");
      expect(typeof delta.getStats).toBe("function");
      expect(typeof delta.isEmpty).toBe("function");
      expect(typeof delta.merge).toBe("function");
      expect(typeof delta.invert).toBe("function");
      expect(typeof delta.createPatch).toBe("function");
      expect(typeof delta.applyPatch).toBe("function");
    });
  });

  it("compares context store with new data", async () => {
    await runApp(async () => {
      const delta = useDelta();
      const storeContext = useStoreContext();
      
      // Add initial data to context store
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      // Create new data store with additional data
      const newStore = new Store();
      newStore.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      newStore.addQuad(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      
      const result = delta.compareWith(newStore);
      
      expect(result).toBeDefined();
      expect(result.addedCount).toBe(1);
      expect(result.removedCount).toBe(0);
      expect(result.unchangedCount).toBe(1);
      expect(result.contextSize).toBe(1);
      expect(result.newDataSize).toBe(2);
    });
  });

  it("syncs context store with new data", async () => {
    await runApp(async () => {
      const delta = useDelta();
      const storeContext = useStoreContext();
      
      // Add initial data to context store
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      const originalSize = storeContext.store.size;
      
      // Create new data store with additional data
      const newStore = new Store();
      newStore.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      newStore.addQuad(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      
      const result = await delta.syncWith(newStore);
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.added).toBe(1);
      expect(result.removed).toBe(0);
      expect(result.originalSize).toBe(originalSize);
      expect(result.finalSize).toBe(originalSize + 1);
      expect(result.changes).toBeDefined();
      expect(result.changes.addedCount).toBe(1);
    });
  });

  it("applies changes to current store", async () => {
    await runApp(async () => {
      const delta = useDelta();
      const storeContext = useStoreContext();
      
      // Add initial data
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      const originalSize = storeContext.store.size;
      
      // Create changes
      const added = new Store();
      added.addQuad(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      
      const removed = new Store();
      removed.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      const result = await delta.apply({ added, removed });
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.added).toBe(1);
      expect(result.removed).toBe(1);
      expect(result.originalSize).toBe(originalSize);
      expect(result.finalSize).toBe(originalSize); // Net change is 0
    });
  });

  it("supports dry run mode", async () => {
    await runApp(async () => {
      const delta = useDelta();
      const storeContext = useStoreContext();
      
      const originalSize = storeContext.store.size;
      
      // Create changes
      const added = new Store();
      added.addQuad(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      
      const removed = new Store();
      
      const result = await delta.apply({ added, removed }, { dryRun: true });
      
      expect(result).toBeDefined();
      expect(result.dryRun).toBe(true);
      expect(result.finalSize).toBe(originalSize); // No actual changes
    });
  });

  it("provides detailed statistics", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      const added = new Store();
      added.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      added.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/age"),
        literal("30")
      ));
      
      const removed = new Store();
      removed.addQuad(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      
      const stats = delta.getStats({ added, removed });
      
      expect(stats).toBeDefined();
      expect(stats.added.quads).toBe(2);
      expect(stats.added.subjects).toBe(1);
      expect(stats.added.predicates).toBe(2);
      expect(stats.removed.quads).toBe(1);
      expect(stats.total.netChange).toBe(1);
      expect(stats.coverage.addedSubjects).toContain("http://example.org/alice");
    });
  });

  it("detects empty changes", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      const emptyChanges = {
        added: new Store(),
        removed: new Store()
      };
      
      const nonEmptyChanges = {
        added: new Store(),
        removed: new Store()
      };
      nonEmptyChanges.added.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      expect(delta.isEmpty(emptyChanges)).toBe(true);
      expect(delta.isEmpty(nonEmptyChanges)).toBe(false);
      
      // Test with null/undefined - should throw JavaScript error
      expect(() => delta.isEmpty(null)).toThrow();
      expect(() => delta.isEmpty({})).toThrow();
    });
  });

  it("merges multiple change sets", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      const changes1 = {
        added: new Store(),
        removed: new Store()
      };
      changes1.added.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      const changes2 = {
        added: new Store(),
        removed: new Store()
      };
      changes2.added.addQuad(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      
      const merged = delta.merge(changes1, changes2);
      
      expect(merged).toBeDefined();
      expect(merged.added.size).toBe(2);
      expect(merged.removed.size).toBe(0);
    });
  });

  it("inverts changes", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      const added = new Store();
      added.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      const removed = new Store();
      removed.addQuad(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      
      const inverted = delta.invert({ added, removed });
      
      expect(inverted).toBeDefined();
      expect(inverted.added.size).toBe(1);
      expect(inverted.removed.size).toBe(1);
      expect(inverted.added.has(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ))).toBe(true);
    });
  });

  it("creates patches in different formats", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      const added = new Store();
      added.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      const removed = new Store();
      
      const turtlePatch = await delta.createPatch({ added, removed }, { format: "Turtle" });
      const nquadsPatch = await delta.createPatch({ added, removed }, { format: "N-Quads" });
      
      expect(turtlePatch).toBeDefined();
      expect(turtlePatch.addedCount).toBe(1);
      expect(turtlePatch.removedCount).toBe(0);
      expect(turtlePatch.format).toBe("Turtle");
      expect(typeof turtlePatch.added).toBe("string");
      
      expect(nquadsPatch).toBeDefined();
      expect(nquadsPatch.format).toBe("N-Quads");
      expect(typeof nquadsPatch.added).toBe("string");
    });
  });

  it("applies patches", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      const added = new Store();
      added.addQuad(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      const removed = new Store();
      
      const patch = await delta.createPatch({ added, removed });
      const result = await delta.applyPatch(patch);
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.added).toBe(1);
      expect(result.removed).toBe(0);
    });
  });

  it("handles error cases gracefully", async () => {
    await runApp(async () => {
      const delta = useDelta();
      
      // Test invalid changes - these will throw JavaScript errors, not custom errors
      await expect(delta.apply(null)).rejects.toThrow();
      await expect(delta.apply({})).rejects.toThrow();
      
      // Test invalid patch - these will throw JavaScript errors, not custom errors
      await expect(delta.applyPatch(null)).rejects.toThrow();
      await expect(delta.applyPatch({})).rejects.toThrow();
      
      // Test invalid stats - these will throw JavaScript errors, not custom errors
      expect(() => delta.getStats(null)).toThrow();
      expect(() => delta.getStats({})).toThrow();
      
      // Test invalid invert - these will throw JavaScript errors, not custom errors
      expect(() => delta.invert(null)).toThrow();
    });
  });

  it("supports deterministic sorting", async () => {
    await runApp(async () => {
      const delta = useDelta({ deterministic: true });
      const storeContext = useStoreContext();
      
      // Add data to context store in non-deterministic order
      storeContext.add(quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/name"),
        literal("Bob")
      ));
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      ));
      
      // Compare with empty store (should show all as removed)
      const emptyStore = new Store();
      const result = delta.compareWith(emptyStore);
      
      expect(result).toBeDefined();
      expect(result.removed.size).toBe(2);
      // Should be sorted deterministically
      const quads = [...result.removed];
      expect(quads[0].subject.value).toBe("http://example.org/alice");
      expect(quads[1].subject.value).toBe("http://example.org/bob");
    });
  });
});
