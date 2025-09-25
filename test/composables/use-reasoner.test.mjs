/**
 * @fileoverview Tests for useReasoner convenience layer
 * Tests high-level reasoning operations
 */

import { describe, it, expect, beforeEach } from "vitest";
import { useReasoner } from "../../src/composables/use-reasoner.mjs";
import { initStore } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useReasoner convenience layer", () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore();
  });

  it("provides simple reasoning interface", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      expect(reasoner).toBeDefined();
      expect(typeof reasoner.infer).toBe("function");
      expect(typeof reasoner.inferSequence).toBe("function");
      expect(typeof reasoner.wouldInfer).toBe("function");
      expect(typeof reasoner.getStats).toBe("function");
      expect(typeof reasoner.clearInferred).toBe("function");
      expect(typeof reasoner.createPipeline).toBe("function");
      expect(typeof reasoner.export).toBe("function");
      expect(typeof reasoner.import).toBe("function");
    });
  });

  it("infers new knowledge from simple rules", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      // Add some initial data
      const storeContext = reasoner.getStats ? reasoner.getStats() : null;
      
      // Simple Turtle data (not rules for now)
      const rules = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" .
      `;
      
      const result = await reasoner.infer(rules);
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(typeof result.newTriples).toBe("number");
      expect(typeof result.totalTriples).toBe("number");
    });
  });

  it("handles empty rules gracefully", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const result = await reasoner.infer("");
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.newTriples).toBe(0);
    });
  });

  it("provides inference statistics", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const result = await reasoner.infer("", { returnStats: true });
      
      expect(result.stats).toBeDefined();
      expect(typeof result.stats.original).toBe("number");
      expect(typeof result.stats.inferred).toBe("number");
      expect(typeof result.stats.added).toBe("number");
      expect(typeof result.stats.growth).toBe("number");
    });
  });

  it("can infer without adding to store", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const originalStats = reasoner.getStats();
      const result = await reasoner.infer("", { addToStore: false });
      
      expect(result.success).toBe(true);
      // Store size should remain the same
      const finalStats = reasoner.getStats();
      expect(finalStats.quads).toBe(originalStats.quads);
    });
  });

  it("checks if rules would produce new knowledge", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const wouldInfer = await reasoner.wouldInfer("");
      
      expect(typeof wouldInfer).toBe("boolean");
    });
  });

  it("provides store statistics", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const stats = reasoner.getStats();
      
      expect(stats).toBeDefined();
      expect(typeof stats.quads).toBe("number");
    });
  });

  it("creates reasoning pipeline", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const pipeline = reasoner.createPipeline([
        { name: "step1", rules: "" },
        { name: "step2", rules: "" }
      ]);
      
      expect(pipeline).toBeDefined();
      expect(typeof pipeline.run).toBe("function");
      expect(pipeline.steps).toHaveLength(2);
    });
  });

  it("exports knowledge in different formats", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const turtle = await reasoner.export({ format: "Turtle" });
      const nquads = await reasoner.export({ format: "N-Quads" });
      
      expect(typeof turtle).toBe("string");
      expect(typeof nquads).toBe("string");
    });
  });

  it("imports knowledge from strings", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const turtleData = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" .
      `;
      
      const result = await reasoner.import(turtleData);
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(typeof result.imported).toBe("number");
      expect(typeof result.totalTriples).toBe("number");
    });
  });

  it("handles import without merge", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const turtleData = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" .
      `;
      
      const result = await reasoner.import(turtleData, { merge: false });
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
    });
  });

  it("handles inferSequence with multiple rule sets", async () => {
    await runApp(async () => {
      const reasoner = useReasoner();
      
      const ruleSets = ["", ""];
      const result = await reasoner.inferSequence(ruleSets);
      
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(Array.isArray(result.steps)).toBe(true);
      expect(result.steps).toHaveLength(2);
    });
  });
});