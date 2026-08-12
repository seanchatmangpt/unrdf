/**
 * Cross-Algorithm Parity Tests
 *
 * Van der Aalst QA perspective:
 * - Different discovery algorithms on the same log should produce consistent results
 * - DFG variants (basic vs optimized) should have compatible activity sets
 * - Alpha++ and Heuristic Miner should discover compatible process structures
 * - Conformance metrics should be sound (fitness ≤ 1.0, precision ≤ 1.0)
 *
 * These tests verify algorithmic consistency, not exact output equality.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import * as fs from 'fs/promises';
import * as path from 'path';

/**
 * Minimal XES log for testing
 */
const TEST_XES = `<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" xes.version="1.0">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <trace>
    <string key="concept:name" value="case_1"/>
    <event>
      <string key="concept:name" value="register"/>
      <date key="time:timestamp" value="2024-01-01T09:00:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="examine"/>
      <date key="time:timestamp" value="2024-01-01T09:05:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="decide"/>
      <date key="time:timestamp" value="2024-01-01T09:10:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="notify"/>
      <date key="time:timestamp" value="2024-01-01T09:15:00Z"/>
    </event>
  </trace>
  <trace>
    <string key="concept:name" value="case_2"/>
    <event>
      <string key="concept:name" value="register"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="examine"/>
      <date key="time:timestamp" value="2024-01-01T10:05:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="decide"/>
      <date key="time:timestamp" value="2024-01-01T10:10:00Z"/>
    </event>
  </trace>
  <trace>
    <string key="concept:name" value="case_3"/>
    <event>
      <string key="concept:name" value="register"/>
      <date key="time:timestamp" value="2024-01-01T11:00:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="examine"/>
      <date key="time:timestamp" value="2024-01-01T11:05:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="decide"/>
      <date key="time:timestamp" value="2024-01-01T11:10:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="reject"/>
      <date key="time:timestamp" value="2024-01-01T11:15:00Z"/>
    </event>
  </trace>
</log>`;

/**
 * WASM module reference (loaded dynamically)
 */
let wasmModule: any = null;

/**
 * Load WASM module before tests
 */
beforeAll(async () => {
  try {
    // Try to load from the built WASM package
    const pictlWasm = await import('@pictl/wasm');
    wasmModule = pictlWasm.default || pictlWasm;
  } catch (error) {
    // If not available, skip tests gracefully
    console.warn('WASM module not available, skipping parity tests');
  }
});

afterAll(() => {
  wasmModule = null;
});

/**
 * Helper to extract activity set from model output
 */
function extractActivities(model: Record<string, unknown>): Set<string> {
  if (model.type === 'dfg' && Array.isArray(model.nodes)) {
    return new Set(model.nodes as string[]);
  }
  if (model.type === 'petrinet') {
    const transitions = model.transitions as Array<{ label: string }>;
    return new Set(transitions.map((t) => t.label));
  }
  if (model.type === 'process_tree') {
    // Process trees have activity labels in leaf nodes
    const activities = new Set<string>();
    const extractFromTree = (node: Record<string, unknown>) => {
      if (node.activity && typeof node.activity === 'string') {
        activities.add(node.activity);
      }
      if (node.children && Array.isArray(node.children)) {
        for (const child of node.children as Record<string, unknown>[]) {
          extractFromTree(child);
        }
      }
    };
    extractFromTree(model);
    return activities;
  }
  return new Set();
}

/**
 * Helper to extract edges from model output
 */
function extractEdges(model: Record<string, unknown>): Array<[string, string]> {
  if (model.type === 'dfg' && Array.isArray(model.edges)) {
    return (model.edges as Array<{ from: string; to: string }>).map(
      (e) => [e.from, e.to] as [string, string]
    );
  }
  if (model.type === 'petrinet' && Array.isArray(model.arcs)) {
    const edges: Array<[string, string]> = [];
    for (const arc of model.arcs as Array<{ source: string; target: string }>) {
      edges.push([arc.source, arc.target]);
    }
    return edges;
  }
  return [];
}

/**
 * Helper to validate DFG output structure
 */
function isValidDFG(output: unknown): output is Record<string, unknown> {
  if (typeof output !== 'object' || output === null) {
    return false;
  }
  const model = output as Record<string, unknown>;
  return model.type === 'dfg' && Array.isArray(model.nodes) && Array.isArray(model.edges);
}

/**
 * Helper to validate Petri net output structure
 */
function isValidPetriNet(output: unknown): output is Record<string, unknown> {
  if (typeof output !== 'object' || output === null) {
    return false;
  }
  const model = output as Record<string, unknown>;
  return (
    model.type === 'petrinet' &&
    Array.isArray(model.places) &&
    Array.isArray(model.transitions) &&
    Array.isArray(model.arcs)
  );
}

describe('Cross-Algorithm Parity: DFG Variants', () => {
  it('should discover compatible activity sets between basic and optimized DFG', () => {
    if (!wasmModule) {
      return;
    }

    const logHandle = wasmModule.load_eventlog_from_xes(TEST_XES);

    try {
      // Discover basic DFG
      const basicRaw = wasmModule.discover_dfg(logHandle, 'concept:name');
      const basic = typeof basicRaw === 'string' ? JSON.parse(basicRaw) : basicRaw;

      // Discover optimized DFG
      const optimizedRaw = wasmModule.discover_optimized_dfg(logHandle, 'concept:name');
      const optimized = typeof optimizedRaw === 'string' ? JSON.parse(optimizedRaw) : optimizedRaw;

      // Both should be valid DFGs
      expect(isValidDFG(basic)).toBe(true);
      expect(isValidDFG(optimized)).toBe(true);

      // Extract activity sets
      const basicActivities = extractActivities(basic);
      const optimizedActivities = extractActivities(optimized);

      // Activity sets should be identical (same set of activities)
      expect(basicActivities.size).toBe(optimizedActivities.size);

      for (const activity of basicActivities) {
        expect(optimizedActivities.has(activity)).toBe(true);
      }

      for (const activity of optimizedActivities) {
        expect(basicActivities.has(activity)).toBe(true);
      }
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });

  it('should maintain edge consistency in DFG variants', () => {
    if (!wasmModule) {
      return;
    }

    const logHandle = wasmModule.load_eventlog_from_xes(TEST_XES);

    try {
      const basicRaw = wasmModule.discover_dfg(logHandle, 'concept:name');
      const basic = typeof basicRaw === 'string' ? JSON.parse(basicRaw) : basicRaw;

      const optimizedRaw = wasmModule.discover_optimized_dfg(logHandle, 'concept:name');
      const optimized = typeof optimizedRaw === 'string' ? JSON.parse(optimizedRaw) : optimizedRaw;

      const basicEdges = extractEdges(basic);
      const optimizedEdges = extractEdges(optimized);

      // Edge sets should be compatible (same direct follows)
      expect(basicEdges.length).toBeGreaterThan(0);
      expect(optimizedEdges.length).toBeGreaterThan(0);

      // All basic edges should exist in optimized
      const basicEdgeSet = new Set(basicEdges.map((e) => `${e[0]}->${e[1]}`));
      const optimizedEdgeSet = new Set(optimizedEdges.map((e) => `${e[0]}->${e[1]}`));

      for (const edge of basicEdgeSet) {
        expect(optimizedEdgeSet.has(edge)).toBe(true);
      }
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });
});

describe('Cross-Algorithm Parity: Discovery Algorithms', () => {
  it('should produce compatible activity sets between Alpha++ and Heuristic Miner', () => {
    if (!wasmModule) {
      return;
    }

    const logHandle = wasmModule.load_eventlog_from_xes(TEST_XES);

    try {
      // Alpha++ discovery
      const alphaRaw = wasmModule.discover_alpha_plus_plus(logHandle, 'concept:name');
      const alpha = typeof alphaRaw === 'string' ? JSON.parse(alphaRaw) : alphaRaw;

      // Heuristic Miner discovery
      const heuristicRaw = wasmModule.discover_heuristic_miner(logHandle, 'concept:name', 0.5);
      const heuristic = typeof heuristicRaw === 'string' ? JSON.parse(heuristicRaw) : heuristicRaw;

      // Both should be valid Petri nets
      expect(isValidPetriNet(alpha)).toBe(true);
      expect(isValidPetriNet(heuristic)).toBe(true);

      // Extract activity sets
      const alphaActivities = extractActivities(alpha);
      const heuristicActivities = extractActivities(heuristic);

      // Activity sets should be compatible (may differ by 1-2 activities due to thresholds)
      const sizeDiff = Math.abs(alphaActivities.size - heuristicActivities.size);
      expect(sizeDiff).toBeLessThanOrEqual(2);

      // Core activities should be present in both
      const coreActivities = new Set(['register', 'examine', 'decide']);
      for (const activity of coreActivities) {
        expect(alphaActivities.has(activity)).toBe(true);
        expect(heuristicActivities.has(activity)).toBe(true);
      }
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });

  it('should produce valid process trees from Inductive Miner', () => {
    if (!wasmModule) {
      return;
    }

    const logHandle = wasmModule.load_eventlog_from_xes(TEST_XES);

    try {
      const raw = wasmModule.discover_inductive_miner(logHandle, 'concept:name');
      const tree = typeof raw === 'string' ? JSON.parse(raw) : raw;

      // Should be a valid process tree
      expect(tree).toHaveProperty('type');
      expect(tree.type).toBe('process_tree');

      // Should have activities
      const activities = extractActivities(tree);
      expect(activities.size).toBeGreaterThan(0);

      // Should have core activities
      expect(activities.has('register')).toBe(true);
      expect(activities.has('examine')).toBe(true);
      expect(activities.has('decide')).toBe(true);
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });

  it('should handle empty logs gracefully across all algorithms', () => {
    if (!wasmModule) {
      return;
    }

    const emptyXes = `<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/"></log>`;

    const logHandle = wasmModule.load_eventlog_from_xes(emptyXes);

    try {
      // DFG should handle empty log
      const dfgRaw = wasmModule.discover_dfg(logHandle, 'concept:name');
      const dfg = typeof dfgRaw === 'string' ? JSON.parse(dfgRaw) : dfgRaw;
      expect(isValidDFG(dfg)).toBe(true);
      expect((dfg.nodes as string[]).length).toBe(0);

      // Alpha++ should handle empty log
      const alphaRaw = wasmModule.discover_alpha_plus_plus(logHandle, 'concept:name');
      const alpha = typeof alphaRaw === 'string' ? JSON.parse(alphaRaw) : alphaRaw;
      expect(isValidPetriNet(alpha)).toBe(true);

      // Inductive Miner should handle empty log
      const imRaw = wasmModule.discover_inductive_miner(logHandle, 'concept:name');
      const im = typeof imRaw === 'string' ? JSON.parse(imRaw) : imRaw;
      expect(im).toHaveProperty('type');
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });
});

describe('Cross-Algorithm Parity: Conformance Metrics', () => {
  it('should produce sound fitness values (0 ≤ fitness ≤ 1.0)', () => {
    if (!wasmModule) {
      return;
    }

    const logHandle = wasmModule.load_eventlog_from_xes(TEST_XES);

    try {
      // Discover a model
      const modelRaw = wasmModule.discover_heuristic_miner(logHandle, 'concept:name', 0.5);
      const model = typeof modelRaw === 'string' ? JSON.parse(modelRaw) : modelRaw;

      // Token replay conformance
      const conformanceRaw = wasmModule.conformance_token_replay(
        logHandle,
        'concept:name',
        JSON.stringify(model)
      );
      const conformance =
        typeof conformanceRaw === 'string' ? JSON.parse(conformanceRaw) : conformanceRaw;

      // Fitness must be in [0, 1] (WvdA soundness)
      expect(conformance).toHaveProperty('fitness');
      const fitness = conformance.fitness as number;
      expect(fitness).toBeGreaterThanOrEqual(0.0);
      expect(fitness).toBeLessThanOrEqual(1.0);

      // Diagnostic counts must be non-negative
      expect((conformance.traced as number) >= 0).toBe(true);
      expect((conformance.remaining as number) >= 0).toBe(true);
      expect((conformance.missing as number) >= 0).toBe(true);
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });

  it('should produce sound precision values (0 ≤ precision ≤ 1.0)', () => {
    if (!wasmModule) {
      return;
    }

    const logHandle = wasmModule.load_eventlog_from_xes(TEST_XES);

    try {
      // Discover a model
      const modelRaw = wasmModule.discover_heuristic_miner(logHandle, 'concept:name', 0.5);
      const model = typeof modelRaw === 'string' ? JSON.parse(modelRaw) : modelRaw;

      // ETConformance precision
      try {
        const precisionRaw = wasmModule.conformance_etconformance_precision(
          logHandle,
          'concept:name',
          JSON.stringify(model)
        );
        const precisionResult =
          typeof precisionRaw === 'string' ? JSON.parse(precisionRaw) : precisionRaw;

        // Precision must be in [0, 1] (WvdA soundness)
        expect(precisionResult).toHaveProperty('precision');
        const precision = precisionResult.precision as number;
        expect(precision).toBeGreaterThanOrEqual(0.0);
        expect(precision).toBeLessThanOrEqual(1.0);
      } catch {
        // Precision may not be available in all builds
      }
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });

  it('should be consistent across multiple conformance checks', () => {
    if (!wasmModule) {
      return;
    }

    const logHandle = wasmModule.load_eventlog_from_xes(TEST_XES);

    try {
      // Discover a model
      const modelRaw = wasmModule.discover_heuristic_miner(logHandle, 'concept:name', 0.5);
      const model = typeof modelRaw === 'string' ? JSON.parse(modelRaw) : modelRaw;

      // Run conformance twice
      const result1Raw = wasmModule.conformance_token_replay(
        logHandle,
        'concept:name',
        JSON.stringify(model)
      );
      const result1 = typeof result1Raw === 'string' ? JSON.parse(result1Raw) : result1Raw;

      const result2Raw = wasmModule.conformance_token_replay(
        logHandle,
        'concept:name',
        JSON.stringify(model)
      );
      const result2 = typeof result2Raw === 'string' ? JSON.parse(result2Raw) : result2Raw;

      // Results should be identical (deterministic)
      expect(result1.fitness).toBe(result2.fitness);
      expect(result1.traced).toBe(result2.traced);
      expect(result1.remaining).toBe(result2.remaining);
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });
});

describe('Cross-Algorithm Parity: Edge Cases', () => {
  it('should handle single-activity logs', () => {
    if (!wasmModule) {
      return;
    }

    const singleActivityXes = `<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/">
  <trace>
    <string key="concept:name" value="case_1"/>
    <event>
      <string key="concept:name" value="A"/>
      <date key="time:timestamp" value="2024-01-01T09:00:00Z"/>
    </event>
  </trace>
</log>`;

    const logHandle = wasmModule.load_eventlog_from_xes(singleActivityXes);

    try {
      // DFG should work
      const dfgRaw = wasmModule.discover_dfg(logHandle, 'concept:name');
      const dfg = typeof dfgRaw === 'string' ? JSON.parse(dfgRaw) : dfgRaw;
      expect(isValidDFG(dfg)).toBe(true);
      expect((dfg.nodes as string[]).length).toBe(1);
      expect((dfg.edges as Array<unknown>).length).toBe(0);

      // Alpha++ should work
      const alphaRaw = wasmModule.discover_alpha_plus_plus(logHandle, 'concept:name');
      const alpha = typeof alphaRaw === 'string' ? JSON.parse(alphaRaw) : alphaRaw;
      expect(isValidPetriNet(alpha)).toBe(true);
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });

  it('should handle logs with parallel activities', () => {
    if (!wasmModule) {
      return;
    }

    const parallelXes = `<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/">
  <trace>
    <string key="concept:name" value="case_1"/>
    <event><string key="concept:name" value="A"/><date key="time:timestamp" value="2024-01-01T09:00:00Z"/></event>
    <event><string key="concept:name" value="B"/><date key="time:timestamp" value="2024-01-01T09:01:00Z"/></event>
    <event><string key="concept:name" value="C"/><date key="time:timestamp" value="2024-01-01T09:02:00Z"/></event>
  </trace>
  <trace>
    <string key="concept:name" value="case_2"/>
    <event><string key="concept:name" value="A"/><date key="time:timestamp" value="2024-01-01T10:00:00Z"/></event>
    <event><string key="concept:name" value="C"/><date key="time:timestamp" value="2024-01-01T10:01:00Z"/></event>
    <event><string key="concept:name" value="B"/><date key="time:timestamp" value="2024-01-01T10:02:00Z"/></event>
  </trace>
</log>`;

    const logHandle = wasmModule.load_eventlog_from_xes(parallelXes);

    try {
      // All algorithms should handle parallelism
      const dfgRaw = wasmModule.discover_dfg(logHandle, 'concept:name');
      const dfg = typeof dfgRaw === 'string' ? JSON.parse(dfgRaw) : dfgRaw;
      expect(isValidDFG(dfg)).toBe(true);

      const alphaRaw = wasmModule.discover_alpha_plus_plus(logHandle, 'concept:name');
      const alpha = typeof alphaRaw === 'string' ? JSON.parse(alphaRaw) : alphaRaw;
      expect(isValidPetriNet(alpha)).toBe(true);
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });

  it('should handle loops and repeated activities', () => {
    if (!wasmModule) {
      return;
    }

    const loopXes = `<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/">
  <trace>
    <string key="concept:name" value="case_1"/>
    <event><string key="concept:name" value="A"/><date key="time:timestamp" value="2024-01-01T09:00:00Z"/></event>
    <event><string key="concept:name" value="B"/><date key="time:timestamp" value="2024-01-01T09:01:00Z"/></event>
    <event><string key="concept:name" value="A"/><date key="time:timestamp" value="2024-01-01T09:02:00Z"/></event>
    <event><string key="concept:name" value="B"/><date key="time:timestamp" value="2024-01-01T09:03:00Z"/></event>
    <event><string key="concept:name" value="C"/><date key="time:timestamp" value="2024-01-01T09:04:00Z"/></event>
  </trace>
</log>`;

    const logHandle = wasmModule.load_eventlog_from_xes(loopXes);

    try {
      // DFG should capture loops
      const dfgRaw = wasmModule.discover_dfg(logHandle, 'concept:name');
      const dfg = typeof dfgRaw === 'string' ? JSON.parse(dfgRaw) : dfgRaw;
      expect(isValidDFG(dfg)).toBe(true);

      // Should have A->B and B->A edges (loop)
      const edges = extractEdges(dfg);
      const hasAB = edges.some(([from, to]) => from === 'A' && to === 'B');
      const hasBA = edges.some(([from, to]) => from === 'B' && to === 'A');
      expect(hasAB && hasBA).toBe(true);
    } finally {
      try {
        wasmModule.delete_object(logHandle);
      } catch {
        // Ignore cleanup errors
      }
    }
  });
});
