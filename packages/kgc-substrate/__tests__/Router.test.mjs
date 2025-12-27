/**
 * @fileoverview Router Tests - XOR, AND/OR chains, no-match, replay determinism
 */

import { describe, it, expect } from 'vitest';
import { parsePredicates, routeTask, validateConstraints, getRoutingStats } from '../src/Router.mjs';

describe('Router - parsePredicates', () => {
  it('should parse simple equality predicate', () => {
    const predicates = parsePredicates("language=='js'");
    expect(predicates).toEqual([
      { field: 'language', operator: '==', value: 'js' },
    ]);
  });

  it('should parse multiple predicates with AND', () => {
    const predicates = parsePredicates("requires_auth==true AND language=='js'");
    expect(predicates).toHaveLength(2);
    expect(predicates[0]).toEqual({ field: 'requires_auth', operator: '==', value: true });
    expect(predicates[1]).toEqual({ field: 'language', operator: '==', value: 'js' });
  });

  it('should parse predicates with OR operator', () => {
    const predicates = parsePredicates("language=='js' OR language=='ts'");
    expect(predicates).toHaveLength(2);
  });

  it('should parse IN operator with array', () => {
    const predicates = parsePredicates("language in ['js','ts','py']");
    expect(predicates).toEqual([
      { field: 'language', operator: 'in', value: ['js', 'ts', 'py'] },
    ]);
  });

  it('should parse NOT operator', () => {
    const predicates = parsePredicates("env not ['prod','staging']");
    expect(predicates).toEqual([
      { field: 'env', operator: 'not', value: ['prod', 'staging'] },
    ]);
  });

  it('should parse negated predicate with NOT prefix', () => {
    const predicates = parsePredicates("NOT env=='prod'");
    expect(predicates).toEqual([
      { field: 'env', operator: '!=', value: 'prod' },
    ]);
  });

  it('should parse numeric values', () => {
    const predicates = parsePredicates('priority>=5');
    expect(predicates).toEqual([
      { field: 'priority', operator: '>=', value: 5 },
    ]);
  });

  it('should parse boolean values', () => {
    const predicates = parsePredicates('enabled==true');
    expect(predicates).toEqual([
      { field: 'enabled', operator: '==', value: true },
    ]);
  });

  it('should handle empty constraint string', () => {
    const predicates = parsePredicates('');
    expect(predicates).toEqual([]);
  });

  it('should throw error for invalid syntax', () => {
    expect(() => parsePredicates('invalid syntax here')).toThrow(/Invalid predicate syntax/);
  });

  it('should throw error for exceeding MAX_PREDICATES', () => {
    // Generate constraint with >1000 predicates
    const hugeConstraint = Array(1001)
      .fill("field==value")
      .join(' AND ');
    expect(() => parsePredicates(hugeConstraint)).toThrow(/maximum/);
  });

  it('should throw error for non-string input', () => {
    expect(() => parsePredicates(123)).toThrow(/Constraint must be a string/);
  });
});

describe('Router - routeTask (XOR semantics)', () => {
  it('should route to agent matching XOR constraint (A AND NOT B)', () => {
    const workItem = {
      id: 'task-xor-1',
      predicates: "env=='dev' AND NOT env=='prod'",
    };

    const agents = [
      { id: 'dev-agent', capabilities: { env: 'dev' } },
      { id: 'prod-agent', capabilities: { env: 'prod' } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBe('dev-agent');
  });

  it('should not route if both XOR conditions are true', () => {
    const workItem = {
      id: 'task-xor-2',
      predicates: "flag_a==true AND NOT flag_b==true",
    };

    const agents = [
      { id: 'agent-both', capabilities: { flag_a: true, flag_b: true } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBeNull();
  });

  it('should route for XOR using NOT IN operator', () => {
    const workItem = {
      id: 'task-xor-3',
      predicates: "env in ['dev','test'] AND env not ['prod']",
    };

    const agents = [
      { id: 'dev-agent', capabilities: { env: 'dev' } },
      { id: 'prod-agent', capabilities: { env: 'prod' } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBe('dev-agent');
  });
});

describe('Router - routeTask (Complex AND/OR chains)', () => {
  it('should handle pure AND chain (all must match)', () => {
    const workItem = {
      id: 'task-and',
      predicates: "language=='js' AND requires_auth==true AND priority>=3",
    };

    const agents = [
      { id: 'agent-1', capabilities: { language: 'js', requires_auth: true, priority: 5 } },
      { id: 'agent-2', capabilities: { language: 'js', requires_auth: false, priority: 5 } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBe('agent-1');
  });

  it('should handle pure OR chain (any can match)', () => {
    const workItem = {
      id: 'task-or',
      predicates: "language=='js' OR language=='ts' OR language=='py'",
    };

    const agents = [
      { id: 'agent-py', capabilities: { language: 'py' } },
      { id: 'agent-go', capabilities: { language: 'go' } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBe('agent-py');
  });

  it('should handle mixed AND/OR expressions', () => {
    // (language=='js' AND auth==true) OR (language=='py' AND auth==false)
    const workItem = {
      id: 'task-mixed',
      predicates: "language=='js' AND requires_auth==true OR language=='py' AND requires_auth==false",
    };

    const agents = [
      { id: 'agent-js-auth', capabilities: { language: 'js', requires_auth: true } },
      { id: 'agent-py-no-auth', capabilities: { language: 'py', requires_auth: false } },
      { id: 'agent-go', capabilities: { language: 'go', requires_auth: true } },
    ];

    // First matching agent (js with auth)
    const result = routeTask(workItem, agents);
    expect(result).toBe('agent-js-auth');
  });

  it('should handle complex nested conditions', () => {
    const workItem = {
      id: 'task-complex',
      predicates: "tier in ['premium','enterprise'] AND region=='us-east' AND load<80",
    };

    const agents = [
      { id: 'agent-1', capabilities: { tier: 'free', region: 'us-east', load: 50 } },
      { id: 'agent-2', capabilities: { tier: 'premium', region: 'us-west', load: 60 } },
      { id: 'agent-3', capabilities: { tier: 'premium', region: 'us-east', load: 70 } },
      { id: 'agent-4', capabilities: { tier: 'enterprise', region: 'us-east', load: 95 } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBe('agent-3');
  });

  it('should handle triple AND chain', () => {
    const workItem = {
      id: 'task-triple-and',
      predicates: "lang=='js' AND framework=='react' AND version>=18",
    };

    const agents = [
      { id: 'agent-match', capabilities: { lang: 'js', framework: 'react', version: 18 } },
      { id: 'agent-no-match', capabilities: { lang: 'js', framework: 'vue', version: 18 } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBe('agent-match');
  });
});

describe('Router - routeTask (No-match scenarios)', () => {
  it('should return null when no agent matches', () => {
    const workItem = {
      id: 'task-no-match',
      predicates: "language=='rust'",
    };

    const agents = [
      { id: 'agent-js', capabilities: { language: 'js' } },
      { id: 'agent-py', capabilities: { language: 'py' } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBeNull();
  });

  it('should return null when predicates are unsatisfiable', () => {
    const workItem = {
      id: 'task-unsatisfiable',
      predicates: "value>100 AND value<50", // Impossible
    };

    const agents = [
      { id: 'agent-1', capabilities: { value: 75 } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBeNull();
  });

  it('should return null when agent list is empty', () => {
    const workItem = {
      id: 'task-empty-agents',
      predicates: "language=='js'",
    };

    const result = routeTask(workItem, []);
    expect(result).toBeNull();
  });

  it('should return null when agent lacks required capability', () => {
    const workItem = {
      id: 'task-missing-cap',
      predicates: "missing_field==true",
    };

    const agents = [
      { id: 'agent-1', capabilities: { other_field: true } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBeNull();
  });
});

describe('Router - Replay Determinism', () => {
  it('should return identical results for identical inputs', () => {
    const workItem = {
      id: 'task-replay',
      predicates: "language=='js' AND priority>=5",
    };

    const agents = [
      { id: 'agent-1', capabilities: { language: 'js', priority: 5 } },
      { id: 'agent-2', capabilities: { language: 'js', priority: 8 } },
      { id: 'agent-3', capabilities: { language: 'py', priority: 10 } },
    ];

    // Execute multiple times
    const results = [];
    for (let i = 0; i < 10; i++) {
      results.push(routeTask(workItem, agents));
    }

    // All results must be identical
    const firstResult = results[0];
    expect(results.every(r => r === firstResult)).toBe(true);
    expect(firstResult).toBe('agent-1'); // First matching agent
  });

  it('should produce deterministic results for complex predicates', () => {
    const workItem = {
      id: 'task-complex-replay',
      predicates: "tier in ['premium','enterprise'] OR priority>9 AND region=='us-east'",
    };

    const agents = [
      { id: 'agent-A', capabilities: { tier: 'free', priority: 10, region: 'us-east' } },
      { id: 'agent-B', capabilities: { tier: 'premium', priority: 5, region: 'us-west' } },
      { id: 'agent-C', capabilities: { tier: 'enterprise', priority: 3, region: 'eu-west' } },
    ];

    const results = Array(20).fill(null).map(() => routeTask(workItem, agents));

    // Verify all results are identical
    expect(new Set(results).size).toBe(1);
    expect(results[0]).toBe('agent-A'); // First match (priority>9 AND region=='us-east')
  });

  it('should maintain determinism with no-match cases', () => {
    const workItem = {
      id: 'task-no-match-replay',
      predicates: "nonexistent_field=='value'",
    };

    const agents = [
      { id: 'agent-1', capabilities: { other: 'data' } },
    ];

    const results = Array(5).fill(null).map(() => routeTask(workItem, agents));

    expect(results.every(r => r === null)).toBe(true);
  });

  it('should be deterministic across different predicate orderings', () => {
    const workItem1 = {
      id: 'task-order-1',
      predicates: "language=='js' AND framework=='react'",
    };

    const workItem2 = {
      id: 'task-order-2',
      predicates: "framework=='react' AND language=='js'",
    };

    const agents = [
      { id: 'agent-match', capabilities: { language: 'js', framework: 'react' } },
    ];

    const result1 = routeTask(workItem1, agents);
    const result2 = routeTask(workItem2, agents);

    expect(result1).toBe(result2);
    expect(result1).toBe('agent-match');
  });
});

describe('Router - Input Validation', () => {
  it('should throw error for invalid workItem schema', () => {
    const invalidWorkItem = { id: 'test' }; // Missing predicates

    const agents = [{ id: 'agent-1', capabilities: {} }];

    expect(() => routeTask(invalidWorkItem, agents)).toThrow();
  });

  it('should throw error for invalid agent schema', () => {
    const workItem = { id: 'task-1', predicates: "field=='value'" };

    const invalidAgents = [{ id: 'agent-1' }]; // Missing capabilities

    expect(() => routeTask(workItem, invalidAgents)).toThrow();
  });

  it('should throw error for non-array agents', () => {
    const workItem = { id: 'task-1', predicates: "field=='value'" };

    expect(() => routeTask(workItem, 'not-an-array')).toThrow();
  });
});

describe('Router - validateConstraints', () => {
  it('should validate correct constraints', () => {
    const result = validateConstraints("language=='js' AND requires_auth==true");
    expect(result.valid).toBe(true);
    expect(result.predicateCount).toBe(2);
  });

  it('should return error for invalid constraints', () => {
    const result = validateConstraints('invalid syntax!!!');
    expect(result.valid).toBe(false);
    expect(result.error).toBeDefined();
  });

  it('should validate empty constraints', () => {
    const result = validateConstraints('');
    expect(result.valid).toBe(true);
    expect(result.predicateCount).toBe(0);
  });
});

describe('Router - getRoutingStats', () => {
  it('should provide routing statistics', () => {
    const workItems = [
      { id: 'task-1', predicates: "language=='js'" },
      { id: 'task-2', predicates: "language=='py'" },
      { id: 'task-3', predicates: "language=='js'" },
      { id: 'task-4', predicates: "language=='go'" },
    ];

    const agents = [
      { id: 'agent-js', capabilities: { language: 'js' } },
      { id: 'agent-py', capabilities: { language: 'py' } },
    ];

    const stats = getRoutingStats(workItems, agents);

    expect(stats.totalItems).toBe(4);
    expect(stats.routed).toBe(3);
    expect(stats.unrouted).toBe(1);
    expect(stats.routingMap).toEqual({
      'agent-js': 2,
      'agent-py': 1,
    });
  });
});

describe('Router - Edge Cases', () => {
  it('should handle comparison operators correctly', () => {
    const agents = [
      { id: 'agent-low', capabilities: { value: 3 } },
      { id: 'agent-mid', capabilities: { value: 7 } },
      { id: 'agent-high', capabilities: { value: 12 } },
    ];

    expect(routeTask({ id: 't1', predicates: 'value>5' }, agents)).toBe('agent-mid');
    expect(routeTask({ id: 't2', predicates: 'value>=7' }, agents)).toBe('agent-mid');
    expect(routeTask({ id: 't3', predicates: 'value<10' }, agents)).toBe('agent-low');
    expect(routeTask({ id: 't4', predicates: 'value<=3' }, agents)).toBe('agent-low');
  });

  it('should handle != operator', () => {
    const workItem = { id: 'task-neq', predicates: "env!='prod'" };
    const agents = [
      { id: 'dev-agent', capabilities: { env: 'dev' } },
      { id: 'prod-agent', capabilities: { env: 'prod' } },
    ];

    expect(routeTask(workItem, agents)).toBe('dev-agent');
  });

  it('should handle case-insensitive operators (AND/OR)', () => {
    const workItem = { id: 'task-case', predicates: "a==1 and b==2 or c==3" };
    const agents = [
      { id: 'agent-1', capabilities: { a: 1, b: 2 } },
    ];

    const result = routeTask(workItem, agents);
    expect(result).toBe('agent-1');
  });

  it('should handle whitespace variations', () => {
    const predicates1 = parsePredicates("language=='js'");
    const predicates2 = parsePredicates("  language  ==  'js'  ");

    expect(predicates1).toEqual(predicates2);
  });
});
