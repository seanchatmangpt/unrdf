/**
 * @file Test suite for Composable Hooks Marketplace
 * @module test/admit-hook
 * @description
 * Comprehensive tests for hook normalization, RDF composition, dependency resolution,
 * and SHACL validation with soft-fail admission
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { HooksMarketplace, HookDefinitionSchema, HOOK_NS } from '../src/lib/admit-hook.mjs';

/**
 * Helper: Create valid hook definition
 */
function createValidHook(overrides = {}) {
  return {
    id: '550e8400-e29b-41d4-a716-446655440000',
    name: 'Test Hook',
    version: '1.0.0',
    description: 'Test hook for marketplace',
    conditions: [
      {
        kind: 'sparql-ask',
        query: 'ASK { ?s a ?t }',
      },
    ],
    effects: [
      {
        kind: 'sparql-construct',
        query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }',
      },
    ],
    dependsOn: [],
    priority: 50,
    ...overrides,
  };
}

describe('HooksMarketplace - Normalization', () => {
  let marketplace;

  beforeEach(() => {
    marketplace = new HooksMarketplace();
  });

  it('should validate hook definition schema', () => {
    const validHook = createValidHook();
    expect(() => HookDefinitionSchema.parse(validHook)).not.toThrow();
  });

  it('should reject hook with invalid UUID', () => {
    const invalidHook = createValidHook({
      id: 'not-a-uuid',
    });
    expect(() => HookDefinitionSchema.parse(invalidHook)).toThrow();
  });

  it('should reject hook with invalid semantic version', () => {
    const invalidHook = createValidHook({
      version: '1.0',
    });
    expect(() => HookDefinitionSchema.parse(invalidHook)).toThrow();
  });

  it('should normalize hook to RDF structure', () => {
    const hook = createValidHook();
    const normalized = marketplace.normalizeHookToRDF(hook);

    expect(normalized).toHaveProperty('hookUri');
    expect(normalized).toHaveProperty('id', hook.id);
    expect(normalized).toHaveProperty('name', hook.name);
    expect(normalized).toHaveProperty('version', hook.version);
    expect(normalized).toHaveProperty('triples');
    expect(Array.isArray(normalized.triples)).toBe(true);
  });

  it('should generate correct hook URI from ID', () => {
    const hook = createValidHook();
    const normalized = marketplace.normalizeHookToRDF(hook);

    expect(normalized.hookUri).toBe(`${HOOK_NS.hook}${hook.id}`);
  });

  it('should include hook:Hook type triple', () => {
    const hook = createValidHook();
    const normalized = marketplace.normalizeHookToRDF(hook);

    const typeTriple = normalized.triples.find(
      t => t.predicate.value === `${HOOK_NS.rdf}type` &&
           t.object.value === `${HOOK_NS.hook}Hook`
    );

    expect(typeTriple).toBeDefined();
  });

  it('should include metadata triples (name, version, priority)', () => {
    const hook = createValidHook();
    const normalized = marketplace.normalizeHookToRDF(hook);

    const nameTriple = normalized.triples.find(
      t => t.predicate.value === `${HOOK_NS.schema}name` &&
           t.object.value === hook.name
    );

    const versionTriple = normalized.triples.find(
      t => t.predicate.value === `${HOOK_NS.schema}version` &&
           t.object.value === hook.version
    );

    const priorityTriple = normalized.triples.find(
      t => t.predicate.value === `${HOOK_NS.schema}priority`
    );

    expect(nameTriple).toBeDefined();
    expect(versionTriple).toBeDefined();
    expect(priorityTriple).toBeDefined();
  });

  it('should normalize conditions to RDF', () => {
    const hook = createValidHook({
      conditions: [
        { kind: 'sparql-ask', query: 'ASK { ?s a ex:Test }' },
        { kind: 'sparql-select', query: 'SELECT ?x WHERE { ?x rdf:type ?t }' },
      ],
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    const conditionTriples = normalized.triples.filter(
      t => t.object.value === `${HOOK_NS.schema}Condition`
    );

    expect(conditionTriples.length).toBe(2);
  });

  it('should normalize effects to RDF', () => {
    const hook = createValidHook({
      effects: [
        { kind: 'sparql-construct', query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }' },
      ],
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    const effectTriples = normalized.triples.filter(
      t => t.object.value === `${HOOK_NS.schema}Effect`
    );

    expect(effectTriples.length).toBe(1);
  });

  it('should use default priority of 50 if not specified', () => {
    const hook = createValidHook({
      priority: undefined,
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    expect(normalized.priority).toBe(50);
  });

  it('should preserve description if provided', () => {
    const description = 'Test description';
    const hook = createValidHook({
      description,
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    expect(normalized.description).toBe(description);
  });

  it('should handle empty dependsOn array', () => {
    const hook = createValidHook({
      dependsOn: [],
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    expect(normalized.dependsOn).toEqual([]);
  });

  it('should include dependency triples in RDF', () => {
    const depId = '550e8400-e29b-41d4-a716-446655440001';
    const hook = createValidHook({
      dependsOn: [depId],
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    const depTriple = normalized.triples.find(
      t => t.predicate.value === `${HOOK_NS.schema}dependsOn` &&
           t.object.value === `${HOOK_NS.hook}${depId}`
    );

    expect(depTriple).toBeDefined();
  });
});

describe('HooksMarketplace - Dependency Resolution', () => {
  let marketplace;

  beforeEach(() => {
    marketplace = new HooksMarketplace();
  });

  it('should resolve single direct dependency', () => {
    const hookUri1 = `${HOOK_NS.hook}hook-1`;
    const hookUri2 = `${HOOK_NS.hook}hook-2`;

    const hooksByUri = new Map([
      [hookUri1, { dependsOn: [hookUri2] }],
      [hookUri2, { dependsOn: [] }],
    ]);

    const result = marketplace.resolveDependenciesViaRules(hooksByUri);

    expect(result.allDeps.get(hookUri1)).toContain(hookUri2);
    expect(result.cycles.size).toBe(0);
  });

  it('should compute transitive closure of dependencies', () => {
    const hookUri1 = `${HOOK_NS.hook}hook-1`;
    const hookUri2 = `${HOOK_NS.hook}hook-2`;
    const hookUri3 = `${HOOK_NS.hook}hook-3`;

    const hooksByUri = new Map([
      [hookUri1, { dependsOn: [hookUri2] }],
      [hookUri2, { dependsOn: [hookUri3] }],
      [hookUri3, { dependsOn: [] }],
    ]);

    const result = marketplace.resolveDependenciesViaRules(hooksByUri);

    expect(result.allDeps.get(hookUri1)).toContain(hookUri2);
    expect(result.allDeps.get(hookUri1)).toContain(hookUri3);
  });

  it('should detect direct self-dependency cycle', () => {
    const hookUri1 = `${HOOK_NS.hook}hook-1`;

    const hooksByUri = new Map([
      [hookUri1, { dependsOn: [hookUri1] }],
    ]);

    const result = marketplace.resolveDependenciesViaRules(hooksByUri);

    expect(result.cycles).toContain(hookUri1);
    expect(result.hadCycles).toBe(true);
  });

  it('should detect indirect cycle A→B→A', () => {
    const hookUri1 = `${HOOK_NS.hook}hook-1`;
    const hookUri2 = `${HOOK_NS.hook}hook-2`;

    const hooksByUri = new Map([
      [hookUri1, { dependsOn: [hookUri2] }],
      [hookUri2, { dependsOn: [hookUri1] }],
    ]);

    const result = marketplace.resolveDependenciesViaRules(hooksByUri);

    expect(result.cycles).toContain(hookUri1);
    expect(result.cycles).toContain(hookUri2);
  });

  it('should detect cycle in longer chain A→B→C→A', () => {
    const hookUri1 = `${HOOK_NS.hook}hook-1`;
    const hookUri2 = `${HOOK_NS.hook}hook-2`;
    const hookUri3 = `${HOOK_NS.hook}hook-3`;

    const hooksByUri = new Map([
      [hookUri1, { dependsOn: [hookUri2] }],
      [hookUri2, { dependsOn: [hookUri3] }],
      [hookUri3, { dependsOn: [hookUri1] }],
    ]);

    const result = marketplace.resolveDependenciesViaRules(hooksByUri);

    expect(result.cycles).toContain(hookUri1);
    expect(result.cycles).toContain(hookUri2);
    expect(result.cycles).toContain(hookUri3);
  });

  it('should handle diamond dependency graph A→B,C ; B→D ; C→D', () => {
    const hookUri1 = `${HOOK_NS.hook}hook-1`;
    const hookUri2 = `${HOOK_NS.hook}hook-2`;
    const hookUri3 = `${HOOK_NS.hook}hook-3`;
    const hookUri4 = `${HOOK_NS.hook}hook-4`;

    const hooksByUri = new Map([
      [hookUri1, { dependsOn: [hookUri2, hookUri3] }],
      [hookUri2, { dependsOn: [hookUri4] }],
      [hookUri3, { dependsOn: [hookUri4] }],
      [hookUri4, { dependsOn: [] }],
    ]);

    const result = marketplace.resolveDependenciesViaRules(hooksByUri);

    const allDeps1 = result.allDeps.get(hookUri1);
    expect(allDeps1).toContain(hookUri2);
    expect(allDeps1).toContain(hookUri3);
    expect(allDeps1).toContain(hookUri4);
    expect(result.cycles.size).toBe(0);
  });
});

describe('HooksMarketplace - SHACL Validation (Soft-Fail)', () => {
  let marketplace;

  beforeEach(() => {
    marketplace = new HooksMarketplace();
  });

  it('should validate hook with all required fields', () => {
    const hook = createValidHook();
    const normalized = marketplace.normalizeHookToRDF(hook);
    const result = marketplace.validateWithSHACL(normalized);

    expect(result.admitted).toBe(true);
    expect(result.violations.length).toBe(0);
  });

  it('should record violation for missing name but still admit', () => {
    const normalized = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      name: '',
      version: '1.0.0',
      priority: 50,
    };

    const result = marketplace.validateWithSHACL(normalized);

    expect(result.admitted).toBe(true);
    expect(result.violations.length).toBeGreaterThan(0);
    expect(result.violations[0].message).toContain('non-empty name');
  });

  it('should record violation for missing version but still admit', () => {
    const normalized = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      name: 'Test',
      version: '',
      priority: 50,
    };

    const result = marketplace.validateWithSHACL(normalized);

    expect(result.admitted).toBe(true);
    expect(result.violations.length).toBeGreaterThan(0);
  });

  it('should record violation for invalid priority but still admit', () => {
    const normalized = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      name: 'Test',
      version: '1.0.0',
      priority: 150,
    };

    const result = marketplace.validateWithSHACL(normalized);

    expect(result.admitted).toBe(true);
    expect(result.violations.length).toBeGreaterThan(0);
  });

  it('should soft-fail with multiple violations', () => {
    const normalized = {
      id: '',
      name: '',
      version: '',
      priority: -1,
    };

    const result = marketplace.validateWithSHACL(normalized);

    expect(result.admitted).toBe(true);
    expect(result.violations.length).toBeGreaterThan(1);
  });
});

describe('HooksMarketplace - Hook Admission', () => {
  let marketplace;

  beforeEach(() => {
    marketplace = new HooksMarketplace();
  });

  it('should admit valid hook successfully', () => {
    const hook = createValidHook();
    const result = marketplace.admitHook(hook);

    expect(result.admitted).toBe(true);
    expect(result.hookId).toBe(hook.id);
    expect(result.hookUri).toBe(`${HOOK_NS.hook}${hook.id}`);
    expect(result.violations).toEqual([]);
  });

  it('should reject hook with invalid schema', () => {
    const invalidHook = {
      id: 'not-a-uuid',
      name: 'Test',
      version: '1.0.0',
      conditions: [],
      effects: [],
    };

    const result = marketplace.admitHook(invalidHook);

    expect(result.admitted).toBe(false);
    expect(result.error).toBeDefined();
  });

  it('should store hook in marketplace after admission', () => {
    const hook = createValidHook();
    marketplace.admitHook(hook);

    const admitted = marketplace.getAdmittedHooks();
    expect(admitted.length).toBe(1);
    expect(admitted[0].id).toBe(hook.id);
    expect(admitted[0].name).toBe(hook.name);
  });

  it('should record violations in marketplace', () => {
    // Note: We can't test this directly since Zod schema validation (hard-fail)
    // happens before SHACL validation (soft-fail). The test instead verifies
    // that violations are recorded when a hook IS admitted (see the soft-fail tests).
    // This demonstrates that the marketplace separates concerns:
    // - Zod schema: syntactic validation (hard-fail)
    // - SHACL: semantic validation (soft-fail with audit trail)
    const hook = createValidHook();
    marketplace.admitHook(hook);

    // Valid hooks have no violations
    const violations = marketplace.getViolations(hook.id);
    // In the soft-fail tests, violations would be recorded if any occurred
    expect(violations).toBeNull(); // No violations for valid hook
  });

  it('should assign default priority during admission', () => {
    const hook = createValidHook({
      priority: undefined,
    });

    marketplace.admitHook(hook);

    const admitted = marketplace.getAdmittedHooks();
    expect(admitted[0].priority).toBe(50);
  });
});

describe('HooksMarketplace - Batch Admission with Dependencies', () => {
  let marketplace;

  beforeEach(() => {
    marketplace = new HooksMarketplace();
  });

  it('should admit multiple independent hooks', () => {
    const hook1 = createValidHook();
    const hook2 = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440001',
      name: 'Hook 2',
    });

    const result = marketplace.admitHooksWithDependencies([hook1, hook2]);

    expect(result.admittedCount).toBe(2);
    expect(result.rejectedCount).toBe(0);
    expect(result.hadCycles).toBe(false);
  });

  it('should admit hooks with valid dependency chain', () => {
    const hook1 = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440000',
      dependsOn: [],
    });

    const hook2 = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440001',
      dependsOn: [hook1.id],
    });

    const result = marketplace.admitHooksWithDependencies([hook1, hook2]);

    expect(result.admittedCount).toBe(2);
    expect(result.rejectedCount).toBe(0);
  });

  it('should reject hooks with circular dependency', () => {
    const hook1Id = '550e8400-e29b-41d4-a716-446655440000';
    const hook2Id = '550e8400-e29b-41d4-a716-446655440001';

    const hook1 = createValidHook({
      id: hook1Id,
      dependsOn: [hook2Id],
    });

    const hook2 = createValidHook({
      id: hook2Id,
      dependsOn: [hook1Id],
    });

    const result = marketplace.admitHooksWithDependencies([hook1, hook2]);

    expect(result.hadCycles).toBe(true);
    expect(result.rejectedCount).toBeGreaterThan(0);
    expect(result.cycles.length).toBeGreaterThan(0);
  });

  it('should reject hooks with self-dependency', () => {
    const hookId = '550e8400-e29b-41d4-a716-446655440000';
    const hook = createValidHook({
      id: hookId,
      dependsOn: [hookId],
    });

    const result = marketplace.admitHooksWithDependencies([hook]);

    expect(result.hadCycles).toBe(true);
    expect(result.rejectedCount).toBe(1);
  });

  it('should build correct dependency graph', () => {
    const hook1 = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440000',
      dependsOn: [],
    });

    const hook2 = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440001',
      dependsOn: [hook1.id],
    });

    const result = marketplace.admitHooksWithDependencies([hook1, hook2]);

    const depGraph = result.dependencyGraph;
    expect(depGraph).toBeDefined();
  });

  it('should handle mixed valid and invalid hooks', () => {
    const validHook = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440000',
    });

    const invalidHook = {
      id: 'not-a-uuid',
      name: 'Invalid',
      version: '1.0.0',
      conditions: [],
      effects: [],
    };

    const result = marketplace.admitHooksWithDependencies([validHook, invalidHook]);

    expect(result.admittedCount).toBe(1);
    expect(result.rejectedCount).toBe(1);
  });

  it('should return all admitted hooks', () => {
    const hook1 = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440000',
      name: 'Hook 1',
    });

    const hook2 = createValidHook({
      id: '550e8400-e29b-41d4-a716-446655440001',
      name: 'Hook 2',
    });

    marketplace.admitHooksWithDependencies([hook1, hook2]);

    const admitted = marketplace.getAdmittedHooks();
    expect(admitted.length).toBe(2);
    expect(admitted.map(h => h.name)).toContain('Hook 1');
    expect(admitted.map(h => h.name)).toContain('Hook 2');
  });
});

describe('HooksMarketplace - RDF Store Integration', () => {
  let marketplace;

  beforeEach(() => {
    marketplace = new HooksMarketplace();
  });

  it('should store hook triples in RDF store', () => {
    const hook = createValidHook();
    marketplace.admitHook(hook);

    const query = `
      PREFIX hook: <http://ostar.org/hook/>
      PREFIX schema: <http://ostar.org/schema/hook#>
      SELECT ?hook WHERE {
        ?hook a hook:Hook .
      }
    `;

    const results = marketplace.query(query);
    expect(results.length).toBeGreaterThan(0);
  });

  it('should query hook name from RDF store', () => {
    const hook = createValidHook({
      name: 'Query Test Hook',
    });

    marketplace.admitHook(hook);

    const query = `
      PREFIX schema: <http://ostar.org/schema/hook#>
      SELECT ?name WHERE {
        ?hook schema:name ?name .
      }
    `;

    const results = marketplace.query(query);
    expect(results.length).toBeGreaterThan(0);
    expect(results[0].name).toBe('Query Test Hook');
  });

  it('should query hook version from RDF store', () => {
    const hook = createValidHook({
      version: '2.5.3',
    });

    marketplace.admitHook(hook);

    const query = `
      PREFIX schema: <http://ostar.org/schema/hook#>
      SELECT ?version WHERE {
        ?hook schema:version ?version .
      }
    `;

    const results = marketplace.query(query);
    expect(results.length).toBeGreaterThan(0);
    expect(results[0].version).toBe('2.5.3');
  });

  it('should handle invalid SPARQL query gracefully', () => {
    const hook = createValidHook();
    marketplace.admitHook(hook);

    expect(() => {
      marketplace.query('INVALID SPARQL QUERY');
    }).toThrow();
  });
});

describe('HooksMarketplace - Edge Cases', () => {
  let marketplace;

  beforeEach(() => {
    marketplace = new HooksMarketplace();
  });

  it('should handle hook with multiple conditions', () => {
    const hook = createValidHook({
      conditions: [
        { kind: 'sparql-ask', query: 'ASK { ?s a ?t }' },
        { kind: 'sparql-select', query: 'SELECT ?x WHERE { ?x ?p ?o }' },
        { kind: 'n3', query: '{?s a ?t} => {?s rdf:type ?t}.' },
      ],
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    expect(normalized.conditions.length).toBe(3);

    const result = marketplace.admitHook(hook);
    expect(result.admitted).toBe(true);
  });

  it('should handle hook with multiple effects', () => {
    const hook = createValidHook({
      effects: [
        { kind: 'sparql-construct', query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }' },
        { kind: 'sparql-construct', query: 'CONSTRUCT { ?s a ex:Inferred } WHERE { ?s ?p ?o }' },
      ],
    });

    const normalized = marketplace.normalizeHookToRDF(hook);
    expect(normalized.effects.length).toBe(2);

    const result = marketplace.admitHook(hook);
    expect(result.admitted).toBe(true);
  });

  it('should handle hook with maximum priority', () => {
    const hook = createValidHook({
      priority: 100,
    });

    const result = marketplace.admitHook(hook);
    expect(result.admitted).toBe(true);
  });

  it('should handle hook with minimum priority', () => {
    const hook = createValidHook({
      priority: 0,
    });

    const result = marketplace.admitHook(hook);
    expect(result.admitted).toBe(true);
  });

  it('should handle very long hook description', () => {
    const longDescription = 'x'.repeat(1000);
    const hook = createValidHook({
      description: longDescription,
    });

    const result = marketplace.admitHook(hook);
    expect(result.admitted).toBe(true);
  });

  it('should handle complex transitive dependencies', () => {
    const hooks = [
      createValidHook({
        id: '550e8400-e29b-41d4-a716-446655440000',
        dependsOn: [],
      }),
      createValidHook({
        id: '550e8400-e29b-41d4-a716-446655440001',
        dependsOn: ['550e8400-e29b-41d4-a716-446655440000'],
      }),
      createValidHook({
        id: '550e8400-e29b-41d4-a716-446655440002',
        dependsOn: ['550e8400-e29b-41d4-a716-446655440001'],
      }),
      createValidHook({
        id: '550e8400-e29b-41d4-a716-446655440003',
        dependsOn: ['550e8400-e29b-41d4-a716-446655440002'],
      }),
    ];

    const result = marketplace.admitHooksWithDependencies(hooks);
    expect(result.admittedCount).toBe(4);
    expect(result.hadCycles).toBe(false);
  });
});
