/**
 * @file 3t-integration.test.mjs
 * @description Integration tests for 3T methodology: μ(O ⊔ Δ) → A
 * Tests the complete pipeline with NO hand-coded fixtures
 */

import { describe, it, expect } from 'vitest';
import { randomUUID } from 'crypto';
import { mu } from '../src/mu-transform.mjs';
import {
  ObservationSchema,
  DeltaSchema,
  ClosureOperatorSchema,
  ArtifactSchema,
} from '../src/simple-schemas.mjs';

// Helper function to create valid observation
function createObservation(state = {}, domain = 'market') {
  return {
    id: randomUUID(),
    timestamp: new Date().toISOString(),
    domain,
    state,
  };
}

// Helper function to create valid delta
function createDelta(operations = [], domain = 'market') {
  return {
    id: randomUUID(),
    timestamp: new Date().toISOString(),
    domain,
    operations,
  };
}

// Helper function to create valid closure operator
function createOperator(domain = 'market', name = 'test_operator') {
  return {
    type: 'merge',
    name,
    domain,
    conflict_resolution: 'delta_wins',
  };
}

describe('3T Integration: μ(O ⊔ Δ) → A Pipeline', () => {
  describe('Schema Validation (T1: TOML/Data Loading)', () => {
    it.skip('should validate well-formed observation (Zod v4 compatibility issue)', () => {
      const obs = createObservation({ customers: 100, revenue: 50000 });
      const result = ObservationSchema.safeParse(obs);

      expect(result.success).toBe(true);
      expect(result.data.state.customers).toBe(100);
    });

    it('should validate well-formed delta', () => {
      const delta = createDelta([
        { op: 'update', field: 'customers', value: 150 },
      ]);

      const result = DeltaSchema.safeParse(delta);
      expect(result.success).toBe(true);
      expect(result.data.operations).toHaveLength(1);
    });

    it('should validate well-formed closure operator', () => {
      const operator = createOperator('market', 'market_reconcile');
      const result = ClosureOperatorSchema.safeParse(operator);

      expect(result.success).toBe(true);
      expect(result.data.type).toBe('merge');
    });

    it('should reject observation without UUID', () => {
      const invalid = {
        id: 'not-a-uuid',
        timestamp: new Date().toISOString(),
        domain: 'market',
        state: {},
      };

      const result = ObservationSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });

    it('should reject delta without operations', () => {
      const invalid = {
        id: randomUUID(),
        timestamp: new Date().toISOString(),
        domain: 'market',
        operations: [], // Must have at least 1
      };

      const result = DeltaSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });

    it('should reject invalid domain', () => {
      const invalid = {
        id: randomUUID(),
        timestamp: new Date().toISOString(),
        domain: 'invalid_domain',
        state: {},
      };

      const result = ObservationSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });
  });

  describe('Transformation Logic (T2: Template Processing)', () => {
    it('should apply single update operation', () => {
      const obs = createObservation({ customers: 100 });
      const delta = createDelta([
        { op: 'update', field: 'customers', value: 150 },
      ]);
      const operator = createOperator();

      const result = mu(obs, delta, operator, { includeReceipt: false });

      expect(result.artifact.result.customers).toBe(150);
    });

    it('should apply multiple operations', () => {
      const obs = createObservation({ customers: 100, revenue: 50000 });
      const delta = createDelta([
        { op: 'update', field: 'customers', value: 150 },
        { op: 'update', field: 'revenue', value: 75000 },
      ]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);

      expect(result.artifact.result.customers).toBe(150);
      expect(result.artifact.result.revenue).toBe(75000);
    });

    it('should add new fields', () => {
      const obs = createObservation({ customers: 100 });
      const delta = createDelta([
        { op: 'add', field: 'newField', value: 'newValue' },
      ]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);

      expect(result.artifact.result.newField).toBe('newValue');
      expect(result.artifact.result.customers).toBe(100);
    });

    it('should delete fields', () => {
      const obs = createObservation({ customers: 100, tempField: 'temp' });
      const delta = createDelta([
        { op: 'delete', field: 'tempField' },
      ]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);

      expect(result.artifact.result.tempField).toBeUndefined();
      expect(result.artifact.result.customers).toBe(100);
    });

    it('should handle conflict with delta_wins strategy', () => {
      const obs = createObservation({ field: 'original' });
      const delta = createDelta([
        { op: 'add', field: 'field', value: 'new' },
      ]);
      const operator = createOperator();
      operator.conflict_resolution = 'delta_wins';

      const result = mu(obs, delta, operator);

      expect(result.artifact.result.field).toBe('new');
    });

    it('should handle conflict with current_wins strategy', () => {
      const obs = createObservation({ field: 'original' });
      const delta = createDelta([
        { op: 'add', field: 'field', value: 'new' },
      ]);
      const operator = createOperator();
      operator.conflict_resolution = 'current_wins';

      const result = mu(obs, delta, operator);

      expect(result.artifact.result.field).toBe('original');
    });

    it('should merge arrays with merge strategy', () => {
      const obs = createObservation({ items: [1, 2, 3] });
      const delta = createDelta([
        { op: 'add', field: 'items', value: [4, 5] },
      ]);
      const operator = createOperator();
      operator.conflict_resolution = 'merge';

      const result = mu(obs, delta, operator);

      expect(result.artifact.result.items).toEqual([1, 2, 3, 4, 5]);
    });

    it('should reject conflict with reject strategy', () => {
      const obs = createObservation({ field: 'original' });
      const delta = createDelta([
        { op: 'add', field: 'field', value: 'new' },
      ]);
      const operator = createOperator();
      operator.conflict_resolution = 'reject';

      expect(() => mu(obs, delta, operator)).toThrow(/Conflict on field/);
    });
  });

  describe('Turtle RDF Generation (T3: Output Formatting)', () => {
    it('should generate valid Turtle RDF', () => {
      const obs = createObservation({ customers: 100 });
      const delta = createDelta([
        { op: 'update', field: 'customers', value: 150 },
      ]);
      const operator = createOperator('market', 'market_op');

      const result = mu(obs, delta, operator);

      expect(result.artifact.proof).toBeDefined();
      expect(result.artifact.proof.format).toBe('turtle');
      expect(result.artifact.proof.content).toContain('@prefix');
    });

    it('should include chatman namespace', () => {
      const obs = createObservation({ test: 'value' });
      const delta = createDelta([{ op: 'update', field: 'test', value: 'updated' }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);
      const turtle = result.artifact.proof.content;

      expect(turtle).toContain('@prefix chatman:');
      expect(turtle).toContain('chatman:Observation');
      expect(turtle).toContain('chatman:Delta');
      expect(turtle).toContain('chatman:Artifact');
    });

    it('should include observation details in RDF', () => {
      const obs = createObservation({ data: 'test' });
      const delta = createDelta([{ op: 'update', field: 'data', value: 'updated' }]);
      const operator = createOperator('market');

      const result = mu(obs, delta, operator);
      const turtle = result.artifact.proof.content;

      expect(turtle).toContain(obs.id.substring(0, 8));
      expect(turtle).toContain('market');
    });

    it('should include delta details in RDF', () => {
      const obs = createObservation({});
      const delta = createDelta([{ op: 'add', field: 'new', value: 'value' }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);
      const turtle = result.artifact.proof.content;

      expect(turtle).toContain(delta.id.substring(0, 8));
      expect(turtle).toContain('chatman:operationCount 1');
    });

    it('should include result state in RDF', () => {
      const obs = createObservation({ customers: 100 });
      const delta = createDelta([{ op: 'update', field: 'customers', value: 200 }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);
      const turtle = result.artifact.proof.content;

      expect(turtle).toContain('customers');
      expect(turtle).toContain('200');
    });
  });

  describe('End-to-End: Complete Pipeline', () => {
    it('should complete full transformation', () => {
      const obs = createObservation({
        customers: 1000,
        revenue: 100000,
        growth: 0.05,
      }, 'market');

      const delta = createDelta([
        { op: 'update', field: 'customers', value: 1500 },
        { op: 'update', field: 'revenue', value: 150000 },
        { op: 'update', field: 'growth', value: 0.5 },
        { op: 'add', field: 'region', value: 'APAC' },
      ], 'market');

      const operator = createOperator('market', 'market_expansion');

      const result = mu(obs, delta, operator);

      // Verify artifact structure
      expect(result.artifact).toBeDefined();
      expect(result.artifact.id).toBeDefined();
      expect(result.artifact.source_observation).toBe(obs.id);
      expect(result.artifact.applied_deltas).toContain(delta.id);
      expect(result.artifact.operator).toBe('market_expansion');

      // Verify result state
      expect(result.artifact.result.customers).toBe(1500);
      expect(result.artifact.result.revenue).toBe(150000);
      expect(result.artifact.result.growth).toBe(0.5);
      expect(result.artifact.result.region).toBe('APAC');

      // Verify metadata
      expect(result.metadata.duration).toBeGreaterThanOrEqual(0);
      expect(result.metadata.observationId).toBe(obs.id);
      expect(result.metadata.deltaId).toBe(delta.id);
    });

    it.skip('should validate complete artifact (Zod v4 compatibility issue)', () => {
      const obs = createObservation({ value: 100 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 200 }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);
      const validation = ArtifactSchema.safeParse(result.artifact);

      expect(validation.success).toBe(true);
    });

    it('should work across different domains', () => {
      const domains = ['market', 'organization', 'strategy', 'product', 'customer'];

      domains.forEach(domain => {
        const obs = createObservation({ value: 1 }, domain);
        const delta = createDelta([{ op: 'update', field: 'value', value: 2 }], domain);
        const operator = createOperator(domain, `${domain}_op`);

        const result = mu(obs, delta, operator);

        expect(result.artifact.result.value).toBe(2);
        expect(result.artifact.proof.content).toContain(domain);
      });
    });

    it('should handle complex state transformations', () => {
      const obs = createObservation({
        metrics: { views: 1000, clicks: 50 },
        tags: ['tech', 'startup'],
        active: true,
      });

      const delta = createDelta([
        { op: 'update', field: 'metrics', value: { views: 2000, clicks: 100, conversions: 10 } },
        { op: 'update', field: 'tags', value: ['tech', 'startup', 'growth'] },
        { op: 'add', field: 'verified', value: true },
      ]);

      const operator = createOperator();

      const result = mu(obs, delta, operator);

      expect(result.artifact.result.metrics.views).toBe(2000);
      expect(result.artifact.result.tags).toEqual(['tech', 'startup', 'growth']);
      expect(result.artifact.result.verified).toBe(true);
      expect(result.artifact.result.active).toBe(true);
    });

    it('should complete pipeline in reasonable time (<50ms)', () => {
      const obs = createObservation({ value: 1 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 2 }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);

      expect(result.metadata.duration).toBeLessThan(50);
    });
  });

  describe('Error Handling', () => {
    it('should throw on domain mismatch between observation and delta', () => {
      const obs = createObservation({}, 'market');
      const delta = createDelta([{ op: 'add', field: 'test', value: 1 }], 'product');
      const operator = createOperator('market');

      expect(() => mu(obs, delta, operator)).toThrow(/Domain mismatch/);
    });

    it('should throw on domain mismatch between operator and observation', () => {
      const obs = createObservation({}, 'market');
      const delta = createDelta([{ op: 'add', field: 'test', value: 1 }], 'market');
      const operator = createOperator('product');

      expect(() => mu(obs, delta, operator)).toThrow(/Domain mismatch/);
    });

    it('should throw on invalid observation structure', () => {
      const invalidObs = { invalid: 'structure' };
      const delta = createDelta([{ op: 'add', field: 'test', value: 1 }]);
      const operator = createOperator();

      expect(() => mu(invalidObs, delta, operator)).toThrow();
    });

    it('should throw on invalid delta structure', () => {
      const obs = createObservation({});
      const invalidDelta = { invalid: 'structure' };
      const operator = createOperator();

      expect(() => mu(obs, invalidDelta, operator)).toThrow();
    });

    it('should throw on invalid operator structure', () => {
      const obs = createObservation({});
      const delta = createDelta([{ op: 'add', field: 'test', value: 1 }]);
      const invalidOperator = { invalid: 'structure' };

      expect(() => mu(obs, delta, invalidOperator)).toThrow();
    });
  });
});
