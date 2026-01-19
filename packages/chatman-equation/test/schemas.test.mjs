/**
 * @file Chatman Equation Schema Tests
 * @module @unrdf/chatman-equation/test/schemas
 */

import { describe, it, expect } from 'vitest';
import {
  ObservationSchema,
  DeltaSchema,
  ClosureOperatorSchema,
  ArtifactSchema,
  UnificationMappingSchema,
  validateObservation,
  validateDelta,
  validateClosureOperator,
  validateArtifact,
  safeValidate,
} from '../src/schemas.mjs';

describe('ObservationSchema', () => {
  it('should validate valid observation', () => {
    // Arrange
    const observation = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      domain: 'market',
      state: {
        supply: 10000,
        demand: 12000,
        price: 100.0,
      },
    };

    // Act
    const result = validateObservation(observation);

    // Assert
    expect(result).toEqual(observation);
  });

  it('should reject observation with invalid domain', () => {
    // Arrange
    const observation = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      domain: 'invalid',
      state: {},
    };

    // Act & Assert
    expect(() => validateObservation(observation)).toThrow();
  });

  it('should reject observation without required fields', () => {
    // Arrange
    const observation = {
      id: crypto.randomUUID(),
      domain: 'market',
    };

    // Act & Assert
    expect(() => validateObservation(observation)).toThrow();
  });
});

describe('DeltaSchema', () => {
  it('should validate valid delta', () => {
    // Arrange
    const delta = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      domain: 'market',
      operations: [
        {
          op: 'update',
          field: 'price',
          value: 95.0,
          reason: 'Market adjustment',
        },
      ],
    };

    // Act
    const result = validateDelta(delta);

    // Assert
    expect(result).toEqual(delta);
  });

  it('should reject delta without operations', () => {
    // Arrange
    const delta = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      domain: 'market',
      operations: [],
    };

    // Act & Assert
    expect(() => validateDelta(delta)).toThrow();
  });

  it('should validate all operation types', () => {
    // Arrange
    const delta = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      domain: 'product',
      operations: [
        { op: 'add', field: 'feature', value: 'dark_mode' },
        { op: 'update', field: 'version', value: '2.0.0' },
        { op: 'delete', field: 'legacy_api', value: true },
      ],
    };

    // Act
    const result = validateDelta(delta);

    // Assert
    expect(result.operations).toHaveLength(3);
  });
});

describe('ClosureOperatorSchema', () => {
  it('should validate valid closure operator', () => {
    // Arrange
    const operator = {
      type: 'reconcile',
      name: 'market_equilibrium',
      domain: 'market',
      conflict_resolution: 'delta_wins',
    };

    // Act
    const result = validateClosureOperator(operator);

    // Assert
    expect(result).toEqual(operator);
  });

  it('should apply default conflict resolution', () => {
    // Arrange
    const operator = {
      type: 'merge',
      name: 'simple_merge',
      domain: 'organization',
    };

    // Act
    const result = validateClosureOperator(operator);

    // Assert
    expect(result.conflict_resolution).toBe('delta_wins');
  });

  it('should validate all operator types', () => {
    // Arrange
    const types = ['merge', 'transform', 'reconcile', 'compose'];

    // Act & Assert
    for (const type of types) {
      const operator = {
        type,
        name: `${type}_operator`,
        domain: 'strategy',
      };
      expect(() => validateClosureOperator(operator)).not.toThrow();
    }
  });
});

describe('ArtifactSchema', () => {
  it('should validate valid artifact', () => {
    // Arrange
    const artifact = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      source_observation: crypto.randomUUID(),
      applied_deltas: [crypto.randomUUID()],
      operator: 'market_equilibrium',
      result: {
        equilibrium_price: 95.0,
        market_cleared: true,
      },
    };

    // Act
    const result = validateArtifact(artifact);

    // Assert
    expect(result).toEqual(artifact);
  });

  it('should reject artifact without applied deltas', () => {
    // Arrange
    const artifact = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      source_observation: crypto.randomUUID(),
      applied_deltas: [],
      operator: 'test',
      result: {},
    };

    // Act & Assert
    expect(() => validateArtifact(artifact)).toThrow();
  });

  it('should allow optional proof and receipt', () => {
    // Arrange
    const artifact = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      source_observation: crypto.randomUUID(),
      applied_deltas: [crypto.randomUUID()],
      operator: 'test',
      result: {},
      proof: { hash: 'abc123', signature: 'xyz789' },
      receipt: { status: 'success' },
    };

    // Act
    const result = validateArtifact(artifact);

    // Assert
    expect(result.proof).toBeDefined();
    expect(result.receipt).toBeDefined();
  });
});

describe('UnificationMappingSchema', () => {
  it('should validate valid unification mapping', () => {
    // Arrange
    const mapping = {
      description: 'Market dynamics mapping',
      observation_fields: ['supply', 'demand', 'price'],
      delta_fields: ['supply_change', 'demand_change'],
      artifact_fields: ['equilibrium_price', 'market_cleared'],
      closure_operator: 'market_equilibrium',
      invariants: {
        supply_positive: 'supply >= 0',
        demand_positive: 'demand >= 0',
      },
    };

    // Act
    const result = safeValidate(UnificationMappingSchema, mapping);

    // Assert
    expect(result.success).toBe(true);
    expect(result.data).toEqual(mapping);
  });

  it('should reject mapping without required fields', () => {
    // Arrange
    const mapping = {
      description: 'Incomplete mapping',
      observation_fields: ['field1'],
    };

    // Act
    const result = safeValidate(UnificationMappingSchema, mapping);

    // Assert
    expect(result.success).toBe(false);
  });
});

describe('Safe Validation', () => {
  it('should return success result for valid data', () => {
    // Arrange
    const observation = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      domain: 'customer',
      state: { nps: 8 },
    };

    // Act
    const result = safeValidate(ObservationSchema, observation);

    // Assert
    expect(result.success).toBe(true);
    expect(result.data).toEqual(observation);
  });

  it('should return error result for invalid data', () => {
    // Arrange
    const invalidData = {
      id: 'not-a-uuid',
      domain: 'invalid',
    };

    // Act
    const result = safeValidate(ObservationSchema, invalidData);

    // Assert
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });
});
