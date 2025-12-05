import { describe, test, expect } from 'vitest';
import { HookRegistry } from '../../src/core/patterns/hook-registry.mjs';

describe('HookRegistry - Pattern Tests', () => {
  test('registers and validates a simple hook', () => {
    const registry = new HookRegistry();

    registry.register('status', {
      validate: (value) => {
        const allowed = ['active', 'inactive'];
        return allowed.includes(value)
          ? { valid: true }
          : { valid: false, reason: 'Invalid status' };
      },
    });

    expect(registry.validate('status', 'active')).toEqual({ valid: true });
    expect(registry.validate('status', 'invalid')).toMatchObject({
      valid: false,
      reason: 'Invalid status',
    });
  });

  test('validates numeric constraints', () => {
    const registry = new HookRegistry();

    registry.register('budget', {
      validate: (value) => {
        const budget = parseInt(value, 10);
        if (isNaN(budget)) return { valid: false, reason: 'Must be a number' };
        if (budget < 0) return { valid: false, reason: 'Cannot be negative' };
        if (budget > 100000) return { valid: false, reason: 'Exceeds limit' };
        return { valid: true };
      },
    });

    expect(registry.validate('budget', '50000')).toEqual({ valid: true });
    expect(registry.validate('budget', '150000')).toMatchObject({
      valid: false,
      reason: 'Exceeds limit',
    });
    expect(registry.validate('budget', 'not-a-number')).toMatchObject({
      valid: false,
      reason: 'Must be a number',
    });
  });

  test('returns valid=true for unregistered fields', () => {
    const registry = new HookRegistry();

    // No hook registered for 'unknown'
    expect(registry.validate('unknown', 'any-value')).toEqual({ valid: true });
  });

  test('lists registered fields', () => {
    const registry = new HookRegistry();

    registry.register('field1', { validate: () => ({ valid: true }) });
    registry.register('field2', { validate: () => ({ valid: true }) });

    const fields = registry.listFields();
    expect(fields).toContain('field1');
    expect(fields).toContain('field2');
    expect(fields.length).toBe(2);
  });

  test('unregisters hooks', () => {
    const registry = new HookRegistry();

    registry.register('field1', { validate: () => ({ valid: true }) });
    expect(registry.get('field1')).toBeTruthy();

    registry.unregister('field1');
    expect(registry.get('field1')).toBeNull();
  });

  test('validates batch of values', () => {
    const registry = new HookRegistry();

    registry.register('name', {
      validate: (value) => {
        if (!value || value.trim().length === 0) {
          return { valid: false, reason: 'Cannot be empty' };
        }
        return { valid: true };
      },
    });

    registry.register('age', {
      validate: (value) => {
        const age = parseInt(value, 10);
        if (age < 18) return { valid: false, reason: 'Must be 18+' };
        return { valid: true };
      },
    });

    const result = registry.validateBatch({
      name: 'John',
      age: '25',
    });

    expect(result.valid).toBe(true);
    expect(result.errors).toBeUndefined();
  });

  test('returns errors on batch validation failure', () => {
    const registry = new HookRegistry();

    registry.register('name', {
      validate: (value) => {
        return value.length > 0 ? { valid: true } : { valid: false, reason: 'Empty' };
      },
    });

    registry.register('age', {
      validate: (value) => {
        const age = parseInt(value, 10);
        return age >= 18 ? { valid: true } : { valid: false, reason: 'Too young' };
      },
    });

    const result = registry.validateBatch({
      name: '',
      age: '15',
    });

    expect(result.valid).toBe(false);
    expect(result.errors).toEqual({
      name: 'Empty',
      age: 'Too young',
    });
  });

  test('throws error if hook missing validate function', () => {
    const registry = new HookRegistry();

    expect(() => {
      registry.register('field', {}); // Missing validate function
    }).toThrow('must have a validate function');
  });

  test('creates reusable batch validator', () => {
    const registry = new HookRegistry();

    registry.register('status', {
      validate: (value) => {
        const allowed = ['on', 'off'];
        return allowed.includes(value)
          ? { valid: true }
          : { valid: false, reason: 'Invalid' };
      },
    });

    const validator = registry.createValidator();

    expect(validator({ status: 'on' }).valid).toBe(true);
    expect(validator({ status: 'maybe' }).valid).toBe(false);
  });

  test('handles validation errors gracefully', () => {
    const registry = new HookRegistry();

    registry.register('field', {
      validate: (value) => {
        throw new Error('Unexpected validation error');
      },
    });

    const result = registry.validate('field', 'anything');

    expect(result.valid).toBe(false);
    expect(result.reason).toContain('Unexpected validation error');
  });
});
