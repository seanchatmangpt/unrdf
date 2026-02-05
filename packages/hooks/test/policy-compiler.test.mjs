/**
 * Policy Compiler Tests - FAST SUITE MINIMAL
 * Essential tests only: ALLOW_ALL, hook compilation, error handling
 * REMOVED: DENY_ALL, caching, statistics, complex patterns
 */
import { describe, it, expect, beforeEach } from 'vitest';
import {
  PolicyPatterns,
  compilePolicy,
  compileHook,
  executeCompiledHook,
  clearPolicyCache,
} from '../src/policy-compiler.mjs';

describe('Policy Compilation - Fast Suite', () => {
  beforeEach(() => {
    clearPolicyCache();
  });

  it('should compile ALLOW_ALL policy', () => {
    const policy = { type: PolicyPatterns.ALLOW_ALL };
    const compiled = compilePolicy(policy);
    const quad = { subject: { value: 'test' } };
    expect(compiled(quad)).toBe(true);
  });
});

describe('Hook Compilation - Fast Suite', () => {
  beforeEach(() => {
    clearPolicyCache();
  });

  it('should compile and execute hook', () => {
    const hook = {
      name: 'test-hook',
      validate: (quad) => quad.subject.value === 'valid',
    };
    const quad1 = { subject: { value: 'valid' } };
    const quad2 = { subject: { value: 'invalid' } };

    const result1 = executeCompiledHook(hook, quad1);
    const result2 = executeCompiledHook(hook, quad2);

    expect(result1.valid).toBe(true);
    expect(result2.valid).toBe(false);
  });

  it('should handle validation errors gracefully', () => {
    const hook = {
      name: 'test-hook',
      validate: () => { throw new Error('Test error'); },
    };
    const quad = { subject: { value: 'test' } };
    const result = executeCompiledHook(hook, quad);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('Test error');
  });
});
