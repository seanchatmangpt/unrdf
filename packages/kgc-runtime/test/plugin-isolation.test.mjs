/**
 * Plugin Isolation Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { PluginIsolation, createPublicAPI } from '../src/plugin-isolation.mjs';

describe('PluginIsolation', () => {
  let isolation;

  beforeEach(() => {
    isolation = new PluginIsolation();
  });

  // Test 11: Capability Checking
  it('should allow whitelisted capabilities', () => {
    const allowed = isolation.checkCapability('receipt:generate');
    expect(allowed).toBe(true);
  });

  it('should block non-whitelisted capabilities in strict mode', () => {
    const denied = isolation.checkCapability('filesystem:write');
    expect(denied).toBe(false);
  });

  it('should always block dangerous capabilities', () => {
    isolation.grantCapability('custom:action'); // This works

    // But blocked capabilities are never allowed
    expect(() => {
      isolation.grantCapability('process:spawn');
    }).toThrow(/Cannot grant blocked capability/);
  });

  // Test 12: Isolated Execution
  it('should execute functions with required capabilities', async () => {
    const result = await isolation.executeIsolated(
      async () => {
        return { data: 42 };
      },
      ['receipt:generate']
    );

    expect(result.data).toBe(42);
  });

  it('should reject execution if capabilities are denied', async () => {
    await expect(
      isolation.executeIsolated(
        async () => {
          return { data: 42 };
        },
        ['filesystem:write']
      )
    ).rejects.toThrow(/Capability denied/);
  });

  // Test 13: Access Logging
  it('should log all capability access attempts', () => {
    isolation.checkCapability('receipt:generate');
    isolation.checkCapability('filesystem:write');

    const accessLog = isolation.getAccessLog();

    expect(accessLog.length).toBeGreaterThanOrEqual(2);
    expect(accessLog.some(entry => entry.capability === 'receipt:generate')).toBe(true);
    expect(accessLog.some(entry => entry.capability === 'filesystem:write')).toBe(true);
  });

  it('should track usage statistics', () => {
    isolation.checkCapability('receipt:generate');
    isolation.checkCapability('receipt:generate');
    isolation.checkCapability('receipt:validate');

    const stats = isolation.getUsageStats();

    expect(stats['receipt:generate']).toBe(2);
    expect(stats['receipt:validate']).toBe(1);
  });

  // Test 14: Plugin Capability Validation
  it('should validate plugin manifest capabilities', () => {
    const validation = isolation.validatePluginCapabilities([
      'receipt:generate',
      'custom:action',
      'filesystem:write',
    ]);

    expect(validation.allowed).toContain('receipt:generate');
    expect(validation.denied).toContain('custom:action');
    expect(validation.blocked).toContain('filesystem:write');
  });
});

describe('createPublicAPI', () => {
  it('should create proxy with only whitelisted methods', () => {
    const fullAPI = {
      publicMethod: () => 'public',
      privateMethod: () => 'private',
      data: { value: 42 },
    };

    const publicAPI = createPublicAPI(fullAPI, ['publicMethod', 'data']);

    expect(publicAPI.publicMethod()).toBe('public');
    expect(publicAPI.privateMethod).toBeUndefined();
    expect(publicAPI.data.value).toBe(42);
  });
});
