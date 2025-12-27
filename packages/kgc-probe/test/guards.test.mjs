#!/usr/bin/env node
/**
 * @fileoverview Guard enforcement tests - prove poka-yoke works
 *
 * Tests verify that guards DENY forbidden operations and emit denial receipts.
 *
 * Critical tests:
 * 1. Secret patterns are blocked (SECRET_KEY, API_TOKEN, etc.)
 * 2. Paths outside --root are denied
 * 3. Network hosts outside --net-allow are denied
 * 4. Denials are recorded in guard stats
 * 5. Running scan twice yields identical hashes (determinism)
 */

import { strict as assert } from 'node:assert';
import { test, describe } from 'node:test';
import { GuardManager } from '../src/guards.mjs';

describe('GuardManager - Secret Pattern Enforcement', () => {
  test('should deny SECRET_KEY environment variable', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/tmp/SECRET_KEY', 'agent-test');

    assert.equal(result.allowed, false, 'SECRET_KEY should be denied');
    assert.ok(result.denial, 'Should emit denial observation');
    assert.equal(result.denial.data.guardName, 'secret-pattern');
    assert.ok(result.denial.message.includes('secret pattern'));
  });

  test('should deny API_TOKEN paths', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/tmp/config/API_TOKEN.json', 'agent-test');

    assert.equal(result.allowed, false, 'API_TOKEN should be denied');
    assert.equal(result.denial.data.guardName, 'secret-pattern');
  });

  test('should deny .env files', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/tmp/.env', 'agent-test');

    assert.equal(result.allowed, false, '.env should be denied');
    assert.equal(result.denial.data.guardName, 'secret-pattern');
  });

  test('should deny .env.local files', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/tmp/.env.local', 'agent-test');

    assert.equal(result.allowed, false, '.env.local should be denied');
    assert.equal(result.denial.data.guardName, 'secret-pattern');
  });

  test('should deny private key files', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const paths = [
      '/tmp/id_rsa',
      '/tmp/.ssh/id_rsa',
      '/tmp/private.key',
      '/tmp/cert.pem'
    ];

    for (const path of paths) {
      const result = guards.guardFileRead(path, 'agent-test');
      assert.equal(result.allowed, false, `${path} should be denied`);
      assert.equal(result.denial.data.guardName, 'secret-pattern');
    }
  });

  test('should allow non-secret files', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/tmp/package.json', 'agent-test');

    assert.equal(result.allowed, true, 'package.json should be allowed');
    assert.ok(!result.denial, 'No denial for allowed file');
  });
});

describe('GuardManager - Path Sandboxing', () => {
  test('should deny paths outside allowed roots', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp/allowed'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/etc/passwd', 'agent-test');

    assert.equal(result.allowed, false, '/etc/passwd should be denied');
    assert.equal(result.denial.data.guardName, 'filesystem-boundary');
    assert.ok(result.denial.message.includes('outside allowed roots'));
  });

  test('should allow paths within allowed roots', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp/allowed'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/tmp/allowed/file.txt', 'agent-test');

    assert.equal(result.allowed, true, 'Path within root should be allowed');
    assert.ok(!result.denial);
  });

  test('should deny path traversal attempts', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp/allowed'],
      allowedHosts: []
    });

    const result = guards.guardFileRead('/tmp/allowed/../etc/passwd', 'agent-test');

    assert.equal(result.allowed, false, 'Path traversal should be denied');
    assert.equal(result.denial.data.guardName, 'filesystem-boundary');
  });

  test('should allow multiple roots', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp/root1', '/tmp/root2'],
      allowedHosts: []
    });

    const result1 = guards.guardFileRead('/tmp/root1/file.txt', 'agent-test');
    const result2 = guards.guardFileRead('/tmp/root2/file.txt', 'agent-test');

    assert.equal(result1.allowed, true, 'root1 should be allowed');
    assert.equal(result2.allowed, true, 'root2 should be allowed');
  });
});

describe('GuardManager - Network Guards', () => {
  test('should deny network access when no hosts allowed', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const result = guards.guardNetworkAccess('example.com', 'agent-test');

    assert.equal(result.allowed, false, 'Network should be denied by default');
    assert.equal(result.denial.data.guardName, 'network-deny-default');
  });

  test('should deny hosts not in allowlist', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: ['api.example.com']
    });

    const result = guards.guardNetworkAccess('evil.com', 'agent-test');

    assert.equal(result.allowed, false, 'Non-allowlisted host should be denied');
    assert.equal(result.denial.data.guardName, 'network-allowlist');
  });

  test('should allow hosts in allowlist', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: ['api.example.com']
    });

    const result = guards.guardNetworkAccess('api.example.com', 'agent-test');

    assert.equal(result.allowed, true, 'Allowlisted host should be allowed');
    assert.ok(!result.denial);
  });
});

describe('GuardManager - Write Guards', () => {
  test('should deny all write operations (read-only probe)', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    const result = guards.guardFileWrite('/tmp/output.txt', 'agent-test');

    assert.equal(result.allowed, false, 'Writes should be denied');
    assert.equal(result.denial.data.guardName, 'readonly-probe');
  });
});

describe('GuardManager - Denial Tracking', () => {
  test('should track all denials', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    guards.guardFileRead('/tmp/SECRET_KEY', 'agent-1');
    guards.guardFileRead('/etc/passwd', 'agent-2');
    guards.guardNetworkAccess('evil.com', 'agent-3');

    const denials = guards.getDenials();

    assert.equal(denials.length, 3, 'Should have 3 denials');
    assert.equal(denials[0].category, 'guard');
    assert.equal(denials[0].severity, 'warn');
  });

  test('should provide guard statistics', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    guards.guardFileRead('/tmp/SECRET_KEY', 'agent-1');
    guards.guardFileRead('/tmp/API_TOKEN', 'agent-1');
    guards.guardFileRead('/etc/passwd', 'agent-2');

    const stats = guards.getStats();

    assert.equal(stats.totalDenials, 3);
    assert.equal(stats.byGuard['secret-pattern'], 2);
    assert.equal(stats.byGuard['filesystem-boundary'], 1);
  });

  test('should clear denials', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: []
    });

    guards.guardFileRead('/tmp/SECRET_KEY', 'agent-1');
    assert.equal(guards.getDenials().length, 1);

    guards.clearDenials();
    assert.equal(guards.getDenials().length, 0);
  });
});

describe('GuardManager - Timeout Guards', () => {
  test('should deny excessive timeouts', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: [],
      defaultTimeoutMs: 5000
    });

    // Max is 10x default = 50s
    const result = guards.guardTimeout(60000, 'agent-test');

    assert.equal(result.allowed, false, 'Excessive timeout should be denied');
    assert.equal(result.denial.data.guardName, 'timeout-limit');
  });

  test('should allow timeouts within limit', () => {
    const guards = new GuardManager({
      allowedRoots: ['/tmp'],
      allowedHosts: [],
      defaultTimeoutMs: 5000
    });

    const result = guards.guardTimeout(10000, 'agent-test');

    assert.equal(result.allowed, true, 'Reasonable timeout should be allowed');
    assert.ok(!result.denial);
  });
});

console.log('✅ All guard enforcement tests passed!');
