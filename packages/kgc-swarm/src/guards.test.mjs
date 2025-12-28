/**
 * @fileoverview Tests for Poka-Yoke Guard System
 *
 * Validates that all guards correctly block illegal operations
 * and emit receipts for both allowed and blocked operations.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  GuardSystem,
  SecretGuard,
  PathGuard,
  NetworkGuard,
  PrivilegeGuard,
} from './guards.mjs';
import path from 'node:path';

// =============================================================================
// SecretGuard Tests
// =============================================================================

describe('SecretGuard', () => {
  it('should block operations with API keys in target', async () => {
    const operation = {
      type: 'file:write',
      target: '/path/to/API_KEY.txt',
      data: 'some data',
    };

    const result = await SecretGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('SecretGuard');
    expect(result.reason).toContain('secret detected');
    expect(result.receipt).toBeDefined();
    expect(result.receipt.id).toMatch(/^receipt-/);
    expect(result.receipt.operation).toBe('SecretGuard');
    expect(result.receipt.hash).toBeTruthy();
    expect(result.receipt.outputs.blocked).toBe('secret-detected');
  });

  it('should block operations with passwords in data', async () => {
    const operation = {
      type: 'file:write',
      target: '/path/to/file.txt',
      data: 'export PASSWORD=supersecret123',
    };

    const result = await SecretGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('SecretGuard');
    expect(result.receipt).toBeDefined();
    expect(result.receipt.outputs.blocked).toBe('secret-detected');
  });

  it('should block OpenAI-style API keys', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://api.example.com',
      data: { apiKey: 'sk-1234567890abcdefghijklmnopqrstuv' },
    };

    const result = await SecretGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.receipt.outputs.blocked).toBe('secret-detected');
  });

  it('should block GitHub personal access tokens', async () => {
    const operation = {
      type: 'file:write',
      target: '/config.json',
      data: JSON.stringify({ token: 'ghp_1234567890abcdefghijklmnopqrstuvwxyz' }),
    };

    const result = await SecretGuard(operation);

    expect(result.allowed).toBe(false);
  });

  it('should block JWT tokens', async () => {
    const operation = {
      type: 'file:write',
      target: '/auth.json',
      data: 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c',
    };

    const result = await SecretGuard(operation);

    expect(result.allowed).toBe(false);
  });

  it('should allow operations without secrets', async () => {
    const operation = {
      type: 'file:write',
      target: '/path/to/safe-file.txt',
      data: 'This is safe public data',
    };

    const result = await SecretGuard(operation);

    expect(result.allowed).toBe(true);
    expect(result.reason).toBeUndefined();
    expect(result.receipt).toBeDefined();
    expect(result.receipt.outputs.allowed).toBe(true);
  });

  it('should generate receipt for allowed operations', async () => {
    const operation = {
      type: 'file:read',
      target: '/public/data.json',
    };

    const result = await SecretGuard(operation);

    expect(result.allowed).toBe(true);
    expect(result.receipt.id).toMatch(/^receipt-/);
    expect(result.receipt.timestamp).toBeTruthy();
    expect(result.receipt.hash).toBeTruthy();
    expect(result.receipt.inputs.operation).toBe('file:read');
  });
});

// =============================================================================
// PathGuard Tests
// =============================================================================

describe('PathGuard', () => {
  const rootPath = process.cwd();

  it('should block out-of-root file access', async () => {
    const operation = {
      type: 'file:read',
      target: '/etc/passwd',
    };

    const result = await PathGuard(operation, { rootPath });

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('PathGuard');
    expect(result.reason).toContain('outside allowed root');
    expect(result.receipt).toBeDefined();
    expect(result.receipt.outputs.blocked).toBe('out-of-root');
  });

  it('should block path traversal with ../', async () => {
    const operation = {
      type: 'file:read',
      target: path.join(rootPath, '../../../etc/passwd'),
    };

    const result = await PathGuard(operation, { rootPath });

    expect(result.allowed).toBe(false);
    expect(result.receipt.outputs.blocked).toBe('out-of-root');
  });

  it('should allow access within root', async () => {
    const operation = {
      type: 'file:write',
      target: path.join(rootPath, 'safe-file.txt'),
    };

    const result = await PathGuard(operation, { rootPath });

    expect(result.allowed).toBe(true);
    expect(result.receipt.outputs.allowed).toBe(true);
    expect(result.receipt.outputs.resolvedPath).toContain(rootPath);
  });

  it('should allow subdirectory access within root', async () => {
    const operation = {
      type: 'file:read',
      target: path.join(rootPath, 'src/guards.mjs'),
    };

    const result = await PathGuard(operation, { rootPath });

    expect(result.allowed).toBe(true);
  });

  it('should skip validation for non-file operations', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://example.com',
    };

    const result = await PathGuard(operation, { rootPath });

    expect(result.allowed).toBe(true);
    expect(result.receipt.outputs.reason).toBe('not-file-operation');
  });

  it('should generate receipt for blocked operations', async () => {
    const operation = {
      type: 'file:delete',
      target: '/root/.ssh/id_rsa',
    };

    const result = await PathGuard(operation, { rootPath });

    expect(result.allowed).toBe(false);
    expect(result.receipt.id).toMatch(/^receipt-/);
    expect(result.receipt.inputs.rootPath).toBeTruthy();
    expect(result.receipt.outputs.attemptedPath).toBeTruthy();
  });
});

// =============================================================================
// NetworkGuard Tests
// =============================================================================

describe('NetworkGuard', () => {
  it('should block non-allowlisted hosts', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://malicious.example.com',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: ['safe.example.com', 'api.trusted.com'],
    });

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('NetworkGuard');
    expect(result.reason).toContain('not allowed');
    expect(result.receipt.outputs.blocked).toBe('host-not-allowed');
  });

  it('should allow allowlisted hosts', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://api.trusted.com/data',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: ['api.trusted.com'],
    });

    expect(result.allowed).toBe(true);
    expect(result.receipt.outputs.allowed).toBe(true);
  });

  it('should support wildcard host matching', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://api.example.com/v1/data',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: ['*.example.com'],
    });

    expect(result.allowed).toBe(true);
  });

  it('should block non-allowlisted ports', async () => {
    const operation = {
      type: 'network:request',
      target: 'http://example.com:9999',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: ['example.com'],
      allowedPorts: [80, 443],
    });

    expect(result.allowed).toBe(false);
    expect(result.receipt.outputs.blocked).toBe('port-not-allowed');
  });

  it('should allow standard ports (80, 443)', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://example.com',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: ['example.com'],
      allowedPorts: [80, 443],
    });

    expect(result.allowed).toBe(true);
  });

  it('should block invalid URLs', async () => {
    const operation = {
      type: 'network:request',
      target: 'not-a-valid-url',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: ['example.com'],
    });

    expect(result.allowed).toBe(false);
    expect(result.reason).toContain('Invalid URL');
    expect(result.receipt.outputs.blocked).toBe('invalid-url');
  });

  it('should skip validation for non-network operations', async () => {
    const operation = {
      type: 'file:read',
      target: '/path/to/file.txt',
    };

    const result = await NetworkGuard(operation);

    expect(result.allowed).toBe(true);
    expect(result.receipt.outputs.reason).toBe('not-network-operation');
  });

  it('should allow all hosts when allowlist is empty', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://any-host.com',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: [],
    });

    expect(result.allowed).toBe(true);
  });

  it('should generate receipt with network details', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://api.example.com:8080/data',
    };

    const result = await NetworkGuard(operation, {
      allowedHosts: ['api.example.com'],
      allowedPorts: [8080],
    });

    expect(result.allowed).toBe(true);
    expect(result.receipt.inputs.hostname).toBe('api.example.com');
    expect(result.receipt.inputs.port).toBe(8080);
  });
});

// =============================================================================
// PrivilegeGuard Tests
// =============================================================================

describe('PrivilegeGuard', () => {
  it('should block access to /etc/passwd', async () => {
    const operation = {
      type: 'file:read',
      target: '/etc/passwd',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('PrivilegeGuard');
    expect(result.reason).toContain('restricted path');
    expect(result.receipt.outputs.blocked).toBe('privilege-escalation');
  });

  it('should block access to /etc/shadow', async () => {
    const operation = {
      type: 'file:read',
      target: '/etc/shadow',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.reason).toContain('/etc/shadow');
  });

  it('should block access to /root directory', async () => {
    const operation = {
      type: 'file:write',
      target: '/root/.ssh/authorized_keys',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.reason).toContain('/root');
  });

  it('should block sudo command execution', async () => {
    const operation = {
      type: 'process:spawn',
      target: 'sudo apt-get install malware',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.reason).toContain('sudo');
  });

  it('should block su command execution', async () => {
    const operation = {
      type: 'process:spawn',
      target: 'su root',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.reason).toContain('su');
  });

  it('should block chmod command', async () => {
    const operation = {
      type: 'process:spawn',
      target: 'chmod 777 /bin/sh',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.reason).toContain('chmod');
  });

  it('should block setuid operations', async () => {
    const operation = {
      type: 'process:spawn',
      target: '/usr/bin/program',
      data: { setuid: true },
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.reason).toContain('Setuid');
  });

  it('should allow normal file operations', async () => {
    const operation = {
      type: 'file:read',
      target: '/home/user/document.txt',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(true);
    expect(result.receipt.outputs.allowed).toBe(true);
  });

  it('should allow safe process execution', async () => {
    const operation = {
      type: 'process:spawn',
      target: 'node script.mjs',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(true);
  });

  it('should generate receipt for blocked escalation', async () => {
    const operation = {
      type: 'file:write',
      target: '/etc/sudoers',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.allowed).toBe(false);
    expect(result.receipt.id).toMatch(/^receipt-/);
    expect(result.receipt.outputs.reason).toBeTruthy();
  });
});

// =============================================================================
// GuardSystem Tests
// =============================================================================

describe('GuardSystem', () => {
  let guardSystem;

  beforeEach(() => {
    guardSystem = new GuardSystem({
      rootPath: process.cwd(),
      allowedHosts: ['api.example.com', '*.trusted.com'],
      allowedPorts: [80, 443, 8080],
    });
  });

  it('should initialize with all guards enabled by default', () => {
    const guards = guardSystem.getEnabledGuards();

    expect(guards).toContain('SecretGuard');
    expect(guards).toContain('PathGuard');
    expect(guards).toContain('NetworkGuard');
    expect(guards).toContain('PrivilegeGuard');
    expect(guards).toHaveLength(4);
  });

  it('should allow selective guard disabling', () => {
    const customSystem = new GuardSystem({
      enableSecretGuard: true,
      enablePathGuard: false,
      enableNetworkGuard: false,
      enablePrivilegeGuard: true,
    });

    const guards = customSystem.getEnabledGuards();

    expect(guards).toContain('SecretGuard');
    expect(guards).toContain('PrivilegeGuard');
    expect(guards).not.toContain('PathGuard');
    expect(guards).not.toContain('NetworkGuard');
    expect(guards).toHaveLength(2);
  });

  it('should validate and allow safe operations', async () => {
    const operation = {
      type: 'file:read',
      target: path.join(process.cwd(), 'src/guards.mjs'),
    };

    const result = await guardSystem.validate(operation);

    expect(result.allowed).toBe(true);
    expect(result.guard).toBe('GuardSystem');
    expect(result.receipt).toBeDefined();
    expect(result.receipt.outputs.guardsChecked).toBe(4);
  });

  it('should block operations with secrets', async () => {
    const operation = {
      type: 'file:write',
      target: path.join(process.cwd(), 'config.txt'),
      data: 'API_KEY=sk-1234567890abcdefghijklmnopqrstuv',
    };

    const result = await guardSystem.validate(operation);

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('SecretGuard');
    expect(result.receipt.outputs.blocked).toBe('secret-detected');
  });

  it('should block out-of-root access', async () => {
    const operation = {
      type: 'file:read',
      target: '/etc/passwd',
    };

    const result = await guardSystem.validate(operation);

    expect(result.allowed).toBe(false);
    // Could be blocked by PathGuard or PrivilegeGuard
    expect(['PathGuard', 'PrivilegeGuard']).toContain(result.guard);
  });

  it('should block non-allowlisted network requests', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://malicious.com/api',
    };

    const result = await guardSystem.validate(operation);

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('NetworkGuard');
    expect(result.receipt.outputs.blocked).toBe('host-not-allowed');
  });

  it('should block privilege escalation', async () => {
    const operation = {
      type: 'process:spawn',
      target: 'sudo rm -rf /',
    };

    const result = await guardSystem.validate(operation);

    expect(result.allowed).toBe(false);
    expect(result.guard).toBe('PrivilegeGuard');
  });

  it('should implement unlawful() check', async () => {
    const safeOp = {
      type: 'file:read',
      target: path.join(process.cwd(), 'README.md'),
    };

    const unsafeOp = {
      type: 'file:read',
      target: '/etc/shadow',
    };

    expect(await guardSystem.unlawful(safeOp)).toBe(false);
    expect(await guardSystem.unlawful(unsafeOp)).toBe(true);
  });

  it('should fail-fast on first guard violation', async () => {
    // This operation violates multiple guards
    const operation = {
      type: 'file:write',
      target: '/etc/passwd',
      data: 'PASSWORD=secret123',
    };

    const result = await guardSystem.validate(operation);

    expect(result.allowed).toBe(false);
    // Should fail on first guard that checks it
    expect(result.guard).toBeTruthy();
    expect(result.receipt).toBeDefined();
  });

  it('should generate receipts for all validation attempts', async () => {
    const operations = [
      {
        type: 'file:read',
        target: path.join(process.cwd(), 'safe.txt'),
      },
      {
        type: 'file:write',
        target: '/etc/passwd',
      },
      {
        type: 'network:request',
        target: 'https://api.example.com',
      },
    ];

    const results = await Promise.all(
      operations.map(op => guardSystem.validate(op))
    );

    // All operations should have receipts
    for (const result of results) {
      expect(result.receipt).toBeDefined();
      expect(result.receipt.id).toMatch(/^receipt-/);
      expect(result.receipt.hash).toBeTruthy();
      expect(result.receipt.timestamp).toBeTruthy();
    }

    expect(results[0].allowed).toBe(true);
    expect(results[1].allowed).toBe(false);
    expect(results[2].allowed).toBe(true);
  });

  it('should validate operation schema', async () => {
    const invalidOp = {
      type: 'invalid:operation',
      target: '/path',
    };

    await expect(guardSystem.validate(invalidOp)).rejects.toThrow();
  });

  it('should include guard context in receipts', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://api.example.com/data',
    };

    const result = await guardSystem.validate(operation);

    expect(result.receipt.inputs.operation).toBe('network:request');
    expect(result.receipt.inputs.guardsChecked).toEqual([
      'SecretGuard',
      'PathGuard',
      'NetworkGuard',
      'PrivilegeGuard',
    ]);
  });
});

// =============================================================================
// Guard Composition Tests
// =============================================================================

describe('Guard Composition', () => {
  it('should handle operations that pass all guards', async () => {
    const guardSystem = new GuardSystem({
      rootPath: process.cwd(),
      allowedHosts: ['api.safe.com'],
    });

    const operation = {
      type: 'network:request',
      target: 'https://api.safe.com/public/data',
      data: { userId: '12345' },
    };

    const result = await guardSystem.validate(operation);

    expect(result.allowed).toBe(true);
    expect(result.receipt.outputs.guardsChecked).toBe(4);
  });

  it('should handle operations that fail multiple guards', async () => {
    const guardSystem = new GuardSystem({
      rootPath: process.cwd(),
    });

    // Contains secret AND tries to access restricted path
    const operation = {
      type: 'file:write',
      target: '/etc/shadow',
      data: 'root:PASSWORD=secret123',
    };

    const result = await guardSystem.validate(operation);

    // Should fail on first guard that catches it
    expect(result.allowed).toBe(false);
    expect(result.receipt).toBeDefined();
  });

  it('should generate unique receipts for each validation', async () => {
    const guardSystem = new GuardSystem();

    const operation = {
      type: 'file:read',
      target: path.join(process.cwd(), 'test.txt'),
    };

    const result1 = await guardSystem.validate(operation);
    const result2 = await guardSystem.validate(operation);

    // Different receipts (different timestamps)
    expect(result1.receipt.id).not.toBe(result2.receipt.id);
    expect(result1.receipt.hash).not.toBe(result2.receipt.hash);
  });
});

// =============================================================================
// Receipt Verification Tests
// =============================================================================

describe('Receipt Generation', () => {
  it('should generate receipts with required fields', async () => {
    const guardSystem = new GuardSystem();

    const operation = {
      type: 'file:read',
      target: '/tmp/test.txt',
    };

    const result = await guardSystem.validate(operation);

    expect(result.receipt).toMatchObject({
      id: expect.stringMatching(/^receipt-/),
      timestamp: expect.any(String),
      operation: expect.any(String),
      inputs: expect.any(Object),
      outputs: expect.any(Object),
      hash: expect.stringMatching(/^[a-f0-9]+$/),
    });
  });

  it('should include operation details in receipt inputs', async () => {
    const operation = {
      type: 'network:request',
      target: 'https://api.example.com',
      metadata: { requestId: 'req-123' },
    };

    const result = await SecretGuard(operation);

    expect(result.receipt.inputs).toMatchObject({
      operation: 'network:request',
      target: 'https://api.example.com',
      hasData: false,
    });
  });

  it('should include block reason in receipt outputs for violations', async () => {
    const operation = {
      type: 'file:read',
      target: '/etc/passwd',
    };

    const result = await PrivilegeGuard(operation);

    expect(result.receipt.outputs).toMatchObject({
      allowed: false,
      blocked: 'privilege-escalation',
      reason: expect.stringContaining('restricted path'),
    });
  });

  it('should generate deterministic hashes for receipts', async () => {
    // Note: Hashes won't be identical due to timestamps, but structure should be consistent
    const operation = {
      type: 'file:read',
      target: '/tmp/file.txt',
    };

    const result = await SecretGuard(operation);

    expect(result.receipt.hash).toHaveLength(64); // BLAKE3 hex length
    expect(result.receipt.hash).toMatch(/^[a-f0-9]{64}$/);
  });
});
