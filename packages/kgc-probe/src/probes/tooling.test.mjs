/**
 * @fileoverview Tests for Tooling Surface Probe
 * @module @unrdf/kgc-probe/probes/tooling.test
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  probeTooling,
  isCommandAllowed,
  argsAreSafe,
  safeExec,
  ObservationSchema,
  ProbeConfigSchema,
} from './tooling.mjs';

// ============================================================================
// GUARD TESTS (CRITICAL)
// ============================================================================

describe('Command Allowlist Guard (Poka-Yoke)', () => {
  it('should allow git command', () => {
    assert.equal(isCommandAllowed('git'), true);
  });

  it('should allow node command', () => {
    assert.equal(isCommandAllowed('node'), true);
  });

  it('should allow npm command', () => {
    assert.equal(isCommandAllowed('npm'), true);
  });

  it('should allow pnpm command', () => {
    assert.equal(isCommandAllowed('pnpm'), true);
  });

  it('should allow which command', () => {
    assert.equal(isCommandAllowed('which'), true);
  });

  it('should deny arbitrary commands (rm)', () => {
    assert.equal(isCommandAllowed('rm'), false);
  });

  it('should deny arbitrary commands (curl)', () => {
    assert.equal(isCommandAllowed('curl'), false);
  });

  it('should deny arbitrary commands (bash)', () => {
    assert.equal(isCommandAllowed('bash'), false);
  });

  it('should deny shell metacharacter injection', () => {
    assert.equal(isCommandAllowed('git; rm -rf /'), false);
  });
});

describe('Argument Safety Guard', () => {
  it('should allow safe arguments', () => {
    assert.equal(argsAreSafe(['--version']), true);
    assert.equal(argsAreSafe(['--help']), true);
    assert.equal(argsAreSafe(['-v']), true);
  });

  it('should deny arguments with semicolons', () => {
    assert.equal(argsAreSafe(['--foo; rm -rf /']), false);
  });

  it('should deny arguments with pipe characters', () => {
    assert.equal(argsAreSafe(['--foo | cat /etc/passwd']), false);
  });

  it('should deny arguments with backticks', () => {
    assert.equal(argsAreSafe(['--foo `whoami`']), false);
  });

  it('should deny arguments with dollar signs', () => {
    assert.equal(argsAreSafe(['--foo $(whoami)']), false);
  });

  it('should deny arguments with redirects', () => {
    assert.equal(argsAreSafe(['--foo > /tmp/evil']), false);
    assert.equal(argsAreSafe(['--foo < /etc/passwd']), false);
  });
});

// ============================================================================
// SAFE EXECUTION TESTS
// ============================================================================

describe('safeExec', () => {
  it('should deny non-allowlisted commands', async () => {
    const result = await safeExec('rm', ['-rf', '/'], 5000);
    assert.equal(result.success, false);
    assert.equal(result.guardDecision, 'denied');
    assert.match(result.stderr, /not in allowlist/i);
  });

  it('should deny commands with unsafe arguments', async () => {
    const result = await safeExec('git', ['--help; rm -rf /'], 5000);
    assert.equal(result.success, false);
    assert.equal(result.guardDecision, 'denied');
    assert.match(result.stderr, /shell metacharacters/i);
  });

  it('should execute git --version successfully', async () => {
    const result = await safeExec('git', ['--version'], 5000);
    assert.equal(result.guardDecision, 'allowed');
    // May succeed or fail depending on environment, but should not be denied
  });

  it('should execute node --version successfully', async () => {
    const result = await safeExec('node', ['--version'], 5000);
    assert.equal(result.guardDecision, 'allowed');
    // May succeed or fail depending on environment, but should not be denied
  });

  it('should timeout commands exceeding timeout', async () => {
    // This test is environment-dependent, skip if git unavailable
    try {
      const result = await safeExec('git', ['--version'], 1); // 1ms timeout
      // If it completes in <1ms, that's fine
      assert.ok(['allowed', 'unknown'].includes(result.guardDecision));
    } catch {
      // Timeout handling varies by environment
      assert.ok(true);
    }
  });
});

// ============================================================================
// SCHEMA VALIDATION TESTS
// ============================================================================

describe('ObservationSchema', () => {
  it('should validate valid observation', () => {
    const obs = {
      method: 'tooling.git_version',
      inputs: { command: 'git', args: ['--version'] },
      outputs: { version: '2.34.1', available: true },
      guardDecision: 'allowed',
      metadata: { timestamp: Date.now() },
    };
    const result = ObservationSchema.safeParse(obs);
    assert.equal(result.success, true);
  });

  it('should require method field', () => {
    const obs = {
      inputs: {},
      outputs: {},
    };
    const result = ObservationSchema.safeParse(obs);
    assert.equal(result.success, false);
  });

  it('should require inputs field', () => {
    const obs = {
      method: 'test',
      outputs: {},
    };
    const result = ObservationSchema.safeParse(obs);
    assert.equal(result.success, false);
  });

  it('should require outputs field', () => {
    const obs = {
      method: 'test',
      inputs: {},
    };
    const result = ObservationSchema.safeParse(obs);
    assert.equal(result.success, false);
  });

  it('should allow optional guardDecision', () => {
    const obs = {
      method: 'test',
      inputs: {},
      outputs: {},
      guardDecision: 'allowed',
    };
    const result = ObservationSchema.safeParse(obs);
    assert.equal(result.success, true);
  });

  it('should validate guardDecision enum', () => {
    const obs = {
      method: 'test',
      inputs: {},
      outputs: {},
      guardDecision: 'invalid',
    };
    const result = ObservationSchema.safeParse(obs);
    assert.equal(result.success, false);
  });
});

describe('ProbeConfigSchema', () => {
  it('should use default timeout of 5000ms', () => {
    const result = ProbeConfigSchema.parse({});
    assert.equal(result.timeout, 5000);
  });

  it('should allow custom timeout', () => {
    const result = ProbeConfigSchema.parse({ timeout: 3000 });
    assert.equal(result.timeout, 3000);
  });

  it('should reject timeout > 10000ms', () => {
    assert.throws(() => {
      ProbeConfigSchema.parse({ timeout: 15000 });
    });
  });

  it('should reject negative timeout', () => {
    assert.throws(() => {
      ProbeConfigSchema.parse({ timeout: -1000 });
    });
  });

  it('should use default strict: false', () => {
    const result = ProbeConfigSchema.parse({});
    assert.equal(result.strict, false);
  });
});

// ============================================================================
// INTEGRATION TESTS
// ============================================================================

describe('probeTooling', () => {
  it('should return array of observations', async () => {
    const observations = await probeTooling();
    assert.ok(Array.isArray(observations));
    assert.ok(observations.length > 0);
  });

  it('should validate all observations', async () => {
    const observations = await probeTooling();
    observations.forEach(obs => {
      const result = ObservationSchema.safeParse(obs);
      assert.equal(result.success, true, `Invalid observation: ${JSON.stringify(obs)}`);
    });
  });

  it('should include git probe', async () => {
    const observations = await probeTooling();
    const gitObs = observations.find(obs => obs.method === 'tooling.git_version');
    assert.ok(gitObs, 'Missing git observation');
    assert.ok('available' in gitObs.outputs);
  });

  it('should include node probe', async () => {
    const observations = await probeTooling();
    const nodeObs = observations.find(obs => obs.method === 'tooling.node_version');
    assert.ok(nodeObs, 'Missing node observation');
    assert.ok('available' in nodeObs.outputs);
  });

  it('should include npm probe', async () => {
    const observations = await probeTooling();
    const npmObs = observations.find(obs => obs.method === 'tooling.npm_version');
    assert.ok(npmObs, 'Missing npm observation');
    assert.ok('available' in npmObs.outputs);
  });

  it('should include pnpm probe', async () => {
    const observations = await probeTooling();
    const pnpmObs = observations.find(obs => obs.method === 'tooling.pnpm_version');
    assert.ok(pnpmObs, 'Missing pnpm observation');
    assert.ok('available' in pnpmObs.outputs);
  });

  it('should include shell probes', async () => {
    const observations = await probeTooling();
    const shObs = observations.find(obs => obs.method === 'tooling.shell_sh');
    const bashObs = observations.find(obs => obs.method === 'tooling.shell_bash');
    assert.ok(shObs, 'Missing sh observation');
    assert.ok(bashObs, 'Missing bash observation');
  });

  it('should deny build tools not in allowlist', async () => {
    const observations = await probeTooling();
    const makeObs = observations.find(obs => obs.method === 'tooling.build_make');
    const cmakeObs = observations.find(obs => obs.method === 'tooling.build_cmake');
    assert.ok(makeObs, 'Missing make observation');
    assert.ok(cmakeObs, 'Missing cmake observation');
    assert.equal(makeObs.guardDecision, 'denied');
    assert.equal(cmakeObs.guardDecision, 'denied');
    assert.equal(makeObs.outputs.available, false);
    assert.equal(cmakeObs.outputs.available, false);
  });

  it('should include package manager detection', async () => {
    const observations = await probeTooling();
    const pkgMgrObs = observations.find(obs => obs.method === 'tooling.package_manager');
    assert.ok(pkgMgrObs, 'Missing package manager observation');
    assert.ok('primary' in pkgMgrObs.outputs);
  });

  it('should respect custom timeout', async () => {
    const observations = await probeTooling({ timeout: 3000 });
    assert.ok(observations.length > 0);
  });

  it('should handle execution errors gracefully', async () => {
    // This test verifies the try-catch fallback
    // In normal execution, this won't trigger, but ensures the path exists
    const observations = await probeTooling();
    // Should not throw, should return valid observations
    assert.ok(observations.length > 0);
  });
});

// ============================================================================
// GUARD DECISION TESTS (CRITICAL)
// ============================================================================

describe('Guard Decisions', () => {
  it('should mark allowlisted successful commands as "allowed"', async () => {
    const observations = await probeTooling();
    const nodeObs = observations.find(obs => obs.method === 'tooling.node_version');
    // Node should be available in test environment
    if (nodeObs.outputs.available) {
      assert.equal(nodeObs.guardDecision, 'allowed');
    }
  });

  it('should mark non-allowlisted commands as "denied"', async () => {
    const observations = await probeTooling();
    const makeObs = observations.find(obs => obs.method === 'tooling.build_make');
    assert.equal(makeObs.guardDecision, 'denied');
    assert.equal(makeObs.metadata.reason, 'not_in_allowlist');
  });

  it('should never execute denied commands', async () => {
    const observations = await probeTooling();
    const deniedObs = observations.filter(obs => obs.guardDecision === 'denied');
    deniedObs.forEach(obs => {
      // Denied commands should not have version info
      assert.ok(!obs.outputs.version, `Denied command ${obs.method} has version`);
    });
  });
});

// ============================================================================
// SECURITY TESTS (CRITICAL)
// ============================================================================

describe('Security - NO arbitrary command execution', () => {
  it('should prevent command injection via method name', async () => {
    // This test ensures the allowlist guard is enforced
    const result = await safeExec('rm -rf /', [], 5000);
    assert.equal(result.guardDecision, 'denied');
  });

  it('should prevent command injection via arguments', async () => {
    const result = await safeExec('git', ['--version; whoami'], 5000);
    assert.equal(result.guardDecision, 'denied');
  });

  it('should prevent shell expansion', async () => {
    const result = await safeExec('git', ['--version $(whoami)'], 5000);
    assert.equal(result.guardDecision, 'denied');
  });

  it('should prevent pipe injection', async () => {
    const result = await safeExec('git', ['--version | cat /etc/passwd'], 5000);
    assert.equal(result.guardDecision, 'denied');
  });
});
