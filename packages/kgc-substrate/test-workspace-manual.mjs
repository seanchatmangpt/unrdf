/**
 * Manual test runner for workspace isolation
 * Runs key tests without vitest dependency
 */

import {
  createWorkspace,
  verifyWorkspaceIsolation,
  WorkspaceViolationError,
} from './src/Workspace.mjs';
import * as fs from 'fs/promises';
import { existsSync } from 'fs';

const tests = [];
const results = { passed: 0, failed: 0, errors: [] };

function test(name, fn) {
  tests.push({ name, fn });
}

async function runTests() {
  console.log('\n🧪 Running Workspace Isolation Tests\n');

  for (const { name, fn } of tests) {
    try {
      await fn();
      results.passed++;
      console.log(`✅ ${name}`);
    } catch (error) {
      results.failed++;
      results.errors.push({ test: name, error: error.message });
      console.log(`❌ ${name}`);
      console.log(`   Error: ${error.message}`);
    }
  }

  console.log(`\n📊 Results: ${results.passed} passed, ${results.failed} failed\n`);

  if (results.failed > 0) {
    console.log('Failed tests:');
    results.errors.forEach(({ test, error }) => {
      console.log(`  - ${test}: ${error}`);
    });
    process.exit(1);
  }
}

// Cleanup helper
const workspaces = [];
async function cleanup() {
  for (const ws of workspaces) {
    try {
      await ws.cleanup();
    } catch (err) {
      // Ignore
    }
  }
}

// Test 1: Basic workspace creation
test('Should create workspace with proper directory structure', async () => {
  const ws = await createWorkspace('test-basic', { namespace: 'test' });
  workspaces.push(ws);

  const root = ws.getRoot();
  if (!root.includes('/tmp/kgc-workspace/test-basic/test')) {
    throw new Error(`Unexpected root path: ${root}`);
  }

  if (!existsSync(root)) {
    throw new Error('Workspace directory not created');
  }
});

// Test 2: Allowed write succeeds
test('Should allow write to declared output', async () => {
  const ws = await createWorkspace('test-write', {
    outputs: new Set(['output.txt']),
  });
  workspaces.push(ws);

  await ws.enforceIOContract({ type: 'write', path: 'output.txt' });
  await ws.writeFile('output.txt', 'test content');

  const content = await fs.readFile(ws.resolvePath('output.txt'), 'utf-8');
  if (content !== 'test content') {
    throw new Error('File content mismatch');
  }
});

// Test 3: Undeclared write rejected
test('Should reject write to undeclared path', async () => {
  const ws = await createWorkspace('test-reject', {
    outputs: new Set(['allowed.txt']),
  });
  workspaces.push(ws);

  let errorThrown = false;
  try {
    await ws.enforceIOContract({ type: 'write', path: 'unauthorized.txt' });
  } catch (error) {
    if (error instanceof WorkspaceViolationError) {
      errorThrown = true;
    }
  }

  if (!errorThrown) {
    throw new Error('Expected WorkspaceViolationError to be thrown');
  }
});

// Test 4: System path blocked
test('Should block access to /etc', async () => {
  const ws = await createWorkspace('test-etc', {
    inputs: new Set(['/etc/passwd']),
  });
  workspaces.push(ws);

  let errorThrown = false;
  try {
    await ws.enforceIOContract({ type: 'read', path: '/etc/passwd' });
  } catch (error) {
    if (
      error instanceof WorkspaceViolationError &&
      error.message.includes('system path')
    ) {
      errorThrown = true;
    }
  }

  if (!errorThrown) {
    throw new Error('Expected system path to be blocked');
  }
});

// Test 5: Symlink escape rejected
test('Should reject symlink escape attempts', async () => {
  const ws = await createWorkspace('test-symlink', {
    readWrite: new Set(['safe.txt']),
  });
  workspaces.push(ws);

  const linkPath = ws.resolvePath('safe.txt');
  const targetPath = '/tmp/escape-target.txt';

  try {
    await fs.writeFile(targetPath, 'escaped');
    await fs.symlink(targetPath, linkPath);

    let errorThrown = false;
    try {
      await ws.enforceIOContract({ type: 'read', path: 'safe.txt' });
    } catch (error) {
      if (error instanceof WorkspaceViolationError) {
        errorThrown = true;
      }
    }

    if (!errorThrown) {
      throw new Error('Expected symlink escape to be rejected');
    }
  } finally {
    try {
      await fs.unlink(linkPath);
      await fs.unlink(targetPath);
    } catch (err) {
      // Ignore
    }
  }
});

// Test 6: Cross-agent interference prevented
test('Should prevent cross-agent interference', async () => {
  const ws1 = await createWorkspace('agent-1', {
    outputs: new Set(['data.txt']),
    namespace: 'ns1',
  });
  workspaces.push(ws1);

  const ws2 = await createWorkspace('agent-2', {
    inputs: new Set(['data.txt']),
    namespace: 'ns2',
  });
  workspaces.push(ws2);

  await ws1.writeFile('data.txt', 'agent1 data');

  let errorThrown = false;
  try {
    await ws2.readFile('data.txt');
  } catch (error) {
    errorThrown = true;
  }

  if (!errorThrown) {
    throw new Error('Expected cross-agent access to fail');
  }
});

// Test 7: Workspace isolation verification
test('Should verify workspace isolation', async () => {
  const ws1 = await createWorkspace('iso-1', { namespace: 'ns1' });
  workspaces.push(ws1);

  const ws2 = await createWorkspace('iso-2', { namespace: 'ns2' });
  workspaces.push(ws2);

  const isolated = verifyWorkspaceIsolation(ws1, ws2);
  if (!isolated) {
    throw new Error('Workspaces should be isolated');
  }
});

// Test 8: Enforcement logging
test('Should log enforcement decisions', async () => {
  const ws = await createWorkspace('test-log', {
    inputs: new Set(['allowed.txt']),
  });
  workspaces.push(ws);

  await ws.enforceIOContract({ type: 'read', path: 'allowed.txt' });

  try {
    await ws.enforceIOContract({ type: 'read', path: 'denied.txt' });
  } catch (err) {
    // Expected
  }

  const log = ws.getEnforcementLog();
  if (log.length < 2) {
    throw new Error('Enforcement log should have at least 2 entries');
  }

  const allowed = log.find((e) => e.allowed && e.operation.path === 'allowed.txt');
  const denied = log.find((e) => !e.allowed && e.operation.path === 'denied.txt');

  if (!allowed || !denied) {
    throw new Error('Log should contain both allowed and denied entries');
  }
});

// Test 9: Manifest generation
test('Should generate manifest with hash', async () => {
  const ws = await createWorkspace('test-manifest', {
    inputs: new Set(['in.txt']),
    namespace: 'manifest',
  });
  workspaces.push(ws);

  await ws.enforceIOContract({ type: 'read', path: 'in.txt' });

  const manifest = await ws.getManifest();
  if (!manifest.manifestHash) {
    throw new Error('Manifest should have hash');
  }

  if (manifest.agentId !== 'test-manifest') {
    throw new Error('Manifest agentId mismatch');
  }

  if (!manifest.accessedFiles.includes('in.txt')) {
    throw new Error('Manifest should track accessed files');
  }
});

// Test 10: Directory pattern support
test('Should allow access to files within declared directories', async () => {
  const ws = await createWorkspace('test-dir', {
    readWrite: new Set(['results/']),
  });
  workspaces.push(ws);

  await ws.enforceIOContract({ type: 'write', path: 'results/file1.txt' });
  await ws.enforceIOContract({ type: 'write', path: 'results/subdir/file2.txt' });

  await ws.writeFile('results/file1.txt', 'data1');
  await ws.writeFile('results/subdir/file2.txt', 'data2');

  const content1 = await ws.readFile('results/file1.txt');
  if (content1 !== 'data1') {
    throw new Error('Directory pattern failed for file1');
  }
});

// Run all tests
runTests()
  .then(() => cleanup())
  .then(() => {
    console.log('✅ All tests passed!\n');
    process.exit(0);
  })
  .catch((error) => {
    console.error('Test runner error:', error);
    cleanup().finally(() => process.exit(1));
  });
