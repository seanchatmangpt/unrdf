#!/usr/bin/env node
/**
 * @fileoverview Run monorepo admission tests - Functional verification
 *
 * This script verifies all core functionality:
 * - Package version consistency enforced
 * - Circular dependencies detected and denied
 * - API breaking changes trigger denials
 * - Atomic admission (all-or-none) works
 */

import {
  PackagePartition,
  PackageDelta,
  PartitionCategory,
  PROTECTED_PACKAGES,
  Q_version_consistency,
  Q_dependency_acyclic,
  Q_license_compliance,
  H_core_signature_break,
  H_circular_dependency,
  H_protected_package_delete,
  checkAllCrossPackageGuards,
  checkAllCrossPackageInvariants,
  MonorepoUniverse,
  MonorepoAdmissionEngine
} from './index.mjs';

// Test tracking
let passed = 0;
let failed = 0;

function test(name, fn) {
  try {
    fn();
    console.log(`  [PASS] ${name}`);
    passed++;
  } catch (error) {
    console.log(`  [FAIL] ${name}`);
    console.log(`         ${error.message}`);
    failed++;
  }
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message || 'Assertion failed');
  }
}

function assertEquals(actual, expected, message) {
  if (actual !== expected) {
    throw new Error(`${message || 'Not equal'}: expected ${expected}, got ${actual}`);
  }
}

// Create test fixtures
function createTestPartitions() {
  return [
    {
      name: '@unrdf/core',
      version: '5.0.1',
      category: PartitionCategory.CORE,
      dependencies: [],
      exports: [{ path: '.', entryPoint: './src/index.mjs', isPublic: true }],
      license: 'MIT',
      testCoverage: 85,
      docCoverage: 90
    },
    {
      name: '@unrdf/oxigraph',
      version: '5.0.1',
      category: PartitionCategory.CORE,
      dependencies: [],
      license: 'MIT'
    },
    {
      name: '@unrdf/hooks',
      version: '5.0.1',
      category: PartitionCategory.HOOKS,
      dependencies: [
        { name: '@unrdf/core', version: '5.0.1', isWorkspace: true }
      ],
      license: 'MIT'
    },
    {
      name: '@unrdf/yawl',
      version: '5.0.1',
      category: PartitionCategory.YAWL,
      dependencies: [
        { name: '@unrdf/core', version: '5.0.1', isWorkspace: true },
        { name: '@unrdf/hooks', version: '5.0.1', isWorkspace: true }
      ],
      license: 'MIT'
    },
    {
      name: '@unrdf/ml-inference',
      version: '5.0.1',
      category: PartitionCategory.ML,
      dependencies: [
        { name: '@unrdf/core', version: '5.0.1', isWorkspace: true }
      ],
      license: 'MIT'
    },
    {
      name: '@unrdf/serverless',
      version: '5.0.1',
      category: PartitionCategory.INFRASTRUCTURE,
      dependencies: [
        { name: '@unrdf/core', version: '5.0.1', isWorkspace: true }
      ],
      license: 'MIT'
    }
  ];
}

function loadPartitions() {
  const partitions = new Map();
  for (const config of createTestPartitions()) {
    partitions.set(config.name, new PackagePartition(config));
  }
  return partitions;
}

// Run tests
console.log('\n=== Monorepo Admission Tests ===\n');

console.log('1. PackagePartition Tests');
test('creates partition with valid config', () => {
  const partition = new PackagePartition({
    name: '@unrdf/test',
    version: '1.0.0',
    category: PartitionCategory.UTILITY,
    license: 'MIT'
  });
  assertEquals(partition.name, '@unrdf/test', 'name');
  assert(partition.getHash(), 'has hash');
});

test('parses semantic version correctly', () => {
  const partition = new PackagePartition({
    name: '@unrdf/test',
    version: '5.2.3-beta.1',
    category: PartitionCategory.UTILITY
  });
  const parsed = partition.parseVersion();
  assertEquals(parsed.major, 5, 'major');
  assertEquals(parsed.minor, 2, 'minor');
  assertEquals(parsed.patch, 3, 'patch');
  assertEquals(parsed.prerelease, 'beta.1', 'prerelease');
});

test('identifies protected packages', () => {
  const corePartition = new PackagePartition({
    name: '@unrdf/core',
    version: '5.0.1',
    category: PartitionCategory.CORE
  });
  assert(corePartition.isProtectedPartition(), 'core is protected');
  assert(PROTECTED_PACKAGES.has('@unrdf/core'), 'core in set');
});

console.log('\n2. PackageDelta Tests');
test('creates delta with changes', () => {
  const delta = new PackageDelta({
    agent: 'test-agent',
    changes: [
      { packageName: '@unrdf/core', changeType: 'version_bump', details: { newVersion: '5.0.2' } }
    ]
  });
  assertEquals(delta.agent, 'test-agent', 'agent');
  assertEquals(delta.changes.length, 1, 'changes length');
  assert(delta.getAffectedPackages().has('@unrdf/core'), 'affected core');
});

test('identifies breaking changes', () => {
  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/core', changeType: 'api_breaking', details: {} }
    ]
  });
  assert(delta.hasBreakingChanges(), 'has breaking');
  assertEquals(delta.getBreakingChanges().length, 1, 'breaking count');
});

console.log('\n3. Q_version_consistency Tests');
test('passes when all versions consistent', () => {
  const partitions = loadPartitions();
  const result = Q_version_consistency(partitions, null);
  assert(result.passed, 'should pass');
});

test('fails when protected package version mismatches', () => {
  const partitions = loadPartitions();
  partitions.set('@unrdf/hooks', new PackagePartition({
    name: '@unrdf/hooks',
    version: '4.0.0', // Mismatched!
    category: PartitionCategory.HOOKS,
    license: 'MIT'
  }));
  const result = Q_version_consistency(partitions, null);
  assert(!result.passed, 'should fail');
  assert(result.violations.length > 0, 'has violations');
});

console.log('\n4. Q_dependency_acyclic Tests');
test('passes when no cycles exist', () => {
  const partitions = loadPartitions();
  const result = Q_dependency_acyclic(partitions, null);
  assert(result.passed, 'should pass');
  assert(result.metadata.topologicalOrder, 'has topo order');
});

test('fails when circular dependency exists', () => {
  const partitions = loadPartitions();
  // Create cycle: core -> hooks -> core
  partitions.set('@unrdf/core', new PackagePartition({
    name: '@unrdf/core',
    version: '5.0.1',
    category: PartitionCategory.CORE,
    dependencies: [
      { name: '@unrdf/hooks', version: '5.0.1', isWorkspace: true }
    ],
    license: 'MIT'
  }));
  const result = Q_dependency_acyclic(partitions, null);
  assert(!result.passed, 'should fail');
  assert(result.reason.includes('cycle'), 'reason mentions cycle');
});

console.log('\n5. Q_license_compliance Tests');
test('passes when all licenses compatible', () => {
  const partitions = loadPartitions();
  const result = Q_license_compliance(partitions, null);
  assert(result.passed, 'should pass');
});

test('fails when incompatible license found', () => {
  const partitions = loadPartitions();
  partitions.set('@unrdf/problem', new PackagePartition({
    name: '@unrdf/problem',
    version: '1.0.0',
    category: PartitionCategory.UTILITY,
    license: 'GPL-3.0'
  }));
  const result = Q_license_compliance(partitions, null);
  assert(!result.passed, 'should fail');
});

console.log('\n6. H_core_signature_break Tests');
test('allows non-breaking changes', () => {
  const partitions = loadPartitions();
  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/core', changeType: 'version_bump', details: { bumpType: 'patch' } }
    ]
  });
  const result = H_core_signature_break(partitions, delta);
  assert(result.allowed, 'should allow');
});

test('blocks breaking changes without major bump', () => {
  const partitions = loadPartitions();
  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/core', changeType: 'api_breaking', details: { symbol: 'createStore' } }
    ]
  });
  const result = H_core_signature_break(partitions, delta);
  assert(!result.allowed, 'should block');
});

console.log('\n7. H_circular_dependency Tests');
test('allows valid dependency addition', () => {
  const partitions = loadPartitions();
  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/serverless', changeType: 'dependency_add', details: { dependency: '@unrdf/hooks' } }
    ]
  });
  const result = H_circular_dependency(partitions, delta);
  assert(result.allowed, 'should allow');
});

test('blocks dependency creating cycle', () => {
  const partitions = loadPartitions();
  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/core', changeType: 'dependency_add', details: { dependency: '@unrdf/hooks' } }
    ]
  });
  const result = H_circular_dependency(partitions, delta);
  assert(!result.allowed, 'should block');
});

console.log('\n8. H_protected_package_delete Tests');
test('allows deprecation of non-protected', () => {
  const partitions = loadPartitions();
  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/serverless', changeType: 'deprecation' }
    ]
  });
  const result = H_protected_package_delete(partitions, delta);
  assert(result.allowed, 'should allow');
});

test('blocks deprecation of protected', () => {
  const partitions = loadPartitions();
  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/core', changeType: 'deprecation' }
    ]
  });
  const result = H_protected_package_delete(partitions, delta);
  assert(!result.allowed, 'should block');
});

console.log('\n9. MonorepoUniverse Tests');
test('loads partitions and builds graph', () => {
  const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
  universe.loadPartitionsFromData(createTestPartitions());
  assertEquals(universe.partitions.size, 6, 'partition count');
  assert(universe.getDependencies('@unrdf/yawl').has('@unrdf/core'), 'yawl deps on core');
  assert(universe.getDependents('@unrdf/core').has('@unrdf/hooks'), 'core has hooks dependent');
});

test('computes transitive dependents', () => {
  const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
  universe.loadPartitionsFromData(createTestPartitions());
  const transitives = universe.getTransitiveDependents('@unrdf/core');
  assert(transitives.has('@unrdf/hooks'), 'includes hooks');
  assert(transitives.has('@unrdf/yawl'), 'includes yawl');
});

test('provides topological order', () => {
  const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
  universe.loadPartitionsFromData(createTestPartitions());
  const order = universe.getTopologicalOrder();
  assertEquals(order.length, 6, 'order length');
  // In topological order, packages with no deps come first
  // Core has no deps so may be first; hooks depends on core so comes after
  const coreIndex = order.indexOf('@unrdf/core');
  const hooksIndex = order.indexOf('@unrdf/hooks');
  // Both should be in order
  assert(coreIndex >= 0, 'core in order');
  assert(hooksIndex >= 0, 'hooks in order');
});

console.log('\n10. MonorepoAdmissionEngine Tests');
test('admits valid delta', async () => {
  const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
  universe.loadPartitionsFromData(createTestPartitions());
  const engine = new MonorepoAdmissionEngine(universe, { strictMode: true });

  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/serverless', changeType: 'version_bump', details: { newVersion: '5.0.2' } }
    ]
  });

  const result = await engine.admit(delta);
  assert(result.admitted, 'should admit');
  assertEquals(result.decision, 'ALLOW', 'decision');
  assert(result.receipt, 'has receipt');
});

test('denies breaking changes to core', async () => {
  const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
  universe.loadPartitionsFromData(createTestPartitions());
  const engine = new MonorepoAdmissionEngine(universe, { strictMode: true });

  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/core', changeType: 'api_breaking', details: { symbol: 'createStore' } }
    ]
  });

  const result = await engine.admit(delta);
  assert(!result.admitted, 'should deny');
  assertEquals(result.decision, 'DENY', 'decision');
});

test('provides atomic admission guarantee', async () => {
  const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
  universe.loadPartitionsFromData(createTestPartitions());
  const engine = new MonorepoAdmissionEngine(universe, { strictMode: true });

  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/serverless', changeType: 'version_bump', details: { newVersion: '5.0.2' } },
      { packageName: '@unrdf/core', changeType: 'deprecation' } // This will fail
    ]
  });

  const result = await engine.admit(delta);
  assert(!result.admitted, 'should deny atomically');
  assert(result.atomicGuarantee, 'has atomic guarantee');
});

test('generates verifiable receipts', async () => {
  const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
  universe.loadPartitionsFromData(createTestPartitions());
  const engine = new MonorepoAdmissionEngine(universe, { strictMode: true, generateReceipts: true });

  const delta = new PackageDelta({
    agent: 'test',
    changes: [
      { packageName: '@unrdf/serverless', changeType: 'version_bump' }
    ]
  });

  const result = await engine.admit(delta);
  assert(result.receipt, 'has receipt');
  assert(result.receipt.outputHashes.receiptHash, 'has receipt hash');

  const verification = await engine.verifyReceipt(result.receipt);
  assert(verification.valid, 'receipt valid');
});

// Run async tests
console.log('\n11. Async Integration Tests');
(async () => {
  await test('admits valid delta', async () => {
    const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
    universe.loadPartitionsFromData(createTestPartitions());
    const engine = new MonorepoAdmissionEngine(universe);

    const result = await engine.admit(new PackageDelta({
      agent: 'ci',
      changes: [{ packageName: '@unrdf/serverless', changeType: 'version_bump' }]
    }));

    assert(result.admitted, 'should admit');
  });

  await test('tracks statistics', async () => {
    const universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
    universe.loadPartitionsFromData(createTestPartitions());
    const engine = new MonorepoAdmissionEngine(universe);
    engine.resetStats();

    // Admit valid
    await engine.admit(new PackageDelta({
      agent: 'test',
      changes: [{ packageName: '@unrdf/serverless', changeType: 'version_bump' }]
    }));

    // Deny invalid
    await engine.admit(new PackageDelta({
      agent: 'test',
      changes: [{ packageName: '@unrdf/core', changeType: 'deprecation' }]
    }));

    const stats = engine.getStats();
    assertEquals(stats.totalProcessed, 2, 'total');
    assertEquals(stats.allowed, 1, 'allowed');
    assertEquals(stats.denied, 1, 'denied');
  });

  // Print summary
  console.log('\n=== Test Summary ===');
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);
  console.log(`Total:  ${passed + failed}`);

  if (failed > 0) {
    console.log('\nSome tests failed!');
    process.exit(1);
  } else {
    console.log('\nAll tests passed!');
    process.exit(0);
  }
})();
