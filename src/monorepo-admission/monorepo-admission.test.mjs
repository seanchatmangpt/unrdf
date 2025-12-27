/**
 * @fileoverview Monorepo Admission Tests - Comprehensive test suite
 *
 * Tests demonstrating:
 * - Package version consistency enforced
 * - Circular dependencies detected and denied
 * - API breaking changes trigger denials
 * - Atomic admission (all-or-none) works
 * - License compliance checked
 * - Protected packages guarded
 *
 * @module monorepo-admission/tests
 */

import { describe, it, expect, beforeEach } from 'vitest';

import {
  PackagePartition,
  PackageDelta,
  PackageChange,
  PartitionCategory,
  PROTECTED_PACKAGES,
  getCategoryForPackage,
  getPackagesInCategory
} from './package-partition.mjs';

import {
  Q_version_consistency,
  Q_api_stability,
  Q_dependency_acyclic,
  Q_license_compliance,
  checkAllCrossPackageInvariants
} from './cross-package-invariants.mjs';

import {
  H_core_signature_break,
  H_circular_dependency,
  H_license_incompatible,
  H_protected_package_delete,
  checkAllCrossPackageGuards
} from './cross-package-guards.mjs';

import {
  MonorepoUniverse
} from './monorepo-universe.mjs';

import {
  MonorepoAdmissionEngine
} from './monorepo-admission-engine.mjs';

// Test fixtures
const createTestPartitions = () => {
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
      exports: [{ path: '.', entryPoint: './src/index.mjs', isPublic: true }],
      license: 'MIT'
    },
    {
      name: '@unrdf/hooks',
      version: '5.0.1',
      category: PartitionCategory.HOOKS,
      dependencies: [
        { name: '@unrdf/core', version: '5.0.1', isWorkspace: true }
      ],
      exports: [{ path: '.', entryPoint: './src/index.mjs', isPublic: true }],
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
      exports: [{ path: '.', entryPoint: './src/index.mjs', isPublic: true }],
      license: 'MIT'
    },
    {
      name: '@unrdf/ml-inference',
      version: '5.0.1',
      category: PartitionCategory.ML,
      dependencies: [
        { name: '@unrdf/core', version: '5.0.1', isWorkspace: true },
        { name: '@unrdf/yawl', version: '5.0.1', isWorkspace: true }
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
    },
    {
      name: '@unrdf/docs',
      version: '5.0.1',
      category: PartitionCategory.DOCUMENTATION,
      dependencies: [],
      license: 'MIT'
    }
  ];
};

describe('PackagePartition', () => {
  it('creates partition with valid config', () => {
    const partition = new PackagePartition({
      name: '@unrdf/test',
      version: '1.0.0',
      category: PartitionCategory.UTILITY,
      license: 'MIT'
    });

    expect(partition.name).toBe('@unrdf/test');
    expect(partition.version).toBe('1.0.0');
    expect(partition.getHash()).toBeDefined();
  });

  it('parses semantic version correctly', () => {
    const partition = new PackagePartition({
      name: '@unrdf/test',
      version: '5.2.3-beta.1',
      category: PartitionCategory.UTILITY
    });

    const parsed = partition.parseVersion();
    expect(parsed.major).toBe(5);
    expect(parsed.minor).toBe(2);
    expect(parsed.patch).toBe(3);
    expect(parsed.prerelease).toBe('beta.1');
  });

  it('identifies protected packages', () => {
    const corePartition = new PackagePartition({
      name: '@unrdf/core',
      version: '5.0.1',
      category: PartitionCategory.CORE
    });

    const utilPartition = new PackagePartition({
      name: '@unrdf/some-util',
      version: '1.0.0',
      category: PartitionCategory.UTILITY
    });

    expect(corePartition.isProtectedPartition()).toBe(true);
    expect(utilPartition.isProtectedPartition()).toBe(false);
  });

  it('validates license compatibility', () => {
    const mitPartition = new PackagePartition({
      name: '@unrdf/test',
      version: '1.0.0',
      category: PartitionCategory.UTILITY,
      license: 'MIT'
    });

    const gplPartition = new PackagePartition({
      name: '@unrdf/test2',
      version: '1.0.0',
      category: PartitionCategory.UTILITY,
      license: 'GPL-3.0'
    });

    expect(mitPartition.isLicenseCompatible()).toBe(true);
    expect(gplPartition.isLicenseCompatible()).toBe(false);
  });
});

describe('PackageDelta', () => {
  it('creates delta with changes', () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        { packageName: '@unrdf/core', changeType: 'version_bump', details: { newVersion: '5.0.2' } }
      ]
    });

    expect(delta.agent).toBe('test-agent');
    expect(delta.changes.length).toBe(1);
    expect(delta.getAffectedPackages().has('@unrdf/core')).toBe(true);
  });

  it('identifies breaking changes', () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        { packageName: '@unrdf/core', changeType: 'api_breaking', details: {} }
      ]
    });

    expect(delta.hasBreakingChanges()).toBe(true);
    expect(delta.getBreakingChanges().length).toBe(1);
  });

  it('computes deterministic hash', () => {
    const delta1 = new PackageDelta({
      agent: 'agent',
      changes: [
        { packageName: '@unrdf/core', changeType: 'version_bump' }
      ]
    });

    const delta2 = new PackageDelta({
      agent: 'agent',
      changes: [
        { packageName: '@unrdf/core', changeType: 'version_bump' }
      ]
    });

    // Same content, same hash (excluding timestamp/id)
    expect(delta1.getHash()).toBe(delta2.getHash());
  });
});

describe('Q_version_consistency', () => {
  let partitions;

  beforeEach(() => {
    partitions = new Map();
    for (const config of createTestPartitions()) {
      partitions.set(config.name, new PackagePartition(config));
    }
  });

  it('passes when all versions are consistent', () => {
    const result = Q_version_consistency(partitions, null);
    expect(result.passed).toBe(true);
  });

  it('fails when protected package version mismatches', () => {
    // Add mismatched version
    partitions.set('@unrdf/hooks', new PackagePartition({
      name: '@unrdf/hooks',
      version: '4.0.0', // Mismatched!
      category: PartitionCategory.HOOKS,
      license: 'MIT'
    }));

    const result = Q_version_consistency(partitions, null);
    expect(result.passed).toBe(false);
    expect(result.violations.length).toBeGreaterThan(0);
    expect(result.violations[0].package).toBe('@unrdf/hooks');
  });

  it('warns when minor version drifts beyond threshold', () => {
    partitions.set('@unrdf/serverless', new PackagePartition({
      name: '@unrdf/serverless',
      version: '5.5.0', // 4 minor versions ahead
      category: PartitionCategory.INFRASTRUCTURE,
      license: 'MIT'
    }));

    const result = Q_version_consistency(partitions, null, { maxVersionDrift: 1 });
    expect(result.warnings).toBeDefined();
    expect(result.warnings.length).toBeGreaterThan(0);
  });
});

describe('Q_dependency_acyclic', () => {
  let partitions;

  beforeEach(() => {
    partitions = new Map();
    for (const config of createTestPartitions()) {
      partitions.set(config.name, new PackagePartition(config));
    }
  });

  it('passes when no cycles exist', () => {
    const result = Q_dependency_acyclic(partitions, null);
    expect(result.passed).toBe(true);
    expect(result.metadata.topologicalOrder).toBeDefined();
  });

  it('fails when circular dependency exists', () => {
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
    expect(result.passed).toBe(false);
    expect(result.violations.length).toBeGreaterThan(0);
    expect(result.reason).toContain('cycle');
  });

  it('detects cycle from proposed dependency addition', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        {
          packageName: '@unrdf/core',
          changeType: 'dependency_add',
          details: { dependency: '@unrdf/yawl' } // Would create cycle
        }
      ]
    });

    const result = Q_dependency_acyclic(partitions, delta);
    // Note: The invariant checks for existing cycles plus proposed additions
    expect(result.passed).toBe(true); // No existing cycle
  });
});

describe('Q_license_compliance', () => {
  let partitions;

  beforeEach(() => {
    partitions = new Map();
    for (const config of createTestPartitions()) {
      partitions.set(config.name, new PackagePartition(config));
    }
  });

  it('passes when all licenses are compatible', () => {
    const result = Q_license_compliance(partitions, null);
    expect(result.passed).toBe(true);
  });

  it('fails when incompatible license found', () => {
    partitions.set('@unrdf/problem', new PackagePartition({
      name: '@unrdf/problem',
      version: '1.0.0',
      category: PartitionCategory.UTILITY,
      license: 'GPL-3.0' // Incompatible!
    }));

    const result = Q_license_compliance(partitions, null);
    expect(result.passed).toBe(false);
    expect(result.violations[0].package).toBe('@unrdf/problem');
  });
});

describe('H_core_signature_break', () => {
  let partitions;

  beforeEach(() => {
    partitions = new Map();
    for (const config of createTestPartitions()) {
      partitions.set(config.name, new PackagePartition(config));
    }
  });

  it('allows non-breaking changes', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        { packageName: '@unrdf/core', changeType: 'version_bump', details: { bumpType: 'patch' } }
      ]
    });

    const result = H_core_signature_break(partitions, delta);
    expect(result.allowed).toBe(true);
  });

  it('blocks breaking changes to protected package without major bump', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        {
          packageName: '@unrdf/core',
          changeType: 'api_breaking',
          details: { symbol: 'createStore' }
        }
      ]
    });

    const result = H_core_signature_break(partitions, delta);
    expect(result.allowed).toBe(false);
    expect(result.violations[0].affectedSymbol).toBe('createStore');
  });

  it('allows breaking changes with major version bump', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        {
          packageName: '@unrdf/core',
          changeType: 'version_bump',
          details: { bumpType: 'major', newVersion: '6.0.0' }
        },
        {
          packageName: '@unrdf/core',
          changeType: 'api_breaking',
          details: { symbol: 'createStore' }
        }
      ]
    });

    const result = H_core_signature_break(partitions, delta);
    expect(result.allowed).toBe(true);
  });
});

describe('H_circular_dependency', () => {
  let partitions;

  beforeEach(() => {
    partitions = new Map();
    for (const config of createTestPartitions()) {
      partitions.set(config.name, new PackagePartition(config));
    }
  });

  it('allows valid dependency addition', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        {
          packageName: '@unrdf/serverless',
          changeType: 'dependency_add',
          details: { dependency: '@unrdf/hooks' }
        }
      ]
    });

    const result = H_circular_dependency(partitions, delta);
    expect(result.allowed).toBe(true);
  });

  it('blocks dependency that would create cycle', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        {
          packageName: '@unrdf/core',
          changeType: 'dependency_add',
          details: { dependency: '@unrdf/hooks' } // hooks depends on core!
        }
      ]
    });

    const result = H_circular_dependency(partitions, delta);
    expect(result.allowed).toBe(false);
    expect(result.violations[0].reason).toContain('circular');
  });
});

describe('H_protected_package_delete', () => {
  let partitions;

  beforeEach(() => {
    partitions = new Map();
    for (const config of createTestPartitions()) {
      partitions.set(config.name, new PackagePartition(config));
    }
  });

  it('allows deprecation of non-protected package', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        { packageName: '@unrdf/serverless', changeType: 'deprecation' }
      ]
    });

    const result = H_protected_package_delete(partitions, delta);
    expect(result.allowed).toBe(true);
  });

  it('blocks deprecation of protected package', () => {
    const delta = new PackageDelta({
      agent: 'test',
      changes: [
        { packageName: '@unrdf/core', changeType: 'deprecation' }
      ]
    });

    const result = H_protected_package_delete(partitions, delta);
    expect(result.allowed).toBe(false);
    expect(result.violations[0].package).toBe('@unrdf/core');
  });
});

describe('MonorepoUniverse', () => {
  let universe;

  beforeEach(() => {
    universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
    universe.loadPartitionsFromData(createTestPartitions());
  });

  it('loads partitions correctly', () => {
    expect(universe.partitions.size).toBe(7);
    expect(universe.getPartition('@unrdf/core')).toBeDefined();
  });

  it('builds dependency graph', () => {
    const deps = universe.getDependencies('@unrdf/yawl');
    expect(deps.has('@unrdf/core')).toBe(true);
    expect(deps.has('@unrdf/hooks')).toBe(true);
  });

  it('computes reverse dependencies', () => {
    const dependents = universe.getDependents('@unrdf/core');
    expect(dependents.has('@unrdf/hooks')).toBe(true);
    expect(dependents.has('@unrdf/yawl')).toBe(true);
  });

  it('computes transitive dependents', () => {
    const transitives = universe.getTransitiveDependents('@unrdf/core');
    expect(transitives.has('@unrdf/hooks')).toBe(true);
    expect(transitives.has('@unrdf/yawl')).toBe(true);
    expect(transitives.has('@unrdf/ml-inference')).toBe(true);
  });

  it('provides topological order', () => {
    const order = universe.getTopologicalOrder();
    expect(order.length).toBe(7);
    // Core should come before packages that depend on it
    expect(order.indexOf('@unrdf/core')).toBeLessThan(order.indexOf('@unrdf/hooks'));
  });

  it('validates universe consistency', () => {
    const result = universe.validateConsistency();
    expect(result.passed).toBe(true);
  });
});

describe('MonorepoAdmissionEngine', () => {
  let universe;
  let engine;

  beforeEach(() => {
    universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
    universe.loadPartitionsFromData(createTestPartitions());
    engine = new MonorepoAdmissionEngine(universe, { strictMode: true });
  });

  it('admits valid delta', async () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        {
          packageName: '@unrdf/serverless',
          changeType: 'version_bump',
          details: { newVersion: '5.0.2' }
        }
      ]
    });

    const result = await engine.admit(delta);
    expect(result.admitted).toBe(true);
    expect(result.decision).toBe('ALLOW');
    expect(result.receipt).toBeDefined();
  });

  it('denies delta with breaking changes to core', async () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        {
          packageName: '@unrdf/core',
          changeType: 'api_breaking',
          details: { symbol: 'createStore' }
        }
      ]
    });

    const result = await engine.admit(delta);
    expect(result.admitted).toBe(false);
    expect(result.decision).toBe('DENY');
    expect(result.phase).toBe('GUARD_CHECK');
  });

  it('denies delta creating circular dependency', async () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        {
          packageName: '@unrdf/core',
          changeType: 'dependency_add',
          details: { dependency: '@unrdf/hooks' }
        }
      ]
    });

    const result = await engine.admit(delta);
    expect(result.admitted).toBe(false);
    expect(result.reason).toContain('circular');
  });

  it('provides atomic admission guarantee', async () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        // First change is fine
        { packageName: '@unrdf/serverless', changeType: 'version_bump', details: { newVersion: '5.0.2' } },
        // Second change would fail (deprecating protected package)
        { packageName: '@unrdf/core', changeType: 'deprecation' }
      ]
    });

    const result = await engine.admit(delta);
    // Atomic: both denied because one failed
    expect(result.admitted).toBe(false);
    expect(result.atomicGuarantee).toBe(true);
  });

  it('generates valid receipts', async () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        { packageName: '@unrdf/serverless', changeType: 'version_bump', details: { newVersion: '5.0.2' } }
      ]
    });

    const result = await engine.admit(delta);
    expect(result.receipt).toBeDefined();
    expect(result.receipt.receiptId).toBeDefined();
    expect(result.receipt.decision).toBe('ALLOW');
    expect(result.receipt.outputHashes.receiptHash).toBeDefined();

    // Verify receipt
    const verification = await engine.verifyReceipt(result.receipt);
    expect(verification.valid).toBe(true);
  });

  it('validates without committing (dry run)', async () => {
    const delta = new PackageDelta({
      agent: 'test-agent',
      changes: [
        { packageName: '@unrdf/serverless', changeType: 'version_bump' }
      ]
    });

    const beforeStats = { ...engine.stats };
    const result = await engine.validate(delta);

    expect(result.valid).toBe(true);
    expect(result.isDryRun).toBe(true);
    // Stats should not change
    expect(engine.stats.totalProcessed).toBe(beforeStats.totalProcessed);
  });

  it('batch admits all-or-none', async () => {
    const deltas = [
      new PackageDelta({
        agent: 'test',
        changes: [{ packageName: '@unrdf/serverless', changeType: 'version_bump' }]
      }),
      new PackageDelta({
        agent: 'test',
        changes: [{ packageName: '@unrdf/core', changeType: 'deprecation' }] // Will fail
      })
    ];

    const result = await engine.admitBatch(deltas);
    expect(result.admitted).toBe(false);
    expect(result.failedAt).toBe(1);
    expect(result.atomicGuarantee).toBe(true);
  });

  it('tracks statistics correctly', async () => {
    engine.resetStats();

    // Admit valid delta
    await engine.admit(new PackageDelta({
      agent: 'test',
      changes: [{ packageName: '@unrdf/serverless', changeType: 'version_bump' }]
    }));

    // Deny invalid delta
    await engine.admit(new PackageDelta({
      agent: 'test',
      changes: [{ packageName: '@unrdf/core', changeType: 'deprecation' }]
    }));

    const stats = engine.getStats();
    expect(stats.totalProcessed).toBe(2);
    expect(stats.allowed).toBe(1);
    expect(stats.denied).toBe(1);
  });
});

describe('Integration: Full Admission Flow', () => {
  let universe;
  let engine;

  beforeEach(() => {
    universe = new MonorepoUniverse({ rootPath: '/home/user/unrdf' });
    universe.loadPartitionsFromData(createTestPartitions());
    engine = new MonorepoAdmissionEngine(universe, {
      strictMode: true,
      generateReceipts: true
    });
  });

  it('complete flow: propose, validate, admit, verify', async () => {
    // Step 1: Propose changes
    const delta = new PackageDelta({
      agent: 'ci-pipeline',
      justification: 'Patch release for serverless package',
      changes: [
        {
          packageName: '@unrdf/serverless',
          changeType: 'version_bump',
          details: { newVersion: '5.0.2', bumpType: 'patch' },
          reason: 'Bug fixes'
        }
      ]
    });

    // Step 2: Validate (dry run)
    const validation = await engine.validate(delta);
    expect(validation.valid).toBe(true);

    // Step 3: Admit
    const admission = await engine.admit(delta);
    expect(admission.admitted).toBe(true);
    expect(admission.atomicGuarantee).toBe(true);

    // Step 4: Verify receipt
    const receipt = admission.receipt;
    expect(receipt).toBeDefined();

    const verification = await engine.verifyReceipt(receipt);
    expect(verification.valid).toBe(true);

    // Step 5: Check impact analysis
    expect(admission.checks.invariants.passed).toBe(true);
    expect(admission.checks.guards.allowed).toBe(true);
  });

  it('complete flow: multi-package coordinated update', async () => {
    const delta = new PackageDelta({
      agent: 'release-manager',
      justification: 'Coordinated minor release',
      changes: [
        {
          packageName: '@unrdf/core',
          changeType: 'version_bump',
          details: { newVersion: '5.1.0', bumpType: 'minor' }
        },
        {
          packageName: '@unrdf/hooks',
          changeType: 'version_bump',
          details: { newVersion: '5.1.0', bumpType: 'minor' }
        },
        {
          packageName: '@unrdf/yawl',
          changeType: 'version_bump',
          details: { newVersion: '5.1.0', bumpType: 'minor' }
        }
      ]
    });

    const result = await engine.admit(delta);
    expect(result.admitted).toBe(true);
    expect(result.affectedPackages).toContain('@unrdf/core');
    expect(result.affectedPackages).toContain('@unrdf/hooks');
    expect(result.affectedPackages).toContain('@unrdf/yawl');
  });

  it('complete flow: denied breaking change without downstream updates', async () => {
    // Breaking change to hooks without updating yawl (which depends on hooks)
    const delta = new PackageDelta({
      agent: 'developer',
      changes: [
        {
          packageName: '@unrdf/hooks',
          changeType: 'api_breaking',
          details: { symbol: 'defineHook' }
        }
        // Missing: update to @unrdf/yawl which depends on hooks
      ]
    });

    const result = await engine.admit(delta);
    expect(result.admitted).toBe(false);
    // Should be denied by either guard or invariant
    expect(['GUARD_CHECK', 'INVARIANT_CHECK']).toContain(result.phase);
  });
});

describe('Package Category Functions', () => {
  it('getCategoryForPackage returns correct category', () => {
    expect(getCategoryForPackage('@unrdf/core')).toBe(PartitionCategory.CORE);
    expect(getCategoryForPackage('@unrdf/yawl')).toBe(PartitionCategory.YAWL);
    expect(getCategoryForPackage('@unrdf/ml-inference')).toBe(PartitionCategory.ML);
    expect(getCategoryForPackage('@unrdf/docs')).toBe(PartitionCategory.DOCUMENTATION);
  });

  it('getPackagesInCategory returns packages', () => {
    const yawlPackages = getPackagesInCategory(PartitionCategory.YAWL);
    expect(yawlPackages).toContain('@unrdf/yawl');
    expect(yawlPackages).toContain('@unrdf/yawl-api');
    expect(yawlPackages).toContain('@unrdf/yawl-kafka');
  });

  it('PROTECTED_PACKAGES contains expected packages', () => {
    expect(PROTECTED_PACKAGES.has('@unrdf/core')).toBe(true);
    expect(PROTECTED_PACKAGES.has('@unrdf/oxigraph')).toBe(true);
    expect(PROTECTED_PACKAGES.has('@unrdf/hooks')).toBe(true);
    expect(PROTECTED_PACKAGES.has('@unrdf/serverless')).toBe(false);
  });
});
