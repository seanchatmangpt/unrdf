/**
 * Poka-Yoke Proof Test: Permission Guard (Actor-Resource-Action)
 *
 * Demonstrates permission checking before operations
 * Based on Disney Governed Universe Ontology model
 *
 * Expected behavior:
 * - Authorized actors can perform allowed actions
 * - Unauthorized actors are blocked
 * - Protected resources reject modifications
 */

// Actor roles (from Disney BU model, standalone implementation)
const ActorRole = {
  SYSTEM_ADMIN: 'system-admin',
  REVIEWER: 'reviewer',
  CONTRIBUTOR: 'contributor',
  VIEWER: 'viewer'
};

// Resource types (from Disney partitions)
const ResourceType = {
  INDUSTRIAL_SUBSTRATE: 'industrial-substrate',
  CORPORATE_CANON: 'corporate-canon',
  BU_OVERLAY: 'bu-overlay',
  EXECUTION_LEDGER: 'execution-ledger'
};

// Actions
const Action = {
  ADMIT: 'admit',
  FREEZE: 'freeze',
  SEAL: 'seal',
  READ: 'read',
  EDIT: 'edit'
};

/**
 * Permission Guard - Enforces actor-resource-action policies
 */
class PermissionGuard {
  constructor() {
    // Permission matrix: [role][resource][action] -> allowed
    this.permissions = this._initializePermissions();
    this.auditLog = [];
  }

  _initializePermissions() {
    return {
      [ActorRole.SYSTEM_ADMIN]: {
        [ResourceType.INDUSTRIAL_SUBSTRATE]: [Action.READ], // Read-only even for admin
        [ResourceType.CORPORATE_CANON]: [Action.READ, Action.ADMIT, Action.FREEZE, Action.SEAL],
        [ResourceType.BU_OVERLAY]: [Action.READ, Action.ADMIT, Action.FREEZE, Action.SEAL],
        [ResourceType.EXECUTION_LEDGER]: [Action.READ, Action.ADMIT] // Immutable, no freeze
      },
      [ActorRole.REVIEWER]: {
        [ResourceType.INDUSTRIAL_SUBSTRATE]: [Action.READ],
        [ResourceType.CORPORATE_CANON]: [Action.READ, Action.ADMIT],
        [ResourceType.BU_OVERLAY]: [Action.READ, Action.ADMIT],
        [ResourceType.EXECUTION_LEDGER]: [Action.READ]
      },
      [ActorRole.CONTRIBUTOR]: {
        [ResourceType.INDUSTRIAL_SUBSTRATE]: [Action.READ],
        [ResourceType.CORPORATE_CANON]: [Action.READ],
        [ResourceType.BU_OVERLAY]: [Action.READ, Action.ADMIT],
        [ResourceType.EXECUTION_LEDGER]: [Action.READ]
      },
      [ActorRole.VIEWER]: {
        [ResourceType.INDUSTRIAL_SUBSTRATE]: [Action.READ],
        [ResourceType.CORPORATE_CANON]: [Action.READ],
        [ResourceType.BU_OVERLAY]: [Action.READ],
        [ResourceType.EXECUTION_LEDGER]: [Action.READ]
      }
    };
  }

  /**
   * Check if action is permitted
   * @param {string} actor - Actor identifier
   * @param {string} role - Actor role
   * @param {string} resource - Resource type
   * @param {string} action - Action to perform
   * @returns {Object} Permission check result
   * @throws {Error} If permission denied
   */
  checkPermission(actor, role, resource, action) {
    const timestamp = Date.now();

    // Validate inputs
    if (!actor || typeof actor !== 'string') {
      throw new TypeError('Permission guard: actor must be non-empty string');
    }
    if (!Object.values(ActorRole).includes(role)) {
      throw new Error(`Permission guard: Invalid role '${role}'`);
    }
    if (!Object.values(ResourceType).includes(resource)) {
      throw new Error(`Permission guard: Invalid resource '${resource}'`);
    }
    if (!Object.values(Action).includes(action)) {
      throw new Error(`Permission guard: Invalid action '${action}'`);
    }

    // Check permission matrix
    const rolePermissions = this.permissions[role];
    const resourcePermissions = rolePermissions?.[resource] || [];
    const allowed = resourcePermissions.includes(action);

    // Audit log
    this.auditLog.push({
      timestamp,
      actor,
      role,
      resource,
      action,
      allowed,
      decision: allowed ? 'ALLOW' : 'DENY'
    });

    if (!allowed) {
      throw new Error(
        `❌ Permission denied: ${role} cannot ${action} on ${resource} (actor: ${actor})`
      );
    }

    return {
      allowed: true,
      actor,
      role,
      resource,
      action,
      timestamp
    };
  }

  /**
   * Get audit log
   */
  getAuditLog() {
    return this.auditLog;
  }

  /**
   * Get permission summary for role
   */
  getPermissionSummary(role) {
    if (!Object.values(ActorRole).includes(role)) {
      throw new Error(`Invalid role: ${role}`);
    }
    return this.permissions[role];
  }
}

/**
 * Run poka-yoke proof test
 */
async function runProofTest() {
  console.log('🔬 Poka-Yoke Proof Test: Permission Guard\n');

  const guard = new PermissionGuard();
  const results = {
    passed: 0,
    failed: 0,
    tests: []
  };

  // Test 1: System admin can admit to corporate canon
  try {
    console.log('Test 1: System admin admits to corporate canon');
    const result = guard.checkPermission(
      'admin@example.com',
      ActorRole.SYSTEM_ADMIN,
      ResourceType.CORPORATE_CANON,
      Action.ADMIT
    );
    console.log(`✅ PASS: Permission granted for ${result.role} to ${result.action}\n`);
    results.passed++;
    results.tests.push({ name: 'Admin admits to canon', status: 'PASS' });
  } catch (error) {
    console.log(`❌ FAIL: ${error.message}\n`);
    results.failed++;
    results.tests.push({ name: 'Admin admits to canon', status: 'FAIL', error: error.message });
  }

  // Test 2: Reviewer can admit to BU overlay
  try {
    console.log('Test 2: Reviewer admits to BU overlay');
    const result = guard.checkPermission(
      'reviewer@example.com',
      ActorRole.REVIEWER,
      ResourceType.BU_OVERLAY,
      Action.ADMIT
    );
    console.log(`✅ PASS: Permission granted for ${result.role} to ${result.action}\n`);
    results.passed++;
    results.tests.push({ name: 'Reviewer admits to overlay', status: 'PASS' });
  } catch (error) {
    console.log(`❌ FAIL: ${error.message}\n`);
    results.failed++;
    results.tests.push({ name: 'Reviewer admits to overlay', status: 'FAIL', error: error.message });
  }

  // Test 3: Contributor cannot admit to corporate canon
  try {
    console.log('Test 3: Contributor attempts to admit to corporate canon');
    guard.checkPermission(
      'contributor@example.com',
      ActorRole.CONTRIBUTOR,
      ResourceType.CORPORATE_CANON,
      Action.ADMIT
    );
    console.log(`❌ FAIL: Permission should have been denied\n`);
    results.failed++;
    results.tests.push({ name: 'Contributor denied canon admit', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Contributor denied canon admit', status: 'PASS' });
  }

  // Test 4: Viewer cannot admit anywhere
  try {
    console.log('Test 4: Viewer attempts to admit to BU overlay');
    guard.checkPermission(
      'viewer@example.com',
      ActorRole.VIEWER,
      ResourceType.BU_OVERLAY,
      Action.ADMIT
    );
    console.log(`❌ FAIL: Permission should have been denied\n`);
    results.failed++;
    results.tests.push({ name: 'Viewer denied admit', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Viewer denied admit', status: 'PASS' });
  }

  // Test 5: Nobody can edit industrial substrate (read-only)
  try {
    console.log('Test 5: System admin attempts to edit industrial substrate');
    guard.checkPermission(
      'admin@example.com',
      ActorRole.SYSTEM_ADMIN,
      ResourceType.INDUSTRIAL_SUBSTRATE,
      Action.EDIT
    );
    console.log(`❌ FAIL: Industrial substrate is read-only\n`);
    results.failed++;
    results.tests.push({ name: 'Substrate read-only', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Substrate read-only', status: 'PASS' });
  }

  // Test 6: Reviewer cannot freeze (only admin)
  try {
    console.log('Test 6: Reviewer attempts to freeze corporate canon');
    guard.checkPermission(
      'reviewer@example.com',
      ActorRole.REVIEWER,
      ResourceType.CORPORATE_CANON,
      Action.FREEZE
    );
    console.log(`❌ FAIL: Only admin can freeze\n`);
    results.failed++;
    results.tests.push({ name: 'Only admin freezes', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Only admin freezes', status: 'PASS' });
  }

  // Test 7: Everyone can read
  try {
    console.log('Test 7: Viewer reads industrial substrate');
    const result = guard.checkPermission(
      'viewer@example.com',
      ActorRole.VIEWER,
      ResourceType.INDUSTRIAL_SUBSTRATE,
      Action.READ
    );
    console.log(`✅ PASS: Read access granted for ${result.role}\n`);
    results.passed++;
    results.tests.push({ name: 'Viewer can read', status: 'PASS' });
  } catch (error) {
    console.log(`❌ FAIL: ${error.message}\n`);
    results.failed++;
    results.tests.push({ name: 'Viewer can read', status: 'FAIL', error: error.message });
  }

  // Display audit log
  console.log('📊 Audit Log:');
  const auditLog = guard.getAuditLog();
  auditLog.forEach((entry, i) => {
    const icon = entry.decision === 'ALLOW' ? '✅' : '❌';
    console.log(
      `  ${i + 1}. ${icon} ${entry.role} → ${entry.action} on ${entry.resource} (${entry.decision})`
    );
  });
  console.log();

  // Summary
  console.log('═══════════════════════════════════════════════════════════');
  console.log(`Results: ${results.passed} passed, ${results.failed} failed`);
  console.log(`Success Rate: ${((results.passed / (results.passed + results.failed)) * 100).toFixed(1)}%`);
  console.log('═══════════════════════════════════════════════════════════\n');

  // Permission Matrix
  console.log('Permission Matrix:');
  console.log('');
  console.log('┌─────────────┬──────────────┬──────────────┬──────────────┬──────────────┐');
  console.log('│ Role        │ Substrate    │ Canon        │ BU Overlay   │ Ledger       │');
  console.log('├─────────────┼──────────────┼──────────────┼──────────────┼──────────────┤');
  console.log('│ Admin       │ READ         │ READ,ADMIT,  │ READ,ADMIT,  │ READ,ADMIT   │');
  console.log('│             │              │ FREEZE,SEAL  │ FREEZE,SEAL  │              │');
  console.log('├─────────────┼──────────────┼──────────────┼──────────────┼──────────────┤');
  console.log('│ Reviewer    │ READ         │ READ,ADMIT   │ READ,ADMIT   │ READ         │');
  console.log('├─────────────┼──────────────┼──────────────┼──────────────┼──────────────┤');
  console.log('│ Contributor │ READ         │ READ         │ READ,ADMIT   │ READ         │');
  console.log('├─────────────┼──────────────┼──────────────┼──────────────┼──────────────┤');
  console.log('│ Viewer      │ READ         │ READ         │ READ         │ READ         │');
  console.log('└─────────────┴──────────────┴──────────────┴──────────────┴──────────────┘');
  console.log('');

  return results;
}

// Run test
runProofTest().catch(console.error);
