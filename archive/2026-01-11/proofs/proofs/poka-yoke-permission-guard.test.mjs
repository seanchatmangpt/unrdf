import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { PermissionGuard } from '../packages/kgc-4d/src/guards/permission-guard.mjs';

describe('Poka-Yoke: Permission Guard', () => {
  it('should allow operation for authorized actor', () => {
    const guard = new PermissionGuard();
    guard.registerPolicy({
      universe_id: 'test-universe',
      admissible_actors: ['alice', 'bob'],
      operations: ['appendEvent', 'read'],
      created_at: new Date().toISOString(),
    });

    assert.doesNotThrow(() => 
      guard.guard('alice', 'appendEvent', 'test-universe')
    );
  });

  it('should deny operation for unauthorized actor', () => {
    const guard = new PermissionGuard();
    guard.registerPolicy({
      universe_id: 'test-universe',
      admissible_actors: ['alice'],
      operations: ['appendEvent'],
      created_at: new Date().toISOString(),
    });

    assert.throws(
      () => guard.guard('eve', 'appendEvent', 'test-universe'),
      /Actor eve not in admissible_actors/
    );
  });

  it('should deny operation not in policy', () => {
    const guard = new PermissionGuard();
    guard.registerPolicy({
      universe_id: 'test-universe',
      admissible_actors: ['alice'],
      operations: ['read'],
      created_at: new Date().toISOString(),
    });

    assert.throws(
      () => guard.guard('alice', 'appendEvent', 'test-universe'),
      /Operation appendEvent not allowed/
    );
  });

  it('should deny if no policy registered', () => {
    const guard = new PermissionGuard();
    assert.throws(
      () => guard.guard('alice', 'appendEvent', 'unknown-universe'),
      /No policy found/
    );
  });

  it('should provide soft-fail check method', () => {
    const guard = new PermissionGuard();
    guard.registerPolicy({
      universe_id: 'test-universe',
      admissible_actors: ['alice'],
      operations: ['appendEvent'],
      created_at: new Date().toISOString(),
    });

    const result = guard.check('eve', 'appendEvent', 'test-universe');
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('not in admissible_actors'));
  });
});
