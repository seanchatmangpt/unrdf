import test from 'node:test';
import assert from 'node:assert/strict';
import {
  AtomVMIdleEstate,
  IdleEstateRefusal,
  IDLE_ESTATE_WORK_CLASS,
} from '../src/idle-estate.mjs';

function fixture() {
  let now = 100;
  let nextId = 0;
  const estate = new AtomVMIdleEstate({
    estateId: 'enterprise-west',
    clock: () => now,
    idFactory: () => `id-${++nextId}`,
  });

  const host = hostId => ({
    hostId,
    runtime: 'atomvm',
    standing: 'admitted',
    idleWindow: { startMs: 100, endMs: 1000 },
    drainDeadlineMs: 900,
    maxCpuUnits: 4,
    maxMemoryMb: 4096,
    maxStorageMb: 1024,
    networkCapabilities: ['rdf.read'],
    allowedWorkloadClasses: [IDLE_ESTATE_WORK_CLASS],
  });

  estate.admitHost(host('host-a'));
  estate.admitHost(host('host-b'));

  return {
    estate,
    setNow(value) {
      now = value;
    },
  };
}

function job(jobId, overrides = {}) {
  return {
    jobId,
    semanticSubject: `urn:gall:work-order:${jobId}`,
    workOrderIri: `urn:gall:work-order:${jobId}`,
    checkpointIri: `urn:gall:checkpoint:${jobId}`,
    graphDigest: 'sha256:' + 'a'.repeat(64),
    repositoryIdentity: 'seanchatmangpt/unrdf',
    baseSha: 'b'.repeat(40),
    semanticAuthority: 'NONE',
    standing: 'admitted',
    workloadClass: IDLE_ESTATE_WORK_CLASS,
    payload: { query: 'bounded unknown' },
    resources: { cpuUnits: 1, memoryMb: 256, storageMb: 32 },
    authority: { networkCapabilities: ['rdf.read'], filesystem: 'none' },
    ...overrides,
  };
}

test('leases two independent admitted AtomVM hosts using only typed envelopes', () => {
  const { estate } = fixture();
  estate.openIdle('host-a', 100);
  estate.openIdle('host-b', 100);
  estate.admitJob(job('job-a'));
  estate.admitJob(job('job-b'));

  const leaseA = estate.lease('job-a', { now: 110 });
  const leaseB = estate.lease('job-b', { now: 110 });

  assert.notEqual(leaseA.hostId, leaseB.hostId);
  assert.deepEqual(new Set([leaseA.hostId, leaseB.hostId]), new Set(['host-a', 'host-b']));
  assert.equal(leaseA.authority.filesystem, 'none');
});

test('host outside admitted idle window accepts no work', () => {
  const { estate } = fixture();
  assert.throws(
    () => estate.openIdle('host-a', 99),
    error => error instanceof IdleEstateRefusal && error.code === 'OUTSIDE_IDLE_WINDOW_REFUSED',
  );
  assert.equal(estate.snapshot().hosts.find(host => host.hostId === 'host-a').state, 'PRIMARY');
});

test('CPU/memory/storage envelope overflow refuses before execution', () => {
  const { estate } = fixture();
  estate.openIdle('host-a', 100);
  estate.admitJob(job('oversize', { resources: { cpuUnits: 99, memoryMb: 99999, storageMb: 99999 } }));
  assert.throws(
    () => estate.lease('oversize', { hostId: 'host-a', now: 110 }),
    error => error instanceof IdleEstateRefusal && error.code === 'NO_ELIGIBLE_HOST_REFUSED',
  );
  assert.equal(estate.snapshot().hosts.find(host => host.hostId === 'host-a').state, 'ADMITTED');
});

test('drain deadline stops new leases and restores PRIMARY', () => {
  const { estate } = fixture();
  estate.openIdle('host-a', 100);
  estate.admitJob(job('late'));
  const drained = estate.drainExpired(900);
  assert.deepEqual(drained, ['host-a']);
  assert.equal(estate.snapshot().hosts.find(host => host.hostId === 'host-a').state, 'PRIMARY');
  assert.throws(
    () => estate.lease('late', { hostId: 'host-a', now: 900 }),
    error => error instanceof IdleEstateRefusal && error.code === 'NO_ELIGIBLE_HOST_REFUSED',
  );
});

test('replayed content identity never executes a second consequence', async () => {
  const { estate } = fixture();
  estate.openIdle('host-a', 100);
  estate.admitJob(job('once'));
  const lease = estate.lease('once', { hostId: 'host-a', now: 110 });
  let calls = 0;
  const executor = {
    async execute() {
      calls += 1;
      return { value: 42 };
    },
  };

  const first = await estate.execute(lease.leaseId, executor, 120);
  const replay = estate.replayOutcome('once');
  assert.equal(first.status, 'COMPLETED');
  assert.equal(replay.receiptDigest, first.receiptDigest);
  assert.equal(calls, 1);
  assert.throws(
    () => estate.lease('once', { hostId: 'host-a', now: 130 }),
    error => error instanceof IdleEstateRefusal && error.code === 'REPLAY_LEASE_REFUSED',
  );
  assert.equal(calls, 1);
});

test('job cannot widen host network or filesystem authority', () => {
  const { estate } = fixture();
  estate.openIdle('host-a', 100);
  estate.admitJob(job('authority-breach', {
    authority: { networkCapabilities: ['rdf.read', 'internet.egress'], filesystem: 'root' },
  }));

  assert.throws(
    () => estate.lease('authority-breach', { hostId: 'host-a', now: 120 }),
    error => error instanceof IdleEstateRefusal && error.code === 'NO_ELIGIBLE_HOST_REFUSED',
  );
});

test('abrupt host loss produces receipted UNKNOWN and returns host to PRIMARY', async () => {
  const { estate } = fixture();
  estate.openIdle('host-a', 100);
  estate.admitJob(job('host-loss'));
  const lease = estate.lease('host-loss', { hostId: 'host-a', now: 110 });

  const receipt = await estate.execute(
    lease.leaseId,
    {
      async execute() {
        throw new Error('host disappeared');
      },
    },
    120,
  );

  assert.equal(receipt.status, 'UNKNOWN');
  assert.equal(estate.replayOutcome('host-loss').receiptDigest, receipt.receiptDigest);
  assert.equal(estate.snapshot().hosts.find(host => host.hostId === 'host-a').state, 'PRIMARY');
  assert.ok(estate.receipts().some(item => item.kind === 'EXECUTION_UNKNOWN'));
});


test('job package is bound to exact WorkOrder graph and base identity', () => {
  const { estate } = fixture();

  const admitted = estate.admitJob(job('exact-subject'));
  assert.equal(admitted.semanticSubject, admitted.workOrderIri);
  assert.equal(admitted.graphDigest, 'sha256:' + 'a'.repeat(64));
  assert.equal(admitted.repositoryIdentity, 'seanchatmangpt/unrdf');
  assert.equal(admitted.baseSha, 'b'.repeat(40));
  assert.equal(admitted.semanticAuthority, 'NONE');
});

test('moved WorkOrder identity is refused before leasing', () => {
  const { estate } = fixture();

  assert.throws(
    () => estate.admitJob(job('moved', { workOrderIri: 'urn:gall:work-order:other' })),
    error => error instanceof IdleEstateRefusal && error.code === 'SEMANTIC_SUBJECT_MISMATCH_REFUSED',
  );
});

test('branch name cannot replace exact base SHA', () => {
  const { estate } = fixture();

  assert.throws(
    () => estate.admitJob(job('branch-base', { baseSha: 'main' })),
    error => error instanceof IdleEstateRefusal && error.code === 'BASE_SHA_REFUSED',
  );
});

test('edge execution returns candidate evidence and no semantic authority', async () => {
  const { estate } = fixture();
  estate.openIdle('host-a', 100);
  estate.admitJob(job('candidate-result'));
  const lease = estate.lease('candidate-result', { hostId: 'host-a', now: 110 });

  const receipt = await estate.execute(
    lease.leaseId,
    { async execute() { return { value: 42 }; } },
    120,
  );

  assert.equal(receipt.status, 'COMPLETED');
  assert.equal(receipt.semanticStanding, 'CANDIDATE');
  assert.equal(receipt.semanticAuthority, 'NONE');
  assert.equal(receipt.workOrderIri, 'urn:gall:work-order:candidate-result');
});
