import { createHash, randomUUID } from 'node:crypto';

export const IDLE_ESTATE_WORK_CLASS = 'UNKNOWN_IDLE_ESTATE';

export class IdleEstateRefusal extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = 'IdleEstateRefusal';
    this.code = code;
    this.details = Object.freeze({ ...details });
  }
}

function canonical(value) {
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  if (value && typeof value === 'object') {
    return `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}`;
  }
  return JSON.stringify(value);
}

function digest(value) {
  return createHash('sha256').update(canonical(value)).digest('hex');
}

function assertStableId(value, field) {
  if (typeof value !== 'string' || !/^[a-zA-Z0-9][a-zA-Z0-9._:-]*$/.test(value)) {
    throw new IdleEstateRefusal('INVALID_ID_REFUSED', `${field} must be a stable identifier`, { field, value });
  }
}

function assertAbsoluteIri(value, field) {
  if (typeof value !== 'string' || value.length === 0 || !value.includes(':')) {
    throw new IdleEstateRefusal('SEMANTIC_IDENTITY_REFUSED', `${field} must be an absolute IRI`, { field, value });
  }
}

function assertGraphDigest(value) {
  if (typeof value !== 'string' || !/^sha256:[0-9a-f]{64}$/.test(value)) {
    throw new IdleEstateRefusal('GRAPH_DIGEST_REFUSED', 'graphDigest must be sha256:<64 lowercase hex>', { value });
  }
}

function assertGitSha(value) {
  if (typeof value !== 'string' || !/^[0-9a-f]{40}$/.test(value)) {
    throw new IdleEstateRefusal('BASE_SHA_REFUSED', 'baseSha must be a full lowercase git SHA', { value });
  }
}

function assertRepositoryIdentity(value) {
  if (typeof value !== 'string' || !/^[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+$/.test(value)) {
    throw new IdleEstateRefusal('REPOSITORY_IDENTITY_REFUSED', 'repositoryIdentity must be owner/repo', { value });
  }
}

function subset(requested = [], allowed = []) {
  const admitted = new Set(allowed);
  return requested.every(value => admitted.has(value));
}

function frozenReceipt(body) {
  const receiptDigest = digest(body);
  return Object.freeze({ ...body, receiptDigest });
}

/**
 * Bounded enterprise idle-estate scheduler for AtomVM cells.
 *
 * Hosts begin and end in PRIMARY. The scheduler can only lease admitted
 * UNKNOWN_IDLE_ESTATE packages inside explicit time/resource/authority
 * envelopes. It never widens package authority and replay never re-executes.
 */
export class AtomVMIdleEstate {
  #hosts = new Map();
  #jobs = new Map();
  #leases = new Map();
  #receipts = [];
  #jobOutcomes = new Map();

  constructor({ estateId, clock = () => Date.now(), idFactory = randomUUID } = {}) {
    assertStableId(estateId, 'estateId');
    this.estateId = estateId;
    this.clock = clock;
    this.idFactory = idFactory;
  }

  admitHost(envelope) {
    const {
      hostId,
      idleWindow,
      maxCpuUnits,
      maxMemoryMb,
      maxStorageMb,
      networkCapabilities = [],
      allowedWorkloadClasses = [IDLE_ESTATE_WORK_CLASS],
      drainDeadlineMs,
      standing = 'admitted',
      maxTemperatureC = null,
      maxPowerW = null,
      runtime = 'atomvm',
    } = envelope ?? {};

    assertStableId(hostId, 'hostId');
    if (this.#hosts.has(hostId)) {
      throw new IdleEstateRefusal('DUPLICATE_HOST_REFUSED', `Host ${hostId} already exists`, { hostId });
    }
    if (standing !== 'admitted') {
      throw new IdleEstateRefusal('HOST_STANDING_REFUSED', 'Host must have admitted standing', { hostId, standing });
    }
    if (runtime !== 'atomvm') {
      throw new IdleEstateRefusal('RUNTIME_REFUSED', 'Idle-estate host must expose the AtomVM runtime', { hostId, runtime });
    }
    if (
      !idleWindow ||
      !Number.isFinite(idleWindow.startMs) ||
      !Number.isFinite(idleWindow.endMs) ||
      idleWindow.startMs >= idleWindow.endMs
    ) {
      throw new IdleEstateRefusal('IDLE_WINDOW_REFUSED', 'Host idle window must be finite and ordered', { hostId });
    }
    if (!Number.isFinite(drainDeadlineMs) || drainDeadlineMs > idleWindow.endMs || drainDeadlineMs <= idleWindow.startMs) {
      throw new IdleEstateRefusal('DRAIN_DEADLINE_REFUSED', 'Drain deadline must fall inside the idle window', { hostId });
    }
    for (const [field, value] of Object.entries({ maxCpuUnits, maxMemoryMb, maxStorageMb })) {
      if (!Number.isFinite(value) || value <= 0) {
        throw new IdleEstateRefusal('RESOURCE_ENVELOPE_REFUSED', `${field} must be finite and positive`, { hostId, field, value });
      }
    }

    const host = Object.freeze({
      hostId,
      runtime,
      standing,
      idleWindow: Object.freeze({ ...idleWindow }),
      maxCpuUnits,
      maxMemoryMb,
      maxStorageMb,
      maxTemperatureC,
      maxPowerW,
      networkCapabilities: Object.freeze([...networkCapabilities].sort()),
      allowedWorkloadClasses: Object.freeze([...allowedWorkloadClasses].sort()),
      drainDeadlineMs,
      state: 'PRIMARY',
      leaseId: null,
    });

    this.#hosts.set(hostId, host);
    this.#receipt('HOST_ADMITTED', { hostId, state: 'PRIMARY', envelopeDigest: digest(host) });
    return host;
  }

  admitJob(job) {
    const {
      jobId,
      semanticSubject,
      workOrderIri,
      checkpointIri,
      graphDigest,
      repositoryIdentity,
      baseSha,
      workloadClass = IDLE_ESTATE_WORK_CLASS,
      payload,
      resources,
      authority = {},
      semanticAuthority = 'NONE',
      standing = 'admitted',
    } = job ?? {};

    assertStableId(jobId, 'jobId');
    if (this.#jobs.has(jobId)) {
      throw new IdleEstateRefusal('DUPLICATE_JOB_REFUSED', `Job ${jobId} already exists`, { jobId });
    }
    if (standing !== 'admitted') {
      throw new IdleEstateRefusal('JOB_STANDING_REFUSED', 'Job must have admitted standing', { jobId, standing });
    }
    if (workloadClass !== IDLE_ESTATE_WORK_CLASS) {
      throw new IdleEstateRefusal('WORKLOAD_CLASS_REFUSED', 'Only UNKNOWN_IDLE_ESTATE is admitted by this scheduler', { jobId, workloadClass });
    }
    if (typeof semanticSubject !== 'string' || semanticSubject.length === 0) {
      throw new IdleEstateRefusal('SEMANTIC_SUBJECT_REFUSED', 'semanticSubject is required', { jobId });
    }
    assertAbsoluteIri(workOrderIri, 'workOrderIri');
    assertAbsoluteIri(checkpointIri, 'checkpointIri');
    assertGraphDigest(graphDigest);
    assertRepositoryIdentity(repositoryIdentity);
    assertGitSha(baseSha);
    if (semanticSubject !== workOrderIri) {
      throw new IdleEstateRefusal(
        'SEMANTIC_SUBJECT_MISMATCH_REFUSED',
        'semanticSubject must equal workOrderIri',
        { jobId, semanticSubject, workOrderIri },
      );
    }
    if (semanticAuthority !== 'NONE') {
      throw new IdleEstateRefusal(
        'SEMANTIC_AUTHORITY_REFUSED',
        'idle-estate execution cannot manufacture semantic authority',
        { jobId, semanticAuthority },
      );
    }
    for (const field of ['cpuUnits', 'memoryMb', 'storageMb']) {
      if (!Number.isFinite(resources?.[field]) || resources[field] < 0) {
        throw new IdleEstateRefusal('JOB_RESOURCE_REFUSED', `${field} must be finite and non-negative`, { jobId, field });
      }
    }

    const body = {
      jobId,
      semanticSubject,
      workOrderIri,
      checkpointIri,
      graphDigest,
      repositoryIdentity,
      baseSha,
      workloadClass,
      payload,
      resources: Object.freeze({ ...resources }),
      authority: Object.freeze({
        networkCapabilities: Object.freeze([...(authority.networkCapabilities ?? [])].sort()),
        filesystem: authority.filesystem ?? 'none',
      }),
      semanticAuthority: 'NONE',
      standing,
    };
    const admitted = Object.freeze({ ...body, packageDigest: digest(body) });
    this.#jobs.set(jobId, admitted);
    this.#receipt('JOB_ADMITTED', {
      jobId,
      packageDigest: admitted.packageDigest,
      semanticSubject,
      workOrderIri,
      checkpointIri,
      graphDigest,
      repositoryIdentity,
      baseSha,
      semanticAuthority: 'NONE',
    });
    return admitted;
  }

  openIdle(hostId, now = this.clock()) {
    const host = this.#requireHost(hostId);
    if (host.state !== 'PRIMARY') {
      throw new IdleEstateRefusal('HOST_STATE_REFUSED', 'Only PRIMARY hosts may enter the idle-estate path', { hostId, state: host.state });
    }
    if (now < host.idleWindow.startMs || now >= host.drainDeadlineMs) {
      throw new IdleEstateRefusal('OUTSIDE_IDLE_WINDOW_REFUSED', 'Host is not inside its admitted lease window', { hostId, now });
    }

    this.#replaceHost(hostId, { state: 'IDLE_CANDIDATE' });
    this.#receipt('HOST_IDLE_CANDIDATE', { hostId, now });
    this.#replaceHost(hostId, { state: 'ADMITTED' });
    this.#receipt('HOST_IDLE_ADMITTED', { hostId, now });
    return this.#requireHost(hostId);
  }

  eligibleHosts(jobId, now = this.clock()) {
    const job = this.#requireJob(jobId);
    return Object.freeze(
      [...this.#hosts.values()]
        .filter(host => this.#eligible(host, job, now))
        .sort((a, b) => a.hostId.localeCompare(b.hostId))
        .map(host => host.hostId),
    );
  }

  lease(jobId, { hostId = null, now = this.clock() } = {}) {
    const job = this.#requireJob(jobId);
    if (this.#jobOutcomes.has(job.packageDigest)) {
      throw new IdleEstateRefusal('REPLAY_LEASE_REFUSED', 'Completed/unknown job identity cannot be leased again', { jobId, packageDigest: job.packageDigest });
    }

    const candidates = hostId ? [this.#requireHost(hostId)] : this.eligibleHosts(jobId, now).map(id => this.#requireHost(id));
    const host = candidates.find(candidate => this.#eligible(candidate, job, now));
    if (!host) {
      throw new IdleEstateRefusal('NO_ELIGIBLE_HOST_REFUSED', 'No admitted host can satisfy the job envelope', { jobId, hostId, now });
    }

    const leaseId = this.idFactory();
    const leaseBody = {
      leaseId,
      estateId: this.estateId,
      hostId: host.hostId,
      jobId: job.jobId,
      workOrderIri: job.workOrderIri,
      checkpointIri: job.checkpointIri,
      graphDigest: job.graphDigest,
      repositoryIdentity: job.repositoryIdentity,
      baseSha: job.baseSha,
      packageDigest: job.packageDigest,
      leasedAt: now,
      semanticAuthority: 'NONE',
      authority: job.authority,
      resources: job.resources,
    };
    const lease = Object.freeze({ ...leaseBody, leaseDigest: digest(leaseBody) });
    this.#leases.set(leaseId, lease);
    this.#replaceHost(host.hostId, { state: 'LEASED', leaseId });
    this.#receipt('LEASE_CREATED', { ...leaseBody });
    return lease;
  }

  async execute(leaseId, executor, now = this.clock()) {
    const lease = this.#requireLease(leaseId);
    const host = this.#requireHost(lease.hostId);
    const job = this.#requireJob(lease.jobId);

    const replay = this.#jobOutcomes.get(job.packageDigest);
    if (replay) return replay;
    if (host.state !== 'LEASED' || host.leaseId !== leaseId) {
      throw new IdleEstateRefusal('LEASE_STATE_REFUSED', 'Lease is not the host active lease', { leaseId, hostId: host.hostId, state: host.state });
    }
    if (now >= host.drainDeadlineMs) {
      this.drain(host.hostId, now);
      throw new IdleEstateRefusal('DRAIN_DEADLINE_REFUSED', 'Lease reached drain deadline before execution', { leaseId, hostId: host.hostId });
    }
    if (!executor || typeof executor.execute !== 'function') {
      throw new IdleEstateRefusal('EXECUTOR_REQUIRED_REFUSED', 'AtomVM executor.execute is required', { leaseId });
    }

    this.#replaceHost(host.hostId, { state: 'EXECUTING' });
    this.#receipt('EXECUTION_STARTED', { leaseId, hostId: host.hostId, jobId: job.jobId, packageDigest: job.packageDigest, now });

    let outcome;
    try {
      const result = await executor.execute({ host: this.#requireHost(host.hostId), job, lease });
      outcome = this.#receipt('EXECUTION_COMPLETED', {
        leaseId,
        hostId: host.hostId,
        jobId: job.jobId,
        packageDigest: job.packageDigest,
        workOrderIri: job.workOrderIri,
        graphDigest: job.graphDigest,
        status: 'COMPLETED',
        semanticStanding: 'CANDIDATE',
        semanticAuthority: 'NONE',
        result,
      });
      this.#replaceHost(host.hostId, { state: 'ADMITTED', leaseId: null });
    } catch (error) {
      outcome = this.#receipt('EXECUTION_UNKNOWN', {
        leaseId,
        hostId: host.hostId,
        jobId: job.jobId,
        packageDigest: job.packageDigest,
        workOrderIri: job.workOrderIri,
        graphDigest: job.graphDigest,
        status: 'UNKNOWN',
        semanticStanding: 'UNKNOWN',
        semanticAuthority: 'NONE',
        error: { name: error?.name ?? 'Error', message: error?.message ?? String(error) },
      });
      this.#replaceHost(host.hostId, { state: 'PRIMARY', leaseId: null });
    }

    this.#jobOutcomes.set(job.packageDigest, outcome);
    this.#leases.delete(leaseId);
    return outcome;
  }

  drain(hostId, now = this.clock()) {
    const host = this.#requireHost(hostId);
    if (host.state === 'EXECUTING') {
      throw new IdleEstateRefusal('ACTIVE_EXECUTION_REFUSED', 'Host cannot finish drain while execution is active', { hostId });
    }
    if (host.state === 'PRIMARY') return host;

    this.#replaceHost(hostId, { state: 'DRAINING', leaseId: null });
    this.#receipt('HOST_DRAINING', { hostId, now });
    this.#replaceHost(hostId, { state: 'PRIMARY', leaseId: null });
    this.#receipt('HOST_PRIMARY_RESTORED', { hostId, now });
    return this.#requireHost(hostId);
  }

  drainExpired(now = this.clock()) {
    const drained = [];
    for (const host of [...this.#hosts.values()].sort((a, b) => a.hostId.localeCompare(b.hostId))) {
      if (host.state !== 'PRIMARY' && host.state !== 'EXECUTING' && now >= host.drainDeadlineMs) {
        this.drain(host.hostId, now);
        drained.push(host.hostId);
      }
    }
    return Object.freeze(drained);
  }

  replayOutcome(jobId) {
    const job = this.#requireJob(jobId);
    return this.#jobOutcomes.get(job.packageDigest) ?? null;
  }

  receipts() {
    return Object.freeze([...this.#receipts]);
  }

  snapshot() {
    return Object.freeze({
      estateId: this.estateId,
      hosts: Object.freeze([...this.#hosts.values()]),
      jobs: Object.freeze([...this.#jobs.values()]),
      leases: Object.freeze([...this.#leases.values()]),
      receipts: this.receipts(),
    });
  }

  #eligible(host, job, now) {
    if (host.standing !== 'admitted' || host.runtime !== 'atomvm' || host.state !== 'ADMITTED') return false;
    if (now < host.idleWindow.startMs || now >= host.drainDeadlineMs) return false;
    if (!host.allowedWorkloadClasses.includes(job.workloadClass)) return false;
    if (job.resources.cpuUnits > host.maxCpuUnits) return false;
    if (job.resources.memoryMb > host.maxMemoryMb) return false;
    if (job.resources.storageMb > host.maxStorageMb) return false;
    if (!subset(job.authority.networkCapabilities, host.networkCapabilities)) return false;
    if (job.authority.filesystem !== 'none' && job.authority.filesystem !== 'local') return false;
    return true;
  }

  #replaceHost(hostId, changes) {
    const previous = this.#requireHost(hostId);
    this.#hosts.set(hostId, Object.freeze({ ...previous, ...changes }));
  }

  #receipt(kind, details) {
    const body = {
      receiptId: this.idFactory(),
      estateId: this.estateId,
      kind,
      observedAt: this.clock(),
      ...details,
    };
    const receipt = frozenReceipt(body);
    this.#receipts.push(receipt);
    return receipt;
  }

  #requireHost(hostId) {
    const host = this.#hosts.get(hostId);
    if (!host) throw new IdleEstateRefusal('UNKNOWN_HOST_REFUSED', `Unknown host ${hostId}`, { hostId });
    return host;
  }

  #requireJob(jobId) {
    const job = this.#jobs.get(jobId);
    if (!job) throw new IdleEstateRefusal('UNKNOWN_JOB_REFUSED', `Unknown job ${jobId}`, { jobId });
    return job;
  }

  #requireLease(leaseId) {
    const lease = this.#leases.get(leaseId);
    if (!lease) throw new IdleEstateRefusal('UNKNOWN_LEASE_REFUSED', `Unknown lease ${leaseId}`, { leaseId });
    return lease;
  }
}

export function createAtomVMIdleEstate(options) {
  return new AtomVMIdleEstate(options);
}
