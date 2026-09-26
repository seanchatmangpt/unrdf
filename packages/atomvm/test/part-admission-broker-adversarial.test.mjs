/**
 * @file Boundary and adversarial cases for the mandatory part-admission broker
 * beyond the #109 falsifiers: forged canonical records, malformed host context,
 * stale subject, cross-requirement receipts, wrong digests, duplicate delivery,
 * concurrent reordering, and delegate result forgery.
 *
 * Chicago style: real @unrdf/core records, real @unrdf/receipts receipts, real
 * sha256 for the adversary, and a real in-process delegate whose execution log
 * is the observed state.
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { createPartPassport, createPartRequirement } from '@unrdf/core';
import { createSubstitutionReceipt } from '@unrdf/receipts';
import {
  PartAdmissionBrokerRefusal,
  createPartAdmissionBroker,
} from '../src/part-admission-broker.mjs';

const contract = {
  semanticContract: 'urn:contract:semantic:v1',
  inputContract: 'urn:contract:input:v1',
  outputContract: 'urn:contract:output:v1',
  stateContract: 'urn:contract:state:v1',
  rulesContract: 'urn:contract:rules:v1',
  informationFlowContract: 'urn:contract:flow:v1',
  lifecycleContract: 'urn:contract:lifecycle:v1',
};

function requirementFor(requirementId = 'urn:requirement:atomvm-adversarial') {
  return createPartRequirement({
    requirementId,
    ...contract,
    expectedInputs: ['input:a'],
    requiredGuarantees: ['output:a'],
    allowedFailures: ['failure:timeout'],
    allowedAuthorityIssuers: ['issuer:host'],
    allowedCapabilities: ['cap:read', 'cap:write'],
    maxDelegationDepth: 0,
    resourceCeilings: { memoryMiB: 64 },
    requiredVerifiers: ['verify:semantic'],
    receiptSchema: 'unrdf.execution-receipt/1',
    allowedRuntimes: ['atomvm'],
  });
}

const requirement = requirementFor();

function passport(overrides = {}) {
  return createPartPassport({
    partId: 'urn:part:atomvm-adversarial',
    version: '1.0.0',
    artifactDigest: 'sha256:adversarial-1',
    ...contract,
    acceptedInputs: ['input:a'],
    guarantees: ['output:a'],
    failures: ['failure:timeout'],
    authorityIssuer: 'issuer:host',
    authoritySubject: 'subject:worker',
    capabilities: ['cap:read'],
    resources: { memoryMiB: 32 },
    verifiers: ['verify:semantic'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replay: true,
    runtime: 'atomvm',
    provenance: {
      source: 'ontology://parts/atomvm-adversarial',
      artifactDigest: overrides.artifactDigest ?? 'sha256:adversarial-1',
    },
    ...overrides,
  });
}

const context = {
  hostCapabilities: ['cap:read', 'cap:write'],
  hostAuthorityIssuers: ['issuer:host'],
  hostResourceCeilings: { memoryMiB: 48 },
};

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map(key => [key, canonical(value[key])])
    );
  }
  return value;
}

function sha256(value) {
  return createHash('sha256')
    .update(JSON.stringify(canonical(value)))
    .digest('hex');
}

/** Adversary: edit a record and recompute its unkeyed digest. */
function forge(record, edits) {
  const { digest: _digest, ...body } = { ...record, ...edits };
  return { ...body, digest: sha256(body) };
}

function receiptFor({ req = requirement, candidate, ctx = context, receiptId = 'adversarial' }) {
  return createSubstitutionReceipt({
    requirement: req,
    candidate,
    context: ctx,
    receiptId,
    timestamp: 0,
  });
}

function recordingDelegate(resultFor = request => ({ echoed: request })) {
  const delegate = {
    log: [],
    async execute(request) {
      delegate.log.push(request);
      return resultFor(request);
    },
  };
  return delegate;
}

async function assertRefused(broker, delegate, code, reasonCode) {
  await assert.rejects(
    () => broker.execute({ operation: 'atomvm.execute' }),
    error =>
      error instanceof PartAdmissionBrokerRefusal &&
      error.code === code &&
      (reasonCode === undefined ||
        error.details.judgement.reasons.some(entry => entry.code === reasonCode))
  );
  assert.deepEqual(delegate.log, [], 'delegate must not run without admission');
}

test('forged NaN resource passport with a receipt minted over it never actuates', async () => {
  const forged = forge(passport(), { resources: { memoryMiB: Number.NaN } });
  const receipt = receiptFor({ candidate: forged });
  assert.equal(receipt.action, 'part.substitution.refuse');

  const delegate = recordingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate: forged,
    context,
    substitutionReceipt: receipt,
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_REFUSED', 'PASSPORT_INTEGRITY_REFUSED');
});

test('malformed host context is refused with a typed broker refusal before actuation', async () => {
  const candidate = passport();
  for (const malformed of [
    { ...context, hostCapabilities: 'cap:read cap:write' },
    { ...context, hostResourceCeilings: { memoryMiB: Number.NaN } },
  ]) {
    const delegate = recordingDelegate();
    const broker = createPartAdmissionBroker({
      requirement,
      candidate,
      context: malformed,
      substitutionReceipt: receiptFor({ candidate }),
      delegate,
    });
    await assertRefused(broker, delegate, 'PART_SUBSTITUTION_REFUSED', 'CONTEXT_MALFORMED_REFUSED');
  }
});

test('stale subject: a receipt for the previous version of the same part is refused', async () => {
  const previous = passport();
  const current = passport({ version: '1.0.1', artifactDigest: 'sha256:adversarial-2' });
  assert.equal(previous.partId, current.partId);

  const delegate = recordingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate: current,
    context,
    substitutionReceipt: receiptFor({ candidate: previous }),
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_RECEIPT_INVALID');
});

test('a receipt minted against a different requirement is refused', async () => {
  const candidate = passport();
  const other = requirementFor('urn:requirement:some-other-host');
  const delegate = recordingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: receiptFor({ req: other, candidate }),
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_RECEIPT_INVALID');
});

test('wrong digest: a receipt whose candidate digest was swapped is refused', async () => {
  const candidate = passport();
  const other = passport({ partId: 'urn:part:other', artifactDigest: 'sha256:other' });
  const receipt = receiptFor({ candidate });
  const swapped = {
    ...receipt,
    metadata: { ...receipt.metadata, candidateDigest: other.digest },
  };
  const delegate = recordingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: swapped,
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_RECEIPT_INVALID');
});

test('duplicate delivery and concurrent reordering yield one identical admission per call', async () => {
  const candidate = passport();
  const receipt = receiptFor({ candidate });
  const delegate = recordingDelegate(request => ({ echoed: request }));
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: receipt,
    delegate,
  });

  const order = [5, 0, 7, 2, 2, 6, 1, 4, 3, 5, 0, 7];
  const results = await Promise.all(order.map(sequence => broker.execute({ sequence })));

  assert.equal(delegate.log.length, order.length);
  assert.deepEqual(
    results.map(result => result.echoed.sequence),
    order
  );
  const admissions = new Set(results.map(result => JSON.stringify(result.partAdmission)));
  assert.equal(admissions.size, 1);
  assert.equal(results[0].partAdmission.substitutionReceiptHash, receipt.receiptHash);
  assert.equal(results[0].partAdmission.candidateDigest, candidate.digest);
});

test('a delegate cannot forge its own partAdmission and arrays are wrapped, not spread', async () => {
  const candidate = passport();
  const receipt = receiptFor({ candidate });

  const forging = recordingDelegate(() => ({
    value: 1,
    partAdmission: { candidateDigest: 'forged', substitutionReceiptHash: 'forged' },
  }));
  const forgingBroker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: receipt,
    delegate: forging,
  });
  const forged = await forgingBroker.execute({});
  assert.equal(forged.partAdmission.candidateDigest, candidate.digest);
  assert.equal(forged.partAdmission.substitutionReceiptHash, receipt.receiptHash);
  assert.ok(Object.isFrozen(forged.partAdmission));

  const listing = recordingDelegate(() => ['a', 'b']);
  const listingBroker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: receipt,
    delegate: listing,
  });
  const wrapped = await listingBroker.execute({});
  assert.deepEqual(wrapped.result, ['a', 'b']);
  assert.equal(wrapped.partAdmission.candidateDigest, candidate.digest);
});

test('construction refuses missing bindings and non-data receipts before any broker exists', () => {
  const candidate = passport();
  assert.throws(() => createPartAdmissionBroker({ candidate }), TypeError);
  assert.throws(() => createPartAdmissionBroker({ requirement }), TypeError);
  assert.throws(() => createPartAdmissionBroker(), TypeError);
  assert.throws(
    () =>
      createPartAdmissionBroker({
        requirement,
        candidate,
        context,
        substitutionReceipt: { ...receiptFor({ candidate }), toJSON: () => ({}) },
        delegate: recordingDelegate(),
      }),
    error => error.name === 'DataCloneError'
  );
});

test('a prototype-named undeclared resource demand never actuates, even with a receipt minted over it', async () => {
  for (const key of ['constructor', 'toString', 'valueOf', 'hasOwnProperty', 'isPrototypeOf']) {
    const candidate = passport({ resources: { memoryMiB: 32, [key]: 1e12 } });
    const receipt = receiptFor({ candidate, receiptId: `proto-${key}` });
    assert.equal(receipt.action, 'part.substitution.refuse', key);
    const delegate = recordingDelegate();
    const broker = createPartAdmissionBroker({
      requirement,
      candidate,
      context,
      substitutionReceipt: receipt,
      delegate,
    });
    await assertRefused(broker, delegate, 'PART_SUBSTITUTION_REFUSED', 'RESOURCE_CEILING_REFUSED');
  }
});
