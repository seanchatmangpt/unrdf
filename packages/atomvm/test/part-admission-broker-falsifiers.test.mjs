/**
 * @file Adversarial falsifiers for the mandatory part-admission broker.
 *
 * Chicago style: real @unrdf/core passports/requirements, real @unrdf/receipts
 * receipts, a real in-process delegate whose execution count is the observed
 * state. Every case asserts the delegate never ran (zero unreceipted actuation)
 * and the typed refusal code that stopped it.
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import { createPartPassport, createPartRequirement } from '@unrdf/core';
import { createSubstitutionReceipt, verifySubstitutionReceipt } from '@unrdf/receipts';
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

const requirement = createPartRequirement({
  requirementId: 'urn:requirement:atomvm-falsifier',
  ...contract,
  expectedInputs: ['input:a'],
  requiredGuarantees: ['output:a'],
  allowedEffects: [],
  allowedFailures: ['failure:timeout'],
  allowedAuthorityIssuers: ['issuer:host'],
  allowedCapabilities: ['cap:read', 'cap:write'],
  maxDelegationDepth: 0,
  resourceCeilings: { memoryMiB: 64 },
  requiredVerifiers: ['verify:semantic'],
  receiptSchema: 'unrdf.execution-receipt/1',
  replayRequired: true,
  allowedRuntimes: ['atomvm'],
});

function passport(overrides = {}) {
  const artifactDigest = overrides.artifactDigest ?? 'sha256:atomvm-worker-a';
  return createPartPassport({
    partId: 'urn:part:atomvm-worker-a',
    version: '1.0.0',
    artifactDigest,
    ...contract,
    acceptedInputs: ['input:a'],
    guarantees: ['output:a'],
    effects: [],
    failures: ['failure:timeout'],
    authorityIssuer: 'issuer:host',
    authoritySubject: 'subject:worker',
    capabilities: ['cap:read'],
    delegationDepth: 0,
    resources: { memoryMiB: 32 },
    verifiers: ['verify:semantic'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replay: true,
    runtime: 'atomvm',
    provenance: { source: 'ontology://parts/atomvm-worker', artifactDigest },
    ...overrides,
  });
}

const context = {
  hostCapabilities: ['cap:read', 'cap:write'],
  hostAuthorityIssuers: ['issuer:host'],
  hostResourceCeilings: { memoryMiB: 48 },
};

function receiptFor(candidate, ctx = context, receiptId = 'falsifier') {
  return createSubstitutionReceipt({
    requirement,
    candidate,
    context: ctx,
    receiptId,
    timestamp: 0,
  });
}

function countingDelegate() {
  const delegate = {
    executions: 0,
    async execute(request) {
      delegate.executions += 1;
      return { request };
    },
  };
  return delegate;
}

async function assertRefused(broker, delegate, code) {
  await assert.rejects(
    () => broker.execute({ operation: 'atomvm.execute' }),
    error => error instanceof PartAdmissionBrokerRefusal && error.code === code,
  );
  assert.equal(delegate.executions, 0, 'delegate must not run without admission');
}

test('receipt for one admitted part cannot actuate a different admitted part', async () => {
  const partA = passport();
  const partB = passport({
    partId: 'urn:part:atomvm-worker-b',
    artifactDigest: 'sha256:atomvm-worker-b',
  });
  // Both parts are independently substitutable: the refusal must come from receipt binding.
  assert.equal(verifySubstitutionReceipt(receiptFor(partB), { requirement, candidate: partB, context }).valid, true);

  const delegate = countingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate: partB,
    context,
    substitutionReceipt: receiptFor(partA),
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_RECEIPT_INVALID');
});

test('receipt minted under a different still-admitting host boundary is refused', async () => {
  const candidate = passport();
  const wideHost = { ...context, hostResourceCeilings: { memoryMiB: 64 } };
  const delegate = countingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: receiptFor(candidate, wideHost),
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_RECEIPT_INVALID');
});

test('refusal receipt cannot be replayed as an admission once the host widens', async () => {
  const candidate = passport({ capabilities: ['cap:read', 'cap:write'] });
  const narrowHost = { ...context, hostCapabilities: ['cap:read'] };
  const refusal = receiptFor(candidate, narrowHost, 'refusal');
  assert.equal(refusal.action, 'part.substitution.refuse');

  const delegate = countingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: refusal,
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_RECEIPT_INVALID');
});

test('passport edited after manufacture is refused on integrity before receipts are consulted', async () => {
  const candidate = passport();
  const forged = { ...candidate, capabilities: ['cap:read', 'cap:write', 'cap:admin'] };
  const delegate = countingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate: forged,
    context,
    substitutionReceipt: receiptFor(candidate),
    delegate,
  });
  await assert.rejects(
    () => broker.execute({ operation: 'atomvm.execute' }),
    error => error instanceof PartAdmissionBrokerRefusal
      && error.code === 'PART_SUBSTITUTION_REFUSED'
      && error.details.judgement.reasons.some(r => r.code === 'PASSPORT_INTEGRITY_REFUSED'),
  );
  assert.equal(delegate.executions, 0);
});

test('receipt hash tampering is refused before actuation', async () => {
  const candidate = passport();
  const receipt = receiptFor(candidate);
  const delegate = countingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: { ...receipt, receiptHash: '0'.repeat(64) },
    delegate,
  });
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_RECEIPT_INVALID');
});

test('mutating the caller context after construction cannot widen the admitted boundary', async () => {
  const candidate = passport({ resources: { memoryMiB: 60 } });
  const callerContext = structuredClone(context);
  const delegate = countingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context: callerContext,
    substitutionReceipt: receiptFor(candidate, { ...context, hostResourceCeilings: { memoryMiB: 64 } }),
    delegate,
  });
  callerContext.hostResourceCeilings.memoryMiB = 64;
  await assertRefused(broker, delegate, 'PART_SUBSTITUTION_REFUSED');
});

test('admitted execution is bound to the exact candidate and receipt it verified', async () => {
  const candidate = passport();
  const receipt = receiptFor(candidate);
  const delegate = countingDelegate();
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt: receipt,
    delegate,
  });
  const result = await broker.execute({ operation: 'atomvm.execute' });
  assert.equal(delegate.executions, 1);
  assert.equal(result.partAdmission.requirementDigest, requirement.digest);
  assert.equal(result.partAdmission.candidateDigest, candidate.digest);
  assert.equal(result.partAdmission.substitutionReceiptHash, receipt.receiptHash);
  assert.ok(Object.isFrozen(result));
});
