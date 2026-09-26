import test from 'node:test';
import assert from 'node:assert/strict';
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

function fixture(candidateOverrides = {}) {
  const requirement = createPartRequirement({
    requirementId: 'urn:requirement:atomvm-part',
    ...contract,
    expectedInputs: ['input:a'],
    requiredGuarantees: ['output:a'],
    allowedEffects: [],
    allowedFailures: ['failure:timeout'],
    allowedAuthorityIssuers: ['issuer:host'],
    allowedCapabilities: ['cap:read'],
    maxDelegationDepth: 0,
    resourceCeilings: { memoryMiB: 64 },
    requiredVerifiers: ['verify:semantic'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replayRequired: true,
    allowedRuntimes: ['atomvm'],
  });
  const candidate = createPartPassport({
    partId: 'urn:part:atomvm-worker',
    version: '1.0.0',
    artifactDigest: 'sha256:atomvm-worker',
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
    provenance: {
      source: 'ontology://parts/atomvm-worker',
      artifactDigest: 'sha256:atomvm-worker',
    },
    ...candidateOverrides,
  });
  const context = {
    hostCapabilities: ['cap:read'],
    hostAuthorityIssuers: ['issuer:host'],
    hostResourceCeilings: { memoryMiB: 48 },
  };
  return { requirement, candidate, context };
}

test('executes delegate only after fresh admission and receipt verification', async () => {
  const { requirement, candidate, context } = fixture();
  const substitutionReceipt = createSubstitutionReceipt({
    requirement,
    candidate,
    context,
    receiptId: 'atomvm-admission',
    timestamp: 0,
  });
  let executions = 0;
  const delegate = {
    async execute(request) {
      executions += 1;
      return { marker: 'atomvm_swarm_alive', request };
    },
  };
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt,
    delegate,
  });

  const result = await broker.execute({ operation: 'atomvm.execute' });
  assert.equal(executions, 1);
  assert.equal(result.marker, 'atomvm_swarm_alive');
  assert.equal(result.partAdmission.candidateDigest, candidate.digest);
  assert.equal(result.partAdmission.substitutionReceiptHash, substitutionReceipt.receiptHash);
});

test('zero unreceipted actuation prevents delegate execution', async () => {
  const { requirement, candidate, context } = fixture();
  let executions = 0;
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    delegate: { execute: async () => { executions += 1; } },
  });

  await assert.rejects(
    () => broker.execute({ operation: 'atomvm.execute' }),
    error => error instanceof PartAdmissionBrokerRefusal
      && error.code === 'PART_SUBSTITUTION_RECEIPT_REQUIRED',
  );
  assert.equal(executions, 0);
});

test('host authority drift invalidates a previously admitted receipt before actuation', async () => {
  const { requirement, candidate, context } = fixture();
  const substitutionReceipt = createSubstitutionReceipt({
    requirement,
    candidate,
    context,
    receiptId: 'host-bound',
    timestamp: 0,
  });
  let executions = 0;
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context: {
      ...context,
      hostCapabilities: [],
    },
    substitutionReceipt,
    delegate: { execute: async () => { executions += 1; } },
  });

  await assert.rejects(
    () => broker.execute({ operation: 'atomvm.execute' }),
    error => error instanceof PartAdmissionBrokerRefusal
      && error.code === 'PART_SUBSTITUTION_REFUSED',
  );
  assert.equal(executions, 0);
});

test('tampered admission receipt is refused before delegate execution', async () => {
  const { requirement, candidate, context } = fixture();
  const receipt = createSubstitutionReceipt({
    requirement,
    candidate,
    context,
    receiptId: 'tamper-test',
    timestamp: 0,
  });
  const substitutionReceipt = {
    ...receipt,
    metadata: { ...receipt.metadata, candidateDigest: 'tampered' },
  };
  let executions = 0;
  const broker = createPartAdmissionBroker({
    requirement,
    candidate,
    context,
    substitutionReceipt,
    delegate: { execute: async () => { executions += 1; } },
  });

  await assert.rejects(
    () => broker.execute({ operation: 'atomvm.execute' }),
    error => error instanceof PartAdmissionBrokerRefusal
      && error.code === 'PART_SUBSTITUTION_RECEIPT_INVALID',
  );
  assert.equal(executions, 0);
});
