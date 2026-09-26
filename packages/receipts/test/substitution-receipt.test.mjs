import test from 'node:test';
import assert from 'node:assert/strict';
import { createPartPassport, createPartRequirement } from '@unrdf/core';
import {
  createSubstitutionReceipt,
  verifySubstitutionReceipt,
} from '../src/substitution-receipt.mjs';

const contract = {
  semanticContract: 'urn:contract:semantic:v1',
  inputContract: 'urn:contract:input:v1',
  outputContract: 'urn:contract:output:v1',
  stateContract: 'urn:contract:state:v1',
  rulesContract: 'urn:contract:rules:v1',
  informationFlowContract: 'urn:contract:flow:v1',
  lifecycleContract: 'urn:contract:lifecycle:v1',
};

function requirement() {
  return createPartRequirement({
    requirementId: 'urn:requirement:receipt-test',
    ...contract,
    expectedInputs: ['input:a'],
    requiredGuarantees: ['output:a'],
    allowedEffects: ['effect:cache'],
    allowedFailures: ['failure:timeout'],
    allowedAuthorityIssuers: ['issuer:host'],
    allowedCapabilities: ['cap:read'],
    maxDelegationDepth: 0,
    resourceCeilings: { memoryMiB: 64 },
    requiredVerifiers: ['verify:semantic'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replayRequired: true,
    allowedRuntimes: ['wasm'],
  });
}

function passport(overrides = {}) {
  return createPartPassport({
    partId: 'urn:part:receipt-test',
    version: '1.0.0',
    artifactDigest: 'sha256:receipt-test',
    ...contract,
    acceptedInputs: ['input:a'],
    guarantees: ['output:a'],
    effects: ['effect:cache'],
    failures: ['failure:timeout'],
    authorityIssuer: 'issuer:host',
    authoritySubject: 'subject:part',
    capabilities: ['cap:read'],
    delegationDepth: 0,
    resources: { memoryMiB: 32 },
    verifiers: ['verify:semantic'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replay: true,
    runtime: 'wasm',
    provenance: {
      source: 'ontology://receipt-test',
      artifactDigest: 'sha256:receipt-test',
    },
    ...overrides,
  });
}

test('receipts admitted substitution and re-verifies the exact judgement', () => {
  const req = requirement();
  const candidate = passport();
  const context = {
    hostCapabilities: ['cap:read'],
    hostAuthorityIssuers: ['issuer:host'],
    hostResourceCeilings: { memoryMiB: 48 },
  };
  const receipt = createSubstitutionReceipt({
    requirement: req,
    candidate,
    context,
    receiptId: 'receipt-admitted',
    timestamp: 0,
  });

  assert.equal(receipt.schema, 'unrdf.part-substitution-receipt/1');
  assert.equal(receipt.action, 'part.substitution.admit');
  assert.equal(receipt.status, 'success');
  assert.equal(receipt.exitCode, 0);

  const verification = verifySubstitutionReceipt(receipt, {
    requirement: req,
    candidate,
    context,
  });
  assert.equal(verification.valid, true);
  assert.equal(verification.state, 'ADMITTED');
});

test('receipts typed refusal and preserves the falsifier', () => {
  const req = requirement();
  const candidate = passport({ capabilities: ['cap:read', 'cap:root'] });
  const receipt = createSubstitutionReceipt({
    requirement: req,
    candidate,
    receiptId: 'receipt-refused',
    timestamp: 0,
  });

  assert.equal(receipt.action, 'part.substitution.refuse');
  assert.equal(receipt.status, 'refused');
  assert.equal(receipt.exitCode, 2);
  assert.equal(receipt.metadata.falsifier.code, 'AUTHORITY_WIDENING_REFUSED');

  const verification = verifySubstitutionReceipt(receipt, {
    requirement: req,
    candidate,
  });
  assert.equal(verification.valid, true);
  assert.equal(verification.state, 'REFUSED');
});

test('receipt does not verify against a different candidate or host boundary', () => {
  const req = requirement();
  const candidate = passport();
  const receipt = createSubstitutionReceipt({
    requirement: req,
    candidate,
    receiptId: 'receipt-bound',
    timestamp: 0,
  });

  const replacement = passport({
    partId: 'urn:part:receipt-test-v2',
    version: '2.0.0',
  });

  const verification = verifySubstitutionReceipt(receipt, {
    requirement: req,
    candidate: replacement,
  });
  assert.equal(verification.valid, false);
  assert.ok(verification.errors.includes('candidate subject mismatch'));
  assert.ok(verification.errors.includes('candidate digest mismatch'));
});
