/**
 * @file Canonical-record falsifiers for the interchangeable-part law.
 *
 * The requirement/passport digest is an unkeyed sha256 of the canonical body,
 * so an adversary can edit any field and recompute a self-consistent digest.
 * These cases play that adversary with the real hash (no doubles) and assert
 * the law refuses every record that its own manufacturer would not produce,
 * never throws on malformed input, and still admits every record it would.
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import {
  PartSubstitutionRefusal,
  assertSubstitutable,
  createPartPassport,
  createPartRequirement,
  evaluateSubstitution,
  verifyPartPassport,
  verifyPartRequirement,
} from '../src/interchangeable-part.mjs';

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
  requirementId: 'urn:requirement:canonical',
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

function passport(overrides = {}) {
  return createPartPassport({
    partId: 'urn:part:canonical',
    version: '1.0.0',
    artifactDigest: 'sha256:canonical',
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
    provenance: { source: 'ontology://parts/canonical', artifactDigest: 'sha256:canonical' },
    ...overrides,
  });
}

const host = { hostResourceCeilings: { memoryMiB: 48 } };

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

/** The adversary: edit a record, then recompute its unkeyed digest. */
function forge(record, edits) {
  const { digest: _digest, ...body } = { ...record, ...edits };
  const digest = createHash('sha256')
    .update(JSON.stringify(canonical(body)))
    .digest('hex');
  return { ...body, digest };
}

function reasonCodes(judgement) {
  return judgement.reasons.map(entry => entry.code);
}

test('NaN resource demand with a recomputed digest is refused, not admitted under every ceiling', () => {
  const forged = forge(passport(), { resources: { memoryMiB: Number.NaN } });
  assert.equal(verifyPartPassport(forged).valid, false);
  assert.equal(verifyPartPassport(forged).reason, 'MALFORMED_RECORD');

  const judgement = evaluateSubstitution(requirement, forged, host);
  assert.equal(judgement.state, 'REFUSED');
  assert.deepEqual(reasonCodes(judgement), ['PASSPORT_INTEGRITY_REFUSED']);
  assert.throws(
    () => assertSubstitutable(requirement, forged, host),
    error => error instanceof PartSubstitutionRefusal
  );
});

test('negative resource demand and negative delegation depth are refused even with valid digests', () => {
  for (const edits of [
    { resources: { memoryMiB: -1 } },
    { delegationDepth: -5 },
    { delegationDepth: 0.5 },
    { replay: 'yes' },
  ]) {
    const forged = forge(passport(), edits);
    const judgement = evaluateSubstitution(requirement, forged, host);
    assert.equal(judgement.state, 'REFUSED', JSON.stringify(edits));
    assert.deepEqual(reasonCodes(judgement), ['PASSPORT_INTEGRITY_REFUSED'], JSON.stringify(edits));
  }
});

test('structurally malformed passports are refused totally instead of crashing the law', () => {
  for (const edits of [
    { capabilities: 'cap:read' },
    { effects: { length: 0 } },
    { resources: [32] },
    { provenance: null },
    { authorityIssuer: 7 },
  ]) {
    const forged = forge(passport(), edits);
    let judgement;
    assert.doesNotThrow(() => {
      judgement = evaluateSubstitution(requirement, forged, host);
    }, JSON.stringify(edits));
    assert.equal(judgement.state, 'REFUSED');
    assert.equal(judgement.falsifier.code, 'PASSPORT_INTEGRITY_REFUSED');
  }
});

test('non-canonical but valid-typed records (extra keys, unsorted or duplicate sets) are refused', () => {
  const cases = [
    [{ smuggled: 'cap:admin' }, 'NON_CANONICAL_RECORD'],
    [{ capabilities: ['cap:write', 'cap:read'] }, 'NON_CANONICAL_RECORD'],
    [{ verifiers: ['verify:semantic', 'verify:semantic'] }, 'NON_CANONICAL_RECORD'],
  ];
  for (const [edits, expected] of cases) {
    const forged = forge(passport(), edits);
    assert.equal(verifyPartPassport(forged).reason, expected, JSON.stringify(edits));
    assert.equal(evaluateSubstitution(requirement, forged, host).state, 'REFUSED');
  }
});

test('a requirement with a NaN ceiling cannot admit an unbounded resource demand', () => {
  const forgedRequirement = forge(requirement, { resourceCeilings: { memoryMiB: Number.NaN } });
  const greedy = passport({ resources: { memoryMiB: 1e9 } });
  const judgement = evaluateSubstitution(forgedRequirement, greedy, {});
  assert.equal(verifyPartRequirement(forgedRequirement).valid, false);
  assert.equal(judgement.state, 'REFUSED');
  assert.deepEqual(reasonCodes(judgement), ['REQUIREMENT_INTEGRITY_REFUSED']);
});

test('malformed host context is a typed refusal, never an untyped exception', () => {
  for (const context of [
    null,
    [],
    'cap:read',
    { hostCapabilities: 'cap:read cap:write' },
    { hostAuthorityIssuers: [7] },
    { hostResourceCeilings: { memoryMiB: Number.NaN } },
    { hostResourceCeilings: { memoryMiB: -1 } },
  ]) {
    let judgement;
    assert.doesNotThrow(() => {
      judgement = evaluateSubstitution(requirement, passport(), context);
    }, JSON.stringify(context));
    assert.equal(judgement.state, 'REFUSED', JSON.stringify(context));
    assert.deepEqual(
      reasonCodes(judgement),
      ['CONTEXT_MALFORMED_REFUSED'],
      JSON.stringify(context)
    );
  }
});

test('every manufactured record is a fixed point: hardening does not refuse lawful parts', () => {
  let admitted = 0;
  for (const memoryMiB of [0, 1, 16, 47.5, 48]) {
    for (const capabilities of [[], ['cap:read'], ['cap:write', 'cap:read', 'cap:read']]) {
      for (const metadata of [{}, { z: 1, a: [2, 1] }]) {
        const candidate = passport({ resources: { memoryMiB }, capabilities, metadata });
        assert.equal(verifyPartPassport(candidate).valid, true);
        assert.equal(evaluateSubstitution(requirement, candidate, host).state, 'ADMITTED');
        admitted += 1;
      }
    }
  }
  assert.equal(admitted, 30);
  assert.equal(verifyPartRequirement(requirement).valid, true);
});

test('judgement is invariant under host-context key and set reordering', () => {
  const candidate = passport();
  const a = evaluateSubstitution(requirement, candidate, {
    hostCapabilities: ['cap:read', 'cap:write'],
    hostAuthorityIssuers: ['issuer:host'],
    hostResourceCeilings: { memoryMiB: 48, cpuMs: 10 },
  });
  const b = evaluateSubstitution(requirement, candidate, {
    hostResourceCeilings: { cpuMs: 10, memoryMiB: 48 },
    hostAuthorityIssuers: ['issuer:host', 'issuer:host'],
    hostCapabilities: ['cap:write', 'cap:read'],
  });
  assert.equal(a.state, 'ADMITTED');
  assert.equal(a.digest, b.digest);
});
