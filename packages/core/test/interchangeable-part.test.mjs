import test from 'node:test';
import assert from 'node:assert/strict';
import {
  PartSubstitutionRefusal,
  assertSubstitutable,
  createPartPassport,
  createPartRequirement,
  evaluateSubstitution,
  verifyPartPassport,
} from '../src/interchangeable-part.mjs';

function requirement(overrides = {}) {
  return createPartRequirement({
    requirementId: 'urn:test:requirement:v1',
    semanticContract: 'urn:contract:semantic:v1',
    inputContract: 'urn:contract:input:v1',
    outputContract: 'urn:contract:output:v1',
    stateContract: 'urn:contract:state:v1',
    rulesContract: 'urn:contract:rules:v1',
    informationFlowContract: 'urn:contract:flow:v1',
    lifecycleContract: 'urn:contract:lifecycle:v1',
    expectedInputs: ['input:a'],
    requiredGuarantees: ['output:normalized'],
    allowedEffects: ['effect:write-cache'],
    allowedFailures: ['failure:timeout'],
    allowedAuthorityIssuers: ['issuer:host'],
    allowedCapabilities: ['cap:read', 'cap:write-cache'],
    maxDelegationDepth: 1,
    resourceCeilings: { memoryMiB: 128, cpuMs: 50 },
    requiredVerifiers: ['verify:semantic', 'verify:replay'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replayRequired: true,
    allowedRuntimes: ['wasm', 'atomvm'],
    ...overrides,
  });
}

function passport(overrides = {}) {
  return createPartPassport({
    partId: 'urn:part:normalizer',
    version: '1.2.3',
    artifactDigest: 'sha256:artifact',
    semanticContract: 'urn:contract:semantic:v1',
    inputContract: 'urn:contract:input:v1',
    outputContract: 'urn:contract:output:v1',
    stateContract: 'urn:contract:state:v1',
    rulesContract: 'urn:contract:rules:v1',
    informationFlowContract: 'urn:contract:flow:v1',
    lifecycleContract: 'urn:contract:lifecycle:v1',
    acceptedInputs: ['input:a', 'input:b'],
    guarantees: ['output:normalized', 'output:typed'],
    effects: ['effect:write-cache'],
    failures: ['failure:timeout'],
    authorityIssuer: 'issuer:host',
    authoritySubject: 'subject:normalizer',
    capabilities: ['cap:read'],
    delegationDepth: 0,
    resources: { memoryMiB: 64, cpuMs: 20 },
    verifiers: ['verify:semantic', 'verify:replay', 'verify:extra'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replay: true,
    runtime: 'wasm',
    provenance: {
      source: 'ontology://parts/normalizer',
      artifactDigest: 'sha256:artifact',
    },
    ...overrides,
  });
}

test('admits consequence-preserving substitution with narrower authority and resources', () => {
  const result = evaluateSubstitution(requirement(), passport(), {
    hostCapabilities: ['cap:read'],
    hostAuthorityIssuers: ['issuer:host'],
    hostResourceCeilings: { memoryMiB: 96, cpuMs: 25 },
  });

  assert.equal(result.state, 'ADMITTED');
  assert.deepEqual(result.reasons, []);
  assert.deepEqual(result.effectiveAuthority.capabilities, ['cap:read']);
  assert.deepEqual(result.effectiveResourceCeilings, { cpuMs: 25, memoryMiB: 96 });
});

test('refuses API-compatible part that widens consequences or authority', () => {
  const candidate = passport({
    effects: ['effect:write-cache', 'effect:network'],
    capabilities: ['cap:read', 'cap:root'],
  });
  const result = evaluateSubstitution(requirement(), candidate);

  assert.equal(result.state, 'REFUSED');
  assert.ok(result.reasons.some(item => item.code === 'CONSEQUENCE_WIDENING_REFUSED'));
  assert.ok(result.reasons.some(item => item.code === 'AUTHORITY_WIDENING_REFUSED'));
});

test('refuses resource, failure, verifier, and replay drift', () => {
  const candidate = passport({
    failures: ['failure:timeout', 'failure:data-loss'],
    resources: { memoryMiB: 256, cpuMs: 20 },
    verifiers: ['verify:semantic'],
    replay: false,
  });
  const result = evaluateSubstitution(requirement(), candidate);
  const codes = result.reasons.map(item => item.code);

  assert.ok(codes.includes('FAILURE_WIDENING_REFUSED'));
  assert.ok(codes.includes('RESOURCE_CEILING_REFUSED'));
  assert.ok(codes.includes('EVIDENCE_INCOMPLETE_REFUSED'));
  assert.ok(codes.includes('REPLAY_REQUIRED_REFUSED'));
});

test('exact contract mismatch is a substitution falsifier', () => {
  const result = evaluateSubstitution(requirement(), passport({
    informationFlowContract: 'urn:contract:flow:weaker',
  }));

  assert.equal(result.state, 'REFUSED');
  assert.equal(result.falsifier.code, 'INFORMATION_FLOW_CONTRACT_MISMATCH');
});

test('passport digest detects post-manufacture drift', () => {
  const admitted = passport();
  const changed = { ...admitted, runtime: 'node' };

  assert.equal(verifyPartPassport(admitted).valid, true);
  assert.deepEqual(verifyPartPassport(changed), { valid: false, reason: 'DIGEST_MISMATCH' });
});

test('assertSubstitutable returns judgement or typed refusal', () => {
  assert.equal(assertSubstitutable(requirement(), passport()).state, 'ADMITTED');

  assert.throws(
    () => assertSubstitutable(requirement(), passport({ runtime: 'node' })),
    error => error instanceof PartSubstitutionRefusal
      && error.code === 'PART_SUBSTITUTION_REFUSED'
      && error.judgement.reasons.some(item => item.code === 'RUNTIME_REFUSED'),
  );
});
