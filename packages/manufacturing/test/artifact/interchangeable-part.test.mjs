import test from 'node:test';
import assert from 'node:assert/strict';
import { createPartRequirement, evaluateSubstitution } from '@unrdf/core';
import { createArtifact } from '../../src/artifact/index.mjs';
import { manufacturePartPassport } from '../../src/part-passport.mjs';

const contract = {
  semanticContract: 'urn:contract:semantic:v1',
  inputContract: 'urn:contract:input:v1',
  outputContract: 'urn:contract:output:v1',
  stateContract: 'urn:contract:state:v1',
  rulesContract: 'urn:contract:rules:v1',
  informationFlowContract: 'urn:contract:flow:v1',
  lifecycleContract: 'urn:contract:lifecycle:v1',
};

test('manufactures deterministic passport identity from artifact receipt and ontology provenance', () => {
  const artifact = createArtifact('json', { result: 'normalized' });
  const manufactured = manufacturePartPassport({
    artifact,
    ontologySource: 'ontology://parts/normalizer',
    part: {
      partId: 'urn:part:normalizer',
      version: '1.0.0',
      ...contract,
      acceptedInputs: ['input:a'],
      guarantees: ['output:normalized'],
      effects: [],
      failures: ['failure:timeout'],
      authorityIssuer: 'issuer:host',
      authoritySubject: 'subject:normalizer',
      capabilities: ['cap:read'],
      delegationDepth: 0,
      resources: { memoryMiB: 32 },
      verifiers: ['verify:semantic', 'verify:replay'],
      receiptSchema: 'unrdf.execution-receipt/1',
      replay: true,
      runtime: 'wasm',
    },
  });

  assert.equal(manufactured.passport.artifactDigest, artifact.receipt);
  assert.equal(manufactured.passport.provenance.source, 'ontology://parts/normalizer');
  assert.equal(manufactured.passport.provenance.artifactDigest, artifact.receipt);
  assert.equal(manufactured.causality.depth, 1);

  const requirement = createPartRequirement({
    requirementId: 'urn:requirement:normalizer',
    ...contract,
    expectedInputs: ['input:a'],
    requiredGuarantees: ['output:normalized'],
    allowedEffects: [],
    allowedFailures: ['failure:timeout'],
    allowedAuthorityIssuers: ['issuer:host'],
    allowedCapabilities: ['cap:read'],
    maxDelegationDepth: 0,
    resourceCeilings: { memoryMiB: 64 },
    requiredVerifiers: ['verify:semantic', 'verify:replay'],
    receiptSchema: 'unrdf.execution-receipt/1',
    replayRequired: true,
    allowedRuntimes: ['wasm'],
  });

  assert.equal(evaluateSubstitution(requirement, manufactured.passport).state, 'ADMITTED');
});

test('artifact receipt overrides caller-supplied digest so identity cannot drift from manufactured output', () => {
  const artifact = createArtifact('json', { stable: true });
  const manufactured = manufacturePartPassport({
    artifact,
    ontologySource: 'ontology://parts/stable',
    part: {
      partId: 'urn:part:stable',
      version: '1.0.0',
      artifactDigest: 'sha256:spoofed',
      provenance: { source: 'spoofed', artifactDigest: 'sha256:spoofed' },
      ...contract,
      acceptedInputs: [],
      guarantees: [],
      effects: [],
      failures: [],
      authorityIssuer: 'issuer:host',
      authoritySubject: 'subject:stable',
      capabilities: [],
      resources: {},
      verifiers: [],
      receiptSchema: 'unrdf.execution-receipt/1',
      replay: true,
      runtime: 'wasm',
    },
  });

  assert.equal(manufactured.passport.artifactDigest, artifact.receipt);
  assert.equal(manufactured.passport.provenance.source, 'ontology://parts/stable');
});
