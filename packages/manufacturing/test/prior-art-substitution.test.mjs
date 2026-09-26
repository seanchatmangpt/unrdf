import { createHash } from 'node:crypto';
import { describe, expect, it } from 'vitest';

import {
  createPartPassport,
  createPartRequirement,
} from '@unrdf/core';

import {
  evaluatePriorArtSubstitution,
  replayPriorArtSubstitution,
} from '../src/prior-art-substitution.mjs';

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(
      Object.keys(value).sort().map((key) => [key, canonical(value[key])]),
    );
  }
  return value;
}

function digest(value) {
  return 'sha256:' + createHash('sha256')
    .update(JSON.stringify(canonical(value)))
    .digest('hex');
}

const supplier = Object.freeze({
  kind: 'CodeGraph',
  repository: 'supplier/codegraph',
  commit: '0123456789abcdef0123456789abcdef01234567',
  release: 'codegraph_release_v1',
  release_digest: 'sha256:' + 'a'.repeat(64),
});

function priorArtCandidate(overrides = {}) {
  const body = {
    schema: 'unrdf.epr-candidate/1',
    supplier,
    subject: {
      part_id: 'codegraph:file:1',
      file_id: '1',
      language: 'Python',
    },
    candidate: {
      part_id: 'codegraph:file:2',
      file_id: '2',
      language: 'Rust',
    },
    required_axes: ['algorithm', 'domain'],
    shared_semantics: {
      algorithm: ['wikidata:Q12105'],
      domain: ['wikidata:Q131476'],
    },
    semantic_delta: {},
    semantic_exact: true,
    behavioral_equivalence_claimed: false,
    behavioral_qualification_required: true,
    qualification_receipt: null,
    authority: 'NONE',
    grants_do_authority: false,
    standing: 'CANDIDATE',
    ...overrides,
  };

  return Object.freeze({
    ...body,
    candidate_digest: digest(body),
  });
}

const contract = Object.freeze({
  semanticContract: 'urn:test:semantic:v1',
  inputContract: 'urn:test:input:v1',
  outputContract: 'urn:test:output:v1',
  stateContract: 'urn:test:state:v1',
  rulesContract: 'urn:test:rules:v1',
  informationFlowContract: 'urn:test:flow:v1',
  lifecycleContract: 'urn:test:lifecycle:v1',
});

function requirement() {
  return createPartRequirement({
    requirementId: 'req:semantic-replacement',
    ...contract,
    expectedInputs: ['graph'],
    requiredGuarantees: ['shortest-path'],
    allowedEffects: ['read'],
    allowedFailures: ['timeout'],
    allowedAuthorityIssuers: ['brce'],
    allowedCapabilities: ['select'],
    maxDelegationDepth: 1,
    resourceCeilings: { cpu: 2 },
    requiredVerifiers: ['semantic-court', 'behavioral-court'],
    receiptSchema: 'consumer.receipt/1',
    replayRequired: true,
    allowedRuntimes: ['node'],
  });
}

function passport(record, overrides = {}) {
  const artifactDigest =
    'sha256:' + 'b'.repeat(64);

  return createPartPassport({
    partId: 'part:rust-dijkstra',
    version: '1',
    artifactDigest,
    ...contract,
    acceptedInputs: ['graph'],
    guarantees: ['shortest-path'],
    effects: ['read'],
    failures: ['timeout'],
    authorityIssuer: 'brce',
    authoritySubject: 'consumer:test',
    capabilities: ['select'],
    delegationDepth: 1,
    resources: { cpu: 1 },
    verifiers: ['semantic-court', 'behavioral-court'],
    receiptSchema: 'consumer.receipt/1',
    replay: true,
    runtime: 'node',
    provenance: {
      source: 'manufactured:rust-dijkstra',
      artifactDigest,
      priorArtSupplierRepository: record.supplier.repository,
      priorArtSupplierCommit: record.supplier.commit,
      priorArtReleaseDigest: record.supplier.release_digest,
    },
    metadata: {
      priorArtCandidateDigest: record.candidate_digest,
      priorArtCandidatePartId: record.candidate.part_id,
    },
    ...overrides,
  });
}

describe('prior-art substitution manufacturing bridge', () => {
  it('keeps a behaviorally admitted prior-art candidate at CANDIDATE until receipt', () => {
    const record = priorArtCandidate();
    const candidatePassport = passport(record);
    const evaluation = evaluatePriorArtSubstitution({
      priorArtCandidate: record,
      requirement: requirement(),
      candidatePassport,
    });

    expect(evaluation).toMatchObject({
      behavioral_state: 'ADMITTED',
      receipt_required: true,
      qualification_receipt: null,
      standing: 'CANDIDATE',
      authority: 'NONE',
      grants_do_authority: false,
    });
    expect(evaluation.evaluation_digest).toMatch(/^sha256:[0-9a-f]{64}$/);
  });

  it('refuses semantic equivalence laundering before the behavioral court', () => {
    const record = priorArtCandidate({ behavioral_equivalence_claimed: true });
    const candidatePassport = passport(record);

    expect(() => evaluatePriorArtSubstitution({
      priorArtCandidate: record,
      requirement: requirement(),
      candidatePassport,
    })).toThrow('REFUSED:EPR_EQUIVALENCE_LAUNDERING');
  });

  it('refuses forged candidate evidence and passport/supplier rebinding', () => {
    const record = priorArtCandidate();
    const forged = { ...record, semantic_exact: false };

    expect(() => evaluatePriorArtSubstitution({
      priorArtCandidate: forged,
      requirement: requirement(),
      candidatePassport: passport(record),
    })).toThrow('REFUSED:EPR_CANDIDATE_DIGEST');

    const rebound = passport(record, {
      provenance: {
        source: 'manufactured:rust-dijkstra',
        artifactDigest: 'sha256:' + 'b'.repeat(64),
        priorArtSupplierRepository: record.supplier.repository,
        priorArtSupplierCommit: '89abcdef0123456789abcdef0123456789abcdef',
        priorArtReleaseDigest: record.supplier.release_digest,
      },
    });

    expect(() => evaluatePriorArtSubstitution({
      priorArtCandidate: record,
      requirement: requirement(),
      candidatePassport: rebound,
    })).toThrow('REFUSED:EPR_PASSPORT_SUPPLIER_COMMIT_MISMATCH');
  });

  it('leaves a behavioral consequence widening refused even with exact semantic evidence', () => {
    const record = priorArtCandidate();
    const widened = passport(record, { effects: ['read', 'write'] });

    const evaluation = evaluatePriorArtSubstitution({
      priorArtCandidate: record,
      requirement: requirement(),
      candidatePassport: widened,
    });

    expect(evaluation.behavioral_state).toBe('REFUSED');
    expect(evaluation.falsifier?.code).toBe('CONSEQUENCE_WIDENING_REFUSED');
    expect(evaluation.standing).toBe('CANDIDATE');
    expect(evaluation.qualification_receipt).toBeNull();
  });

  it('replays the exact behavioral evaluation byte-identically', () => {
    const record = priorArtCandidate();
    const input = {
      priorArtCandidate: record,
      requirement: requirement(),
      candidatePassport: passport(record),
    };
    const evaluation = evaluatePriorArtSubstitution(input);
    const replay = replayPriorArtSubstitution(evaluation, input);

    expect(replay).toMatchObject({
      valid: true,
      state: 'CANDIDATE',
      second_run_byte_identical: true,
      receipt_required: true,
      authority: 'NONE',
      grants_do_authority: false,
    });
    expect(replay.observed_evaluation_digest).toBe(evaluation.evaluation_digest);
  });
});
