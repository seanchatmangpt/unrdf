import { describe, expect, it } from 'vitest';

import {
  EPR_CANDIDATE_SCHEMA,
  fromCodeGraphTables,
  manufacturePriorArtCandidate,
  serializePriorArtCandidate,
  verifyPriorArtCandidate,
} from '../src/index.mjs';

const supplier = Object.freeze({
  repository: 'supplier/codegraph',
  commit: '0123456789abcdef0123456789abcdef01234567',
  release_digest: 'sha256:' + 'a'.repeat(64),
});

function graph() {
  return fromCodeGraphTables({
    files: [
      { file_id: 1, sample_id: 'py', language: 'Python' },
      { file_id: 2, sample_id: 'rs', language: 'Rust' },
      { file_id: 3, sample_id: 'go', language: 'Go' },
    ],
    concepts: {
      algorithms: [
        { concept_id: 10, name: 'dijkstra', wikidata_qid: 'Q12105' },
        { concept_id: 11, name: 'bellman-ford', wikidata_qid: 'Q294195' },
      ],
      domains: [
        { concept_id: 20, name: 'graph theory', wikidata_qid: 'Q131476' },
      ],
      paradigms: [],
      design_patterns: [],
    },
    edges: {
      file_algorithm: [
        { file_id: 1, concept_id: 10 },
        { file_id: 2, concept_id: 10 },
        { file_id: 3, concept_id: 11 },
      ],
      file_domain: [
        { file_id: 1, concept_id: 20 },
        { file_id: 2, concept_id: 20 },
        { file_id: 3, concept_id: 20 },
      ],
      file_paradigm: [],
      file_design_pattern: [],
    },
  });
}

describe('EPR prior-art candidate manufacture', () => {
  it('binds CodeGraph classification to an immutable supplier without laundering equivalence', () => {
    const candidate = manufacturePriorArtCandidate(graph(), '1', '2', {
      requiredAxes: ['algorithm', 'domain'],
      supplier,
    });

    expect(candidate).toMatchObject({
      schema: EPR_CANDIDATE_SCHEMA,
      supplier: {
        repository: supplier.repository,
        commit: supplier.commit,
        release_digest: supplier.release_digest,
      },
      subject: { part_id: 'codegraph:file:1' },
      candidate: { part_id: 'codegraph:file:2' },
      behavioral_equivalence_claimed: false,
      behavioral_qualification_required: true,
      qualification_receipt: null,
      authority: 'NONE',
      grants_do_authority: false,
      standing: 'CANDIDATE',
    });
    expect(candidate.candidate_digest).toMatch(/^sha256:[0-9a-f]{64}$/);
  });

  it('refuses mutable supplier refs and unbound release evidence', () => {
    expect(() => manufacturePriorArtCandidate(graph(), '1', '2', {
      supplier: { ...supplier, commit: 'main' },
    })).toThrow('REFUSED_EPR_MUTABLE_SUPPLIER_REF');

    expect(() => manufacturePriorArtCandidate(graph(), '1', '2', {
      supplier: { ...supplier, release_digest: 'sha256:PENDING' },
    })).toThrow('REFUSED_EPR_RELEASE_DIGEST');
  });

  it('refuses semantic adjacency when any required axis fails', () => {
    expect(() => manufacturePriorArtCandidate(graph(), '1', '3', {
      requiredAxes: ['algorithm', 'domain'],
      supplier,
    })).toThrow('REFUSED_EPR_SEMANTIC_FALSIFIER:algorithm');
  });

  it('replays byte-identically against the exact graph and supplier', () => {
    const source = graph();
    const candidate = manufacturePriorArtCandidate(source, '1', '2', {
      requiredAxes: ['algorithm', 'domain'],
      supplier,
    });
    const verification = verifyPriorArtCandidate(candidate, source, { supplier });

    expect(verification).toMatchObject({
      valid: true,
      state: 'CANDIDATE',
      second_run_byte_identical: true,
      authority: 'NONE',
      grants_do_authority: false,
    });
    expect(verification.replay_digest).toBe(candidate.candidate_digest);
    expect(serializePriorArtCandidate(candidate))
      .toBe(serializePriorArtCandidate(
        manufacturePriorArtCandidate(source, '1', '2', {
          requiredAxes: ['algorithm', 'domain'],
          supplier,
        }),
      ));
  });

  it('detects candidate-record mutation and supplier drift on replay', () => {
    const source = graph();
    const candidate = manufacturePriorArtCandidate(source, '1', '2', {
      requiredAxes: ['algorithm', 'domain'],
      supplier,
    });

    expect(verifyPriorArtCandidate(
      { ...candidate, behavioral_equivalence_claimed: true },
      source,
      { supplier },
    )).toMatchObject({
      valid: false,
      state: 'REFUSED',
      reason: 'EPR_REPLAY_MISMATCH',
    });

    expect(verifyPriorArtCandidate(candidate, source, {
      supplier: { ...supplier, commit: '89abcdef0123456789abcdef0123456789abcdef' },
    })).toMatchObject({
      valid: false,
      state: 'REFUSED',
      reason: 'EPR_REPLAY_MISMATCH',
    });
  });
});
