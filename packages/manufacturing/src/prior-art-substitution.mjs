/**
 * @file Bridge EPR supplier candidates into canonical interchangeable-parts law.
 * @module @unrdf/manufacturing/prior-art-substitution
 *
 * Semantic classification is discovery evidence only. This module does not
 * trust semantic overlap as behavioral equivalence and does not manufacture
 * PartPassports. A separately manufactured passport must bind the exact EPR
 * candidate digest and supplier subject before the canonical core
 * evaluateSubstitution court is invoked.
 */

import { createHash } from 'node:crypto';
import { evaluateSubstitution } from '@unrdf/core';

export const EPR_CANDIDATE_SCHEMA = 'unrdf.epr-candidate/1';
export const EPR_SUBSTITUTION_EVALUATION_SCHEMA =
  'unrdf.epr-substitution-evaluation/1';

const SHA40 = /^[0-9a-f]{40}$/u;
const SHA256 = /^sha256:[0-9a-f]{64}$/u;

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

function verifyCandidateDigest(record) {
  if (!record || record.schema !== EPR_CANDIDATE_SCHEMA) {
    throw new Error('REFUSED:EPR_CANDIDATE_SCHEMA');
  }
  const { candidate_digest: stated, ...body } = record;
  const observed = digest(body);
  if (!SHA256.test(stated ?? '') || stated !== observed) {
    throw new Error('REFUSED:EPR_CANDIDATE_DIGEST');
  }
  return stated;
}

function verifyDiscoveryBoundary(record) {
  if (record.authority !== 'NONE' || record.grants_do_authority !== false) {
    throw new Error('REFUSED:EPR_AMBIENT_AUTHORITY');
  }
  if (record.standing !== 'CANDIDATE') {
    throw new Error('REFUSED:EPR_STANDING_LAUNDERING');
  }
  if (record.behavioral_equivalence_claimed !== false
      || record.behavioral_qualification_required !== true) {
    throw new Error('REFUSED:EPR_EQUIVALENCE_LAUNDERING');
  }
  if (record.qualification_receipt !== null) {
    throw new Error('REFUSED:EPR_UNRECEIPTED_QUALIFICATION_LAUNDERING');
  }
  if (!SHA40.test(record.supplier?.commit ?? '')) {
    throw new Error('REFUSED:EPR_MUTABLE_SUPPLIER_REF');
  }
  if (!SHA256.test(record.supplier?.release_digest ?? '')) {
    throw new Error('REFUSED:EPR_RELEASE_DIGEST');
  }
  if (typeof record.candidate?.part_id !== 'string'
      || record.candidate.part_id.length === 0) {
    throw new Error('REFUSED:EPR_CANDIDATE_ID');
  }
}

function verifyPassportBinding(record, passport) {
  const metadata = passport?.metadata;
  const provenance = passport?.provenance;

  if (!metadata || metadata.priorArtCandidateDigest !== record.candidate_digest) {
    throw new Error('REFUSED:EPR_PASSPORT_CANDIDATE_DIGEST_MISMATCH');
  }
  if (metadata.priorArtCandidatePartId !== record.candidate.part_id) {
    throw new Error('REFUSED:EPR_PASSPORT_PART_ID_MISMATCH');
  }
  if (provenance?.priorArtSupplierRepository !== record.supplier.repository) {
    throw new Error('REFUSED:EPR_PASSPORT_SUPPLIER_REPOSITORY_MISMATCH');
  }
  if (provenance?.priorArtSupplierCommit !== record.supplier.commit) {
    throw new Error('REFUSED:EPR_PASSPORT_SUPPLIER_COMMIT_MISMATCH');
  }
  if (provenance?.priorArtReleaseDigest !== record.supplier.release_digest) {
    throw new Error('REFUSED:EPR_PASSPORT_RELEASE_DIGEST_MISMATCH');
  }
}

/**
 * Evaluate a supplier-discovered candidate under the canonical behavioral law.
 *
 * ADMITTED here means only that the existing PartRequirement/PartPassport
 * substitution court admits the candidate. It does not create a
 * QualificationReceipt and never grants DO. The result explicitly requires
 * a canonical substitution receipt before any actuation boundary may use it.
 */
export function evaluatePriorArtSubstitution({
  priorArtCandidate,
  requirement,
  candidatePassport,
  context = {},
} = {}) {
  const candidateDigest = verifyCandidateDigest(priorArtCandidate);
  verifyDiscoveryBoundary(priorArtCandidate);
  verifyPassportBinding(priorArtCandidate, candidatePassport);

  const judgement = evaluateSubstitution(
    requirement,
    candidatePassport,
    context,
  );

  const core = {
    schema: EPR_SUBSTITUTION_EVALUATION_SCHEMA,
    prior_art_candidate_digest: candidateDigest,
    requirement_digest: requirement?.digest ?? null,
    passport_digest: candidatePassport?.digest ?? null,
    judgement_digest: judgement.digest,
    behavioral_state: judgement.state,
    falsifier: judgement.falsifier,
    receipt_required: true,
    qualification_receipt: null,
    standing: 'CANDIDATE',
    authority: 'NONE',
    grants_do_authority: false,
  };

  return Object.freeze({
    ...core,
    evaluation_digest: digest(core),
  });
}

/**
 * Verify a stored evaluation by rerunning the canonical behavioral court.
 */
export function replayPriorArtSubstitution(
  evaluation,
  input,
) {
  const replay = evaluatePriorArtSubstitution(input);
  const valid =
    evaluation?.schema === EPR_SUBSTITUTION_EVALUATION_SCHEMA
    && evaluation?.evaluation_digest === replay.evaluation_digest
    && JSON.stringify(canonical(evaluation)) === JSON.stringify(canonical(replay));

  return Object.freeze({
    valid,
    state: valid ? 'CANDIDATE' : 'REFUSED',
    observed_evaluation_digest: replay.evaluation_digest,
    second_run_byte_identical: valid,
    receipt_required: true,
    authority: 'NONE',
    grants_do_authority: false,
  });
}
