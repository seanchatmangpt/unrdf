/**
 * @file Receipts for interchangeable-part substitution admission/refusal.
 * @module @unrdf/receipts/substitution-receipt
 */
import { evaluateSubstitution } from '@unrdf/core';
import { createReceipt, verifyReceipt } from './receipt-verifier.mjs';

export const SUBSTITUTION_RECEIPT_SCHEMA = 'unrdf.part-substitution-receipt/1';

function inputsFor(requirement, candidate, context) {
  return {
    requirementDigest: requirement.digest,
    candidateDigest: candidate.digest,
    context,
  };
}

function actionFor(judgement) {
  return judgement.state === 'ADMITTED'
    ? 'part.substitution.admit'
    : 'part.substitution.refuse';
}

/**
 * Create a tamper-evident receipt for a fresh substitution evaluation.
 * A caller cannot inject a precomputed judgement; it is always recomputed.
 */
export function createSubstitutionReceipt({
  requirement,
  candidate,
  context = {},
  actor = 'unrdf.interchangeable-part',
  authority = null,
  receiptId,
  previousReceiptHash = null,
  timestamp,
} = {}, options = {}) {
  const judgement = evaluateSubstitution(requirement, candidate, context);
  const inputs = inputsFor(requirement, candidate, context);
  const evidence = candidate.verifiers ?? [];

  return createReceipt({
    receiptId,
    subject: candidate.partId,
    action: actionFor(judgement),
    actor,
    authority,
    previousReceiptHash,
    inputs,
    outputs: judgement,
    evidence,
    status: judgement.state === 'ADMITTED' ? 'success' : 'refused',
    exitCode: judgement.state === 'ADMITTED' ? 0 : 2,
    timestamp,
    metadata: {
      requirementDigest: requirement.digest,
      candidateDigest: candidate.digest,
      judgementDigest: judgement.digest,
      falsifier: judgement.falsifier,
    },
  }, {
    ...options,
    schema: SUBSTITUTION_RECEIPT_SCHEMA,
  });
}

/**
 * Verify receipt integrity and re-evaluate the law against the current inputs.
 */
export function verifySubstitutionReceipt(
  receipt,
  { requirement, candidate, context = {} } = {},
  options = {},
) {
  const errors = [];
  if (!requirement || !candidate) {
    return { valid: false, state: 'REFUSED', errors: ['requirement and candidate are required'] };
  }

  const judgement = evaluateSubstitution(requirement, candidate, context);
  const inputs = inputsFor(requirement, candidate, context);
  const evidence = candidate.verifiers ?? [];
  const generic = verifyReceipt(receipt, {
    inputs,
    outputs: judgement,
    evidence,
    hmacKey: options.hmacKey,
    previousReceiptHash: options.previousReceiptHash,
  });
  errors.push(...generic.errors);

  if (receipt?.schema !== SUBSTITUTION_RECEIPT_SCHEMA) errors.push('substitution receipt schema mismatch');
  if (receipt?.subject !== candidate.partId) errors.push('candidate subject mismatch');
  if (receipt?.action !== actionFor(judgement)) errors.push('substitution action mismatch');
  if (receipt?.metadata?.requirementDigest !== requirement.digest) errors.push('requirement digest mismatch');
  if (receipt?.metadata?.candidateDigest !== candidate.digest) errors.push('candidate digest mismatch');
  if (receipt?.metadata?.judgementDigest !== judgement.digest) errors.push('judgement digest mismatch');

  const expectedStatus = judgement.state === 'ADMITTED' ? 'success' : 'refused';
  const expectedExitCode = judgement.state === 'ADMITTED' ? 0 : 2;
  if (receipt?.status !== expectedStatus) errors.push('substitution status mismatch');
  if (receipt?.exitCode !== expectedExitCode) errors.push('substitution exit code mismatch');

  return {
    valid: errors.length === 0,
    state: errors.length === 0 ? judgement.state : 'REFUSED',
    errors,
    judgement,
  };
}
