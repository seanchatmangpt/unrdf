/**
 * @file Manufacture deterministic interchangeable-part passports from μ(O) artifacts.
 * @module @unrdf/manufacturing/part-passport
 */
import { createPartPassport } from '@unrdf/core';
import { CausalityChain } from './causality/index.mjs';

function requireArtifact(artifact) {
  if (!artifact || typeof artifact !== 'object') throw new TypeError('artifact is required');
  if (typeof artifact.receipt !== 'string' || artifact.receipt.length === 0) {
    throw new TypeError('artifact.receipt is required');
  }
  return artifact;
}

/**
 * Project an admitted manufactured artifact into an interchangeable-part passport.
 *
 * The passport digest contains only deterministic provenance. The wall-clock
 * causality trace is returned separately so replayable identity is not polluted
 * by observation time.
 */
export function manufacturePartPassport({
  artifact,
  ontologySource,
  part,
} = {}) {
  requireArtifact(artifact);
  if (typeof ontologySource !== 'string' || ontologySource.length === 0) {
    throw new TypeError('ontologySource is required');
  }
  if (!part || typeof part !== 'object') throw new TypeError('part specification is required');

  const passport = createPartPassport({
    ...part,
    artifactDigest: artifact.receipt,
    provenance: {
      ...(part.provenance ?? {}),
      source: ontologySource,
      artifactDigest: artifact.receipt,
    },
  });

  const causality = new CausalityChain(ontologySource)
    .addStep(
      'manufacture-interchangeable-part-passport',
      {
        artifactType: artifact.type,
        artifactDigest: artifact.receipt,
      },
      {
        partId: passport.partId,
        partVersion: passport.version,
        passportDigest: passport.digest,
      },
    );

  return Object.freeze({
    artifact,
    passport,
    causality: causality.toJSON(),
  });
}
