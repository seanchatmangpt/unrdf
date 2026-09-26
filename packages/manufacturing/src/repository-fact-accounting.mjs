/**
 * @file Exact-subject ERRC repository-fact accounting.
 * @module manufacturing/repository-fact-accounting
 *
 * Unknown ownership is intentionally charged as handwritten.
 * Generated/reused/materialized claims require explicit evidence.
 */

import crypto from 'node:crypto';
import { z } from 'zod';

export const OwnershipKind = Object.freeze({
  HANDWRITTEN: 'handwritten',
  GENERATED: 'generated',
  REUSED: 'reused',
  MATERIALIZED: 'materialized',
  UNKNOWN: 'unknown',
});

const SHA40 = /^[0-9a-f]{40}$/;
const SHA256 = /^(?:sha256:)?[0-9a-f]{64}$/;

export const ExactGitSubjectSchema = z.object({
  repository: z.string().min(1),
  commit: z.string().regex(SHA40, 'immutable 40-hex commit required'),
});

export const RepositoryFactSchema = z.object({
  path: z.string().min(1),
  loc: z.number().int().nonnegative(),
  bytes: z.number().int().nonnegative().optional(),
  blobSha: z.string().min(1).optional(),
  contentDigest: z.string().regex(SHA256).optional(),
  ownership: z.enum([
    OwnershipKind.HANDWRITTEN,
    OwnershipKind.GENERATED,
    OwnershipKind.REUSED,
    OwnershipKind.MATERIALIZED,
    OwnershipKind.UNKNOWN,
  ]).default(OwnershipKind.UNKNOWN),
  sourceOwner: z.string().min(1).optional(),
  generator: z.string().min(1).optional(),
  generatorDigest: z.string().regex(SHA256).optional(),
  sourceDigest: z.string().regex(SHA256).optional(),
  materializationReceipt: z.string().regex(SHA256).optional(),
});

export const AccountingInputSchema = z.object({
  subject: ExactGitSubjectSchema,
  facts: z.array(RepositoryFactSchema),
});

function canonicalJson(value) {
  if (Array.isArray(value)) return '[' + value.map(canonicalJson).join(',') + ']';
  if (value && typeof value === 'object') {
    return '{' + Object.keys(value).sort().map(
      (key) => JSON.stringify(key) + ':' + canonicalJson(value[key]),
    ).join(',') + '}';
  }
  return JSON.stringify(value);
}

function sha256(value) {
  return 'sha256:' + crypto.createHash('sha256').update(value).digest('hex');
}

function normalizedOwnership(fact) {
  if (fact.ownership === OwnershipKind.UNKNOWN) return OwnershipKind.HANDWRITTEN;

  if (fact.ownership === OwnershipKind.GENERATED) {
    return fact.generator && fact.generatorDigest
      ? OwnershipKind.GENERATED
      : OwnershipKind.HANDWRITTEN;
  }

  if (fact.ownership === OwnershipKind.REUSED) {
    return fact.sourceOwner && fact.sourceDigest
      ? OwnershipKind.REUSED
      : OwnershipKind.HANDWRITTEN;
  }

  if (fact.ownership === OwnershipKind.MATERIALIZED) {
    return fact.sourceOwner && fact.sourceDigest && fact.materializationReceipt
      ? OwnershipKind.MATERIALIZED
      : OwnershipKind.HANDWRITTEN;
  }

  return OwnershipKind.HANDWRITTEN;
}

/**
 * Account repository LOC from immutable repository facts.
 *
 * Net maintained LOC is handwritten LOC plus generator source LOC. Generated,
 * reused and materialized projections are not double-charged when their
 * ownership evidence is valid. A generator/source file still appears as its
 * own repository fact and is therefore counted according to that fact.
 *
 * @param {unknown} input
 * @returns {{
 *   schema: string,
 *   subject: {repository:string, commit:string},
 *   totals: object,
 *   ownership: object,
 *   compressionRatio: number,
 *   reuseRatio: number,
 *   maintainedRatio: number,
 *   factDigest: string,
 *   receiptDigest: string,
 *   facts: Array<object>
 * }}
 */
export function accountRepositoryFacts(input) {
  const parsed = AccountingInputSchema.parse(input);
  const seen = new Set();

  const facts = parsed.facts
    .map((fact) => {
      if (seen.has(fact.path)) {
        throw new Error(`REFUSED:DUPLICATE_REPOSITORY_FACT:${fact.path}`);
      }
      seen.add(fact.path);
      const accountedOwnership = normalizedOwnership(fact);
      return {
        ...fact,
        claimedOwnership: fact.ownership,
        accountedOwnership,
        ownershipDowngraded: accountedOwnership !== fact.ownership,
      };
    })
    .sort((a, b) => a.path.localeCompare(b.path));

  const loc = {
    handwritten: 0,
    generated: 0,
    reused: 0,
    materialized: 0,
  };

  for (const fact of facts) loc[fact.accountedOwnership] += fact.loc;

  const totalLoc = Object.values(loc).reduce((a, b) => a + b, 0);
  const netMaintainedLoc = loc.handwritten;
  const avoidedMaintainedLoc = totalLoc - netMaintainedLoc;
  const compressionRatio = netMaintainedLoc === 0
    ? (totalLoc === 0 ? 1 : Number.POSITIVE_INFINITY)
    : totalLoc / netMaintainedLoc;
  const reuseRatio = totalLoc === 0
    ? 0
    : (loc.reused + loc.materialized) / totalLoc;
  const generatedRatio = totalLoc === 0 ? 0 : loc.generated / totalLoc;
  const maintainedRatio = totalLoc === 0 ? 0 : netMaintainedLoc / totalLoc;

  const factPayload = {
    subject: parsed.subject,
    facts: facts.map((fact) => ({
      path: fact.path,
      loc: fact.loc,
      bytes: fact.bytes ?? null,
      blobSha: fact.blobSha ?? null,
      contentDigest: fact.contentDigest ?? null,
      claimedOwnership: fact.claimedOwnership,
      accountedOwnership: fact.accountedOwnership,
      sourceOwner: fact.sourceOwner ?? null,
      generator: fact.generator ?? null,
      generatorDigest: fact.generatorDigest ?? null,
      sourceDigest: fact.sourceDigest ?? null,
      materializationReceipt: fact.materializationReceipt ?? null,
    })),
  };
  const factDigest = sha256(canonicalJson(factPayload));

  const receiptCore = {
    schema: 'unrdf.errc-repository-fact-receipt/1',
    subject: parsed.subject,
    totals: {
      totalLoc,
      netMaintainedLoc,
      avoidedMaintainedLoc,
      handwrittenLoc: loc.handwritten,
      generatedLoc: loc.generated,
      reusedLoc: loc.reused,
      materializedLoc: loc.materialized,
    },
    ratios: {
      compressionRatio,
      reuseRatio,
      generatedRatio,
      maintainedRatio,
    },
    factDigest,
  };

  return {
    ...receiptCore,
    compressionRatio,
    reuseRatio,
    maintainedRatio,
    ownership: loc,
    facts,
    receiptDigest: sha256(canonicalJson(receiptCore)),
  };
}

/**
 * Compare two ERRC receipts without treating a larger generated tree as
 * increased maintenance burden.
 *
 * @param {ReturnType<typeof accountRepositoryFacts>} before
 * @param {ReturnType<typeof accountRepositoryFacts>} after
 */
export function compareSemanticCompression(before, after) {
  if (before.subject.repository !== after.subject.repository) {
    throw new Error('REFUSED:ERRC_SUBJECT_REPOSITORY_MISMATCH');
  }
  return {
    repository: before.subject.repository,
    beforeCommit: before.subject.commit,
    afterCommit: after.subject.commit,
    maintainedLocDelta: after.totals.netMaintainedLoc - before.totals.netMaintainedLoc,
    totalLocDelta: after.totals.totalLoc - before.totals.totalLoc,
    avoidedMaintainedLocDelta:
      after.totals.avoidedMaintainedLoc - before.totals.avoidedMaintainedLoc,
    compressionRatioDelta: after.compressionRatio - before.compressionRatio,
    reuseRatioDelta: after.reuseRatio - before.reuseRatio,
  };
}

/**
 * Deterministic machine representation. Two runs over the same facts produce
 * byte-identical UTF-8 output including the final newline.
 *
 * @param {ReturnType<typeof accountRepositoryFacts>} receipt
 */
export function serializeRepositoryFactReceipt(receipt) {
  return canonicalJson(receipt) + '\n';
}
