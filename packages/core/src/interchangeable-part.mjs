/**
 * @file Consequence-preserving interchangeable part admission.
 * @module @unrdf/core/interchangeable-part
 *
 * A part is substitutable only when its observable contract is preserved and
 * its authority, effects, failures, resource demand, and evidence do not widen.
 * API shape alone is never sufficient evidence of interchangeability.
 */
import { createHash } from 'node:crypto';

export const PART_REQUIREMENT_SCHEMA = 'unrdf.part-requirement/1';
export const PART_PASSPORT_SCHEMA = 'unrdf.part-passport/1';
export const SUBSTITUTION_JUDGEMENT_SCHEMA = 'unrdf.part-substitution-judgement/1';

export class PartSubstitutionRefusal extends Error {
  constructor(judgement) {
    super(`Part substitution refused: ${judgement.reasons.map(reason => reason.code).join(', ')}`);
    this.name = 'PartSubstitutionRefusal';
    this.code = 'PART_SUBSTITUTION_REFUSED';
    this.judgement = judgement;
  }
}

function deepFreeze(value) {
  if (Array.isArray(value)) {
    value.forEach(deepFreeze);
    return Object.freeze(value);
  }
  if (value && typeof value === 'object') {
    Object.values(value).forEach(deepFreeze);
    return Object.freeze(value);
  }
  return value;
}

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map(key => [key, canonical(value[key])]));
  }
  return value;
}

function digest(value) {
  return createHash('sha256').update(JSON.stringify(canonical(value))).digest('hex');
}

function digestRecord(record) {
  const { digest: _digest, ...body } = record;
  return digest(body);
}

function text(value, name) {
  if (typeof value !== 'string' || value.trim() === '') {
    throw new TypeError(`${name} must be a non-empty string`);
  }
  return value;
}

function stringSet(values, name) {
  if (values === undefined) return [];
  if (!Array.isArray(values)) throw new TypeError(`${name} must be an array`);
  const normalized = values.map((value, index) => text(value, `${name}[${index}]`));
  return [...new Set(normalized)].sort();
}

function budget(value, name) {
  if (value === undefined) return {};
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new TypeError(`${name} must be an object`);
  }
  const normalized = {};
  for (const key of Object.keys(value).sort()) {
    text(key, `${name} key`);
    const observed = value[key];
    if (typeof observed !== 'number' || !Number.isFinite(observed) || observed < 0) {
      throw new TypeError(`${name}.${key} must be a finite non-negative number`);
    }
    normalized[key] = observed;
  }
  return normalized;
}

function subset(left, right) {
  const permitted = new Set(right);
  return left.every(value => permitted.has(value));
}

function missing(required, observed) {
  const present = new Set(observed);
  return required.filter(value => !present.has(value));
}

function widened(observed, allowed) {
  const permitted = new Set(allowed);
  return observed.filter(value => !permitted.has(value));
}

function reason(code, path, required, observed) {
  return deepFreeze({ code, path, required: canonical(required), observed: canonical(observed) });
}

/**
 * Verify a requirement/passport record.
 *
 * The digest is an unkeyed sha256 of the body, so anyone can recompute it after
 * editing a field. A matching digest therefore only proves self-consistency; the
 * record must also be a fixed point of its own manufacturer (`remanufacture`),
 * otherwise values the constructor refuses (NaN or negative resources, negative
 * delegation depth, non-array capability sets, unsorted/duplicate sets, extra
 * keys) would reach evaluation with a valid digest. NaN serializes to null in
 * JSON, so a NaN resource demand would otherwise pass every `>` ceiling check.
 */
function verifyDigest(record, schema, remanufacture) {
  if (!record || typeof record !== 'object' || Array.isArray(record)) {
    return { valid: false, reason: 'MISSING_RECORD' };
  }
  if (record.schema !== schema) return { valid: false, reason: 'SCHEMA_MISMATCH' };
  if (typeof record.digest !== 'string') return { valid: false, reason: 'DIGEST_MISSING' };
  const tampered = () => record.digest !== digestRecord(record);
  let rebuilt;
  try {
    rebuilt = remanufacture(record);
  } catch {
    return { valid: false, reason: tampered() ? 'DIGEST_MISMATCH' : 'MALFORMED_RECORD' };
  }
  const { digest: _digest, ...body } = record;
  const { digest: rebuiltDigest, ...rebuiltBody } = rebuilt;
  // Compare structurally rather than by digest: JSON.stringify maps NaN and
  // Infinity to null, so equal digests do not imply equal values.
  if (!sameValue(body, rebuiltBody)) {
    return { valid: false, reason: tampered() ? 'DIGEST_MISMATCH' : 'NON_CANONICAL_RECORD' };
  }
  // Structurally equal bodies canonicalize identically, so the rebuilt digest is
  // the body digest: one sha256 on the admitted path instead of two.
  return record.digest === rebuiltDigest
    ? { valid: true, reason: null }
    : { valid: false, reason: 'DIGEST_MISMATCH' };
}

function sameValue(left, right) {
  if (Object.is(left, right)) return true;
  if (Array.isArray(left) || Array.isArray(right)) {
    return (
      Array.isArray(left) &&
      Array.isArray(right) &&
      left.length === right.length &&
      left.every((value, index) => sameValue(value, right[index]))
    );
  }
  if (!left || !right || typeof left !== 'object' || typeof right !== 'object') return false;
  const leftKeys = Object.keys(left).sort();
  const rightKeys = Object.keys(right).sort();
  return (
    leftKeys.length === rightKeys.length &&
    leftKeys.every((key, index) => key === rightKeys[index] && sameValue(left[key], right[key]))
  );
}

function contractFields(source) {
  return {
    semanticContract: text(source.semanticContract, 'semanticContract'),
    inputContract: text(source.inputContract, 'inputContract'),
    outputContract: text(source.outputContract, 'outputContract'),
    stateContract: text(source.stateContract, 'stateContract'),
    rulesContract: text(source.rulesContract, 'rulesContract'),
    informationFlowContract: text(source.informationFlowContract, 'informationFlowContract'),
    lifecycleContract: text(source.lifecycleContract, 'lifecycleContract'),
  };
}

/**
 * Define the host/caller contract a replacement must satisfy.
 */
export function createPartRequirement(input = {}) {
  const body = canonical({
    schema: PART_REQUIREMENT_SCHEMA,
    requirementId: text(input.requirementId, 'requirementId'),
    ...contractFields(input),
    expectedInputs: stringSet(input.expectedInputs, 'expectedInputs'),
    requiredGuarantees: stringSet(input.requiredGuarantees, 'requiredGuarantees'),
    allowedEffects: stringSet(input.allowedEffects, 'allowedEffects'),
    allowedFailures: stringSet(input.allowedFailures, 'allowedFailures'),
    allowedAuthorityIssuers: stringSet(input.allowedAuthorityIssuers, 'allowedAuthorityIssuers'),
    allowedCapabilities: stringSet(input.allowedCapabilities, 'allowedCapabilities'),
    maxDelegationDepth: input.maxDelegationDepth ?? 0,
    resourceCeilings: budget(input.resourceCeilings, 'resourceCeilings'),
    requiredVerifiers: stringSet(input.requiredVerifiers, 'requiredVerifiers'),
    receiptSchema: text(input.receiptSchema, 'receiptSchema'),
    replayRequired: input.replayRequired !== false,
    allowedRuntimes: stringSet(input.allowedRuntimes, 'allowedRuntimes'),
    metadata: canonical(input.metadata ?? {}),
  });

  if (!Number.isInteger(body.maxDelegationDepth) || body.maxDelegationDepth < 0) {
    throw new TypeError('maxDelegationDepth must be a non-negative integer');
  }
  if (body.allowedAuthorityIssuers.length === 0) throw new TypeError('allowedAuthorityIssuers must not be empty');
  if (body.allowedRuntimes.length === 0) throw new TypeError('allowedRuntimes must not be empty');

  return deepFreeze({ ...body, digest: digest(body) });
}

/**
 * Manufacture the deterministic passport carried by a concrete part.
 */
export function createPartPassport(input = {}) {
  const body = canonical({
    schema: PART_PASSPORT_SCHEMA,
    partId: text(input.partId, 'partId'),
    version: text(input.version, 'version'),
    artifactDigest: text(input.artifactDigest, 'artifactDigest'),
    ...contractFields(input),
    acceptedInputs: stringSet(input.acceptedInputs, 'acceptedInputs'),
    guarantees: stringSet(input.guarantees, 'guarantees'),
    effects: stringSet(input.effects, 'effects'),
    failures: stringSet(input.failures, 'failures'),
    authorityIssuer: text(input.authorityIssuer, 'authorityIssuer'),
    authoritySubject: text(input.authoritySubject, 'authoritySubject'),
    capabilities: stringSet(input.capabilities, 'capabilities'),
    delegationDepth: input.delegationDepth ?? 0,
    resources: budget(input.resources, 'resources'),
    verifiers: stringSet(input.verifiers, 'verifiers'),
    receiptSchema: text(input.receiptSchema, 'receiptSchema'),
    replay: input.replay === true,
    runtime: text(input.runtime, 'runtime'),
    provenance: canonical(input.provenance ?? {}),
    metadata: canonical(input.metadata ?? {}),
  });

  if (!Number.isInteger(body.delegationDepth) || body.delegationDepth < 0) {
    throw new TypeError('delegationDepth must be a non-negative integer');
  }
  if (!body.provenance.source || !body.provenance.artifactDigest) {
    throw new TypeError('provenance.source and provenance.artifactDigest are required');
  }
  if (body.provenance.artifactDigest !== body.artifactDigest) {
    throw new TypeError('provenance.artifactDigest must equal artifactDigest');
  }

  return deepFreeze({ ...body, digest: digest(body) });
}

export function verifyPartRequirement(requirement) {
  return verifyDigest(requirement, PART_REQUIREMENT_SCHEMA, createPartRequirement);
}

export function verifyPartPassport(passport) {
  return verifyDigest(passport, PART_PASSPORT_SCHEMA, createPartPassport);
}

function integrityRefusal(reasons, requirement, candidate) {
  const body = canonical({
    schema: SUBSTITUTION_JUDGEMENT_SCHEMA,
    state: 'REFUSED',
    requirementDigest: typeof requirement?.digest === 'string' ? requirement.digest : null,
    candidateDigest: typeof candidate?.digest === 'string' ? candidate.digest : null,
    effectiveAuthority: { issuers: [], capabilities: [] },
    effectiveResourceCeilings: {},
    reasons,
    falsifier: reasons[0] ?? null,
  });
  return deepFreeze({ ...body, digest: digest(body) });
}

function contextRefusal(context) {
  if (context === null || typeof context !== 'object' || Array.isArray(context)) {
    return reason('CONTEXT_MALFORMED_REFUSED', 'context', 'object', context === null ? null : typeof context);
  }
  for (const [field, check] of [
    ['hostCapabilities', stringSet],
    ['hostAuthorityIssuers', stringSet],
    ['hostResourceCeilings', budget],
  ]) {
    if (context[field] === undefined) continue;
    try {
      check(context[field], `context.${field}`);
    } catch (error) {
      return reason('CONTEXT_MALFORMED_REFUSED', `context.${field}`, 'well-formed', error.message);
    }
  }
  return null;
}

/**
 * Evaluate consequence-preserving substitution.
 *
 * Inclusion direction is deliberate:
 * - accepted inputs and guarantees may grow (caller obligations still work);
 * - effects, failures, authority, delegation, and resource demand may only shrink;
 * - verifier evidence may grow;
 * - semantic/state/rule/information-flow/lifecycle contracts must remain exact.
 *
 * Host context further intersects authority and resource ceilings.
 */
export function evaluateSubstitution(requirement, candidate, context = {}) {
  const reasons = [];

  const requirementVerification = verifyPartRequirement(requirement);
  if (!requirementVerification.valid) {
    reasons.push(reason('REQUIREMENT_INTEGRITY_REFUSED', 'requirement.digest', 'valid', requirementVerification.reason));
  }
  const passportVerification = verifyPartPassport(candidate);
  if (!passportVerification.valid) {
    reasons.push(reason('PASSPORT_INTEGRITY_REFUSED', 'candidate.digest', 'valid', passportVerification.reason));
  }
  const malformedContext = contextRefusal(context);
  if (malformedContext) reasons.push(malformedContext);
  // Unverified or malformed records are not evaluated field by field: their
  // shape is unknown, so the law refuses on integrity alone (total, never throws).
  if (reasons.length > 0) return integrityRefusal(reasons, requirement, candidate);

  const exactContracts = [
    ['semanticContract', 'SEMANTIC_CONTRACT_MISMATCH'],
    ['inputContract', 'INPUT_CONTRACT_MISMATCH'],
    ['outputContract', 'OUTPUT_CONTRACT_MISMATCH'],
    ['stateContract', 'STATE_CONTRACT_MISMATCH'],
    ['rulesContract', 'RULES_CONTRACT_MISMATCH'],
    ['informationFlowContract', 'INFORMATION_FLOW_CONTRACT_MISMATCH'],
    ['lifecycleContract', 'LIFECYCLE_CONTRACT_MISMATCH'],
  ];
  for (const [field, code] of exactContracts) {
    if (candidate?.[field] !== requirement?.[field]) {
      reasons.push(reason(code, field, requirement?.[field], candidate?.[field]));
    }
  }

  const missingInputs = missing(requirement?.expectedInputs ?? [], candidate?.acceptedInputs ?? []);
  if (missingInputs.length) {
    reasons.push(reason('INPUT_COVERAGE_REFUSED', 'acceptedInputs', requirement.expectedInputs, candidate.acceptedInputs));
  }

  const missingGuarantees = missing(requirement?.requiredGuarantees ?? [], candidate?.guarantees ?? []);
  if (missingGuarantees.length) {
    reasons.push(reason('GUARANTEE_COVERAGE_REFUSED', 'guarantees', requirement.requiredGuarantees, candidate.guarantees));
  }

  const widenedEffects = widened(candidate?.effects ?? [], requirement?.allowedEffects ?? []);
  if (widenedEffects.length) {
    reasons.push(reason('CONSEQUENCE_WIDENING_REFUSED', 'effects', requirement.allowedEffects, candidate.effects));
  }

  const widenedFailures = widened(candidate?.failures ?? [], requirement?.allowedFailures ?? []);
  if (widenedFailures.length) {
    reasons.push(reason('FAILURE_WIDENING_REFUSED', 'failures', requirement.allowedFailures, candidate.failures));
  }

  const hostCapabilities = context.hostCapabilities === undefined
    ? requirement?.allowedCapabilities ?? []
    : stringSet(context.hostCapabilities, 'context.hostCapabilities');
  const effectiveCapabilities = (requirement?.allowedCapabilities ?? [])
    .filter(capability => hostCapabilities.includes(capability))
    .sort();
  if (!subset(candidate?.capabilities ?? [], effectiveCapabilities)) {
    reasons.push(reason('AUTHORITY_WIDENING_REFUSED', 'capabilities', effectiveCapabilities, candidate?.capabilities ?? []));
  }

  const hostAuthorityIssuers = context.hostAuthorityIssuers === undefined
    ? requirement?.allowedAuthorityIssuers ?? []
    : stringSet(context.hostAuthorityIssuers, 'context.hostAuthorityIssuers');
  const effectiveIssuers = (requirement?.allowedAuthorityIssuers ?? [])
    .filter(issuer => hostAuthorityIssuers.includes(issuer))
    .sort();
  if (!effectiveIssuers.includes(candidate?.authorityIssuer)) {
    reasons.push(reason('AUTHORITY_ISSUER_REFUSED', 'authorityIssuer', effectiveIssuers, candidate?.authorityIssuer));
  }

  if ((candidate?.delegationDepth ?? Infinity) > (requirement?.maxDelegationDepth ?? -1)) {
    reasons.push(reason('DELEGATION_DEPTH_REFUSED', 'delegationDepth', requirement?.maxDelegationDepth, candidate?.delegationDepth));
  }

  const hostResources = context.hostResourceCeilings === undefined
    ? requirement?.resourceCeilings ?? {}
    : budget(context.hostResourceCeilings, 'context.hostResourceCeilings');
  const effectiveResourceCeilings = {};
  for (const key of Object.keys(requirement?.resourceCeilings ?? {}).sort()) {
    const requirementCeiling = requirement.resourceCeilings[key];
    const hostCeiling = Object.hasOwn(hostResources, key) ? hostResources[key] : undefined;
    if (hostCeiling !== undefined) effectiveResourceCeilings[key] = Math.min(requirementCeiling, hostCeiling);
  }
  for (const [key, observed] of Object.entries(candidate?.resources ?? {})) {
    const ceiling = effectiveResourceCeilings[key];
    if (ceiling === undefined || observed > ceiling) {
      reasons.push(reason('RESOURCE_CEILING_REFUSED', `resources.${key}`, ceiling ?? 'UNADMITTED', observed));
    }
  }

  const missingVerifiers = missing(requirement?.requiredVerifiers ?? [], candidate?.verifiers ?? []);
  if (missingVerifiers.length) {
    reasons.push(reason('EVIDENCE_INCOMPLETE_REFUSED', 'verifiers', requirement.requiredVerifiers, candidate.verifiers));
  }

  if (candidate?.receiptSchema !== requirement?.receiptSchema) {
    reasons.push(reason('RECEIPT_SCHEMA_MISMATCH', 'receiptSchema', requirement?.receiptSchema, candidate?.receiptSchema));
  }
  if (requirement?.replayRequired && candidate?.replay !== true) {
    reasons.push(reason('REPLAY_REQUIRED_REFUSED', 'replay', true, candidate?.replay));
  }
  if (!(requirement?.allowedRuntimes ?? []).includes(candidate?.runtime)) {
    reasons.push(reason('RUNTIME_REFUSED', 'runtime', requirement?.allowedRuntimes ?? [], candidate?.runtime));
  }
  if (!candidate?.provenance?.source || candidate?.provenance?.artifactDigest !== candidate?.artifactDigest) {
    reasons.push(reason('PROVENANCE_INCOMPLETE_REFUSED', 'provenance', 'source + matching artifactDigest', candidate?.provenance));
  }

  const body = canonical({
    schema: SUBSTITUTION_JUDGEMENT_SCHEMA,
    state: reasons.length === 0 ? 'ADMITTED' : 'REFUSED',
    requirementDigest: requirement?.digest ?? null,
    candidateDigest: candidate?.digest ?? null,
    effectiveAuthority: {
      issuers: effectiveIssuers,
      capabilities: effectiveCapabilities,
    },
    effectiveResourceCeilings,
    reasons,
    falsifier: reasons[0] ?? null,
  });

  return deepFreeze({ ...body, digest: digest(body) });
}

export function assertSubstitutable(requirement, candidate, context = {}) {
  const judgement = evaluateSubstitution(requirement, candidate, context);
  if (judgement.state !== 'ADMITTED') throw new PartSubstitutionRefusal(judgement);
  return judgement;
}
