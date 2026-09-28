/**
 * @file Continuum tier topology: browser -> edge -> fog -> cloud.
 *
 * Browser-safe (no node: imports). Data enters at the low tier (first mile,
 * travelling up) and is delivered from the high tier back down (last mile).
 */

/** Ordered low -> high. */
export const TIERS = Object.freeze(['browser', 'edge', 'fog', 'cloud']);

export const TIER_LEVEL = Object.freeze({ browser: 0, edge: 1, fog: 2, cloud: 3 });

/** The root of trust and system of record. */
export const ROOT_TIER = 'cloud';

/** What tier_witness.erl computes over its fixed corpus (Adler-32 halves + length). */
export const WITNESS_CORPUS = Object.freeze({ length: 22, adlerA: 2288, adlerB: 25919 });

export function assertTier(tier) {
  if (!Object.hasOwn(TIER_LEVEL, tier)) {
    throw new TypeError(`unknown tier: ${String(tier)} (expected one of ${TIERS.join(', ')})`);
  }
  return tier;
}

/** @returns {string|null} the next tier towards the cloud, or null for the root. */
export function upstreamOf(tier) {
  assertTier(tier);
  return TIERS[TIER_LEVEL[tier] + 1] ?? null;
}

/**
 * Exact text the tier witness prints when it is alive and computed correctly.
 * Matching it proves the right tier's program ran AND the VM computed the
 * expected Adler-32 over the corpus.
 */
export function witnessMarker(tier) {
  assertTier(tier);
  const { length, adlerA, adlerB } = WITNESS_CORPUS;
  return `{atomvm_tier_alive,${tier},${TIER_LEVEL[tier]},${length},${adlerA},${adlerB}}`;
}
