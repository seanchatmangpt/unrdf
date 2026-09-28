/**
 * @file Seedable random number generation
 * @module ai-ml-innovations/rng
 *
 * @description
 * Randomness is a design input of the federated-learning primitives (DP noise,
 * secure-aggregation masks, node sampling). When a `seed` is supplied the stream is
 * fully deterministic (mulberry32); otherwise the runtime's `Math.random` is used.
 *
 * NOTE: a seeded stream is for reproducible experiments and tests only. Production
 * differential privacy / masking must NOT be seeded with a guessable value.
 */

/**
 * Create a uniform [0, 1) random function.
 *
 * @param {number} [seed] - Optional integer seed. Omitted => Math.random.
 * @returns {() => number} Random function returning values in [0, 1)
 */
export function createRandom(seed) {
  if (seed === undefined || seed === null) {
    return Math.random;
  }

  let state = seed >>> 0;
  return function mulberry32() {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

/**
 * Standard normal sample via Box-Muller using the supplied uniform source.
 *
 * @param {() => number} random - Uniform [0, 1) source
 * @returns {number} N(0, 1) sample
 */
export function standardNormal(random) {
  // 1 - random() lies in (0, 1], so log() never sees 0
  const u1 = 1 - random();
  const u2 = random();
  return Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
}

export default createRandom;
