/** Clean replay runner for deterministic plans. */
import { compareReplay } from './receipt-chain.mjs';

/**
 * Run an operation twice with fresh setup and cleanup, and compare the normalized results.
 * @param {Function} operation - Async function `(context, {attempt})` to run.
 * @param {Object} [hooks] - Optional hooks.
 * @param {Function} [hooks.setup] - Creates the context for each attempt.
 * @param {Function} [hooks.cleanup] - Called after each attempt, even on error.
 * @param {Function} [hooks.normalize] - Maps each result before comparison.
 * @returns {Promise<Object>} `{runs, match, firstDigest, secondDigest, state}`.
 * @throws {TypeError} If operation is not a function.
 */
export async function replay(operation, {
  setup = async () => ({}),
  cleanup = async () => {},
  normalize = value => value,
} = {}) {
  if (typeof operation !== 'function') throw new TypeError('operation must be a function');
  const runs = [];
  for (let attempt = 1; attempt <= 2; attempt++) {
    const context = await setup({ attempt });
    try {
      runs.push(normalize(await operation(context, { attempt })));
    } finally {
      await cleanup(context, { attempt });
    }
  }
  return { runs, ...compareReplay(runs[0], runs[1]) };
}

/**
 * Replay an operation and throw unless both runs match.
 * @param {Function} operation - Operation passed to replay.
 * @param {Object} [options] - Options passed to replay.
 * @returns {Promise<Object>} The replay result.
 * @throws {Error} With message REPLAY_DIFFERENCE and `result` if the runs differ.
 */
export async function requireReplayMatch(operation, options) {
  const result = await replay(operation, options);
  if (!result.match) throw Object.assign(new Error('REPLAY_DIFFERENCE'), { result });
  return result;
}
