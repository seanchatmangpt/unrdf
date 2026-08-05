/** Clean replay runner for deterministic plans. */
import { compareReplay } from './receipt-chain.mjs';

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

export async function requireReplayMatch(operation, options) {
  const result = await replay(operation, options);
  if (!result.match) throw Object.assign(new Error('REPLAY_DIFFERENCE'), { result });
  return result;
}
