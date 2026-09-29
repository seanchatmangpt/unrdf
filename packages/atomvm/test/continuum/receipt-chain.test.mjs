/**
 * The receipt chain is what a consumer trusts instead of the network, so every
 * way a chain can be wrong gets its own case. Pure state-in / verdict-out.
 */
import { describe, expect, it } from 'vitest';
import {
  TIERS,
  TIER_LEVEL,
  upstreamOf,
  witnessMarker,
  assertTier,
} from '../../src/continuum/tiers.mjs';
import {
  canonical,
  payloadDigestOf,
  receiptDigestIsValid,
  sealReceipt,
  sha256Hex,
  verifyChain,
} from '../../src/continuum/receipt-chain.mjs';

const PAYLOAD = { n: 1 };

async function chainOf(tiers, phase, { payload = PAYLOAD, executionDigest = 'x' } = {}) {
  const payloadDigest = await payloadDigestOf(payload);
  const chain = [];
  for (const tier of tiers) {
    chain.push(
      await sealReceipt({
        tier,
        nodeId: `${tier}-1`,
        phase,
        payloadDigest,
        prev: chain.at(-1)?.digest ?? null,
        executionDigest,
        at: 't',
      })
    );
  }
  return { chain, payloadDigest };
}

const verdict = async (chain, payloadDigest, phase) => verifyChain(chain, { payloadDigest, phase });

describe('tier topology', () => {
  it('orders tiers low to high and links each to the next towards the cloud', () => {
    expect(TIERS).toEqual(['browser', 'edge', 'fog', 'cloud']);
    expect(TIERS.map(upstreamOf)).toEqual(['edge', 'fog', 'cloud', null]);
    expect(TIERS.map(tier => TIER_LEVEL[tier])).toEqual([0, 1, 2, 3]);
    expect(() => assertTier('mist')).toThrow(/unknown tier/);
    expect(() => assertTier('__proto__')).toThrow(/unknown tier/);
  });

  it('names the exact witness output each tier must produce', () => {
    expect(witnessMarker('edge')).toBe('{atomvm_tier_alive,edge,1,22,2288,25919}');
    expect(new Set(TIERS.map(witnessMarker)).size).toBe(4);
  });
});

describe('canonical form and digests', () => {
  it('is independent of key order and sensitive to every value', async () => {
    expect(canonical({ b: 1, a: [1, { d: 4, c: 3 }] })).toBe(
      canonical({ a: [1, { c: 3, d: 4 }], b: 1 })
    );
    expect(await payloadDigestOf({ a: 1 })).not.toBe(await payloadDigestOf({ a: 2 }));
    expect(await sha256Hex('abc')).toBe(
      'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
    ); // FIPS 180 vector
  });

  it('refuses values JSON cannot represent instead of hashing something else', () => {
    expect(() => canonical(undefined)).toThrow(TypeError);
    expect(() => canonical(() => 1)).toThrow(TypeError);
  });
});

describe('verifyChain', () => {
  it('accepts a contiguous ascending ingest chain and a contiguous descending delivery chain', async () => {
    const up = await chainOf(['browser', 'edge', 'fog', 'cloud'], 'ingest');
    expect(await verdict(up.chain, up.payloadDigest, 'ingest')).toEqual({
      valid: true,
      tiers: ['browser', 'edge', 'fog', 'cloud'],
    });
    const down = await chainOf(['cloud', 'fog', 'edge', 'browser'], 'deliver');
    expect(await verdict(down.chain, down.payloadDigest, 'deliver')).toMatchObject({ valid: true });
    const single = await chainOf(['edge'], 'deliver');
    expect(await verdict(single.chain, single.payloadDigest, 'deliver')).toMatchObject({
      valid: true,
    });
  });

  it.each([
    ['ingest skipping a tier', ['browser', 'fog'], 'ingest', 'TIER_ORDER_VIOLATION'],
    ['ingest going downwards', ['cloud', 'fog'], 'ingest', 'TIER_ORDER_VIOLATION'],
    ['ingest repeating a tier', ['edge', 'edge'], 'ingest', 'TIER_ORDER_VIOLATION'],
    ['delivery skipping a tier', ['cloud', 'edge'], 'deliver', 'TIER_ORDER_VIOLATION'],
    ['delivery going upwards', ['edge', 'fog'], 'deliver', 'TIER_ORDER_VIOLATION'],
  ])('rejects %s', async (_label, tiers, phase, code) => {
    const { chain, payloadDigest } = await chainOf(tiers, phase);
    expect(await verdict(chain, payloadDigest, phase)).toMatchObject({ valid: false, code });
  });

  it('rejects an empty chain and an unknown phase', async () => {
    const { payloadDigest } = await chainOf(['edge'], 'ingest');
    expect(await verdict([], payloadDigest, 'ingest')).toMatchObject({
      valid: false,
      code: 'CHAIN_EMPTY',
    });
    expect(await verdict(undefined, payloadDigest, 'ingest')).toMatchObject({
      valid: false,
      code: 'CHAIN_EMPTY',
    });
    const { chain } = await chainOf(['edge'], 'ingest');
    expect(await verdict(chain, payloadDigest, 'sideways')).toMatchObject({
      valid: false,
      code: 'PHASE_UNKNOWN',
    });
  });

  it('rejects a chain of the wrong phase (ingest receipts cannot pose as delivery)', async () => {
    const { chain, payloadDigest } = await chainOf(['edge'], 'ingest');
    expect(await verdict(chain, payloadDigest, 'deliver')).toMatchObject({
      valid: false,
      code: 'PHASE_MISMATCH',
    });
  });

  it('rejects receipts that attest a different payload', async () => {
    const { chain } = await chainOf(['edge', 'fog'], 'ingest');
    const other = await payloadDigestOf({ n: 2 });
    expect(await verdict(chain, other, 'ingest')).toMatchObject({
      valid: false,
      code: 'PAYLOAD_DIGEST_MISMATCH',
    });
  });

  it('rejects any edited field (the digest no longer matches the contents)', async () => {
    const { chain, payloadDigest } = await chainOf(['edge', 'fog'], 'ingest');
    for (const field of ['tier', 'nodeId', 'executionDigest', 'at', 'prev']) {
      const edited = [chain[0], { ...chain[1], [field]: `${chain[1][field]}!` }];
      const result = await verdict(edited, payloadDigest, 'ingest');
      expect(result.valid, `editing ${field}`).toBe(false);
      expect(result.code).toBe('RECEIPT_DIGEST_MISMATCH');
    }
    expect(await receiptDigestIsValid({ ...chain[0], extra: 1 })).toBe(false);
    expect(await receiptDigestIsValid(null)).toBe(false);
    expect(await receiptDigestIsValid({})).toBe(false);
  });

  it('rejects broken links, a non-null genesis, splicing, and unknown tiers', async () => {
    const { chain, payloadDigest } = await chainOf(['edge', 'fog', 'cloud'], 'ingest');
    // dropping the middle receipt breaks the link
    expect(await verdict([chain[0], chain[2]], payloadDigest, 'ingest')).toMatchObject({
      valid: false,
      code: 'CHAIN_LINK_BROKEN',
    });
    // starting mid-chain: the first receipt must be genesis
    expect(await verdict(chain.slice(1), payloadDigest, 'ingest')).toMatchObject({
      valid: false,
      code: 'GENESIS_PREV_NOT_NULL',
    });
    // a different, individually valid chain spliced in
    const foreign = await chainOf(['edge', 'fog', 'cloud'], 'ingest', {
      executionDigest: 'another-run',
    });
    expect(await verdict([chain[0], foreign.chain[1]], payloadDigest, 'ingest')).toMatchObject({
      valid: false,
      code: 'CHAIN_LINK_BROKEN',
    });
    // a validly sealed receipt from a tier that does not exist
    const alien = await sealReceipt({
      tier: 'mist',
      nodeId: 'm',
      phase: 'ingest',
      payloadDigest,
      prev: null,
      executionDigest: 'x',
      at: 't',
    });
    expect(await verdict([alien], payloadDigest, 'ingest')).toMatchObject({
      valid: false,
      code: 'UNKNOWN_TIER',
    });
    // receipts from another protocol version are refused, not guessed at
    const { digest: _ignored, ...body } = chain[0];
    const v2 = { ...body, v: 2 };
    const sealedV2 = { ...v2, digest: await sha256Hex(canonical(v2)) };
    expect(await verdict([sealedV2], payloadDigest, 'ingest')).toMatchObject({
      valid: false,
      code: 'RECEIPT_VERSION_REFUSED',
    });
  });
});
