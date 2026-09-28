/**
 * @file Continuum receipt chain. Browser-safe: SHA-256 comes from WebCrypto,
 * which exists in browsers and Node >= 20, so the very same verifier runs in
 * the browser tier and on cloud/fog/edge nodes.
 *
 * Each tier that touches a payload seals one receipt. Receipts link by digest.
 *   ingest  (first mile)  : levels strictly ascend by 1  (e.g. browser->edge->fog->cloud)
 *   deliver (last mile)   : levels strictly descend by 1 (e.g. cloud->fog->edge->browser)
 */
import { TIER_LEVEL } from './tiers.mjs';

export const RECEIPT_VERSION = 1;

export function canonical(value) {
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  if (value && typeof value === 'object') {
    return `{${Object.keys(value)
      .sort()
      .map(key => `${JSON.stringify(key)}:${canonical(value[key])}`)
      .join(',')}}`;
  }
  const encoded = JSON.stringify(value);
  if (encoded === undefined) throw new TypeError('value is not JSON-serialisable');
  return encoded;
}

export async function sha256Hex(input) {
  const bytes = typeof input === 'string' ? new TextEncoder().encode(input) : input;
  const digest = await globalThis.crypto.subtle.digest('SHA-256', bytes);
  return Array.from(new Uint8Array(digest), byte => byte.toString(16).padStart(2, '0')).join('');
}

export function payloadDigestOf(payload) {
  return sha256Hex(canonical(payload));
}

/**
 * @param {{tier:string,nodeId:string,phase:'ingest'|'deliver',payloadDigest:string,
 *          prev:string|null,executionDigest:string,at:string}} body
 */
export async function sealReceipt(body) {
  const sealed = { v: RECEIPT_VERSION, ...body };
  return Object.freeze({ ...sealed, digest: await sha256Hex(canonical(sealed)) });
}

export async function receiptDigestIsValid(receipt) {
  if (!receipt || typeof receipt !== 'object' || typeof receipt.digest !== 'string') return false;
  const { digest, ...body } = receipt;
  try {
    return digest === (await sha256Hex(canonical(body)));
  } catch {
    return false;
  }
}

const fail = (code, reason) => ({ valid: false, code, reason });

/**
 * Verify a receipt chain end to end.
 *
 * @param {object[]} chain
 * @param {{payloadDigest: string, phase: 'ingest'|'deliver'}} expected
 * @returns {Promise<{valid: true, tiers: string[]} | {valid: false, code: string, reason: string}>}
 */
export async function verifyChain(chain, { payloadDigest, phase }) {
  if (!Array.isArray(chain) || chain.length === 0)
    return fail('CHAIN_EMPTY', 'chain has no receipts');
  const step = phase === 'ingest' ? 1 : phase === 'deliver' ? -1 : 0;
  if (step === 0) return fail('PHASE_UNKNOWN', `unknown phase ${String(phase)}`);

  for (let i = 0; i < chain.length; i++) {
    const receipt = chain[i];
    if (!(await receiptDigestIsValid(receipt))) {
      return fail('RECEIPT_DIGEST_MISMATCH', `receipt ${i} does not hash to its digest`);
    }
    if (receipt.v !== RECEIPT_VERSION)
      return fail('RECEIPT_VERSION_REFUSED', `receipt ${i} has version ${receipt.v}`);
    if (!Object.hasOwn(TIER_LEVEL, receipt.tier))
      return fail('UNKNOWN_TIER', `receipt ${i} names tier ${receipt.tier}`);
    if (receipt.phase !== phase)
      return fail('PHASE_MISMATCH', `receipt ${i} is ${receipt.phase}, expected ${phase}`);
    if (receipt.payloadDigest !== payloadDigest) {
      return fail('PAYLOAD_DIGEST_MISMATCH', `receipt ${i} attests a different payload`);
    }
    if (i === 0) {
      if (receipt.prev !== null)
        return fail('GENESIS_PREV_NOT_NULL', 'first receipt must not link backwards');
    } else {
      if (receipt.prev !== chain[i - 1].digest)
        return fail('CHAIN_LINK_BROKEN', `receipt ${i} does not link to receipt ${i - 1}`);
      if (TIER_LEVEL[receipt.tier] !== TIER_LEVEL[chain[i - 1].tier] + step) {
        return fail(
          'TIER_ORDER_VIOLATION',
          `${chain[i - 1].tier} -> ${receipt.tier} skips or reverses a tier`
        );
      }
    }
  }
  return { valid: true, tiers: chain.map(receipt => receipt.tier) };
}
