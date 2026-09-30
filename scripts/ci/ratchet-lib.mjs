/**
 * @file Ratchet helpers shared by the CI security gates.
 *
 * A ratchet fails CI only on findings that are NEW relative to a committed baseline, so existing
 * debt stays visible (and listed in the baseline) without turning every unrelated PR red, while
 * anything that gets worse is still caught.
 */

/**
 * Compare current finding counts against a baseline.
 * @param {Map<string, number>} current - key -> count now
 * @param {Record<string, number>} baseline - key -> accepted count
 * @returns {{added: Array<{key: string, now: number, was: number}>, grown: Array<{key: string, now: number, was: number}>, fixed: Array<{key: string, now: number, was: number}>}}
 */
export function compare(current, baseline) {
  const added = [];
  const grown = [];
  const fixed = [];
  for (const [key, now] of current) {
    const was = baseline[key] ?? 0;
    if (was === 0) added.push({ key, now, was });
    else if (now > was) grown.push({ key, now, was });
    else if (now < was) fixed.push({ key, now, was });
  }
  for (const [key, was] of Object.entries(baseline)) {
    if (!current.has(key)) fixed.push({ key, now: 0, was });
  }
  return { added, grown, fixed };
}

/**
 * Serialize counts as a deterministic (sorted) baseline object.
 * @param {Map<string, number>} current
 * @returns {Record<string, number>}
 */
export function toBaseline(current) {
  return Object.fromEntries([...current].sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)));
}

/**
 * Parse a JSON document that may be preceded by a progress banner on stdout.
 * @param {string} text
 * @returns {unknown}
 */
export function parseJsonLoose(text) {
  const start = text.search(/^[{[]/m);
  if (start < 0) throw new Error('no JSON document found in input');
  return JSON.parse(text.slice(start));
}

/**
 * Group a list of findings into key -> count.
 * @param {Array<Record<string, unknown>>} findings
 * @param {(f: Record<string, unknown>) => string | null} keyOf - return null to skip a finding
 * @returns {Map<string, number>}
 */
export function countBy(findings, keyOf) {
  const out = new Map();
  for (const f of findings) {
    const key = keyOf(f);
    if (key) out.set(key, (out.get(key) ?? 0) + 1);
  }
  return out;
}

/** Severity order used for `--min-severity`. */
export const SEVERITY_RANK = { info: 0, low: 1, moderate: 2, medium: 2, high: 3, critical: 4 };
