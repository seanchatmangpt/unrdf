import { createHash, createHmac, timingSafeEqual } from 'node:crypto';

export class ReceiptVerificationError extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'ReceiptVerificationError';
    this.code = 'RECEIPT_VERIFICATION_ERROR';
    this.details = details;
  }
}

function normalize(value, seen = new WeakSet()) {
  if (value === null || typeof value !== 'object') {
    if (typeof value === 'bigint') return { $bigint: value.toString() };
    if (typeof value === 'number' && !Number.isFinite(value)) return { $number: String(value) };
    return value;
  }
  if (seen.has(value)) throw new TypeError('Cannot canonicalize cyclic data');
  seen.add(value);
  try {
    if (Array.isArray(value)) return value.map(item => normalize(item, seen));
    if (value instanceof Uint8Array) return { $bytes: Buffer.from(value).toString('base64') };
    if (value instanceof Date) return { $date: value.toISOString() };
    if (value instanceof Set) return { $set: [...value].map(item => normalize(item, seen)).sort(compareCanonical) };
    if (value instanceof Map) return { $map: [...value].map(([key, item]) => [normalize(key, seen), normalize(item, seen)]).sort(compareCanonical) };
    const output = {};
    for (const key of Object.keys(value).sort()) output[key] = normalize(value[key], seen);
    return output;
  } finally {
    seen.delete(value);
  }
}

function compareCanonical(left, right) {
  return JSON.stringify(left).localeCompare(JSON.stringify(right));
}

export function canonicalJson(value) {
  return JSON.stringify(normalize(value));
}

export function hashValue(value, algorithm = 'sha256') {
  return createHash(algorithm).update(typeof value === 'string' ? value : canonicalJson(value)).digest('hex');
}

function receiptBody(receipt) {
  const { receiptHash: _hash, signature: _signature, ...body } = receipt;
  return body;
}

export function createReceipt(input, options = {}) {
  if (!input?.subject || !input?.action) throw new TypeError('receipt requires subject and action');
  const body = {
    schema: options.schema || 'unrdf.receipt/1',
    receiptId: input.receiptId || hashValue({ subject: input.subject, action: input.action, nonce: input.nonce || options.nonce || '' }).slice(0, 32),
    subject: input.subject,
    action: input.action,
    actor: input.actor || 'unknown',
    authority: input.authority || null,
    previousReceiptHash: input.previousReceiptHash || null,
    inputHash: input.inputHash || hashValue(input.inputs ?? null),
    outputHash: input.outputHash || hashValue(input.outputs ?? null),
    evidenceHash: input.evidenceHash || hashValue(input.evidence ?? []),
    status: input.status || 'success',
    exitCode: input.exitCode ?? 0,
    timestamp: input.timestamp ?? options.now?.() ?? Date.now(),
    metadata: input.metadata || {},
  };
  const receiptHash = hashValue(body);
  const receipt = { ...body, receiptHash };
  if (options.hmacKey) receipt.signature = signReceipt(receipt, options.hmacKey);
  return receipt;
}

export function signReceipt(receipt, key) {
  const bytes = Buffer.isBuffer(key) ? key : Buffer.from(String(key));
  return createHmac('sha256', bytes).update(receipt.receiptHash).digest('hex');
}

export function verifySignature(receipt, key) {
  if (!receipt.signature) return { valid: false, reason: 'signature missing' };
  const expected = Buffer.from(signReceipt({ ...receipt, signature: undefined }, key), 'hex');
  const actual = Buffer.from(receipt.signature, 'hex');
  return { valid: actual.length === expected.length && timingSafeEqual(actual, expected), reason: actual.length === expected.length && timingSafeEqual(actual, expected) ? null : 'signature mismatch' };
}

export function verifyReceipt(receipt, options = {}) {
  const errors = [];
  for (const field of ['schema', 'receiptId', 'subject', 'action', 'inputHash', 'outputHash', 'evidenceHash', 'receiptHash']) {
    if (!receipt?.[field]) errors.push(`missing ${field}`);
  }
  if (receipt && hashValue(receiptBody(receipt)) !== receipt.receiptHash) errors.push('receipt hash mismatch');
  if (options.inputs !== undefined && hashValue(options.inputs) !== receipt.inputHash) errors.push('input hash mismatch');
  if (options.outputs !== undefined && hashValue(options.outputs) !== receipt.outputHash) errors.push('output hash mismatch');
  if (options.evidence !== undefined && hashValue(options.evidence) !== receipt.evidenceHash) errors.push('evidence hash mismatch');
  if (options.previousReceiptHash !== undefined && receipt.previousReceiptHash !== options.previousReceiptHash) errors.push('previous receipt mismatch');
  if (options.hmacKey) {
    const signature = verifySignature(receipt, options.hmacKey);
    if (!signature.valid) errors.push(signature.reason);
  }
  return { valid: errors.length === 0, errors };
}

export function verifyReceiptChain(receipts, options = {}) {
  const errors = [];
  let previous = options.genesisHash ?? null;
  for (let index = 0; index < receipts.length; index++) {
    const receipt = receipts[index];
    const result = verifyReceipt(receipt, { hmacKey: options.hmacKey, previousReceiptHash: previous });
    if (!result.valid) errors.push({ index, receiptId: receipt?.receiptId, errors: result.errors });
    previous = receipt?.receiptHash || null;
  }
  return { valid: errors.length === 0, errors, head: previous, count: receipts.length };
}

export class ReceiptLedger {
  constructor(options = {}) {
    this.hmacKey = options.hmacKey;
    this.receipts = [];
    this.byId = new Map();
  }

  append(input, options = {}) {
    if (input.receiptId && this.byId.has(input.receiptId)) return this.byId.get(input.receiptId);
    const receipt = createReceipt({ ...input, previousReceiptHash: this.receipts.at(-1)?.receiptHash || null }, { ...options, hmacKey: this.hmacKey });
    if (this.byId.has(receipt.receiptId)) return this.byId.get(receipt.receiptId);
    this.receipts.push(receipt);
    this.byId.set(receipt.receiptId, receipt);
    return receipt;
  }

  verify() {
    return verifyReceiptChain(this.receipts, { hmacKey: this.hmacKey });
  }

  get(receiptId) { return this.byId.get(receiptId) || null; }
  head() { return this.receipts.at(-1) || null; }
  export() { return this.receipts.map(receipt => structuredClone(receipt)); }
}

function pairHash(left, right) {
  return hashValue(`node:${left}:${right}`);
}

export function buildReceiptMerkleTree(receipts) {
  const leaves = receipts.map(receipt => hashValue(`leaf:${receipt.receiptHash}`));
  if (!leaves.length) return { root: hashValue('empty'), leaves, levels: [leaves] };
  const levels = [leaves];
  let current = leaves;
  while (current.length > 1) {
    const next = [];
    for (let index = 0; index < current.length; index += 2) next.push(pairHash(current[index], current[index + 1] || current[index]));
    levels.push(next);
    current = next;
  }
  return { root: current[0], leaves, levels };
}

export function createReceiptMerkleProof(tree, index) {
  if (!Number.isInteger(index) || index < 0 || index >= tree.leaves.length) throw new RangeError('leaf index out of range');
  const siblings = [];
  let cursor = index;
  for (let level = 0; level < tree.levels.length - 1; level++) {
    const nodes = tree.levels[level];
    const siblingIndex = cursor % 2 === 0 ? cursor + 1 : cursor - 1;
    siblings.push({ side: cursor % 2 === 0 ? 'right' : 'left', hash: nodes[siblingIndex] || nodes[cursor] });
    cursor = Math.floor(cursor / 2);
  }
  return { index, leaf: tree.leaves[index], siblings, root: tree.root };
}

export function verifyReceiptMerkleProof(receipt, proof) {
  let hash = hashValue(`leaf:${receipt.receiptHash}`);
  if (hash !== proof.leaf) return false;
  for (const sibling of proof.siblings) hash = sibling.side === 'left' ? pairHash(sibling.hash, hash) : pairHash(hash, sibling.hash);
  return hash === proof.root;
}

function flatten(value, prefix = '', out = {}) {
  if (value === null || typeof value !== 'object' || Array.isArray(value)) {
    out[prefix || '$'] = value;
    return out;
  }
  for (const key of Object.keys(value).sort()) flatten(value[key], prefix ? `${prefix}.${key}` : key, out);
  return out;
}

export function createSelectiveDisclosure(value, disclosedPaths, options = {}) {
  const flat = flatten(value);
  const salt = options.salt || hashValue({ nonce: options.nonce || '', value }).slice(0, 32);
  const commitments = {};
  const disclosed = {};
  for (const [path, field] of Object.entries(flat)) {
    commitments[path] = hashValue({ path, field, salt });
    if (disclosedPaths.includes(path)) disclosed[path] = field;
  }
  return { root: hashValue(commitments), commitments, disclosed, salt };
}

export function verifySelectiveDisclosure(disclosure) {
  const errors = [];
  if (hashValue(disclosure.commitments) !== disclosure.root) errors.push('commitment root mismatch');
  for (const [path, value] of Object.entries(disclosure.disclosed || {})) {
    if (hashValue({ path, field: value, salt: disclosure.salt }) !== disclosure.commitments[path]) errors.push(`field commitment mismatch: ${path}`);
  }
  return { valid: errors.length === 0, errors };
}

export function verifyReplay(receipt, replay) {
  const errors = [];
  if (hashValue(replay.inputs ?? null) !== receipt.inputHash) errors.push('replay input mismatch');
  if (hashValue(replay.outputs ?? null) !== receipt.outputHash) errors.push('replay output mismatch');
  if ((replay.exitCode ?? 0) !== receipt.exitCode) errors.push('replay exit code mismatch');
  return { valid: errors.length === 0, state: errors.length ? 'REPLAY_DIFFERENCE' : 'REPLAY_MATCH', errors };
}
