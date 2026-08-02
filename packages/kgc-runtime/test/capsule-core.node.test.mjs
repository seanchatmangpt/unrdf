import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
  canonicalJson,
  createArtifactManifest,
  createCapsule,
  deterministicDigest,
  parseCanonicalJson,
  parseCapsule,
  replayCapsule,
  serializeCapsule,
  verifyCapsule,
} from '../src/capsule-core.mjs';

const identity = { subject: 'repo@example', source: { repository: 'unrdf', sha: 'abc123' } };

function buildCapsule(result = { ok: true }) {
  return createCapsule(identity)
    .addInput('config', { z: 1, a: new Set(['b', 'a']), count: 3n })
    .addTool('node', { version: process.version })
    .recordChange({ path: 'src/a.mjs', operation: 'modify' })
    .addArtifact({ path: 'dist/a.txt', content: 'hello', mediaType: 'text/plain' })
    .seal({ standing: 'PARTIAL_ALIVE', result, exclusions: ['network'] });
}

test('canonicalJson is stable across object insertion order', () => {
  assert.equal(canonicalJson({ b: 2, a: 1 }), canonicalJson({ a: 1, b: 2 }));
  assert.equal(deterministicDigest({ b: 2, a: 1 }), deterministicDigest({ a: 1, b: 2 }));
});

test('canonicalJson preserves rich values', () => {
  const value = {
    undefinedValue: undefined,
    big: 42n,
    date: new Date('2026-01-01T00:00:00.000Z'),
    bytes: Uint8Array.of(1, 2, 3),
    set: new Set(['b', 'a']),
    map: new Map([['b', 2], ['a', 1]]),
    infinity: Infinity,
  };
  const restored = parseCanonicalJson(canonicalJson(value));
  assert.equal(restored.undefinedValue, undefined);
  assert.equal(restored.big, 42n);
  assert.equal(restored.date.toISOString(), '2026-01-01T00:00:00.000Z');
  assert.deepEqual([...restored.bytes], [1, 2, 3]);
  assert.deepEqual([...restored.set], ['a', 'b']);
  assert.deepEqual([...restored.map], [['a', 1], ['b', 2]]);
  assert.equal(restored.infinity, Infinity);
});

test('canonicalJson refuses cycles and functions', () => {
  const cyclic = {};
  cyclic.self = cyclic;
  assert.throws(() => canonicalJson(cyclic), /Cycles/);
  assert.throws(() => canonicalJson({ fn() {} }), /Functions/);
});

test('artifact manifests bind path, type, size and digest', () => {
  const manifest = createArtifactManifest([
    { path: 'b.txt', content: 'b' },
    { path: 'a.txt', content: 'alpha', mediaType: 'text/plain' },
  ]);
  assert.deepEqual(manifest.map(item => item.path), ['a.txt', 'b.txt']);
  assert.equal(manifest[0].size, 5);
  assert.equal(manifest[0].digest.length, 64);
});

test('artifact manifests refuse unsafe and duplicate paths', () => {
  assert.throws(() => createArtifactManifest([{ path: '../x', content: '' }]), /normalized/);
  assert.throws(() => createArtifactManifest([{ path: '/x', content: '' }]), /relative/);
  assert.throws(() => createArtifactManifest([
    { path: 'x', content: '1' },
    { path: 'x', content: '2' },
  ]), /Duplicate/);
});

test('capsule builder seals deterministic evidence with a receipt chain', () => {
  const capsule = buildCapsule();
  assert.equal(capsule.schema, 'unrdf.capsule/v1');
  assert.equal(capsule.receipts.length, 6);
  assert.equal(capsule.receipts.at(-1).digest, capsule.rootDigest);
  assert.equal(capsule.capsuleDigest.length, 64);
  assert.deepEqual(verifyCapsule(capsule), { valid: true, errors: [] });
});

test('sealed capsule builder rejects further mutation', () => {
  const builder = createCapsule(identity);
  builder.seal();
  assert.throws(() => builder.addInput('late', true), /SEALED/);
  assert.throws(() => builder.recordChange({}), /SEALED/);
});

test('capsule builder refuses duplicate identities within collections', () => {
  const builder = createCapsule(identity).addInput('x', 1).addTool('node', {});
  assert.throws(() => builder.addInput('x', 2), /Duplicate/);
  assert.throws(() => builder.addTool('node', {}), /Duplicate/);
  builder.addArtifact({ path: 'x.txt', content: 'one' });
  assert.throws(() => builder.addArtifact({ path: 'x.txt', content: 'two' }), /Duplicate/);
});

test('verifyCapsule detects receipt tampering', () => {
  const capsule = buildCapsule();
  const tampered = parseCapsule(serializeCapsule(capsule));
  tampered.receipts[2].event.name = 'tampered';
  const result = verifyCapsule(tampered);
  assert.equal(result.valid, false);
  assert.ok(result.errors.some(error => error.startsWith('RECEIPT_DIGEST')));
});

test('verifyCapsule detects capsule body tampering', () => {
  const capsule = buildCapsule();
  const tampered = parseCapsule(serializeCapsule(capsule));
  tampered.result.ok = false;
  assert.equal(verifyCapsule(tampered).valid, false);
});

test('replayCapsule reports match for independently built equal capsules', () => {
  const left = buildCapsule();
  const right = buildCapsule();
  assert.equal(left.capsuleDigest, right.capsuleDigest);
  assert.deepEqual(replayCapsule(left, right).differences, []);
  assert.equal(replayCapsule(left, right).state, 'REPLAY_MATCH');
});

test('replayCapsule reports precise differences', () => {
  const result = replayCapsule(buildCapsule({ ok: true }), buildCapsule({ ok: false }));
  assert.equal(result.state, 'REPLAY_DIFFERENCE');
  assert.deepEqual(result.differences.map(item => item.path), ['/result/ok']);
});

test('capsule serialization round-trips rich input values', () => {
  const capsule = buildCapsule();
  const restored = parseCapsule(serializeCapsule(capsule));
  assert.equal(restored.inputs.config.count, 3n);
  assert.deepEqual([...restored.inputs.config.a], ['a', 'b']);
  assert.equal(verifyCapsule(restored).valid, true);
});
