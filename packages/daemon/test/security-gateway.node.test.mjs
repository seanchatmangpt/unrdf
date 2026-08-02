import assert from 'node:assert/strict';
import { mkdirSync, symlinkSync, writeFileSync } from 'node:fs';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import {
  AuditLedger,
  NonceStore,
  SlidingWindowRateLimiter,
  authorize,
  canonicalRequest,
  createSecurityGateway,
  detectInjection,
  redactSecrets,
  resolveSandboxPath,
  signRequest,
  verifyRequestSignature,
} from '../src/security-gateway.mjs';

function request(overrides = {}) {
  return {
    method: 'POST',
    path: '/v1/query',
    timestamp: 1_000,
    nonce: 'nonce-1',
    headers: { 'content-type': 'application/json', 'x-extra': 'value' },
    body: { query: 'SELECT * WHERE { ?s ?p ?o }' },
    ...overrides,
  };
}

test('canonicalRequest normalizes methods and headers', () => {
  const value = canonicalRequest(request({ method: 'post', headers: { Z: ' 2 ', a: '1' } }));
  assert.equal(value.method, 'POST');
  assert.deepEqual(value.headers, { a: '1', z: '2' });
  assert.equal(value.bodyDigest.length, 64);
});

test('request signatures are stable and timing-safe verified', () => {
  const original = request();
  const signature = signRequest(original, 'secret');
  assert.equal(verifyRequestSignature(original, signature, 'secret'), true);
  assert.equal(verifyRequestSignature({ ...original, body: { query: 'changed' } }, signature, 'secret'), false);
  assert.equal(verifyRequestSignature(original, '00', 'secret'), false);
});

test('NonceStore detects replays and permits reuse after expiry', () => {
  let now = 0;
  const store = new NonceStore({ ttlMs: 10, maxEntries: 2, clock: () => now });
  assert.deepEqual(store.consume('x'), { accepted: true });
  assert.equal(store.consume('x').reason, 'NONCE_REPLAY');
  now = 11;
  assert.deepEqual(store.consume('x'), { accepted: true });
});

test('NonceStore enforces bounded memory', () => {
  const store = new NonceStore({ ttlMs: 100, maxEntries: 2, clock: () => 0 });
  store.consume('a');
  store.consume('b');
  store.consume('c');
  assert.equal(store.entries.size, 2);
  assert.equal(store.entries.has('a'), false);
});

test('SlidingWindowRateLimiter rejects excess requests and recovers', () => {
  let now = 0;
  const limiter = new SlidingWindowRateLimiter({ limit: 2, windowMs: 10, clock: () => now });
  assert.equal(limiter.consume('a').allowed, true);
  assert.equal(limiter.consume('a').remaining, 0);
  assert.equal(limiter.consume('a').allowed, false);
  now = 11;
  assert.equal(limiter.consume('a').allowed, true);
});

test('resolveSandboxPath contains paths and refuses traversal', () => {
  const root = path.join(tmpdir(), 'sandbox-root');
  assert.equal(resolveSandboxPath(root, 'a/b'), path.join(root, 'a/b'));
  assert.throws(() => resolveSandboxPath(root, '../outside'), /PATH_ESCAPE/);
  assert.throws(() => resolveSandboxPath(root, '/absolute'), /PATH_ESCAPE/);
});

test('resolveSandboxPath detects symlink escape when requested', async () => {
  const directory = await mkdtemp(path.join(tmpdir(), 'unrdf-gateway-'));
  const root = path.join(directory, 'root');
  const outside = path.join(directory, 'outside');
  mkdirSync(root);
  mkdirSync(outside);
  writeFileSync(path.join(outside, 'secret'), 'x');
  symlinkSync(outside, path.join(root, 'link'));
  assert.throws(() => resolveSandboxPath(root, 'link/secret', { realpath: true }), /SYMLINK_ESCAPE/);
  await rm(directory, { recursive: true, force: true });
});

test('detectInjection returns contextual findings with locations', () => {
  const command = detectInjection('echo ok && rm -rf /', 'command');
  assert.equal(command[0].type, 'COMMAND_CHAIN');
  assert.equal(command[0].line, 1);
  assert.ok(command[0].column > 1);
  assert.equal(detectInjection("' OR 1=1 --", 'sql').some(item => item.severity === 'high'), true);
  assert.equal(detectInjection('SERVICE <https://evil.test/sparql>', 'sparql')[0].type, 'SPARQL_SERVICE');
});

test('detectInjection catches header and control-character injection', () => {
  assert.equal(detectInjection('ok\r\nX-Evil: yes', 'header')[0].severity, 'critical');
  assert.equal(detectInjection(`safe\u0000bad`, 'generic')[0].type, 'CONTROL_CHARACTER');
});

test('redactSecrets redacts secret keys and token-like strings', () => {
  const value = redactSecrets({
    authorization: 'Bearer abcdefghijklmnopqrstuvwxyz',
    nested: { password: 'secret', note: 'github_pat_1234567890abcdef' },
  });
  assert.equal(value.authorization, '[REDACTED]');
  assert.equal(value.nested.password, '[REDACTED]');
  assert.equal(value.nested.note, '[REDACTED]');
});

const policy = {
  statements: [
    { id: 'writers', effect: 'allow', principals: ['writer'], actions: ['graph:*'], resources: ['urn:graph'] },
    { id: 'blocked-delete', effect: 'deny', principals: ['writer'], actions: ['graph:delete'], resources: ['urn:graph'] },
  ],
};

test('authorize supports roles, action wildcards, and explicit deny', () => {
  const principal = { id: 'alice', roles: ['writer'] };
  assert.equal(authorize(principal, 'graph:read', 'urn:graph', policy).allowed, true);
  assert.equal(authorize(principal, 'graph:delete', 'urn:graph', policy).reason, 'EXPLICIT_DENY');
  assert.equal(authorize(principal, 'graph:read', 'urn:other', policy).reason, 'NO_ALLOW');
});

test('AuditLedger creates and verifies a redacted hash chain', () => {
  const ledger = new AuditLedger();
  ledger.append({ type: 'one', token: 'secret' });
  ledger.append({ type: 'two' });
  assert.equal(ledger.entries[0].event.token, '[REDACTED]');
  assert.equal(ledger.entries[1].previousDigest, ledger.entries[0].digest);
  assert.equal(ledger.verify(), true);
  ledger.entries[0].event.type = 'tampered';
  assert.equal(ledger.verify(), false);
});

test('security gateway permits a valid authorized request', () => {
  let now = 1_000;
  const gateway = createSecurityGateway({ secret: 'secret', policy, clock: () => now });
  const original = request();
  original.signature = signRequest(original, 'secret');
  const result = gateway.process(original, {
    principal: { id: 'alice', roles: ['writer'] },
    action: 'graph:read',
    resource: 'urn:graph',
    scan: [{ context: 'sparql', value: original.body.query }],
  });
  assert.equal(result.allowed, true);
  assert.equal(result.reason, 'PERMITTED');
  assert.equal(gateway.audit.verify(), true);
});

test('security gateway refuses invalid signatures, replay, and rate excess', () => {
  const nonceStore = new NonceStore({ ttlMs: 1000 });
  const rateLimiter = new SlidingWindowRateLimiter({ limit: 1, windowMs: 1000 });
  const gateway = createSecurityGateway({ secret: 'secret', policy, nonceStore, rateLimiter, clock: () => 1000 });
  const invalid = request({ nonce: 'bad' });
  assert.equal(gateway.process(invalid, { principal: { roles: ['writer'] }, action: 'graph:read', resource: 'urn:graph' }).reason, 'INVALID_SIGNATURE');

  const first = request({ nonce: 'one' });
  first.signature = signRequest(first, 'secret');
  assert.equal(gateway.process(first, { principal: { roles: ['writer'] }, action: 'graph:read', resource: 'urn:graph' }).allowed, true);
  assert.equal(gateway.process(first, { principal: { roles: ['writer'] }, action: 'graph:read', resource: 'urn:graph' }).reason, 'NONCE_REPLAY');

  const second = request({ nonce: 'two' });
  second.signature = signRequest(second, 'secret');
  assert.equal(gateway.process(second, { principal: { roles: ['writer'] }, action: 'graph:read', resource: 'urn:graph' }).reason, 'RATE_LIMITED');
});

test('security gateway refuses timestamp skew, unauthorized access, and injection', () => {
  const gateway = createSecurityGateway({ secret: 'secret', policy, clock: () => 1000, maxSkewMs: 10 });
  const stale = request({ nonce: 'stale', timestamp: 0 });
  stale.signature = signRequest(stale, 'secret');
  assert.equal(gateway.process(stale, { principal: { roles: ['writer'] }, action: 'graph:read', resource: 'urn:graph' }).reason, 'TIMESTAMP_SKEW');

  const forbidden = request({ nonce: 'forbidden' });
  forbidden.signature = signRequest(forbidden, 'secret');
  assert.equal(gateway.process(forbidden, { principal: { roles: [] }, action: 'graph:read', resource: 'urn:graph' }).reason, 'FORBIDDEN');

  const injected = request({ nonce: 'injected' });
  injected.signature = signRequest(injected, 'secret');
  assert.equal(gateway.process(injected, {
    principal: { roles: ['writer'] }, action: 'graph:read', resource: 'urn:graph',
    scan: [{ context: 'sparql', value: 'SERVICE <https://evil.test/sparql>' }],
  }).reason, 'INJECTION_DETECTED');
});

test('gateway audit entries never retain supplied credentials', () => {
  const gateway = createSecurityGateway({ secret: 'secret', policy, clock: () => 1000 });
  const original = request({ body: { password: 'super-secret' } });
  original.signature = signRequest(original, 'secret');
  gateway.process(original, { principal: { roles: [] }, action: 'graph:read', resource: 'urn:graph' });
  assert.equal(JSON.stringify(gateway.audit.entries).includes('super-secret'), false);
});
