/**
 * Shared, mock-free helpers for the continuum suites: real TierNodes on real
 * loopback sockets running the real AtomVM (native if installed, else the
 * bundled wasm launcher).
 */
import { createServer } from 'node:http';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { TierNode } from '../../src/continuum/tier-node.mjs';

export const PACKAGE_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
export const tierAvm = tier => join(PACKAGE_ROOT, 'test/fixtures/tiers', `tier_${tier}.avm`);
export const programAvm = name => join(PACKAGE_ROOT, 'test/fixtures/programs', `${name}.avm`);

export async function request(base, method, path, body, headers = {}) {
  const response = await fetch(new URL(path, base), {
    method,
    headers: body === undefined ? headers : { 'content-type': 'application/json', ...headers },
    body: body === undefined ? undefined : typeof body === 'string' ? body : JSON.stringify(body),
  });
  return {
    status: response.status,
    headers: response.headers,
    json: await response.json().catch(() => null),
  };
}

/** Start a TierNode and register it for teardown. */
export function tierFleet() {
  const nodes = [];
  return {
    nodes,
    async start(options) {
      const node = new TierNode({ avmPath: tierAvm(options.tier), ...options });
      nodes.push(node);
      await node.start();
      return node;
    },
    /** Bring up cloud -> fog -> edge in lawful bootstrap order. */
    async bootstrapAll(extra = {}) {
      const cloud = await this.start({ tier: 'cloud', ...extra.cloud });
      const fog = await this.start({ tier: 'fog', upstreamUrl: cloud.url, ...extra.fog });
      const edge = await this.start({ tier: 'edge', upstreamUrl: fog.url, ...extra.edge });
      return { cloud, fog, edge };
    },
    async stopAll() {
      await Promise.allSettled(nodes.map(node => node.stop()));
      nodes.length = 0;
    },
  };
}

/** A real HTTP server whose behaviour the test controls (an impostor tier, a dead network...). */
export async function httpServer(handler) {
  const server = createServer(handler);
  await new Promise(resolvePromise => server.listen(0, '127.0.0.1', resolvePromise));
  return {
    url: `http://127.0.0.1:${server.address().port}`,
    async close() {
      server.closeAllConnections?.();
      await new Promise(resolvePromise => server.close(resolvePromise));
    },
  };
}

export function json(res, status, body) {
  const text = JSON.stringify(body);
  res.writeHead(status, {
    'content-type': 'application/json',
    'content-length': Buffer.byteLength(text),
  });
  res.end(text);
}
