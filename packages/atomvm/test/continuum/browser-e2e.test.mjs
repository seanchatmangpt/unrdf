/**
 * Chicago-school end-to-end suite for the BROWSER tier.
 *
 * A real Chromium loads a page that boots the real AtomVM WebAssembly build,
 * runs the browser tier witness inside the page, then drives the first mile
 * (POST to a real edge) and last mile (GET from it) over real cross-origin HTTP
 * with COOP/COEP on, verifying every receipt with WebCrypto in the page.
 *
 * Behind the edge stand real fog and cloud TierNodes running real AtomVM. The
 * only fakes are impostor servers used as falsifiers. No Chromium => the suite
 * FAILS (set CHROMIUM_PATH); it never silently skips the browser tier.
 */
import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { chromium } from '@playwright/test';
import { existsSync, readFileSync } from 'node:fs';
import { createServer } from 'node:http';
import { join } from 'node:path';
import { atomvmAssetName } from '../../src/assets.mjs';
import { payloadDigestOf, sealReceipt } from '../../src/continuum/receipt-chain.mjs';
import { witnessMarker } from '../../src/continuum/tiers.mjs';
import { PACKAGE_ROOT, httpServer, json, request, tierAvm, tierFleet } from './helpers.mjs';

const TEST_TIMEOUT = 90_000;
const MODULES = new Set(['tiers.mjs', 'receipt-chain.mjs', 'browser-client.mjs']);

/** Serves the page shell, the browser-tier ES modules and the AtomVM web build. */
async function pageServer({ isolated = true } = {}) {
  const assets = {
    '/atomvm/AtomVM.js': [
      join(PACKAGE_ROOT, 'public', atomvmAssetName('web', 'js')),
      'text/javascript',
    ],
    '/atomvm/AtomVM.wasm': [
      join(PACKAGE_ROOT, 'public', atomvmAssetName('web', 'wasm')),
      'application/wasm',
    ],
    '/atomvm/tier_browser.avm': [tierAvm('browser'), 'application/octet-stream'],
  };
  const server = createServer((req, res) => {
    const headers = isolated
      ? {
          'cross-origin-opener-policy': 'same-origin',
          'cross-origin-embedder-policy': 'require-corp',
          'cross-origin-resource-policy': 'same-origin',
        }
      : {};
    const send = (status, type, body) => {
      res.writeHead(status, { ...headers, 'content-type': type });
      res.end(body);
    };
    if (req.url === '/')
      return send(
        200,
        'text/html',
        '<!doctype html><title>continuum browser tier</title><body></body>'
      );
    const module = /^\/src\/continuum\/([a-z-]+\.mjs)$/.exec(req.url)?.[1];
    if (module && MODULES.has(module)) {
      return send(
        200,
        'text/javascript',
        readFileSync(join(PACKAGE_ROOT, 'src/continuum', module))
      );
    }
    if (assets[req.url]) return send(200, assets[req.url][1], readFileSync(assets[req.url][0]));
    return send(404, 'text/plain', 'not found');
  });
  await new Promise(resolvePromise => server.listen(0, '127.0.0.1', resolvePromise));
  return {
    url: `http://127.0.0.1:${server.address().port}`,
    close: () =>
      new Promise(resolvePromise => {
        server.closeAllConnections?.();
        server.close(resolvePromise);
      }),
  };
}

/*
 * Code that runs INSIDE the page is kept as plain source strings: the test runner
 * transforms function bodies (rewriting import()), which the browser cannot execute.
 */
const inPage = (page, source, arg) => page.evaluate(`(${source})(${JSON.stringify(arg)})`);

/** In-page: construct the BrowserTier, bootstrap it, report what happened (never throws across the bridge). */
const BOOTSTRAP_IN_PAGE = `async ({ edgeUrl }) => {
  const { BrowserTier } = await import('/src/continuum/browser-client.mjs');
  window.tier = new BrowserTier({
    edgeUrl,
    assets: { scriptUrl: '/atomvm/AtomVM.js', wasmUrl: '/atomvm/AtomVM.wasm', avmUrl: '/atomvm/tier_browser.avm' },
  });
  try {
    return { ok: true, ...(await window.tier.bootstrap()), state: window.tier.state };
  } catch (error) {
    return { ok: false, code: error.code, message: String(error.message), state: window.tier.state };
  }
}`;

/** In-page: construct a BrowserTier without bootstrapping it. */
const UNBOOTED_IN_PAGE = `async ({ edgeUrl }) => {
  const { BrowserTier } = await import('/src/continuum/browser-client.mjs');
  window.tier = new BrowserTier({ edgeUrl, assets: {} });
}`;

const IN_PAGE = {
  ingest: `async ({ payload }) => {
    try {
      return { ok: true, ...(await window.tier.ingest(payload)) };
    } catch (error) {
      return { ok: false, code: error.code, message: String(error.message) };
    }
  }`,
  fetchDelivery: `async ({ digest }) => {
    try {
      return { ok: true, ...(await window.tier.fetchDelivery(digest)) };
    } catch (error) {
      return { ok: false, code: error.code, message: String(error.message) };
    }
  }`,
};

let browser;
const cleanups = [];
beforeAll(async () => {
  const executablePath =
    process.env.CHROMIUM_PATH ?? ['/opt/pw-browsers/chromium'].find(existsSync);
  browser = await chromium.launch({ executablePath, args: ['--no-sandbox'] });
}, TEST_TIMEOUT);
afterAll(async () => {
  await browser?.close();
});
afterEach(async () => {
  while (cleanups.length) await cleanups.pop()();
});

/** Fleet + page origin wired together the way production is: edge only trusts the app origin. */
async function continuum({ edgeOverrides = {}, trust = ['edge'] } = {}) {
  const app = await pageServer();
  cleanups.push(() => app.close());
  const fleet = tierFleet();
  cleanups.push(() => fleet.stopAll());
  const origins = tier => (trust.includes(tier) ? { allowedOrigins: [app.url] } : {});
  const tiers = await fleet.bootstrapAll({
    fog: origins('fog'),
    edge: { ...origins('edge'), ...edgeOverrides },
  });
  const context = await browser.newContext();
  cleanups.push(() => context.close());
  const page = await context.newPage();
  const pageErrors = [];
  page.on('pageerror', error => pageErrors.push(error.message));
  await page.goto(app.url);
  return { app, fleet, ...tiers, page, pageErrors };
}

describe('browser tier: bootstrapping', () => {
  it(
    'boots real AtomVM WASM in the page, proves the browser witness, and confirms the edge is Alive',
    async () => {
      const { edge, page, pageErrors } = await continuum();
      expect(await page.evaluate(() => crossOriginIsolated)).toBe(true);

      const boot = await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: edge.url });
      expect(boot.ok, boot.message).toBe(true);
      expect(boot.state).toBe('Alive');
      expect(boot.marker).toBe(witnessMarker('browser'));
      expect(boot.bootDigest).toMatch(/^[0-9a-f]{64}$/);
      expect(boot.edge).toMatchObject({ tier: 'edge', state: 'Alive' });
      expect(pageErrors).toEqual([]);
    },
    TEST_TIMEOUT
  );

  it(
    'refuses to bootstrap when the edge is unreachable',
    async () => {
      const { edge, page } = await continuum();
      const url = edge.url;
      await edge.stop();
      const boot = await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: url });
      expect(boot.ok).toBe(false);
      expect(boot.state).toBe('Refused');
    },
    TEST_TIMEOUT
  );

  it(
    'refuses to bootstrap against a node that is not an edge (pointed at the fog)',
    async () => {
      const { fog, page } = await continuum({ trust: ['edge', 'fog'] }); // fog reachable from the page, so the refusal is about identity
      const boot = await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: fog.url });
      expect(boot).toMatchObject({ ok: false, code: 'EDGE_NOT_ALIVE_REFUSED', state: 'Refused' });
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a page that is not cross-origin isolated (AtomVM WASM cannot run there)',
    async () => {
      const plain = await pageServer({ isolated: false });
      cleanups.push(() => plain.close());
      const fleet = tierFleet();
      cleanups.push(() => fleet.stopAll());
      const { edge } = await fleet.bootstrapAll({ edge: { allowedOrigins: [plain.url] } });
      const page = await (await browser.newContext()).newPage();
      await page.goto(plain.url);
      const boot = await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: edge.url });
      expect(boot).toMatchObject({
        ok: false,
        code: 'CROSS_ORIGIN_ISOLATION_REFUSED',
        state: 'Refused',
      });
    },
    TEST_TIMEOUT
  );

  it(
    'is denied by an edge that does not list the page origin (default-deny CORS)',
    async () => {
      const { edge, fleet } = await continuum({
        edgeOverrides: { allowedOrigins: ['https://someone-else.example'] },
      });
      const stranger = await pageServer();
      cleanups.push(() => stranger.close());
      const page = await (await browser.newContext()).newPage();
      await page.goto(stranger.url);
      const boot = await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: edge.url });
      expect(boot.ok).toBe(false);
      expect(boot.state).toBe('Refused');
      expect(fleet.nodes.find(node => node.tier === 'edge').recordCount()).toBe(0);
    },
    TEST_TIMEOUT
  );
});

describe('browser tier: first mile and last mile through real cloud, fog and edge', () => {
  it(
    'browser -> edge -> fog -> cloud durably, then back down: warm from the edge and cold from the cloud',
    async () => {
      const { cloud, fog, edge, page, pageErrors } = await continuum();
      const boot = await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: edge.url });
      expect(boot.ok, boot.message).toBe(true);

      // ---- FIRST MILE
      const payload = { user: 'ada', cart: [{ sku: 'A-1', qty: 2 }], note: 'first mile' };
      const digest = await payloadDigestOf(payload);
      const sent = await inPage(page, IN_PAGE.ingest, { payload });
      expect(sent.ok, sent.message).toBe(true);
      expect(sent.digest).toBe(digest); // WebCrypto in the page and node:crypto-free Node agree on identity
      expect(sent.tiers).toEqual(['browser', 'edge', 'fog', 'cloud']);
      expect(sent.chain[0].executionDigest).toBe(boot.bootDigest); // browser receipt is bound to its AtomVM boot
      expect([cloud, fog, edge].map(node => node.hasRecord(digest))).toEqual([true, true, true]);
      for (const node of [cloud, fog, edge]) {
        const [execution] = node.executionReceipts();
        expect(execution.result.stdout).toContain(witnessMarker(node.tier));
        expect(
          sent.chain.some(receipt => receipt.executionDigest === execution.receiptDigest)
        ).toBe(true);
      }

      // ---- LAST MILE (warm): the edge already holds it
      const warm = await inPage(page, IN_PAGE.fetchDelivery, { digest });
      expect(warm.ok, warm.message).toBe(true);
      expect(warm.payload).toEqual(payload);
      expect(warm.servedBy).toBe('edge');
      expect(warm.tiers).toEqual(['edge', 'browser']);
      expect(warm.ingestTiers).toEqual(['browser', 'edge', 'fog', 'cloud']);

      // ---- LAST MILE (cold): data that exists only in the cloud descends through every tier
      const coldPayload = { catalogue: 'winter', items: [1, 2, 3, 4] };
      const coldDigest = await payloadDigestOf(coldPayload);
      const seeded = await request(cloud.url, 'POST', '/ingest', {
        payload: coldPayload,
        origin: 'cms',
        payloadDigest: coldDigest,
      });
      expect(seeded.json.status).toBe('stored');
      expect(edge.hasRecord(coldDigest)).toBe(false);

      const cold = await inPage(page, IN_PAGE.fetchDelivery, { digest: coldDigest });
      expect(cold.ok, cold.message).toBe(true);
      expect(cold.payload).toEqual(coldPayload);
      expect(cold.servedBy).toBe('cloud');
      expect(cold.tiers).toEqual(['cloud', 'fog', 'edge', 'browser']);
      expect(cold.chain.at(-1).prev).toBe(cold.chain.at(-2).digest);
      expect([fog.hasRecord(coldDigest), edge.hasRecord(coldDigest)]).toEqual([true, true]);

      // ---- and a repeat is now an edge hit that does not disturb upstream
      const fogDeliveries = (await request(fog.url, 'GET', '/health')).json.counters.deliveries;
      const repeat = await inPage(page, IN_PAGE.fetchDelivery, { digest: coldDigest });
      expect(repeat.servedBy).toBe('edge');
      expect((await request(fog.url, 'GET', '/health')).json.counters.deliveries).toBe(
        fogDeliveries
      );
      expect(pageErrors).toEqual([]);
    },
    TEST_TIMEOUT
  );

  it(
    'is refused and leaves no trace when the fog is down: the browser is not told its data is safe',
    async () => {
      const { cloud, fog, edge, page } = await continuum();
      expect((await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: edge.url })).ok).toBe(true);
      await fog.stop();
      const sent = await inPage(page, IN_PAGE.ingest, { payload: { doomed: true } });
      expect(sent).toMatchObject({ ok: false, code: 'FIRST_MILE_REFUSED' });
      expect(cloud.recordCount()).toBe(0);
      expect(edge.recordCount()).toBe(0);
    },
    TEST_TIMEOUT
  );

  it(
    'reports a missing record as NOT_FOUND',
    async () => {
      const { edge, page } = await continuum();
      expect((await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: edge.url })).ok).toBe(true);
      const missing = await inPage(page, IN_PAGE.fetchDelivery, { digest: 'c'.repeat(64) });
      expect(missing).toMatchObject({ ok: false, code: 'NOT_FOUND_REFUSED' });
    },
    TEST_TIMEOUT
  );

  it(
    'cannot ingest or fetch before it has bootstrapped',
    async () => {
      const { edge, page } = await continuum();
      await inPage(page, UNBOOTED_IN_PAGE, { edgeUrl: edge.url });
      expect(await inPage(page, IN_PAGE.ingest, { payload: { early: true } })).toMatchObject({
        ok: false,
        code: 'NOT_ALIVE_REFUSED',
      });
      expect(await inPage(page, IN_PAGE.fetchDelivery, { digest: 'd'.repeat(64) })).toMatchObject({
        ok: false,
        code: 'NOT_ALIVE_REFUSED',
      });
    },
    TEST_TIMEOUT
  );
});

describe('browser tier: last-mile falsifiers (an impostor edge)', () => {
  /** An impostor edge that is Alive and CORS-correct, then lies on delivery. */
  async function pageAgainstImpostor(deliver) {
    const app = await pageServer();
    cleanups.push(() => app.close());
    const impostor = await httpServer(async (req, res) => {
      res.setHeader('access-control-allow-origin', app.url);
      res.setHeader('cross-origin-resource-policy', 'cross-origin');
      if (req.method === 'OPTIONS') {
        res.writeHead(204, {
          'access-control-allow-methods': 'GET, POST, OPTIONS',
          'access-control-allow-headers': 'content-type',
        });
        return res.end();
      }
      if (req.url === '/health') return json(res, 200, { tier: 'edge', state: 'Alive' });
      return deliver(req, res);
    });
    cleanups.push(() => impostor.close());
    const page = await (await browser.newContext()).newPage();
    await page.goto(app.url);
    const boot = await inPage(page, BOOTSTRAP_IN_PAGE, { edgeUrl: impostor.url });
    expect(boot.ok, boot.message).toBe(true);
    return page;
  }

  const payload = { balance: 100 };

  it(
    'refuses a delivered payload that does not hash to what was requested',
    async () => {
      const digest = await payloadDigestOf(payload);
      const page = await pageAgainstImpostor((_req, res) =>
        json(res, 200, {
          record: { payload: { balance: 1_000_000 }, origin: 'x', ingestChain: [] },
          chain: [],
          servedBy: 'edge',
        })
      );
      expect(await inPage(page, IN_PAGE.fetchDelivery, { digest })).toMatchObject({
        ok: false,
        code: 'PAYLOAD_TAMPER_REFUSED',
      });
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a correct payload whose delivery receipts were forged',
    async () => {
      const digest = await payloadDigestOf(payload);
      const genuine = await sealReceipt({
        tier: 'edge',
        nodeId: 'evil',
        phase: 'deliver',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const page = await pageAgainstImpostor((_req, res) =>
        json(res, 200, {
          record: { payload, origin: 'x', ingestChain: [] },
          chain: [{ ...genuine, nodeId: 'edited' }],
          servedBy: 'edge',
        })
      );
      expect(await inPage(page, IN_PAGE.fetchDelivery, { digest })).toMatchObject({
        ok: false,
        code: 'LAST_MILE_CHAIN_INVALID_REFUSED',
      });
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a record the cloud never acknowledged (no provenance)',
    async () => {
      const digest = await payloadDigestOf(payload);
      const receipt = await sealReceipt({
        tier: 'edge',
        nodeId: 'evil',
        phase: 'deliver',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const page = await pageAgainstImpostor((_req, res) =>
        json(res, 200, {
          record: { payload, origin: 'x', ingestChain: [] },
          chain: [receipt],
          servedBy: 'edge',
        })
      );
      expect(await inPage(page, IN_PAGE.fetchDelivery, { digest })).toMatchObject({
        ok: false,
        code: 'PROVENANCE_REFUSED',
      });
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a first-mile acknowledgement that never reached the cloud',
    async () => {
      const page = await pageAgainstImpostor(async (req, res) => {
        const chunks = [];
        for await (const chunk of req) chunks.push(chunk);
        const { chain } = JSON.parse(Buffer.concat(chunks));
        return json(res, 200, { status: 'stored', chain }); // echoes only the browser's own receipt
      });
      expect(await inPage(page, IN_PAGE.ingest, { payload: { wish: 'durable' } })).toMatchObject({
        ok: false,
        code: 'FIRST_MILE_NOT_DURABLE_REFUSED',
      });
    },
    TEST_TIMEOUT
  );
});
