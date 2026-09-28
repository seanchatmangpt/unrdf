/**
 * @file Federation span conformance (.claude/rules/scoped/p2-otel-tracing.md)
 *
 * Uses an in-memory span exporter to assert, for every span the federation package emits:
 *  - name is `federation.*`
 *  - required attributes are present (node_id, peer_count, receipt_count)
 *  - status is explicitly OK or ERROR (never UNSET), ERROR carries the failure
 *  - each span is ended exactly once (only ended spans are exported; double-end is detected)
 */
import { describe, it, expect, beforeAll, beforeEach, afterAll } from 'vitest';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import {
  BasicTracerProvider,
  InMemorySpanExporter,
  SimpleSpanProcessor,
} from '@opentelemetry/sdk-trace-base';

const exporter = new InMemorySpanExporter();
const provider = new BasicTracerProvider({ spanProcessors: [new SimpleSpanProcessor(exporter)] });
const endedTwice = [];

let createPeerManager;
let createCoordinator;
let createFederationCoordinator;
let createConsensusManager;
let createPredictor;

beforeAll(async () => {
  trace.setGlobalTracerProvider(provider);
  // Detect double span.end(): the SDK calls diag.warn on the second end
  const { diag } = await import('@opentelemetry/api');
  diag.setLogger(
    {
      verbose() {},
      debug() {},
      info() {},
      error() {},
      warn: (...args) => {
        if (String(args[0]).includes('ended Span') || String(args[0]).includes('end() has been')) {
          endedTwice.push(args.join(' '));
        }
      },
    },
    { logLevel: 60 }
  );
  ({ createPeerManager } = await import('../src/federation/peer-manager.mjs'));
  ({ createCoordinator } = await import('../src/federation/coordinator.mjs'));
  ({ createFederationCoordinator } = await import('../src/federation/federation-coordinator.mjs'));
  ({ createConsensusManager } = await import('../src/federation/consensus-manager.mjs'));
  ({ createPredictor } = await import('../src/ml/predictor.mjs'));
});

afterAll(async () => {
  await provider.shutdown();
});

beforeEach(() => {
  exporter.reset();
  endedTwice.length = 0;
});

const spans = () => exporter.getFinishedSpans();
const named = name => spans().filter(s => s.name === name);

function assertConformant(list) {
  expect(list.length).toBeGreaterThan(0);
  for (const s of list) {
    expect(s.name, `span name ${s.name}`).toMatch(/^federation\./);
    expect(s.attributes['federation.node_id'], `${s.name} node_id`).toEqual(expect.any(String));
    expect(s.attributes['federation.peer_count'], `${s.name} peer_count`).toEqual(
      expect.any(Number)
    );
    expect(s.attributes['federation.receipt_count'], `${s.name} receipt_count`).toEqual(
      expect.any(Number)
    );
    expect(
      [SpanStatusCode.OK, SpanStatusCode.ERROR],
      `${s.name} status must be explicit`
    ).toContain(s.status.code);
    if (s.status.code === SpanStatusCode.ERROR) {
      expect(s.status.message, `${s.name} ERROR message`).toBeTruthy();
    }
  }
  expect(endedTwice).toEqual([]);
}

describe('federation span conformance', () => {
  it('peer-manager spans: names, attributes, OK status, error path', () => {
    const pm = createPeerManager();
    pm.registerPeer('p1', 'http://127.0.0.1:1/sparql');
    pm.updateStatus('p1', 'degraded');
    pm.updateStatus('missing', 'degraded');
    pm.unregisterPeer('p1');
    expect(() => pm.registerPeer('bad', 'not-a-url')).toThrow();

    expect(named('federation.register_peer')).toHaveLength(2);
    expect(named('federation.update_peer_status')).toHaveLength(2);
    expect(named('federation.unregister_peer')).toHaveLength(1);
    assertConformant(spans());

    const [okReg, badReg] = named('federation.register_peer');
    expect(okReg.status.code).toBe(SpanStatusCode.OK);
    expect(badReg.status.code).toBe(SpanStatusCode.ERROR);
    expect(badReg.events.some(e => e.name === 'exception')).toBe(true);
  });

  it('peer-manager ping: unreachable peer ends span with ERROR, reachable path never UNSET', async () => {
    const pm = createPeerManager();
    pm.registerPeer('down', 'http://127.0.0.1:1/');
    expect(await pm.ping('down', 500)).toBe(false);
    expect(await pm.ping('unknown')).toBe(false);

    const pings = named('federation.ping_peer');
    expect(pings).toHaveLength(2);
    assertConformant(spans());
    expect(pings[0].status.code).toBe(SpanStatusCode.ERROR);
    expect(pings[1].status.code).toBe(SpanStatusCode.OK);
  });

  it('coordinator: add/remove/query/health spans conform; empty federation query is ERROR', async () => {
    const c = createCoordinator({ strategy: 'broadcast', timeout: 500 });
    const empty = await c.query('SELECT * WHERE { ?s ?p ?o }');
    expect(empty.success).toBe(false);
    await c.addPeer('down', 'http://127.0.0.1:1/');
    c.removePeer('down');
    await c.healthCheck();

    for (const n of [
      'federation.query',
      'federation.add_peer',
      'federation.remove_peer',
      'federation.health_check',
    ]) {
      expect(named(n).length, n).toBeGreaterThan(0);
    }
    assertConformant(spans());
    expect(named('federation.query')[0].status.code).toBe(SpanStatusCode.ERROR);
  });

  it('coordinator predictive bypass: single query span, child spans, each ended once', async () => {
    const c = createCoordinator({
      strategy: 'broadcast',
      timeout: 500,
      enablePredictiveBypass: true,
      predictorConfig: { confidenceThreshold: 0 },
      peers: [{ id: 'down', endpoint: 'http://127.0.0.1:1/' }],
    });
    const result = await c.query('SELECT * WHERE { ?s ?p ?o }');
    expect(result).toBeDefined();

    expect(named('federation.query')).toHaveLength(1);
    expect(named('federation.predictive_bypass')).toHaveLength(1);
    // The bypass decision must actually have been evaluated (not swallowed by an error)
    expect(named('federation.predictive_bypass')[0].status.code).toBe(SpanStatusCode.OK);
    const bypass = named('federation.execute_with_bypass');
    expect(bypass).toHaveLength(1);
    // child of federation.query
    const q = named('federation.query')[0];
    expect(bypass[0].parentSpanContext?.spanId).toBe(q.spanContext().spanId);
    assertConformant(spans());
  });

  it('federation coordinator: initialize / register / deregister spans, ERROR on failure', async () => {
    const fc = createFederationCoordinator({ enableConsensus: false, healthCheckInterval: 60000 });
    await fc.initialize();
    await expect(fc.deregisterStore('nope')).rejects.toThrow('Store not found');
    await expect(fc.registerStore({})).rejects.toThrow();
    await fc.shutdown();

    expect(named('federation.initialize')).toHaveLength(1);
    expect(named('federation.deregister_store')[0].status.code).toBe(SpanStatusCode.ERROR);
    expect(named('federation.register_store')[0].status.code).toBe(SpanStatusCode.ERROR);
    assertConformant(spans());
  });

  it('consensus manager: initialize OK, replicate as non-leader ERROR with quorum id', async () => {
    const cm = createConsensusManager({ nodeId: 'n1' });
    await cm.initialize();
    await expect(cm.replicate({ type: 'X' })).rejects.toThrow('Only leader');
    if (typeof cm.shutdown === 'function') await cm.shutdown();

    const init = named('federation.consensus_initialize')[0];
    expect(init.attributes['federation.node_id']).toBe('n1');
    const rep = named('federation.consensus_replicate')[0];
    expect(rep.status.code).toBe(SpanStatusCode.ERROR);
    expect(rep.attributes['federation.quorum_id']).toEqual(expect.any(String));
    assertConformant(spans());
  });

  it('predictor spans use federation.* names and real SpanStatusCode values', () => {
    const p = createPredictor();
    p.train(); // insufficient samples path
    const features = {
      queryLength: 10,
      healthyPeerCount: 3,
      recentSuccessRate: 0.9,
      avgPeerLatency: 10,
      queryComplexity: 1,
      peerCount: 3,
    };
    try {
      p.predict(features);
    } catch {
      // schema mismatch is fine: we only assert span outcome below
    }
    try {
      p.predict({ bad: true });
    } catch {
      /* expected */
    }
    const list = spans().filter(s => s.name.includes('predictor'));
    expect(list.length).toBeGreaterThanOrEqual(2);
    assertConformant(list);
    const failed = list.filter(s => s.status.code === SpanStatusCode.ERROR);
    expect(failed.length).toBeGreaterThanOrEqual(1);
  });
});
