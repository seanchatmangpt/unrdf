#!/usr/bin/env node

// generate-traces.mjs — Synthetic OTLP trace generator
//
// Generates realistic trace data with three scenarios:
//   happy path, error path, slow path
//
// Usage:
//   node generate-traces.mjs                    # default: 50 traces, 10% error rate
//   TRACE_COUNT=100 ERROR_RATE=0.2 node generate-traces.mjs
//   OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 node generate-traces.mjs
//
// Also importable:
//   import { generate } from './generate-traces.mjs'

import { randomUUID } from 'node:crypto';
import { URL } from 'node:url';
import { fileURLToPath, urlToFileURL } from 'node:url';
import { dirname, join } from 'node:path';
import { writeFile } from 'node:fs/promises';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

const ENDPOINT = process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'http://localhost:4318/v1/traces';
const TRACE_COUNT = parseInt(process.env.TRACE_COUNT || '50', 10);
const ERROR_RATE = parseFloat(process.env.ERROR_RATE || '0.1');

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function randomItem(arr) {
  return arr[Math.floor(Math.random() * arr.length)];
}

function randomBetween(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function nanosSinceEpoch() {
  const ms = Date.now();
  const s = Math.floor(ms / 1000);
  const ns = (ms % 1000) * 1_000_000 + Math.floor(Math.random() * 1_000_000);
  return [s.toString(), ns.toString().padStart(9, '0')];
}

function durationNanos(durationMs) {
  const s = Math.floor(durationMs / 1000);
  const ns = (durationMs % 1000) * 1_000_000;
  return [s.toString(), ns.toString().padStart(9, '0')];
}

// ---------------------------------------------------------------------------
// Scenario definitions
// ---------------------------------------------------------------------------

const SERVICES = [
  'frontend',
  'api-gateway',
  'user-service',
  'order-service',
  'payment-service',
  'inventory-service',
  'notification-service',
  'search-service',
];

const OPERATIONS = {
  'frontend': ['HTTP GET /', 'HTTP GET /products', 'HTTP POST /checkout', 'HTTP GET /profile'],
  'api-gateway': ['proxy_request', 'rate_limit_check', 'auth_verify', 'route_request'],
  'user-service': ['getUser', 'createUser', 'updateUser', 'authenticate'],
  'order-service': ['createOrder', 'getOrder', 'listOrders', 'updateOrderStatus'],
  'payment-service': ['processPayment', 'refundPayment', 'getPaymentStatus', 'validateCard'],
  'inventory-service': ['checkStock', 'reserveItem', 'releaseItem', 'restockItem'],
  'notification-service': ['sendEmail', 'sendSMS', 'sendPush', 'queueNotification'],
  'search-service': ['search', 'indexDocument', 'suggest', 'facetQuery'],
};

const ERROR_DESCRIPTIONS = [
  'connection refused',
  'timeout exceeded',
  'internal server error',
  'resource not found',
  'permission denied',
  'invalid request body',
  'service unavailable',
  'rate limit exceeded',
];

function buildTraceId() {
  return randomUUID().replace(/-/g, '');
}

function buildSpanId() {
  return randomUUID().replace(/-/g, '').slice(0, 16);
}

// ---------------------------------------------------------------------------
// Scenario generators
// ---------------------------------------------------------------------------

/**
 * Happy path: service A -> service B -> service C
 * All spans STATUS_CODE_OK, realistic durations
 */
function happyPath() {
  const traceId = buildTraceId();
  const rootSpanId = buildSpanId();
  const [startS, startNs] = nanosSinceEpoch();

  const serviceA = randomItem(SERVICES.slice(0, 4));
  const serviceB = randomItem(SERVICES.slice(4));
  const serviceC = randomItem(SERVICES.slice(4).filter(s => s !== serviceB));

  const durationA = randomBetween(5, 50);
  const durationB = randomBetween(3, 30);
  const durationC = randomBetween(2, 20);

  const [durAs, durAn] = durationNanos(durationA);
  const [durBs, durBn] = durationNanos(durationB);

  const midNs = parseInt(startNs, 10) + durationA * 1_000_000;
  const midS = startS + Math.floor(midNs / 1_000_000_000);
  const midNNs = (midNs % 1_000_000_000).toString().padStart(9, '0');

  const lateNs = parseInt(midNNs, 10) + durationB * 1_000_000;
  const lateS = midS + Math.floor(lateNs / 1_000_000_000);
  const lateNNs = (lateNs % 1_000_000_000).toString().padStart(9, '0');

  const spanA = {
    traceId,
    spanId: rootSpanId,
    parentSpanId: '',
    name: `${serviceA}.${randomItem(OPERATIONS[serviceA])}`,
    kind: 2, // SPAN_KIND_SERVER (internal)
    startTimeUnixNano: `${startS}${startNs}`,
    endTimeUnixNano: `${parseInt(startS, 10) + parseInt(durAs, 10)}${durAn}`,
    attributes: [
      { key: 'service.name', value: { stringValue: serviceA } },
      { key: 'deployment.environment', value: { stringValue: 'synthetic' } },
      { key: 'telemetry.sdk.language', value: { stringValue: 'nodejs' } },
      { key: 'http.method', value: { stringValue: randomItem(['GET', 'POST', 'PUT']) } },
      { key: 'http.status_code', value: { intValue: randomItem([200, 201]) } },
      { key: 'http.url', value: { stringValue: `https://api.example.com/${serviceA}` } },
    ],
    status: { code: 1 }, // STATUS_CODE_OK
  };

  const childBId = buildSpanId();
  const childCId = buildSpanId();

  const spanB = {
    traceId,
    spanId: childBId,
    parentSpanId: rootSpanId,
    name: `${serviceB}.${randomItem(OPERATIONS[serviceB])}`,
    kind: 3, // SPAN_KIND_CLIENT
    startTimeUnixNano: `${midS}${midNNs}`,
    endTimeUnixNano: `${parseInt(midS, 10) + parseInt(durBs, 10)}${durBn}`,
    attributes: [
      { key: 'service.name', value: { stringValue: serviceB } },
      { key: 'deployment.environment', value: { stringValue: 'synthetic' } },
      { key: 'rpc.system', value: { stringValue: 'grpc' } },
      { key: 'rpc.method', value: { stringValue: randomItem(OPERATIONS[serviceB]) } },
      { key: 'rpc.grpc.status_code', value: { intValue: 0 } },
    ],
    status: { code: 1 },
  };

  const [durCs, durCn] = durationNanos(durationC);
  const spanC = {
    traceId,
    spanId: childCId,
    parentSpanId: childBId,
    name: `${serviceC}.${randomItem(OPERATIONS[serviceC])}`,
    kind: 2,
    startTimeUnixNano: `${lateS}${lateNNs}`,
    endTimeUnixNano: `${parseInt(lateS, 10) + parseInt(durCs, 10)}${durCn}`,
    attributes: [
      { key: 'service.name', value: { stringValue: serviceC } },
      { key: 'deployment.environment', value: { stringValue: 'synthetic' } },
      { key: 'db.system', value: { stringValue: 'postgresql' } },
      { key: 'db.operation', value: { stringValue: 'SELECT' } },
    ],
    status: { code: 1 },
  };

  return { traceId, spans: [spanA, spanB, spanC] };
}

/**
 * Error path: root span succeeds but a child span fails with STATUS_CODE_ERROR
 */
function errorPath() {
  const traceId = buildTraceId();
  const rootSpanId = buildSpanId();
  const [startS, startNs] = nanosSinceEpoch();

  const serviceA = randomItem(SERVICES.slice(0, 4));
  const serviceB = randomItem(SERVICES.slice(4));

  const durationA = randomBetween(10, 80);
  const durationB = randomBetween(5, 40);

  const [durAs, durAn] = durationNanos(durationA);
  const [durBs, durBn] = durationNanos(durationB);

  const midNs = parseInt(startNs, 10) + durationA * 1_000_000;
  const midS = startS + Math.floor(midNs / 1_000_000_000);
  const midNNs = (midNs % 1_000_000_000).toString().padStart(9, '0');

  const errorDesc = randomItem(ERROR_DESCRIPTIONS);

  const spanA = {
    traceId,
    spanId: rootSpanId,
    parentSpanId: '',
    name: `${serviceA}.${randomItem(OPERATIONS[serviceA])}`,
    kind: 2,
    startTimeUnixNano: `${startS}${startNs}`,
    endTimeUnixNano: `${parseInt(startS, 10) + parseInt(durAs, 10)}${durAn}`,
    attributes: [
      { key: 'service.name', value: { stringValue: serviceA } },
      { key: 'deployment.environment', value: { stringValue: 'synthetic' } },
      { key: 'http.method', value: { stringValue: randomItem(['GET', 'POST']) } },
      { key: 'http.status_code', value: { intValue: 500 } },
      { key: 'http.url', value: { stringValue: `https://api.example.com/${serviceA}` } },
    ],
    status: {
      code: 2, // STATUS_CODE_ERROR
      message: errorDesc,
    },
    events: [
      {
        name: 'exception',
        timeUnixNano: `${midS}${midNNs}`,
        attributes: [
          { key: 'exception.type', value: { stringValue: 'Error' } },
          { key: 'exception.message', value: { stringValue: errorDesc } },
        ],
      },
    ],
  };

  const childBId = buildSpanId();
  const spanB = {
    traceId,
    spanId: childBId,
    parentSpanId: rootSpanId,
    name: `${serviceB}.${randomItem(OPERATIONS[serviceB])}`,
    kind: 3,
    startTimeUnixNano: `${midS}${midNNs}`,
    endTimeUnixNano: `${parseInt(midS, 10) + parseInt(durBs, 10)}${durBn}`,
    attributes: [
      { key: 'service.name', value: { stringValue: serviceB } },
      { key: 'deployment.environment', value: { stringValue: 'synthetic' } },
      { key: 'rpc.system', value: { stringValue: 'grpc' } },
      { key: 'rpc.method', value: { stringValue: randomItem(OPERATIONS[serviceB]) } },
      { key: 'rpc.grpc.status_code', value: { intValue: 2 } },
    ],
    status: {
      code: 2,
      message: errorDesc,
    },
    events: [
      {
        name: 'exception',
        timeUnixNano: `${midS}${midNNs}`,
        attributes: [
          { key: 'exception.type', value: { stringValue: 'RpcError' } },
          { key: 'exception.message', value: { stringValue: errorDesc } },
        ],
      },
    ],
  };

  return { traceId, spans: [spanA, spanB] };
}

/**
 * Slow path: root span has very high latency (simulates slow DB, queue, etc.)
 */
function slowPath() {
  const traceId = buildTraceId();
  const rootSpanId = buildSpanId();
  const [startS, startNs] = nanosSinceEpoch();

  const service = randomItem(SERVICES);
  const duration = randomBetween(2000, 8000); // 2-8 seconds

  const [durS, durN] = durationNanos(duration);

  const span = {
    traceId,
    spanId: rootSpanId,
    parentSpanId: '',
    name: `${service}.${randomItem(OPERATIONS[service])}`,
    kind: 2,
    startTimeUnixNano: `${startS}${startNs}`,
    endTimeUnixNano: `${parseInt(startS, 10) + parseInt(durS, 10)}${durN}`,
    attributes: [
      { key: 'service.name', value: { stringValue: service } },
      { key: 'deployment.environment', value: { stringValue: 'synthetic' } },
      { key: 'http.method', value: { stringValue: randomItem(['GET', 'POST']) } },
      { key: 'http.status_code', value: { intValue: 200 } },
      { key: 'http.url', value: { stringValue: `https://api.example.com/${service}` } },
    ],
    status: { code: 1 },
  };

  return { traceId, spans: [span] };
}

// ---------------------------------------------------------------------------
// OTLP JSON builder
// ---------------------------------------------------------------------------

function buildOtlpRequest(traces) {
  const resourceSpans = traces.map(({ traceId, spans }) => {
    const scopeSpans = spans.map(span => ({
      spanId: span.spanId,
      traceId: span.traceId,
      parentSpanId: span.parentSpanId || undefined,
      name: span.name,
      kind: span.kind,
      startTimeUnixNano: span.startTimeUnixNano,
      endTimeUnixNano: span.endTimeUnixNano,
      attributes: span.attributes,
      status: span.status,
      events: span.events || undefined,
    }));

    return {
      resource: {
        attributes: [
          { key: 'service.name', value: { stringValue: spans[0].attributes.find(a => a.key === 'service.name')?.value.stringValue || 'unknown' } },
          { key: 'deployment.environment', value: { stringValue: 'synthetic' } },
          { key: 'telemetry.sdk.name', value: { stringValue: 'synthetic-traces' } },
          { key: 'telemetry.sdk.version', value: { stringValue: '1.0.0' } },
        ],
      },
      scopeSpans: [
        {
          scope: { name: 'synthetic-traces', version: '1.0.0' },
          spans: scopeSpans,
        },
      ],
    };
  });

  return { resourceSpans };
}

// ---------------------------------------------------------------------------
// Main generate function
// ---------------------------------------------------------------------------

/**
 *
 */
export async function generate() {
  console.log(`Synthetic trace generator`);
  console.log(`  Endpoint : ${ENDPOINT}`);
  console.log(`  Count    : ${TRACE_COUNT}`);
  console.log(`  Error    : ${(ERROR_RATE * 100).toFixed(1)}%`);
  console.log();

  const traces = [];

  for (let i = 0; i < TRACE_COUNT; i++) {
    const roll = Math.random();
    if (roll < ERROR_RATE) {
      traces.push(errorPath());
    } else if (roll < ERROR_RATE + 0.15) {
      traces.push(slowPath());
    } else {
      traces.push(happyPath());
    }
  }

  const otlpPayload = buildOtlpRequest(traces);

  // Send to OTel Collector
  const url = new URL(ENDPOINT);
  const totalSpans = traces.reduce((sum, t) => sum + t.spans.length, 0);
  const errorCount = traces.filter(t => t.spans.some(s => s.status.code === 2)).length;
  const slowCount = traces.filter(t => {
    const first = t.spans[0];
    const dur = parseInt(first.endTimeUnixNano) - parseInt(first.startTimeUnixNano);
    return dur > 1_000_000_000; // > 1 second
  }).length;

  console.log(`Generated ${traces.length} traces (${totalSpans} spans)`);
  console.log(`  Happy: ${traces.length - errorCount - slowCount}`);
  console.log(`  Error: ${errorCount}`);
  console.log(`  Slow : ${slowCount}`);
  console.log();

  try {
    const response = await fetch(url.toString(), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(otlpPayload),
    });

    if (!response.ok) {
      console.error(`Failed to send traces: HTTP ${response.status} ${response.statusText}`);
      const text = await response.text();
      console.error(text);
      process.exit(1);
    }

    console.log(`Traces sent successfully to ${ENDPOINT}`);
  } catch (err) {
    // In CI or when collector is not running, write to file instead
    console.warn(`Could not reach ${ENDPOINT}: ${err.message}`);
    console.warn(`Writing traces to ${join(__dirname, 'synthetic-traces.json')} instead`);

    await writeFile(
      join(__dirname, 'synthetic-traces.json'),
      JSON.stringify(otlpPayload, null, 2),
    );
    console.log(`Traces written to synthetic-traces.json (${JSON.stringify(otlpPayload).length} bytes)`);
  }
}

// ---------------------------------------------------------------------------
// Direct execution
// ---------------------------------------------------------------------------

if (import.meta.url === urlToFileURL(process.argv[1])) {
  await generate();
}
