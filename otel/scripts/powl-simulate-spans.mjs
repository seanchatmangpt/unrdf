#!/usr/bin/env node
/**
 * powl-simulate-spans — POWL AST → Synthetic OTLP trace JSON
 *
 * Generates sample OTLP-compatible trace data conforming to the weaver-derived
 * span schema for a given POWL process model.
 *
 * Concurrency model (mirrors powl-to-weaver.mjs span derivation):
 *   partial_order children → sibling spans (same parentSpanId)
 *   sequential edges      → predecessor span as parent
 *   loop.do              → child span, loop.redo as sibling on re-entry
 *   xor branches         → one branch chosen, becomes child of choice span
 *
 * Usage:
 *   node otel/scripts/powl-simulate-spans.mjs \
 *     --powl-ast artifacts/powl/hotel/ast.json \
 *     --traces 5 \
 *     --output /tmp/hotel-traces.json
 *
 *   # Send to OTLP endpoint:
 *   OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
 *   node otel/scripts/powl-simulate-spans.mjs --powl-ast ... --send
 */

import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const DIR = dirname(dirname(fileURLToPath(import.meta.url)))

// ── ID generation ────────────────────────────────────────────────────────────

let _idCounter = 0
function nextHex(len) {
  return String(++_idCounter).padStart(len, '0')
}
function traceId() { return nextHex(32) }
function spanId()  { return nextHex(16) }

// ── Span tree construction from POWL AST ─────────────────────────────────────

/**
 * Walk POWL AST and generate OTLP span objects.
 * Returns array of span objects in completion order.
 *
 * @param {object} node       — POWL AST node
 * @param {string} tId        — trace ID
 * @param {string|null} parentSId — parent span ID (null = root)
 * @param {number} startMs    — start timestamp ms
 * @param {number} durationMs — duration per activity
 * @param {string} processId  — slug
 * @param {number} [branchIdx] — XOR branch index chosen
 * @returns {{ spans: object[], endMs: number }}
 */
function buildSpans(node, tId, parentSId, startMs, durationMs, processId, rng = Math.random) {
  const toSnake = s => s.toLowerCase().replace(/[\s\-]+/g, '_').replace(/[^a-z0-9_]/g, '')
  const spans = []

  function makeSpan(name, parentId, startNs, endNs, attrs = {}) {
    return {
      traceId: tId,
      spanId: spanId(),
      parentSpanId: parentId,
      name,
      kind: 2,  // SPAN_KIND_INTERNAL
      startTimeUnixNano: String(startNs * 1_000_000),
      endTimeUnixNano: String(endNs * 1_000_000),
      attributes: [
        { key: 'powl.model.slug', value: { stringValue: processId } },
        ...Object.entries(attrs).map(([k, v]) => ({
          key: k,
          value: typeof v === 'boolean' ? { boolValue: v }
               : typeof v === 'number'  ? { doubleValue: v }
               : { stringValue: String(v) },
        })),
      ],
      status: { code: 0 },
    }
  }

  switch (node.type) {
    case 'activity':
    case 'agent_task':
    case 'human_task':
    case 'tool_task': {
      const name = `${processId}.${toSnake(node.label)}`
      const sId = spanId()
      const span = makeSpan(name, parentSId, startMs, startMs + durationMs, {
        'powl.node.type': node.type,
        'powl.activity.label': node.label,
      })
      span.spanId = sId
      spans.push(span)
      return { spans, endMs: startMs + durationMs, lastSpanId: sId }
    }

    case 'silent':
      return { spans, endMs: startMs, lastSpanId: parentSId }

    case 'xor': {
      const choiceIdx = Math.floor(rng() * node.children.length)
      const choiceName = `${processId}.choice.${choiceIdx}`
      const choiceSId = spanId()
      const choiceSpan = makeSpan(choiceName, parentSId, startMs, startMs + durationMs / 2, {
        'powl.node.type': 'xor_branch',
        'powl.xor.branch_index': choiceIdx,
      })
      choiceSpan.spanId = choiceSId
      spans.push(choiceSpan)
      const child = buildSpans(node.children[choiceIdx], tId, choiceSId, startMs + durationMs / 2, durationMs, processId, rng)
      spans.push(...child.spans)
      // Update choice span end to child end
      choiceSpan.endTimeUnixNano = String(child.endMs * 1_000_000)
      return { spans, endMs: child.endMs, lastSpanId: child.lastSpanId }
    }

    case 'loop': {
      const doName = `${processId}.loop.do`
      const doSId = spanId()
      const doParent = makeSpan(doName, parentSId, startMs, startMs + durationMs, { 'powl.node.type': 'loop_do' })
      doParent.spanId = doSId
      spans.push(doParent)
      const doChild = buildSpans(node.doBody, tId, doSId, startMs, durationMs, processId, rng)
      spans.push(...doChild.spans)
      let endMs = doChild.endMs
      doParent.endTimeUnixNano = String(endMs * 1_000_000)

      if (node.redoBody && rng() > 0.5) {
        const redoName = `${processId}.loop.redo`
        const redoSId = spanId()
        const redoParent = makeSpan(redoName, parentSId, endMs, endMs + durationMs, { 'powl.node.type': 'loop_redo' })
        redoParent.spanId = redoSId
        spans.push(redoParent)
        const redoChild = buildSpans(node.redoBody, tId, redoSId, endMs, durationMs, processId, rng)
        spans.push(...redoChild.spans)
        endMs = redoChild.endMs
        redoParent.endTimeUnixNano = String(endMs * 1_000_000)
      }
      return { spans, endMs, lastSpanId: doSId }
    }

    case 'partial_order': {
      // Topological order for execution, siblings share parent
      const childMap = new Map(node.children.map(c => [c.id, c]))
      const adj = new Map(node.children.map(c => [c.id, []]))
      const inDegree = new Map(node.children.map(c => [c.id, 0]))
      for (const { source, target } of (node.edges ?? [])) {
        if (adj.has(source) && adj.has(target)) {
          adj.get(source).push(target)
          inDegree.set(target, inDegree.get(target) + 1)
        }
      }

      let currentStart = startMs
      let maxEnd = startMs
      const endTimes = new Map()
      const queue = [...node.children.filter(c => inDegree.get(c.id) === 0)]

      while (queue.length) {
        const concurrent = [...queue]
        queue.length = 0

        // All concurrent nodes share the same start time
        const levelStart = Math.max(currentStart, ...concurrent.map(c => endTimes.get(c.id) ?? currentStart))
        for (const child of concurrent) {
          const childResult = buildSpans(childMap.get(child.id), tId, parentSId, levelStart, durationMs, processId, rng)
          spans.push(...childResult.spans)
          endTimes.set(child.id, childResult.endMs)
          maxEnd = Math.max(maxEnd, childResult.endMs)

          for (const neighbor of adj.get(child.id)) {
            const deg = inDegree.get(neighbor) - 1
            inDegree.set(neighbor, deg)
            if (deg === 0) queue.push(childMap.get(neighbor))
          }
        }
        currentStart = maxEnd
      }
      return { spans, endMs: maxEnd, lastSpanId: parentSId }
    }

    default:
      return { spans, endMs: startMs, lastSpanId: parentSId }
  }
}

// ── OTLP format builder ────────────────────────────────────────────────────

function buildOtlpTrace(spec, processId, traceIdx, rng = Math.random) {
  const tId = traceId()
  const baseMs = Date.now() + traceIdx * 300_000
  const result = buildSpans(spec.root, tId, null, baseMs, 1000, processId, rng)

  return {
    traceId: tId,
    spans: result.spans,
    resource: {
      attributes: [
        { key: 'service.name', value: { stringValue: processId } },
        { key: 'powl.model.slug', value: { stringValue: processId } },
      ],
    },
  }
}

// ── CLI ────────────────────────────────────────────────────────────────────

function parseArgs(argv) {
  const args = {}
  for (let i = 0; i < argv.length; i++) {
    if (argv[i].startsWith('--')) {
      const key = argv[i].slice(2)
      args[key] = argv[i + 1] ?? true
      i++
    }
  }
  return args
}

const args = parseArgs(process.argv.slice(2))
const astPath = args['powl-ast'] ?? process.env.POWL_AST_PATH
const traceCount = parseInt(args['traces'] ?? '3')
const outputPath = args['output']
const doSend = args['send'] === 'true' || args['send'] === true
const endpoint = process.env.OTEL_EXPORTER_OTLP_ENDPOINT ?? 'http://localhost:4318'

if (!astPath) {
  console.error('Usage: --powl-ast <path> [--traces N] [--output path] [--send]')
  process.exit(1)
}

const spec = JSON.parse(readFileSync(astPath, 'utf-8'))
const processId = spec.metadata?.processId
  ?? spec.metadata?.title?.toLowerCase().replace(/\s+/g, '-')
  ?? 'process'

const traces = []
for (let i = 0; i < traceCount; i++) {
  traces.push(buildOtlpTrace(spec, processId, i))
}

const otlpPayload = {
  resourceSpans: traces.map(t => ({
    resource: t.resource,
    scopeSpans: [{
      scope: { name: 'powl-simulate-spans', version: '1.0.0' },
      spans: t.spans,
    }],
  })),
}

const json = JSON.stringify(otlpPayload, null, 2)

if (outputPath) {
  mkdirSync(dirname(outputPath), { recursive: true })
  writeFileSync(outputPath, json)
  console.log(`Written ${traces.length} traces (${traces.reduce((n, t) => n + t.spans.length, 0)} spans) → ${outputPath}`)
}

if (doSend) {
  const { default: fetch } = await import('node-fetch').catch(() => ({ default: globalThis.fetch }))
  const url = `${endpoint}/v1/traces`
  try {
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: json,
    })
    console.log(`Sent to ${url}: HTTP ${res.status}`)
  } catch (err) {
    console.error(`Send failed: ${err.message}`)
  }
}

if (!outputPath && !doSend) {
  process.stdout.write(json + '\n')
}
