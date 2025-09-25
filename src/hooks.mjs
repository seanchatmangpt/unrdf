// src/hooks.mjs
// Minimal unrdf Knowledge Hooks core (ESM) - Composable Integration
// - Uses existing unrdf composables (useStoreContext, useGraph, etc.)
// - defineHook() (pure spec) + evaluateHook() (pure eval) + planHook()
// - Predicates: ASK, THRESHOLD, DELTA, SHACL (stub), WINDOW (tumbling)
// - Receipts: deterministic-ish hashes for data/spec, timings, evidence
// - Front-matter hook loader

import matter from 'gray-matter'
import fs from 'node:fs/promises'
import crypto from 'node:crypto'
import { useStoreContext } from './context/index.mjs'
import { useGraph } from './composables/use-graph.mjs'
import { useCanon } from './composables/use-canon.mjs'
import { defaultStorage } from './utils/storage-utils.mjs'

/** @typedef {{
 *  id: string,
 *  name?: string,
 *  description?: string,
 *  select?: string,
 *  ask?: string,
 *  predicates: Array<
 *    | { kind:'ASK', spec:{ query:string } }
 *    | { kind:'THRESHOLD', spec:{ var:string, op:'>'|'>='|'<'|'<='|'=='|'!=', value:number } }
 *    | { kind:'DELTA', spec:{ key:string[], prev?:string[] } }
 *    | { kind:'SHACL', spec:{ shapes?:string, strict?:boolean } }
 *    | { kind:'WINDOW', spec:{ var:string, size:string, op:'count'|'sum'|'avg', cmp?:{ op:'>'|'>='|'<'|'<='|'=='|'!=', value:number } } }
 *  >,
 *  combine?: 'AND'|'OR',
 *  effect?: (ctx:{ rows:any[], receipt:any, store:any, graph:any }) => Promise<void>|void
 * }} HookSpec */

/** Canonical-ish JSON (stable keys) */
function stable(obj) {
  return JSON.stringify(obj, Object.keys(obj).sort(), 2)
}

/** sha1 of string */
function sha1(s) {
  return crypto.createHash('sha1').update(s).digest('hex')
}

/** Freeze the hook spec for referential transparency */
export function defineHook(spec /** @type {HookSpec} */) {
  const normalized = {
    id: spec.id,
    name: spec.name,
    description: spec.description,
    select: spec.select,
    ask: spec.ask,
    predicates: spec.predicates ?? [],
    combine: spec.combine ?? 'AND',
    effect: spec.effect
  }
  return Object.freeze(normalized)
}

/** Optional: explain how a hook would be evaluated (no execution) */
export function planHook(hook /** @type {HookSpec} */) {
  return {
    id: hook.id,
    name: hook.name,
    queryPlan: hook.select ? 'SELECT' : hook.ask ? 'ASK' : 'none',
    predicatePlan: hook.predicates.map((p, i) => ({ i, kind: p.kind, spec: p.spec })),
    combine: hook.combine ?? 'AND'
  }
}

/** ---- Predicate registry (extensible) ---- */
const predicates = {
  /** ASK: run boolean SPARQL */
  ASK: async (spec, { graph }) => {
    const ok = await graph.ask(spec.query)
    return { ok, meta: { kind: 'ASK' } }
  },

  /** THRESHOLD: filter SELECT rows by numeric comparison on ?var */
  THRESHOLD: async (spec, ctx) => {
    const { rows } = ctx
    const getNum = (row) => {
      const term = row[spec.var]
      const value = term?.value ?? term ?? NaN
      return Number(value)
    }
    const cmp = {
      '>':  (x,y)=>x>y, '>=':(x,y)=>x>=y, '<':  (x,y)=>x<y,
      '<=': (x,y)=>x<=y,'==':(x,y)=>x==y,'!=':(x,y)=>x!=y
    }[spec.op]
    const matched = rows.filter(row => cmp(getNum(row), spec.value))
    return { ok: matched.length>0, meta: { matched: matched.length } }
  },

  /** DELTA: naive digest over key vars; fires if current != previous */
  DELTA: async (spec, ctx) => {
    const { rows, baseline } = ctx
    const digests = rows.map(row => sha1(stable(
      Object.fromEntries(spec.key.map(k => {
        const term = row[k]
        const value = term?.value ?? term ?? null
        return [k, value]
      }))
    )))
    const current = Array.from(new Set(digests)).sort()

    // Use baseline data if available
    let previous = []
    if (baseline && baseline.keyDigests) {
      previous = baseline.keyDigests
    } else if (spec.prev) {
      previous = spec.prev.slice().sort()
    }

    const changed = stable(current) !== stable(previous)
    return {
      ok: changed,
      meta: {
        current,
        previous,
        changeType: spec.change || 'any',
        keyCount: spec.key?.length || 0
      }
    }
  },

  /** SHACL: stub (integrate rdf-validate-shacl later) */
  SHACL: async (spec) => {
    return { ok: true, meta: { note: 'SHACL stub (todo: integrate validator)' } }
  },

  /** WINDOW (tumbling): group by size (e.g. '5m'), aggregate, compare */
  WINDOW: async (spec, ctx) => {
    // Prototype: treat all rows as one window
    const { rows } = ctx
    const nums = rows.map(row => {
      const term = row[spec.var]
      const value = term?.value ?? term ?? NaN
      return Number(value)
    }).filter(n=>!Number.isNaN(n))
    const agg = spec.op === 'count' ? nums.length
             : spec.op === 'sum'   ? nums.reduce((a,b)=>a+b,0)
             : spec.op === 'avg'   ? (nums.length? nums.reduce((a,b)=>a+b,0)/nums.length : 0)
             : 0
    let ok = true
    if (spec.cmp) {
      const f = { '>':(x,y)=>x>y, '>=':(x,y)=>x>=y, '<':(x,y)=>x<y, '<=':(x,y)=>x<=y, '==':(x,y)=>x==y, '!=':(x,y)=>x!=y }[spec.cmp.op]
      ok = f(agg, spec.cmp.value)
    }
    return { ok, meta: { agg, op: spec.op } }
  }
}

/** Allow external registration of new predicate kinds */
export function registerPredicate(kind, impl) {
  predicates[kind] = impl
}

/** Evaluate a hook using existing composables and return a deterministic receipt */
export async function evaluateHook(hook /** @type {HookSpec} */, options = {}) {
  const t0 = Date.now()
  const { persist = true } = options

  // Get composables from context
  const storeContext = useStoreContext()
  const graph = useGraph()
  const canon = useCanon()

  // Load baseline if persisting
  let baseline = null
  if (persist) {
    try {
      baseline = await defaultStorage.loadBaseline(hook.id)
    } catch (error) {
      console.warn(`Warning: Could not load baseline for ${hook.id}: ${error.message}`)
    }
  }

  // Run base query if provided
  let rows = []
  if (hook.select) {
    rows = await graph.select(hook.select)
  } else if (hook.ask && !hook.select) {
    // ASK-only hooks can rely solely on ASK predicate, but we keep rows empty
  }

  // Evaluate predicates with baseline context
  const results = []
  let fired = hook.combine === 'AND'
  for (const p of hook.predicates) {
    const impl = predicates[p.kind]
    if (!impl) throw new Error(`Unknown predicate kind: ${p.kind}`)
    const r = await impl(p.spec, { store: storeContext.store, graph, rows, baseline })
    results.push({ kind: p.kind, ok: r.ok, meta: r.meta })
    fired = hook.combine === 'AND' ? (fired && r.ok) : (fired || r.ok)
  }

  // Provenance-ish hashes
  const prov = {
    hookId: hook.id,
    qHash: hook.select ? sha1(hook.select) : hook.ask ? sha1(hook.ask) : null,
    pHash: sha1(stable(hook.predicates)),
    sHash: sha1(String(storeContext.size)), // crude; replace with URDNA2015 later
    bHash: baseline ? sha1(stable(baseline)) : null
  }

  const receipt = {
    id: hook.id,
    fired,
    predicates: results,
    durations: { totalMs: Date.now() - t0 },
    provenance: prov,
    at: new Date().toISOString(),
    baselineUsed: baseline ? true : false
  }

  // Optional effect (pure core keeps effects outside the decision)
  if (fired && typeof hook.effect === 'function') {
    await hook.effect({ rows, receipt, store: storeContext.store, graph, baseline })
  }

  // Persist receipt and baseline if requested
  if (persist) {
    try {
      await defaultStorage.saveReceipt(receipt)

      // Update baseline with current data
      const newBaseline = {
        hookId: hook.id,
        timestamp: receipt.at,
        dataHash: sha1(stable(rows)),
        provenanceHash: prov.sHash,
        rowCount: rows.length,
        predicates: hook.predicates.map(p => ({ kind: p.kind, spec: p.spec }))
      }
      await defaultStorage.saveBaseline(hook.id, newBaseline)
    } catch (error) {
      console.warn(`Warning: Could not persist evaluation data: ${error.message}`)
    }
  }

  return receipt
}

/** Load a hook spec from a Markdown front-matter file */
export async function loadFrontmatterHook(filePath) {
  const raw = await fs.readFile(filePath, 'utf8')
  const fm = matter(raw)
  if (!fm.data?.hook?.id) throw new Error(`Missing hook.id in ${filePath}`)
  return defineHook(fm.data.hook)
}
