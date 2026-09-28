/**
 * @fileoverview Knowledge hooks runtime for the playground
 *
 * A hook selects rows from an RDF store with a SPARQL SELECT and evaluates a
 * list of predicates over those rows. The hook "fires" according to its
 * combine mode (AND / OR). Every evaluation returns a receipt with timings and
 * content hashes (query, predicates, store) so a result can be traced back to
 * exactly what produced it.
 *
 * Built-in predicate kinds: THRESHOLD, ASK, WINDOW. More can be added with
 * registerPredicate().
 *
 * @module playground/hooks
 */

import { createHash } from 'node:crypto';
import { z } from 'zod';
import { selectRows, termValue } from './store.mjs';

const COMPARATORS = {
  '>': (a, b) => a > b,
  '>=': (a, b) => a >= b,
  '<': (a, b) => a < b,
  '<=': (a, b) => a <= b,
  '==': (a, b) => a === b,
  '!=': (a, b) => a !== b
};

const comparatorSchema = z.enum(Object.keys(COMPARATORS));

const hookSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  description: z.string().optional(),
  select: z.string().min(1),
  predicates: z
    .array(
      z.object({
        kind: z.string().min(1),
        spec: z.record(z.string(), z.unknown()).default({})
      })
    )
    .default([]),
  combine: z.enum(['AND', 'OR']).default('AND')
});

/** Registered predicate evaluators keyed by kind. */
const predicateRegistry = new Map();

/**
 * @param {string} text - Text to hash
 * @returns {string} Hex SHA-256 digest
 */
function sha256(text) {
  return createHash('sha256').update(text).digest('hex');
}

/**
 * Compare with one of the supported operators.
 *
 * @param {number|string} left - Left operand
 * @param {string} op - Operator
 * @param {number|string} right - Right operand
 * @returns {boolean} Comparison result
 */
function compare(left, op, right) {
  const parsed = comparatorSchema.parse(op);
  return COMPARATORS[parsed](left, right);
}

/**
 * Register a predicate evaluator.
 *
 * @param {string} kind - Predicate kind (e.g. 'HEALTH_SCORE')
 * @param {(spec: object, ctx: {rows: object[], store: object, hook: object}) => Promise<{ok: boolean, meta?: object}>|{ok: boolean, meta?: object}} evaluator - Evaluator
 * @returns {void}
 */
export function registerPredicate(kind, evaluator) {
  if (typeof kind !== 'string' || kind.length === 0) {
    throw new TypeError('registerPredicate: kind must be a non-empty string');
  }
  if (typeof evaluator !== 'function') {
    throw new TypeError('registerPredicate: evaluator must be a function');
  }
  predicateRegistry.set(kind, evaluator);
}

// THRESHOLD: true when at least one row satisfies `?var op value`.
registerPredicate('THRESHOLD', (spec, { rows }) => {
  const { var: variable, op, value } = z
    .object({ var: z.string().min(1), op: comparatorSchema, value: z.number() })
    .parse(spec);
  const matched = rows.filter(row => {
    const term = row[variable];
    return term !== undefined && compare(Number(termValue(term)), op, value);
  }).length;
  return { ok: matched > 0, meta: { var: variable, op, value, matched, rows: rows.length } };
});

// ASK: runs an ASK query against the whole store.
registerPredicate('ASK', (spec, { store }) => {
  const { query } = z.object({ query: z.string().min(1) }).parse(spec);
  if (!/^\s*(PREFIX\s+\S+\s+<[^>]*>\s*|BASE\s+<[^>]*>\s*)*ASK\b/i.test(query)) {
    throw new Error('ASK predicate requires an ASK query');
  }
  const result = store.query(query);
  return { ok: result === true, meta: { query } };
});

// WINDOW: aggregates ?var over the selected rows and compares the aggregate.
registerPredicate('WINDOW', (spec, { rows }) => {
  const { var: variable, size, op, cmp } = z
    .object({
      var: z.string().min(1),
      size: z.string().optional(),
      op: z.enum(['count', 'sum', 'avg', 'min', 'max']).default('count'),
      cmp: z.object({ op: comparatorSchema, value: z.number() })
    })
    .parse(spec);
  const values = rows
    .filter(row => row[variable] !== undefined)
    .map(row => Number(termValue(row[variable])));
  let aggregate;
  if (op === 'count') aggregate = values.length;
  else if (values.length === 0) aggregate = 0;
  else if (op === 'sum') aggregate = values.reduce((a, b) => a + b, 0);
  else if (op === 'avg') aggregate = values.reduce((a, b) => a + b, 0) / values.length;
  else if (op === 'min') aggregate = Math.min(...values);
  else aggregate = Math.max(...values);
  // `size` is carried in the receipt: the in-memory store has no time
  // dimension, so the window spans every selected row.
  return { ok: compare(aggregate, cmp.op, cmp.value), meta: { var: variable, size, op, aggregate } };
});

/**
 * Validate and normalise a hook definition.
 *
 * @param {object} definition - Hook definition
 * @returns {object} Hook
 * @throws {Error} If the definition is invalid
 */
export function defineHook(definition) {
  const result = hookSchema.safeParse(definition);
  if (!result.success) {
    const detail = result.error.issues
      .map(issue => `${issue.path.join('.') || 'hook'}: ${issue.message}`)
      .join('; ');
    throw new Error(`Invalid hook definition: ${detail}`);
  }
  return Object.freeze(result.data);
}

/**
 * Describe how a hook would be executed without running it.
 *
 * @param {object} hook - Hook from defineHook()
 * @returns {{queryPlan: string, predicatePlan: object[], combine: string}} Plan
 */
export function planHook(hook) {
  const queryType = /^\s*(?:(?:PREFIX|BASE)\b[^\n]*\n\s*)*(\w+)/i.exec(hook.select)?.[1];
  return {
    queryPlan: (queryType || 'UNKNOWN').toUpperCase(),
    predicatePlan: hook.predicates.map((predicate, index) => ({
      order: index,
      kind: predicate.kind,
      registered: predicateRegistry.has(predicate.kind)
    })),
    combine: hook.combine
  };
}

/**
 * Evaluate a hook against a store.
 *
 * @param {object} hook - Hook from defineHook()
 * @param {object} store - Oxigraph-backed store (see store.mjs)
 * @returns {Promise<object>} Receipt
 * @throws {Error} If the hook uses an unregistered predicate kind
 */
export async function evaluateHook(hook, store) {
  const started = performance.now();
  const rows = selectRows(store, hook.select);
  const queryDone = performance.now();

  const results = [];
  for (const predicate of hook.predicates) {
    const evaluator = predicateRegistry.get(predicate.kind);
    if (!evaluator) {
      throw new Error(`Unknown predicate kind: ${predicate.kind}`);
    }
    const outcome = await evaluator(predicate.spec, { rows, store, hook });
    results.push({ kind: predicate.kind, ok: outcome.ok === true, meta: outcome.meta });
  }
  const finished = performance.now();

  let fired;
  if (results.length === 0) fired = rows.length > 0;
  else if (hook.combine === 'OR') fired = results.some(r => r.ok);
  else fired = results.every(r => r.ok);

  const storeDump = store
    .dump({ format: 'application/n-quads' })
    .split('\n')
    .filter(Boolean)
    .sort()
    .join('\n');

  return {
    hookId: hook.id,
    fired,
    rowCount: rows.length,
    predicates: results,
    durations: {
      queryMs: Math.round((queryDone - started) * 1000) / 1000,
      predicateMs: Math.round((finished - queryDone) * 1000) / 1000,
      totalMs: Math.round((finished - started) * 1000) / 1000
    },
    at: new Date().toISOString(),
    provenance: {
      qHash: sha256(hook.select),
      pHash: sha256(JSON.stringify(hook.predicates)),
      sHash: sha256(storeDump)
    }
  };
}
