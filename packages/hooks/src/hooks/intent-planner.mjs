import { createHash } from 'node:crypto';

const STRATEGIES = new Set(['refuse', 'priority', 'merge']);

function canonical(value) {
  if (value === null || typeof value !== 'object') return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  return `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}`;
}

function digest(value) {
  return createHash('sha256').update(canonical(value)).digest('hex');
}

function getPath(object, path) {
  if (!path) return object;
  const parts = Array.isArray(path)
    ? path
    : String(path).replace(/\[(\d+)\]/g, '.$1').split('.').filter(Boolean);
  return parts.reduce((value, key) => value?.[key], object);
}

function assertPatternSafe(pattern) {
  const source = pattern instanceof RegExp ? pattern.source : String(pattern);
  if (source.length > 256) throw new TypeError('Regular expression is too long');
  if (/\([^)]*[+*][^)]*\)[+*]|\.\*[+*]|\.\+[*+]/.test(source)) {
    throw new TypeError('Potentially catastrophic regular expression refused');
  }
  return pattern instanceof RegExp ? pattern : new RegExp(source);
}

function compare(operator, actual, expected) {
  switch (operator) {
    case 'eq': return Object.is(actual, expected);
    case 'ne': return !Object.is(actual, expected);
    case 'gt': return actual > expected;
    case 'gte': return actual >= expected;
    case 'lt': return actual < expected;
    case 'lte': return actual <= expected;
    case 'in': return Array.isArray(expected) && expected.some(value => Object.is(value, actual));
    case 'contains':
      return Array.isArray(actual)
        ? actual.some(value => Object.is(value, expected))
        : typeof actual === 'string' && actual.includes(String(expected));
    case 'exists': return expected === false ? actual === undefined : actual !== undefined;
    case 'matches': return typeof actual === 'string' && assertPatternSafe(expected).test(actual);
    default: throw new TypeError(`Unsupported condition operator: ${operator}`);
  }
}

/**
 * Evaluate the hook condition DSL without executing arbitrary code.
 *
 * Supported forms:
 * - `{ all: [condition...] }`
 * - `{ any: [condition...] }`
 * - `{ not: condition }`
 * - `{ path, op, value }`
 *
 * @param {object|boolean} condition
 * @param {object} context
 * @returns {boolean}
 */
export function evaluateCondition(condition, context) {
  if (condition === true || condition === undefined) return true;
  if (condition === false || condition === null) return false;
  if (typeof condition !== 'object' || Array.isArray(condition)) {
    throw new TypeError('Condition must be a boolean or object');
  }
  if ('all' in condition) {
    if (!Array.isArray(condition.all)) throw new TypeError('condition.all must be an array');
    return condition.all.every(item => evaluateCondition(item, context));
  }
  if ('any' in condition) {
    if (!Array.isArray(condition.any)) throw new TypeError('condition.any must be an array');
    return condition.any.some(item => evaluateCondition(item, context));
  }
  if ('not' in condition) return !evaluateCondition(condition.not, context);
  if (!condition.path || !condition.op) {
    throw new TypeError('Leaf conditions require path and op');
  }
  return compare(condition.op, getPath(context, condition.path), condition.value);
}

function materialize(value, context) {
  if (Array.isArray(value)) return value.map(item => materialize(item, context));
  if (value && typeof value === 'object') {
    if (Object.keys(value).length === 1 && typeof value.$context === 'string') {
      return getPath(context, value.$context);
    }
    return Object.fromEntries(
      Object.entries(value).map(([key, item]) => [key, materialize(item, context)]),
    );
  }
  return value;
}

function normalizeRule(rule) {
  if (!rule || typeof rule !== 'object') throw new TypeError('Rule must be an object');
  if (!rule.id || !rule.intent?.type) throw new TypeError('Rule requires id and intent.type');
  if (!rule.intent.authority) throw new TypeError(`Rule ${rule.id} requires intent.authority`);
  return {
    id: String(rule.id),
    priority: Number(rule.priority ?? 0),
    when: rule.when ?? true,
    intent: rule.intent,
  };
}

function normalizeIntent(rule, context, sequence) {
  const parameters = materialize(rule.intent.parameters ?? {}, context);
  const target = materialize(rule.intent.target ?? null, context);
  const dependencies = [...new Set((rule.intent.dependsOn ?? []).map(String))].sort();
  const resources = [...new Set((rule.intent.resources ?? []).map(String))].sort();
  const idempotencyKey = String(
    materialize(rule.intent.idempotencyKey ?? `${rule.id}:${digest({ target, parameters })}`, context),
  );
  const intent = {
    id: rule.intent.id ? String(rule.intent.id) : `${rule.id}:${sequence}`,
    ruleId: rule.id,
    type: String(rule.intent.type),
    authority: String(rule.intent.authority),
    target,
    parameters,
    dependsOn: dependencies,
    resources,
    priority: Number(rule.intent.priority ?? rule.priority),
    idempotencyKey,
  };
  return Object.freeze({ ...intent, digest: digest(intent) });
}

function mergeValues(left, right) {
  if (Array.isArray(left) && Array.isArray(right)) {
    return [...new Map([...left, ...right].map(value => [canonical(value), value])).values()];
  }
  if (
    left && right && typeof left === 'object' && typeof right === 'object'
    && !Array.isArray(left) && !Array.isArray(right)
  ) {
    const keys = [...new Set([...Object.keys(left), ...Object.keys(right)])].sort();
    return Object.fromEntries(keys.map(key => [
      key,
      key in left && key in right ? mergeValues(left[key], right[key]) : (left[key] ?? right[key]),
    ]));
  }
  if (Object.is(left, right)) return left;
  throw new Error('Unmergeable values');
}

function resolveConflicts(intents, strategy) {
  const accepted = [];
  const refused = [];
  for (const intent of intents) {
    const conflicts = accepted.filter(candidate => (
      candidate.resources.some(resource => intent.resources.includes(resource))
      && !Object.is(candidate.target, intent.target)
    ));
    if (!conflicts.length) {
      accepted.push(intent);
      continue;
    }
    if (strategy === 'refuse') {
      refused.push({ intent, reason: 'RESOURCE_CONFLICT', conflicts: conflicts.map(item => item.id) });
      continue;
    }
    if (strategy === 'priority') {
      const strongest = [intent, ...conflicts].sort((a, b) => b.priority - a.priority || a.id.localeCompare(b.id))[0];
      for (const conflict of conflicts) {
        const index = accepted.indexOf(conflict);
        if (conflict !== strongest && index >= 0) {
          accepted.splice(index, 1);
          refused.push({ intent: conflict, reason: 'LOWER_PRIORITY', conflicts: [strongest.id] });
        }
      }
      if (strongest === intent) accepted.push(intent);
      else refused.push({ intent, reason: 'LOWER_PRIORITY', conflicts: [strongest.id] });
      continue;
    }
    try {
      const base = conflicts[0];
      const merged = Object.freeze({
        ...base,
        id: [base.id, intent.id].sort().join('+'),
        parameters: mergeValues(base.parameters, intent.parameters),
        resources: [...new Set([...base.resources, ...intent.resources])].sort(),
        dependsOn: [...new Set([...base.dependsOn, ...intent.dependsOn])].sort(),
        priority: Math.max(base.priority, intent.priority),
        idempotencyKey: digest([base.idempotencyKey, intent.idempotencyKey].sort()),
      });
      const index = accepted.indexOf(base);
      accepted[index] = Object.freeze({ ...merged, digest: digest(merged) });
    } catch {
      refused.push({ intent, reason: 'UNMERGEABLE_CONFLICT', conflicts: conflicts.map(item => item.id) });
    }
  }
  return { accepted, refused };
}

function orderIntents(intents) {
  const byId = new Map(intents.map(intent => [intent.id, intent]));
  const indegree = new Map(intents.map(intent => [intent.id, 0]));
  const outgoing = new Map(intents.map(intent => [intent.id, []]));
  for (const intent of intents) {
    for (const dependency of intent.dependsOn) {
      if (!byId.has(dependency)) throw new Error(`Missing intent dependency: ${dependency}`);
      indegree.set(intent.id, indegree.get(intent.id) + 1);
      outgoing.get(dependency).push(intent.id);
    }
  }
  const ready = [...intents.filter(intent => indegree.get(intent.id) === 0)]
    .sort((a, b) => b.priority - a.priority || a.id.localeCompare(b.id));
  const ordered = [];
  while (ready.length) {
    const intent = ready.shift();
    ordered.push(intent);
    for (const target of outgoing.get(intent.id).sort()) {
      indegree.set(target, indegree.get(target) - 1);
      if (indegree.get(target) === 0) {
        ready.push(byId.get(target));
        ready.sort((a, b) => b.priority - a.priority || a.id.localeCompare(b.id));
      }
    }
  }
  if (ordered.length !== intents.length) throw new Error('Intent dependency cycle detected');
  return ordered;
}

/**
 * Build a deterministic, non-actuating intent plan from declarative rules.
 */
export class IntentPlanner {
  constructor({ rules = [], conflictStrategy = 'refuse' } = {}) {
    if (!STRATEGIES.has(conflictStrategy)) throw new TypeError(`Unsupported conflict strategy: ${conflictStrategy}`);
    this.conflictStrategy = conflictStrategy;
    this.rules = rules.map(normalizeRule).sort((a, b) => b.priority - a.priority || a.id.localeCompare(b.id));
  }

  addRule(rule) {
    this.rules.push(normalizeRule(rule));
    this.rules.sort((a, b) => b.priority - a.priority || a.id.localeCompare(b.id));
    return this;
  }

  plan(context = {}) {
    const matchedRules = [];
    const manufactured = [];
    const deduplicated = [];
    const seen = new Map();
    for (const rule of this.rules) {
      if (!evaluateCondition(rule.when, context)) continue;
      matchedRules.push(rule.id);
      const intent = normalizeIntent(rule, context, manufactured.length);
      if (seen.has(intent.idempotencyKey)) {
        deduplicated.push({ intent, duplicateOf: seen.get(intent.idempotencyKey).id });
        continue;
      }
      seen.set(intent.idempotencyKey, intent);
      manufactured.push(intent);
    }
    const { accepted, refused } = resolveConflicts(manufactured, this.conflictStrategy);
    const intents = orderIntents(accepted);
    const body = {
      schema: 'unrdf.intent-plan/v1',
      strategy: this.conflictStrategy,
      contextDigest: digest(context),
      matchedRules,
      intents,
      refused,
      deduplicated,
    };
    return Object.freeze({ ...body, receipt: Object.freeze({ algorithm: 'sha256', digest: digest(body) }) });
  }
}

export function createIntentPlanner(options) {
  return new IntentPlanner(options);
}
