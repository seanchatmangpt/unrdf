import { createHash } from 'node:crypto';

const WILDCARD = null;

function termKey(term) {
  if (term === null || term === undefined) return null;
  if (typeof term === 'string') return term;
  const type = term.termType ?? 'Unknown';
  switch (type) {
    case 'NamedNode': return `N<${term.value}>`;
    case 'BlankNode': return `B<${term.value}>`;
    case 'Variable': return `V<${term.value}>`;
    case 'DefaultGraph': return 'D<>';
    case 'Literal':
      return `L<${term.value}>@${term.language ?? ''}^^${term.datatype?.value ?? ''}`;
    case 'Quad': return `Q<${quadKey(term)}>`;
    default: return `${type}<${term.value ?? String(term)}>`;
  }
}

function quadKey(quad) {
  return [quad.subject, quad.predicate, quad.object, quad.graph].map(termKey).join(' ');
}

function canonical(value) {
  if (value === null || typeof value !== 'object') return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  return `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}`;
}

function digest(value) {
  return createHash('sha256').update(canonical(value)).digest('hex');
}

function normalizeQuad(quad) {
  if (!quad?.subject || !quad?.predicate || !quad?.object) {
    throw new TypeError('Quad requires subject, predicate, and object');
  }
  return Object.freeze({
    subject: quad.subject,
    predicate: quad.predicate,
    object: quad.object,
    graph: quad.graph ?? { termType: 'DefaultGraph', value: '' },
  });
}

function patternKeys(pattern = {}) {
  return {
    subject: termKey(pattern.subject ?? WILDCARD),
    predicate: termKey(pattern.predicate ?? WILDCARD),
    object: termKey(pattern.object ?? WILDCARD),
    graph: termKey(pattern.graph ?? WILDCARD),
  };
}

function matches(keys, pattern) {
  return (pattern.subject === null || keys.subject === pattern.subject)
    && (pattern.predicate === null || keys.predicate === pattern.predicate)
    && (pattern.object === null || keys.object === pattern.object)
    && (pattern.graph === null || keys.graph === pattern.graph);
}

function addNested(root, keys, value) {
  let current = root;
  for (const key of keys.slice(0, -1)) {
    if (!current.has(key)) current.set(key, new Map());
    current = current.get(key);
  }
  const finalKey = keys.at(-1);
  if (!current.has(finalKey)) current.set(finalKey, new Set());
  current.get(finalKey).add(value);
}

function deleteNested(root, keys, value) {
  const stack = [];
  let current = root;
  for (const key of keys.slice(0, -1)) {
    if (!current.has(key)) return;
    stack.push([current, key]);
    current = current.get(key);
  }
  const finalKey = keys.at(-1);
  const bucket = current.get(finalKey);
  if (!bucket) return;
  bucket.delete(value);
  if (!bucket.size) current.delete(finalKey);
  for (let index = stack.length - 1; index >= 0; index -= 1) {
    const [parent, key] = stack[index];
    const child = parent.get(key);
    if (child?.size === 0) parent.delete(key);
  }
}

function collectNested(root, keys) {
  const visit = (node, index) => {
    if (index === keys.length) return node instanceof Set ? [...node] : [];
    const key = keys[index];
    if (key !== null) {
      const child = node.get(key);
      return child ? visit(child, index + 1) : [];
    }
    return [...node.values()].flatMap(child => visit(child, index + 1));
  };
  return visit(root, 0);
}

function cloneTerm(term) {
  if (!term || typeof term !== 'object') return term;
  const copy = { termType: term.termType, value: term.value };
  if ('language' in term) copy.language = term.language;
  if (term.datatype) copy.datatype = cloneTerm(term.datatype);
  return copy;
}

function serializeQuad(quad) {
  return {
    subject: cloneTerm(quad.subject),
    predicate: cloneTerm(quad.predicate),
    object: cloneTerm(quad.object),
    graph: cloneTerm(quad.graph),
  };
}

function restoreTerm(term) {
  return term ? Object.freeze({ ...term, datatype: term.datatype ? restoreTerm(term.datatype) : undefined }) : term;
}

function restoreQuad(quad) {
  return Object.freeze({
    subject: restoreTerm(quad.subject),
    predicate: restoreTerm(quad.predicate),
    object: restoreTerm(quad.object),
    graph: restoreTerm(quad.graph),
  });
}

/**
 * In-memory incremental RDF index with deterministic snapshots and subscriptions.
 */
export class IncrementalGraphIndex {
  constructor() {
    this.quads = new Map();
    this.spo = new Map();
    this.pos = new Map();
    this.osp = new Map();
    this.graph = new Map();
    this.version = 0;
    this.subscriptions = new Map();
    this.nextSubscriptionId = 1;
  }

  #keys(quad) {
    return {
      subject: termKey(quad.subject),
      predicate: termKey(quad.predicate),
      object: termKey(quad.object),
      graph: termKey(quad.graph),
    };
  }

  #index(key, quad) {
    const keys = this.#keys(quad);
    addNested(this.spo, [keys.subject, keys.predicate, keys.object, keys.graph], key);
    addNested(this.pos, [keys.predicate, keys.object, keys.subject, keys.graph], key);
    addNested(this.osp, [keys.object, keys.subject, keys.predicate, keys.graph], key);
    addNested(this.graph, [keys.graph, keys.subject, keys.predicate, keys.object], key);
  }

  #unindex(key, quad) {
    const keys = this.#keys(quad);
    deleteNested(this.spo, [keys.subject, keys.predicate, keys.object, keys.graph], key);
    deleteNested(this.pos, [keys.predicate, keys.object, keys.subject, keys.graph], key);
    deleteNested(this.osp, [keys.object, keys.subject, keys.predicate, keys.graph], key);
    deleteNested(this.graph, [keys.graph, keys.subject, keys.predicate, keys.object], key);
  }

  #emit(change) {
    for (const subscription of this.subscriptions.values()) {
      if (matches(this.#keys(change.quad), subscription.pattern)) {
        try { subscription.listener(change); } catch { /* listeners are isolated */ }
      }
    }
  }

  add(quad) {
    const normalized = normalizeQuad(quad);
    const key = quadKey(normalized);
    if (this.quads.has(key)) return false;
    this.quads.set(key, normalized);
    this.#index(key, normalized);
    this.version += 1;
    this.#emit(Object.freeze({ type: 'add', quad: normalized, key, version: this.version }));
    return true;
  }

  remove(quad) {
    const key = quadKey(normalizeQuad(quad));
    const existing = this.quads.get(key);
    if (!existing) return false;
    this.#unindex(key, existing);
    this.quads.delete(key);
    this.version += 1;
    this.#emit(Object.freeze({ type: 'remove', quad: existing, key, version: this.version }));
    return true;
  }

  applyBatch(operations) {
    if (!Array.isArray(operations)) throw new TypeError('operations must be an array');
    const normalized = operations.map(operation => {
      if (!['add', 'remove'].includes(operation?.type)) throw new TypeError(`Invalid operation type: ${operation?.type}`);
      return { type: operation.type, quad: normalizeQuad(operation.quad) };
    });
    const applied = [];
    for (const operation of normalized) {
      const changed = operation.type === 'add' ? this.add(operation.quad) : this.remove(operation.quad);
      if (changed) applied.push(operation);
    }
    return Object.freeze({ applied: applied.length, version: this.version });
  }

  plan(pattern = {}) {
    const keys = patternKeys(pattern);
    const candidates = [
      { index: 'SPO', root: this.spo, keys: [keys.subject, keys.predicate, keys.object, keys.graph] },
      { index: 'POS', root: this.pos, keys: [keys.predicate, keys.object, keys.subject, keys.graph] },
      { index: 'OSP', root: this.osp, keys: [keys.object, keys.subject, keys.predicate, keys.graph] },
      { index: 'GRAPH', root: this.graph, keys: [keys.graph, keys.subject, keys.predicate, keys.object] },
    ];
    const prefix = candidate => {
      let count = 0;
      for (const key of candidate.keys) {
        if (key === null) break;
        count += 1;
      }
      return count;
    };
    candidates.sort((left, right) => prefix(right) - prefix(left) || left.index.localeCompare(right.index));
    const selected = candidates[0];
    return Object.freeze({ index: selected.index, boundPrefix: prefix(selected), keys: selected.keys });
  }

  match(pattern = {}) {
    const normalized = patternKeys(pattern);
    const plan = this.plan(pattern);
    const roots = { SPO: this.spo, POS: this.pos, OSP: this.osp, GRAPH: this.graph };
    const keys = plan.index === 'SPO'
      ? [normalized.subject, normalized.predicate, normalized.object, normalized.graph]
      : plan.index === 'POS'
        ? [normalized.predicate, normalized.object, normalized.subject, normalized.graph]
        : plan.index === 'OSP'
          ? [normalized.object, normalized.subject, normalized.predicate, normalized.graph]
          : [normalized.graph, normalized.subject, normalized.predicate, normalized.object];
    return collectNested(roots[plan.index], keys)
      .map(key => this.quads.get(key))
      .filter(Boolean)
      .sort((left, right) => quadKey(left).localeCompare(quadKey(right)));
  }

  estimate(pattern = {}) {
    return this.match(pattern).length;
  }

  subscribe(pattern, listener) {
    if (typeof listener !== 'function') throw new TypeError('listener must be a function');
    const id = this.nextSubscriptionId++;
    this.subscriptions.set(id, { pattern: patternKeys(pattern), listener });
    return () => this.subscriptions.delete(id);
  }

  stats() {
    const subjects = new Set();
    const predicates = new Set();
    const objects = new Set();
    const graphs = new Set();
    for (const quad of this.quads.values()) {
      const keys = this.#keys(quad);
      subjects.add(keys.subject);
      predicates.add(keys.predicate);
      objects.add(keys.object);
      graphs.add(keys.graph);
    }
    return Object.freeze({
      size: this.quads.size,
      version: this.version,
      subjects: subjects.size,
      predicates: predicates.size,
      objects: objects.size,
      graphs: graphs.size,
      subscriptions: this.subscriptions.size,
    });
  }

  snapshot() {
    const body = {
      schema: 'unrdf.incremental-index/v1',
      version: this.version,
      quads: [...this.quads.values()].sort((a, b) => quadKey(a).localeCompare(quadKey(b))).map(serializeQuad),
    };
    return Object.freeze({ ...body, digest: digest(body) });
  }

  restore(snapshot) {
    const { digest: expected, ...body } = snapshot ?? {};
    if (body.schema !== 'unrdf.incremental-index/v1' || digest(body) !== expected) {
      throw new Error('Invalid index snapshot');
    }
    this.clear();
    for (const quad of body.quads) this.add(restoreQuad(quad));
    this.version = body.version;
    return this;
  }

  diff(snapshot) {
    const current = this.snapshot();
    const before = new Map((snapshot?.quads ?? []).map(quad => [quadKey(restoreQuad(quad)), quad]));
    const after = new Map(current.quads.map(quad => [quadKey(restoreQuad(quad)), quad]));
    return Object.freeze({
      added: [...after.keys()].filter(key => !before.has(key)).sort().map(key => after.get(key)),
      removed: [...before.keys()].filter(key => !after.has(key)).sort().map(key => before.get(key)),
      fromVersion: snapshot?.version ?? null,
      toVersion: current.version,
    });
  }

  verify() {
    const expected = [...this.quads.keys()].sort();
    const all = [
      collectNested(this.spo, [null, null, null, null]),
      collectNested(this.pos, [null, null, null, null]),
      collectNested(this.osp, [null, null, null, null]),
      collectNested(this.graph, [null, null, null, null]),
    ];
    const errors = [];
    for (const [index, keys] of all.entries()) {
      const actual = [...new Set(keys)].sort();
      if (canonical(actual) !== canonical(expected)) errors.push(`INDEX_${index}_MISMATCH`);
    }
    for (const [key, quad] of this.quads) if (quadKey(quad) !== key) errors.push(`KEY_MISMATCH:${key}`);
    return Object.freeze({ valid: errors.length === 0, errors });
  }

  compact() {
    const snapshot = this.snapshot();
    this.restore(snapshot);
    return snapshot;
  }

  clear() {
    this.quads.clear();
    this.spo.clear();
    this.pos.clear();
    this.osp.clear();
    this.graph.clear();
    this.version = 0;
  }
}

export function createIncrementalGraphIndex() {
  return new IncrementalGraphIndex();
}

export { termKey, quadKey };
