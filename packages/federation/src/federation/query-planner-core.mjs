import { createHash } from 'node:crypto';

export class FederationPlanError extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'FederationPlanError';
    this.code = 'FEDERATION_PLAN_ERROR';
    this.details = details;
  }
}

export class FederationExecutionError extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'FederationExecutionError';
    this.code = 'FEDERATION_EXECUTION_ERROR';
    this.details = details;
  }
}

export function canonicalBinding(binding) {
  const out = {};
  for (const key of Object.keys(binding || {}).sort()) out[key] = binding[key];
  return JSON.stringify(out);
}

function stripComments(query) {
  return query.replace(/#[^\n\r]*/g, ' ');
}

function tokenizeWhere(body) {
  const tokens = [];
  let current = '';
  let quote = null;
  let angle = false;
  let depth = 0;
  for (let index = 0; index < body.length; index++) {
    const char = body[index];
    if (quote) {
      current += char;
      if (char === quote && body[index - 1] !== '\\') quote = null;
      continue;
    }
    if (char === '"' || char === "'") { quote = char; current += char; continue; }
    if (char === '<') { angle = true; current += char; continue; }
    if (char === '>' && angle) { angle = false; current += char; continue; }
    if (!angle && char === '{') { depth++; current += char; continue; }
    if (!angle && char === '}') { depth--; current += char; continue; }
    if (!angle && depth === 0 && char === '.') {
      if (current.trim()) tokens.push(current.trim());
      current = '';
      continue;
    }
    current += char;
  }
  if (current.trim()) tokens.push(current.trim());
  return tokens;
}

function parseTerm(token) {
  const trimmed = token.trim();
  if (trimmed.startsWith('?')) return { kind: 'variable', value: trimmed.slice(1) };
  if (trimmed.startsWith('<') && trimmed.endsWith('>')) return { kind: 'iri', value: trimmed.slice(1, -1) };
  if (trimmed.startsWith('"')) return { kind: 'literal', value: trimmed };
  if (trimmed === 'a') return { kind: 'iri', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' };
  return { kind: 'prefixed', value: trimmed };
}

function splitTerms(statement) {
  const terms = [];
  let current = '';
  let quote = null;
  let angle = false;
  for (let index = 0; index < statement.length; index++) {
    const char = statement[index];
    if (quote) {
      current += char;
      if (char === quote && statement[index - 1] !== '\\') quote = null;
      continue;
    }
    if (char === '"' || char === "'") { quote = char; current += char; continue; }
    if (char === '<') { angle = true; current += char; continue; }
    if (char === '>' && angle) { angle = false; current += char; continue; }
    if (!angle && /\s/.test(char)) {
      if (current) { terms.push(current); current = ''; }
    } else current += char;
  }
  if (current) terms.push(current);
  return terms;
}

export function parseBasicSparql(query) {
  if (typeof query !== 'string' || !query.trim()) throw new FederationPlanError('SPARQL query is empty');
  const clean = stripComments(query);
  const select = clean.match(/SELECT\s+(DISTINCT\s+)?([\s\S]*?)\s+WHERE\s*\{/i);
  if (!select) throw new FederationPlanError('Only SELECT ... WHERE queries are supported');
  const start = select.index + select[0].length;
  let depth = 1;
  let end = start;
  for (; end < clean.length && depth > 0; end++) {
    if (clean[end] === '{') depth++;
    else if (clean[end] === '}') depth--;
  }
  if (depth !== 0) throw new FederationPlanError('Unbalanced WHERE braces');
  let body = clean.slice(start, end - 1);
  const variables = select[2].trim() === '*' ? ['*'] : [...select[2].matchAll(/\?(\w+)/g)].map(match => match[1]);
  const patterns = [];
  const filters = [];
  const optionals = [];

  // Pull top-level OPTIONAL blocks before splitting triple statements. This avoids
  // treating `OPTIONAL { ... } FILTER(...)` as one statement when no dot follows
  // the optional group. Nested groups are intentionally outside this basic parser.
  body = body.replace(/OPTIONAL\s*\{([^{}]*)\}/gi, (_match, inner) => {
    optionals.push(...tokenizeWhere(inner).map(parseTripleStatement));
    return ' ';
  });
  body = body.replace(/FILTER\s*\(([^()]*)\)/gi, match => {
    filters.push(match.trim());
    return ' ';
  });
  body = body.replace(/VALUES\s+[^{}]+\{[^{}]*\}/gi, match => {
    filters.push(match.trim());
    return ' ';
  });

  for (const statement of tokenizeWhere(body)) patterns.push(parseTripleStatement(statement));
  if (!patterns.length && !optionals.length) throw new FederationPlanError('Query contains no triple patterns');
  return { distinct: Boolean(select[1]), variables, patterns, optionals, filters, query };
}

function parseTripleStatement(statement) {
  const terms = splitTerms(statement);
  if (terms.length !== 3) throw new FederationPlanError(`Unsupported triple pattern: ${statement}`);
  return { subject: parseTerm(terms[0]), predicate: parseTerm(terms[1]), object: parseTerm(terms[2]), text: statement };
}

function variablesOf(pattern) {
  return ['subject', 'predicate', 'object'].flatMap(field => pattern[field].kind === 'variable' ? [pattern[field].value] : []);
}

function sourceSupports(source, pattern) {
  const metadata = source.metadata || {};
  if (metadata.available === false) return false;
  if (pattern.predicate.kind === 'iri' && Array.isArray(metadata.predicates) && metadata.predicates.length) {
    if (!metadata.predicates.includes(pattern.predicate.value)) return false;
  }
  if (pattern.object.kind === 'iri' && pattern.predicate.value?.endsWith('#type') && Array.isArray(metadata.classes) && metadata.classes.length) {
    if (!metadata.classes.includes(pattern.object.value)) return false;
  }
  return true;
}

function estimate(source, pattern) {
  const metadata = source.metadata || {};
  const predicate = pattern.predicate.kind === 'iri' ? pattern.predicate.value : null;
  const predicateCount = predicate && metadata.predicateCardinality?.[predicate];
  const base = predicateCount ?? metadata.cardinality ?? 1_000_000;
  const constants = ['subject', 'predicate', 'object'].filter(field => pattern[field].kind !== 'variable').length;
  const selectivity = 10 ** constants;
  const latency = metadata.latencyMs ?? 10;
  const reliability = metadata.reliability ?? 1;
  return Math.max(1, base / selectivity) * (1 + latency / 1000) / Math.max(0.01, reliability);
}

function renderTerm(term, binding = {}) {
  if (term.kind === 'variable') {
    const bound = binding[term.value];
    if (bound == null) return `?${term.value}`;
    if (typeof bound === 'string' && (bound.startsWith('<') || bound.startsWith('"') || bound.startsWith('_:'))) return bound;
    return `<${bound.value || bound}>`;
  }
  if (term.kind === 'iri') return `<${term.value}>`;
  return term.value;
}

export function renderPattern(pattern, binding = {}) {
  return `${renderTerm(pattern.subject, binding)} ${renderTerm(pattern.predicate, binding)} ${renderTerm(pattern.object, binding)} .`;
}

export function buildFederationPlan(query, sources, options = {}) {
  const parsed = typeof query === 'string' ? parseBasicSparql(query) : query;
  if (!Array.isArray(sources) || !sources.length) throw new FederationPlanError('No federation sources supplied');
  const steps = [];
  const allPatterns = parsed.patterns.map(pattern => ({ ...pattern, optional: false }))
    .concat(parsed.optionals.map(pattern => ({ ...pattern, optional: true })));

  for (let index = 0; index < allPatterns.length; index++) {
    const pattern = allPatterns[index];
    const candidates = sources.filter(source => sourceSupports(source, pattern)).map(source => ({
      sourceId: source.id,
      source,
      cost: estimate(source, pattern),
    })).sort((a, b) => a.cost - b.cost || a.sourceId.localeCompare(b.sourceId));
    if (!candidates.length) throw new FederationPlanError('No source can satisfy triple pattern', { pattern });
    steps.push({
      id: `pattern-${index}`,
      pattern,
      variables: variablesOf(pattern),
      candidates,
      estimatedRows: Math.ceil(candidates[0].cost),
      optional: pattern.optional,
    });
  }

  const ordered = [];
  const remaining = [...steps];
  const bound = new Set();
  while (remaining.length) {
    remaining.sort((left, right) => {
      const leftShared = left.variables.filter(variable => bound.has(variable)).length;
      const rightShared = right.variables.filter(variable => bound.has(variable)).length;
      return rightShared - leftShared || left.estimatedRows - right.estimatedRows || left.id.localeCompare(right.id);
    });
    const step = remaining.shift();
    ordered.push(step);
    for (const variable of step.variables) bound.add(variable);
  }

  const body = {
    queryHash: createHash('sha256').update(parsed.query || JSON.stringify(parsed)).digest('hex'),
    variables: parsed.variables,
    distinct: parsed.distinct,
    filters: parsed.filters,
    steps: ordered.map(step => ({
      id: step.id,
      pattern: step.pattern,
      variables: step.variables,
      candidates: step.candidates.map(({ sourceId, cost }) => ({ sourceId, cost })),
      estimatedRows: step.estimatedRows,
      optional: step.optional,
    })),
    strategy: options.strategy || 'bind-join',
  };
  return { ...body, planHash: createHash('sha256').update(JSON.stringify(body)).digest('hex'), sources: new Map(sources.map(source => [source.id, source])) };
}

function compatible(left, right) {
  for (const key of Object.keys(left)) if (key in right && JSON.stringify(left[key]) !== JSON.stringify(right[key])) return false;
  return true;
}

export function joinBindings(leftRows, rightRows, optional = false) {
  const result = [];
  for (const left of leftRows) {
    let matched = false;
    for (const right of rightRows) {
      if (!compatible(left, right)) continue;
      matched = true;
      result.push({ ...left, ...right });
    }
    if (optional && !matched) result.push(left);
  }
  return result;
}

function applyDistinct(rows) {
  const seen = new Set();
  return rows.filter(row => {
    const key = canonicalBinding(row);
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

async function executeCandidate(step, candidate, bindings, options) {
  const source = candidate.source;
  if (typeof source.query !== 'function') throw new FederationExecutionError(`Source ${source.id} has no query function`);
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(new Error('source timeout')), options.timeoutMs);
  try {
    const subquery = `SELECT * WHERE { ${renderPattern(step.pattern, bindings)} }`;
    const rows = await source.query(subquery, { bindings, signal: controller.signal, pattern: step.pattern });
    if (!Array.isArray(rows)) throw new FederationExecutionError(`Source ${source.id} returned non-array results`);
    return rows;
  } finally {
    clearTimeout(timeout);
  }
}

export async function executeFederationPlan(plan, options = {}) {
  const config = {
    timeoutMs: options.timeoutMs ?? 30_000,
    failover: options.failover !== false,
    maxIntermediateRows: options.maxIntermediateRows ?? 100_000,
  };
  let rows = [{}];
  const trace = [];

  for (const step of plan.steps) {
    const candidateDefs = step.candidates.map(candidate => ({ ...candidate, source: plan.sources.get(candidate.sourceId) }));
    const perBinding = [];
    for (const binding of rows) {
      let result = null;
      let lastError = null;
      let selected = null;
      for (const candidate of candidateDefs) {
        try {
          result = await executeCandidate(step, candidate, binding, config);
          selected = candidate.sourceId;
          break;
        } catch (error) {
          lastError = error;
          if (!config.failover) throw error;
        }
      }
      if (result == null) throw new FederationExecutionError(`All sources failed for ${step.id}`, { cause: lastError?.message });
      perBinding.push(...joinBindings([binding], result, step.optional));
      trace.push({ stepId: step.id, sourceId: selected, inputBinding: binding, outputRows: result.length });
      if (perBinding.length > config.maxIntermediateRows) throw new FederationExecutionError('Intermediate row bound exceeded', { maxIntermediateRows: config.maxIntermediateRows });
    }
    rows = perBinding;
  }

  if (!plan.variables.includes('*')) rows = rows.map(row => Object.fromEntries(plan.variables.filter(variable => variable in row).map(variable => [variable, row[variable]])));
  // SPARQL DISTINCT applies to the projected solution sequence, not the wider
  // internal bindings used while evaluating the graph pattern.
  if (plan.distinct) rows = applyDistinct(rows);
  rows.sort((a, b) => canonicalBinding(a).localeCompare(canonicalBinding(b)));
  return { rows, trace, planHash: plan.planHash };
}
