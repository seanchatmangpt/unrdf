import { createHash, createHmac, timingSafeEqual } from 'node:crypto';
import { realpathSync } from 'node:fs';
import path from 'node:path';

const SAFE_METHODS = new Set(['GET', 'POST', 'PUT', 'PATCH', 'DELETE']);
const SECRET_KEY = /(authorization|cookie|secret|token|password|api[-_]?key|private[-_]?key)/i;

function canonical(value) {
  if (value === null || typeof value !== 'object') return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  return `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}`;
}

function digest(value) {
  return createHash('sha256').update(canonical(value)).digest('hex');
}

function normalizedHeaders(headers = {}) {
  return Object.fromEntries(Object.entries(headers)
    .map(([key, value]) => [key.toLowerCase(), String(value).trim()])
    .filter(([key]) => key !== 'x-unrdf-signature')
    .sort(([a], [b]) => a.localeCompare(b)));
}

export function canonicalRequest(request) {
  const method = String(request.method ?? '').toUpperCase();
  if (!SAFE_METHODS.has(method)) throw new TypeError(`Unsupported method: ${method}`);
  const pathname = String(request.path ?? '/');
  if (!pathname.startsWith('/')) throw new TypeError('Request path must be absolute');
  return {
    method,
    path: pathname,
    timestamp: Number(request.timestamp),
    nonce: String(request.nonce ?? ''),
    headers: normalizedHeaders(request.headers),
    bodyDigest: digest(request.body ?? null),
  };
}

export function signRequest(request, secret, algorithm = 'sha256') {
  if (!secret) throw new TypeError('Signing secret is required');
  return createHmac(algorithm, secret).update(canonical(canonicalRequest(request))).digest('hex');
}

export function verifyRequestSignature(request, signature, secret, algorithm = 'sha256') {
  if (typeof signature !== 'string') return false;
  const expected = Buffer.from(signRequest(request, secret, algorithm), 'hex');
  const provided = Buffer.from(signature, 'hex');
  return expected.length === provided.length && timingSafeEqual(expected, provided);
}

export class NonceStore {
  constructor({ ttlMs = 5 * 60_000, maxEntries = 10_000, clock = () => Date.now() } = {}) {
    if (!(ttlMs > 0) || !(maxEntries > 0)) throw new TypeError('NonceStore bounds must be positive');
    this.ttlMs = ttlMs;
    this.maxEntries = maxEntries;
    this.clock = clock;
    this.entries = new Map();
  }

  purge(now = this.clock()) {
    for (const [key, expiresAt] of this.entries) if (expiresAt <= now) this.entries.delete(key);
  }

  consume(nonce, now = this.clock()) {
    const key = String(nonce ?? '');
    if (!key) return { accepted: false, reason: 'NONCE_REQUIRED' };
    this.purge(now);
    if (this.entries.has(key)) return { accepted: false, reason: 'NONCE_REPLAY' };
    while (this.entries.size >= this.maxEntries) this.entries.delete(this.entries.keys().next().value);
    this.entries.set(key, now + this.ttlMs);
    return { accepted: true };
  }
}

export class SlidingWindowRateLimiter {
  constructor({ limit = 100, windowMs = 60_000, maxSubjects = 10_000, clock = () => Date.now() } = {}) {
    if (!(limit > 0) || !(windowMs > 0)) throw new TypeError('Rate limiter bounds must be positive');
    this.limit = limit;
    this.windowMs = windowMs;
    this.maxSubjects = maxSubjects;
    this.clock = clock;
    this.subjects = new Map();
  }

  consume(subject, now = this.clock()) {
    const key = String(subject ?? 'anonymous');
    const cutoff = now - this.windowMs;
    const timestamps = (this.subjects.get(key) ?? []).filter(value => value > cutoff);
    if (timestamps.length >= this.limit) {
      return { allowed: false, remaining: 0, retryAfterMs: Math.max(0, timestamps[0] + this.windowMs - now) };
    }
    timestamps.push(now);
    this.subjects.delete(key);
    this.subjects.set(key, timestamps);
    while (this.subjects.size > this.maxSubjects) this.subjects.delete(this.subjects.keys().next().value);
    return { allowed: true, remaining: this.limit - timestamps.length, retryAfterMs: 0 };
  }
}

export function resolveSandboxPath(root, candidate, { realpath = false } = {}) {
  if (!root) throw new TypeError('Sandbox root is required');
  const rootPath = path.resolve(root);
  const target = path.resolve(rootPath, String(candidate ?? '.'));
  if (target !== rootPath && !target.startsWith(`${rootPath}${path.sep}`)) {
    throw new Error('PATH_ESCAPE');
  }
  if (!realpath) return target;
  const realRoot = realpathSync(rootPath);
  const realTarget = realpathSync(target);
  if (realTarget !== realRoot && !realTarget.startsWith(`${realRoot}${path.sep}`)) {
    throw new Error('SYMLINK_ESCAPE');
  }
  return realTarget;
}

function lineAndColumn(text, offset) {
  const prefix = text.slice(0, offset);
  const lines = prefix.split('\n');
  return { line: lines.length, column: lines.at(-1).length + 1 };
}

function finding(type, severity, text, match, message) {
  return {
    type,
    severity,
    message,
    offset: match.index,
    length: match[0].length,
    sample: text.slice(match.index, match.index + Math.min(match[0].length, 80)),
    ...lineAndColumn(text, match.index),
  };
}

export function detectInjection(text, context = 'generic') {
  const input = String(text ?? '');
  const patterns = {
    command: [
      ['COMMAND_CHAIN', 'high', /(?:^|\s)(?:&&|\|\||;|`|\$\()/g, 'Shell command chaining or substitution'],
      ['COMMAND_REDIRECT', 'medium', /(?:^|\s)(?:>>?|<<?)\s*[^\s]/g, 'Shell redirection'],
    ],
    sql: [
      ['SQL_COMMENT', 'high', /(?:--|\/\*)/g, 'SQL comment injection'],
      ['SQL_UNION', 'high', /\bunion\s+(?:all\s+)?select\b/gi, 'SQL UNION injection'],
      ['SQL_STACKED', 'high', /;\s*(?:drop|delete|update|insert|alter|create)\b/gi, 'Stacked SQL statement'],
      ['SQL_BOOLEAN', 'medium', /(?:'|\")\s*or\s+(?:'[^']*'\s*=\s*'[^']*'|\d+\s*=\s*\d+)/gi, 'SQL boolean bypass'],
    ],
    sparql: [
      ['SPARQL_UPDATE', 'high', /\b(?:load|clear|drop|create|copy|move|add|insert|delete)\b/gi, 'SPARQL update keyword'],
      ['SPARQL_SERVICE', 'high', /\bservice\s*<[^>]+>/gi, 'SPARQL remote SERVICE clause'],
      ['SPARQL_COMMENT', 'medium', /#[^\n]*/g, 'SPARQL comment'],
    ],
    header: [
      ['HEADER_INJECTION', 'critical', /[\r\n]/g, 'HTTP header line injection'],
    ],
    generic: [
      ['CONTROL_CHARACTER', 'high', /[\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F]/g, 'Unsafe control character'],
    ],
  };
  const selected = context === 'generic' ? patterns.generic : [...(patterns[context] ?? []), ...patterns.generic];
  const findings = [];
  for (const [type, severity, pattern, message] of selected) {
    pattern.lastIndex = 0;
    for (const match of input.matchAll(pattern)) findings.push(finding(type, severity, input, match, message));
  }
  return findings.sort((a, b) => a.offset - b.offset || a.type.localeCompare(b.type));
}

export function redactSecrets(value, options = {}) {
  const replacement = options.replacement ?? '[REDACTED]';
  const redactString = input => String(input)
    .replace(/\b(?:bearer\s+)?[A-Za-z0-9_-]{24,}\b/gi, replacement)
    .replace(/\b(?:sk|pk|ghp|github_pat)_[A-Za-z0-9_-]{8,}\b/gi, replacement);
  const visit = (item, key = '') => {
    if (SECRET_KEY.test(key)) return replacement;
    if (Array.isArray(item)) return item.map(value => visit(value));
    if (item && typeof item === 'object') {
      return Object.fromEntries(Object.entries(item).map(([childKey, child]) => [childKey, visit(child, childKey)]));
    }
    return typeof item === 'string' ? redactString(item) : item;
  };
  return visit(value);
}

function actionMatches(pattern, action) {
  if (pattern === '*' || pattern === action) return true;
  if (pattern.endsWith(':*')) return action.startsWith(pattern.slice(0, -1));
  return false;
}

export function authorize(principal, action, resource, policy = {}) {
  const principalId = String(principal?.id ?? principal ?? 'anonymous');
  const roles = new Set(principal?.roles ?? []);
  const applicable = (policy.statements ?? []).filter(statement => (
    (statement.principals ?? ['*']).some(value => value === '*' || value === principalId || roles.has(value))
    && (statement.actions ?? []).some(value => actionMatches(value, action))
    && (statement.resources ?? ['*']).some(value => value === '*' || value === resource)
  ));
  const denied = applicable.find(statement => String(statement.effect).toLowerCase() === 'deny');
  if (denied) return { allowed: false, reason: 'EXPLICIT_DENY', statement: denied.id ?? null };
  const allowed = applicable.find(statement => String(statement.effect).toLowerCase() === 'allow');
  return allowed
    ? { allowed: true, reason: 'ALLOWED', statement: allowed.id ?? null }
    : { allowed: false, reason: 'NO_ALLOW' };
}

export class AuditLedger {
  constructor() { this.entries = []; }

  append(event) {
    const previous = this.entries.at(-1);
    const body = {
      sequence: this.entries.length,
      previousDigest: previous?.digest ?? null,
      event: redactSecrets(event),
    };
    const entry = Object.freeze({ ...body, digest: digest(body) });
    this.entries.push(entry);
    return entry;
  }

  verify() {
    let previous = null;
    for (const entry of this.entries) {
      const { digest: entryDigest, ...body } = entry;
      if (entry.sequence !== (previous ? previous.sequence + 1 : 0)) return false;
      if (entry.previousDigest !== (previous?.digest ?? null)) return false;
      if (digest(body) !== entryDigest) return false;
      previous = entry;
    }
    return true;
  }
}

export function createSecurityGateway({
  secret,
  maxSkewMs = 60_000,
  nonceStore = new NonceStore(),
  rateLimiter = new SlidingWindowRateLimiter(),
  policy = { statements: [] },
  audit = new AuditLedger(),
  clock = () => Date.now(),
} = {}) {
  if (!secret) throw new TypeError('Gateway secret is required');
  return {
    audit,
    process(request, { principal, action, resource = request.path, scan = [] } = {}) {
      const now = clock();
      let canonicalized;
      const refuse = (reason, details = {}) => {
        const entry = audit.append({ type: 'REQUEST_REFUSED', reason, details, principal: principal?.id ?? principal });
        return { allowed: false, reason, auditDigest: entry.digest, details: redactSecrets(details) };
      };
      try {
        canonicalized = canonicalRequest(request);
      } catch (error) {
        return refuse('INVALID_REQUEST', { message: error.message });
      }
      if (!Number.isFinite(canonicalized.timestamp) || Math.abs(now - canonicalized.timestamp) > maxSkewMs) {
        return refuse('TIMESTAMP_SKEW');
      }
      const signature = request.signature ?? request.headers?.['x-unrdf-signature'];
      if (!verifyRequestSignature(request, signature, secret)) return refuse('INVALID_SIGNATURE');
      const nonce = nonceStore.consume(canonicalized.nonce, now);
      if (!nonce.accepted) return refuse(nonce.reason);
      const rate = rateLimiter.consume(principal?.id ?? principal, now);
      if (!rate.allowed) return refuse('RATE_LIMITED', rate);
      const authorization = authorize(principal, action, resource, policy);
      if (!authorization.allowed) return refuse('FORBIDDEN', authorization);
      const findings = scan.flatMap(item => detectInjection(item.value, item.context));
      if (findings.some(item => item.severity === 'critical' || item.severity === 'high')) {
        return refuse('INJECTION_DETECTED', { findings });
      }
      const entry = audit.append({
        type: 'REQUEST_PERMITTED',
        principal: principal?.id ?? principal,
        action,
        resource,
        requestDigest: digest(canonicalized),
        findings,
      });
      return {
        allowed: true,
        reason: 'PERMITTED',
        remaining: rate.remaining,
        findings,
        auditDigest: entry.digest,
        requestDigest: digest(canonicalized),
      };
    },
  };
}
