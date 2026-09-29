/**
 * @file Safe `$variable` binding for SPARQL text (see .claude/rules/scoped/p1-sparql-injection.md)
 * @module kgc-4d-playground/lib/server/sparql-bind
 *
 * Values are converted to RDF terms (IRI, escaped literal, number, boolean) before being
 * substituted; they are never spliced in as raw SPARQL.
 */

const NAME_RE = /^[A-Za-z_]\w*$/;
const ABSOLUTE_IRI_RE = /^[A-Za-z][A-Za-z0-9+.-]*:[^\s<>"{}|^`\\]+$/;

/**
 * Escape a string for use inside a double-quoted SPARQL literal.
 * @param {string} value - Raw text
 * @returns {string} Escaped text
 */
export function escapeLiteral(value) {
  return value
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

/**
 * Convert a JS value to a SPARQL term.
 * - `{ iri: string }`  -> <iri> (validated)
 * - string             -> "escaped literal"
 * - finite number      -> numeric literal
 * - boolean            -> true/false
 * @param {unknown} value - Value to convert
 * @returns {string} SPARQL term
 * @throws {Error} For unsupported or unsafe values
 */
export function toSparqlTerm(value) {
  if (value && typeof value === 'object' && typeof value.iri === 'string') {
    if (!ABSOLUTE_IRI_RE.test(value.iri)) throw new Error(`Invalid IRI: ${JSON.stringify(value.iri)}`);
    return `<${value.iri}>`;
  }
  if (typeof value === 'string') return `"${escapeLiteral(value)}"`;
  if (typeof value === 'number' && Number.isFinite(value)) return String(value);
  if (typeof value === 'boolean') return String(value);
  throw new Error(`Unsupported SPARQL variable value: ${JSON.stringify(value)}`);
}

/**
 * Bind `$name` placeholders in a SPARQL string to safely serialised terms.
 * Placeholders are matched on whole names (`$s` does not touch `$street`) and unknown
 * placeholders are left untouched.
 *
 * @param {string} query - SPARQL text containing `$name` placeholders
 * @param {Record<string, unknown>} [variables] - Name -> value
 * @returns {string} Bound query
 * @throws {Error} On invalid names or values
 */
export function bindSparqlVariables(query, variables = {}) {
  if (typeof query !== 'string') throw new TypeError('query must be a string');
  const terms = new Map();
  for (const [name, value] of Object.entries(variables ?? {})) {
    if (!NAME_RE.test(name)) throw new Error(`Invalid variable name: ${JSON.stringify(name)}`);
    terms.set(name, toSparqlTerm(value));
  }
  // Function replacement: the substituted text is never re-interpreted ($&, $1, ...).
  return query.replace(/\$([A-Za-z_]\w*)/g, (match, name) => (terms.has(name) ? terms.get(name) : match));
}
