/**
 * @file Safe condition-expression evaluator (no eval / new Function)
 * @module @unrdf/chatman-equation/condition-evaluator
 * @description
 * Grammar (lowest to highest precedence):
 *   or      := and ( ('OR' | '||') and )*
 *   and     := not ( ('AND' | '&&') not )*
 *   not     := ('NOT' | '!') not | compare
 *   compare := primary ( ('==' | '===' | '!=' | '!==' | '<' | '<=' | '>' | '>=') primary )?
 *   primary := number | string | true | false | null | identifier | '(' or ')'
 *
 * Identifiers are looked up as own properties of the supplied context only.
 */

const TOKEN_RE =
  /\s*(?:(\d+\.?\d*(?:[eE][+-]?\d+)?|\.\d+)|("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*')|([A-Za-z_][A-Za-z0-9_]*)|(===|!==|==|!=|<=|>=|&&|\|\||[<>!()]))/y;

/**
 * @param {string} src - Expression source
 * @returns {Array<{type: string, value: any}>} Tokens
 */
function tokenize(src) {
  const tokens = [];
  let pos = 0;
  while (pos < src.length) {
    TOKEN_RE.lastIndex = pos;
    const m = TOKEN_RE.exec(src);
    if (!m) {
      if (src.slice(pos).trim() === '') break;
      throw new Error(`Unexpected character at position ${pos}: ${JSON.stringify(src[pos])}`);
    }
    pos = TOKEN_RE.lastIndex;
    if (m[1] !== undefined) tokens.push({ type: 'num', value: Number(m[1]) });
    else if (m[2] !== undefined) tokens.push({ type: 'str', value: unquote(m[2]) });
    else if (m[3] !== undefined) tokens.push({ type: 'id', value: m[3] });
    else tokens.push({ type: 'op', value: m[4] });
  }
  return tokens;
}

/**
 * @param {string} raw - Quoted string literal
 * @returns {string} Unescaped string
 */
function unquote(raw) {
  return raw.slice(1, -1).replace(/\\(.)/g, (_, c) => {
    if (c === 'n') return '\n';
    if (c === 't') return '\t';
    if (c === 'r') return '\r';
    return c;
  });
}

/**
 * Evaluate a boolean/comparison expression against a context without executing code.
 *
 * @param {string} source - Expression, e.g. `user_count > 100 AND domain == "market"`
 * @param {Record<string, unknown>} context - Variable bindings
 * @returns {unknown} Expression value
 * @throws {Error} On syntax errors or unknown identifiers
 */
export function evaluateExpression(source, context) {
  if (typeof source !== 'string') throw new Error('Condition must be a string');
  const tokens = tokenize(source);
  let i = 0;

  const peek = () => tokens[i];
  const isWord = (t, ...words) => t && t.type === 'id' && words.includes(t.value);
  const isOp = (t, ...ops) => t && t.type === 'op' && ops.includes(t.value);

  function parseOr() {
    let left = parseAnd();
    while (isOp(peek(), '||') || isWord(peek(), 'OR')) {
      i++;
      const right = parseAnd();
      left = left || right;
    }
    return left;
  }

  function parseAnd() {
    let left = parseNot();
    while (isOp(peek(), '&&') || isWord(peek(), 'AND')) {
      i++;
      const right = parseNot();
      left = left && right;
    }
    return left;
  }

  function parseNot() {
    if (isOp(peek(), '!') || isWord(peek(), 'NOT')) {
      i++;
      return !parseNot();
    }
    return parseCompare();
  }

  function parseCompare() {
    const left = parsePrimary();
    const t = peek();
    if (isOp(t, '==', '===', '!=', '!==', '<', '<=', '>', '>=')) {
      i++;
      const right = parsePrimary();
      switch (t.value) {
        case '==':
        case '===':
          return left === right;
        case '!=':
        case '!==':
          return left !== right;
        case '<':
          return left < right;
        case '<=':
          return left <= right;
        case '>':
          return left > right;
        default:
          return left >= right;
      }
    }
    return left;
  }

  function parsePrimary() {
    const t = tokens[i++];
    if (!t) throw new Error('Unexpected end of expression');
    if (t.type === 'num' || t.type === 'str') return t.value;
    if (t.type === 'id') {
      if (t.value === 'true') return true;
      if (t.value === 'false') return false;
      if (t.value === 'null') return null;
      if (Object.prototype.hasOwnProperty.call(context, t.value)) return context[t.value];
      throw new Error(`Unknown identifier: ${t.value}`);
    }
    if (isOp(t, '(')) {
      const v = parseOr();
      if (!isOp(tokens[i++], ')')) throw new Error('Expected )');
      return v;
    }
    throw new Error(`Unexpected token: ${t.value}`);
  }

  const result = parseOr();
  if (i < tokens.length) throw new Error(`Unexpected token: ${tokens[i].value}`);
  return result;
}
