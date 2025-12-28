/**
 * @file Safe Expression Evaluator
 * @module @unrdf/yawl/api/safe-expression-evaluator
 *
 * @description
 * Secure expression evaluator that supports basic comparison and logical operators
 * without using dangerous eval() or new Function(). Prevents RCE vulnerabilities.
 *
 * Supported operators: ==, !=, >, <, >=, <=, &&, ||
 * Supported types: numbers, strings, booleans, null
 */

/**
 * Token types for lexical analysis
 */
const TOKEN_TYPES = {
  NUMBER: 'NUMBER',
  STRING: 'STRING',
  BOOLEAN: 'BOOLEAN',
  NULL: 'NULL',
  OPERATOR: 'OPERATOR',
  LOGICAL: 'LOGICAL',
  LPAREN: 'LPAREN',
  RPAREN: 'RPAREN',
  EOF: 'EOF',
};

/**
 * Tokenize an expression into lexical tokens
 * @param {string} expr - Expression to tokenize
 * @returns {Array<{type: string, value: any}>} Array of tokens
 */
function tokenize(expr) {
  const tokens = [];
  let i = 0;

  while (i < expr.length) {
    const char = expr[i];

    // Skip whitespace
    if (/\s/.test(char)) {
      i++;
      continue;
    }

    // Numbers
    if (/\d/.test(char) || (char === '-' && i + 1 < expr.length && /\d/.test(expr[i + 1]))) {
      let num = '';
      if (char === '-') {
        num = '-';
        i++;
      }
      while (i < expr.length && /[\d.]/.test(expr[i])) {
        num += expr[i];
        i++;
      }
      tokens.push({ type: TOKEN_TYPES.NUMBER, value: parseFloat(num) });
      continue;
    }

    // Strings
    if (char === '"' || char === "'") {
      const quote = char;
      let str = '';
      i++; // Skip opening quote
      while (i < expr.length && expr[i] !== quote) {
        if (expr[i] === '\\' && i + 1 < expr.length) {
          i++; // Skip escape char
          str += expr[i];
        } else {
          str += expr[i];
        }
        i++;
      }
      i++; // Skip closing quote
      tokens.push({ type: TOKEN_TYPES.STRING, value: str });
      continue;
    }

    // Booleans
    if (expr.substr(i, 4) === 'true') {
      tokens.push({ type: TOKEN_TYPES.BOOLEAN, value: true });
      i += 4;
      continue;
    }
    if (expr.substr(i, 5) === 'false') {
      tokens.push({ type: TOKEN_TYPES.BOOLEAN, value: false });
      i += 5;
      continue;
    }

    // Null
    if (expr.substr(i, 4) === 'null') {
      tokens.push({ type: TOKEN_TYPES.NULL, value: null });
      i += 4;
      continue;
    }

    // Comparison operators
    if (i + 1 < expr.length) {
      const twoChar = expr.substr(i, 2);
      if (['==', '!=', '>=', '<=', '&&', '||'].includes(twoChar)) {
        const type = (twoChar === '&&' || twoChar === '||') ? TOKEN_TYPES.LOGICAL : TOKEN_TYPES.OPERATOR;
        tokens.push({ type, value: twoChar });
        i += 2;
        continue;
      }
    }

    // Single char operators
    if (['>', '<'].includes(char)) {
      tokens.push({ type: TOKEN_TYPES.OPERATOR, value: char });
      i++;
      continue;
    }

    // Parentheses
    if (char === '(') {
      tokens.push({ type: TOKEN_TYPES.LPAREN, value: '(' });
      i++;
      continue;
    }
    if (char === ')') {
      tokens.push({ type: TOKEN_TYPES.RPAREN, value: ')' });
      i++;
      continue;
    }

    throw new Error(`Unexpected character: ${char} at position ${i}`);
  }

  tokens.push({ type: TOKEN_TYPES.EOF });
  return tokens;
}

/**
 * Parse and evaluate tokens using recursive descent parser
 */
class ExpressionParser {
  constructor(tokens) {
    this.tokens = tokens;
    this.pos = 0;
  }

  current() {
    return this.tokens[this.pos];
  }

  advance() {
    this.pos++;
  }

  /**
   * Parse logical OR expression (lowest precedence)
   */
  parseOr() {
    let left = this.parseAnd();

    while (this.current().type === TOKEN_TYPES.LOGICAL && this.current().value === '||') {
      this.advance();
      const right = this.parseAnd();
      left = left || right;
    }

    return left;
  }

  /**
   * Parse logical AND expression
   */
  parseAnd() {
    let left = this.parseComparison();

    while (this.current().type === TOKEN_TYPES.LOGICAL && this.current().value === '&&') {
      this.advance();
      const right = this.parseComparison();
      left = left && right;
    }

    return left;
  }

  /**
   * Parse comparison expression
   */
  parseComparison() {
    let left = this.parsePrimary();

    if (this.current().type === TOKEN_TYPES.OPERATOR) {
      const operator = this.current().value;
      this.advance();
      const right = this.parsePrimary();

      switch (operator) {
        case '==':
          return left == right; // eslint-disable-line eqeqeq
        case '!=':
          return left != right; // eslint-disable-line eqeqeq
        case '>':
          return left > right;
        case '<':
          return left < right;
        case '>=':
          return left >= right;
        case '<=':
          return left <= right;
        default:
          throw new Error(`Unknown operator: ${operator}`);
      }
    }

    return left;
  }

  /**
   * Parse primary expression (literals and parentheses)
   */
  parsePrimary() {
    const token = this.current();

    // Parenthesized expression
    if (token.type === TOKEN_TYPES.LPAREN) {
      this.advance();
      const result = this.parseOr();
      if (this.current().type !== TOKEN_TYPES.RPAREN) {
        throw new Error('Expected closing parenthesis');
      }
      this.advance();
      return result;
    }

    // Literals
    if ([TOKEN_TYPES.NUMBER, TOKEN_TYPES.STRING, TOKEN_TYPES.BOOLEAN, TOKEN_TYPES.NULL].includes(token.type)) {
      this.advance();
      return token.value;
    }

    throw new Error(`Unexpected token: ${token.type}`);
  }

  /**
   * Parse and evaluate the entire expression
   */
  evaluate() {
    const result = this.parseOr();
    if (this.current().type !== TOKEN_TYPES.EOF) {
      throw new Error('Unexpected tokens after expression');
    }
    return result;
  }
}

/**
 * Safely evaluate a simple expression without using eval() or new Function()
 *
 * @param {string} expression - Expression to evaluate (e.g., "5 > 3", "status == 'complete'")
 * @returns {boolean} Evaluation result
 *
 * @example
 * safeEvaluate('5 > 3') // true
 * safeEvaluate('"hello" == "world"') // false
 * safeEvaluate('true && false') // false
 * safeEvaluate('10 >= 5 || "a" == "b"') // true
 *
 * @throws {Error} If expression contains unsupported syntax
 */
export function safeEvaluate(expression) {
  if (!expression || typeof expression !== 'string') {
    throw new TypeError('Expression must be a non-empty string');
  }

  try {
    const tokens = tokenize(expression);
    const parser = new ExpressionParser(tokens);
    return parser.evaluate();
  } catch (error) {
    throw new Error(`Failed to evaluate expression "${expression}": ${error.message}`);
  }
}
