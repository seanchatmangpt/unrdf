/** Bidirectional TypeScript type-expression and Zod schema codec. */

const PRIMITIVE_ZOD = Object.freeze({
  string: 'z.string()',
  number: 'z.number()',
  boolean: 'z.boolean()',
  bigint: 'z.bigint()',
  symbol: 'z.symbol()',
  unknown: 'z.unknown()',
  any: 'z.any()',
  void: 'z.void()',
  null: 'z.null()',
  undefined: 'z.undefined()',
  never: 'z.never()',
  Date: 'z.date()',
});

function tokenize(source) {
  const tokens = [];
  let index = 0;
  while (index < source.length) {
    const rest = source.slice(index);
    const whitespace = rest.match(/^\s+/);
    if (whitespace) { index += whitespace[0].length; continue; }
    const comment = rest.match(/^(?:\/\/[^\n]*|\/\*[\s\S]*?\*\/)/);
    if (comment) { index += comment[0].length; continue; }
    const string = rest.match(/^(?:"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')/);
    if (string) { tokens.push({ type: 'string', value: string[0] }); index += string[0].length; continue; }
    const number = rest.match(/^-?(?:\d+\.\d+|\d+)(?:[eE][+-]?\d+)?/);
    if (number) { tokens.push({ type: 'number', value: number[0] }); index += number[0].length; continue; }
    const identifier = rest.match(/^[A-Za-z_$][\w$]*/);
    if (identifier) { tokens.push({ type: 'identifier', value: identifier[0] }); index += identifier[0].length; continue; }
    const operator = rest.match(/^(?:=>|\[\]|[{}[\]()<>,|&?:;])/);
    if (operator) { tokens.push({ type: 'punctuation', value: operator[0] }); index += operator[0].length; continue; }
    throw new SyntaxError(`Unexpected token near ${JSON.stringify(rest.slice(0, 20))}`);
  }
  tokens.push({ type: 'eof', value: '<eof>' });
  return tokens;
}

class Parser {
  constructor(source) { this.tokens = tokenize(source); this.index = 0; }
  peek(value) { const token = this.tokens[this.index]; return value == null ? token : token.value === value; }
  consume(value) {
    const token = this.tokens[this.index];
    if (value != null && token.value !== value) throw new SyntaxError(`Expected ${value}, found ${token.value}`);
    this.index += 1;
    return token;
  }
  maybe(value) { if (this.peek(value)) { this.index += 1; return true; } return false; }
  parse() { const type = this.parseUnion(); if (!this.peek('<eof>')) throw new SyntaxError(`Unexpected token ${this.peek().value}`); return type; }
  parseUnion() {
    const types = [this.parseIntersection()];
    while (this.maybe('|')) types.push(this.parseIntersection());
    return types.length === 1 ? types[0] : { kind: 'union', types };
  }
  parseIntersection() {
    const types = [this.parsePostfix()];
    while (this.maybe('&')) types.push(this.parsePostfix());
    return types.length === 1 ? types[0] : { kind: 'intersection', types };
  }
  parsePostfix() {
    let node = this.parsePrimary();
    while (this.maybe('[]')) node = { kind: 'array', element: node };
    return node;
  }
  parsePrimary() {
    if (this.maybe('(')) { const type = this.parseUnion(); this.consume(')'); return type; }
    if (this.maybe('[')) {
      const elements = [];
      let rest = null;
      while (!this.peek(']')) {
        if (this.peek().type === 'identifier' && this.peek().value === '...') throw new SyntaxError('Tuple rest tokenization unsupported');
        elements.push(this.parseUnion());
        if (!this.maybe(',')) break;
      }
      this.consume(']');
      return { kind: 'tuple', elements, rest };
    }
    if (this.maybe('{')) return this.parseObject();
    const token = this.consume();
    if (token.type === 'string') return { kind: 'literal', value: JSON.parse(token.value[0] === "'" ? `"${token.value.slice(1, -1).replace(/"/g, '\\"')}"` : token.value) };
    if (token.type === 'number') return { kind: 'literal', value: Number(token.value) };
    if (token.type !== 'identifier') throw new SyntaxError(`Expected type, found ${token.value}`);
    if (token.value === 'true' || token.value === 'false') return { kind: 'literal', value: token.value === 'true' };
    if (this.maybe('<')) {
      const args = [];
      while (!this.peek('>')) { args.push(this.parseUnion()); if (!this.maybe(',')) break; }
      this.consume('>');
      return { kind: 'generic', name: token.value, args };
    }
    return PRIMITIVE_ZOD[token.value] ? { kind: 'primitive', name: token.value } : { kind: 'reference', name: token.value };
  }
  parseObject() {
    const properties = [];
    let indexSignature = null;
    while (!this.peek('}')) {
      if (this.maybe('[')) {
        const keyName = this.consume().value;
        this.consume(':');
        const keyType = this.parseUnion();
        this.consume(']');
        this.consume(':');
        const valueType = this.parseUnion();
        indexSignature = { keyName, keyType, valueType };
      } else {
        const nameToken = this.consume();
        if (!['identifier', 'string'].includes(nameToken.type)) throw new SyntaxError(`Invalid object property ${nameToken.value}`);
        const name = nameToken.type === 'string' ? nameToken.value.slice(1, -1) : nameToken.value;
        const optional = this.maybe('?');
        this.consume(':');
        properties.push({ name, optional, type: this.parseUnion() });
      }
      this.maybe(';');
      this.maybe(',');
    }
    this.consume('}');
    return { kind: 'object', properties, indexSignature };
  }
}

export function parseTypeScriptType(source) { return new Parser(source).parse(); }

function identifier(value) { return /^[A-Za-z_$][\w$]*$/.test(value) ? value : JSON.stringify(value); }

export function typeAstToZod(ast, options = {}) {
  const reference = options.reference || (name => `z.lazy(() => ${name}Schema)`);
  switch (ast.kind) {
    case 'primitive': return PRIMITIVE_ZOD[ast.name];
    case 'literal': return `z.literal(${JSON.stringify(ast.value)})`;
    case 'reference': return reference(ast.name);
    case 'array': return `z.array(${typeAstToZod(ast.element, options)})`;
    case 'tuple': return `z.tuple([${ast.elements.map(type => typeAstToZod(type, options)).join(', ')}])`;
    case 'union': return `z.union([${ast.types.map(type => typeAstToZod(type, options)).join(', ')}])`;
    case 'intersection': return ast.types.map(type => typeAstToZod(type, options)).reduce((left, right) => `z.intersection(${left}, ${right})`);
    case 'object': {
      const fields = ast.properties.map(property => `${identifier(property.name)}: ${typeAstToZod(property.type, options)}${property.optional ? '.optional()' : ''}`);
      let result = `z.object({ ${fields.join(', ')} })`;
      if (ast.indexSignature) result += `.catchall(${typeAstToZod(ast.indexSignature.valueType, options)})`;
      return result;
    }
    case 'generic': {
      const args = ast.args.map(type => typeAstToZod(type, options));
      switch (ast.name) {
        case 'Array': case 'ReadonlyArray': return `z.array(${args[0] || 'z.unknown()'})`;
        case 'Promise': return `z.promise(${args[0] || 'z.unknown()'})`;
        case 'Set': return `z.set(${args[0] || 'z.unknown()'})`;
        case 'Map': return `z.map(${args[0] || 'z.unknown()'}, ${args[1] || 'z.unknown()'})`;
        case 'Record': return args.length > 1 ? `z.record(${args[0]}, ${args[1]})` : `z.record(z.string(), ${args[0] || 'z.unknown()'})`;
        case 'Partial': return `${args[0]}.partial()`;
        case 'Required': return `${args[0]}.required()`;
        case 'Readonly': return `${args[0]}.readonly()`;
        case 'Nullable': return `${args[0]}.nullable()`;
        default: return reference(ast.name);
      }
    }
    default: throw new Error(`Unsupported TypeScript AST kind ${ast.kind}`);
  }
}

export function typeScriptTypeToZod(source, options = {}) { return typeAstToZod(parseTypeScriptType(source), options); }

function definition(schema) { return schema?._def || schema?.def || {}; }
function rawType(schema) {
  const def = definition(schema);
  const value = def.typeName || def.type || schema?.type || schema?.constructor?.name || '';
  return String(value).replace(/^Zod/, '').toLowerCase();
}
function unwrap(schema) {
  const def = definition(schema);
  return def.innerType || def.type || def.schema || def.out || def.output || def.valueType || null;
}
function objectShape(schema) {
  const def = definition(schema);
  const shape = schema?.shape || def.shape;
  return typeof shape === 'function' ? shape() : shape || {};
}
function enumValues(schema) {
  const def = definition(schema);
  if (Array.isArray(def.values)) return def.values;
  if (def.entries) return Object.values(def.entries);
  if (schema?.options) return schema.options;
  return [];
}
function literalValue(schema) {
  const def = definition(schema);
  if ('value' in def) return def.value;
  if (Array.isArray(def.values) && def.values.length === 1) return def.values[0];
  if (schema?.value !== undefined) return schema.value;
  return undefined;
}
function optionalSchema(schema) { return ['optional', 'default', 'catch'].includes(rawType(schema)); }
function parenthesize(type) { return /[|&]/.test(type) ? `(${type})` : type; }

export function zodSchemaToTypeScript(schema, options = {}, context = { active: new Set() }) {
  if (!schema) return 'unknown';
  if (context.active.has(schema)) return options.recursiveType || 'unknown';
  const type = rawType(schema);
  const def = definition(schema);
  context.active.add(schema);
  try {
    switch (type) {
      case 'string': return 'string';
      case 'number': case 'nan': return 'number';
      case 'boolean': return 'boolean';
      case 'bigint': return 'bigint';
      case 'symbol': return 'symbol';
      case 'date': return 'Date';
      case 'null': return 'null';
      case 'undefined': return 'undefined';
      case 'void': return 'void';
      case 'never': return 'never';
      case 'any': return 'any';
      case 'unknown': return 'unknown';
      case 'literal': return JSON.stringify(literalValue(schema));
      case 'enum': case 'nativeenum': return enumValues(schema).map(value => JSON.stringify(value)).join(' | ') || 'never';
      case 'optional': return `${zodSchemaToTypeScript(unwrap(schema), options, context)} | undefined`;
      case 'nullable': return `${zodSchemaToTypeScript(unwrap(schema), options, context)} | null`;
      case 'default': case 'catch': case 'readonly': case 'brand': case 'branded':
        return zodSchemaToTypeScript(unwrap(schema), options, context);
      case 'array': {
        const element = def.element || def.type || schema.element;
        return `${parenthesize(zodSchemaToTypeScript(element, options, context))}[]`;
      }
      case 'tuple': {
        const items = def.items || schema.items || [];
        const rest = def.rest ? `, ...${zodSchemaToTypeScript(def.rest, options, context)}[]` : '';
        return `[${items.map(item => zodSchemaToTypeScript(item, options, context)).join(', ')}${rest}]`;
      }
      case 'union': case 'discriminatedunion': {
        const optionsList = def.options instanceof Map ? [...def.options.values()] : def.options || schema.options || [];
        return optionsList.map(item => zodSchemaToTypeScript(item, options, context)).join(' | ') || 'never';
      }
      case 'intersection': return `${zodSchemaToTypeScript(def.left, options, context)} & ${zodSchemaToTypeScript(def.right, options, context)}`;
      case 'object': {
        const fields = Object.entries(objectShape(schema)).map(([name, field]) => {
          const optional = optionalSchema(field);
          let fieldType = zodSchemaToTypeScript(field, options, context);
          if (optional) fieldType = fieldType.replace(/\s*\|\s*undefined$/, '');
          return `${identifier(name)}${optional ? '?' : ''}: ${fieldType};`;
        });
        const catchall = def.catchall && rawType(def.catchall) !== 'never' ? ` [key: string]: ${zodSchemaToTypeScript(def.catchall, options, context)};` : '';
        return `{ ${fields.join(' ')}${catchall} }`;
      }
      case 'record': {
        const key = def.keyType || def.key || { _def: { type: 'string' } };
        const value = def.valueType || def.value || def.type;
        return `Record<${zodSchemaToTypeScript(key, options, context)}, ${zodSchemaToTypeScript(value, options, context)}>`;
      }
      case 'map': return `Map<${zodSchemaToTypeScript(def.keyType || def.key, options, context)}, ${zodSchemaToTypeScript(def.valueType || def.value, options, context)}>`;
      case 'set': return `Set<${zodSchemaToTypeScript(def.valueType || def.value || def.type, options, context)}>`;
      case 'promise': return `Promise<${zodSchemaToTypeScript(unwrap(schema), options, context)}>`;
      case 'lazy': {
        const getter = def.getter || schema.getter;
        if (options.lazyName) return options.lazyName(schema);
        return typeof getter === 'function' ? zodSchemaToTypeScript(getter(), options, context) : 'unknown';
      }
      case 'effects': case 'transform': case 'pipeline': case 'pipe':
        return zodSchemaToTypeScript(def.schema || def.out || def.output || def.in || def.input, options, context);
      case 'function': {
        const args = def.args?._def?.items || def.input?._def?.items || [];
        const output = def.returns || def.output;
        return `(${args.map((arg, index) => `arg${index}: ${zodSchemaToTypeScript(arg, options, context)}`).join(', ')}) => ${zodSchemaToTypeScript(output, options, context)}`;
      }
      case 'template_literal': case 'templateliteral': return 'string';
      case 'custom': return options.customType || 'unknown';
      default:
        if (schema?._output !== undefined) return 'unknown';
        return options.unknownType || 'unknown';
    }
  } finally {
    context.active.delete(schema);
  }
}

export function generateTypeScriptDeclaration(schema, options = {}) {
  const name = options.name || 'Generated';
  const exported = options.export === false ? '' : 'export ';
  const type = zodSchemaToTypeScript(schema, options);
  if (rawType(schema) === 'object' && options.interface !== false) {
    const body = type.startsWith('{ ') && type.endsWith(' }') ? type.slice(2, -2) : type;
    return `${exported}interface ${name} {${body}}`;
  }
  return `${exported}type ${name} = ${type};`;
}

export { PRIMITIVE_ZOD };
