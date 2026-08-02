/**
 * Dependency-free SHACL Core compiler and validator for RDF/JS stores.
 *
 * The engine intentionally accepts the narrow store protocol already exposed by
 * UNRDF/Oxigraph: getQuads(subject, predicate, object, graph) or match(...).
 * It implements the high-value SHACL Core surface and RDF property paths while
 * keeping validation deterministic and suitable for incremental streaming use.
 */

export const RDF = Object.freeze({
  type: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
  first: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
  rest: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
  nil: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',
  langString: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',
});

export const XSD = Object.freeze({
  string: 'http://www.w3.org/2001/XMLSchema#string',
  boolean: 'http://www.w3.org/2001/XMLSchema#boolean',
  integer: 'http://www.w3.org/2001/XMLSchema#integer',
  decimal: 'http://www.w3.org/2001/XMLSchema#decimal',
  double: 'http://www.w3.org/2001/XMLSchema#double',
  float: 'http://www.w3.org/2001/XMLSchema#float',
  date: 'http://www.w3.org/2001/XMLSchema#date',
  dateTime: 'http://www.w3.org/2001/XMLSchema#dateTime',
  anyURI: 'http://www.w3.org/2001/XMLSchema#anyURI',
});

export const SH = Object.freeze({
  NodeShape: 'http://www.w3.org/ns/shacl#NodeShape',
  PropertyShape: 'http://www.w3.org/ns/shacl#PropertyShape',
  Violation: 'http://www.w3.org/ns/shacl#Violation',
  Warning: 'http://www.w3.org/ns/shacl#Warning',
  Info: 'http://www.w3.org/ns/shacl#Info',
  targetNode: 'http://www.w3.org/ns/shacl#targetNode',
  targetClass: 'http://www.w3.org/ns/shacl#targetClass',
  targetSubjectsOf: 'http://www.w3.org/ns/shacl#targetSubjectsOf',
  targetObjectsOf: 'http://www.w3.org/ns/shacl#targetObjectsOf',
  property: 'http://www.w3.org/ns/shacl#property',
  path: 'http://www.w3.org/ns/shacl#path',
  inversePath: 'http://www.w3.org/ns/shacl#inversePath',
  alternativePath: 'http://www.w3.org/ns/shacl#alternativePath',
  zeroOrMorePath: 'http://www.w3.org/ns/shacl#zeroOrMorePath',
  oneOrMorePath: 'http://www.w3.org/ns/shacl#oneOrMorePath',
  zeroOrOnePath: 'http://www.w3.org/ns/shacl#zeroOrOnePath',
  minCount: 'http://www.w3.org/ns/shacl#minCount',
  maxCount: 'http://www.w3.org/ns/shacl#maxCount',
  datatype: 'http://www.w3.org/ns/shacl#datatype',
  class: 'http://www.w3.org/ns/shacl#class',
  nodeKind: 'http://www.w3.org/ns/shacl#nodeKind',
  IRI: 'http://www.w3.org/ns/shacl#IRI',
  BlankNode: 'http://www.w3.org/ns/shacl#BlankNode',
  Literal: 'http://www.w3.org/ns/shacl#Literal',
  BlankNodeOrIRI: 'http://www.w3.org/ns/shacl#BlankNodeOrIRI',
  BlankNodeOrLiteral: 'http://www.w3.org/ns/shacl#BlankNodeOrLiteral',
  IRIOrLiteral: 'http://www.w3.org/ns/shacl#IRIOrLiteral',
  minLength: 'http://www.w3.org/ns/shacl#minLength',
  maxLength: 'http://www.w3.org/ns/shacl#maxLength',
  pattern: 'http://www.w3.org/ns/shacl#pattern',
  flags: 'http://www.w3.org/ns/shacl#flags',
  languageIn: 'http://www.w3.org/ns/shacl#languageIn',
  uniqueLang: 'http://www.w3.org/ns/shacl#uniqueLang',
  in: 'http://www.w3.org/ns/shacl#in',
  hasValue: 'http://www.w3.org/ns/shacl#hasValue',
  equals: 'http://www.w3.org/ns/shacl#equals',
  disjoint: 'http://www.w3.org/ns/shacl#disjoint',
  lessThan: 'http://www.w3.org/ns/shacl#lessThan',
  lessThanOrEquals: 'http://www.w3.org/ns/shacl#lessThanOrEquals',
  minExclusive: 'http://www.w3.org/ns/shacl#minExclusive',
  minInclusive: 'http://www.w3.org/ns/shacl#minInclusive',
  maxExclusive: 'http://www.w3.org/ns/shacl#maxExclusive',
  maxInclusive: 'http://www.w3.org/ns/shacl#maxInclusive',
  node: 'http://www.w3.org/ns/shacl#node',
  not: 'http://www.w3.org/ns/shacl#not',
  and: 'http://www.w3.org/ns/shacl#and',
  or: 'http://www.w3.org/ns/shacl#or',
  xone: 'http://www.w3.org/ns/shacl#xone',
  closed: 'http://www.w3.org/ns/shacl#closed',
  ignoredProperties: 'http://www.w3.org/ns/shacl#ignoredProperties',
  qualifiedValueShape: 'http://www.w3.org/ns/shacl#qualifiedValueShape',
  qualifiedMinCount: 'http://www.w3.org/ns/shacl#qualifiedMinCount',
  qualifiedMaxCount: 'http://www.w3.org/ns/shacl#qualifiedMaxCount',
  severity: 'http://www.w3.org/ns/shacl#severity',
  message: 'http://www.w3.org/ns/shacl#message',
  name: 'http://www.w3.org/ns/shacl#name',
  description: 'http://www.w3.org/ns/shacl#description',
  deactivated: 'http://www.w3.org/ns/shacl#deactivated',
});

const CONSTRAINT_COMPONENTS = Object.freeze({
  minCount: 'http://www.w3.org/ns/shacl#MinCountConstraintComponent',
  maxCount: 'http://www.w3.org/ns/shacl#MaxCountConstraintComponent',
  datatype: 'http://www.w3.org/ns/shacl#DatatypeConstraintComponent',
  class: 'http://www.w3.org/ns/shacl#ClassConstraintComponent',
  nodeKind: 'http://www.w3.org/ns/shacl#NodeKindConstraintComponent',
  minLength: 'http://www.w3.org/ns/shacl#MinLengthConstraintComponent',
  maxLength: 'http://www.w3.org/ns/shacl#MaxLengthConstraintComponent',
  pattern: 'http://www.w3.org/ns/shacl#PatternConstraintComponent',
  languageIn: 'http://www.w3.org/ns/shacl#LanguageInConstraintComponent',
  uniqueLang: 'http://www.w3.org/ns/shacl#UniqueLangConstraintComponent',
  in: 'http://www.w3.org/ns/shacl#InConstraintComponent',
  hasValue: 'http://www.w3.org/ns/shacl#HasValueConstraintComponent',
  equals: 'http://www.w3.org/ns/shacl#EqualsConstraintComponent',
  disjoint: 'http://www.w3.org/ns/shacl#DisjointConstraintComponent',
  lessThan: 'http://www.w3.org/ns/shacl#LessThanConstraintComponent',
  lessThanOrEquals: 'http://www.w3.org/ns/shacl#LessThanOrEqualsConstraintComponent',
  minExclusive: 'http://www.w3.org/ns/shacl#MinExclusiveConstraintComponent',
  minInclusive: 'http://www.w3.org/ns/shacl#MinInclusiveConstraintComponent',
  maxExclusive: 'http://www.w3.org/ns/shacl#MaxExclusiveConstraintComponent',
  maxInclusive: 'http://www.w3.org/ns/shacl#MaxInclusiveConstraintComponent',
  node: 'http://www.w3.org/ns/shacl#NodeConstraintComponent',
  not: 'http://www.w3.org/ns/shacl#NotConstraintComponent',
  and: 'http://www.w3.org/ns/shacl#AndConstraintComponent',
  or: 'http://www.w3.org/ns/shacl#OrConstraintComponent',
  xone: 'http://www.w3.org/ns/shacl#XoneConstraintComponent',
  closed: 'http://www.w3.org/ns/shacl#ClosedConstraintComponent',
  qualifiedMinCount: 'http://www.w3.org/ns/shacl#QualifiedMinCountConstraintComponent',
  qualifiedMaxCount: 'http://www.w3.org/ns/shacl#QualifiedMaxCountConstraintComponent',
});

function namedNode(value) {
  return { termType: 'NamedNode', value };
}

function asTerm(value) {
  if (value == null || typeof value === 'object') return value;
  return namedNode(String(value));
}

export function termKey(term) {
  if (!term) return '';
  const datatype = term.datatype?.value || '';
  const language = term.language || '';
  return `${term.termType || ''}|${term.value ?? ''}|${language}|${datatype}`;
}

export function termEquals(left, right) {
  return termKey(left) === termKey(right);
}

function uniqueTerms(terms) {
  const seen = new Set();
  const result = [];
  for (const term of terms || []) {
    const key = termKey(term);
    if (!seen.has(key)) {
      seen.add(key);
      result.push(term);
    }
  }
  return result;
}

function quads(store, subject = null, predicate = null, object = null, graph = null) {
  if (!store) return [];
  const s = asTerm(subject);
  const p = asTerm(predicate);
  const o = asTerm(object);
  const g = asTerm(graph);
  if (typeof store.getQuads === 'function') return Array.from(store.getQuads(s, p, o, g) || []);
  if (typeof store.match === 'function') return Array.from(store.match(s, p, o, g) || []);
  throw new TypeError('SHACL store must implement getQuads() or match()');
}

function objects(store, subject, predicate) {
  return quads(store, subject, predicate, null, null).map(quad => quad.object);
}

function firstObject(store, subject, predicate) {
  return objects(store, subject, predicate)[0] || null;
}

function subjects(store, predicate, object) {
  return quads(store, null, predicate, object, null).map(quad => quad.subject);
}

function literalBoolean(term, fallback = false) {
  if (!term) return fallback;
  return term.value === true || term.value === 'true' || term.value === '1';
}

function literalNumber(term) {
  if (!term) return null;
  const number = Number(term.value);
  return Number.isFinite(number) ? number : null;
}

function literalText(term) {
  return term?.value == null ? null : String(term.value);
}

function isRdfListHead(store, term) {
  return Boolean(term && firstObject(store, term, RDF.first));
}

export function readRdfList(store, head, { maxItems = 10_000 } = {}) {
  if (!head || head.value === RDF.nil) return [];
  const result = [];
  const visited = new Set();
  let current = head;
  while (current && current.value !== RDF.nil) {
    const key = termKey(current);
    if (visited.has(key)) throw new Error(`Cyclic RDF list at ${current.value}`);
    visited.add(key);
    if (result.length >= maxItems) throw new Error(`RDF list exceeds ${maxItems} items`);
    const first = firstObject(store, current, RDF.first);
    const rest = firstObject(store, current, RDF.rest);
    if (!first || !rest) throw new Error(`Malformed RDF list at ${current.value}`);
    result.push(first);
    current = rest;
  }
  return result;
}

function compilePath(store, pathTerm, stack = new Set()) {
  if (!pathTerm) return null;
  if (pathTerm.termType === 'NamedNode') return { kind: 'predicate', predicate: pathTerm };
  const key = termKey(pathTerm);
  if (stack.has(key)) throw new Error(`Cyclic SHACL path at ${pathTerm.value}`);
  stack.add(key);
  try {
    const inverse = firstObject(store, pathTerm, SH.inversePath);
    if (inverse) return { kind: 'inverse', path: compilePath(store, inverse, stack) };
    const alternative = firstObject(store, pathTerm, SH.alternativePath);
    if (alternative) {
      return { kind: 'alternative', paths: readRdfList(store, alternative).map(item => compilePath(store, item, stack)) };
    }
    const zeroOrMore = firstObject(store, pathTerm, SH.zeroOrMorePath);
    if (zeroOrMore) return { kind: 'zeroOrMore', path: compilePath(store, zeroOrMore, stack) };
    const oneOrMore = firstObject(store, pathTerm, SH.oneOrMorePath);
    if (oneOrMore) return { kind: 'oneOrMore', path: compilePath(store, oneOrMore, stack) };
    const zeroOrOne = firstObject(store, pathTerm, SH.zeroOrOnePath);
    if (zeroOrOne) return { kind: 'zeroOrOne', path: compilePath(store, zeroOrOne, stack) };
    if (isRdfListHead(store, pathTerm)) {
      return { kind: 'sequence', paths: readRdfList(store, pathTerm).map(item => compilePath(store, item, stack)) };
    }
    throw new Error(`Unsupported SHACL path expression ${pathTerm.value}`);
  } finally {
    stack.delete(key);
  }
}

export function evaluatePath(store, startNodes, path, { maxDepth = 64, maxNodes = 100_000 } = {}) {
  const starts = uniqueTerms(Array.isArray(startNodes) ? startNodes : [startNodes]);
  if (!path) return starts;

  const step = (nodes, expression, depth) => {
    if (depth > maxDepth) throw new Error(`SHACL path exceeds max depth ${maxDepth}`);
    let values = [];
    switch (expression.kind) {
      case 'predicate':
        for (const node of nodes) values.push(...quads(store, node, expression.predicate, null, null).map(q => q.object));
        break;
      case 'inverse':
        if (expression.path.kind === 'predicate') {
          for (const node of nodes) values.push(...quads(store, null, expression.path.predicate, node, null).map(q => q.subject));
        } else {
          const candidates = uniqueTerms(quads(store).flatMap(q => [q.subject, q.object]));
          for (const candidate of candidates) {
            if (step([candidate], expression.path, depth + 1).some(value => nodes.some(node => termEquals(node, value)))) values.push(candidate);
          }
        }
        break;
      case 'sequence':
        values = nodes;
        for (const nested of expression.paths) values = step(values, nested, depth + 1);
        break;
      case 'alternative':
        for (const nested of expression.paths) values.push(...step(nodes, nested, depth + 1));
        break;
      case 'zeroOrOne':
        values = [...nodes, ...step(nodes, expression.path, depth + 1)];
        break;
      case 'zeroOrMore':
      case 'oneOrMore': {
        const visited = new Map();
        let frontier = nodes;
        if (expression.kind === 'zeroOrMore') for (const node of nodes) visited.set(termKey(node), node);
        while (frontier.length) {
          const next = step(frontier, expression.path, depth + 1);
          frontier = [];
          for (const node of next) {
            const key = termKey(node);
            if (!visited.has(key)) {
              visited.set(key, node);
              frontier.push(node);
              if (visited.size > maxNodes) throw new Error(`SHACL path exceeds max nodes ${maxNodes}`);
            }
          }
        }
        values = Array.from(visited.values());
        break;
      }
      default:
        throw new Error(`Unsupported compiled SHACL path kind ${expression.kind}`);
    }
    const result = uniqueTerms(values);
    if (result.length > maxNodes) throw new Error(`SHACL path exceeds max nodes ${maxNodes}`);
    return result;
  };

  return step(starts, path, 0);
}

function values(store, subject, predicate) {
  return objects(store, subject, predicate);
}

function compileConstraintValues(store, shapeId) {
  const one = predicate => firstObject(store, shapeId, predicate);
  const many = predicate => values(store, shapeId, predicate);
  const list = predicate => {
    const head = one(predicate);
    return head ? readRdfList(store, head) : [];
  };
  return {
    minCount: literalNumber(one(SH.minCount)),
    maxCount: literalNumber(one(SH.maxCount)),
    datatype: one(SH.datatype),
    class: one(SH.class),
    nodeKind: one(SH.nodeKind),
    minLength: literalNumber(one(SH.minLength)),
    maxLength: literalNumber(one(SH.maxLength)),
    pattern: literalText(one(SH.pattern)),
    flags: literalText(one(SH.flags)) || '',
    languageIn: list(SH.languageIn).map(term => String(term.value).toLowerCase()),
    uniqueLang: literalBoolean(one(SH.uniqueLang)),
    in: list(SH.in),
    hasValue: many(SH.hasValue),
    equals: one(SH.equals),
    disjoint: one(SH.disjoint),
    lessThan: one(SH.lessThan),
    lessThanOrEquals: one(SH.lessThanOrEquals),
    minExclusive: one(SH.minExclusive),
    minInclusive: one(SH.minInclusive),
    maxExclusive: one(SH.maxExclusive),
    maxInclusive: one(SH.maxInclusive),
    node: many(SH.node),
    not: many(SH.not),
    and: list(SH.and),
    or: list(SH.or),
    xone: list(SH.xone),
    closed: literalBoolean(one(SH.closed)),
    ignoredProperties: list(SH.ignoredProperties),
    qualifiedValueShape: one(SH.qualifiedValueShape),
    qualifiedMinCount: literalNumber(one(SH.qualifiedMinCount)),
    qualifiedMaxCount: literalNumber(one(SH.qualifiedMaxCount)),
  };
}

function shapeMetadata(store, shapeId) {
  return {
    severity: firstObject(store, shapeId, SH.severity)?.value || SH.Violation,
    messages: values(store, shapeId, SH.message).map(term => term.value),
    name: firstObject(store, shapeId, SH.name)?.value || null,
    description: firstObject(store, shapeId, SH.description)?.value || null,
    deactivated: literalBoolean(firstObject(store, shapeId, SH.deactivated)),
  };
}

export function compileShacl(shapesStore) {
  const nodeIds = uniqueTerms(subjects(shapesStore, RDF.type, SH.NodeShape));
  const propertyIds = uniqueTerms([
    ...subjects(shapesStore, RDF.type, SH.PropertyShape),
    ...nodeIds.flatMap(node => values(shapesStore, node, SH.property)),
  ]);
  const shapesById = new Map();

  const compileBase = (id, type) => ({
    id,
    key: termKey(id),
    type,
    ...shapeMetadata(shapesStore, id),
    constraints: compileConstraintValues(shapesStore, id),
  });

  for (const id of propertyIds) {
    const pathTerm = firstObject(shapesStore, id, SH.path);
    shapesById.set(termKey(id), {
      ...compileBase(id, 'PropertyShape'),
      pathTerm,
      path: pathTerm ? compilePath(shapesStore, pathTerm) : null,
    });
  }

  for (const id of nodeIds) {
    const targets = {
      nodes: values(shapesStore, id, SH.targetNode),
      classes: values(shapesStore, id, SH.targetClass),
      subjectsOf: values(shapesStore, id, SH.targetSubjectsOf),
      objectsOf: values(shapesStore, id, SH.targetObjectsOf),
    };
    const propertyShapes = values(shapesStore, id, SH.property).map(term => termKey(term));
    shapesById.set(termKey(id), {
      ...compileBase(id, 'NodeShape'),
      targets,
      propertyShapes,
    });
  }

  // Compile referenced anonymous node shapes even when rdf:type is omitted.
  const referenced = [];
  for (const shape of shapesById.values()) {
    const c = shape.constraints;
    referenced.push(...c.node, ...c.not, ...c.and, ...c.or, ...c.xone);
    if (c.qualifiedValueShape) referenced.push(c.qualifiedValueShape);
  }
  for (const id of referenced) {
    if (!shapesById.has(termKey(id))) {
      shapesById.set(termKey(id), {
        ...compileBase(id, 'NodeShape'),
        targets: null,
        propertyShapes: values(shapesStore, id, SH.property).map(term => termKey(term)),
      });
    }
  }

  const dependencyPredicates = new Set([RDF.type]);
  for (const shape of shapesById.values()) {
    if (shape.path?.kind === 'predicate') dependencyPredicates.add(shape.path.predicate.value);
    if (shape.targets) {
      for (const predicate of shape.targets.subjectsOf) dependencyPredicates.add(predicate.value);
      for (const predicate of shape.targets.objectsOf) dependencyPredicates.add(predicate.value);
    }
    for (const predicate of [shape.constraints.equals, shape.constraints.disjoint, shape.constraints.lessThan, shape.constraints.lessThanOrEquals]) {
      if (predicate) dependencyPredicates.add(predicate.value);
    }
  }

  return Object.freeze({
    nodeShapes: Array.from(shapesById.values()).filter(shape => shape.type === 'NodeShape' && shape.targets),
    propertyShapes: Array.from(shapesById.values()).filter(shape => shape.type === 'PropertyShape'),
    shapesById,
    dependencyPredicates,
  });
}

function targetNodes(dataStore, shape) {
  const targets = [...shape.targets.nodes];
  for (const targetClass of shape.targets.classes) targets.push(...subjects(dataStore, RDF.type, targetClass));
  for (const predicate of shape.targets.subjectsOf) targets.push(...quads(dataStore, null, predicate, null, null).map(q => q.subject));
  for (const predicate of shape.targets.objectsOf) targets.push(...quads(dataStore, null, predicate, null, null).map(q => q.object));
  return uniqueTerms(targets).sort((a, b) => termKey(a).localeCompare(termKey(b)));
}

function lexical(term) {
  return term?.value == null ? '' : String(term.value);
}

function comparable(term) {
  if (!term) return { type: 'none', value: null };
  const datatype = term.datatype?.value;
  if ([XSD.integer, XSD.decimal, XSD.double, XSD.float].includes(datatype)) {
    const number = Number(term.value);
    return { type: 'number', value: Number.isNaN(number) ? null : number };
  }
  if ([XSD.date, XSD.dateTime].includes(datatype)) {
    const time = Date.parse(term.value);
    return { type: 'date', value: Number.isNaN(time) ? null : time };
  }
  return { type: 'string', value: lexical(term) };
}

function compareTerms(left, right) {
  const a = comparable(left);
  const b = comparable(right);
  if (a.value == null || b.value == null || a.type !== b.type) return null;
  return a.value < b.value ? -1 : a.value > b.value ? 1 : 0;
}

function hasClass(dataStore, node, classTerm) {
  return quads(dataStore, node, RDF.type, classTerm, null).length > 0;
}

function conformsNodeKind(term, nodeKind) {
  switch (nodeKind?.value) {
    case SH.IRI: return term.termType === 'NamedNode';
    case SH.BlankNode: return term.termType === 'BlankNode';
    case SH.Literal: return term.termType === 'Literal';
    case SH.BlankNodeOrIRI: return term.termType === 'BlankNode' || term.termType === 'NamedNode';
    case SH.BlankNodeOrLiteral: return term.termType === 'BlankNode' || term.termType === 'Literal';
    case SH.IRIOrLiteral: return term.termType === 'NamedNode' || term.termType === 'Literal';
    default: return true;
  }
}

function conformsDatatype(term, datatype) {
  if (term.termType !== 'Literal') return false;
  const actual = term.datatype?.value || (term.language ? RDF.langString : XSD.string);
  return actual === datatype.value;
}

function defaultMessage(component, details = '') {
  const name = component.split('#').pop()?.replace('ConstraintComponent', '') || 'SHACL';
  return `${name} constraint violated${details ? `: ${details}` : ''}`;
}

function resultFor(shape, focusNode, component, { path = null, value = null, message = null, details = null } = {}) {
  return {
    severity: shape.severity || SH.Violation,
    sourceShape: shape.id,
    sourceConstraintComponent: component,
    focusNode,
    resultPath: path,
    value,
    message: message || shape.messages[0] || defaultMessage(component, details),
  };
}

function setEquals(left, right) {
  const a = new Set(left.map(termKey));
  const b = new Set(right.map(termKey));
  return a.size === b.size && [...a].every(key => b.has(key));
}

function setIntersects(left, right) {
  const b = new Set(right.map(termKey));
  return left.some(term => b.has(termKey(term)));
}

function evaluateReferencedShape(dataStore, compiled, shapeTerm, node, context) {
  const shape = compiled.shapesById.get(termKey(shapeTerm));
  if (!shape) return { conforms: false, results: [resultFor({ id: shapeTerm, severity: SH.Violation, messages: [] }, node, CONSTRAINT_COMPONENTS.node, { details: 'referenced shape is missing' })] };
  return validateShape(dataStore, compiled, shape, node, context);
}

function validateValueConstraints(dataStore, compiled, shape, focusNode, valueNodes, path, context) {
  const c = shape.constraints;
  const results = [];
  const pushEach = (component, predicate, details) => {
    for (const value of valueNodes) if (!predicate(value)) results.push(resultFor(shape, focusNode, component, { path, value, details }));
  };

  if (c.datatype) pushEach(CONSTRAINT_COMPONENTS.datatype, value => conformsDatatype(value, c.datatype), `expected datatype ${c.datatype.value}`);
  if (c.class) pushEach(CONSTRAINT_COMPONENTS.class, value => hasClass(dataStore, value, c.class), `expected class ${c.class.value}`);
  if (c.nodeKind) pushEach(CONSTRAINT_COMPONENTS.nodeKind, value => conformsNodeKind(value, c.nodeKind), `expected node kind ${c.nodeKind.value}`);
  if (c.minLength != null) pushEach(CONSTRAINT_COMPONENTS.minLength, value => [...lexical(value)].length >= c.minLength, `minimum length ${c.minLength}`);
  if (c.maxLength != null) pushEach(CONSTRAINT_COMPONENTS.maxLength, value => [...lexical(value)].length <= c.maxLength, `maximum length ${c.maxLength}`);
  if (c.pattern != null) {
    let regex;
    try { regex = new RegExp(c.pattern, c.flags); } catch (error) { throw new Error(`Invalid SHACL regex on ${shape.id.value}: ${error.message}`); }
    pushEach(CONSTRAINT_COMPONENTS.pattern, value => regex.test(lexical(value)), `pattern ${c.pattern}`);
  }
  if (c.languageIn.length) pushEach(CONSTRAINT_COMPONENTS.languageIn, value => value.termType === 'Literal' && c.languageIn.includes(String(value.language || '').toLowerCase()), `language in ${c.languageIn.join(', ')}`);
  if (c.uniqueLang) {
    const seen = new Set();
    for (const value of valueNodes) {
      const language = String(value.language || '').toLowerCase();
      if (!language) continue;
      if (seen.has(language)) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.uniqueLang, { path, value, details: `duplicate language ${language}` }));
      seen.add(language);
    }
  }
  if (c.in.length) pushEach(CONSTRAINT_COMPONENTS.in, value => c.in.some(allowed => termEquals(value, allowed)), 'value not in allowed set');
  for (const expected of c.hasValue) {
    if (!valueNodes.some(value => termEquals(value, expected))) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.hasValue, { path, value: expected, details: 'required value missing' }));
  }

  const bounds = [
    ['minExclusive', c.minExclusive, comparison => comparison > 0],
    ['minInclusive', c.minInclusive, comparison => comparison >= 0],
    ['maxExclusive', c.maxExclusive, comparison => comparison < 0],
    ['maxInclusive', c.maxInclusive, comparison => comparison <= 0],
  ];
  for (const [name, bound, predicate] of bounds) {
    if (!bound) continue;
    pushEach(CONSTRAINT_COMPONENTS[name], value => {
      const comparison = compareTerms(value, bound);
      return comparison != null && predicate(comparison);
    }, `${name} ${bound.value}`);
  }

  if (c.node.length) {
    for (const referenced of c.node) {
      for (const value of valueNodes) {
        const nested = evaluateReferencedShape(dataStore, compiled, referenced, value, context);
        if (!nested.conforms) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.node, { path, value, details: `does not conform to ${referenced.value}` }));
      }
    }
  }
  if (c.not.length) {
    for (const referenced of c.not) {
      for (const value of valueNodes) {
        const nested = evaluateReferencedShape(dataStore, compiled, referenced, value, context);
        if (nested.conforms) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.not, { path, value, details: `must not conform to ${referenced.value}` }));
      }
    }
  }
  for (const [name, refs] of [['and', c.and], ['or', c.or], ['xone', c.xone]]) {
    if (!refs.length) continue;
    for (const value of valueNodes) {
      const count = refs.reduce((sum, ref) => sum + (evaluateReferencedShape(dataStore, compiled, ref, value, context).conforms ? 1 : 0), 0);
      const valid = name === 'and' ? count === refs.length : name === 'or' ? count >= 1 : count === 1;
      if (!valid) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS[name], { path, value, details: `${count}/${refs.length} referenced shapes conformed` }));
    }
  }

  if (c.qualifiedValueShape) {
    const count = valueNodes.filter(value => evaluateReferencedShape(dataStore, compiled, c.qualifiedValueShape, value, context).conforms).length;
    if (c.qualifiedMinCount != null && count < c.qualifiedMinCount) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.qualifiedMinCount, { path, details: `${count} qualified values; minimum ${c.qualifiedMinCount}` }));
    if (c.qualifiedMaxCount != null && count > c.qualifiedMaxCount) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.qualifiedMaxCount, { path, details: `${count} qualified values; maximum ${c.qualifiedMaxCount}` }));
  }
  return results;
}

function validatePropertyShape(dataStore, compiled, shape, focusNode, context) {
  if (shape.deactivated) return { conforms: true, results: [] };
  const valueNodes = evaluatePath(dataStore, [focusNode], shape.path, context.options);
  const c = shape.constraints;
  const results = [];
  if (c.minCount != null && valueNodes.length < c.minCount) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.minCount, { path: shape.pathTerm, details: `${valueNodes.length} values; minimum ${c.minCount}` }));
  if (c.maxCount != null && valueNodes.length > c.maxCount) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.maxCount, { path: shape.pathTerm, details: `${valueNodes.length} values; maximum ${c.maxCount}` }));

  const comparisonPaths = [
    ['equals', c.equals, (left, right) => setEquals(left, right)],
    ['disjoint', c.disjoint, (left, right) => !setIntersects(left, right)],
  ];
  for (const [name, predicate, validate] of comparisonPaths) {
    if (!predicate) continue;
    const other = quads(dataStore, focusNode, predicate, null, null).map(q => q.object);
    if (!validate(valueNodes, other)) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS[name], { path: shape.pathTerm, details: `${name} path ${predicate.value}` }));
  }
  for (const [name, predicate, inclusive] of [['lessThan', c.lessThan, false], ['lessThanOrEquals', c.lessThanOrEquals, true]]) {
    if (!predicate) continue;
    const other = quads(dataStore, focusNode, predicate, null, null).map(q => q.object);
    for (const value of valueNodes) {
      for (const right of other) {
        const comparison = compareTerms(value, right);
        if (comparison == null || (inclusive ? comparison > 0 : comparison >= 0)) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS[name], { path: shape.pathTerm, value, details: `${value.value} is not ${inclusive ? '<=' : '<'} ${right.value}` }));
      }
    }
  }

  results.push(...validateValueConstraints(dataStore, compiled, shape, focusNode, valueNodes, shape.pathTerm, context));
  return { conforms: results.length === 0, results };
}

function validateNodeShape(dataStore, compiled, shape, focusNode, context) {
  if (shape.deactivated) return { conforms: true, results: [] };
  const results = validateValueConstraints(dataStore, compiled, shape, focusNode, [focusNode], null, context);
  for (const propertyKey of shape.propertyShapes || []) {
    const propertyShape = compiled.shapesById.get(propertyKey);
    if (propertyShape) results.push(...validatePropertyShape(dataStore, compiled, propertyShape, focusNode, context).results);
  }
  if (shape.constraints.closed) {
    const allowed = new Set(shape.constraints.ignoredProperties.map(term => term.value));
    allowed.add(RDF.type);
    for (const propertyKey of shape.propertyShapes || []) {
      const propertyShape = compiled.shapesById.get(propertyKey);
      if (propertyShape?.path?.kind === 'predicate') allowed.add(propertyShape.path.predicate.value);
    }
    for (const quad of quads(dataStore, focusNode, null, null, null)) {
      if (!allowed.has(quad.predicate.value)) results.push(resultFor(shape, focusNode, CONSTRAINT_COMPONENTS.closed, { path: quad.predicate, value: quad.object, details: `predicate ${quad.predicate.value} is not allowed` }));
    }
  }
  return { conforms: results.length === 0, results };
}

function validateShape(dataStore, compiled, shape, focusNode, context) {
  const recursionKey = `${shape.key}|${termKey(focusNode)}`;
  if (context.active.has(recursionKey)) return { conforms: true, results: [] };
  context.active.add(recursionKey);
  try {
    return shape.type === 'PropertyShape'
      ? validatePropertyShape(dataStore, compiled, shape, focusNode, context)
      : validateNodeShape(dataStore, compiled, shape, focusNode, context);
  } finally {
    context.active.delete(recursionKey);
  }
}

export function validateCompiledShacl(dataStore, compiled, options = {}) {
  const maxViolations = options.maxViolations ?? Number.POSITIVE_INFINITY;
  const selectedFocus = options.focusNodes ? new Set(options.focusNodes.map(termKey)) : null;
  const results = [];
  const context = { active: new Set(), options };
  const shapes = [...compiled.nodeShapes].sort((a, b) => a.key.localeCompare(b.key));
  outer: for (const shape of shapes) {
    for (const focusNode of targetNodes(dataStore, shape)) {
      if (selectedFocus && !selectedFocus.has(termKey(focusNode))) continue;
      const validation = validateShape(dataStore, compiled, shape, focusNode, context);
      for (const result of validation.results) {
        results.push(result);
        if (results.length >= maxViolations) break outer;
      }
    }
  }
  return {
    conforms: results.length === 0,
    results,
    checkedShapes: shapes.length,
    checkedFocusNodes: uniqueTerms(results.map(result => result.focusNode)).length,
  };
}

export function validateShaclCore(dataStore, shapesStore, options = {}) {
  return validateCompiledShacl(dataStore, compileShacl(shapesStore), options);
}

/**
 * Computes a conservative, dependency-aware focus set for a streaming delta.
 * Affected subjects are always included; objects are included for inverse and
 * targetObjectsOf relationships. Class membership changes additionally include
 * the changed subject so targetClass shapes are re-evaluated.
 */
export function affectedFocusNodes(delta, compiled) {
  const changed = [...(delta?.additions || delta?.added || []), ...(delta?.deletions || delta?.removed || [])];
  const nodes = [];
  for (const quad of changed) {
    if (!compiled.dependencyPredicates.has(quad.predicate?.value)) continue;
    nodes.push(quad.subject);
    if (quad.object?.termType !== 'Literal') nodes.push(quad.object);
  }
  return uniqueTerms(nodes);
}

export function validateShaclDelta(dataStore, compiledOrShapesStore, delta, options = {}) {
  const compiled = compiledOrShapesStore?.shapesById ? compiledOrShapesStore : compileShacl(compiledOrShapesStore);
  const focusNodes = affectedFocusNodes(delta, compiled);
  if (focusNodes.length === 0) return { conforms: true, results: [], checkedShapes: 0, checkedFocusNodes: 0, skipped: true };
  return validateCompiledShacl(dataStore, compiled, { ...options, focusNodes });
}
