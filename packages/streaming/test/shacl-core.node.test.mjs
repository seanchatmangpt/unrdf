import test from 'node:test';
import assert from 'node:assert/strict';
import {
  compileShacl,
  validateCompiledShacl,
  validateShaclCore,
  validateShaclDelta,
  evaluatePath,
  SH,
  RDF,
  XSD,
} from '../src/shacl-core.mjs';

const nn = value => ({ termType: 'NamedNode', value });
const bn = value => ({ termType: 'BlankNode', value });
const lit = (value, datatype = XSD.string, language = '') => ({
  termType: 'Literal',
  value: String(value),
  language,
  datatype: nn(language ? RDF.langString : datatype),
});
const q = (subject, predicate, object, graph = { termType: 'DefaultGraph', value: '' }) => ({ subject, predicate: nn(predicate), object, graph });
const same = (a, b) => a == null || b == null ? a == null && b == null : a.termType === b.termType && a.value === b.value && (a.language || '') === (b.language || '') && (a.datatype?.value || '') === (b.datatype?.value || '');

class Store {
  constructor(quads = []) { this.quads = [...quads]; }
  addQuad(quad) { this.quads.push(quad); }
  getQuads(s = null, p = null, o = null, g = null) {
    return this.quads.filter(quad => (!s || same(s, quad.subject)) && (!p || same(p, quad.predicate)) && (!o || same(o, quad.object)) && (!g || same(g, quad.graph)));
  }
  match(...args) { return this.getQuads(...args); }
}

let listCounter = 0;
function list(items, quads) {
  if (!items.length) return nn(RDF.nil);
  const nodes = items.map(() => bn(`list-${++listCounter}`));
  nodes.forEach((node, index) => {
    quads.push(q(node, RDF.first, items[index]));
    quads.push(q(node, RDF.rest, nodes[index + 1] || nn(RDF.nil)));
  });
  return nodes[0];
}

function basicShape({ shape = nn('urn:shape'), targetClass = nn('urn:Person'), property = bn('property'), path = nn('urn:name'), constraints = [] } = {}) {
  return new Store([
    q(shape, RDF.type, nn(SH.NodeShape)),
    q(shape, SH.targetClass, targetClass),
    q(shape, SH.property, property),
    q(property, RDF.type, nn(SH.PropertyShape)),
    q(property, SH.path, path),
    ...constraints,
  ]);
}

function component(result) { return result.sourceConstraintComponent.split('#').pop(); }

test('targetClass plus minCount/maxCount validates cardinality', () => {
  const person = nn('urn:alice');
  const property = bn('name-property');
  const shapes = basicShape({ property, constraints: [q(property, SH.minCount, lit(1, XSD.integer)), q(property, SH.maxCount, lit(1, XSD.integer))] });
  const missing = validateShaclCore(new Store([q(person, RDF.type, nn('urn:Person'))]), shapes);
  assert.equal(missing.conforms, false);
  assert.equal(component(missing.results[0]), 'MinCountConstraintComponent');
  const duplicate = validateShaclCore(new Store([
    q(person, RDF.type, nn('urn:Person')),
    q(person, 'urn:name', lit('Alice')),
    q(person, 'urn:name', lit('Alicia')),
  ]), shapes);
  assert.equal(component(duplicate.results[0]), 'MaxCountConstraintComponent');
});

test('datatype, node kind, lexical length and pattern constraints execute together', () => {
  const property = bn('code-property');
  const shapes = basicShape({ property, path: nn('urn:code'), constraints: [
    q(property, SH.datatype, nn(XSD.string)),
    q(property, SH.nodeKind, nn(SH.Literal)),
    q(property, SH.minLength, lit(3, XSD.integer)),
    q(property, SH.maxLength, lit(5, XSD.integer)),
    q(property, SH.pattern, lit('^[A-Z]+$')),
  ] });
  const person = nn('urn:p');
  const result = validateShaclCore(new Store([
    q(person, RDF.type, nn('urn:Person')),
    q(person, 'urn:code', lit('a')),
  ]), shapes);
  assert.deepEqual(new Set(result.results.map(component)), new Set(['MinLengthConstraintComponent', 'PatternConstraintComponent']));
});

test('languageIn, uniqueLang, sh:in and hasValue enforce value sets', () => {
  const shapeQuads = [];
  const property = bn('label-property');
  const allowedLanguages = list([lit('en'), lit('fr')], shapeQuads);
  const allowedValues = list([lit('Hello', XSD.string, 'en'), lit('Bonjour', XSD.string, 'fr')], shapeQuads);
  const shapes = basicShape({ property, path: nn('urn:label'), constraints: [
    ...shapeQuads,
    q(property, SH.languageIn, allowedLanguages),
    q(property, SH.uniqueLang, lit(true, XSD.boolean)),
    q(property, SH.in, allowedValues),
    q(property, SH.hasValue, lit('Hello', XSD.string, 'en')),
  ] });
  const person = nn('urn:p');
  const result = validateShaclCore(new Store([
    q(person, RDF.type, nn('urn:Person')),
    q(person, 'urn:label', lit('Hola', XSD.string, 'es')),
    q(person, 'urn:label', lit('Hi', XSD.string, 'en')),
    q(person, 'urn:label', lit('Hey', XSD.string, 'en')),
  ]), shapes);
  const components = new Set(result.results.map(component));
  assert.ok(components.has('LanguageInConstraintComponent'));
  assert.ok(components.has('UniqueLangConstraintComponent'));
  assert.ok(components.has('InConstraintComponent'));
  assert.ok(components.has('HasValueConstraintComponent'));
});

test('class and numeric range constraints validate RDF values', () => {
  const property = bn('score-property');
  const shapes = basicShape({ property, path: nn('urn:score'), constraints: [
    q(property, SH.minInclusive, lit(0, XSD.integer)),
    q(property, SH.maxExclusive, lit(100, XSD.integer)),
  ] });
  const person = nn('urn:p');
  const result = validateShaclCore(new Store([
    q(person, RDF.type, nn('urn:Person')),
    q(person, 'urn:score', lit(100, XSD.integer)),
  ]), shapes);
  assert.equal(component(result.results[0]), 'MaxExclusiveConstraintComponent');

  const friendProperty = bn('friend-property');
  const classShapes = basicShape({ property: friendProperty, path: nn('urn:friend'), constraints: [q(friendProperty, SH.class, nn('urn:Person'))] });
  const bob = nn('urn:bob');
  const classResult = validateShaclCore(new Store([
    q(person, RDF.type, nn('urn:Person')),
    q(person, 'urn:friend', bob),
  ]), classShapes);
  assert.equal(component(classResult.results[0]), 'ClassConstraintComponent');
});

test('equals, disjoint, lessThan and lessThanOrEquals compare sibling paths', () => {
  const subject = nn('urn:p');
  const make = (constraint, sibling) => {
    const property = bn(`compare-${constraint}`);
    return basicShape({ property, path: nn('urn:left'), constraints: [q(property, SH[constraint], nn(sibling))] });
  };
  const data = new Store([
    q(subject, RDF.type, nn('urn:Person')),
    q(subject, 'urn:left', lit(10, XSD.integer)),
    q(subject, 'urn:right', lit(5, XSD.integer)),
  ]);
  assert.equal(component(validateShaclCore(data, make('equals', 'urn:right')).results[0]), 'EqualsConstraintComponent');
  assert.equal(validateShaclCore(data, make('disjoint', 'urn:right')).conforms, true);
  assert.equal(component(validateShaclCore(data, make('lessThan', 'urn:right')).results[0]), 'LessThanConstraintComponent');
  assert.equal(component(validateShaclCore(data, make('lessThanOrEquals', 'urn:right')).results[0]), 'LessThanOrEqualsConstraintComponent');
});

test('inverse, sequence, alternative and transitive property paths evaluate', () => {
  const pathQuads = [];
  const inverseNode = bn('inverse');
  pathQuads.push(q(inverseNode, SH.inversePath, nn('urn:parent')));
  const sequenceHead = list([nn('urn:parent'), nn('urn:name')], pathQuads);
  const alternatives = list([nn('urn:name'), nn('urn:alias')], pathQuads);
  const alternativeNode = bn('alternative');
  pathQuads.push(q(alternativeNode, SH.alternativePath, alternatives));
  const closureNode = bn('closure');
  pathQuads.push(q(closureNode, SH.zeroOrMorePath, nn('urn:parent')));
  const shapes = new Store(pathQuads);
  const alice = nn('urn:alice');
  const bob = nn('urn:bob');
  const carol = nn('urn:carol');
  const data = new Store([
    q(alice, 'urn:parent', bob),
    q(bob, 'urn:parent', carol),
    q(bob, 'urn:name', lit('Bob')),
    q(bob, 'urn:alias', lit('B')),
  ]);
  const compiledInverse = { kind: 'inverse', path: { kind: 'predicate', predicate: nn('urn:parent') } };
  assert.deepEqual(evaluatePath(data, [bob], compiledInverse).map(x => x.value), ['urn:alice']);
  const compiled = compileShacl(new Store([
    ...pathQuads,
    q(nn('urn:dummy'), RDF.type, nn(SH.NodeShape)),
  ]));
  assert.ok(compiled);
  assert.deepEqual(evaluatePath(data, [alice], { kind: 'sequence', paths: [{ kind: 'predicate', predicate: nn('urn:parent') }, { kind: 'predicate', predicate: nn('urn:name') }] }).map(x => x.value), ['Bob']);
  assert.deepEqual(new Set(evaluatePath(data, [bob], { kind: 'alternative', paths: [{ kind: 'predicate', predicate: nn('urn:name') }, { kind: 'predicate', predicate: nn('urn:alias') }] }).map(x => x.value)), new Set(['Bob', 'B']));
  assert.deepEqual(evaluatePath(data, [alice], { kind: 'zeroOrMore', path: { kind: 'predicate', predicate: nn('urn:parent') } }).map(x => x.value), ['urn:alice', 'urn:bob', 'urn:carol']);
  assert.ok(sequenceHead && alternativeNode && closureNode && inverseNode);
});

test('closed shapes reject undeclared predicates but honor ignored properties', () => {
  const shapeQuads = [];
  const ignored = list([nn('urn:ignored')], shapeQuads);
  const shape = nn('urn:closed-shape');
  const property = bn('known-property');
  const shapes = basicShape({ shape, property, path: nn('urn:known'), constraints: [
    ...shapeQuads,
    q(shape, SH.closed, lit(true, XSD.boolean)),
    q(shape, SH.ignoredProperties, ignored),
  ] });
  const focus = nn('urn:p');
  const result = validateShaclCore(new Store([
    q(focus, RDF.type, nn('urn:Person')),
    q(focus, 'urn:known', lit('ok')),
    q(focus, 'urn:ignored', lit('ok')),
    q(focus, 'urn:unexpected', lit('bad')),
  ]), shapes);
  assert.equal(result.results.length, 1);
  assert.equal(result.results[0].resultPath.value, 'urn:unexpected');
});

test('node, not, and, or and xone compose referenced shapes', () => {
  const root = nn('urn:root');
  const stringShape = bn('string-shape');
  const shortShape = bn('short-shape');
  const longShape = bn('long-shape');
  const andListQuads = [];
  const andHead = list([stringShape, shortShape], andListQuads);
  const orHead = list([shortShape, longShape], andListQuads);
  const xoneHead = list([shortShape, longShape], andListQuads);
  const shapes = new Store([
    q(root, RDF.type, nn(SH.NodeShape)),
    q(root, SH.targetNode, lit('abc')),
    q(root, SH.and, andHead),
    q(root, SH.or, orHead),
    q(root, SH.xone, xoneHead),
    q(root, SH.not, longShape),
    q(stringShape, SH.datatype, nn(XSD.string)),
    q(shortShape, SH.maxLength, lit(3, XSD.integer)),
    q(longShape, SH.minLength, lit(10, XSD.integer)),
    ...andListQuads,
  ]);
  const result = validateShaclCore(new Store(), shapes);
  assert.equal(result.conforms, true);

  const badShapes = new Store([...shapes.quads, q(root, SH.node, longShape)]);
  const bad = validateShaclCore(new Store(), badShapes);
  assert.ok(bad.results.some(r => component(r) === 'NodeConstraintComponent'));
});

test('qualified value shapes count conforming values', () => {
  const property = bn('members-property');
  const qualified = bn('adult-shape');
  const ageProperty = bn('age-property');
  const shapes = basicShape({ property, path: nn('urn:member'), constraints: [
    q(property, SH.qualifiedValueShape, qualified),
    q(property, SH.qualifiedMinCount, lit(2, XSD.integer)),
    q(qualified, SH.property, ageProperty),
    q(ageProperty, RDF.type, nn(SH.PropertyShape)),
    q(ageProperty, SH.path, nn('urn:age')),
    q(ageProperty, SH.minInclusive, lit(18, XSD.integer)),
  ] });
  const group = nn('urn:group');
  const a = nn('urn:a');
  const b = nn('urn:b');
  const result = validateShaclCore(new Store([
    q(group, RDF.type, nn('urn:Person')),
    q(group, 'urn:member', a),
    q(group, 'urn:member', b),
    q(a, 'urn:age', lit(20, XSD.integer)),
    q(b, 'urn:age', lit(17, XSD.integer)),
  ]), shapes);
  assert.equal(component(result.results[0]), 'QualifiedMinCountConstraintComponent');
});

test('targetNode, targetSubjectsOf and targetObjectsOf select focus nodes deterministically', () => {
  const shape = nn('urn:shape');
  const shapes = new Store([
    q(shape, RDF.type, nn(SH.NodeShape)),
    q(shape, SH.targetNode, nn('urn:direct')),
    q(shape, SH.targetSubjectsOf, nn('urn:knows')),
    q(shape, SH.targetObjectsOf, nn('urn:knows')),
    q(shape, SH.nodeKind, nn(SH.Literal)),
  ]);
  const result = validateShaclCore(new Store([q(nn('urn:a'), 'urn:knows', nn('urn:b'))]), shapes);
  assert.deepEqual(result.results.map(r => r.focusNode.value).sort(), ['urn:a', 'urn:b', 'urn:direct']);
});

test('maxViolations provides deterministic early termination', () => {
  const shape = nn('urn:shape');
  const shapes = new Store([
    q(shape, RDF.type, nn(SH.NodeShape)),
    q(shape, SH.targetNode, nn('urn:a')),
    q(shape, SH.targetNode, nn('urn:b')),
    q(shape, SH.nodeKind, nn(SH.Literal)),
  ]);
  const result = validateShaclCore(new Store(), shapes, { maxViolations: 1 });
  assert.equal(result.results.length, 1);
  assert.equal(result.results[0].focusNode.value, 'urn:a');
});

test('delta validation rechecks only dependency-related focus nodes', () => {
  const property = bn('name-property');
  const shapes = basicShape({ property, constraints: [q(property, SH.minCount, lit(1, XSD.integer))] });
  const compiled = compileShacl(shapes);
  const alice = nn('urn:alice');
  const bob = nn('urn:bob');
  const data = new Store([
    q(alice, RDF.type, nn('urn:Person')),
    q(bob, RDF.type, nn('urn:Person')),
    q(bob, 'urn:name', lit('Bob')),
  ]);
  const result = validateShaclDelta(data, compiled, { deletions: [q(alice, 'urn:name', lit('Alice'))] });
  assert.equal(result.results.length, 1);
  assert.equal(result.results[0].focusNode.value, 'urn:alice');
  const skipped = validateShaclDelta(data, compiled, { additions: [q(alice, 'urn:unrelated', lit('x'))] });
  assert.equal(skipped.skipped, true);
});
