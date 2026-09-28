/**
 * Git swarm OCEL 2.0 primitives.
 *
 * Canonical semantics live here; Git runtimes such as GitVan persist the resulting
 * document without re-interpreting it.
 */
import { namedNode, literal, quad } from '@unrdf/core';
import { admitSwarmIdentity, bindExactSwarmSubject } from './swarm-contracts.mjs';

export const OCEL2_CONTEXT = 'https://ocelstandard.org/ocel2.0/context.json';
export const GIT_SWARM_NS = 'https://unrdf.dev/ns/git-swarm#';

const OBJECT_TYPES = [
  ['Run', ['lane']],
  ['Repository', ['fullName']],
  ['Branch', ['name']],
  ['Commit', ['sha', 'role']],
  ['CodeSurface', ['path']],
  ['ToolEdge', ['name']],
  ['Task', ['id']],
];

const EVENT_TYPES = [
  'task_started',
  'work_selected',
  'scope_locked',
  'source_read',
  'write_attempted',
  'write_succeeded',
  'write_failed',
  'fallback_selected',
  'commit_created',
  'receipt_materialized',
  'publication_attempted',
  'publication_succeeded',
  'publication_failed',
];

function attr(name, type = 'string') {
  return { name, type };
}

function valueAttr(name, value, time) {
  return { name, time, value };
}

function object(id, type, attributes = []) {
  return { id, type, attributes, relationships: [] };
}

function relationship(objectId, qualifier) {
  return { objectId, qualifier };
}

function requireString(value, name) {
  if (!value || typeof value !== 'string') throw new TypeError(`${name} must be a non-empty string`);
  return value;
}

/**
 * Create a standards-shaped OCEL 2.0 document for one Git manufacturing episode.
 *
 * @param {object} input
 * @param {string} input.runId
 * @param {string} input.lane
 * @param {string} input.repository
 * @param {string} input.branch
 * @param {string} input.baseCommit
 * @param {string} input.commit
 * @param {string} input.time ISO timestamp
 * @param {string[]} [input.surfaces]
 * @param {Array<object>} [input.events]
 * @returns {object}
 */
export function createGitSwarmOcel(input) {
  const runId = requireString(input.runId, 'runId');
  const lane = requireString(input.lane, 'lane');
  const repository = requireString(input.repository, 'repository');
  const branch = requireString(input.branch, 'branch');
  const baseCommit = requireString(input.baseCommit, 'baseCommit');
  const commit = requireString(input.commit, 'commit');
  const task = requireString(input.task, 'task');
  const tool = requireString(input.tool, 'tool');
  const admission = admitSwarmIdentity({ repository, branch, commit, baseCommit, task, tool, sequence: 1 });
  if (!admission.admitted) throw new TypeError(`Git swarm subject refused: ${admission.refusals.map(r => r.code).join(',')}`);
  const exactSubject = bindExactSwarmSubject({ repository, branch, commit, task, tool });
  const time = requireString(input.time, 'time');
  const surfaces = [...new Set(input.surfaces ?? [])].sort();
  const suppliedEvents = input.events ?? [];

  const ids = {
    run: `run:${runId}`,
    repo: `repo:${repository}`,
    branch: `branch:${repository}:${branch}`,
    base: `commit:${baseCommit}`,
    commit: `commit:${commit}`,
    task: `task:${task}`,
    tool: `tool:${tool}`,
  };

  const objects = [
    object(ids.run, 'Run', [valueAttr('lane', lane, time), valueAttr('exactSubject', exactSubject, time)]),
    object(ids.repo, 'Repository', [valueAttr('fullName', repository, time)]),
    object(ids.branch, 'Branch', [valueAttr('name', branch, time)]),
    object(ids.base, 'Commit', [valueAttr('sha', baseCommit, time), valueAttr('role', 'base', time)]),
    object(ids.commit, 'Commit', [valueAttr('sha', commit, time), valueAttr('role', 'produced', time)]),
    object(ids.task, 'Task', [valueAttr('id', task, time)]),
    object(ids.tool, 'ToolEdge', [valueAttr('name', tool, time)]),
    ...surfaces.map(path => object(`surface:${repository}:${path}`, 'CodeSurface', [valueAttr('path', path, time)])),
  ];

  const toolNames = [...new Set(suppliedEvents.map(e => e.tool).filter(Boolean).filter(name => name !== tool))].sort();
  for (const tool of toolNames) {
    objects.push(object(`tool:${tool}`, 'ToolEdge', [valueAttr('name', tool, time)]));
  }

  const events = suppliedEvents
    .map((event, index) => {
      const sequence = Number.isInteger(event.sequence) ? event.sequence : index + 1;
      const eventTime = event.time ?? time;
      const relationships = [
        relationship(ids.run, 'run'),
        relationship(ids.repo, 'repository'),
        relationship(ids.branch, 'branch'),
        relationship(ids.commit, 'commit'),
        relationship(ids.task, 'task'),
        relationship(ids.tool, 'admitted-tool'),
      ];
      if (event.surface) relationships.push(relationship(`surface:${repository}:${event.surface}`, 'surface'));
      if (event.tool) relationships.push(relationship(`tool:${event.tool}`, 'edge'));
      return {
        id: event.id ?? `event:${runId}:${String(sequence).padStart(6, '0')}`,
        type: event.type,
        time: eventTime,
        attributes: [
          { name: 'sequence', value: sequence },
          ...(event.outcome === undefined ? [] : [{ name: 'outcome', value: String(event.outcome) }]),
          ...(event.failureClass === undefined ? [] : [{ name: 'failureClass', value: String(event.failureClass) }]),
        ],
        relationships,
      };
    })
    .sort((a, b) => Number(a.attributes[0].value) - Number(b.attributes[0].value));

  return {
    '@context': OCEL2_CONTEXT,
    objectTypes: OBJECT_TYPES.map(([name, names]) => ({
      name,
      attributes: names.map(n => attr(n)),
    })),
    eventTypes: EVENT_TYPES.map(name => ({
      name,
      attributes: [attr('sequence', 'integer'), attr('outcome'), attr('failureClass')],
    })),
    objects,
    events,
  };
}

/**
 * Structural and referential validation for OCEL 2.0 JSON documents.
 * This intentionally checks the reusable interchange boundary, not process conformance.
 */
export function validateOcel2Document(document) {
  const errors = [];
  if (!document || typeof document !== 'object') return { valid: false, errors: ['document must be an object'] };

  for (const key of ['objectTypes', 'eventTypes', 'objects', 'events']) {
    if (!Array.isArray(document[key])) errors.push(`${key} must be an array`);
  }
  if (errors.length) return { valid: false, errors };

  const objectTypes = new Set(document.objectTypes.map(x => x?.name).filter(Boolean));
  const eventTypes = new Set(document.eventTypes.map(x => x?.name).filter(Boolean));
  const objectIds = new Set();

  for (const obj of document.objects) {
    if (!obj?.id) errors.push('object missing id');
    else if (objectIds.has(obj.id)) errors.push(`duplicate object id: ${obj.id}`);
    else objectIds.add(obj.id);
    if (!objectTypes.has(obj?.type)) errors.push(`object ${obj?.id ?? '<unknown>'} uses undeclared type ${obj?.type}`);
  }

  const eventIds = new Set();
  let lastSequence = -Infinity;
  for (const event of document.events) {
    if (!event?.id) errors.push('event missing id');
    else if (eventIds.has(event.id)) errors.push(`duplicate event id: ${event.id}`);
    else eventIds.add(event.id);
    if (!eventTypes.has(event?.type)) errors.push(`event ${event?.id ?? '<unknown>'} uses undeclared type ${event?.type}`);
    if (!event?.time || Number.isNaN(Date.parse(event.time))) errors.push(`event ${event?.id ?? '<unknown>'} has invalid time`);
    const seq = event?.attributes?.find(a => a.name === 'sequence')?.value;
    if (!Number.isInteger(seq)) errors.push(`event ${event?.id ?? '<unknown>'} missing integer sequence`);
    else if (seq <= lastSequence) errors.push(`event sequence must be strictly increasing: ${seq}`);
    else lastSequence = seq;
    for (const rel of event?.relationships ?? []) {
      if (!objectIds.has(rel.objectId)) errors.push(`event ${event.id} references unknown object ${rel.objectId}`);
    }
  }
  return { valid: errors.length === 0, errors };
}

function iri(base, kind, id) {
  return namedNode(`${base}${kind}/${encodeURIComponent(id)}`);
}

/**
 * Project OCEL objects/events/relationships into RDF quads suitable for an UNRDF store.
 */
export function gitSwarmOcelToQuads(document, { baseIri = GIT_SWARM_NS } = {}) {
  const check = validateOcel2Document(document);
  if (!check.valid) throw new TypeError(`Invalid OCEL 2.0 document: ${check.errors.join('; ')}`);

  const RDF_TYPE = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  const OCEL_OBJECT = namedNode(`${baseIri}Object`);
  const OCEL_EVENT = namedNode(`${baseIri}Event`);
  const TYPE = namedNode(`${baseIri}type`);
  const TIME = namedNode(`${baseIri}time`);
  const RELATES = namedNode(`${baseIri}relatesTo`);
  const QUALIFIER = namedNode(`${baseIri}qualifier`);
  const ATTRIBUTE = namedNode(`${baseIri}attribute`);
  const NAME = namedNode(`${baseIri}name`);
  const VALUE = namedNode(`${baseIri}value`);

  const quads = [];
  for (const obj of document.objects) {
    const s = iri(baseIri, 'object', obj.id);
    quads.push(quad(s, RDF_TYPE, OCEL_OBJECT), quad(s, TYPE, literal(obj.type)));
    for (const a of obj.attributes ?? []) {
      const aNode = iri(baseIri, 'attribute', `${obj.id}:${a.name}:${a.time ?? ''}`);
      quads.push(quad(s, ATTRIBUTE, aNode), quad(aNode, NAME, literal(a.name)), quad(aNode, VALUE, literal(String(a.value))));
      if (a.time) quads.push(quad(aNode, TIME, literal(a.time)));
    }
  }

  for (const event of document.events) {
    const s = iri(baseIri, 'event', event.id);
    quads.push(quad(s, RDF_TYPE, OCEL_EVENT), quad(s, TYPE, literal(event.type)), quad(s, TIME, literal(event.time)));
    for (const rel of event.relationships ?? []) {
      const rNode = iri(baseIri, 'relationship', `${event.id}:${rel.qualifier}:${rel.objectId}`);
      quads.push(
        quad(s, RELATES, rNode),
        quad(rNode, VALUE, iri(baseIri, 'object', rel.objectId)),
        quad(rNode, QUALIFIER, literal(rel.qualifier)),
      );
    }
    for (const a of event.attributes ?? []) {
      const aNode = iri(baseIri, 'attribute', `${event.id}:${a.name}`);
      quads.push(quad(s, ATTRIBUTE, aNode), quad(aNode, NAME, literal(a.name)), quad(aNode, VALUE, literal(String(a.value))));
    }
  }
  return quads;
}
