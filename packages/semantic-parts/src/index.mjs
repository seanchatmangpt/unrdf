/**
 * Evidence-bounded semantic software-parts graph.
 *
 * CodeGraph is an observation source, not an authority source. This package
 * converts the public release's normalized table rows into a deterministic
 * graph and discovers substitution candidates by shared grounded semantics.
 * It never executes a replacement.
 */

export const SEMANTIC_PARTS_SCHEMA = 'unrdf.semantic-parts.v1';
export const CODEGRAPH_RELEASE = 'codegraph_release_v1';
export const SEMANTIC_AXES = Object.freeze([
  'algorithm',
  'domain',
  'paradigm',
  'design_pattern',
]);

const AXIS_TABLES = Object.freeze({
  algorithm: { concepts: 'algorithms', edges: 'file_algorithm' },
  domain: { concepts: 'domains', edges: 'file_domain' },
  paradigm: { concepts: 'paradigms', edges: 'file_paradigm' },
  design_pattern: { concepts: 'design_patterns', edges: 'file_design_pattern' },
});

function asArray(value, label) {
  if (!Array.isArray(value)) throw new TypeError(`${label} must be an array`);
  return value;
}

function identity(value, label) {
  if (value === undefined || value === null || value === '') {
    throw new TypeError(`${label} is required`);
  }
  if (typeof value === 'string') return value;
  if (typeof value === 'number' && Number.isFinite(value)) return String(value);
  if (typeof value === 'bigint') return String(value);
  throw new TypeError(`REFUSED_NON_SCALAR_IDENTITY:${label}`);
}

/**
 * Locale-independent code-point ordering. `localeCompare` depends on the
 * host ICU/locale, which would make the canonical graph environment-relative.
 */
function compareCodePoints(left, right) {
  if (left === right) return 0;
  return left < right ? -1 : 1;
}

function asRecord(value, label) {
  if (value === undefined) return {};
  if (value === null || typeof value !== 'object' || Array.isArray(value)) {
    throw new TypeError(`${label} must be an object`);
  }
  return value;
}

function conceptProjection(row) {
  return {
    concept_id: identity(row.concept_id, 'concept_id'),
    name: typeof row.name === 'string' ? row.name : '',
    wikidata_qid: typeof row.wikidata_qid === 'string' && row.wikidata_qid ? row.wikidata_qid : null,
    label: typeof row.label === 'string' && row.label ? row.label : null,
  };
}

function edgeEvidence(row) {
  const evidence = {};
  if (typeof row.confidence === 'number' || typeof row.confidence === 'string') {
    evidence.confidence = row.confidence;
  }
  if (typeof row.description === 'string' && row.description) {
    evidence.description = row.description;
  }
  return evidence;
}

function semanticIdentity(axis, concept) {
  return concept.wikidata_qid
    ? `wikidata:${concept.wikidata_qid}`
    : `codegraph:${axis}:${concept.concept_id}`;
}

function evidenceKey(evidence) {
  return `${typeof evidence.confidence}:${evidence.confidence ?? ''}\u0000${evidence.description ?? ''}`;
}

/**
 * Sort by (semantic_id, concept_id) in code-point order and keep the first
 * entry per semantic identity. Aliases (distinct concept_ids sharing one
 * grounded identity) collapse to the lowest concept_id. A re-delivered edge
 * (same concept_id) is idempotent only when its evidence is identical;
 * conflicting evidence has no canonical winner, so it is refused rather than
 * resolved by input row order. Duplicates are adjacent after the sort, so the
 * check costs one comparison per entry.
 */
function orderedUniqueConcepts(entries, axis, fileId) {
  const sorted = [...entries].sort((left, right) =>
    compareCodePoints(left.semantic_id, right.semantic_id)
    || compareCodePoints(left.concept_id, right.concept_id),
  );
  const unique = [];
  let previous = null;
  for (const entry of sorted) {
    if (previous && previous.semantic_id === entry.semantic_id) {
      if (previous.concept_id === entry.concept_id
        && evidenceKey(previous.evidence) !== evidenceKey(entry.evidence)) {
        throw new Error(`REFUSED_CONFLICTING_DUPLICATE_EDGE:${axis}:${fileId}:${entry.concept_id}`);
      }
      continue;
    }
    unique.push(entry);
    previous = entry;
  }
  return unique;
}

/**
 * Convert already-decoded rows from CodeGraph's public release tables into
 * the canonical semantic-parts graph. Parquet I/O intentionally remains
 * outside this package; callers may use any standards-compliant reader.
 */
export function fromCodeGraphTables(tables) {
  const { files, concepts: rawConcepts, edges: rawEdges } = asRecord(tables, 'tables');
  const concepts = asRecord(rawConcepts, 'concepts');
  const edges = asRecord(rawEdges, 'edges');
  const fileRows = asArray(files, 'files');
  const parts = new Map();

  for (const row of fileRows) {
    const fileId = identity(row.file_id, 'file_id');
    if (parts.has(fileId)) throw new Error(`REFUSED_DUPLICATE_FILE:${fileId}`);
    parts.set(fileId, {
      part_id: `codegraph:file:${fileId}`,
      source: {
        kind: 'CodeGraph',
        release: CODEGRAPH_RELEASE,
        file_id: fileId,
        sample_id: row.sample_id === undefined || row.sample_id === null ? null : String(row.sample_id),
        language: typeof row.language === 'string' ? row.language : null,
      },
      semantics: Object.fromEntries(SEMANTIC_AXES.map((axis) => [axis, []])),
      authority: 'NONE',
      standing: 'OBSERVED',
    });
  }

  for (const axis of SEMANTIC_AXES) {
    const table = AXIS_TABLES[axis];
    const conceptRows = asArray(concepts[table.concepts] ?? [], `concepts.${table.concepts}`);
    const edgeRows = asArray(edges[table.edges] ?? [], `edges.${table.edges}`);
    const byConcept = new Map();

    for (const row of conceptRows) {
      const projected = conceptProjection(row);
      if (byConcept.has(projected.concept_id)) {
        throw new Error(`REFUSED_DUPLICATE_CONCEPT:${axis}:${projected.concept_id}`);
      }
      byConcept.set(projected.concept_id, projected);
    }

    for (const row of edgeRows) {
      const fileId = identity(row.file_id, 'file_id');
      const conceptId = identity(row.concept_id, 'concept_id');
      const part = parts.get(fileId);
      const concept = byConcept.get(conceptId);
      if (!part) throw new Error(`REFUSED_DANGLING_FILE_EDGE:${axis}:${fileId}`);
      if (!concept) throw new Error(`REFUSED_DANGLING_CONCEPT_EDGE:${axis}:${conceptId}`);
      part.semantics[axis].push({
        ...concept,
        semantic_id: semanticIdentity(axis, concept),
        evidence: edgeEvidence(row),
      });
    }
  }

  const graph = {
    schema: SEMANTIC_PARTS_SCHEMA,
    source: { kind: 'CodeGraph', release: CODEGRAPH_RELEASE },
    authority: 'NONE',
    parts: [...parts.values()]
      .map((part) => ({
        ...part,
        semantics: Object.fromEntries(
          SEMANTIC_AXES.map((axis) => [axis, orderedUniqueConcepts(part.semantics[axis], axis, part.source.file_id)]),
        ),
      }))
      .sort((left, right) => compareCodePoints(left.part_id, right.part_id)),
  };

  return admitSemanticPartsGraph(graph);
}

/**
 * Fail-closed structural admission for a semantic-parts graph.
 */
export function admitSemanticPartsGraph(graph) {
  if (!graph || graph.schema !== SEMANTIC_PARTS_SCHEMA) {
    throw new Error('REFUSED_SCHEMA');
  }
  if (graph.authority !== 'NONE') {
    throw new Error('REFUSED_AMBIENT_AUTHORITY');
  }
  const parts = asArray(graph.parts, 'parts');
  const ids = new Set();

  for (const part of parts) {
    if (!part || typeof part !== 'object') throw new Error('REFUSED_PART_NOT_OBJECT');
    const partId = identity(part.part_id, 'part_id');
    if (ids.has(partId)) throw new Error(`REFUSED_DUPLICATE_PART:${partId}`);
    ids.add(partId);
    if (part.authority !== 'NONE') throw new Error(`REFUSED_PART_AUTHORITY:${partId}`);
    if (!part.semantics || typeof part.semantics !== 'object') {
      throw new Error(`REFUSED_SEMANTICS_MISSING:${partId}`);
    }
    for (const axis of SEMANTIC_AXES) {
      const values = asArray(part.semantics[axis] ?? [], `${partId}.semantics.${axis}`);
      const semanticIds = new Set();
      for (const value of values) {
        if (!value || typeof value !== 'object') {
          throw new Error(`REFUSED_SEMANTIC_VALUE_NOT_OBJECT:${partId}:${axis}`);
        }
        const semanticId = identity(value.semantic_id, 'semantic_id');
        if (semanticIds.has(semanticId)) {
          throw new Error(`REFUSED_DUPLICATE_SEMANTIC_ID:${partId}:${semanticId}`);
        }
        semanticIds.add(semanticId);
      }
    }
  }
  return graph;
}

/**
 * Resolve a part by exact part_id first, then by CodeGraph file_id. A file_id
 * that matches more than one part is refused rather than resolved by order.
 */
function partById(graph, partId) {
  const canonical = identity(partId, 'part reference');
  const exact = graph.parts.find((part) => part.part_id === canonical);
  if (exact) return exact;
  const byFile = graph.parts.filter((part) => part.source?.file_id === canonical);
  if (byFile.length > 1) throw new Error(`REFUSED_AMBIGUOUS_PART_REFERENCE:${canonical}`);
  return byFile[0];
}

function axisIdentities(part, axis) {
  return new Set((part.semantics[axis] ?? []).map((item) => item.semantic_id));
}

/**
 * Discover authority-free substitution candidates. A candidate must share at
 * least one semantic identity on every required axis. The result is SELECT
 * evidence only; it is not proof of behavioral equivalence and cannot DO.
 */
export function findAlternatives(
  graph,
  subjectId,
  { requiredAxes = ['algorithm'], minimumShared = 1 } = {},
) {
  admitSemanticPartsGraph(graph);
  const subject = partById(graph, subjectId);
  if (!subject) throw new Error(`REFUSED_UNKNOWN_SUBJECT:${subjectId}`);

  if (!Number.isSafeInteger(minimumShared) || minimumShared < 1) {
    throw new Error(`REFUSED_MINIMUM_SHARED:${String(minimumShared)}`);
  }
  const axes = asArray(requiredAxes, 'requiredAxes').map((axis) => {
    if (!SEMANTIC_AXES.includes(axis)) throw new Error(`REFUSED_UNKNOWN_AXIS:${axis}`);
    return axis;
  });
  if (axes.length === 0) throw new Error('REFUSED_REQUIRED_AXES_EMPTY');
  if (new Set(axes).size !== axes.length) throw new Error('REFUSED_DUPLICATE_REQUIRED_AXIS');

  const subjectSets = Object.fromEntries(axes.map((axis) => [axis, axisIdentities(subject, axis)]));
  for (const axis of axes) {
    if (subjectSets[axis].size === 0) throw new Error(`REFUSED_SUBJECT_AXIS_EMPTY:${axis}`);
  }

  return graph.parts
    .filter((candidate) => candidate.part_id !== subject.part_id)
    .map((candidate) => {
      const shared = {};
      let sharedCount = 0;
      let subjectCount = 0;
      let allAxesSatisfied = true;

      for (const axis of axes) {
        const candidateSet = axisIdentities(candidate, axis);
        const overlap = [...subjectSets[axis]].filter((id) => candidateSet.has(id)).sort();
        shared[axis] = overlap;
        sharedCount += overlap.length;
        subjectCount += subjectSets[axis].size;
        if (overlap.length === 0) allAxesSatisfied = false;
      }

      return {
        part_id: candidate.part_id,
        language: candidate.source?.language ?? null,
        shared,
        shared_count: sharedCount,
        coverage: subjectCount === 0 ? 0 : sharedCount / subjectCount,
        authority: 'NONE',
        standing: 'CANDIDATE',
        all_required_axes_satisfied: allAxesSatisfied,
      };
    })
    .filter((candidate) => candidate.all_required_axes_satisfied && candidate.shared_count >= minimumShared)
    .sort((left, right) =>
      right.coverage - left.coverage
      || right.shared_count - left.shared_count
      || compareCodePoints(left.part_id, right.part_id),
    );
}

/**
 * Cheapest falsifier for a proposed substitution: every required semantic
 * axis must retain at least one exact grounded/canonical concept identity.
 */
export function substitutionSurvivesSemanticFalsifier(graph, subjectId, candidateId, requiredAxes = ['algorithm']) {
  const candidates = findAlternatives(graph, subjectId, { requiredAxes, minimumShared: 1 });
  const candidate = partById(graph, candidateId);
  if (!candidate) throw new Error(`REFUSED_UNKNOWN_CANDIDATE:${candidateId}`);
  return candidates.some((entry) => entry.part_id === candidate.part_id);
}
