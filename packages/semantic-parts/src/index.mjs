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
  return String(value);
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

function orderedUniqueConcepts(entries) {
  const seen = new Set();
  return entries
    .filter((entry) => {
      const key = entry.concept_id;
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    })
    .sort((left, right) => left.concept_id.localeCompare(right.concept_id));
}

/**
 * Convert already-decoded rows from CodeGraph's public release tables into
 * the canonical semantic-parts graph. Parquet I/O intentionally remains
 * outside this package; callers may use any standards-compliant reader.
 */
export function fromCodeGraphTables({ files, concepts = {}, edges = {} }) {
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
          SEMANTIC_AXES.map((axis) => [axis, orderedUniqueConcepts(part.semantics[axis])]),
        ),
      }))
      .sort((left, right) => left.part_id.localeCompare(right.part_id)),
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

function partById(graph, partId) {
  const canonical = String(partId);
  return graph.parts.find((part) => part.part_id === canonical || part.source?.file_id === canonical);
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

  const axes = requiredAxes.map((axis) => {
    if (!SEMANTIC_AXES.includes(axis)) throw new Error(`REFUSED_UNKNOWN_AXIS:${axis}`);
    return axis;
  });
  if (axes.length === 0) throw new Error('REFUSED_REQUIRED_AXES_EMPTY');

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
      || left.part_id.localeCompare(right.part_id),
    );
}

/**
 * Cheapest falsifier for a proposed substitution: every required semantic
 * axis must retain at least one exact grounded/canonical concept identity.
 */
export function substitutionSurvivesSemanticFalsifier(graph, subjectId, candidateId, requiredAxes = ['algorithm']) {
  const candidates = findAlternatives(graph, subjectId, { requiredAxes, minimumShared: 1 });
  return candidates.some((candidate) => candidate.part_id === candidateId);
}
