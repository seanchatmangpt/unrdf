/**
 * Deterministic synthetic CodeGraph release tables for benchmarking.
 * A fixed-seed LCG makes every run build byte-identical input.
 */
export function syntheticTables({ files = 10000, conceptsPerAxis = 500, edgesPerFile = 3, seed = 26926 } = {}) {
  let state = seed >>> 0;
  const next = () => {
    state = (Math.imul(state, 1664525) + 1013904223) >>> 0;
    return state;
  };
  const languages = ['Python', 'Rust', 'JavaScript', 'Go', 'Java', 'Elixir'];
  const axes = [
    ['algorithms', 'file_algorithm'],
    ['domains', 'file_domain'],
    ['paradigms', 'file_paradigm'],
    ['design_patterns', 'file_design_pattern'],
  ];
  const fileRows = Array.from({ length: files }, (_, i) => ({
    file_id: i + 1,
    sample_id: `s-${i + 1}`,
    language: languages[i % languages.length],
  }));
  const concepts = {};
  const edges = {};
  for (const [conceptTable, edgeTable] of axes) {
    concepts[conceptTable] = Array.from({ length: conceptsPerAxis }, (_, c) => ({
      concept_id: c + 1,
      name: `${conceptTable}-${c + 1}`,
      // every 4th concept is ungrounded; every 7th aliases another QID
      wikidata_qid: c % 4 === 3 ? null : `Q${(c % 7 === 6 ? c - 1 : c) + 1000}`,
    }));
    const rows = [];
    for (let f = 1; f <= files; f += 1) {
      const seen = new Set();
      for (let e = 0; e < edgesPerFile; e += 1) {
        const conceptId = (next() % conceptsPerAxis) + 1;
        if (seen.has(conceptId)) continue;
        seen.add(conceptId);
        rows.push({ file_id: f, concept_id: conceptId, confidence: (next() % 100) / 100 });
      }
    }
    edges[edgeTable] = rows;
  }
  return { files: fileRows, concepts, edges };
}
