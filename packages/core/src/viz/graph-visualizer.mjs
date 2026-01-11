/**
 * @file RDF Graph Visualization - DOT, Mermaid, ASCII, HTML/D3.js export
 * @module @unrdf/core/viz/graph-visualizer
 * @description Pure function graph visualizers for RDF graphs
 */

import { z } from 'zod';

/**
 * Visualization options schema
 */
const VisualizationOptionsSchema = z.object({
  limit: z.number().int().positive().optional(),
  includeNamespaces: z.boolean().optional(),
  direction: z.enum(['TB', 'LR', 'BT', 'RL']).optional(),
  nodeShape: z.enum(['box', 'ellipse', 'circle', 'diamond']).optional(),
  theme: z.enum(['light', 'dark', 'neutral']).optional(),
  width: z.number().positive().optional(),
  height: z.number().positive().optional(),
}).optional();

/**
 * Subgraph extraction options schema
 */
const SubgraphOptionsSchema = z.object({
  subject: z.string().optional(),
  predicate: z.string().optional(),
  depth: z.number().int().positive().optional(),
  maxTriples: z.number().int().positive().optional(),
});

/**
 * Shorten IRI to prefix notation
 * @param {string} iri - Full IRI
 * @param {Object} prefixes - Prefix mapping
 * @returns {string} Shortened IRI
 */
function shortenIRI(iri, prefixes = {}) {
  for (const [prefix, namespace] of Object.entries(prefixes)) {
    if (iri.startsWith(namespace)) {
      return `${prefix}:${iri.slice(namespace.length)}`;
    }
  }
  return iri;
}

/**
 * Extract quads from store
 * @param {Object} store - RDF store
 * @param {Object} [options] - Extraction options
 * @returns {Array} Array of quads
 */
function extractQuads(store, options = {}) {
  const quads = [];
  const limit = options.limit || Infinity;

  for (const quad of store.match(null, null, null, null)) {
    if (quads.length >= limit) break;
    quads.push(quad);
  }

  return quads;
}

/**
 * Export RDF graph to DOT format (Graphviz)
 * @param {Object} store - RDF store
 * @param {Object} [options] - Visualization options
 * @returns {string} DOT format representation
 *
 * @example
 * const dot = toDOT(store, { limit: 100, nodeShape: 'ellipse' });
 * console.log(dot);
 */
export function toDOT(store, options = {}) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const opts = VisualizationOptionsSchema.parse(options);
  const quads = extractQuads(store, { limit: opts?.limit });

  const prefixes = {
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
    foaf: 'http://xmlns.com/foaf/0.1/',
  };

  let dot = 'digraph RDF {\n';
  dot += '  rankdir=' + (opts?.direction || 'TB') + ';\n';
  dot += '  node [shape=' + (opts?.nodeShape || 'ellipse') + '];\n\n';

  const nodes = new Set();
  const edges = [];

  for (const quad of quads) {
    const subj = shortenIRI(quad.subject.value, prefixes);
    const pred = shortenIRI(quad.predicate.value, prefixes);
    const obj = quad.object.termType === 'Literal'
      ? `"${quad.object.value.replace(/"/g, '\\"')}"`
      : shortenIRI(quad.object.value, prefixes);

    nodes.add(subj);
    if (quad.object.termType !== 'Literal') {
      nodes.add(obj);
    }

    edges.push(`  "${subj}" -> "${obj}" [label="${pred}"];`);
  }

  for (const node of nodes) {
    dot += `  "${node}";\n`;
  }

  dot += '\n';
  for (const edge of edges) {
    dot += edge + '\n';
  }

  dot += '}\n';
  return dot;
}

/**
 * Export RDF graph to Mermaid diagram format
 * @param {Object} store - RDF store
 * @param {Object} [options] - Visualization options
 * @returns {string} Mermaid diagram representation
 *
 * @example
 * const mermaid = toMermaid(store, { direction: 'LR' });
 * console.log(mermaid);
 */
export function toMermaid(store, options = {}) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const opts = VisualizationOptionsSchema.parse(options);
  const quads = extractQuads(store, { limit: opts?.limit });

  const prefixes = {
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
    foaf: 'http://xmlns.com/foaf/0.1/',
  };

  let mermaid = 'graph ' + (opts?.direction || 'TB') + '\n';

  const nodeIds = new Map();
  let nodeCounter = 0;

  function getNodeId(value) {
    if (!nodeIds.has(value)) {
      nodeIds.set(value, `n${nodeCounter++}`);
    }
    return nodeIds.get(value);
  }

  for (const quad of quads) {
    const subjId = getNodeId(quad.subject.value);
    const subjLabel = shortenIRI(quad.subject.value, prefixes);
    const pred = shortenIRI(quad.predicate.value, prefixes);

    if (quad.object.termType === 'Literal') {
      const objValue = quad.object.value.replace(/"/g, "'");
      mermaid += `  ${subjId}["${subjLabel}"] -->|${pred}| lit${nodeCounter}["${objValue}"]\n`;
      nodeCounter++;
    } else {
      const objId = getNodeId(quad.object.value);
      const objLabel = shortenIRI(quad.object.value, prefixes);
      mermaid += `  ${subjId}["${subjLabel}"] -->|${pred}| ${objId}["${objLabel}"]\n`;
    }
  }

  return mermaid;
}

/**
 * Export RDF graph to ASCII art
 * @param {Object} store - RDF store
 * @param {Object} [options] - Visualization options
 * @returns {string} ASCII art representation
 *
 * @example
 * const ascii = toASCII(store, { limit: 10 });
 * console.log(ascii);
 */
export function toASCII(store, options = {}) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const opts = VisualizationOptionsSchema.parse(options);
  const quads = extractQuads(store, { limit: opts?.limit || 20 });

  const prefixes = {
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
    foaf: 'http://xmlns.com/foaf/0.1/',
  };

  let ascii = '┌─ RDF Graph ─────────────────────────────────────┐\n';

  for (let i = 0; i < quads.length; i++) {
    const quad = quads[i];
    const subj = shortenIRI(quad.subject.value, prefixes);
    const pred = shortenIRI(quad.predicate.value, prefixes);
    const obj = quad.object.termType === 'Literal'
      ? `"${quad.object.value}"`
      : shortenIRI(quad.object.value, prefixes);

    const isLast = i === quads.length - 1;
    const prefix = isLast ? '└──' : '├──';

    ascii += `${prefix} ${subj}\n`;
    ascii += `${isLast ? '   ' : '│  '} --[${pred}]--> ${obj}\n`;
  }

  ascii += '└─────────────────────────────────────────────────┘\n';
  return ascii;
}

/**
 * Generate HTML visualization with D3.js integration
 * @param {Object} store - RDF store
 * @param {Object} [options] - Visualization options
 * @returns {string} HTML with embedded D3.js visualization
 *
 * @example
 * const html = toHTML(store, { width: 800, height: 600 });
 * fs.writeFileSync('graph.html', html);
 */
export function toHTML(store, options = {}) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const opts = VisualizationOptionsSchema.parse(options);
  const quads = extractQuads(store, { limit: opts?.limit });

  const width = opts?.width || 800;
  const height = opts?.height || 600;
  const theme = opts?.theme || 'light';

  const nodes = new Map();
  const links = [];

  for (const quad of quads) {
    const subjId = quad.subject.value;
    const objId = quad.object.termType === 'Literal'
      ? `literal_${quad.object.value}`
      : quad.object.value;

    if (!nodes.has(subjId)) {
      nodes.set(subjId, { id: subjId, label: subjId, type: 'resource' });
    }
    if (!nodes.has(objId)) {
      nodes.set(objId, {
        id: objId,
        label: quad.object.value,
        type: quad.object.termType === 'Literal' ? 'literal' : 'resource'
      });
    }

    links.push({
      source: subjId,
      target: objId,
      label: quad.predicate.value,
    });
  }

  const data = {
    nodes: Array.from(nodes.values()),
    links,
  };

  const colors = theme === 'dark'
    ? { bg: '#1a1a1a', text: '#ffffff', resource: '#4CAF50', literal: '#2196F3' }
    : { bg: '#ffffff', text: '#000000', resource: '#4CAF50', literal: '#2196F3' };

  return `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>RDF Graph Visualization</title>
  <script src="https://d3.js.org/d3.v7.min.js"></script>
  <style>
    body { margin: 0; background: ${colors.bg}; font-family: Arial, sans-serif; }
    svg { border: 1px solid #ccc; }
    .links line { stroke: #999; stroke-opacity: 0.6; stroke-width: 2px; }
    .nodes circle { stroke: #fff; stroke-width: 1.5px; }
    .labels text { fill: ${colors.text}; font-size: 10px; }
  </style>
</head>
<body>
  <svg width="${width}" height="${height}"></svg>
  <script>
    const data = ${JSON.stringify(data)};
    const svg = d3.select('svg');
    const simulation = d3.forceSimulation(data.nodes)
      .force('link', d3.forceLink(data.links).id(d => d.id).distance(100))
      .force('charge', d3.forceManyBody().strength(-300))
      .force('center', d3.forceCenter(${width / 2}, ${height / 2}));

    const link = svg.append('g').attr('class', 'links')
      .selectAll('line').data(data.links).enter().append('line');

    const node = svg.append('g').attr('class', 'nodes')
      .selectAll('circle').data(data.nodes).enter().append('circle')
      .attr('r', 8)
      .attr('fill', d => d.type === 'literal' ? '${colors.literal}' : '${colors.resource}')
      .call(d3.drag()
        .on('start', dragstarted)
        .on('drag', dragged)
        .on('end', dragended));

    const label = svg.append('g').attr('class', 'labels')
      .selectAll('text').data(data.nodes).enter().append('text')
      .text(d => d.label.split('/').pop().substring(0, 20))
      .attr('font-size', 10)
      .attr('dx', 12)
      .attr('dy', 4);

    simulation.on('tick', () => {
      link.attr('x1', d => d.source.x).attr('y1', d => d.source.y)
          .attr('x2', d => d.target.x).attr('y2', d => d.target.y);
      node.attr('cx', d => d.x).attr('cy', d => d.y);
      label.attr('x', d => d.x).attr('y', d => d.y);
    });

    function dragstarted(event) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }
    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }
    function dragended(event) {
      if (!event.active) simulation.alphaTarget(0);
      event.subject.fx = null;
      event.subject.fy = null;
    }
  </script>
</body>
</html>`;
}

/**
 * Extract subgraph from store
 * @param {Object} store - RDF store
 * @param {Object} options - Subgraph options (subject, predicate, depth, maxTriples)
 * @returns {Array} Array of quads in subgraph
 *
 * @example
 * const subgraph = extractSubgraph(store, {
 *   subject: 'http://example.org/alice',
 *   depth: 2
 * });
 */
export function extractSubgraph(store, options) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const opts = SubgraphOptionsSchema.parse(options);
  const result = [];
  const visited = new Set();
  const queue = [];

  if (opts.subject) {
    queue.push({ iri: opts.subject, depth: 0 });
  }

  const maxDepth = opts.depth || 1;
  const maxTriples = opts.maxTriples || 1000;

  while (queue.length > 0 && result.length < maxTriples) {
    const { iri, depth } = queue.shift();

    if (visited.has(iri) || depth > maxDepth) continue;
    visited.add(iri);

    for (const quad of store.match({ termType: 'NamedNode', value: iri }, null, null, null)) {
      if (result.length >= maxTriples) break;
      result.push(quad);

      if (depth < maxDepth && quad.object.termType === 'NamedNode') {
        queue.push({ iri: quad.object.value, depth: depth + 1 });
      }
    }

    if (opts.predicate) {
      for (const quad of store.match(null, { termType: 'NamedNode', value: opts.predicate }, null, null)) {
        if (result.length >= maxTriples) break;
        result.push(quad);
      }
    }
  }

  return result;
}
