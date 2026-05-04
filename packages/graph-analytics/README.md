# @unrdf/graph-analytics

Advanced graph analytics for RDF knowledge graphs using graphlib.

## Features

### 1. RDF to Graph Conversion
Convert RDF triple stores to graphlib Graph instances for analysis.

```javascript
import { createStore } from '@unrdf/oxigraph';
import { rdfToGraph, getGraphStats } from '@unrdf/graph-analytics';

const store = createStore();
// ... add RDF triples ...

const graph = rdfToGraph(store);
const stats = getGraphStats(graph);
console.log(`Nodes: ${stats.nodeCount}, Edges: ${stats.edgeCount}`);
```

### 2. Centrality Algorithms
Compute entity importance using PageRank and degree centrality.

```javascript
import { computePageRank, getTopNodes } from '@unrdf/graph-analytics';

const pagerank = computePageRank(graph, { dampingFactor: 0.85 });
const top10 = getTopNodes(pagerank, 10);
console.log('Most important entities:', top10);
```

### 3. Path Finding
Discover relationships through shortest paths and path enumeration.

```javascript
import { findShortestPath, findAllPaths } from '@unrdf/graph-analytics';

const shortestPath = findShortestPath(graph, sourceUri, targetUri);
console.log('Path:', shortestPath.path);

const allPaths = findAllPaths(graph, sourceUri, targetUri, { maxDepth: 4 });
console.log(`Found ${allPaths.length} paths`);
```

### 4. Community Detection
Identify clusters and communities in knowledge graphs.

```javascript
import { detectCommunitiesLPA, getCommunityStats } from '@unrdf/graph-analytics';

const communities = detectCommunitiesLPA(graph);
const stats = getCommunityStats(communities);
console.log(`Detected ${stats.totalCommunities} communities`);
```

## Installation

```bash
pnpm add @unrdf/graph-analytics
```

## Quick Start

```bash
# Run demo
pnpm demo

# Run tests
pnpm test

# Run with coverage
pnpm test -- --coverage
```

## API Reference

### Converter
- `rdfToGraph(store, options)` - Convert RDF store to graph
- `sparqlResultsToGraph(results, options)` - Convert SPARQL results to graph
- `getGraphStats(graph)` - Compute graph statistics

### Centrality
- `computePageRank(graph, options)` - PageRank algorithm
- `computeDegreeCentrality(graph)` - Degree centrality
- `computeBetweennessCentrality(graph)` - Betweenness centrality
- `getTopNodes(scores, k)` - Get top K nodes by score

### Paths
- `findShortestPath(graph, source, target)` - Dijkstra shortest path
- `findAllPaths(graph, source, target, options)` - All paths enumeration
- `findCommonNeighbors(graph, node1, node2)` - Common neighbors
- `findKHopNeighbors(graph, source, k)` - K-hop neighborhood
- `discoverRelationshipChains(graph, source, target)` - Relationship patterns

### Clustering
- `detectCommunitiesLPA(graph, options)` - Label propagation
- `detectCommunitiesModularity(graph, options)` - Modularity-based
- `findKCore(graph, k)` - K-core decomposition
- `getCommunityStats(communities)` - Community statistics

## Performance

Optimized for RDF knowledge graphs:
- Efficient graph conversion (streaming support)
- Fast PageRank convergence
- Scalable path algorithms
- Memory-efficient community detection

## License

MIT
