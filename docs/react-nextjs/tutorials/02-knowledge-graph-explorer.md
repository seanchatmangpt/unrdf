# Tutorial: Building a Knowledge Graph Explorer

**Learning Objectives:**
- Visualize RDF graphs with Cytoscape
- Navigate graph relationships
- Filter and search graph data
- Use the KnowledgeGraph component

**Prerequisites:**
- Completed [Tutorial 1: Getting Started](./01-getting-started.md)
- 20 minutes

---

## Step 1: Install Visualization Dependencies

The UNRDF React package includes visualization components, but you need to ensure all peer dependencies are installed:

```bash
npm install react-cytoscapejs cytoscape cytoscape-cola
```

**What you just did:** Installed Cytoscape, a powerful graph visualization library used by the `KnowledgeGraph` component.

---

## Step 2: Create Sample Data

Let's create a more interesting knowledge graph with relationships. Create `app/data/sample-graph.js`:

```javascript
export const sampleGraphData = [
  // Alice
  {
    subject: 'http://example.org/people/alice',
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://xmlns.com/foaf/0.1/Person'
  },
  {
    subject: 'http://example.org/people/alice',
    predicate: 'http://xmlns.com/foaf/0.1/name',
    object: 'Alice Johnson'
  },
  {
    subject: 'http://example.org/people/alice',
    predicate: 'http://xmlns.com/foaf/0.1/knows',
    object: 'http://example.org/people/bob'
  },
  {
    subject: 'http://example.org/people/alice',
    predicate: 'http://example.org/worksAt',
    object: 'http://example.org/orgs/acme'
  },

  // Bob
  {
    subject: 'http://example.org/people/bob',
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://xmlns.com/foaf/0.1/Person'
  },
  {
    subject: 'http://example.org/people/bob',
    predicate: 'http://xmlns.com/foaf/0.1/name',
    object: 'Bob Smith'
  },
  {
    subject: 'http://example.org/people/bob',
    predicate: 'http://xmlns.com/foaf/0.1/knows',
    object: 'http://example.org/people/carol'
  },
  {
    subject: 'http://example.org/people/bob',
    predicate: 'http://example.org/worksAt',
    object: 'http://example.org/orgs/acme'
  },

  // Carol
  {
    subject: 'http://example.org/people/carol',
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://xmlns.com/foaf/0.1/Person'
  },
  {
    subject: 'http://example.org/people/carol',
    predicate: 'http://xmlns.com/foaf/0.1/name',
    object: 'Carol Davis'
  },
  {
    subject: 'http://example.org/people/carol',
    predicate: 'http://example.org/worksAt',
    object: 'http://example.org/orgs/techcorp'
  },

  // Organizations
  {
    subject: 'http://example.org/orgs/acme',
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://schema.org/Organization'
  },
  {
    subject: 'http://example.org/orgs/acme',
    predicate: 'http://schema.org/name',
    object: 'Acme Corporation'
  },
  {
    subject: 'http://example.org/orgs/techcorp',
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://schema.org/Organization'
  },
  {
    subject: 'http://example.org/orgs/techcorp',
    predicate: 'http://schema.org/name',
    object: 'TechCorp Inc'
  }
];
```

**What you just did:** Created a sample knowledge graph with people, their relationships (knows), and organizational affiliations (worksAt).

---

## Step 3: Build the Graph Explorer Component

Create `app/components/GraphExplorer.jsx`:

```jsx
'use client';

import { useKnowledgeEngine, useTriples } from 'unrdf-react';
import { KnowledgeGraph } from 'unrdf-react/components';
import { useEffect, useState } from 'react';
import { sampleGraphData } from '../data/sample-graph';

export default function GraphExplorer() {
  const { engine, ready } = useKnowledgeEngine();
  const { data: allTriples, isLoading } = useTriples();
  const [selectedNode, setSelectedNode] = useState(null);
  const [filter, setFilter] = useState('all');

  // Load sample data on mount
  useEffect(() => {
    if (ready && allTriples?.length === 0) {
      engine.addTriples(sampleGraphData);
    }
  }, [ready, engine, allTriples]);

  // Filter triples based on selected type
  const filteredTriples = allTriples?.filter(triple => {
    if (filter === 'all') return true;
    if (filter === 'people') {
      return triple.object === 'http://xmlns.com/foaf/0.1/Person' ||
             triple.subject.includes('/people/');
    }
    if (filter === 'orgs') {
      return triple.object === 'http://schema.org/Organization' ||
             triple.subject.includes('/orgs/');
    }
    return true;
  }) || [];

  // Handle node selection
  const handleNodeClick = (nodeData) => {
    setSelectedNode(nodeData);
  };

  // Get details for selected node
  const getNodeDetails = () => {
    if (!selectedNode) return null;

    return allTriples?.filter(
      t => t.subject === selectedNode.id
    ) || [];
  };

  if (!ready || isLoading) {
    return <div className="p-6">Loading knowledge graph...</div>;
  }

  return (
    <div className="h-screen flex">
      {/* Sidebar - Filters and Details */}
      <div className="w-80 bg-gray-50 p-6 overflow-y-auto border-r">
        <h1 className="text-2xl font-bold mb-6">Graph Explorer</h1>

        {/* Filters */}
        <div className="mb-8">
          <h2 className="text-lg font-semibold mb-3">Filter</h2>
          <div className="space-y-2">
            <button
              onClick={() => setFilter('all')}
              className={`w-full text-left px-3 py-2 rounded ${
                filter === 'all' ? 'bg-blue-500 text-white' : 'bg-white'
              }`}
            >
              All Nodes ({allTriples?.length || 0} triples)
            </button>
            <button
              onClick={() => setFilter('people')}
              className={`w-full text-left px-3 py-2 rounded ${
                filter === 'people' ? 'bg-blue-500 text-white' : 'bg-white'
              }`}
            >
              People Only
            </button>
            <button
              onClick={() => setFilter('orgs')}
              className={`w-full text-left px-3 py-2 rounded ${
                filter === 'orgs' ? 'bg-blue-500 text-white' : 'bg-white'
              }`}
            >
              Organizations Only
            </button>
          </div>
        </div>

        {/* Selected Node Details */}
        {selectedNode && (
          <div className="mb-8">
            <h2 className="text-lg font-semibold mb-3">Node Details</h2>
            <div className="bg-white p-4 rounded shadow">
              <p className="font-mono text-xs text-gray-600 mb-3 break-all">
                {selectedNode.id}
              </p>
              <div className="space-y-2">
                {getNodeDetails()?.map((triple, idx) => (
                  <div key={idx} className="border-l-2 border-blue-500 pl-3">
                    <p className="text-xs text-gray-500 font-mono truncate">
                      {triple.predicate.split('/').pop()}
                    </p>
                    <p className="text-sm break-all">
                      {triple.object}
                    </p>
                  </div>
                ))}
              </div>
            </div>
          </div>
        )}

        {/* Graph Stats */}
        <div className="bg-white p-4 rounded shadow">
          <h2 className="text-lg font-semibold mb-3">Statistics</h2>
          <div className="space-y-1 text-sm">
            <p>Total Triples: <strong>{allTriples?.length || 0}</strong></p>
            <p>Filtered: <strong>{filteredTriples.length}</strong></p>
            <p>Selected: <strong>{selectedNode ? '1 node' : 'None'}</strong></p>
          </div>
        </div>
      </div>

      {/* Main Graph Visualization */}
      <div className="flex-1 p-6">
        <div className="h-full border-2 border-gray-300 rounded-lg overflow-hidden">
          <KnowledgeGraph
            triples={filteredTriples}
            onNodeClick={handleNodeClick}
            layout="cola"
            height="100%"
            styleOptions={{
              nodeColor: (node) => {
                if (node.data.type?.includes('Person')) return '#3b82f6';
                if (node.data.type?.includes('Organization')) return '#10b981';
                return '#6b7280';
              },
              nodeSize: 40,
              edgeColor: '#94a3b8',
              labelSize: 12
            }}
          />
        </div>
      </div>
    </div>
  );
}
```

**What you just did:** Created a graph explorer with:
- **Interactive visualization** using the `KnowledgeGraph` component
- **Filtering** to show specific node types
- **Node selection** to view details
- **Real-time stats** showing graph metrics

---

## Step 4: Update Your App

Update `app/page.jsx`:

```jsx
import GraphExplorer from './components/GraphExplorer';

export default function Home() {
  return <GraphExplorer />;
}
```

---

## Step 5: Run and Explore

```bash
npm run dev
```

**What you should see:**
- A split-screen layout with sidebar and graph visualization
- Nodes representing people (blue) and organizations (green)
- Edges showing relationships
- Click nodes to see their properties
- Filter buttons to show/hide different entity types

---

## Step 6: Customize the Visualization

Try these customizations in the `KnowledgeGraph` component:

### Change Layout Algorithm

```jsx
<KnowledgeGraph
  layout="cose"  // Options: 'cola', 'cose', 'circle', 'grid', 'breadthfirst'
  // ... other props
/>
```

### Custom Node Styling

```jsx
<KnowledgeGraph
  styleOptions={{
    nodeColor: (node) => {
      // Custom color logic
      if (node.data.label?.includes('Alice')) return '#f59e0b';
      return '#3b82f6';
    },
    nodeSize: (node) => {
      // Size based on number of connections
      return 30 + (node.data.connections?.length || 0) * 5;
    },
    nodeShape: 'roundrectangle', // 'ellipse', 'triangle', 'square', etc.
    edgeWidth: 2,
    edgeStyle: 'solid', // 'solid', 'dotted', 'dashed'
  }}
/>
```

### Add Animation

```jsx
<KnowledgeGraph
  animateOnLoad={true}
  animationDuration={1000}
  // ... other props
/>
```

---

## What You Learned

✅ **Visualized RDF graphs** - Used the `KnowledgeGraph` component
✅ **Interactive exploration** - Implemented node selection and filtering
✅ **Custom styling** - Configured colors, sizes, and layouts
✅ **Real-time updates** - Graph automatically updates when data changes

---

## Try It Yourself

Extend the explorer with these features:

1. **Search functionality:** Add a search box to find nodes by name
2. **Expand/collapse:** Click a node to expand its connections
3. **Different layouts:** Add buttons to switch between layout algorithms
4. **Export:** Add a button to export the graph as an image
5. **Path finding:** Highlight the shortest path between two selected nodes

---

## What's Next?

- [Tutorial 3: Real-time Collaboration with Streaming](./03-real-time-streaming.md)
- [Tutorial 4: Advanced Visualizations](./04-advanced-visualizations.md)
- [How-to Guide: Customize Graph Layouts](../how-to/customize-graph-layouts.md)
- [Reference: KnowledgeGraph Component](../reference/components.md#knowledgegraph)

---

## Troubleshooting

**Graph not rendering:**
- Check browser console for Cytoscape errors
- Ensure all peer dependencies are installed
- Verify `triples` prop is not empty

**Performance issues with large graphs:**
- Use filtering to reduce visible nodes
- Consider pagination with `useTriplesPaginated`
- Use simpler layout algorithms (grid, circle)

**Styling not applying:**
- Check that `styleOptions` prop is correctly formatted
- Verify color values are valid CSS colors
- Use browser DevTools to inspect Cytoscape canvas
