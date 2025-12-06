# How-to: Customize Graph Layouts

**Goal:** Configure the appearance and behavior of knowledge graph visualizations.

**Time:** 10-15 minutes

---

## Choose a Layout Algorithm

The `KnowledgeGraph` component supports multiple layout algorithms:

```jsx
import { KnowledgeGraph } from 'unrdf-react/components';

function MyGraph({ triples }) {
  return (
    <KnowledgeGraph
      triples={triples}
      layout="cola"  // Options: cola, cose, circle, grid, breadthfirst, concentric
    />
  );
}
```

**Layout comparison:**

| Layout | Best For | Performance |
|--------|----------|-------------|
| `cola` | Force-directed, general purpose | Medium |
| `cose` | Force-directed, clean separation | Medium |
| `circle` | Simple overview, fixed positions | Fast |
| `grid` | Organized, predictable layout | Fast |
| `breadthfirst` | Hierarchical data, trees | Fast |
| `concentric` | Importance-based rings | Fast |

---

## Custom Node Styling

Configure node appearance with `styleOptions`:

```jsx
<KnowledgeGraph
  triples={triples}
  styleOptions={{
    // Node color - can be function or string
    nodeColor: (node) => {
      if (node.data.type === 'Person') return '#3b82f6';
      if (node.data.type === 'Organization') return '#10b981';
      return '#6b7280';
    },

    // Node size - can be function or number
    nodeSize: (node) => {
      const connections = node.degree() || 0;
      return 30 + (connections * 3);
    },

    // Node shape
    nodeShape: 'ellipse', // Options: ellipse, triangle, square, roundrectangle, etc.

    // Node border
    nodeBorderWidth: 2,
    nodeBorderColor: '#1e293b',

    // Node label
    labelSize: 12,
    labelColor: '#334155',
    labelPosition: 'center', // center, top, bottom, left, right
  }}
/>
```

**Result:** Nodes styled by type with size based on connections.

---

## Custom Edge Styling

Configure edge (relationship) appearance:

```jsx
<KnowledgeGraph
  triples={triples}
  styleOptions={{
    // Edge color
    edgeColor: (edge) => {
      if (edge.data.predicate.includes('knows')) return '#3b82f6';
      if (edge.data.predicate.includes('worksAt')) return '#10b981';
      return '#94a3b8';
    },

    // Edge width
    edgeWidth: 2,

    // Edge style
    edgeStyle: 'solid', // solid, dotted, dashed

    // Edge arrows
    targetArrowShape: 'triangle', // triangle, circle, diamond, none
    sourceArrowShape: 'none',

    // Curve style
    curveStyle: 'bezier', // bezier, straight, haystack, segments

    // Edge label
    edgeLabel: (edge) => {
      // Show last part of predicate URI
      return edge.data.predicate.split('/').pop();
    },
    edgeLabelSize: 10,
    edgeLabelColor: '#64748b',
  }}
/>
```

**Result:** Edges colored by relationship type with visible labels.

---

## Interactive Configuration

Add controls to change layout dynamically:

```jsx
import { useState } from 'react';

function InteractiveGraph({ triples }) {
  const [layout, setLayout] = useState('cola');
  const [nodeSize, setNodeSize] = useState(40);

  return (
    <div>
      {/* Controls */}
      <div className="controls">
        <select value={layout} onChange={(e) => setLayout(e.target.value)}>
          <option value="cola">Cola (Force)</option>
          <option value="cose">Cose (Force)</option>
          <option value="circle">Circle</option>
          <option value="grid">Grid</option>
          <option value="breadthfirst">Breadth First</option>
        </select>

        <label>
          Node Size:
          <input
            type="range"
            min="20"
            max="80"
            value={nodeSize}
            onChange={(e) => setNodeSize(Number(e.target.value))}
          />
        </label>
      </div>

      {/* Graph */}
      <KnowledgeGraph
        triples={triples}
        layout={layout}
        styleOptions={{
          nodeSize,
          // ... other styles
        }}
      />
    </div>
  );
}
```

**Result:** Users can change layout and size in real-time.

---

## Layout-Specific Options

Configure algorithm-specific parameters:

```jsx
<KnowledgeGraph
  triples={triples}
  layout="cola"
  layoutOptions={{
    // Cola-specific options
    animate: true,
    animationDuration: 1000,
    nodeSpacing: 50,
    edgeLength: 100,
    convergenceThreshold: 0.01,
  }}
/>

// Or for breadthfirst layout:
<KnowledgeGraph
  triples={triples}
  layout="breadthfirst"
  layoutOptions={{
    directed: true,
    spacingFactor: 1.5,
    avoidOverlap: true,
    roots: ['#node-id-1'], // Start from specific nodes
  }}
/>
```

**Result:** Fine-tuned layout behavior.

---

## Conditional Styling

Style nodes based on their properties:

```jsx
<KnowledgeGraph
  triples={triples}
  styleOptions={{
    nodeColor: (node) => {
      // Highlight important nodes
      if (node.data.important) return '#ef4444';

      // Dim nodes without connections
      if (node.degree() === 0) return '#d1d5db';

      // Color by community/cluster
      return node.data.cluster
        ? `hsl(${node.data.cluster * 137.5}, 70%, 50%)`
        : '#6b7280';
    },

    nodeOpacity: (node) => {
      // Fade distant nodes
      return node.data.distance > 2 ? 0.3 : 1.0;
    },
  }}
/>
```

**Result:** Visual hierarchy based on node properties.

---

## Highlighting and Selection

Highlight nodes on interaction:

```jsx
import { useState } from 'react';

function HighlightableGraph({ triples }) {
  const [selectedNodeId, setSelectedNodeId] = useState(null);
  const [hoveredNodeId, setHoveredNodeId] = useState(null);

  return (
    <KnowledgeGraph
      triples={triples}
      onNodeClick={(node) => setSelectedNodeId(node.id)}
      onNodeHover={(node) => setHoveredNodeId(node?.id)}
      styleOptions={{
        nodeColor: (node) => {
          if (node.id === selectedNodeId) return '#ef4444'; // Red for selected
          if (node.id === hoveredNodeId) return '#f59e0b'; // Orange for hovered
          return '#3b82f6'; // Blue default
        },
        nodeBorderWidth: (node) => {
          return node.id === selectedNodeId ? 4 : 2;
        },
      }}
    />
  );
}
```

**Result:** Visual feedback on user interaction.

---

## Performance Optimization

For large graphs (1000+ nodes):

```jsx
<KnowledgeGraph
  triples={triples}
  layout="grid" // Faster than force-directed
  styleOptions={{
    // Use static values instead of functions when possible
    nodeColor: '#3b82f6',
    nodeSize: 30,
    edgeWidth: 1,

    // Disable expensive features
    hideEdgeLabels: true,
    hideNodeLabels: false,
  }}

  // Reduce rendering quality for performance
  renderingQuality="low" // Options: low, medium, high

  // Enable virtualization for very large graphs
  virtualizeEdges={true}
/>
```

**Result:** Smooth performance with thousands of nodes.

---

## Responsive Sizing

Make graph adapt to container size:

```jsx
function ResponsiveGraph({ triples }) {
  return (
    <div className="w-full h-screen">
      <KnowledgeGraph
        triples={triples}
        height="100%"
        width="100%"
        fit={true} // Auto-fit to container
        padding={50} // Padding around graph
      />
    </div>
  );
}
```

**Result:** Graph scales with window/container size.

---

## Export Graph as Image

Add export functionality:

```jsx
import { useRef } from 'react';
import { KnowledgeGraph } from 'unrdf-react/components';

function ExportableGraph({ triples }) {
  const graphRef = useRef();

  const exportPNG = () => {
    if (graphRef.current) {
      const png = graphRef.current.exportImage({
        format: 'png',
        scale: 2,
        backgroundColor: '#ffffff'
      });

      // Download
      const link = document.createElement('a');
      link.href = png;
      link.download = 'knowledge-graph.png';
      link.click();
    }
  };

  return (
    <div>
      <button onClick={exportPNG}>Export as PNG</button>
      <KnowledgeGraph
        ref={graphRef}
        triples={triples}
      />
    </div>
  );
}
```

**Result:** Users can download graph as image.

---

## Complete Example

Here's a full example with all customizations:

```jsx
import { useState, useRef } from 'react';
import { KnowledgeGraph } from 'unrdf-react/components';

function FullyCustomizedGraph({ triples }) {
  const [layout, setLayout] = useState('cola');
  const [selectedNode, setSelectedNode] = useState(null);
  const graphRef = useRef();

  return (
    <div className="h-screen flex flex-col">
      {/* Toolbar */}
      <div className="p-4 bg-gray-100 flex gap-4">
        <select value={layout} onChange={(e) => setLayout(e.target.value)}>
          <option value="cola">Force (Cola)</option>
          <option value="circle">Circle</option>
          <option value="grid">Grid</option>
        </select>

        <button onClick={() => graphRef.current?.fit()}>
          Fit to Screen
        </button>

        <button onClick={() => {
          const png = graphRef.current?.exportImage({ format: 'png' });
          if (png) {
            const link = document.createElement('a');
            link.href = png;
            link.download = 'graph.png';
            link.click();
          }
        }}>
          Export PNG
        </button>
      </div>

      {/* Graph */}
      <div className="flex-1">
        <KnowledgeGraph
          ref={graphRef}
          triples={triples}
          layout={layout}
          height="100%"
          onNodeClick={(node) => setSelectedNode(node)}
          styleOptions={{
            nodeColor: (node) => {
              if (node.id === selectedNode?.id) return '#ef4444';
              if (node.data.type === 'Person') return '#3b82f6';
              return '#10b981';
            },
            nodeSize: (node) => 30 + (node.degree() * 5),
            nodeShape: 'roundrectangle',
            labelSize: 12,
            edgeColor: '#94a3b8',
            edgeWidth: 2,
          }}
          layoutOptions={{
            animate: true,
            animationDuration: 500,
          }}
        />
      </div>

      {/* Details Panel */}
      {selectedNode && (
        <div className="p-4 bg-gray-50 border-t">
          <h3 className="font-bold">Selected Node</h3>
          <p className="font-mono text-sm">{selectedNode.id}</p>
          <p>Connections: {selectedNode.degree()}</p>
        </div>
      )}
    </div>
  );
}
```

---

## Related

- [Reference: KnowledgeGraph Component](../reference/components.md#knowledgegraph)
- [Tutorial: Knowledge Graph Explorer](../tutorials/02-knowledge-graph-explorer.md)
- [Explanation: Graph Visualization Principles](../explanation/visualization-principles.md)
