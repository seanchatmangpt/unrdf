# Reference: Visualization Components

Complete API reference for UNRDF React visualization components.

**Package:** `unrdf-react/components`

---

## KnowledgeGraph

Interactive RDF graph visualization using Cytoscape.

### Import

```jsx
import { KnowledgeGraph } from 'unrdf-react/components';
```

### Signature

```typescript
function KnowledgeGraph(props: KnowledgeGraphProps): JSX.Element
```

### Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `triples` | `Triple[]` | **required** | RDF triples to visualize |
| `layout` | `LayoutName` | `'cola'` | Layout algorithm |
| `height` | `string \| number` | `'600px'` | Component height |
| `width` | `string \| number` | `'100%'` | Component width |
| `styleOptions` | `StyleOptions` | `{}` | Visual styling configuration |
| `layoutOptions` | `LayoutOptions` | `{}` | Layout-specific options |
| `onNodeClick` | `(node) => void` | `undefined` | Node click handler |
| `onEdgeClick` | `(edge) => void` | `undefined` | Edge click handler |
| `onNodeHover` | `(node) => void` | `undefined` | Node hover handler |
| `fit` | `boolean` | `true` | Auto-fit graph to container |
| `padding` | `number` | `50` | Padding around graph |
| `animateOnLoad` | `boolean` | `false` | Animate initial layout |
| `animationDuration` | `number` | `1000` | Animation duration (ms) |

### Layout Algorithms

| Name | Description | Performance | Best For |
|------|-------------|-------------|----------|
| `'cola'` | Force-directed with constraints | Medium | General purpose |
| `'cose'` | Force-directed (CoSE algorithm) | Medium | Clean separation |
| `'circle'` | Circular layout | Fast | Overview, fixed positions |
| `'grid'` | Grid layout | Fast | Organized display |
| `'breadthfirst'` | Hierarchical breadth-first | Fast | Tree structures |
| `'concentric'` | Concentric circles | Fast | Importance-based |

### StyleOptions

```typescript
interface StyleOptions {
  // Node styling
  nodeColor?: string | ((node: Node) => string);
  nodeSize?: number | ((node: Node) => number);
  nodeShape?: NodeShape;
  nodeBorderWidth?: number;
  nodeBorderColor?: string | ((node: Node) => string);
  nodeOpacity?: number | ((node: Node) => number);

  // Node labels
  labelSize?: number;
  labelColor?: string;
  labelPosition?: 'center' | 'top' | 'bottom' | 'left' | 'right';
  hideNodeLabels?: boolean;

  // Edge styling
  edgeColor?: string | ((edge: Edge) => string);
  edgeWidth?: number | ((edge: Edge) => number);
  edgeStyle?: 'solid' | 'dotted' | 'dashed';
  edgeOpacity?: number | ((edge: Edge) => number);

  // Edge arrows
  targetArrowShape?: ArrowShape;
  sourceArrowShape?: ArrowShape;

  // Edge labels
  edgeLabel?: string | ((edge: Edge) => string);
  edgeLabelSize?: number;
  edgeLabelColor?: string;
  hideEdgeLabels?: boolean;

  // Curve style
  curveStyle?: 'bezier' | 'straight' | 'haystack' | 'segments';
}
```

**NodeShape:** `'ellipse' | 'triangle' | 'rectangle' | 'roundrectangle' | 'diamond' | 'pentagon' | 'hexagon' | 'octagon' | 'star'`

**ArrowShape:** `'triangle' | 'triangle-tee' | 'circle-triangle' | 'triangle-cross' | 'triangle-backcurve' | 'vee' | 'tee' | 'square' | 'circle' | 'diamond' | 'none'`

### LayoutOptions

Options vary by layout algorithm. Common options:

```typescript
interface LayoutOptions {
  // Animation
  animate?: boolean;
  animationDuration?: number;

  // Spacing
  nodeSpacing?: number;
  edgeLength?: number;

  // Algorithm-specific
  directed?: boolean;           // breadthfirst
  spacingFactor?: number;       // most layouts
  avoidOverlap?: boolean;       // cola, cose
  convergenceThreshold?: number; // cola
  roots?: string[];             // breadthfirst
}
```

### Methods (via ref)

```typescript
interface KnowledgeGraphRef {
  fit: () => void;
  center: () => void;
  zoom: (level: number) => void;
  exportImage: (options: ExportOptions) => string;
  layout: (algorithm: LayoutName) => void;
}
```

### Example

```jsx
import { KnowledgeGraph } from 'unrdf-react/components';
import { useRef } from 'react';

function GraphViewer({ triples }) {
  const graphRef = useRef();

  return (
    <div>
      <KnowledgeGraph
        ref={graphRef}
        triples={triples}
        layout="cola"
        height="600px"
        styleOptions={{
          nodeColor: (node) => {
            if (node.data.type === 'Person') return '#3b82f6';
            return '#10b981';
          },
          nodeSize: 40,
          labelSize: 12,
          edgeColor: '#94a3b8',
        }}
        onNodeClick={(node) => console.log('Clicked:', node.id)}
      />

      <button onClick={() => graphRef.current.fit()}>
        Fit to Screen
      </button>
    </div>
  );
}
```

---

## EntropyCascadeVisualization

Visualize entropy cascade in RDF transformations.

### Import

```jsx
import { EntropyCascadeVisualization } from 'unrdf-react/components';
```

### Props

| Prop | Type | Description |
|------|------|-------------|
| `data` | `EntropyData[]` | Entropy measurements |
| `height` | `number` | Component height |
| `width` | `number` | Component width |

### Example

```jsx
import { EntropyCascadeVisualization } from 'unrdf-react/components';

function EntropyView() {
  const data = [
    { level: 0, entropy: 2.5, label: 'Input' },
    { level: 1, entropy: 1.8, label: 'Transform' },
    { level: 2, entropy: 1.2, label: 'Output' },
  ];

  return <EntropyCascadeVisualization data={data} height={400} />;
}
```

---

## GeospatialVisualization

3D geospatial visualization of RDF data using deck.gl.

### Import

```jsx
import { GeospatialVisualization } from 'unrdf-react/components';
```

### Props

| Prop | Type | Description |
|------|------|-------------|
| `locations` | `GeoLocation[]` | Geographic data points |
| `initialViewState` | `ViewState` | Initial camera position |
| `height` | `string \| number` | Component height |

### Example

```jsx
import { GeospatialVisualization } from 'unrdf-react/components';

function MapView() {
  const locations = [
    { lat: 37.7749, lng: -122.4194, label: 'San Francisco' },
    { lat: 40.7128, lng: -74.0060, label: 'New York' },
  ];

  return (
    <GeospatialVisualization
      locations={locations}
      initialViewState={{
        latitude: 39.8283,
        longitude: -98.5795,
        zoom: 3,
      }}
      height="600px"
    />
  );
}
```

---

## PerformanceMetricsChart

Chart.js-based performance metrics visualization.

### Import

```jsx
import { PerformanceMetricsChart } from 'unrdf-react/components';
```

### Props

| Prop | Type | Description |
|------|------|-------------|
| `metrics` | `Metric[]` | Performance data |
| `type` | `'line' \| 'bar' \| 'radar'` | Chart type |
| `height` | `number` | Chart height |

### Example

```jsx
import { PerformanceMetricsChart } from 'unrdf-react/components';

function Performance() {
  const metrics = [
    { label: 'Query Time', value: 12.5, unit: 'ms' },
    { label: 'Memory', value: 45.2, unit: 'MB' },
  ];

  return (
    <PerformanceMetricsChart
      metrics={metrics}
      type="bar"
      height={300}
    />
  );
}
```

---

## PlantUMLViewer

Render PlantUML diagrams from RDF data.

### Import

```jsx
import { PlantUMLViewer } from 'unrdf-react/components';
```

### Props

| Prop | Type | Description |
|------|------|-------------|
| `diagram` | `string` | PlantUML diagram code |
| `format` | `'svg' \| 'png'` | Output format |

### Example

```jsx
import { PlantUMLViewer } from 'unrdf-react/components';

function DiagramView() {
  const diagram = `
    @startuml
    Alice -> Bob: Hello
    Bob -> Alice: Hi there
    @enduml
  `;

  return <PlantUMLViewer diagram={diagram} format="svg" />;
}
```

---

## FMEADashboard

Failure Mode and Effects Analysis dashboard.

### Import

```jsx
import { FMEADashboard } from 'unrdf-react/components';
```

### Props

| Prop | Type | Description |
|------|------|-------------|
| `failures` | `FailureMode[]` | FMEA data |
| `onUpdate` | `(failure) => void` | Update handler |

### Example

```jsx
import { FMEADashboard } from 'unrdf-react/components';

function QualityView() {
  const failures = [
    {
      id: '1',
      mode: 'Query timeout',
      severity: 8,
      occurrence: 3,
      detection: 6,
    },
  ];

  return (
    <FMEADashboard
      failures={failures}
      onUpdate={(f) => console.log('Updated:', f)}
    />
  );
}
```

---

## InformationFlowAnalysis

Analyze and visualize information flow through RDF graph.

### Import

```jsx
import { InformationFlowAnalysis } from 'unrdf-react/components';
```

### Props

| Prop | Type | Description |
|------|------|-------------|
| `flows` | `FlowData[]` | Information flows |
| `triples` | `Triple[]` | RDF graph data |

### Example

```jsx
import { InformationFlowAnalysis } from 'unrdf-react/components';

function FlowView({ triples }) {
  const flows = [
    { from: 'input', to: 'process', volume: 100 },
    { from: 'process', to: 'output', volume: 95 },
  ];

  return <InformationFlowAnalysis flows={flows} triples={triples} />;
}
```

---

## Best Practices

1. **Lazy load large visualizations:**
   ```jsx
   import dynamic from 'next/dynamic';

   const KnowledgeGraph = dynamic(
     () => import('unrdf-react/components').then(m => m.KnowledgeGraph),
     { ssr: false }
   );
   ```

2. **Memoize style functions:**
   ```jsx
   const styleOptions = useMemo(() => ({
     nodeColor: (node) => getColorForType(node.data.type),
   }), []);
   ```

3. **Handle empty data:**
   ```jsx
   {triples.length > 0 ? (
     <KnowledgeGraph triples={triples} />
   ) : (
     <EmptyState />
   )}
   ```

4. **Responsive sizing:**
   ```jsx
   <div className="h-screen w-full">
     <KnowledgeGraph height="100%" width="100%" />
   </div>
   ```

---

## Related

- [Tutorial: Knowledge Graph Explorer](../tutorials/02-knowledge-graph-explorer.md)
- [How-to: Customize Graph Layouts](../how-to/customize-graph-layouts.md)
- [Explanation: Visualization Architecture](../explanation/visualization-architecture.md)
