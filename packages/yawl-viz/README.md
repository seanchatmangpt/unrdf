# @unrdf/yawl-viz

> Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering

Interactive workflow visualization library that transforms YAWL workflow definitions into beautiful, real-time D3.js graphs with live updates from the YAWL event stream.

## Features

- **Live Event Stream**: Real-time updates via YAWL engine event subscription
- **Van der Aalst Patterns**: Visual encoding for all 20 workflow patterns
- **Interactive Graph**: Zoom, pan, drag nodes with force-directed layout
- **Pattern Recognition**: Automatic detection and visualization of split/join semantics
- **Case Instance Tracking**: Drill-down into specific case executions
- **State Highlighting**: Color-coded task states (enabled, running, completed, etc.)

## Installation

```bash
pnpm add @unrdf/yawl-viz
```

## Quick Start

```javascript
import { createWorkflowEngine } from '@unrdf/yawl';
import { YAWLVisualizer } from '@unrdf/yawl-viz';

// Create YAWL engine
const engine = createWorkflowEngine();

// Register workflow
const workflow = engine.registerWorkflow({
  id: 'approval',
  name: 'Document Approval',
});

workflow.addTask({ id: 'submit', name: 'Submit Document' });
workflow.addTask({ id: 'review', name: 'Review Document', splitType: 'and' });
workflow.addTask({ id: 'approve', name: 'Approve', joinType: 'and' });

workflow.addFlow('submit', 'review');
workflow.addFlow('review', 'approve');

workflow.setStart('submit');
workflow.setEnd(['approve']);

// Create visualizer
const viz = new YAWLVisualizer({
  engine,
  container: document.getElementById('workflow-canvas'),
  width: 1200,
  height: 800,
});

// Render workflow
viz.renderWorkflow('approval');

// Live updates are automatic via event subscription
```

## API Reference

### `YAWLVisualizer`

Main visualizer class.

#### Constructor

```javascript
new YAWLVisualizer(config)
```

**Config Options:**
- `engine` (WorkflowEngine) - YAWL engine instance **(required)**
- `container` (HTMLElement) - DOM container for SVG canvas **(required)**
- `width` (number) - Canvas width in pixels (default: 1200)
- `height` (number) - Canvas height in pixels (default: 800)
- `autoSubscribe` (boolean) - Auto-subscribe to engine events (default: true)

#### Methods

##### `renderWorkflow(workflowId)`

Render a workflow as an interactive graph.

```javascript
const graphData = viz.renderWorkflow('my-workflow');
// Returns: { nodes: [...], links: [...] }
```

##### `renderCase(caseId)`

Render a specific case instance with live work item states.

```javascript
viz.renderCase('case-123');
```

##### `subscribeToEvents()`

Manually subscribe to YAWL engine events for live updates.

```javascript
const unsubscribe = viz.subscribeToEvents();
// Later: unsubscribe();
```

##### `start()` / `stop()`

Start or stop the visualizer (manages event subscriptions).

```javascript
viz.start();  // Subscribe to events
viz.stop();   // Unsubscribe from events
```

##### `destroy()`

Clean up and remove all visualizations.

```javascript
viz.destroy();
```

##### `resetZoom()`

Reset zoom to fit all nodes.

```javascript
viz.resetZoom();
```

### Event Subscription

The visualizer automatically subscribes to these YAWL engine events:

- `task:enabled` - Task becomes available
- `task:started` - Task execution begins
- `task:completed` - Task finishes successfully
- `task:cancelled` - Task is cancelled
- `task:failed` - Task fails
- `case:created` - New case instance created
- `case:started` - Case execution begins
- `case:completed` - Case finishes
- `workflow:registered` - New workflow registered

When events are received, the visualization automatically updates task colors and states.

## Van der Aalst Pattern Visualization

Each workflow pattern has a distinct visual style:

| Pattern | WP# | Visual | Color | Shape |
|---------|-----|--------|-------|-------|
| Sequence | WP1 | ![](https://via.placeholder.com/30/4A90E2/000000?text=+) | Blue | Rectangle |
| Parallel Split | WP2 | ![](https://via.placeholder.com/30/7ED321/000000?text=+) | Green | Diamond |
| Synchronization | WP3 | ![](https://via.placeholder.com/30/7ED321/000000?text=+) | Green | Diamond |
| Exclusive Choice | WP4 | ![](https://via.placeholder.com/30/F5A623/000000?text=+) | Orange | Diamond |
| Simple Merge | WP5 | ![](https://via.placeholder.com/30/F5A623/000000?text=+) | Orange | Diamond |
| Multi-Choice | WP6 | ![](https://via.placeholder.com/30/BD10E0/000000?text=+) | Purple | Diamond |
| Structured Sync Merge | WP7 | ![](https://via.placeholder.com/30/BD10E0/000000?text=+) | Purple | Diamond |

### Task States

Real-time state highlighting:

| State | Color | Description |
|-------|-------|-------------|
| ENABLED | Yellow | Task is available but not started |
| RUNNING | Green | Task is currently executing |
| COMPLETED | Light Green | Task finished successfully |
| CANCELLED | Red | Task was cancelled |
| FAILED | Dark Red | Task execution failed |
| IDLE | Gray | Default state |

## Advanced Usage

### Custom Event Handling

```javascript
const viz = new YAWLVisualizer({
  engine,
  container: document.getElementById('canvas'),
  autoSubscribe: false,  // Disable auto-subscribe
});

// Manual subscription with custom logic
engine.on('task:completed', (event) => {
  console.log(`Task ${event.taskId} completed!`);
  viz.renderCase(event.caseId);  // Manually trigger update
});

viz.renderWorkflow('my-workflow');
```

### Multi-Instance Workflows

```javascript
// Create workflow with parallel multi-instance tasks
const workflow = engine.registerWorkflow({
  id: 'parallel-approval',
  name: 'Parallel Document Approval',
});

workflow.addTask({
  id: 'review',
  name: 'Review',
  splitType: 'and',  // Parallel split (WP2)
  joinType: 'sequence',
});

workflow.addTask({
  id: 'sync',
  name: 'Synchronize Reviews',
  splitType: 'sequence',
  joinType: 'and',  // Synchronization (WP3)
});

// Visualizer automatically detects and renders patterns
viz.renderWorkflow('parallel-approval');
```

### Case Instance Tracking

```javascript
// Create and track case execution
const { case: myCase } = await engine.createCase('approval', {
  document: 'proposal.pdf',
  submitter: 'john@example.com',
});

// Visualize this specific case
viz.renderCase(myCase.id);

// Execute tasks - visualization updates automatically
await engine.startTask(myCase.id, workItemId);
await engine.completeTask(myCase.id, workItemId, { approved: true });
```

## Examples

See the complete working example in `src/examples/approval-workflow.html`:

```bash
cd packages/yawl-viz
pnpm dev
# Open http://localhost:5173/src/examples/approval-workflow.html
```

The example demonstrates:
- Multi-instance parallel approval workflow
- Live event stream display
- Interactive controls (create case, start/complete tasks)
- Real-time state updates
- Van der Aalst pattern visualization

## Architecture

### Graph Generation

1. **Workflow to Graph**: Converts YAWL workflow definition to D3.js graph data
   - Tasks → Nodes with pattern detection
   - Flows → Links (edges)
   - Start/End → Special nodes

2. **Pattern Detection**: Analyzes task split/join types to identify Van der Aalst patterns
   - `splitType: 'and'` → Parallel Split (WP2)
   - `joinType: 'and'` → Synchronization (WP3)
   - `splitType: 'xor'` → Exclusive Choice (WP4)
   - etc.

3. **Layout Engine**: D3 force simulation positions nodes
   - Force-directed layout
   - Collision detection
   - Configurable link distance

### Real-Time Updates

```
YAWL Engine Events → Event Handlers → State Updates → Re-render
```

1. Engine emits events (task:started, task:completed, etc.)
2. Visualizer event handlers update internal state map
3. D3 transitions smoothly update node colors
4. No full re-render needed - just color transitions

## Performance

- **Tested**: Up to 100 nodes and 200 links with smooth rendering
- **Force Simulation**: Optimized with collision detection and link forces
- **Event Throttling**: Updates batched for high-frequency event streams
- **Memory**: Minimal overhead - only stores node/link references and state map

## Browser Support

- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+

Requires ES2020+ and SVG support.

## License

MIT

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

## Related

- [@unrdf/yawl](../yawl) - YAWL workflow engine
- [D3.js](https://d3js.org) - Data visualization library
- [Van der Aalst Workflow Patterns](https://www.workflowpatterns.com/) - Pattern reference
