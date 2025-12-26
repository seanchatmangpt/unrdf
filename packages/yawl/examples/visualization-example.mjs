/**
 * Real-Time Workflow Visualization Example
 * Demonstrates live workflow visualization with D3.js
 */

import { createLiveWorkflowVisualizer, createStaticWorkflowDiagram } from '../src/visualization/live-workflow-viz.mjs';
import { createYawlEngine } from '../src/engine.mjs';
import { createWorkflow } from '../src/workflow.mjs';

/**
 * Example 1: Static workflow diagram
 */
async function staticDiagramExample() {
  console.log('\n=== Static Workflow Diagram ===\n');

  const workflowSpec = {
    id: 'loan-approval',
    name: 'Loan Approval Process',
    tasks: [
      { id: 'submit', name: 'Submit Application', type: 'ATOMIC' },
      { id: 'review', name: 'Review Application', type: 'ATOMIC' },
      { id: 'approve', name: 'Approve Loan', type: 'ATOMIC' },
      { id: 'reject', name: 'Reject Loan', type: 'ATOMIC' }
    ],
    flows: [
      { from: 'submit', to: 'review' },
      { from: 'review', to: 'approve', condition: 'approved' },
      { from: 'review', to: 'reject', condition: 'rejected' }
    ]
  };

  // Note: Requires browser environment with DOM
  // This example shows the API - actual visualization needs HTML container

  console.log('Workflow Specification:');
  console.log(JSON.stringify(workflowSpec, null, 2));

  console.log('\nTo visualize:');
  console.log('1. Create HTML container: <div id="diagram"></div>');
  console.log('2. Call: createStaticWorkflowDiagram(spec, { container: "#diagram" })');
  console.log('3. Open in browser to see visualization');
}

/**
 * Example 2: Live workflow visualizer setup
 */
async function liveVisualizerSetup() {
  console.log('\n=== Live Workflow Visualizer Setup ===\n');

  // Create YAWL engine
  const engine = await createYawlEngine({
    storeUrl: 'memory://',
    enableEvents: true
  });

  // Create workflow
  const workflow = await createWorkflow({
    id: 'order-fulfillment',
    name: 'Order Fulfillment',
    tasks: [
      { id: 'receive', name: 'Receive Order', type: 'ATOMIC' },
      { id: 'process', name: 'Process Payment', type: 'ATOMIC' },
      { id: 'ship', name: 'Ship Order', type: 'ATOMIC' },
      { id: 'complete', name: 'Complete', type: 'ATOMIC' }
    ],
    flows: [
      { from: 'receive', to: 'process' },
      { from: 'process', to: 'ship' },
      { from: 'ship', to: 'complete' }
    ]
  });

  console.log('Workflow created:', workflow.id);

  // Note: Visualizer requires browser environment
  console.log('\nBrowser setup code:');
  console.log(`
    // In browser JavaScript:
    const visualizer = createLiveWorkflowVisualizer(engine, {
      container: '#workflow-viz',
      width: 1200,
      height: 800,
      autoRefresh: true,
      refreshInterval: 1000
    });

    visualizer.start();

    // Workflow events will automatically update visualization
  `);

  console.log('\n✅ Setup complete');
}

/**
 * Example 3: Event handling and visualization updates
 */
function eventHandlingExample() {
  console.log('\n=== Event Handling Example ===\n');

  console.log('Event handler code:');
  console.log(`
    // Subscribe to workflow events
    engine.on('event', (event) => {
      console.log('Workflow event:', event);

      // Visualizer automatically handles these events:
      // - TASK_ENABLED: Node turns green
      // - TASK_STARTED: Node turns blue
      // - TASK_COMPLETED: Node turns light green
      // - TASK_FAILED: Node turns red
      // - TASK_CANCELLED: Node turns gray
    });

    // Start a case to trigger events
    const caseInstance = await engine.startCase('order-fulfillment', {
      orderId: 'ORD-001',
      customerId: 'CUST-123'
    });

    // Events will fire and visualization will update in real-time
  `);

  console.log('\n✅ Event handling configured');
}

/**
 * Example 4: Custom visualization configuration
 */
function customConfigExample() {
  console.log('\n=== Custom Visualization Configuration ===\n');

  const customConfig = {
    container: '#custom-viz',
    width: 1600,
    height: 1000,
    autoRefresh: true,
    refreshInterval: 500, // Update every 500ms
    colorScheme: 'dark',
    colors: {
      enabled: '#00ff00',
      started: '#0080ff',
      completed: '#90ee90',
      failed: '#ff4444',
      cancelled: '#888888'
    }
  };

  console.log('Custom configuration:');
  console.log(JSON.stringify(customConfig, null, 2));

  console.log('\nUsage:');
  console.log(`
    const visualizer = createLiveWorkflowVisualizer(engine, customConfig);
    visualizer.start();

    // Access current state
    const state = visualizer.getState();
    console.log('Nodes:', state.nodes);
    console.log('Edges:', state.edges);
    console.log('Event history:', state.eventHistory);

    // Export as SVG
    const svg = visualizer.exportSVG();
    // Save or display SVG
  `);

  console.log('\n✅ Custom configuration example');
}

/**
 * Example 5: Complete browser example (HTML + JS)
 */
function completeBrowserExample() {
  console.log('\n=== Complete Browser Example ===\n');

  const html = `
<!DOCTYPE html>
<html>
<head>
  <title>YAWL Workflow Visualization</title>
  <style>
    #workflow-viz {
      border: 1px solid #ccc;
      margin: 20px;
    }
    .event-timeline {
      margin: 20px;
      padding: 10px;
      background: #f5f5f5;
    }
  </style>
</head>
<body>
  <h1>Live Workflow Visualization</h1>
  <div id="workflow-viz"></div>

  <script type="module">
    import { createYawlEngine } from '@unrdf/yawl';
    import { createLiveWorkflowVisualizer } from '@unrdf/yawl/visualization';

    async function main() {
      // Create engine
      const engine = await createYawlEngine({
        storeUrl: 'memory://',
        enableEvents: true
      });

      // Create visualizer
      const visualizer = createLiveWorkflowVisualizer(engine, {
        container: '#workflow-viz',
        width: 1200,
        height: 800,
        autoRefresh: true
      });

      // Start visualization
      visualizer.start();

      // Load and start workflow
      const workflow = await engine.loadWorkflow({
        id: 'demo',
        tasks: [/* ... */],
        flows: [/* ... */]
      });

      const caseInstance = await engine.startCase('demo');

      // Events will automatically update visualization
    }

    main();
  </script>
</body>
</html>
  `;

  console.log('Complete HTML example:');
  console.log(html);

  console.log('\n✅ Browser example complete');
}

/**
 * Run all examples
 */
async function main() {
  try {
    console.log('Real-Time Workflow Visualization Examples');
    console.log('==========================================');

    staticDiagramExample();
    await liveVisualizerSetup();
    eventHandlingExample();
    customConfigExample();
    completeBrowserExample();

    console.log('\n✅ All examples completed successfully');
    console.log('\nNote: Visualization requires browser environment with DOM');
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export {
  staticDiagramExample,
  liveVisualizerSetup,
  eventHandlingExample,
  customConfigExample,
  completeBrowserExample
};
