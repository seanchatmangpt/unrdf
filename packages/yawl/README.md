# @unrdf/yawl

Yet Another Workflow Language (YAWL) - Hook-native workflow engine with KGC-4D integration.

## Features

- ✅ Hook-native architecture for policy enforcement
- ✅ Event sourcing with KGC-4D integration
- ✅ Cryptographic receipts (BLAKE3 hash chains)
- ✅ Time-travel and workflow replay
- ✅ All 20 Van der Aalst workflow patterns (WP1-WP20)
- ✅ Resource allocation and management
- ✅ Cancellation regions
- ✅ RDF serialization (@unrdf/oxigraph)

## Installation

```bash
pnpm add @unrdf/yawl
```

## Quick Start

```javascript
import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';

// 1. Create a workflow
const workflow = createWorkflow({
  id: 'approval-workflow',
  name: 'Document Approval',
  tasks: [
    { id: 'submit', name: 'Submit Document' },
    { id: 'review', name: 'Review Document' },
    { id: 'approve', name: 'Approve Document' }
  ],
  controlFlow: [
    { from: 'submit', to: 'review' },
    { from: 'review', to: 'approve' }
  ]
});

// 2. Create a case (workflow instance)
const caseObj = await createCase(workflow);

// 3. Execute tasks
const submitWorkItem = caseObj.getWorkItem('submit');
await enableTask(submitWorkItem);
await startTask(submitWorkItem);
await completeTask(submitWorkItem, { status: 'submitted' });

// Workflow continues through review and approval...
```

## Architecture

### Modular Structure

The package is organized into focused modules:

- `api/` - Workflow API (creation, execution, queries, cancellation, time-travel)
- `engine/` - YAWL engine subsystems (core, events, hooks, health, snapshots, queries)
- `workflow/` - Workflow class (core, validation, patterns, RDF)
- `resources/` - Resource management (participants, tools, roles, capacity)
- `hooks/` - Policy enforcement hooks
- `events/` - Event sourcing
- `cancellation/` - Cancellation handling
- `receipt.mjs` - Cryptographic receipts

### Workflow Patterns

Supports all 20 Van der Aalst patterns:

- WP1: Sequence
- WP2: Parallel Split (AND-split)
- WP3: Synchronization (AND-join)
- WP4: Exclusive Choice (XOR-split)
- WP5: Simple Merge (XOR-join)
- WP6: Multi-Choice (OR-split)
- WP7: Structured Synchronizing Merge (OR-join)
- WP8-WP20: Advanced patterns (cancellation, iteration, state-based, etc.)

## API Reference

See [MIGRATION.md](./MIGRATION.md) for detailed API documentation.

## Development

```bash
# Install dependencies
pnpm install

# Run tests
pnpm test

# Run tests with coverage
pnpm test:coverage

# Run linter
pnpm lint

# Run full validation
pnpm validate
```

## Contributing

See [MIGRATION.md](./MIGRATION.md) for architecture details.

## License

MIT
