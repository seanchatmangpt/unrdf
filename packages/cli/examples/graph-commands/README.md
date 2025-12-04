# Graph Commands Example

Custom CLI commands for graph operations using @unrdf/cli.

## Features

- Load RDF graphs from files
- Display graph statistics
- Merge multiple graphs
- Save graphs in various formats
- Custom command definitions with citty

## Installation

```bash
pnpm install
```

## Usage

### Load Graph

```bash
node src/index.mjs load data/example.ttl
```

### Display Statistics

```bash
node src/index.mjs stats data/example.ttl
```

### Merge Graphs

```bash
node src/index.mjs merge "data/graph1.ttl,data/graph2.ttl" --output merged.ttl --format turtle
```

## Custom Commands

The example demonstrates how to create custom CLI commands:

```javascript
import { defineCommand } from 'citty';

export const customCommand = defineCommand({
  meta: {
    name: 'custom',
    description: 'Custom command'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input file',
      required: true
    }
  },
  async run({ args }) {
    // Command logic
  }
});
```

## API

### loadGraph(filepath)

Load RDF graph from file into N3 Store.

### saveGraph(store, filepath, format)

Save N3 Store to file in specified format.

### getGraphStats(store)

Calculate graph statistics (quads, subjects, predicates, objects).

### mergeGraphs(stores)

Merge multiple N3 Stores into single store.

## Testing

```bash
pnpm test
```
