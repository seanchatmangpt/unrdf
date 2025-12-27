# @unrdf/kgc-runtime

Multi-agent capsule merge and conflict resolution system for KGC (Knowledge Graph Consensus) runtime.

## Features

- **Deterministic Conflict Resolution**: Multiple resolution strategies (earlier_wins, later_wins, lexicographic, merge_all)
- **Conflict Detection**: Automatic detection of overlapping file edits across capsules
- **Conflict Receipts**: Comprehensive audit trail for all merge decisions
- **Type-Safe**: Full Zod validation with JSDoc type definitions
- **Pure Functions**: No side effects, fully testable

## Installation

```bash
pnpm add @unrdf/kgc-runtime
```

## Usage

### Basic Merge

```javascript
import { mergeCapsules } from '@unrdf/kgc-runtime';

const capsules = [
  {
    id: 'agent1',
    o_hash: 'hash_a',
    file_edits: [
      {
        file_path: 'src/main.js',
        line_start: 1,
        line_end: 10,
        content: 'new code',
        operation: 'replace'
      }
    ]
  },
  {
    id: 'agent2',
    o_hash: 'hash_b',
    file_edits: [
      {
        file_path: 'src/main.js',
        line_start: 5,
        line_end: 15,
        content: 'different code',
        operation: 'replace'
      }
    ]
  }
];

const totalOrder = {
  rules: [],
  default_rule: { strategy: 'earlier_wins' }
};

const result = mergeCapsules(capsules, totalOrder);

console.log(result.admitted);  // ['agent1']
console.log(result.denied);    // ['agent2']
console.log(result.conflict_receipts);  // Detailed conflict information
```

## API

### `mergeCapsules(capsules, totalOrder)`

Main API for merging capsules with conflict resolution.

**Parameters:**
- `capsules` - Array of capsule objects
- `totalOrder` - Resolution strategy configuration

**Returns:**
- `admitted` - Array of admitted capsule IDs
- `denied` - Array of denied capsule IDs
- `conflict_receipts` - Array of conflict resolution receipts
- `merged_state` - Final merged state object

### `shardMerge(capsules, totalOrder)`

Lower-level merge function for shard operations.

### Resolution Strategies

1. **earlier_wins** - Capsule with earliest o_hash wins
2. **later_wins** - Capsule with latest o_hash wins
3. **lexicographic** - Lexicographically first capsule_id wins
4. **merge_all** - Admit all capsules (no conflict resolution)

## License

MIT
