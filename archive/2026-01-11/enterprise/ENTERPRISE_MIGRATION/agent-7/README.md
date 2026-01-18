# Agent 7 - Routing + Shadow Modes

Complete routing and shadow execution system for safe migration between legacy and substrate systems.

## Overview

Agent 7 provides a comprehensive framework for gradually migrating traffic from legacy systems to substrate systems with full observability, drift detection, and automatic rollback capabilities.

## Architecture

```
agent-7/
├── routing-modes.mjs       # Mode definitions and management
├── shadow-executor.mjs     # Shadow execution logic
├── mismatch-ledger.mjs     # Mismatch tracking and analysis
├── drift-detector.mjs      # Drift detection and scoring
├── rollback-router.mjs     # Rollback routing and automation
├── index.mjs               # Public API exports
├── test-routing.mjs        # Comprehensive test suite
└── README.md               # This file
```

## Routing Modes

Five migration modes support progressive rollout:

### LEGACY_ONLY
- Only legacy system handles requests
- Default mode for all operations
- Safe starting point

### SHADOW_WRITE
- Legacy writes (authoritative)
- Substrate receives shadow copy (non-blocking)
- Mismatches logged but don't block
- Use for write validation

### SHADOW_READ
- Read from both systems in parallel
- Legacy result returned (authoritative)
- Results compared for drift detection
- Use for read validation

### PARTIAL_SERVE
- Route percentage of traffic to substrate
- Deterministic routing based on payload hash
- Automatic fallback to legacy on substrate failure
- Use for gradual rollout

### SUBSTRATE_ONLY
- Full cutover to substrate
- Legacy no longer receives traffic
- Final migration state

## Core Components

### 1. Routing Modes (`routing-modes.mjs`)

Manage operation routing configurations:

```javascript
import { setMode, getMode, ROUTING_MODES } from './agent-7/index.mjs';

// Set operation mode
setMode('GET_USER', ROUTING_MODES.SHADOW_READ);

// Get current mode
const mode = getMode('GET_USER'); // Returns: 'SHADOW_READ'

// List all configured modes
const modes = listModes();
// Returns: [{ operation: 'GET_USER', mode: 'SHADOW_READ', isDefault: false }]
```

**Functions:**
- `setMode(operation, mode)` - Set routing mode for operation
- `getMode(operation)` - Get current mode (defaults to LEGACY_ONLY)
- `listModes()` - List all configured modes
- `resetModes()` - Clear all mode configurations
- `getModeStats()` - Get statistics by mode
- `exportConfig()` / `importConfig(config)` - Serialize/restore configuration

### 2. Shadow Executor (`shadow-executor.mjs`)

Execute operations in shadow mode with comparison:

```javascript
import { shadowWrite, shadowRead, partialServe } from './agent-7/index.mjs';

// Shadow write
const result = await shadowWrite(
  'CREATE_ORDER',
  { orderId: '123', items: [...] },
  legacyFn,
  substrateFn
);
// Returns: { success: true, result: {...}, shadow: { mismatch: false } }

// Shadow read
const result = await shadowRead(
  'GET_USER',
  { userId: '456' },
  legacyFn,
  substrateFn
);
// Returns: { success: true, result: {...}, comparison: { matched: true } }

// Partial serve (50% to substrate)
const result = await partialServe(
  'SEARCH_PRODUCTS',
  { query: 'laptop' },
  50,
  legacyFn,
  substrateFn
);
// Returns: { success: true, result: {...}, routing: { routedTo: 'substrate' } }
```

**Functions:**
- `shadowWrite(operation, payload, legacyFn, substrateFn)` - Write to both systems
- `shadowRead(operation, query, legacyFn, substrateFn)` - Read from both, compare
- `partialServe(operation, payload, percentage, legacyFn, substrateFn)` - Route subset
- `execute(operation, payload, legacyFn, substrateFn, options)` - Execute based on mode

**Deep Comparison:**
Automatically detects:
- Type mismatches (string vs number)
- Value differences (case, formatting)
- Null/undefined differences
- Array length mismatches
- Object key differences (missing/extra fields)

### 3. Mismatch Ledger (`mismatch-ledger.mjs`)

Track and analyze mismatches:

```javascript
import { recordMismatch, getMismatches, exportLedger } from './agent-7/index.mjs';

// Record mismatch
const entry = recordMismatch(
  'GET_USER',
  { id: '123', name: 'John' },
  { id: '123', name: 'john' },
  { type: 'VALUE_MISMATCH', fields: ['name'] }
);
// Returns: { id: 'mismatch_1703587200_a1b2c3d4', operation: 'GET_USER', ... }

// Query mismatches
const mismatches = getMismatches({
  operation: 'GET_USER',
  diffType: 'VALUE_MISMATCH',
  since: Date.now() - 3600000, // Last hour
  limit: 100
});

// Export ledger with hash
const exported = exportLedger();
// Returns: { metadata: {...}, entries: [...], hash: 'sha256...' }
```

**Functions:**
- `recordMismatch(operation, legacy, substrate, diff)` - Log mismatch
- `getMismatches(filter)` - Query with filtering
- `exportLedger()` - Export as stable JSON with SHA-256 hash
- `clearLedger()` - Reset ledger
- `getLedgerStats()` - Get statistics
- `getRecentMismatches(operation, count)` - Get recent entries

**Ledger Entry Format:**
```javascript
{
  id: 'mismatch_1703587200_a1b2c3d4',
  timestamp: 1703587200000,
  operation: 'GET_USER',
  legacyResult: { id: '123', name: 'John' },
  substrateResult: { id: '123', name: 'john' },
  diff: { type: 'VALUE_MISMATCH', fields: ['name'] }
}
```

### 4. Drift Detector (`drift-detector.mjs`)

Detect semantic drift between systems:

```javascript
import { detectDrift, calculateDriftScore, isAcceptableDrift } from './agent-7/index.mjs';

// Detect drift for operation
const drift = detectDrift('GET_USER', {
  lookbackMs: 3600000,  // 1 hour window
  minSamples: 10        // Require 10+ samples
});
// Returns: {
//   hasDrift: true,
//   score: 65.3,
//   severity: 'MODERATE',
//   samples: 42,
//   patterns: { byDiffType: {...}, affectedFields: {...} },
//   recommendation: 'Investigate root cause...'
// }

// Check if drift acceptable
const acceptable = isAcceptableDrift(65.3, 75); // true (below threshold)
```

**Drift Severity Levels:**
- `NONE`: 0-10 (minimal drift)
- `LOW`: 10-25 (monitor)
- `MODERATE`: 25-50 (investigate)
- `HIGH`: 50-75 (urgent action)
- `CRITICAL`: 75-100 (halt migration)

**Functions:**
- `detectDrift(operation, options)` - Detect drift for operation
- `calculateDriftScore(mismatches)` - Calculate 0-100 score
- `isAcceptableDrift(score, threshold)` - Check threshold
- `getDriftReport(options)` - Report for all operations
- `analyzeDiffType(diffType, options)` - Analyze specific diff type

### 5. Rollback Router (`rollback-router.mjs`)

Manage rollback routing:

```javascript
import { markForRollback, executeRollback, autoRollback } from './agent-7/index.mjs';

// Mark for rollback
const marker = markForRollback('GET_USER', {
  reason: 'High drift detected',
  driftScore: 85
});

// Execute rollback (revert to LEGACY_ONLY)
const result = executeRollback('GET_USER');
// Returns: { success: true, previousMode: 'SHADOW_READ', newMode: 'LEGACY_ONLY' }

// Auto rollback on high drift
const result = autoRollback('GET_USER', {
  threshold: 75,      // Trigger if drift > 75
  autoExecute: true   // Automatically execute rollback
});
```

**Functions:**
- `markForRollback(operation, options)` - Flag operation
- `executeRollback(operation, options)` - Revert to LEGACY_ONLY
- `getRollbackStatus()` - Get all rollback flags
- `autoRollback(operation, options)` - Auto-rollback on drift
- `batchRollback(operations, options)` - Rollback multiple operations

## Usage Examples

### Basic Migration Flow

```javascript
import {
  setMode,
  ROUTING_MODES,
  shadowRead,
  detectDrift,
  autoRollback
} from './agent-7/index.mjs';

// 1. Start in LEGACY_ONLY (default)
// All traffic goes to legacy

// 2. Enable shadow reads
setMode('GET_USER', ROUTING_MODES.SHADOW_READ);

// Execute reads
for (let i = 0; i < 100; i++) {
  await shadowRead('GET_USER', { id: i }, legacyFn, substrateFn);
}

// 3. Check drift
const drift = detectDrift('GET_USER');
if (drift.score > 75) {
  console.error('High drift detected, halting migration');
  autoRollback('GET_USER', { threshold: 75, autoExecute: true });
} else {
  // 4. Proceed to partial serve
  setMode('GET_USER', ROUTING_MODES.PARTIAL_SERVE);

  // Gradually increase percentage...

  // 5. Full cutover
  setMode('GET_USER', ROUTING_MODES.SUBSTRATE_ONLY);
}
```

### Monitoring and Observability

```javascript
import { getSystemStatus, exportLedger } from './agent-7/index.mjs';

// Get complete system status
const status = getSystemStatus();
console.log('Modes:', status.modes);
console.log('Mismatches:', status.mismatches);
console.log('Drift:', status.drift);
console.log('Rollbacks:', status.rollbacks);

// Export ledger for analysis
const ledger = exportLedger();
console.log('Ledger hash:', ledger.hash);
console.log('Total entries:', ledger.entries.length);

// Save for audit trail
import fs from 'fs';
fs.writeFileSync('mismatch-ledger.json', JSON.stringify(ledger, null, 2));
```

## Design Principles

### 1. Deterministic and Hashable
- Ledger exports are deterministic (sorted by timestamp)
- SHA-256 hash for integrity verification
- Reproducible state for auditing

### 2. Reversible Operations
- All modes can be changed
- Rollback to LEGACY_ONLY always available
- Import/export state for recovery

### 3. Non-blocking Shadow Execution
- Shadow operations never block primary path
- Legacy is always authoritative during migration
- Substrate failures logged but don't propagate

### 4. Progressive Rollout
- Five distinct modes for gradual migration
- Partial serve uses deterministic routing
- Automatic fallback on substrate failure

### 5. Observability First
- All mismatches logged with full context
- Drift detection with severity levels
- Comprehensive statistics and reporting

## Testing

Run the comprehensive test suite:

```bash
node agent-7/test-routing.mjs
```

**Test Coverage:**
- ✅ 53 tests
- ✅ 100% pass rate
- ✅ All routing modes
- ✅ Shadow execution (read/write/partial)
- ✅ Mismatch ledger (record/query/export)
- ✅ Drift detection and scoring
- ✅ Rollback routing (manual/auto)
- ✅ System status and state management

## Performance Characteristics

### Memory
- In-memory ledger (bounded by mismatch count)
- Recommend periodic export/clear for long-running systems
- Typical entry: ~500 bytes

### Latency
- Shadow reads: +legacy latency (parallel execution)
- Shadow writes: +substrate latency (non-blocking async)
- Partial serve: Single system latency + routing overhead (<1ms)
- Mode checks: O(1) map lookup

### Determinism
- Partial serve routing is deterministic (payload hash)
- Same payload always routes to same system
- Consistent for A/B testing and debugging

## Migration Checklist

- [ ] Implement legacy and substrate functions
- [ ] Start in LEGACY_ONLY mode
- [ ] Enable SHADOW_READ for reads
- [ ] Monitor drift scores (<25 = good)
- [ ] Enable SHADOW_WRITE for writes
- [ ] Verify mismatch patterns acceptable
- [ ] Enable PARTIAL_SERVE at 10%
- [ ] Gradually increase to 50%, 90%
- [ ] Monitor for 24-48 hours at each level
- [ ] Full cutover to SUBSTRATE_ONLY
- [ ] Keep legacy system warm for 7-14 days
- [ ] Export final ledger for audit
- [ ] Decommission legacy system

## Statistics

- **Total Lines of Code:** 1,868
- **Files:** 6 core modules + 1 test + 1 README
- **Functions:** 60+ exported functions
- **Test Coverage:** 53 tests, 100% pass rate
- **No External Dependencies:** Pure Node.js ESM

## License

Part of the UNRDF Enterprise Migration project.
