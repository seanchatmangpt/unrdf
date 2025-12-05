# How To: Reconstruct State at a Point in Time

**Problem:** You need to see what your knowledge graph looked like at a specific moment in the past.

**Solution:** Use `reconstructState()` to time-travel to any historical point.

## When to Use This

- **Audit**: "What was the state on December 1st?"
- **Recovery**: "Restore to before the bad change"
- **Analysis**: "Compare state across different dates"
- **Debugging**: "When did this value change?"
- **Compliance**: "Prove the state at time of transaction"

## Basic Time Travel

```javascript
import { KGCStore, GitBackbone, reconstructState, freezeUniverse } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./repo');

// Create some events
await store.appendEvent(
  { type: 'CREATE', payload: { description: 'Initial' } },
  [/* mutations */]
);

// Freeze at this point
const snapshot = await freezeUniverse(store, git);

// Make more changes
await store.appendEvent(
  { type: 'UPDATE', payload: { description: 'Changed something' } },
  [/* mutations */]
);

// Time-travel to the snapshot
const pastStore = await reconstructState(store, git, snapshot.tNs);

// Query the past state
const results = pastStore.querySync(`
  PREFIX ex: <http://example.org/>
  SELECT ?s ?p ?o
  WHERE { GRAPH <kgc:Universe> { ?s ?p ?o } }
`);

console.log(`State at ${snapshot.tNs}:`, results.length, 'triples');
```

## Travel to Specific Timestamps

You don't need a snapshot at the exact target time. KGC 4D finds the nearest snapshot and replays events:

```javascript
import { reconstructState } from '@unrdf/kgc-4d';
import { addNanoseconds, now } from '@unrdf/kgc-4d';

// Travel to 10 minutes ago
const tenMinutesAgo = now() - BigInt(600_000_000_000); // 600 seconds in ns

const pastStore = await reconstructState(store, git, tenMinutesAgo);

// Or travel to a specific date
const targetDate = new Date('2024-12-01T12:00:00Z');
const targetNs = BigInt(targetDate.getTime()) * BigInt(1_000_000);

const pointInTimeStore = await reconstructState(store, git, targetNs);
```

## Compare State Across Time

Reconstruct multiple points and compare:

```javascript
async function compareStateOverTime(store, git, timestamps) {
  const states = [];

  for (const ts of timestamps) {
    const pastStore = await reconstructState(store, git, ts);
    const count = pastStore.querySync(`
      PREFIX ex: <http://example.org/>
      SELECT (COUNT(*) as ?count)
      WHERE { GRAPH <kgc:Universe> { ?s ?p ?o } }
    `);
    states.push({ timestamp: ts, tripleCount: count[0].get('count').value });
  }

  return states;
}

// Compare every day for a week
const dayInNs = BigInt(86_400_000_000_000);
const baseTime = now();
const timestamps = Array.from({ length: 7 }, (_, i) =>
  baseTime - (BigInt(i) * dayInNs)
).reverse();

const timeline = await compareStateOverTime(store, git, timestamps);
console.table(timeline);
```

## Handle Missing Snapshots

If no snapshots exist before your target time, `reconstructState()` replays from the beginning:

```javascript
// This works even without any snapshots
const veryOldTime = BigInt('1000000000000000'); // Ancient timestamp

try {
  const store = await reconstructState(originalStore, git, veryOldTime);
  console.log('✓ Reconstructed from event log');
} catch (error) {
  if (error.message.includes('No snapshots')) {
    console.log('No snapshots found, replaying all events');
  }
}
```

## Efficient Time Travel: Snapshot Strategy

For frequently accessed time points, create snapshots at regular intervals:

```javascript
async function createSnapshotCheckpoints(store, git, interval) {
  const snapshots = [];
  const now_ = now();
  let currentTime = now_;

  while (currentTime > BigInt(0)) {
    // Only create if meaningful changes exist
    const snapshot = await freezeUniverse(store, git);
    snapshots.push({ time: currentTime, hash: snapshot.hash });

    // Move back by interval
    currentTime -= interval;

    if (snapshots.length > 100) break; // Safety limit
  }

  return snapshots;
}

// Create hourly snapshots
const hourInNs = BigInt(3_600_000_000_000);
await createSnapshotCheckpoints(store, git, hourInNs);
```

## Reconstruct for Reporting

Generate a historical report:

```javascript
async function generateStateReport(store, git, entity, startDate, endDate) {
  const query = `
    PREFIX ex: <http://example.org/>
    SELECT ?property ?value
    WHERE {
      GRAPH <kgc:Universe> {
        <${entity}> ?property ?value .
      }
    }
  `;

  const dayInNs = BigInt(86_400_000_000_000);
  const startNs = BigInt(new Date(startDate).getTime()) * BigInt(1_000_000);
  const endNs = BigInt(new Date(endDate).getTime()) * BigInt(1_000_000);

  const report = [];

  for (let current = startNs; current <= endNs; current += dayInNs) {
    const pastStore = await reconstructState(store, git, current);
    const results = pastStore.querySync(query);

    report.push({
      date: new Date(Number(current) / 1_000_000).toISOString(),
      properties: results.map(b => ({
        property: b.get('property').value.split('/').pop(),
        value: b.get('value').value,
      })),
    });
  }

  return report;
}

const report = await generateStateReport(
  store,
  git,
  'http://example.org/alice',
  '2024-12-01',
  '2024-12-31'
);

console.table(report);
```

## Troubleshooting

**Q: Time travel is slow**
A: Create snapshots more frequently. Without snapshots, KGC 4D replays all events from the beginning.

**Q: "No snapshots found" error**
A: You can still reconstruct; it just replays from the event log. Create a snapshot with `freezeUniverse()` first.

**Q: Timestamp precision issues**
A: Use BigInt nanoseconds (ns), not milliseconds. Convert dates with: `new Date(...).getTime() * 1_000_000n`

**Q: Memory issues with large replays**
A: Create intermediate snapshots to reduce replay distance. Or load the pastStore in chunks by querying incrementally.

## Performance Tips

1. **Snapshot regularly** - Reduces replay distance
2. **Query efficiently** - Use SPARQL FILTER to limit result sets
3. **Use intermediate stores** - Don't load entire history into memory
4. **Parallel queries** - Load multiple time points concurrently

```javascript
// Parallel reconstruction
const points = [time1, time2, time3];
const stores = await Promise.all(
  points.map(t => reconstructState(originalStore, git, t))
);
```

## Summary

- Use `reconstructState(store, git, targetTimestamp)` to travel back
- Timestamps are BigInt nanoseconds
- Snapshots optimize replay distance
- No snapshots needed; event log is authoritative
- Perfect for audits, recovery, and compliance reporting
