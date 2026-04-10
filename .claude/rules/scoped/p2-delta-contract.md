# Scoped Rule: v6-Core Delta Contract

**Scope**: `@unrdf/v6-core` - YAWL v6 Delta operations

## Delta Structure

All Delta operations return structured metadata:

```javascript
{
  added: Quad[],      // Quads added to store
  removed: Quad[],    // Quads removed from store
  timestamp: string   // ISO 8601 timestamp
}
```

## Operation Patterns

### Insert Delta

```javascript
// Correct
const delta = await store.insert(quad);
expect(delta.added).toHaveLength(1);
expect(delta.removed).toHaveLength(0);

// Wrong - not checking delta
await store.insert(quad);
// What happened? Did it succeed?
```

### Delete Delta

```javascript
// Correct
const delta = await store.delete(quad);
expect(delta.removed).toHaveLength(1);
expect(delta.added).toHaveLength(0);
```

### Update Delta

Update = remove old quad + insert new quad:

```javascript
// Correct
const delta = await store.update(oldQuad, newQuad);
expect(delta.removed).toContainEqual(oldQuad);
expect(delta.added).toContainEqual(newQuad);

// Wrong - expecting single operation
await store.update(oldQuad, newQuad);
// Update is atomic but returns both removed and added
```

## Transaction Deltas

For multi-operation transactions:

```javascript
const delta = await store.transaction(async (tx) => {
  await tx.insert(quad1);
  await tx.insert(quad2);
  await tx.delete(quad3);
});

expect(delta.added).toHaveLength(2);
expect(delta.removed).toHaveLength(1);
```

## Delta Verification

Always verify Delta results in tests:

```javascript
// Correct - test verifies exact changes
const delta = await operation();
expect(delta).toMatchDelta({
  added: [expectedQuad1],
  removed: [expectedQuad2]
});

// Wrong - no verification
await operation();
expect(true).toBe(true); // Meaningless
```

## Streaming Deltas

For bulk operations, Delta may be streamed:

```javascript
for await (const batch of store.bulkInsert(quads)) {
  console.log(`Batch: ${batch.added.length} added`);
}
```

## Files Using Delta

- `packages/v6-core/src/store/*` - Store implementations
- `packages/v6-core/src/transaction/*` - Transaction handling
- Any code importing from `@unrdf/v6-core`
