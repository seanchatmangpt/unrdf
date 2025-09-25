# Knowledge Hooks Migration Guide

## Overview

This guide helps you migrate from the legacy query-based knowledge hooks system to the new event-based system. The new system provides real-time graph mutation detection with better performance and more granular control.

## Key Changes

### 1. **Event-Driven vs Query-Driven**

**Before (Query-Based):**
```javascript
// Hooks were evaluated on-demand
const hook = defineHook({
  id: 'error-count',
  query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
  predicates: [
    { kind: 'COUNT', spec: { operator: '>', value: 5 } }
  ]
});

const result = await evaluateHook(hook);
if (result.fired) {
  console.log('Hook fired!');
}
```

**After (Event-Based):**
```javascript
// Hooks fire automatically on graph events
const hook = defineHook({
  id: 'error-count',
  events: [EVENTS.AFTER_ADD_QUAD], // Listen to specific events
  query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
  predicates: [
    { kind: 'COUNT', spec: { operator: '>', value: 5 } }
  ],
  options: {
    callback: async (result, payload) => {
      console.log('Hook fired!', result.data.count);
    }
  }
});

const unregister = hooks.registerKnowledgeHook(hook);
```

### 2. **Real-Time vs On-Demand**

**Before:**
- Hooks were evaluated manually via `evaluateHook()`
- No automatic triggering
- Required polling or manual checks

**After:**
- Hooks fire automatically when graph events occur
- Real-time response to graph mutations
- No polling required

### 3. **Event Types**

The new system provides granular event types:

```javascript
import { EVENTS } from '../src/engines/event-bus.mjs';

// Store-level events
EVENTS.BEFORE_ADD_QUAD     // Before quad is added
EVENTS.AFTER_ADD_QUAD      // After quad is added
EVENTS.BEFORE_REMOVE_QUAD  // Before quad is removed
EVENTS.AFTER_REMOVE_QUAD   // After quad is removed
EVENTS.BEFORE_CLEAR        // Before store is cleared
EVENTS.AFTER_CLEAR         // After store is cleared

// Engine-level events
EVENTS.BEFORE_IMPORT       // Before data import
EVENTS.AFTER_IMPORT        // After data import
EVENTS.BEFORE_REASON       // Before reasoning
EVENTS.AFTER_REASON        // After reasoning
EVENTS.BEFORE_UPDATE       // Before SPARQL UPDATE
EVENTS.AFTER_UPDATE        // After SPARQL UPDATE
EVENTS.BEFORE_SERIALIZE    // Before serialization
EVENTS.AFTER_SERIALIZE     // After serialization
```

## Migration Steps

### Step 1: Update Hook Definitions

Add `events` array to your hook definitions:

```javascript
// Before
const hook = defineHook({
  id: 'my-hook',
  query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
  predicates: [
    { kind: 'COUNT', spec: { operator: '>', value: 5 } }
  ]
});

// After
const hook = defineHook({
  id: 'my-hook',
  events: [EVENTS.AFTER_ADD_QUAD], // Add event triggers
  query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
  predicates: [
    { kind: 'COUNT', spec: { operator: '>', value: 5 } }
  ],
  options: {
    callback: async (result, payload) => {
      // Handle hook firing
      console.log('Error count exceeded!');
    }
  }
});
```

### Step 2: Replace Manual Evaluation

**Before:**
```javascript
// Manual evaluation
const result = await evaluateHook(hook);
if (result.fired) {
  handleHookFiring(result);
}
```

**After:**
```javascript
// Automatic registration
const unregister = hooks.registerKnowledgeHook(hook);
// Hook will fire automatically when events occur
```

### Step 3: Update Hook Management

**Before:**
```javascript
const hooks = useKnowledgeHooks();

// Manual evaluation
const results = await hooks.evaluateHooks([hook1, hook2]);
```

**After:**
```javascript
const hooks = useKnowledgeHooks();

// Register hooks
const unregister1 = hooks.registerKnowledgeHook(hook1);
const unregister2 = hooks.registerKnowledgeHook(hook2);

// Cleanup when done
unregister1.unregister();
unregister2.unregister();
```

### Step 4: Handle Event Payloads

Event-based hooks receive rich payload information:

```javascript
const hook = defineHook({
  id: 'monitor',
  events: [EVENTS.AFTER_ADD_QUAD],
  query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
  predicates: [
    { kind: 'COUNT', spec: { operator: '>', value: 0 } }
  ],
  options: {
    callback: async (result, payload) => {
      console.log('Event:', payload.event);
      console.log('Quad:', payload.quad.subject.value);
      console.log('Context:', payload.context.source);
      console.log('Timestamp:', payload.context.timestamp);
      console.log('Store size:', payload.store.size);
    }
  }
});
```

## Advanced Features

### 1. **Before Hooks with Veto**

```javascript
const unregister = hooks.registerEventHook(
  EVENTS.BEFORE_ADD_QUAD,
  async (payload) => {
    // Veto if predicate contains 'blocked'
    if (payload.quad.predicate.value.includes('blocked')) {
      console.log('Blocked quad addition');
      return false; // Veto the operation
    }
    return true; // Allow the operation
  },
  { id: 'veto-hook' }
);
```

### 2. **Batch Operations**

```javascript
await hooks.batch(() => {
  store.addQuad(s1, p1, o1);
  store.addQuad(s2, p2, o2);
  store.addQuad(s3, p3, o3);
  // Only one batch event fired
});
```

### 3. **Event Filtering**

```javascript
const unregister = hooks.registerEventHook(
  EVENTS.AFTER_ADD_QUAD,
  async (payload) => {
    console.log('Quad added:', payload.quad.subject.value);
  },
  {
    id: 'filtered-hook',
    filter: {
      predicate: 'http://example.org/name' // Only for name predicates
    }
  }
);
```

### 4. **Performance Monitoring**

```javascript
// Get event statistics
const stats = hooks.getEventStats();
console.log('Total events:', stats.totalEvents);
console.log('Total hooks:', stats.totalHooks);
console.log('Average execution time:', stats.avgExecutionTime);
console.log('Errors:', stats.errors);
```

## Backward Compatibility

The legacy query-based system is still available for gradual migration:

```javascript
const hooks = useKnowledgeHooks();

// Legacy methods still work
const hook = hooks.defineHook({ /* ... */ });
const result = await hooks.evaluateHook(hook);
const results = await hooks.evaluateHooks([hook1, hook2]);
const stats = hooks.getStats(results);
```

## Performance Benefits

### Before (Query-Based)
- Manual evaluation required
- No real-time detection
- Potential performance overhead from repeated queries
- No event batching

### After (Event-Based)
- Automatic real-time detection
- Zero overhead when no hooks registered
- Event batching for bulk operations
- Granular event filtering
- Rich context information

## Example Migration

Here's a complete example of migrating a monitoring system:

**Before:**
```javascript
const hooks = useKnowledgeHooks();

const errorHook = defineHook({
  id: 'error-monitor',
  query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
  predicates: [
    { kind: 'COUNT', spec: { operator: '>', value: 5 } }
  ]
});

// Manual polling
setInterval(async () => {
  const result = await hooks.evaluateHook(errorHook);
  if (result.fired) {
    console.log('Error count exceeded!');
  }
}, 1000);
```

**After:**
```javascript
const hooks = useKnowledgeHooks();

const errorHook = defineHook({
  id: 'error-monitor',
  events: [EVENTS.AFTER_ADD_QUAD],
  query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
  predicates: [
    { kind: 'COUNT', spec: { operator: '>', value: 5 } }
  ],
  options: {
    callback: async (result, payload) => {
      console.log('Error count exceeded!');
      console.log('Triggered by:', payload.quad.subject.value);
    }
  }
});

// Automatic registration - no polling needed
const unregister = hooks.registerKnowledgeHook(errorHook);
```

## Troubleshooting

### Common Issues

1. **Hooks not firing**: Ensure events are enabled in engine options
2. **Performance issues**: Use event filtering to reduce hook execution
3. **Memory leaks**: Always unregister hooks when done
4. **Async errors**: Implement error handling in hook callbacks

### Debug Tips

```javascript
// Enable event statistics
const stats = hooks.getEventStats();
console.log('Event stats:', stats);

// Monitor all events
const unregister = hooks.registerEventHook(
  '*', // Listen to all events
  (payload) => {
    console.log('Event fired:', payload.event);
  }
);
```

## Conclusion

The new event-based system provides:
- **Real-time detection** of graph mutations
- **Better performance** with zero overhead when no hooks registered
- **Granular control** with before/after hooks and veto capability
- **Rich context** with detailed event payloads
- **Batch operations** for efficient bulk processing

Migrate gradually by adding `events` arrays to existing hooks and replacing manual evaluation with automatic registration.
