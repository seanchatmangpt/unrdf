# How-To: Use Knowledge Hooks in React

**Problem**: You need to integrate Knowledge Hooks into a React application for reactive graph updates, form validation, and side effects management.

## Solution

Use Knowledge Hooks with React Context and custom hooks to coordinate RDF operations with component lifecycle. This guide shows the integration pattern and best practices.

---

## Architecture Pattern

Knowledge Hooks integrate with React through three layers:

```
React Component
    ↓
Custom Hook (useStore, useHook)
    ↓
Hook Manager (coordinates execution)
    ↓
RDF Store (mutations)
```

### Layer 1: Hook Manager in React Context

Initialize a global hook manager in your app root:

```javascript
import React, { createContext, useContext, useMemo } from 'react';
import { createHookManager } from 'unrdf';

// Create context for hook manager
const HookManagerContext = createContext(null);

export function HookManagerProvider({ children }) {
  const manager = useMemo(() => {
    return createHookManager({
      maxConcurrent: 10,
      timeout: 30000,
      enableMetrics: true
    });
  }, []);

  return (
    <HookManagerContext.Provider value={manager}>
      {children}
    </HookManagerContext.Provider>
  );
}

export function useHookManager() {
  const manager = useContext(HookManagerContext);
  if (!manager) {
    throw new Error('useHookManager must be used within HookManagerProvider');
  }
  return manager;
}
```

### Layer 2: Custom Hook Wrapper

Create a custom hook that wraps Knowledge Hooks:

```javascript
import { useCallback, useState, useEffect } from 'react';
import { useHookManager } from './HookManagerProvider';

export function useKnowledgeHook(hookDef, deps = []) {
  const manager = useHookManager();
  const [isRegistered, setIsRegistered] = useState(false);
  const [error, setError] = useState(null);

  // Register hook on mount
  useEffect(() => {
    async function register() {
      try {
        await manager.registerHook(hookDef);
        setIsRegistered(true);
        setError(null);
      } catch (err) {
        setError(err.message);
        setIsRegistered(false);
      }
    }

    register();

    // Cleanup: deregister on unmount
    return async () => {
      try {
        await manager.deregisterHook(hookDef.meta.name);
      } catch (err) {
        console.warn('Failed to deregister hook:', err.message);
      }
    };
  }, [manager, hookDef.meta.name, ...deps]);

  // Return hook execution function
  const execute = useCallback(async (event) => {
    if (!isRegistered) {
      throw new Error('Hook not yet registered');
    }
    return manager.executeHook(hookDef, event);
  }, [manager, hookDef, isRegistered]);

  return {
    isRegistered,
    error,
    execute
  };
}
```

### Layer 3: RDF Store Hook

Manage RDF store state in React:

```javascript
import { useState, useCallback, useEffect } from 'react';
import { Store, parseTurtle } from 'unrdf';

export function useRdfStore(initialTurtle = '') {
  const [store, setStore] = useState(() => {
    const newStore = new Store();
    if (initialTurtle) {
      const parsed = parseTurtle(initialTurtle);
      parsed.getQuads().forEach(quad => {
        newStore.addQuad(quad);
      });
    }
    return newStore;
  });

  const addQuads = useCallback((quads) => {
    setStore(prevStore => {
      const newStore = new Store([...prevStore.getQuads()]);
      quads.forEach(quad => newStore.addQuad(quad));
      return newStore;
    });
  }, []);

  const removeQuads = useCallback((quads) => {
    setStore(prevStore => {
      const newStore = new Store([...prevStore.getQuads()]);
      quads.forEach(quad => newStore.removeQuad(quad));
      return newStore;
    });
  }, []);

  const query = useCallback((sparql) => {
    return store.query(sparql);
  }, [store]);

  return {
    store,
    addQuads,
    removeQuads,
    query,
    quadCount: store.size
  };
}
```

---

## Integration Patterns

### Pattern 1: Form with Validation Hook

Automatically validate form changes against SHACL shapes:

```javascript
import { defineHook } from 'unrdf';
import { useKnowledgeHook, useRdfStore } from './hooks';
import { useState } from 'react';

export function PersonForm() {
  const { store, addQuads } = useRdfStore();
  const [name, setName] = useState('');
  const [age, setAge] = useState('');
  const [validationError, setValidationError] = useState(null);

  // Define validation hook
  const validationHook = defineHook({
    meta: {
      name: 'person-validator',
      description: 'Validates person data against SHACL'
    },
    when: {
      kind: 'sparql-ask',
      query: `
        PREFIX schema: <http://schema.org/>
        ASK {
          ?person a schema:Person ;
                  schema:name ?name ;
                  schema:age ?age .
          FILTER (strlen(?name) > 0 && ?age >= 0 && ?age < 150)
        }
      `
    },
    async before({ payload }) {
      // Validate before running
      if (!payload.name || payload.name.length === 0) {
        return {
          cancel: true,
          reason: 'Name cannot be empty'
        };
      }
      if (payload.age < 0 || payload.age > 150) {
        return {
          cancel: true,
          reason: 'Age must be between 0 and 150'
        };
      }
      return payload;
    },
    async run({ payload }) {
      // Create person quad
      const quad = DataFactory.quad(
        DataFactory.namedNode(`http://example.org/person/${Date.now()}`),
        DataFactory.namedNode('http://schema.org/name'),
        DataFactory.literal(payload.name)
      );
      return { quad };
    },
    async after({ result, cancelled, reason }) {
      if (cancelled) {
        setValidationError(reason);
      } else {
        setValidationError(null);
        addQuads([result.quad]);
      }
    }
  });

  // Use hook
  const { execute, error } = useKnowledgeHook(validationHook, [store]);

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      await execute({
        payload: { name, age: parseInt(age) },
        store
      });
      setName('');
      setAge('');
    } catch (err) {
      setValidationError(err.message);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="text"
        value={name}
        onChange={(e) => setName(e.target.value)}
        placeholder="Name"
        required
      />
      <input
        type="number"
        value={age}
        onChange={(e) => setAge(e.target.value)}
        placeholder="Age"
        required
      />
      <button type="submit">Add Person</button>
      {(validationError || error) && (
        <div className="error">
          {validationError || error}
        </div>
      )}
    </form>
  );
}
```

### Pattern 2: Auto-save with Hooks

Automatically persist changes using hooks:

```javascript
import { useEffect } from 'react';
import { defineHook } from 'unrdf';
import { useKnowledgeHook } from './hooks';

export function AutoSaveDocument({ documentId, store }) {
  const autoSaveHook = defineHook({
    meta: {
      name: 'auto-save',
      description: 'Auto-saves document changes'
    },
    when: { kind: 'transaction' },
    async run({ payload }) {
      // Persist to backend
      const response = await fetch(`/api/documents/${documentId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          quads: payload.quads,
          timestamp: Date.now()
        })
      });
      return { saved: response.ok };
    }
  });

  const { execute } = useKnowledgeHook(autoSaveHook);

  // Save whenever store changes
  useEffect(() => {
    const timer = setTimeout(async () => {
      try {
        await execute({
          payload: { quads: store.getQuads() },
          store
        });
      } catch (err) {
        console.error('Auto-save failed:', err);
      }
    }, 5000); // Debounce 5 seconds

    return () => clearTimeout(timer);
  }, [store, execute]);

  return null; // Side-effect only component
}
```

### Pattern 3: Permission Checks with Hooks

Guard mutations with permission-checking hooks:

```javascript
import { defineHook } from 'unrdf';
import { useKnowledgeHook } from './hooks';
import { useAuth } from './auth'; // Your auth context

export function ProtectedStore() {
  const { user } = useAuth();

  const permissionHook = defineHook({
    meta: {
      name: 'permission-check',
      description: 'Validates user permissions before mutation'
    },
    when: { kind: 'transaction' },
    async before({ payload }) {
      // Check permissions
      const hasPermission = await checkUserPermission(
        user.id,
        payload.action,
        payload.resource
      );

      if (!hasPermission) {
        return {
          cancel: true,
          reason: `User ${user.id} lacks permission for ${payload.action}`
        };
      }

      return payload;
    },
    async run({ payload }) {
      return { authorized: true };
    }
  });

  const { execute } = useKnowledgeHook(permissionHook, [user.id]);

  return {
    executeWithPermission: execute
  };
}
```

---

## Component Lifecycle Alignment

Knowledge Hooks align with React lifecycle:

```
Component Mount
    ↓
registerHook() (useEffect with [])
    ↓
Component Renders
    ↓
User Interaction → execute(event)
    ↓
Hook Runs: before → run → after
    ↓
State Updates (via setters)
    ↓
Component Re-renders
    ↓
Component Unmount
    ↓
deregisterHook() (cleanup function)
```

---

## Error Handling with Boundaries

Use React Error Boundary for hook failures:

```javascript
import { Component } from 'react';

export class HookErrorBoundary extends Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error) {
    return { hasError: true, error };
  }

  componentDidCatch(error, errorInfo) {
    console.error('Hook execution failed:', error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return (
        <div className="error-boundary">
          <h2>Hook Execution Failed</h2>
          <p>{this.state.error?.message}</p>
          <button onClick={() => this.setState({ hasError: false })}>
            Retry
          </button>
        </div>
      );
    }

    return this.props.children;
  }
}

// Usage
export function App() {
  return (
    <HookErrorBoundary>
      <YourComponent />
    </HookErrorBoundary>
  );
}
```

---

## Real-World Example: Knowledge Graph Search

Complete example combining hooks, store, and React:

```javascript
import { useState, useMemo } from 'react';
import { defineHook } from 'unrdf';
import { useKnowledgeHook, useRdfStore } from './hooks';

export function KnowledgeGraphSearch() {
  const { store, addQuads } = useRdfStore();
  const [query, setQuery] = useState('');
  const [results, setResults] = useState([]);

  // Index hook - builds search index
  const indexHook = defineHook({
    meta: {
      name: 'search-indexer',
      description: 'Builds search index on store changes'
    },
    when: { kind: 'transaction' },
    async run({ payload }) {
      const terms = query.toLowerCase().split(' ');
      const matches = [];

      store.getQuads().forEach(quad => {
        terms.forEach(term => {
          if (
            quad.subject.value?.includes(term) ||
            quad.predicate.value?.includes(term) ||
            quad.object.value?.includes(term)
          ) {
            matches.push(quad);
          }
        });
      });

      return { matches };
    },
    async after({ result }) {
      setResults(result.matches || []);
    }
  });

  const { execute } = useKnowledgeHook(indexHook, [query, store]);

  const handleSearch = async () => {
    try {
      await execute({ payload: { query }, store });
    } catch (err) {
      console.error('Search failed:', err);
    }
  };

  return (
    <div>
      <input
        type="text"
        value={query}
        onChange={(e) => setQuery(e.target.value)}
        placeholder="Search RDF graph..."
      />
      <button onClick={handleSearch}>Search</button>

      <div className="results">
        <h3>Results: {results.length}</h3>
        <ul>
          {results.map((quad, idx) => (
            <li key={idx}>
              {quad.subject.value} → {quad.predicate.value} → {quad.object.value}
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
}
```

---

## Best Practices

### 1. Dependency Management

Always include proper dependencies in `useEffect`:

```javascript
// ✓ Correct
useEffect(() => {
  manager.registerHook(hook);
  return () => manager.deregisterHook(hook.meta.name);
}, [manager, hook.meta.name]);

// ✗ Wrong - missing dependencies
useEffect(() => {
  manager.registerHook(hook);
}, []);
```

### 2. Memoization

Memoize hook definitions to prevent unnecessary re-registration:

```javascript
const myHook = useMemo(() =>
  defineHook({
    // ...
  }),
  [dependencies]
);
```

### 3. Error Recovery

Always handle hook execution errors:

```javascript
try {
  await execute(event);
} catch (err) {
  // Log error
  console.error('Hook failed:', err);
  // Update UI
  setError(err.message);
  // Retry or fallback
}
```

### 4. Performance

Debounce hook execution for high-frequency events:

```javascript
import { useDeferredValue, useCallback } from 'react';

const debouncedQuery = useDeferredValue(queryInput);

const executeHook = useCallback(async () => {
  await execute({ payload: { query: debouncedQuery } });
}, [debouncedQuery, execute]);
```

---

## Related Guides

- [How-To: Create Knowledge Hooks](./create-knowledge-hooks.md) - Hook definition patterns
- [How-To: Handle Transactions](./handle-transactions.md) - Transaction management
- [Tutorial: Knowledge Hooks](../tutorials/02-knowledge-hooks.md) - Complete walkthrough
- [Reference: Knowledge Hooks API](../reference/knowledge-hooks-api.md) - API details
