# Tutorial: Real-time Collaboration with Streaming

**Learning Objectives:**
- Set up real-time change feeds
- Build collaborative knowledge graphs
- Handle concurrent updates
- Implement optimistic UI updates

**Prerequisites:**
- Completed [Tutorial 1: Getting Started](./01-getting-started.md)
- Basic understanding of WebSockets (helpful but not required)
- 25 minutes

---

## Step 1: Understanding Change Feeds

UNRDF React provides the `useChangeFeed` hook for real-time updates. Unlike polling, change feeds push updates to your UI instantly when the knowledge graph changes.

**Key concepts:**
- **Change feed** - Stream of graph modifications (add/delete triples)
- **Subscription** - Active listener for graph changes
- **Delta** - Individual change event with before/after state

---

## Step 2: Create a Collaborative To-Do List

Let's build a to-do list that updates in real-time across multiple browser tabs.

Create `app/components/CollaborativeTodoList.jsx`:

```jsx
'use client';

import { useKnowledgeEngine, useChangeFeed, useTriples } from 'unrdf-react';
import { useState, useCallback } from 'react';

const TODO_NS = 'http://example.org/todo/';

export default function CollaborativeTodoList() {
  const { engine, ready } = useKnowledgeEngine();
  const [newTodo, setNewTodo] = useState('');
  const [optimisticUpdates, setOptimisticUpdates] = useState([]);

  // Query all todos
  const { data: todoTriples } = useTriples({
    predicate: `${TODO_NS}text`
  });

  // Subscribe to real-time changes
  const changes = useChangeFeed({
    filter: (change) => {
      // Only listen to todo-related changes
      return change.triple.subject.startsWith(TODO_NS);
    },
    onUpdate: (change) => {
      console.log('Real-time update:', change);
      // Remove optimistic update if confirmed
      if (change.type === 'add') {
        setOptimisticUpdates(prev =>
          prev.filter(id => id !== change.triple.subject)
        );
      }
    }
  });

  // Extract todos from triples
  const todos = todoTriples?.map(triple => ({
    id: triple.subject,
    text: triple.object,
    completed: false // We'll add this in a moment
  })) || [];

  // Add optimistic todos (not yet confirmed)
  const allTodos = [
    ...optimisticUpdates.map(id => ({
      id,
      text: 'Adding...',
      completed: false,
      optimistic: true
    })),
    ...todos
  ];

  // Add new todo with optimistic UI
  const addTodo = useCallback(async () => {
    if (!ready || !newTodo.trim()) return;

    const todoId = `${TODO_NS}${Date.now()}`;

    // Optimistic update - show immediately
    setOptimisticUpdates(prev => [...prev, todoId]);
    setNewTodo('');

    try {
      // Add to knowledge graph
      await engine.addTriples([
        {
          subject: todoId,
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: `${TODO_NS}Task`
        },
        {
          subject: todoId,
          predicate: `${TODO_NS}text`,
          object: newTodo
        },
        {
          subject: todoId,
          predicate: `${TODO_NS}completed`,
          object: 'false'
        },
        {
          subject: todoId,
          predicate: `${TODO_NS}createdAt`,
          object: new Date().toISOString()
        }
      ]);
    } catch (error) {
      // Remove optimistic update on error
      setOptimisticUpdates(prev => prev.filter(id => id !== todoId));
      console.error('Failed to add todo:', error);
    }
  }, [ready, newTodo, engine]);

  // Delete todo
  const deleteTodo = useCallback(async (todoId) => {
    if (!ready) return;

    try {
      // Remove all triples for this todo
      const triplesToDelete = todoTriples?.filter(
        t => t.subject === todoId
      ) || [];

      await engine.deleteTriples(triplesToDelete);
    } catch (error) {
      console.error('Failed to delete todo:', error);
    }
  }, [ready, engine, todoTriples]);

  return (
    <div className="max-w-2xl mx-auto p-6">
      <div className="mb-8">
        <h1 className="text-3xl font-bold mb-2">Collaborative To-Do List</h1>
        <p className="text-gray-600">
          Open this page in multiple tabs to see real-time updates!
          {changes?.length > 0 && (
            <span className="ml-2 text-green-600 font-semibold">
              🟢 {changes.length} recent changes
            </span>
          )}
        </p>
      </div>

      {/* Add Todo Form */}
      <div className="mb-6 flex gap-2">
        <input
          type="text"
          value={newTodo}
          onChange={(e) => setNewTodo(e.target.value)}
          onKeyPress={(e) => e.key === 'Enter' && addTodo()}
          placeholder="What needs to be done?"
          className="flex-1 px-4 py-2 border rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
          disabled={!ready}
        />
        <button
          onClick={addTodo}
          disabled={!ready || !newTodo.trim()}
          className="px-6 py-2 bg-blue-500 text-white rounded-lg hover:bg-blue-600 disabled:bg-gray-300"
        >
          Add
        </button>
      </div>

      {/* Todo List */}
      <div className="space-y-2">
        {allTodos.length === 0 ? (
          <p className="text-center text-gray-500 py-8">
            No todos yet. Add one to get started!
          </p>
        ) : (
          allTodos.map((todo) => (
            <div
              key={todo.id}
              className={`flex items-center gap-3 p-4 border rounded-lg ${
                todo.optimistic ? 'bg-blue-50 border-blue-200' : 'bg-white'
              }`}
            >
              <input
                type="checkbox"
                checked={todo.completed}
                className="w-5 h-5"
                disabled={todo.optimistic}
              />
              <span className={`flex-1 ${todo.completed ? 'line-through text-gray-500' : ''}`}>
                {todo.text}
              </span>
              {todo.optimistic && (
                <span className="text-xs text-blue-600">Syncing...</span>
              )}
              {!todo.optimistic && (
                <button
                  onClick={() => deleteTodo(todo.id)}
                  className="text-red-500 hover:text-red-700 text-sm"
                >
                  Delete
                </button>
              )}
            </div>
          ))
        )}
      </div>

      {/* Change Feed Monitor */}
      <div className="mt-8 p-4 bg-gray-50 rounded-lg">
        <h3 className="font-semibold mb-2">Recent Changes (last 5)</h3>
        <div className="space-y-1 text-sm font-mono">
          {changes?.slice(-5).reverse().map((change, idx) => (
            <div key={idx} className="flex gap-2">
              <span className={change.type === 'add' ? 'text-green-600' : 'text-red-600'}>
                {change.type === 'add' ? '+' : '-'}
              </span>
              <span className="text-gray-600 truncate">
                {change.triple.predicate.split('/').pop()}: {change.triple.object}
              </span>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
```

**What you just did:** Created a collaborative to-do list with:
- **Real-time updates** using `useChangeFeed`
- **Optimistic UI** for instant feedback
- **Change monitoring** to see what's happening
- **Multi-tab sync** automatic across browser tabs

---

## Step 3: Test Multi-Tab Collaboration

1. Run your app: `npm run dev`
2. Open [http://localhost:3000](http://localhost:3000)
3. Duplicate the tab (Cmd+T or Ctrl+T, then navigate to same URL)
4. Add a todo in one tab
5. **Watch it appear instantly in the other tab!**

---

## Step 4: Add Completion Tracking

Let's add the ability to mark todos as complete. Update the component:

```jsx
// Add this function inside CollaborativeTodoList component
const toggleComplete = useCallback(async (todoId, currentStatus) => {
  if (!ready) return;

  try {
    // Delete old status triple
    await engine.deleteTriples([{
      subject: todoId,
      predicate: `${TODO_NS}completed`,
      object: String(currentStatus)
    }]);

    // Add new status triple
    await engine.addTriples([{
      subject: todoId,
      predicate: `${TODO_NS}completed`,
      object: String(!currentStatus)
    }]);
  } catch (error) {
    console.error('Failed to toggle todo:', error);
  }
}, [ready, engine]);

// Update the checkbox in the render section
<input
  type="checkbox"
  checked={todo.completed}
  onChange={() => toggleComplete(todo.id, todo.completed)}
  className="w-5 h-5"
  disabled={todo.optimistic}
/>
```

Now when you check a todo in one tab, it updates in all tabs instantly!

---

## Step 5: Add User Presence

Let's show who else is viewing the list. Add this to your component:

```jsx
'use client';

import { useKnowledgeEngine, useChangeFeed, useTriples, usePresence } from 'unrdf-react';
import { useState, useCallback, useEffect } from 'react';

// ... (previous imports and constants)

export default function CollaborativeTodoList() {
  // ... (previous state and hooks)

  // Track user presence
  const { users, announce } = usePresence({
    channel: 'todo-list',
    metadata: {
      color: `#${Math.floor(Math.random()*16777215).toString(16)}` // Random color
    }
  });

  // Announce presence on mount
  useEffect(() => {
    if (ready) {
      announce({ status: 'active' });
    }
  }, [ready, announce]);

  return (
    <div className="max-w-2xl mx-auto p-6">
      {/* Add this after the title */}
      <div className="mb-4 flex items-center gap-2">
        <span className="text-sm text-gray-600">Active users:</span>
        {users.map((user, idx) => (
          <div
            key={user.id}
            className="w-8 h-8 rounded-full flex items-center justify-center text-white text-xs font-semibold"
            style={{ backgroundColor: user.metadata.color }}
            title={user.id}
          >
            {idx + 1}
          </div>
        ))}
      </div>

      {/* ... rest of component */}
    </div>
  );
}
```

**What you just did:** Added user presence tracking so you can see who else is viewing the list.

---

## What You Learned

✅ **Real-time updates** - Used `useChangeFeed` for instant synchronization
✅ **Optimistic UI** - Showed changes immediately before server confirmation
✅ **Change monitoring** - Tracked individual graph modifications
✅ **Multi-user collaboration** - Built presence awareness

---

## Try It Yourself

Enhance the collaborative app with:

1. **User names:** Add a name input and show who added each todo
2. **Typing indicators:** Show when someone is typing a new todo
3. **Conflict resolution:** Handle when two users edit the same todo
4. **Undo/Redo:** Use the change feed history for undo functionality
5. **Offline support:** Queue changes when offline and sync when reconnected

---

## Advanced: Custom Change Feed Filters

You can filter changes more precisely:

```jsx
const changes = useChangeFeed({
  // Only listen to adds, ignore deletes
  filter: (change) => change.type === 'add',

  // Only listen to specific predicates
  predicateFilter: [`${TODO_NS}text`, `${TODO_NS}completed`],

  // Throttle updates to avoid overwhelming UI
  throttle: 100, // ms

  // Keep last N changes in memory
  maxHistory: 50
});
```

---

## What's Next?

- [Tutorial 4: Advanced Visualizations](./04-advanced-visualizations.md)
- [How-to Guide: Implement Offline Sync](../how-to/offline-sync.md)
- [How-to Guide: Handle Conflicts](../how-to/handle-conflicts.md)
- [Reference: Streaming Hooks API](../reference/streaming-hooks.md)

---

## Troubleshooting

**Changes not syncing between tabs:**
- Check that both tabs are using the same knowledge engine instance
- Verify `useChangeFeed` is subscribed (check console logs)
- Ensure changes are actually being saved to the graph

**Optimistic updates stuck as "Syncing...":**
- Check browser console for errors during `addTriples`
- Verify the `onUpdate` callback is removing optimistic IDs
- Make sure triple subjects match between optimistic and real updates

**Performance degradation with many changes:**
- Use `throttle` option to reduce update frequency
- Limit `maxHistory` to keep memory usage low
- Consider debouncing user input before adding triples
