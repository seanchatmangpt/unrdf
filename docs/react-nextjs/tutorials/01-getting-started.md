# Tutorial: Getting Started with UNRDF React Hooks

**Learning Objectives:**
- Set up your first UNRDF React application
- Create a simple knowledge graph
- Query and display RDF data
- Understand the core hooks

**Prerequisites:**
- Node.js 18+ installed
- Basic knowledge of React
- 15 minutes

---

## Step 1: Create a New React Application

First, let's create a new Next.js application with the UNRDF React package:

```bash
# Create a new Next.js app
npx create-next-app@latest my-unrdf-app
cd my-unrdf-app

# Install UNRDF React
npm install unrdf-react
```

**What you just did:** Created a Next.js project and installed the UNRDF React hooks library.

---

## Step 2: Set Up the Knowledge Engine Provider

Open `app/layout.jsx` and wrap your application with the `KnowledgeEngineProvider`:

```jsx
import { KnowledgeEngineProvider } from 'unrdf-react';
import './globals.css';

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body>
        <KnowledgeEngineProvider>
          {children}
        </KnowledgeEngineProvider>
      </body>
    </html>
  );
}
```

**What you just did:** Added the UNRDF context provider that makes the knowledge engine available to all components.

---

## Step 3: Create Your First Knowledge Graph Component

Create a new file `app/components/PersonProfile.jsx`:

```jsx
'use client';

import { useKnowledgeEngine, useTriples } from 'unrdf-react';
import { useState } from 'react';

export default function PersonProfile() {
  const { engine, ready } = useKnowledgeEngine();
  const [name, setName] = useState('');
  const [email, setEmail] = useState('');

  // Query all person triples
  const { data: people } = useTriples({
    predicate: 'http://xmlns.com/foaf/0.1/name'
  });

  const addPerson = async () => {
    if (!ready || !name || !email) return;

    const personId = `http://example.org/people/${Date.now()}`;

    // Add triples to the knowledge graph
    await engine.addTriples([
      {
        subject: personId,
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://xmlns.com/foaf/0.1/Person'
      },
      {
        subject: personId,
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: name
      },
      {
        subject: personId,
        predicate: 'http://xmlns.com/foaf/0.1/mbox',
        object: `mailto:${email}`
      }
    ]);

    // Clear form
    setName('');
    setEmail('');
  };

  return (
    <div className="p-6 max-w-2xl mx-auto">
      <h1 className="text-2xl font-bold mb-6">Person Directory</h1>

      {/* Add Person Form */}
      <div className="mb-8 p-4 border rounded">
        <h2 className="text-xl mb-4">Add New Person</h2>
        <input
          type="text"
          placeholder="Name"
          value={name}
          onChange={(e) => setName(e.target.value)}
          className="border p-2 mr-2 rounded"
        />
        <input
          type="email"
          placeholder="Email"
          value={email}
          onChange={(e) => setEmail(e.target.value)}
          className="border p-2 mr-2 rounded"
        />
        <button
          onClick={addPerson}
          disabled={!ready}
          className="bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600 disabled:bg-gray-300"
        >
          Add Person
        </button>
      </div>

      {/* Display People */}
      <div>
        <h2 className="text-xl mb-4">People ({people?.length || 0})</h2>
        {people?.map((triple, idx) => (
          <div key={idx} className="p-3 mb-2 border rounded">
            <p className="font-semibold">{triple.object}</p>
            <p className="text-sm text-gray-600">{triple.subject}</p>
          </div>
        ))}
      </div>
    </div>
  );
}
```

**What you just did:** Created a component that:
- Uses `useKnowledgeEngine` to access the RDF store
- Uses `useTriples` to query for person names
- Adds new people to the knowledge graph
- Displays all people in the graph

---

## Step 4: Use the Component

Update `app/page.jsx` to use your new component:

```jsx
import PersonProfile from './components/PersonProfile';

export default function Home() {
  return <PersonProfile />;
}
```

---

## Step 5: Run Your Application

```bash
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) in your browser.

**What you should see:**
- A form to add people
- A list of people (initially empty)
- When you add a person, they appear in the list immediately

---

## What You Learned

✅ **Set up UNRDF React** - Installed the package and configured the provider
✅ **Created RDF triples** - Used the knowledge engine to add structured data
✅ **Queried the graph** - Used `useTriples` to retrieve data reactively
✅ **Built a reactive UI** - React hooks automatically re-render when data changes

---

## Try It Yourself

Experiment with these modifications:

1. **Add more fields:** Try adding a phone number or address field
2. **Filter the list:** Add a search box to filter people by name
3. **Delete people:** Add a delete button for each person
4. **Use SPARQL:** Replace `useTriples` with `useSPARQLQuery` for more complex queries

---

## What's Next?

- [Tutorial 2: Building a Knowledge Graph Explorer](./02-knowledge-graph-explorer.md)
- [Tutorial 3: Real-time Collaboration with Streaming](./03-real-time-streaming.md)
- [How-to Guide: Query with SPARQL](../how-to/query-with-sparql.md)
- [Reference: Core Hooks API](../reference/core-hooks.md)

---

## Troubleshooting

**"engine is null" error:**
- Make sure `KnowledgeEngineProvider` wraps your component
- Check that you're using `'use client'` directive in client components

**Data not updating:**
- Ensure you're awaiting async operations
- Check browser console for errors
- Verify the `ready` state before operations

**Build errors:**
- Run `npm install` to ensure dependencies are installed
- Clear `.next` folder and rebuild: `rm -rf .next && npm run dev`
