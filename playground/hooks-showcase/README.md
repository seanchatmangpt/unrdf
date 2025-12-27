# UNRDF Hooks Showcase

Interactive Next.js + shadcn/ui demo showcasing all 40 UNRDF React hooks.

## Quick Start

```bash
cd playground/hooks-showcase
pnpm install
pnpm dev
```

Open [http://localhost:3000](http://localhost:3000)

## Features

- **Interactive demos** for all 7 hook categories
- **shadcn/ui components** for beautiful, accessible UI
- **Dark theme** optimized for developer experience
- **Code examples** for each hook

## Hook Categories

| Category | Hooks | Tier | Usage |
|----------|-------|------|-------|
| **Core** | useKnowledgeEngine, useTransaction, ... | T1 | 40% |
| **Streaming** | useChangeFeed, useStreamProcessor, ... | T1 | 20% |
| **Dark Matter** | useDarkMatterCore, useQueryAnalyzer, ... | T1 | 15% |
| **Federation** | useFederatedSystem, useConsensusManager, ... | T3 | 5% |
| **Composition** | useKnowledgeStack, useOfflineStore | NEW | - |
| **Error & Recovery** | useErrorBoundary, useRecovery, ... | T2 | 15% |
| **Form & UI** | useSPARQLEditor, useGraphVisualizer, ... | T2 | 10% |

## Tech Stack

- **Next.js 14** - React framework
- **shadcn/ui** - UI component library
- **Tailwind CSS** - Styling
- **Radix UI** - Accessible primitives

## Structure

```
hooks-showcase/
├── app/
│   ├── layout.jsx      # Root layout
│   ├── page.jsx        # Main dashboard
│   └── globals.css     # Tailwind styles
├── components/
│   ├── demos/          # Hook demo components
│   │   ├── core-demo.jsx
│   │   ├── streaming-demo.jsx
│   │   ├── dark-matter-demo.jsx
│   │   ├── federation-demo.jsx
│   │   ├── composition-demo.jsx
│   │   ├── error-recovery-demo.jsx
│   │   └── form-ui-demo.jsx
│   └── ui/             # shadcn components
│       ├── button.jsx
│       ├── card.jsx
│       ├── tabs.jsx
│       └── badge.jsx
└── lib/
    └── utils.js        # Utility functions
```

## Development

The demos use mock implementations for browser compatibility.
In production, import from `unrdf/react-hooks`:

```javascript
// Production usage
import { useKnowledgeEngine, useChangeFeed } from 'unrdf/react-hooks';
```

## License

MIT
