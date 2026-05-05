# UNRDF Ecosystem Roadmap

**Last Updated:** March 15, 2026

---

## Overview

UNRDF is evolving from a monolithic library into an **ecosystem of composable packages**. This roadmap outlines planned packages, timelines, and dependencies.

**Core Philosophy:** Small, focused packages that work together seamlessly.

---

## Package Overview

| Package | Status | Release | Description |
|---------|--------|---------|-------------|
| **unrdf** | ✅ Shipped | vlatest (Mar 2026) | Core RDF library |
| **@unrdf/cli** | 📋 Designed | Q2 2026 | Command-line interface |
| **@unrdf/knowledge-engine** | 📋 Designed | Q2 2026 | gRPC server |
| **@unrdf/web** | 📋 Planned | Q3 2026 | REST API server |
| **@unrdf/react** | 📋 Planned | Q3 2026 | React hooks |
| **@unrdf/vue** | 📋 Planned | Q3 2026 | Vue composables |
| **@unrdf/graphql** | 📋 Planned | Q4 2026 | GraphQL gateway |
| **@unrdf/ui** | 📋 Planned | Q4 2026 | Graph explorer UI |
| **@unrdf/lsp** | 🔮 Future | 2027 | Language server protocol |
| **@unrdf/vscode** | 🔮 Future | 2027 | VS Code extension |

**Legend:**
- ✅ Shipped
- 📋 Design complete, implementation in progress
- 🔮 Planned, design pending

---

## Timeline

### Q2 2026

#### @unrdf/cli vlatest
**Status:** 📋 Design complete (see [CLI-PACKAGE-DESIGN.md](./CLI-PACKAGE-DESIGN.md))

**Features:**
- Core commands (parse, query, validate, convert)
- Interactive init with templates
- Development server with web UI
- Shell completions (bash, zsh, fish)
- Hook management commands

**Dependencies:**
- `unrdf@^latest`
- `citty@^latest`
- `consola@^latest`

**Milestone:** Beta release May 2026, GA June 2026

#### @unrdf/knowledge-engine vlatest
**Status:** 📋 Design in progress

**Features:**
- gRPC server for remote RDF operations
- Multi-language client support (Python, Go, Rust)
- Distributed query execution
- Cluster mode (multiple knowledge-engine instances)
- Authentication and authorization

**Dependencies:**
- `unrdf@^latest`
- `@grpc/grpc-js@^latest`
- `@grpc/proto-loader@^latest`

**Milestone:** Beta release June 2026, GA July 2026

---

### Q3 2026

#### @unrdf/web vlatest
**Status:** 📋 Planned

**Features:**
- REST API server for RDF operations
- OpenAPI latest documentation
- WebSocket support for real-time updates
- Rate limiting and caching
- Swagger UI for API exploration
- JWT authentication

**Dependencies:**
- `unrdf@^latest`
- `hono@^latest` (or Express)
- `swagger-ui-express@^latest`

**API Routes:**
```
GET    /api/query              # SPARQL query
POST   /api/data               # Add RDF data
GET    /api/data/:graph        # Get graph data
DELETE /api/data/:graph        # Delete graph
POST   /api/validate           # SHACL validation
GET    /api/schemas            # List SHACL shapes
WS     /api/subscribe          # Real-time updates
```

**Milestone:** Beta release Aug 2026, GA Sep 2026

#### @unrdf/react vlatest
**Status:** 📋 Planned

**Features:**
- React hooks for RDF operations
- Query result caching
- Optimistic updates
- Suspense support
- TypeScript definitions

**Hooks:**
```tsx
import { useQuery, useMutation, useKnowledgeEngine } from '@unrdf/react';

function MyComponent() {
  const { engine } = useKnowledgeEngine();

  const { data, loading, error } = useQuery({
    query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
    type: 'sparql-select'
  });

  const [addData] = useMutation({
    onSuccess: () => console.log('Data added')
  });

  return (
    <div>
      {loading && <p>Loading...</p>}
      {error && <p>Error: {error.message}</p>}
      {data && <Table data={data} />}
    </div>
  );
}
```

**Dependencies:**
- `unrdf@^latest`
- `react@^latest`

**Milestone:** Beta release Aug 2026, GA Sep 2026

#### @unrdf/vue vlatest
**Status:** 📋 Planned

**Features:**
- Vue 3 composables for RDF operations
- Auto-unwrapping refs
- Suspense support
- TypeScript definitions

**Composables:**
```vue
<script setup>
import { useQuery, useMutation, useKnowledgeEngine } from '@unrdf/vue';

const { engine } = useKnowledgeEngine();

const { data, loading, error } = useQuery({
  query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
  type: 'sparql-select'
});

const { mutate: addData } = useMutation({
  onSuccess: () => console.log('Data added')
});
</script>

<template>
  <div>
    <p v-if="loading">Loading...</p>
    <p v-if="error">Error: {{ error.message }}</p>
    <Table v-if="data" :data="data" />
  </div>
</template>
```

**Dependencies:**
- `unrdf@^latest`
- `vue@^latest`

**Milestone:** Beta release Aug 2026, GA Sep 2026

---

### Q4 2026

#### @unrdf/graphql vlatest
**Status:** 📋 Planned

**Features:**
- GraphQL gateway for RDF data
- Auto-generate GraphQL schema from RDF ontology
- SPARQL to GraphQL query translation
- Real-time subscriptions (GraphQL subscriptions)
- Federation support

**Example:**
```graphql
# Auto-generated from FOAF ontology
type Person {
  id: ID!
  name: String!
  age: Int
  knows: [Person!]!
}

type Query {
  person(id: ID!): Person
  persons(filter: PersonFilter): [Person!]!
}

type Mutation {
  createPerson(input: CreatePersonInput!): Person!
  updatePerson(id: ID!, input: UpdatePersonInput!): Person!
  deletePerson(id: ID!): Boolean!
}

type Subscription {
  personAdded: Person!
  personUpdated: Person!
}
```

**Dependencies:**
- `unrdf@^latest`
- `graphql@^latest`
- `@graphql-tools/schema@^latest`

**Milestone:** Beta release Nov 2026, GA Dec 2026

#### @unrdf/ui vlatest
**Status:** 📋 Planned

**Features:**
- Interactive graph explorer
- Visual SPARQL query builder
- SHACL shape editor
- Real-time collaboration
- Import/export RDF data
- Visual diff for changes

**Tech Stack:**
- React + TypeScript
- D3.js for graph visualization
- Monaco Editor for SPARQL
- Tailwind CSS for styling

**Screenshots:** (Mockups to be added)

**Dependencies:**
- `unrdf@^latest`
- `@unrdf/react@^latest`
- `@unrdf/web@^latest` (backend)
- `react@^latest`
- `d3@^latest`

**Milestone:** Beta release Nov 2026, GA Dec 2026

---

### 2027

#### @unrdf/lsp vlatest
**Status:** 🔮 Future

**Features:**
- Language server for RDF formats (Turtle, SPARQL)
- Autocomplete for prefixes and URIs
- Syntax highlighting
- Error diagnostics
- Go-to-definition
- Find references
- Rename refactoring

**Protocol Support:**
- Language Server Protocol latest
- Debug Adapter Protocol

**Editor Integration:**
- VS Code
- Vim/Neovim
- Emacs
- Sublime Text
- IntelliJ IDEA

**Milestone:** GA Q2 2027

#### @unrdf/vscode vlatest
**Status:** 🔮 Future

**Features:**
- VS Code extension using @unrdf/lsp
- RDF file templates
- SPARQL query runner
- Inline SHACL validation
- Graph visualization panel
- Debugger for Knowledge Hooks

**Marketplace:** https://marketplace.visualstudio.com/items?itemName=unrdf.vscode

**Milestone:** GA Q2 2027

---

## Package Dependencies

### Dependency Graph

```
unrdf (core)
├── @unrdf/cli
├── @unrdf/knowledge-engine
├── @unrdf/web
│   └── @unrdf/ui (uses web as backend)
├── @unrdf/react
│   └── (optional) @unrdf/web (for remote data)
├── @unrdf/vue
│   └── (optional) @unrdf/web (for remote data)
├── @unrdf/graphql
│   └── (optional) @unrdf/web (for HTTP transport)
└── @unrdf/lsp
    └── @unrdf/vscode
```

### Version Compatibility

| Package Version | Core Version | Notes |
|----------------|--------------|-------|
| @unrdf/cli@1.x | unrdf@^latest | Requires vlatest+ for browser features |
| @unrdf/knowledge-engine@1.x | unrdf@^latest | Requires vlatest+ for isolated-vm |
| @unrdf/web@1.x | unrdf@^latest | Compatible with vlatest+ |
| @unrdf/react@1.x | unrdf@^latest | Works with vlatest+, optimized for vlatest+ |
| @unrdf/vue@1.x | unrdf@^latest | Works with vlatest+, optimized for vlatest+ |
| @unrdf/graphql@1.x | unrdf@^latest | Compatible with vlatest+ |
| @unrdf/ui@1.x | unrdf@^latest | Requires @unrdf/web@^latest |

---

## Release Coordination

### Coordinated Releases

Some packages will be released together for feature parity:

**Q2 2026 Bundle:**
- @unrdf/cli@latest
- @unrdf/knowledge-engine@latest

**Q3 2026 Bundle:**
- @unrdf/web@latest
- @unrdf/react@latest
- @unrdf/vue@latest

**Q4 2026 Bundle:**
- @unrdf/graphql@latest
- @unrdf/ui@latest

### Release Process

1. **Design:** Create design document (like CLI-PACKAGE-DESIGN.md)
2. **Feedback:** 2-week community feedback period
3. **Implementation:** Build package with tests
4. **Beta:** Release beta version for testing
5. **RC:** Release candidate after beta testing
6. **GA:** General availability after RC stabilization

### Semantic Versioning

All packages follow [SemVer latest](https://semver.org/):

- **MAJOR:** Breaking changes
- **MINOR:** New features (backward compatible)
- **PATCH:** Bug fixes

**Example:**
```
@unrdf/cli@latest
            │ │ └─ Patch: Bug fix
            │ └─── Minor: New feature
            └───── Major: Breaking change
```

---

## Community Feedback

### How to Provide Feedback

**Design Phase (before implementation):**
- Comment on design documents (GitHub Discussions)
- Open issues with "enhancement" label
- Participate in RFC discussions

**Beta Phase (during testing):**
- Report bugs with "beta" label
- Request features with "enhancement" label
- Share use cases and requirements

**Post-Release:**
- Open issues for bugs
- Request features for next version
- Contribute PRs

### Priority Process

Feature prioritization based on:
1. **Community votes** - GitHub issue 👍 reactions
2. **Use case impact** - How many users benefit
3. **Complexity** - Implementation effort
4. **Dependencies** - Blocking other features

---

## Contributing to Ecosystem

### Package Development Guidelines

**1. Design First**
- Create design document
- Get community feedback
- Iterate on design

**2. Test-Driven Development**
- Write tests first
- Aim for 90%+ coverage
- Include E2E tests

**3. Documentation**
- README with examples
- API documentation (JSDoc)
- Migration guides (if breaking changes)

**4. Compatibility**
- Maintain compatibility with core
- Support latest 2 major Node.js versions
- Support latest browsers (Chrome, Firefox, Safari, Edge)

### Submitting New Package Ideas

1. **Open Discussion:** Start GitHub Discussion with proposal
2. **Write RFC:** If proposal gets support, write RFC (Request for Comments)
3. **Prototype:** Build proof-of-concept
4. **Proposal PR:** Submit design document as PR
5. **Implementation:** After approval, implement package

**Template for new package proposal:**
```markdown
# Package Proposal: @unrdf/{name}

## Problem
[What problem does this solve?]

## Solution
[How does this package solve it?]

## API Design
[Proposed API with examples]

## Dependencies
[What packages does it depend on?]

## Alternatives
[What alternatives were considered?]

## Community Feedback
[Link to discussion thread]
```

---

## Funding and Sponsorship

### Current Status
All packages are currently developed by volunteer contributors.

### Sponsorship Opportunities
- **GitHub Sponsors:** https://github.com/sponsors/unrdf
- **OpenCollective:** (Coming soon)

### Sponsor Benefits
- Priority feature requests
- Logo on website
- Early access to betas
- Dedicated support channel

---

## Long-Term Vision (2027+)

### Potential Future Packages

| Package | Description | Status |
|---------|-------------|--------|
| **@unrdf/mobile** | React Native components | 🔮 Exploring |
| **@unrdf/cloud** | Hosted RDF service | 🔮 Exploring |
| **@unrdf/federation** | Federated query engine | 🔮 Exploring |
| **@unrdf/ai** | AI-powered query suggestions | 🔮 Exploring |
| **@unrdf/migrate** | Data migration tools | 🔮 Exploring |
| **@unrdf/streaming** | Stream processing | 🔮 Exploring |

### Research Areas
- Vector embeddings for semantic search
- Natural language to SPARQL translation
- Automated ontology generation
- Real-time collaboration features
- Blockchain integration for provenance

---

## Summary

### 2026 Ecosystem Goals

**Q2 2026:**
- ✅ Core library (unrdf@latest) - **Shipped**
- 📋 CLI package (@unrdf/cli@latest)
- 📋 gRPC server (@unrdf/knowledge-engine@latest)

**Q3 2026:**
- 📋 REST API server (@unrdf/web@latest)
- 📋 React hooks (@unrdf/react@latest)
- 📋 Vue composables (@unrdf/vue@latest)

**Q4 2026:**
- 📋 GraphQL gateway (@unrdf/graphql@latest)
- 📋 Graph explorer UI (@unrdf/ui@latest)

**Total packages by end of 2026:** 8

---

## Questions?

- **GitHub Discussions:** https://github.com/unrdf/unrdf/discussions
- **Email:** maintainers@unrdf.org
- **Discord:** (Coming Q2 2026)

---

**Help shape the ecosystem!** 🚀

Open a discussion: https://github.com/unrdf/unrdf/discussions/new
