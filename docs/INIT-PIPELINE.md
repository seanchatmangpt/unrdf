# UNRDF Project Initialization Pipeline

Complete documentation for the UNRDF initialization pipeline—the "instant and complete" wiring system.

## Overview

The UNRDF init pipeline is a **9-phase orchestration engine** that takes a project from "raw code" to "fully modeled and wired" in one coherent flow:

```
npx unrdf init → what the hell just happened? it's all wired.
```

## What Happens During Init

### Phase 1: Stack Profile Recognition
**Detects** what kind of project this is automatically.

- Scans `package.json` dependencies
- Examines key files (`next.config.*`, `nest-cli.json`, etc.)
- Returns: `profileId` like `'react-next-app-router'`, `'react-spa'`, `'nest-rest'`, etc.

**Why it matters:** Picks which patterns and templates are relevant for this specific stack.

### Phase 2: Domain Model Inference
**Extracts** a usable domain ontology from existing code.

- Scans for Zod schemas, TS types, ORM models (Prisma, TypeORM, Sequelize)
- Detects API DTOs and request/response types
- Returns: Domain Store with entities, fields, types, and relationships

**Example output:**
```turtle
dom:User rdf:type dom:Entity
dom:User dom:hasField dom:User.email
dom:User.email dom:fieldType xsd:string
dom:User dom:relatesTo dom:Order
```

### Phase 3: File Pattern Induction
**Learns** generator templates from existing code families.

- Groups similar files (components, routes, tests, docs)
- Identifies variable parts and invariants
- Extracts output patterns: `src/features/{{entity}}/{{Entity}}Page.tsx`

**Example:**
```turtle
gen:ComponentTemplate rdf:type gen:Template
gen:ComponentTemplate gen:outputPattern "src/features/{{entity}}/{{Entity}}.tsx"
gen:ComponentTemplate gen:producesRole unproj:Component
```

### Phase 4: Filesystem Graph Construction
**Builds** a project structure ontology from the file system.

- Creates RDF triples for every file, folder, and role
- Associates files with features
- Marks test coverage, orphaned files, missing roles

### Phase 5: Domain + Files Linking
**Connects** domain entities to project files.

- Maps `User` entity to `src/features/user/*` files
- Identifies which entities are missing views/APIs/tests
- Builds feature-to-entity relationships

### Phase 6: Baseline Snapshot Creation
**Captures** a hash of the current state.

- Encodes: FS structure + domain ontology + template mappings
- Stores hash + timestamp for drift detection
- Enables: `unrdf drift --check` later

### Phase 7: Automatic Hook Derivation
**Establishes** invariants that keep code and model aligned.

Derived hooks enforce patterns like:
- "Every Feature must have at least one View"
- "Every Feature with API must have Test"
- "Files under features/* must belong to some Feature"
- Stack-specific rules (e.g., Next.js app router structure)

### Phase 8: Policy Pack Registration
**Registers** the derived hooks with the Knowledge Engine.

- Hooks are transaction hooks that validate on write
- Violations raise clear errors
- Developers can't accidentally break structure

### Phase 9: Project Report Generation
**Produces** a human-readable summary.

- Lists features, files by role, test coverage
- Identifies missing pieces (features without tests, orphaned files)
- Suggests next steps

## CLI Usage

### Quick Start
```bash
npx unrdf init
```
Initializes current directory with full wiring.

### With Options
```bash
# Initialize specific path
npx unrdf init --root /path/to/project

# Preview without applying hooks
npx unrdf init --dry-run

# Verbose output with phase details
npx unrdf init --verbose

# Skip certain phases
npx unrdf init --skip-snapshot --skip-hooks
```

### Output Example
```
======================================================================
  PROJECT INITIALIZATION REPORT
======================================================================

📦 Tech Stack: react-next-app-router + vitest
   └─ Detected: next, vitest, pnpm

🎯 Features: 12
   └─ Component: 45
   └─ Api: 38
   └─ Test: 32

   ⚠️  Missing Tests: dashboard, notifications

📊 Domain Model: 18 entities
   └─ User (8 fields)
   └─ Order (12 fields)
   └─ Product (6 fields)
   └─ ... and 15 more

📄 Files: 234
   └─ Component: 45
   └─ Api: 38
   └─ Test: 32

✅ Test Coverage: 82%

💡 Summary:
   Well-structured 12-feature project with 82% test coverage.
   Missing tests: dashboard, notifications. Docs needed: auth, payments.

======================================================================
✨ Initialization complete! Your project is now fully wired.
```

## Programmatic Usage

### Quick Pipeline
```javascript
import { createProjectInitializationPipeline } from 'unrdf/project-engine'

const result = await createProjectInitializationPipeline('/path/to/project', {
  verbose: true,
  dryRun: false,
  skipSnapshot: false,
  skipHooks: false,
})

// result = {
//   success: boolean,
//   receipt: { phases: {...}, totalDuration: number },
//   report: { summary, features, entities, stats },
//   state: { fsStore, projectStore, domainStore, templateGraph, snapshot }
// }
```

### Manual Step-by-Step
```javascript
import {
  scanFileSystemToStore,
  detectStackFromFs,
  buildProjectModelFromFs,
  classifyFiles,
  inferDomainModel,
  inferTemplatesFromProject,
  planMaterialization,
  applyMaterializationPlan,
  createStructureSnapshot,
  deriveHooksFromStructure,
  buildProjectReport,
} from 'unrdf/project-engine'

// Step 1: Filesystem
const fsStore = await scanFileSystemToStore('/path')

// Step 2: Stack
const stackProfile = detectStackFromFs({ fsStore })

// Step 3: Project model
const projectStore = buildProjectModelFromFs({ fsStore })

// ... etc
```

## Next Steps After Init

### 1. Verify Everything Works
```bash
pnpm test
pnpm build
```

### 2. Check Drift
```bash
# Later, verify code hasn't drifted from the model
unrdf drift --check
```

### 3. Generate New Features
```bash
# Generate components, APIs, tests for new domain entities
unrdf generate --entity NewFeature
```

### 4. Sync Model with Code
```bash
# If you manually edited the project, re-sync
unrdf sync
```

## Architecture

### Data Flow

```
Filesystem
    ↓
scanFileSystemToStore → fsStore
    ↓
[+] detectStackFromFs → stackProfile
[+] buildProjectModelFromFs → projectStore
[+] classifyFiles → role annotations
[+] inferDomainModel → domainStore
[+] inferTemplatesFromProject → templateGraph
[+] createStructureSnapshot → snapshot
[+] deriveHooksFromStructure → hooks[]
[+] buildProjectReport → report
    ↓
Result: { receipt, report, state }
```

### Stores Involved

| Store | Purpose | Built By |
|-------|---------|----------|
| `fsStore` | Filesystem graph (files, folders, paths) | `scanFileSystemToStore` |
| `projectStore` | Project structure (features, modules, roles) | `buildProjectModelFromFs` + `classifyFiles` |
| `domainStore` | Domain ontology (entities, fields, relations) | `inferDomainModel` |
| `templateGraph` | Code generation patterns | `inferTemplatesFromProject` |
| `snapshot` | Baseline for drift detection | `createStructureSnapshot` |

### Receipts for Audit Trail

Each phase returns a receipt with:
- `success`: boolean
- `duration`: milliseconds
- `data`: phase-specific output

Combined into single `result.receipt` with audit trail.

## Ontologies Involved

### unfs (Filesystem Ontology)
- Classes: `File`, `Folder`, `Path`
- Properties: `relativePath`, `size`, `modifiedAt`

### unproj (Project Structure Ontology)
- Classes: `Feature`, `Module`, `FileGroup`, roles like `View`, `Api`, `Test`
- Properties: `hasFeature`, `hasFile`, `belongsToFeature`, `hasRole`

### dom (Domain Ontology)
- Classes: `Entity`, `Field`, `Relationship`
- Properties: `hasField`, `fieldType`, `relatesTo`

### gen (Generator Ontology)
- Classes: `Template`, `Variable`
- Properties: `outputPattern`, `targetsClass`, `producesRole`

## Advanced: Extending Init

### Custom Stack Detection
```javascript
import { detectStackFromFs } from 'unrdf/project-engine'

const stackProfile = detectStackFromFs({ fsStore })
// returns: { uiFramework, webFramework, apiFramework, testFramework, packageManager }

// Use for custom logic
if (stackProfile.webFramework === 'next') {
  // Next.js-specific handling
}
```

### Custom Domain Inference
```javascript
import { inferDomainModel } from 'unrdf/project-engine'

const { store: domainStore, summary } = await inferDomainModel(fsStore, stackProfile)

// Add custom entities
const customEntity = namedNode('http://example.org/domain#CustomEntity')
domainStore.addQuad(
  customEntity,
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://example.org/domain#Entity')
)
```

### Custom Hook Rules
```javascript
import { deriveHooksFromStructure } from 'unrdf/project-engine'

const hooks = deriveHooksFromStructure(projectStore, stackProfile)

// Add custom hooks before registering
const customHook = defineHook({
  name: 'my-custom-rule',
  triggeredBy: 'quadAdded',
  condition: (context) => { /* ... */ },
  effect: (context) => { /* ... */ }
})

hooks.push(customHook)

// Register all
const manager = new KnowledgeHookManager()
hooks.forEach(h => manager.addKnowledgeHook(h))
```

## Troubleshooting

### Init fails with "Cannot read property 'getQuads'"
**Cause:** Zod validation is stripping prototype from N3 Store.
**Fix:** Use `z.custom()` instead of `z.object({}).passthrough()`.

### Domain model is empty
**Cause:** No Zod schemas, TS types, or ORM models detected.
**Fix:** Check that schema files are in expected locations:
- Zod: `src/schemas/`, `lib/schemas/`, `src/types/`
- Prisma: `prisma/schema.prisma`
- TypeORM: `src/**/*.entity.ts`
- TypeScript: `src/**/*.ts` with `type X = {}` or `interface X {}`

### Hooks validation errors after init
**Cause:** Derived hooks are too strict for your actual code patterns.
**Fix:** Skip hook registration with `--skip-hooks` and register manually:
```bash
npx unrdf init --skip-hooks
# Then manually curate and register hooks
```

## Performance

Typical performance (measured on UNRDF itself):
- FS scan: ~150ms (2865 files)
- Stack detection: ~4ms
- Project model: ~5ms
- File classification: ~17ms
- Domain inference: ~1ms
- Template inference: ~0ms
- Snapshot: ~11ms
- Hook derivation: ~0ms
- Report: ~1ms
- **Total: ~190ms**

## See Also

- [Project Engine Architecture](./docs/PROJECT-ENGINE.md)
- [Knowledge Hooks Reference](./docs/KNOWLEDGE-HOOKS.md)
- [Domain Modeling Guide](./docs/DOMAIN-MODELING.md)
- [CLI Command Reference](./docs/CLI.md)
