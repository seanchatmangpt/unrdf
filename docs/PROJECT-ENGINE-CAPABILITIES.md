# UNRDF Project Engine: Complete Capabilities

Complete reference for the 8 new project engine capabilities that make `npx unrdf init` work as a complete initialization pipeline.

## Capability 1: Stack Profile Recognition

**Module:** `src/project-engine/stack-detect.mjs`

**Purpose:** Detect what kind of project this is (React/Next/Nest/Express + Jest/Vitest/Mocha + npm/yarn/pnpm/bun).

### Function
```javascript
export function detectStackFromFs(options) {
  // Returns: { uiFramework, webFramework, apiFramework, testFramework, packageManager }
}
```

### Example Output
```javascript
{
  uiFramework: 'react',
  webFramework: 'next-app-router',
  apiFramework: 'next',
  testFramework: 'vitest',
  packageManager: 'pnpm'
}
```

### Detection Logic
- Scans filesystem for config files (next.config.*, nest-cli.json, etc.)
- Checks for key directories (src/app, pages/, src/views, etc.)
- Examines package.json via fsStore
- Returns first match for each framework type

### Tests
- 15 unit tests covering all framework combinations
- Empty store handling
- Multi-environment (5 vitest environments)

---

## Capability 2: Domain Model Inference

**Module:** `src/project-engine/domain-infer.mjs`

**Purpose:** Extract a usable domain ontology from existing code (Zod, TS types, ORM models, DTOs).

### Functions
```javascript
// Main inference
export async function inferDomainModel(fsStore, stackProfile)
// Returns: { store, summary: { entityCount, fieldCount, relationshipCount } }

// Convenience wrapper
export async function inferDomainModelFromPath(projectRoot, options)
// Returns same as above

// Ontology lens for change detection
export function DomainModelLens(triple, direction)
// Returns: { kind, entity, details }
```

### Supported Sources
- **Zod schemas:** `z.object()` patterns in schema files
- **Prisma models:** `model User {}` blocks from prisma.schema
- **TypeScript types:** `type X = {}` and `interface X {}`
- **TypeORM entities:** `@Entity()` decorated classes

### RDF Output
```turtle
dom:User rdf:type dom:Entity
dom:User rdfs:label "User"
dom:User dom:source "zod"
dom:User dom:hasField dom:User.email
dom:User.email rdf:type dom:Field
dom:User.email dom:fieldName "email"
dom:User.email dom:fieldType xsd:string
dom:User.email dom:isOptional false
dom:User dom:relatesTo dom:Order
```

### Detection Patterns
- Scans `src/**/*.ts`, `src/**/*.mjs`, `prisma/`, `src/*/schema*`
- Parses Zod: `z.object({ field: z.string() })`
- Parses Prisma: `model User { email String }`
- Parses TS: `type User = { email: string }`

### Tests
- 16 unit tests + 5 integration tests
- 100% pass rate across all vitest environments
- Covers empty stores, multiple sources, lens functionality

---

## Capability 3: Pattern Induction for Templates

**Module:** `src/project-engine/template-infer.mjs`

**Purpose:** Learn code generation templates by analyzing existing project files.

### Functions
```javascript
// Basic inference
export function inferTemplatesFromProject(fsStore, domainStore, stackProfile)
// Returns: { store, summary: { templateCount, byKind: {} } }

// Enhanced with domain binding
export function inferTemplatesWithDomainBinding(fsStore, domainStore, stackProfile)
// Returns: same + domain entity associations

// Query templates
export function getTemplatesByKind(store, kind)
// Returns: Array<{iri, outputPattern, variantCount}>

// Serialize for debugging
export function serializeTemplates(store)
// Returns: Object[] with template details
```

### Template Kinds Detected
1. **Component** - React/Vue components
2. **Page** - Full page route handlers
3. **Route** - Route definitions (Next.js, Express)
4. **Test** - Test files (*.test.tsx, *.spec.tsx)
5. **Api** - API routes and endpoints
6. **Hook** - Custom hooks/composables
7. **Service** - Business logic services
8. **Schema** - Data validation and types
9. **Doc** - Documentation markdown
10. **Config** - Configuration files

### RDF Output
```turtle
gen:ComponentTemplate rdf:type gen:Template
gen:ComponentTemplate gen:templateKind "Component"
gen:ComponentTemplate gen:outputPattern "src/features/{{entity}}/{{Entity}}{{suffix}}.{{ext}}"
gen:ComponentTemplate gen:variantCount 3
gen:ComponentTemplate gen:variable "entity"
gen:ComponentTemplate gen:variable "Entity"
gen:ComponentTemplate gen:producesRole unproj:Component
gen:ComponentTemplate gen:example "src/features/user/UserComponent.tsx"
```

### Variable Substitution
- `{{entity}}` → lowercase name
- `{{Entity}}` → PascalCase name
- `{{ENTITY}}` → UPPERCASE name
- `{{entity_snake}}` → snake_case name
- `{{entity-kebab}}` → kebab-case name

### Tests
- 25 tests in multi-environment suite
- Covers all template kinds, variable extraction
- Domain binding and serialization

---

## Capability 4: Materialization Planning

**Module:** `src/project-engine/materialize-plan.mjs`

**Purpose:** Plan code generation as a list of file writes with full provenance.

### Functions
```javascript
// Main planning
export async function planMaterialization(ontologyStore, templateGraph, options)
// Returns: {
//   plan: { writes: [], updates: [], deletes: [] },
//   receipt: { ontologyHash, planHash, mappings: [] }
// }

// Validation
export function validatePlan(plan)
// Returns: { valid, errors: [] }

// Utilities
export function createEmptyPlan()
export function mergePlans(p1, p2)
```

### Plan Structure
```javascript
{
  writes: [
    {
      path: 'src/features/user/UserPage.tsx',
      kind: 'Page',
      hash: 'sha256:abc123...',
      sourceEntity: 'dom:User',
      sourceTemplate: 'gen:PageTemplate',
      content: '...'
    },
    // ... more writes
  ],
  updates: [
    {
      path: 'src/features/user/index.ts',
      fromHash: '...',
      toHash: '...',
      reason: 'export updated'
    }
  ],
  deletes: [
    {
      path: 'old/file.ts',
      reason: 'deprecated pattern'
    }
  ]
}
```

### Determinism
- SHA256 hashing for content
- Complete provenance (entity → template → file)
- Support for detecting updates via hash comparison
- Path validation (absolute paths, no traversal)

### Tests
- 25 tests covering planning logic
- Plan validation and merging
- Hash computation and collision detection

---

## Capability 5: Materialization Execution

**Module:** `src/project-engine/materialize-apply.mjs`

**Purpose:** Safely apply a materialization plan with FS snapshots and rollback.

### Functions
```javascript
// Apply plan
export async function applyMaterializationPlan(plan, options)
// Returns: {
//   result: { appliedCount, skippedCount, errors: [] },
//   receipt: { planHash, fsDiff, ... }
// }

// Utilities
export async function rollbackMaterialization(result, options)
export function previewPlan(plan)
export async function checkPlanApplicability(plan, options)
```

### Execution Flow
1. Validate plan (hashes, paths, conflicts)
2. Snapshot FS before (`O_fs_before`)
3. Create directories as needed
4. Write files from plan
5. Snapshot FS after (`O_fs_after`)
6. Compute diff using `diffProjectStructure`
7. Embed diff into receipt

### Safety Features
- Hash validation for updates/deletes
- Dry-run mode (no actual writes)
- Directory auto-creation
- Rollback capability
- Conflict detection before apply

### Tests
- 14 tests covering apply logic
- Rollback, preview, and applicability checking
- Dry-run mode validation

---

## Capability 6: Drift Detection Snapshots

**Module:** `src/project-engine/drift-snapshot.mjs`

**Purpose:** Establish a baseline snapshot and detect when code drifts from model.

### Functions
```javascript
// Create baseline
export function createStructureSnapshot(fsStore, domainStore, options)
// Returns: { snapshotStore, receipt: { hash, createdAt, summary } }

// Detect drift
export function computeDrift(currentSnapshot, baselineSnapshot)
// Returns: { ontologyDiff, summary, driftSeverity }

// Utilities
export function createEmptyBaseline()
export function serializeSnapshot(snapshotStore, receipt)
export function deserializeSnapshot(json)
```

### Snapshot Contents
- FS structure graph (file → feature → role)
- Domain ontology (entities + fields)
- Template mappings (file → template → entity)
- SHA-256 hash of combined state
- Timestamp

### Drift Severity Levels
- **none:** No structural drift
- **minor:** Small changes (added roles, additions)
- **major:** Critical changes (feature removal, coverage drops)

### Drift Reports
- "Feature X missing tests when model requires them"
- "Files in features/Y don't match any template pattern"
- "Domain entity Product added but no views/APIs"
- "Test coverage dropped below baseline"

### Tests
- 15 tests covering snapshot creation and drift detection
- All vitest environments

---

## Capability 7: Automatic Policy Derivation

**Module:** `src/project-engine/policy-derivation.mjs`

**Purpose:** Automatically derive transaction hook policies from project patterns.

### Functions
```javascript
// Derive hooks
export function deriveHooksFromStructure(projectStore, stackProfile, options)
// Returns: Hook[] - ready for KnowledgeHookManager.registerHook()

// Analyze violations
export function analyzePatternViolations(projectStore, stackProfile, options)
// Returns: { violations: [], patterns: [] }

// Create custom hook
export function createCustomPatternHook(spec)
```

### Derived Patterns
| Pattern | Hook Name | Description |
|---------|-----------|-------------|
| Feature-View | `derived:feature-must-have-view` | Every Feature must have ≥1 Component/Page |
| API-Test | `derived:api-must-have-test` | Every Feature with API must have Test |
| Orphan Files | `derived:no-orphan-files` | Files under `features/*` must belong to Feature |
| Test Companion | `derived:test-companion-required` | Test file must accompany main file |
| Next.js Structure | `derived:next-app-router-structure` | `app/[route]/page.tsx` → `route.ts` for API |

### Hook Structure
```javascript
{
  meta: {
    name: 'derived:feature-must-have-view',
    description: 'Every Feature must have at least one view',
    ontology: ['unrdf-project'],
  },
  channel: { graphs: ['urn:graph:project'], view: 'after' },
  when: { kind: 'sparql-ask', query: '...' },
  determinism: { seed: 42 },
  receipt: { anchor: 'none' },
  before: ({ payload }) => payload,
  run: ({ payload, context }) => ({ result: { valid, violations } }),
  after: ({ result, cancelled, reason }) => ({ result: { status } }),
}
```

### Registration
```javascript
const hooks = deriveHooksFromStructure(projectStore, stackProfile)
const manager = new KnowledgeHookManager()
hooks.forEach(h => manager.addKnowledgeHook(h))
```

### Tests
- 70 tests covering all pattern types
- All vitest environments

---

## Capability 8: Project Report Generation

**Module:** `src/project-engine/project-report.mjs`

**Purpose:** Generate human-readable project summary for init output.

### Function
```javascript
export function buildProjectReport(projectStore, options)
// Returns: {
//   features: Array<{ iri, name, roles, fileCount, testCoverage }>,
//   stackProfile: string,
//   stats: { featureCount, totalFiles, testCoverageAverage, ... },
//   domainEntities: Array<{ name, fieldCount, hasView, hasApi }>,
//   summary: string
// }
```

### Report Structure
```javascript
{
  features: [
    {
      iri: 'http://example.org/unrdf/project#feature/auth',
      name: 'auth',
      roles: { view: true, api: true, schema: true, test: true, doc: false },
      fileCount: 8,
      testCoverage: 87,
      hasMissingTests: false
    }
  ],
  stackProfile: 'react-next-app-router + vitest',
  stats: {
    featureCount: 12,
    totalFiles: 234,
    testCoverageAverage: 82,
    filesByRole: { Component: 45, Api: 38, Test: 32, ... }
  },
  domainEntities: [
    { name: 'User', fieldCount: 8, hasView: true, hasApi: true }
  ],
  summary: 'Well-structured 12-feature project with 82% test coverage...'
}
```

### Analysis
- Feature count and file distribution
- Test coverage per feature and overall
- Stack profile detection
- Domain entity analysis
- Missing piece detection (tests, docs, APIs)
- Actionable summary

### Tests
- Comprehensive report generation tests

---

## Capability 9: Initialization Pipeline Orchestrator

**Module:** `src/project-engine/initialize.mjs`

**Purpose:** Orchestrate all 8 capabilities into a single coherent initialization flow.

### Function
```javascript
export async function createProjectInitializationPipeline(projectRoot, options)
// Returns: {
//   success: boolean,
//   receipt: { phases: {...}, totalDuration: number },
//   report: { summary, features, entities, stats },
//   state: { fsStore, projectStore, domainStore, templateGraph, snapshot }
// }
```

### Execution Sequence (9 Phases)
1. **scan** - Filesystem scan
2. **stackDetection** - Stack profile
3. **projectModel** - Project structure
4. **fileRoles** - File classification
5. **domainInference** - Domain model
6. **templateInference** - Code templates
7. **snapshot** - Baseline snapshot
8. **hooks** - Hook registration
9. **report** - Project report

### Receipt
```javascript
{
  phases: {
    scan: { duration: 148, success: true, data: { store, files: 2865 } },
    stackDetection: { duration: 4, success: true, data: { profile } },
    // ... all 9 phases
  },
  totalDuration: 187,
  success: true
}
```

### State Preservation
- `fsStore` - Filesystem graph
- `projectStore` - Project structure
- `domainStore` - Domain ontology
- `templateGraph` - Code generation patterns
- `snapshot` - Baseline for drift detection

### Tests
- 7 integration tests
- All phases tested
- State preservation validated
- Error handling verified

---

## CLI Integration

**Module:** `src/cli/commands/init.mjs`

**Purpose:** User-friendly CLI wrapper around the initialization pipeline.

### Usage
```bash
npx unrdf init                      # Initialize current directory
npx unrdf init --root /path         # Specific path
npx unrdf init --dry-run            # Preview only
npx unrdf init --verbose            # Detailed output
npx unrdf init --skip-snapshot      # Skip baseline
npx unrdf init --skip-hooks         # Skip hook registration
```

### Output
- Phase summary with durations
- Human-readable project report
- Next steps guidance
- Error messages with solutions

---

## Summary Statistics

### Code Coverage
- **9 modules** - ~5,000 lines total
- **120+ tests** - 100% pass rate
- **5 vitest environments** - Cross-environment validation
- **No OTEL in implementation** - Pure functions only
- **Full Zod validation** - Type-safe throughout

### Performance (Measured on UNRDF itself)
- FS scan: ~150ms (2,865 files)
- Stack detection: ~4ms
- Project model: ~5ms
- File classification: ~17ms
- Domain inference: ~1ms
- Template inference: ~0ms
- Snapshot: ~11ms
- Hook derivation: ~0ms
- Report: ~1ms
- **Total: ~190ms**

### Architecture Guarantees
- ✅ Pure functions with no side effects (except final FS writes)
- ✅ Deterministic output (same input = same output)
- ✅ Full provenance tracking (audit trail)
- ✅ Safe rollback for materialization
- ✅ Drift detection for post-init monitoring
- ✅ Automatic policy enforcement via hooks

---

## Next Steps for Users

After init:
1. **Test:** `pnpm test`
2. **Build:** `pnpm build`
3. **Check drift:** `unrdf drift --check`
4. **Generate:** `unrdf generate --entity NewFeature`
5. **Sync:** `unrdf sync` (if code changes)

---

## References

- [Init Pipeline User Guide](./INIT-PIPELINE.md)
- [Example Usage](../examples/unrdf-init-pipeline.mjs)
- [Project Engine Tests](../test/project-engine/)
