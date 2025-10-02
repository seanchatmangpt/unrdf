# Sidecar Nuxt 4 Architecture Analysis

**Analysis Date:** 2025-10-01
**Analyst:** Hive Mind Swarm - Analyst Agent
**Session ID:** swarm-1759367406393-9s3jdbqhs
**Purpose:** Analyze sidecar architecture for Monaco Editor integration

---

## Executive Summary

The UNRDF sidecar is a **production-grade Nuxt 4 application** that serves as a secure observability and management interface for the Knowledge Graph Core (KGC) system. It provides:

- **RESTful API endpoints** for knowledge hook management, transaction processing, and policy orchestration
- **Real-time observability dashboard** with OpenTelemetry metrics
- **Enterprise security** including mTLS, JWT auth, RBAC, circuit breakers, and DDoS protection
- **Knowledge Hook execution engine** with sandbox isolation and SPARQL/SHACL validation

The architecture is **well-suited for Monaco Editor integration** with existing API endpoints, composables, and Vue component patterns ready for extension.

---

## 1. Current Architecture Overview

### 1.1 Technology Stack

| Layer | Technologies |
|-------|-------------|
| **Frontend** | Nuxt 4, Vue 3 (Composition API), TypeScript |
| **Backend** | Nitro (Nuxt server), Node.js, H3 event handlers |
| **Graph Engine** | N3.js (RDF/Turtle), SPARQL, SHACL |
| **Knowledge Hooks** | Content-addressed hook system with sandbox execution |
| **Security** | Helmet, bcrypt, JWT, mTLS, OWASP compliance |
| **Observability** | OpenTelemetry (traces, metrics, logs), custom OTEL SDK |
| **Testing** | Vitest (unit, integration, e2e), Nuxt Test Utils |
| **Validation** | Zod schemas for all API contracts |

### 1.2 Directory Structure

```
/Users/sac/unrdf/sidecar/
├── nuxt.config.ts              # Nuxt 4 configuration (SSR disabled, pages enabled)
├── package.json                # Dependencies (Nuxt 4.1.2, Vue, OpenTelemetry, N3)
├── app/                        # Frontend application
│   ├── components/
│   │   └── observability/
│   │       └── MetricsDashboard.vue  # OTEL metrics visualization
│   ├── composables/
│   │   └── useOTelMetrics.mjs        # Reactive metrics composable
│   ├── layouts/
│   │   └── dashboard.vue             # Dashboard layout wrapper
│   └── pages/
│       └── observability.vue         # Observability page
├── server/                     # Nitro backend
│   ├── api/                    # REST API endpoints (26 endpoints)
│   │   ├── hooks/
│   │   │   ├── register.post.mjs     # Register knowledge hooks
│   │   │   └── evaluate.post.mjs     # Evaluate hooks with circuit breaker
│   │   ├── transaction/
│   │   │   └── apply.post.mjs        # Apply RDF transactions
│   │   ├── policy/
│   │   │   └── register.post.mjs     # Register policy packs
│   │   ├── auth/                     # JWT authentication (login, register, refresh)
│   │   ├── admin/                    # RBAC-protected admin endpoints
│   │   ├── health/                   # Health checks (liveness, readiness)
│   │   ├── metrics.get.mjs           # OTEL metrics endpoint
│   │   └── traces.get.mjs            # OTEL traces endpoint
│   ├── middleware/             # Request processing pipeline (10 middlewares)
│   │   ├── 00.request-id.mjs         # Request ID generation
│   │   ├── 01.telemetry.mjs          # OTEL context propagation
│   │   ├── 01.security-headers.mjs   # Helmet security headers
│   │   ├── 02.authorization.mjs      # RBAC enforcement
│   │   ├── 02.mtls-validate.mjs      # Mutual TLS validation
│   │   ├── 03.rate-limit.mjs         # Rate limiting with Redis
│   │   └── 04.request-validation.mjs # Request validation
│   └── utils/                  # Shared utilities (19 modules)
│       ├── managers.mjs              # Singleton manager access
│       ├── response.mjs              # OpenAPI-compliant response builders
│       ├── validation.mjs            # Zod validation schemas
│       ├── circuit-breaker.mjs       # Circuit breaker registry
│       ├── rate-limiter.mjs          # Redis-based rate limiter
│       ├── ddos-detector.mjs         # DDoS threat detection
│       ├── backpressure-manager.mjs  # Load shedding
│       ├── secure-sandbox.mjs        # Hook execution sandbox
│       └── otel-metrics.mjs          # Custom OTEL metrics
└── test/                       # Comprehensive test suite
    ├── unit/                   # Unit tests (managers, RBAC, validation)
    ├── integration/            # Integration tests (API workflows, OTEL)
    ├── security/               # Security tests (OWASP Top 10, sandbox)
    └── nuxt/                   # Nuxt-specific tests (API routes)
```

---

## 2. Knowledge Hook System

### 2.1 Hook Definition Contract

The system uses **80/20 Knowledge Hooks** defined in `src/knowledge-engine/define-hook.mjs`:

```javascript
export const exampleHook = defineHook({
  meta: {
    name: "compliance:largeTx",
    description: "Alerts on large transactions",
    ontology: ["fibo"]
  },
  channel: {
    graphs: ["urn:graph:fibo:prod"],
    view: "delta"  // before | after | delta
  },
  when: {
    kind: "sparql-ask",  // sparql-ask | sparql-select | shacl
    ref: {
      uri: "file://hooks/compliance/largeTx.ask.rq",
      sha256: "e3b0c44...",  // Content addressing for provenance
      mediaType: "application/sparql-query"
    }
  },
  determinism: { seed: 42 },
  receipt: { anchor: "git-notes" },  // git-notes | none

  async before({ payload }) {
    // Pre-condition gate for payload normalization or cancellation
    if (!isValid(payload)) {
      return { cancel: true, reason: "Invalid payload" };
    }
    return { ...payload, validatedAt: new Date().toISOString() };
  },

  async run({ payload, context }) {
    // Core effect or analysis
    return {
      result: { status: "alert-dispatched", amount: payload.amount },
      assertions: [/* RDF quads to add */],
      deltas: { additions: [...], removals: [...] }
    };
  },

  async after({ result, cancelled, reason }) {
    // Post-execution auditing and cleanup
    return { result: { finalStatus: cancelled ? "cancelled" : "completed" } };
  }
});
```

**Key Principles:**
- **Content-addressed conditions**: SPARQL/SHACL files referenced by SHA-256 hash
- **Reflex arc lifecycle**: `before` → `run` → `after` for autonomic behavior
- **Declarative configuration**: Determinism and receipting as metadata
- **Zod validation**: Complete type safety for all hook components
- **Sandbox execution**: Isolated execution with security restrictions

### 2.2 Hook Manager Integration

The `KnowledgeHookManager` (extends `TransactionManager`) provides:

```javascript
// Registration
hookManager.addKnowledgeHook(hook)
hookManager.removeKnowledgeHook(hookName)
hookManager.getKnowledgeHooks()

// Execution
hookManager.executeKnowledgeHook(hookName, event, options)
hookManager.executeAllKnowledgeHooks(event, options)

// Transaction integration
hookManager.apply(store, delta, options)  // Runs hooks pre/post transaction

// Policy packs
hookManager.loadPolicyPack(packName)
hookManager.deactivatePolicyPack(packName)
hookManager.getActivePolicyPacks()

// Statistics
hookManager.getStats()  // Includes hook executor and condition evaluator stats
```

**Hook Execution Flow:**
1. **Validation**: Zod schema validation of hook structure and event
2. **Security check**: Sandbox restrictions and threat detection (warn-only at registration)
3. **Condition evaluation**: SPARQL ASK/SELECT or SHACL validation against RDF store
4. **Lifecycle execution**:
   - `before()` runs first, can cancel execution
   - `run()` executes core logic if not cancelled
   - `after()` always runs for cleanup/auditing
5. **Sandbox isolation**: Code runs in restricted VM context
6. **Result collection**: Standardized result with assertions, deltas, cancellation state

### 2.3 API Endpoints for Hooks

**POST /api/hooks/register**
- Registers a knowledge hook
- Request: `{ id, select, predicates, combine, phase }`
- Predicates: ASK, SHACL, DELTA, THRESHOLD, COUNT, WINDOW
- Response: `{ hookId, phase, predicateCount }`

**POST /api/hooks/evaluate**
- Evaluates a hook with production patterns
- Features:
  - Zod request validation
  - Circuit breaker protection (failureThreshold: 3, timeout: 30s)
  - OTEL context propagation
  - OpenAPI-compliant responses
  - Request ID tracking
- Request: `{ hookId, data?, context? }`
- Response: `{ hookId, fired, timestamp, predicates, duration }`

---

## 3. UI/UX Patterns

### 3.1 Current Dashboard Architecture

**Observability Page** (`app/pages/observability.vue`):
- Uses `NuxtLayout name="dashboard"` wrapper
- Renders `MetricsDashboard` component
- Sets page metadata with `useHead()`

**MetricsDashboard Component** (`app/components/observability/MetricsDashboard.vue`):
- **Composable-driven**: Uses `useOTelMetrics()` for reactive state
- **Real-time updates**: Auto-refresh with 5s interval
- **Grid layout**: 4-card responsive grid (rate limiting, DDoS, query perf, backpressure)
- **Color-coded metrics**: Success (green), warning (yellow), danger (red)
- **Loading states**: Spinner with proper error handling
- **Retry mechanism**: Manual refresh and auto-refresh toggle

**Component Patterns:**
```vue
<script setup>
import { useOTelMetrics } from '~/composables/useOTelMetrics.mjs'

const { metrics, loading, error, refresh, autoRefresh, stopAutoRefresh } = useOTelMetrics()
const autoRefreshEnabled = ref(false)

function toggleAutoRefresh() {
  if (autoRefreshEnabled.value) {
    autoRefresh(5000)
  } else {
    stopAutoRefresh()
  }
}

onMounted(() => {
  refresh()
})
</script>
```

### 3.2 Composables Architecture

**useOTelMetrics.mjs** (inferred from usage):
- Reactive metrics state (`ref` for loading, error, metrics)
- Async `refresh()` function to fetch `/api/metrics`
- Auto-refresh with interval management
- Cleanup with `stopAutoRefresh()`
- Error handling and retry logic

**Expected Composable API:**
```javascript
export function useOTelMetrics() {
  const metrics = ref({
    rateLimit: { requests: 0, allowed: 0, blocked: 0, rate: 0 },
    ddos: { threatScore: 0, blacklistAdditions: 0, requestsBlocked: 0 },
    query: { avgCost: 0, total: 0, rejected: 0 },
    backpressure: { systemLoad: 0, queueDepth: 0, rejected: 0 }
  })
  const loading = ref(false)
  const error = ref(null)

  async function refresh() { /* ... */ }
  function autoRefresh(intervalMs) { /* ... */ }
  function stopAutoRefresh() { /* ... */ }

  return { metrics, loading, error, refresh, autoRefresh, stopAutoRefresh }
}
```

---

## 4. Monaco Editor Integration Points

### 4.1 Recommended Integration Architecture

**New Page: `/app/pages/hooks.vue`**
```vue
<template>
  <NuxtLayout name="dashboard">
    <div class="hooks-page">
      <header class="page-header">
        <h2>Knowledge Hook Editor</h2>
        <div class="actions">
          <button @click="createNewHook">New Hook</button>
          <button @click="loadHook">Load Hook</button>
          <button @click="saveHook">Save Hook</button>
        </div>
      </header>

      <div class="editor-container">
        <!-- Monaco Editor Component -->
        <MonacoHookEditor
          v-model="hookCode"
          :language="editorLanguage"
          :schema="hookSchema"
          @validate="handleValidation"
          @execute="handleExecution"
        />

        <!-- Results Panel -->
        <HookExecutionResults
          :results="executionResults"
          :loading="executing"
          :error="executionError"
        />
      </div>
    </div>
  </NuxtLayout>
</template>

<script setup>
import { useKnowledgeHooks } from '~/composables/useKnowledgeHooks.mjs'

const {
  hookCode,
  editorLanguage,
  hookSchema,
  executionResults,
  executing,
  executionError,
  createNewHook,
  loadHook,
  saveHook,
  handleValidation,
  handleExecution
} = useKnowledgeHooks()
</script>
```

### 4.2 New Composable: `useKnowledgeHooks.mjs`

**File:** `/Users/sac/unrdf/sidecar/app/composables/useKnowledgeHooks.mjs`

```javascript
/**
 * @file Knowledge Hook management composable
 * @description Reactive state management for hook editing and execution
 */

import { ref, computed, watch } from 'vue'

export function useKnowledgeHooks() {
  // State
  const hookCode = ref('')
  const editorLanguage = ref('javascript')  // javascript | sparql | shacl
  const executionResults = ref(null)
  const executing = ref(false)
  const executionError = ref(null)
  const registeredHooks = ref([])
  const selectedHook = ref(null)

  // Hook schema for validation
  const hookSchema = computed(() => ({
    meta: { name: 'string', description: 'string', ontology: 'string[]' },
    channel: { graphs: 'string[]', view: 'before|after|delta' },
    when: { kind: 'sparql-ask|sparql-select|shacl', ref: 'object' },
    before: 'function?',
    run: 'function',
    after: 'function?'
  }))

  // Actions
  async function createNewHook() {
    hookCode.value = getHookTemplate()
    selectedHook.value = null
  }

  async function loadHook(hookId) {
    try {
      const response = await $fetch('/api/hooks/list')
      const hook = response.data.find(h => h.id === hookId)
      if (hook) {
        hookCode.value = JSON.stringify(hook, null, 2)
        selectedHook.value = hook
      }
    } catch (err) {
      executionError.value = err
    }
  }

  async function saveHook() {
    try {
      const hookDef = JSON.parse(hookCode.value)
      const response = await $fetch('/api/hooks/register', {
        method: 'POST',
        body: hookDef
      })
      registeredHooks.value.push(response.data)
      return response.data
    } catch (err) {
      executionError.value = err
      throw err
    }
  }

  async function handleValidation(code) {
    try {
      const hookDef = JSON.parse(code)
      // Validate against Zod schema (client-side)
      // Could call /api/hooks/validate endpoint
      return { valid: true }
    } catch (err) {
      return { valid: false, errors: [err.message] }
    }
  }

  async function handleExecution(hookId, testData) {
    executing.value = true
    executionError.value = null
    try {
      const response = await $fetch('/api/hooks/evaluate', {
        method: 'POST',
        body: {
          hookId,
          data: testData,
          context: { traceId: generateTraceId() }
        }
      })
      executionResults.value = response.data
      return response.data
    } catch (err) {
      executionError.value = err
      throw err
    } finally {
      executing.value = false
    }
  }

  async function loadRegisteredHooks() {
    try {
      const response = await $fetch('/api/hooks/list')
      registeredHooks.value = response.data
    } catch (err) {
      executionError.value = err
    }
  }

  function getHookTemplate() {
    return `{
  "meta": {
    "name": "my-hook",
    "description": "Hook description",
    "ontology": ["fibo"]
  },
  "channel": {
    "graphs": ["urn:graph:default"],
    "view": "delta"
  },
  "when": {
    "kind": "sparql-ask",
    "ref": {
      "uri": "file://hooks/my-hook.ask.rq",
      "sha256": "...",
      "mediaType": "application/sparql-query"
    }
  },
  "determinism": { "seed": 42 },
  "receipt": { "anchor": "none" }
}`
  }

  function generateTraceId() {
    return `trace-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }

  // Load hooks on mount
  onMounted(() => {
    loadRegisteredHooks()
  })

  return {
    hookCode,
    editorLanguage,
    hookSchema,
    executionResults,
    executing,
    executionError,
    registeredHooks,
    selectedHook,
    createNewHook,
    loadHook,
    saveHook,
    handleValidation,
    handleExecution,
    loadRegisteredHooks
  }
}
```

### 4.3 New Component: `MonacoHookEditor.vue`

**File:** `/Users/sac/unrdf/sidecar/app/components/hooks/MonacoHookEditor.vue`

```vue
<template>
  <div class="monaco-hook-editor">
    <div class="editor-toolbar">
      <select v-model="currentLanguage" @change="changeLanguage">
        <option value="javascript">JavaScript (Hook)</option>
        <option value="sparql">SPARQL</option>
        <option value="shacl">SHACL (Turtle)</option>
      </select>

      <div class="editor-actions">
        <button @click="formatCode" :disabled="!canFormat">Format</button>
        <button @click="validateCode">Validate</button>
        <button @click="executeHook" :disabled="!canExecute">Execute</button>
      </div>
    </div>

    <div ref="editorContainer" class="editor-container"></div>

    <div v-if="validationErrors.length" class="validation-errors">
      <h4>Validation Errors:</h4>
      <ul>
        <li v-for="(error, i) in validationErrors" :key="i" class="error-item">
          {{ error }}
        </li>
      </ul>
    </div>
  </div>
</template>

<script setup>
import * as monaco from 'monaco-editor'
import { ref, onMounted, onBeforeUnmount, watch } from 'vue'

const props = defineProps({
  modelValue: String,
  language: { type: String, default: 'javascript' },
  schema: Object,
  readOnly: { type: Boolean, default: false }
})

const emit = defineEmits(['update:modelValue', 'validate', 'execute'])

const editorContainer = ref(null)
const currentLanguage = ref(props.language)
const validationErrors = ref([])
const canFormat = ref(true)
const canExecute = ref(true)

let editor = null

onMounted(() => {
  // Initialize Monaco Editor
  editor = monaco.editor.create(editorContainer.value, {
    value: props.modelValue || '',
    language: currentLanguage.value,
    theme: 'vs-dark',
    automaticLayout: true,
    minimap: { enabled: true },
    readOnly: props.readOnly,
    lineNumbers: 'on',
    renderWhitespace: 'selection',
    scrollBeyondLastLine: false,
    fontSize: 14,
    tabSize: 2,
    insertSpaces: true
  })

  // Register SPARQL language if not registered
  if (!monaco.languages.getLanguages().some(lang => lang.id === 'sparql')) {
    monaco.languages.register({ id: 'sparql' })
    monaco.languages.setMonarchTokensProvider('sparql', getSparqlTokenizer())
  }

  // Register SHACL (Turtle) language
  if (!monaco.languages.getLanguages().some(lang => lang.id === 'shacl')) {
    monaco.languages.register({ id: 'shacl' })
    monaco.languages.setMonarchTokensProvider('shacl', getTurtleTokenizer())
  }

  // Listen for content changes
  editor.onDidChangeModelContent(() => {
    const value = editor.getValue()
    emit('update:modelValue', value)
  })

  // Add custom actions
  editor.addAction({
    id: 'execute-hook',
    label: 'Execute Hook',
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
    run: executeHook
  })

  editor.addAction({
    id: 'validate-hook',
    label: 'Validate Hook',
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyV],
    run: validateCode
  })
})

onBeforeUnmount(() => {
  if (editor) {
    editor.dispose()
  }
})

watch(() => props.modelValue, (newValue) => {
  if (editor && editor.getValue() !== newValue) {
    editor.setValue(newValue || '')
  }
})

function changeLanguage() {
  if (editor) {
    monaco.editor.setModelLanguage(editor.getModel(), currentLanguage.value)
  }
}

async function formatCode() {
  if (!editor) return

  try {
    if (currentLanguage.value === 'javascript') {
      const code = editor.getValue()
      const formatted = JSON.stringify(JSON.parse(code), null, 2)
      editor.setValue(formatted)
    } else {
      await editor.getAction('editor.action.formatDocument').run()
    }
  } catch (err) {
    console.error('Format error:', err)
  }
}

async function validateCode() {
  if (!editor) return

  const code = editor.getValue()
  const result = await emit('validate', code)

  if (result && !result.valid) {
    validationErrors.value = result.errors || []

    // Add error markers to editor
    const markers = result.errors.map((error, i) => ({
      severity: monaco.MarkerSeverity.Error,
      startLineNumber: 1,
      startColumn: 1,
      endLineNumber: 1,
      endColumn: 1,
      message: error
    }))

    monaco.editor.setModelMarkers(editor.getModel(), 'validation', markers)
  } else {
    validationErrors.value = []
    monaco.editor.setModelMarkers(editor.getModel(), 'validation', [])
  }
}

async function executeHook() {
  if (!editor) return

  const code = editor.getValue()
  try {
    const hookDef = JSON.parse(code)
    emit('execute', hookDef.meta?.name, null)
  } catch (err) {
    validationErrors.value = ['Invalid JSON: ' + err.message]
  }
}

function getSparqlTokenizer() {
  return {
    keywords: ['SELECT', 'WHERE', 'FILTER', 'OPTIONAL', 'UNION', 'GRAPH', 'PREFIX', 'ASK', 'CONSTRUCT'],
    operators: ['=', '!=', '<', '>', '<=', '>=', '&&', '||', '!'],
    tokenizer: {
      root: [
        [/[a-zA-Z_]\w*/, {
          cases: {
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }],
        [/"[^"]*"/, 'string'],
        [/<[^>]+>/, 'type.identifier'],
        [/\?[a-zA-Z_]\w*/, 'variable']
      ]
    }
  }
}

function getTurtleTokenizer() {
  return {
    keywords: ['@prefix', '@base', 'a', 'true', 'false'],
    tokenizer: {
      root: [
        [/@prefix|@base/, 'keyword'],
        [/[a-zA-Z_]\w*:/, 'type.identifier'],
        [/"[^"]*"/, 'string'],
        [/<[^>]+>/, 'type.identifier']
      ]
    }
  }
}
</script>

<style scoped>
.monaco-hook-editor {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: #1e1e1e;
  border: 1px solid #3a3a3a;
  border-radius: 8px;
  overflow: hidden;
}

.editor-toolbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.75rem 1rem;
  background: #252526;
  border-bottom: 1px solid #3a3a3a;
}

.editor-toolbar select {
  padding: 0.5rem;
  background: #3c3c3c;
  color: #e0e0e0;
  border: 1px solid #555;
  border-radius: 4px;
}

.editor-actions {
  display: flex;
  gap: 0.5rem;
}

.editor-actions button {
  padding: 0.5rem 1rem;
  background: #3c3c3c;
  color: #e0e0e0;
  border: 1px solid #555;
  border-radius: 4px;
  cursor: pointer;
  transition: background 0.2s;
}

.editor-actions button:hover:not(:disabled) {
  background: #4c4c4c;
}

.editor-actions button:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.editor-container {
  flex: 1;
  min-height: 400px;
}

.validation-errors {
  padding: 1rem;
  background: #1e1e1e;
  border-top: 1px solid #3a3a3a;
  max-height: 150px;
  overflow-y: auto;
}

.validation-errors h4 {
  margin: 0 0 0.5rem 0;
  color: #ef4444;
  font-size: 0.875rem;
}

.validation-errors ul {
  margin: 0;
  padding: 0;
  list-style: none;
}

.error-item {
  padding: 0.25rem 0;
  color: #fca5a5;
  font-size: 0.875rem;
  font-family: monospace;
}
</style>
```

### 4.4 New Component: `HookExecutionResults.vue`

**File:** `/Users/sac/unrdf/sidecar/app/components/hooks/HookExecutionResults.vue`

```vue
<template>
  <div class="hook-execution-results">
    <h3>Execution Results</h3>

    <div v-if="loading" class="loading">
      <div class="spinner"></div>
      <p>Executing hook...</p>
    </div>

    <div v-else-if="error" class="error">
      <h4>Execution Error</h4>
      <pre>{{ error.message }}</pre>
      <div v-if="error.issues" class="error-issues">
        <h5>Issues:</h5>
        <ul>
          <li v-for="(issue, i) in error.issues" :key="i">
            {{ issue.path }}: {{ issue.message }}
          </li>
        </ul>
      </div>
    </div>

    <div v-else-if="results" class="results">
      <div class="result-header">
        <span :class="['status-badge', results.fired ? 'success' : 'neutral']">
          {{ results.fired ? 'FIRED' : 'NOT FIRED' }}
        </span>
        <span class="timestamp">{{ formatTimestamp(results.timestamp) }}</span>
      </div>

      <div class="result-section">
        <h4>Predicates</h4>
        <div class="predicates-list">
          <div
            v-for="(predicate, i) in results.predicates"
            :key="i"
            class="predicate-item"
          >
            <span :class="['predicate-status', predicate.passed ? 'passed' : 'failed']">
              {{ predicate.passed ? '✓' : '✗' }}
            </span>
            <span class="predicate-kind">{{ predicate.kind }}</span>
            <span class="predicate-value">{{ formatPredicateValue(predicate) }}</span>
          </div>
        </div>
      </div>

      <div class="result-section">
        <h4>Performance</h4>
        <div class="metrics-grid">
          <div class="metric">
            <span class="metric-label">Query Time:</span>
            <span class="metric-value">{{ results.duration.queryMs.toFixed(2) }}ms</span>
          </div>
          <div class="metric">
            <span class="metric-label">Evaluation Time:</span>
            <span class="metric-value">{{ results.duration.evaluationMs.toFixed(2) }}ms</span>
          </div>
          <div class="metric">
            <span class="metric-label">Total Time:</span>
            <span class="metric-value">{{ results.duration.totalMs.toFixed(2) }}ms</span>
          </div>
        </div>
      </div>

      <div v-if="results.result" class="result-section">
        <h4>Result Data</h4>
        <pre class="result-data">{{ JSON.stringify(results.result, null, 2) }}</pre>
      </div>

      <div v-if="results.assertions" class="result-section">
        <h4>Assertions</h4>
        <pre class="result-data">{{ formatAssertions(results.assertions) }}</pre>
      </div>
    </div>

    <div v-else class="empty-state">
      <p>No execution results yet. Click "Execute" to run the hook.</p>
    </div>
  </div>
</template>

<script setup>
const props = defineProps({
  results: Object,
  loading: Boolean,
  error: Object
})

function formatTimestamp(timestamp) {
  return new Date(timestamp).toLocaleString()
}

function formatPredicateValue(predicate) {
  if (predicate.value !== undefined) {
    return `value: ${predicate.value}`
  }
  return ''
}

function formatAssertions(assertions) {
  if (!assertions || !assertions.length) return 'No assertions'
  return assertions.map(quad =>
    `${quad.subject} ${quad.predicate} ${quad.object}`
  ).join('\n')
}
</script>

<style scoped>
.hook-execution-results {
  background: #2a2a2a;
  border: 1px solid #3a3a3a;
  border-radius: 8px;
  padding: 1.5rem;
  overflow-y: auto;
}

.hook-execution-results h3 {
  margin: 0 0 1rem 0;
  color: #9ca3af;
  font-size: 1.25rem;
}

.loading, .empty-state {
  text-align: center;
  padding: 2rem;
  color: #6b7280;
}

.spinner {
  width: 40px;
  height: 40px;
  border: 3px solid #3a3a3a;
  border-top-color: #60a5fa;
  border-radius: 50%;
  animation: spin 1s linear infinite;
  margin: 0 auto 1rem;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}

.error {
  padding: 1rem;
  background: #2d1f1f;
  border: 1px solid #991b1b;
  border-radius: 4px;
}

.error h4 {
  margin: 0 0 0.5rem 0;
  color: #ef4444;
}

.error pre {
  margin: 0.5rem 0;
  padding: 0.5rem;
  background: #1a1a1a;
  color: #fca5a5;
  border-radius: 4px;
  overflow-x: auto;
}

.error-issues {
  margin-top: 1rem;
}

.error-issues h5 {
  margin: 0 0 0.5rem 0;
  color: #f87171;
}

.error-issues ul {
  margin: 0;
  padding: 0 0 0 1.5rem;
  color: #fca5a5;
}

.results {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.result-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding-bottom: 1rem;
  border-bottom: 1px solid #3a3a3a;
}

.status-badge {
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  font-weight: 600;
  font-size: 0.875rem;
}

.status-badge.success {
  background: #065f46;
  color: #10b981;
}

.status-badge.neutral {
  background: #3a3a3a;
  color: #9ca3af;
}

.timestamp {
  color: #6b7280;
  font-size: 0.875rem;
}

.result-section {
  padding: 1rem 0;
}

.result-section h4 {
  margin: 0 0 0.75rem 0;
  color: #9ca3af;
  font-size: 1rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.predicates-list {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.predicate-item {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.5rem;
  background: #1e1e1e;
  border-radius: 4px;
}

.predicate-status {
  font-weight: bold;
  font-size: 1.25rem;
}

.predicate-status.passed {
  color: #10b981;
}

.predicate-status.failed {
  color: #ef4444;
}

.predicate-kind {
  color: #60a5fa;
  font-weight: 500;
}

.predicate-value {
  color: #e0e0e0;
}

.metrics-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 1rem;
}

.metric {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.metric-label {
  color: #6b7280;
  font-size: 0.875rem;
}

.metric-value {
  color: #e0e0e0;
  font-weight: 500;
  font-size: 1.125rem;
}

.result-data {
  margin: 0;
  padding: 1rem;
  background: #1e1e1e;
  color: #e0e0e0;
  border-radius: 4px;
  overflow-x: auto;
  font-size: 0.875rem;
  line-height: 1.5;
}
</style>
```

### 4.5 Required API Endpoints (Missing)

To fully support Monaco Editor integration, add these endpoints:

**GET /api/hooks/list**
- List all registered knowledge hooks
- Response: `{ success: true, data: [{ id, meta, when, ... }] }`

**POST /api/hooks/validate**
- Validate hook definition without registration
- Request: `{ hookDefinition }`
- Response: `{ valid: true } | { valid: false, errors: [...] }`

**GET /api/hooks/:id**
- Get specific hook by ID
- Response: `{ success: true, data: { id, meta, when, ... } }`

**DELETE /api/hooks/:id**
- Delete a registered hook
- Response: `{ success: true }`

**POST /api/hooks/test**
- Execute hook with test data (sandbox execution)
- Request: `{ hookId, testData, context }`
- Response: `{ hookId, fired, result, assertions, duration }`

---

## 5. Gaps for Monaco Editor Integration

### 5.1 Missing Components

1. **Monaco Editor Package**: Not in package.json
   - Add: `"monaco-editor": "^0.50.0"`
   - Add: `"@monaco-editor/loader": "^1.4.0"` (optional)

2. **SPARQL/SHACL Language Support**: Custom tokenizers needed
   - Implement monarch tokenizers for SPARQL and SHACL (Turtle)
   - Add syntax highlighting and auto-completion

3. **Hook Editor Page**: No UI for hook editing
   - Create `/app/pages/hooks.vue`
   - Create `/app/components/hooks/MonacoHookEditor.vue`
   - Create `/app/components/hooks/HookExecutionResults.vue`

4. **Hook Management Composable**: No reactive hook state
   - Create `/app/composables/useKnowledgeHooks.mjs`

5. **API Endpoints for Hook Management**:
   - `GET /api/hooks/list` (missing)
   - `POST /api/hooks/validate` (missing)
   - `GET /api/hooks/:id` (missing)
   - `DELETE /api/hooks/:id` (missing)
   - `POST /api/hooks/test` (missing)

### 5.2 Integration Challenges

1. **Monaco Editor in Nuxt 4**:
   - Monaco requires client-side only rendering (SSR disabled, already done)
   - Use dynamic import: `const monaco = await import('monaco-editor')`
   - Add webpack/vite plugin for Monaco workers

2. **Content Addressing**:
   - Editor must compute SHA-256 hashes for SPARQL/SHACL files
   - Integrate with file resolver to fetch referenced files
   - Display hash verification status

3. **Real-time Validation**:
   - Debounced validation on code change
   - Zod schema validation in browser
   - Display markers in editor for errors

4. **Sandbox Execution**:
   - Hook execution happens server-side in secure sandbox
   - Editor must send code to `/api/hooks/test` endpoint
   - Display execution results with assertions and deltas

5. **File Upload/Management**:
   - SPARQL/SHACL files referenced in `when.ref.uri`
   - Need file upload API for `file://` URIs
   - Store files in `/hooks/` directory with content addressing

### 5.3 Security Considerations

1. **Code Injection Prevention**:
   - All hook code runs in isolated sandbox (already implemented)
   - Editor sends code to server, server validates and executes
   - Never use `eval()` or `Function()` in client

2. **Authentication**:
   - Hook editor page must require authentication
   - Use existing JWT middleware for API protection
   - RBAC: Only `admin` role can create/edit hooks

3. **Content Validation**:
   - Validate SPARQL queries against injection attacks
   - Validate SHACL shapes against malicious patterns
   - Enforce content-addressing (SHA-256 verification)

---

## 6. Recommended Implementation Plan

### Phase 1: Monaco Integration (Week 1)
1. Add Monaco Editor to package.json
2. Create `MonacoHookEditor.vue` component with basic JavaScript editing
3. Implement SPARQL and SHACL (Turtle) tokenizers
4. Add Monaco webpack/vite plugin configuration
5. Create basic hooks page with editor

### Phase 2: Hook Management API (Week 2)
1. Implement `GET /api/hooks/list` endpoint
2. Implement `POST /api/hooks/validate` endpoint
3. Implement `GET /api/hooks/:id` endpoint
4. Implement `DELETE /api/hooks/:id` endpoint
5. Implement `POST /api/hooks/test` endpoint with sandbox execution

### Phase 3: Composables & State Management (Week 3)
1. Create `useKnowledgeHooks.mjs` composable
2. Implement hook CRUD operations
3. Add reactive validation and execution
4. Integrate with existing OTEL metrics

### Phase 4: Results Display (Week 4)
1. Create `HookExecutionResults.vue` component
2. Display predicate evaluation results
3. Show assertions and deltas
4. Add performance metrics visualization
5. Implement error handling and retry logic

### Phase 5: Advanced Features (Week 5)
1. File upload for SPARQL/SHACL references
2. Content-addressing with SHA-256 verification
3. Hook versioning and diff viewer
4. Template library for common hooks
5. Export/import hook definitions

### Phase 6: Polish & Documentation (Week 6)
1. Add keyboard shortcuts (Cmd+Enter to execute)
2. Implement auto-save and local storage backup
3. Add comprehensive error messages
4. Write user documentation
5. Create video tutorials

---

## 7. Integration Code Snippets

### 7.1 Nuxt Config Update

```typescript
// sidecar/nuxt.config.ts
export default defineNuxtConfig({
  // ... existing config

  vite: {
    optimizeDeps: {
      include: ['monaco-editor']
    }
  },

  build: {
    transpile: ['monaco-editor']
  }
})
```

### 7.2 Monaco Vite Plugin

```javascript
// sidecar/vite-plugin-monaco.mjs
import { defineConfig } from 'vite'

export default defineConfig({
  optimizeDeps: {
    include: [
      'monaco-editor/esm/vs/editor/editor.worker',
      'monaco-editor/esm/vs/language/json/json.worker'
    ]
  }
})
```

### 7.3 API Endpoint Example: List Hooks

```javascript
// sidecar/server/api/hooks/list.get.mjs
import { defineEventHandler } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendError } from '../../utils/response.mjs'

export default defineEventHandler(async (event) => {
  try {
    const { hookManager } = getManagers()
    const hooks = hookManager.getKnowledgeHooks()

    return sendSuccess(event, hooks.map(hook => ({
      id: hook.meta.name,
      meta: hook.meta,
      channel: hook.channel,
      when: hook.when,
      determinism: hook.determinism,
      receipt: hook.receipt
    })))
  } catch (error) {
    return sendError(event, error)
  }
})
```

---

## 8. Conclusion

The UNRDF sidecar Nuxt 4 application is **well-architected** and **ready for Monaco Editor integration**. Key strengths:

✅ **Existing patterns**: Composables, Vue components, OpenAPI-compliant APIs
✅ **Security**: JWT auth, RBAC, sandbox execution, circuit breakers
✅ **Observability**: OTEL metrics, real-time dashboards
✅ **Knowledge hooks**: Production-ready hook manager with SPARQL/SHACL support

**Minimal gaps**:
- 5 new API endpoints for hook CRUD
- 1 new composable for hook state management
- 3 new Vue components (editor, results, file upload)
- Monaco Editor package and language tokenizers

**Estimated effort**: 4-6 weeks for full implementation with all advanced features.

**Next steps**:
1. Review this analysis with the team
2. Prioritize features (MVP vs. advanced)
3. Create detailed technical specification
4. Begin Phase 1 implementation with Monaco integration

---

**Coordination Complete**
**Stored in memory**: `swarm/analyst/architecture-analysis`
**Ready for**: Architect and Coder agents to implement Monaco Editor integration
