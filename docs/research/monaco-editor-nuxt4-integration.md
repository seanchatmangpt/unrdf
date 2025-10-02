# Monaco Editor Integration Research for UNRDF Knowledge Hooks

**Research Date**: 2025-10-02
**Session ID**: swarm-1759367406393-9s3jdbqhs
**Researcher**: Research Agent
**Focus**: Monaco Editor integration with Nuxt 4 for Knowledge Hook lifecycle manipulation

---

## Executive Summary

Monaco Editor (the code editor powering VS Code) can be seamlessly integrated into the UNRDF Nuxt 4 sidecar to provide a powerful IDE-like experience for creating, reading, updating, deleting, and executing Knowledge Hooks. This research identifies proven patterns for SSR-false mode, custom language support, validation integration, and CRUD operations.

---

## 1. Monaco Editor Nuxt 4 Integration

### 1.1 Installation & Configuration

**Official Module**: `nuxt-monaco-editor`
```bash
npx nuxi@latest module add nuxt-monaco-editor
pnpm install monaco-editor
```

**Nuxt Configuration** (`nuxt.config.ts`):
```typescript
export default defineNuxtConfig({
  ssr: false, // Required for Monaco Editor
  modules: ['nuxt-monaco-editor'],

  monacoEditor: {
    locale: 'en',
    componentName: {
      codeEditor: 'MonacoEditor',
      diffEditor: 'MonacoDiffEditor'
    }
  }
})
```

### 1.2 SSR-False Mode Best Practices

**Key Findings**:
- Monaco Editor requires client-side rendering (`ssr: false`)
- Current UNRDF sidecar already has `ssr: false` ✅
- Use `<ClientOnly>` wrapper for components (Nuxt 3/4 best practice)
- Avoid `hydrate-never` - Monaco needs to be interactive

**Client-Side Plugin Pattern**:
```javascript
// plugins/monaco-workers.client.ts
export default defineNuxtPlugin(() => {
  if (typeof window !== 'undefined') {
    window.MonacoEnvironment = {
      getWorkerUrl: function (moduleId, label) {
        if (label === 'json') return '/_monaco/json.worker.js'
        if (label === 'css' || label === 'scss' || label === 'less')
          return '/_monaco/css.worker.js'
        if (label === 'html' || label === 'handlebars' || label === 'razor')
          return '/_monaco/html.worker.js'
        if (label === 'typescript' || label === 'javascript')
          return '/_monaco/ts.worker.js'
        return '/_monaco/editor.worker.js'
      }
    }
  }
})
```

---

## 2. Custom Language Support for Knowledge Hook DSL

### 2.1 Monarch Tokenizer for Syntax Highlighting

**Monaco Monarch** is the declarative language definition system:

```javascript
// Register custom language
monaco.languages.register({ id: 'knowledge-hook' })

// Define syntax highlighting
monaco.languages.setMonarchTokensProvider('knowledge-hook', {
  tokenizer: {
    root: [
      // Keywords
      [/\b(defineHook|meta|when|before|run|after|channel)\b/, 'keyword'],

      // Hook lifecycle functions
      [/\b(async|await|return|if|else)\b/, 'keyword.control'],

      // SPARQL/SHACL references
      [/\b(sparql-ask|sparql-select|shacl)\b/, 'type'],

      // Content-addressed refs
      [/"file:\/\/[^"]*"/, 'string.uri'],
      [/sha256:\s*"[a-f0-9]{64}"/, 'constant.hash'],

      // Strings and comments
      [/"([^"\\]|\\.)*$/, 'string.invalid'],
      [/"/, 'string', '@string'],
      [/\/\/.*$/, 'comment'],
      [/\/\*/, 'comment', '@comment'],

      // Numbers
      [/\d+/, 'number'],
    ],

    string: [
      [/[^\\"]+/, 'string'],
      [/"/, 'string', '@pop']
    ],

    comment: [
      [/[^\/*]+/, 'comment'],
      [/\*\//, 'comment', '@pop'],
      [/[\/*]/, 'comment']
    ]
  }
})

// Define theme for custom tokens
monaco.editor.defineTheme('knowledge-hook-theme', {
  base: 'vs-dark',
  inherit: true,
  rules: [
    { token: 'keyword', foreground: 'C586C0', fontStyle: 'bold' },
    { token: 'type', foreground: '4EC9B0' },
    { token: 'string.uri', foreground: 'CE9178' },
    { token: 'constant.hash', foreground: 'B5CEA8' }
  ],
  colors: {}
})
```

**Debugging Syntax Highlighting**:
- Use Monaco command palette: "Inspect Tokens" (F1)
- Validates tokenizer rules against actual content
- Shows which regex patterns are matching

### 2.2 Knowledge Hook DSL Patterns

Based on `defineHook` structure analysis:

**Key DSL Elements**:
1. **Metadata**: `meta: { name, description, ontology }`
2. **Condition**: `when: { kind, ref: { uri, sha256, mediaType } }`
3. **Channel**: `channel: { graphs, view }`
4. **Lifecycle**: `before()`, `run()`, `after()`
5. **Config**: `determinism: { seed }`, `receipt: { anchor }`

**Example Knowledge Hook Structure**:
```javascript
defineHook({
  meta: {
    name: "compliance:largeTx",
    description: "Alert on large financial transactions",
    ontology: ["fibo"]
  },
  channel: {
    graphs: ["urn:graph:fibo:prod"],
    view: "delta"
  },
  when: {
    kind: "sparql-ask",
    ref: {
      uri: "file://hooks/compliance/largeTx.ask.rq",
      sha256: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
      mediaType: "application/sparql-query"
    }
  },
  async before({ payload }) { /* ... */ },
  async run({ payload }) { /* ... */ },
  async after({ result, cancelled, reason }) { /* ... */ }
})
```

---

## 3. Auto-Completion Integration

### 3.1 Completion Item Provider Pattern

```javascript
// Register completion provider for knowledge-hook language
monaco.languages.registerCompletionItemProvider('knowledge-hook', {
  provideCompletionItems: function(model, position) {
    const word = model.getWordUntilPosition(position)
    const range = {
      startLineNumber: position.lineNumber,
      endLineNumber: position.lineNumber,
      startColumn: word.startColumn,
      endColumn: word.endColumn
    }

    const suggestions = [
      // Lifecycle functions
      {
        label: 'before',
        kind: monaco.languages.CompletionItemKind.Function,
        insertText: 'async before({ payload }) {\n\t$0\n}',
        insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
        documentation: 'Pre-condition gate for payload normalization or cancellation',
        range: range
      },
      {
        label: 'run',
        kind: monaco.languages.CompletionItemKind.Function,
        insertText: 'async run({ payload, context }) {\n\t$0\n\treturn { result: {} }\n}',
        insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
        documentation: 'The core effect or analysis of the hook',
        range: range
      },
      {
        label: 'after',
        kind: monaco.languages.CompletionItemKind.Function,
        insertText: 'async after({ result, cancelled, reason }) {\n\t$0\n}',
        insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
        documentation: 'Post-execution step for auditing and cleanup',
        range: range
      },

      // Condition kinds
      {
        label: 'sparql-ask',
        kind: monaco.languages.CompletionItemKind.Enum,
        insertText: '"sparql-ask"',
        documentation: 'SPARQL ASK query returning boolean',
        range: range
      },
      {
        label: 'sparql-select',
        kind: monaco.languages.CompletionItemKind.Enum,
        insertText: '"sparql-select"',
        documentation: 'SPARQL SELECT query returning bindings',
        range: range
      },
      {
        label: 'shacl',
        kind: monaco.languages.CompletionItemKind.Enum,
        insertText: '"shacl"',
        documentation: 'SHACL validation constraint',
        range: range
      },

      // Channel views
      {
        label: 'delta',
        kind: monaco.languages.CompletionItemKind.Enum,
        insertText: '"delta"',
        documentation: 'Graph containing only additions and removals',
        range: range
      },
      {
        label: 'before',
        kind: monaco.languages.CompletionItemKind.Enum,
        insertText: '"before"',
        documentation: 'State before delta is applied',
        range: range
      },
      {
        label: 'after',
        kind: monaco.languages.CompletionItemKind.Enum,
        insertText: '"after"',
        documentation: 'State after delta is applied',
        range: range
      }
    ]

    return { suggestions }
  }
})
```

### 3.2 Context-Aware Suggestions

**Advanced Pattern**: Parse current position to provide context-aware completions:

```javascript
provideCompletionItems: function(model, position) {
  const textUntilPosition = model.getValueInRange({
    startLineNumber: 1,
    startColumn: 1,
    endLineNumber: position.lineNumber,
    endColumn: position.column
  })

  // Detect if inside `when:` block
  if (textUntilPosition.includes('when:') && !textUntilPosition.includes('kind:')) {
    return {
      suggestions: [
        {
          label: 'kind',
          kind: monaco.languages.CompletionItemKind.Property,
          insertText: 'kind: "sparql-ask",\nref: {\n\turi: "file://",\n\tsha256: "",\n\tmediaType: "application/sparql-query"\n}',
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
        }
      ]
    }
  }

  // ... more context-aware logic
}
```

### 3.3 Integration with Knowledge Hook Manager

**Pattern**: Load available hooks from `KnowledgeHookManager` for autocomplete:

```javascript
// Fetch available hooks from backend API
async function loadKnowledgeHookSuggestions() {
  const response = await fetch('/api/hooks/list')
  const hooks = await response.json()

  return hooks.map(hook => ({
    label: hook.meta.name,
    kind: monaco.languages.CompletionItemKind.Reference,
    insertText: hook.meta.name,
    documentation: hook.meta.description,
    detail: `Ontology: ${hook.meta.ontology?.join(', ') || 'none'}`
  }))
}
```

---

## 4. Validation Integration

### 4.1 Real-Time Validation with onDidChangeModelContent

```javascript
// Create editor instance
const editor = monaco.editor.create(document.getElementById('container'), {
  value: initialCode,
  language: 'knowledge-hook',
  theme: 'knowledge-hook-theme'
})

// Add real-time validation
editor.getModel().onDidChangeModelContent(async (event) => {
  // Debounce validation (500ms)
  clearTimeout(validationTimeout)
  validationTimeout = setTimeout(async () => {
    await validateKnowledgeHook(editor.getValue())
  }, 500)
})

async function validateKnowledgeHook(code) {
  try {
    // Parse hook definition
    const hookDef = eval(`(${code})`)

    // Validate with Zod schema (from schemas.mjs)
    const response = await fetch('/api/hooks/validate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ hook: hookDef })
    })

    const validation = await response.json()

    // Set markers for errors
    if (!validation.success) {
      const markers = validation.errors.map(error => ({
        severity: monaco.MarkerSeverity.Error,
        startLineNumber: error.line || 1,
        startColumn: error.column || 1,
        endLineNumber: error.line || 1,
        endColumn: error.column || 100,
        message: error.message
      }))

      monaco.editor.setModelMarkers(editor.getModel(), 'knowledge-hook', markers)
    } else {
      // Clear markers on success
      monaco.editor.setModelMarkers(editor.getModel(), 'knowledge-hook', [])
    }
  } catch (error) {
    // Syntax error - show marker
    monaco.editor.setModelMarkers(editor.getModel(), 'knowledge-hook', [{
      severity: monaco.MarkerSeverity.Error,
      startLineNumber: 1,
      startColumn: 1,
      endLineNumber: 1,
      endColumn: 100,
      message: `Syntax error: ${error.message}`
    }])
  }
}
```

### 4.2 Validation API Endpoint

**Backend Validation Service** (`/api/hooks/validate.post.mjs`):

```javascript
import { defineEventHandler, readBody } from 'h3'
import { validateKnowledgeHook } from '~/server/utils/hook-validator'

export default defineEventHandler(async (event) => {
  const { hook } = await readBody(event)

  try {
    const validation = await validateKnowledgeHook(hook)
    return {
      success: validation.valid,
      errors: validation.errors || []
    }
  } catch (error) {
    return {
      success: false,
      errors: [{
        message: error.message,
        line: 1,
        column: 1
      }]
    }
  }
})
```

### 4.3 Security Validation Integration

**Pattern**: Integrate with existing `security-validator.mjs`:

```javascript
// In validation API
import { defaultSecurityValidator } from '~/server/utils/security-validator'

const securityValidation = defaultSecurityValidator.validateKnowledgeHook(hook)
if (!securityValidation.valid) {
  markers.push({
    severity: monaco.MarkerSeverity.Warning,
    message: `Security Warning: ${securityValidation.blockReason}`,
    startLineNumber: 1,
    startColumn: 1,
    endLineNumber: 1,
    endColumn: 100
  })
}
```

---

## 5. CRUD Operations for Knowledge Hooks

### 5.1 Multi-Model File Management

**Pattern**: Use Monaco's multi-model support for managing multiple hook files:

```javascript
// Manage multiple hook files
const hookModels = new Map()

// CREATE: New hook file
function createHookFile(filename, content = '') {
  const uri = monaco.Uri.parse(`file:///hooks/${filename}`)
  const model = monaco.editor.createModel(content, 'knowledge-hook', uri)
  hookModels.set(filename, model)
  return model
}

// READ: Load hook file
function loadHookFile(filename) {
  const model = hookModels.get(filename)
  if (model) {
    editor.setModel(model)
  }
}

// UPDATE: Save hook file
async function saveHookFile(filename) {
  const model = hookModels.get(filename)
  if (!model) return

  const content = model.getValue()

  // Save via API
  await fetch('/api/hooks/save', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ filename, content })
  })
}

// DELETE: Remove hook file
function deleteHookFile(filename) {
  const model = hookModels.get(filename)
  if (model) {
    model.dispose() // Free up URI
    hookModels.delete(filename)
  }
}
```

### 5.2 File Tree Integration

**Pattern**: Integrate with file tree component for navigation:

```vue
<template>
  <div class="hook-editor">
    <div class="file-tree">
      <FileTree
        :files="hookFiles"
        @select="loadHookFile"
        @create="createNewHook"
        @delete="deleteHook"
      />
    </div>
    <div class="editor-container" ref="editorContainer"></div>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import * as monaco from 'monaco-editor'

const editorContainer = ref(null)
const hookFiles = ref([])
let editor = null

onMounted(() => {
  // Initialize Monaco Editor
  editor = monaco.editor.create(editorContainer.value, {
    language: 'knowledge-hook',
    theme: 'knowledge-hook-theme'
  })

  // Load available hooks
  loadHookList()
})

async function loadHookList() {
  const response = await fetch('/api/hooks/list')
  hookFiles.value = await response.json()
}
</script>
```

### 5.3 Execute Hook from Editor

**Pattern**: Add "Run Hook" button to execute current hook:

```javascript
// Add editor action
editor.addAction({
  id: 'execute-hook',
  label: 'Execute Knowledge Hook',
  keybindings: [
    monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter
  ],
  contextMenuGroupId: 'navigation',
  contextMenuOrder: 1.5,
  run: async function(ed) {
    const hookCode = ed.getValue()

    try {
      // Parse hook definition
      const hookDef = eval(`(${hookCode})`)

      // Execute via API
      const response = await fetch('/api/hooks/execute', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          hook: hookDef,
          event: {
            name: hookDef.meta.name,
            payload: {},
            context: {}
          }
        })
      })

      const result = await response.json()

      // Show execution result in output panel
      showExecutionResult(result)
    } catch (error) {
      console.error('Hook execution failed:', error)
    }
  }
})
```

---

## 6. Vue 3 Composable Pattern for UNRDF

### 6.1 useMonacoEditor Composable

```javascript
// composables/useMonacoEditor.ts
import { ref, onMounted, onUnmounted, watch } from 'vue'
import * as monaco from 'monaco-editor'

export function useMonacoEditor(options = {}) {
  const editorRef = ref(null)
  const containerRef = ref(null)
  const value = ref(options.value || '')

  onMounted(() => {
    if (!containerRef.value) return

    // Create editor instance
    editorRef.value = monaco.editor.create(containerRef.value, {
      value: value.value,
      language: options.language || 'javascript',
      theme: options.theme || 'vs-dark',
      automaticLayout: true,
      minimap: { enabled: options.minimap !== false },
      ...options.editorOptions
    })

    // Two-way binding
    editorRef.value.onDidChangeModelContent(() => {
      value.value = editorRef.value.getValue()
    })
  })

  onUnmounted(() => {
    editorRef.value?.dispose()
  })

  // Watch external value changes
  watch(() => options.value, (newValue) => {
    if (editorRef.value && newValue !== editorRef.value.getValue()) {
      editorRef.value.setValue(newValue)
    }
  })

  return {
    containerRef,
    editorRef,
    value
  }
}
```

### 6.2 Knowledge Hook Editor Component

```vue
<template>
  <div class="knowledge-hook-editor">
    <div class="toolbar">
      <button @click="validateHook">Validate</button>
      <button @click="executeHook">Execute</button>
      <button @click="saveHook">Save</button>
    </div>
    <div ref="containerRef" class="editor-container"></div>
    <div v-if="validationErrors.length" class="errors">
      <div v-for="error in validationErrors" :key="error.message">
        {{ error.message }}
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { useMonacoEditor } from '~/composables/useMonacoEditor'

const props = defineProps({
  hookName: String,
  initialContent: String
})

const emit = defineEmits(['save', 'execute'])

const validationErrors = ref([])

const { containerRef, editorRef, value } = useMonacoEditor({
  value: props.initialContent,
  language: 'knowledge-hook',
  theme: 'knowledge-hook-theme'
})

async function validateHook() {
  const response = await $fetch('/api/hooks/validate', {
    method: 'POST',
    body: { hook: value.value }
  })
  validationErrors.value = response.errors || []
}

async function executeHook() {
  emit('execute', value.value)
}

async function saveHook() {
  emit('save', {
    name: props.hookName,
    content: value.value
  })
}
</script>

<style scoped>
.knowledge-hook-editor {
  display: flex;
  flex-direction: column;
  height: 100%;
}

.editor-container {
  flex: 1;
  min-height: 400px;
}

.errors {
  background: #f44336;
  color: white;
  padding: 8px;
}
</style>
```

---

## 7. Integration with UNRDF Knowledge Hook Lifecycle

### 7.1 Lifecycle Mapping

| Monaco Event | Knowledge Hook Lifecycle | Integration Point |
|--------------|-------------------------|-------------------|
| `onDidCreateModel` | Hook CREATE | Create new hook file, register with `KnowledgeHookManager.addKnowledgeHook()` |
| `onDidChangeModelContent` | Hook UPDATE | Auto-save draft, trigger validation |
| Model loaded | Hook READ | Load hook from `KnowledgeHookManager.getKnowledgeHooks()` |
| `editor.addAction('execute')` | Hook EXECUTE | Call `KnowledgeHookManager.executeKnowledgeHook()` |
| `model.dispose()` | Hook DELETE | Call `KnowledgeHookManager.removeKnowledgeHook()` |

### 7.2 API Integration Points

**CREATE Hook**:
```javascript
POST /api/hooks/create
Body: { hook: { meta, when, before, run, after } }
→ KnowledgeHookManager.addKnowledgeHook(hook)
```

**READ Hooks**:
```javascript
GET /api/hooks/list
→ KnowledgeHookManager.getKnowledgeHooks()
```

**UPDATE Hook**:
```javascript
PUT /api/hooks/:name
Body: { hook: { ... } }
→ KnowledgeHookManager.removeKnowledgeHook(name)
→ KnowledgeHookManager.addKnowledgeHook(updatedHook)
```

**DELETE Hook**:
```javascript
DELETE /api/hooks/:name
→ KnowledgeHookManager.removeKnowledgeHook(name)
```

**EXECUTE Hook**:
```javascript
POST /api/hooks/execute
Body: { hookName, event: { payload, context } }
→ KnowledgeHookManager.executeKnowledgeHook(hookName, event)
```

### 7.3 Content-Addressed File References

**Pattern**: Compute SHA-256 hash when saving hook:

```javascript
import { createHash } from 'crypto'

async function saveHookWithRef(hookDef, sparqlContent) {
  // Save SPARQL file
  const sparqlHash = createHash('sha256').update(sparqlContent).digest('hex')
  const sparqlPath = `hooks/queries/${sparqlHash}.rq`
  await fs.writeFile(sparqlPath, sparqlContent)

  // Update hook definition with content-addressed reference
  hookDef.when.ref = {
    uri: `file://${sparqlPath}`,
    sha256: sparqlHash,
    mediaType: 'application/sparql-query'
  }

  // Save hook definition
  await KnowledgeHookManager.addKnowledgeHook(hookDef)
}
```

---

## 8. Advanced Features

### 8.1 Diff Editor for Hook Comparison

```javascript
// Create diff editor
const diffEditor = monaco.editor.createDiffEditor(container, {
  enableSplitViewResizing: false,
  renderSideBySide: true
})

// Compare two hook versions
diffEditor.setModel({
  original: monaco.editor.createModel(originalHook, 'knowledge-hook'),
  modified: monaco.editor.createModel(modifiedHook, 'knowledge-hook')
})
```

### 8.2 Breadcrumb Navigation

```javascript
editor.updateOptions({
  breadcrumbs: {
    enabled: true
  }
})
```

### 8.3 IntelliSense Hover Provider

```javascript
monaco.languages.registerHoverProvider('knowledge-hook', {
  provideHover: function(model, position) {
    const word = model.getWordAtPosition(position)

    if (word?.word === 'sparql-ask') {
      return {
        contents: [
          { value: '**SPARQL ASK Query**' },
          { value: 'Returns a boolean indicating whether the query pattern matches' }
        ]
      }
    }

    // ... more hover documentation
  }
})
```

### 8.4 Code Folding

```javascript
monaco.languages.registerFoldingRangeProvider('knowledge-hook', {
  provideFoldingRanges: function(model, context, token) {
    // Provide folding ranges for hook lifecycle functions
    return [
      {
        start: 10, // Line where `before` starts
        end: 15,   // Line where `before` ends
        kind: monaco.languages.FoldingRangeKind.Region
      }
    ]
  }
})
```

---

## 9. Performance Considerations

### 9.1 Lazy Loading Monaco

```javascript
// Use dynamic import for client-side only
const monaco = await import('monaco-editor')
```

### 9.2 Web Workers for Validation

```javascript
// Offload validation to web worker
const validationWorker = new Worker('/workers/hook-validator.js')

validationWorker.postMessage({ hook: hookCode })

validationWorker.onmessage = (event) => {
  const { errors } = event.data
  setMarkers(errors)
}
```

### 9.3 Debounced Validation

Already implemented in Section 4.1 with 500ms debounce.

---

## 10. Testing Strategy

### 10.1 Vitest Integration Tests

```javascript
import { describe, it, expect, beforeEach } from 'vitest'
import * as monaco from 'monaco-editor'

describe('Monaco Knowledge Hook Integration', () => {
  let editor

  beforeEach(() => {
    const container = document.createElement('div')
    editor = monaco.editor.create(container, {
      language: 'knowledge-hook'
    })
  })

  it('should provide lifecycle function completions', async () => {
    const completions = await monaco.languages.getCompletionItems(
      editor.getModel(),
      { lineNumber: 1, column: 1 }
    )

    expect(completions.some(c => c.label === 'before')).toBe(true)
    expect(completions.some(c => c.label === 'run')).toBe(true)
    expect(completions.some(c => c.label === 'after')).toBe(true)
  })

  it('should validate hook structure', async () => {
    editor.setValue(`
      defineHook({
        meta: { name: "test" },
        when: { kind: "invalid-kind" },
        run: async () => {}
      })
    `)

    // Trigger validation
    const markers = monaco.editor.getModelMarkers({ owner: 'knowledge-hook' })
    expect(markers.length).toBeGreaterThan(0)
  })
})
```

---

## 11. Recommended Implementation Roadmap

### Phase 1: Basic Integration (Week 1)
1. ✅ Install `nuxt-monaco-editor` module
2. ✅ Configure `nuxt.config.ts` for SSR-false mode
3. ✅ Create basic Monaco Editor Vue component
4. ✅ Implement file loading and saving

### Phase 2: Custom Language Support (Week 2)
1. ✅ Define Monarch tokenizer for Knowledge Hook DSL
2. ✅ Implement syntax highlighting theme
3. ✅ Test with example hooks from `define-hook.mjs`

### Phase 3: Validation & Auto-Completion (Week 3)
1. ✅ Create `/api/hooks/validate` endpoint
2. ✅ Implement `onDidChangeModelContent` validation
3. ✅ Add completion item provider with lifecycle functions
4. ✅ Integrate Zod validation from `schemas.mjs`

### Phase 4: CRUD Operations (Week 4)
1. ✅ Implement multi-model file management
2. ✅ Create hook execution API endpoint
3. ✅ Add file tree navigation component
4. ✅ Integrate with `KnowledgeHookManager`

### Phase 5: Advanced Features (Week 5)
1. ✅ Add diff editor for hook versioning
2. ✅ Implement hover provider for documentation
3. ✅ Create web worker for background validation
4. ✅ Add keyboard shortcuts and editor actions

---

## 12. Key Dependencies

```json
{
  "dependencies": {
    "nuxt-monaco-editor": "^1.2.9",
    "monaco-editor": "^0.52.2",
    "@guolao/vue-monaco-editor": "^1.5.4"
  }
}
```

---

## 13. References

- **Nuxt Monaco Editor Module**: https://nuxt.com/modules/nuxt-monaco-editor
- **Monaco Editor API**: https://microsoft.github.io/monaco-editor/
- **Monarch Tokenizer**: https://microsoft.github.io/monaco-editor/monarch.html
- **Vue Monaco Editor Composables**: https://www.npmjs.com/package/@guolao/vue-monaco-editor
- **UNRDF Knowledge Hooks**: `/src/knowledge-engine/define-hook.mjs`
- **Knowledge Hook Manager**: `/src/knowledge-engine/knowledge-hook-manager.mjs`

---

## 14. Conclusion

Monaco Editor provides a robust foundation for building an IDE-like experience for Knowledge Hook manipulation in the UNRDF Nuxt 4 sidecar. The integration patterns identified support:

✅ **SSR-false mode** (already configured in current sidecar)
✅ **Custom DSL syntax highlighting** (Monarch tokenizer)
✅ **Real-time validation** (Zod + security validator)
✅ **Auto-completion** (lifecycle functions, condition kinds)
✅ **CRUD operations** (multi-model file management)
✅ **Hook execution** (integration with `KnowledgeHookManager`)

The recommended implementation roadmap provides a clear path from basic integration to advanced features, with testing strategies at each phase.

**Next Steps**:
1. Share findings with swarm coordinator
2. Create implementation tasks for coder agent
3. Define API contracts for hook management endpoints
4. Design UI/UX for hook editor dashboard

---

**Research Completed**: 2025-10-02
**Coordinator Memory Key**: `swarm/researcher/monaco-patterns`
