<template>
  <div class="hooks-page">
    <div class="page-header">
      <h1>Knowledge Hooks Management</h1>
      <p class="subtitle">
        Create, edit, and manage knowledge hooks for autonomic RDF governance
      </p>
    </div>

    <div v-if="currentView === 'list'" class="view-container">
      <HookList
        ref="hookListRef"
        @create="handleCreate"
        @edit="handleEdit"
        @execute="handleExecute"
        @delete="handleDelete"
      />
    </div>

    <div v-else-if="currentView === 'create' || currentView === 'edit'" class="view-container">
      <HookEditor
        :hook-id="currentHookId"
        :initial-content="currentHookContent"
        :is-edit-mode="currentView === 'edit'"
        @save="handleSave"
        @cancel="handleCancel"
      />
    </div>

    <div v-else-if="currentView === 'execute'" class="view-container">
      <div class="execution-panel">
        <h2>Execute Hook: {{ currentHook?.name }}</h2>

        <div class="execution-form">
          <label for="hook-data">RDF Data (Turtle format):</label>
          <textarea
            id="hook-data"
            v-model="executionData"
            rows="10"
            placeholder="Enter RDF/Turtle data to evaluate..."
            class="data-input"
          />

          <div class="execution-actions">
            <button @click="executeHook" :disabled="isExecuting" class="btn-primary">
              {{ isExecuting ? 'Executing...' : 'Execute Hook' }}
            </button>
            <button @click="handleCancel" class="btn-outline">
              Cancel
            </button>
          </div>
        </div>

        <div v-if="executionResult" class="execution-result">
          <h3>Execution Result</h3>
          <pre>{{ JSON.stringify(executionResult, null, 2) }}</pre>
        </div>

        <div v-if="executionError" class="execution-error">
          <h3>Execution Error</h3>
          <p>{{ executionError }}</p>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import HookList from '~/components/hooks/HookList.vue'
import HookEditor from '~/components/hooks/HookEditor.vue'

/**
 * @typedef {'list' | 'create' | 'edit' | 'execute'} ViewType
 */

/**
 * @typedef {Object} Hook
 * @property {string} id - Hook identifier
 * @property {string} name - Hook name
 * @property {string} description - Hook description
 * @property {string} phase - Execution phase
 * @property {string} [source] - Optional source code
 */

/** @type {import('vue').Ref<ViewType>} */
const currentView = ref('list')
/** @type {import('vue').Ref<Hook|null>} */
const currentHook = ref(null)
/** @type {import('vue').Ref<string|undefined>} */
const currentHookId = ref(undefined)
/** @type {import('vue').Ref<string|undefined>} */
const currentHookContent = ref(undefined)
/** @type {import('vue').Ref<InstanceType<typeof HookList>|null>} */
const hookListRef = ref(null)

/** @type {import('vue').Ref<string>} */
const executionData = ref('')
/** @type {import('vue').Ref<any>} */
const executionResult = ref(null)
/** @type {import('vue').Ref<string>} */
const executionError = ref('')
/** @type {import('vue').Ref<boolean>} */
const isExecuting = ref(false)

function handleCreate() {
  currentView.value = 'create'
  currentHookId.value = undefined
  currentHookContent.value = undefined
}

/**
 * Handle edit hook action
 * @param {Hook} hook - Hook to edit
 */
async function handleEdit(hook) {
  try {
    // Fetch full hook details including source code
    const response = await fetch(`/api/hooks/${hook.id}`)
    if (!response.ok) {
      throw new Error(`Failed to load hook: ${response.statusText}`)
    }

    const data = await response.json()
    const fullHook = data.data

    currentView.value = 'edit'
    currentHook.value = hook
    currentHookId.value = hook.id
    currentHookContent.value = fullHook.source || generateHookSource(fullHook)
  } catch (error) {
    console.error('Failed to load hook for editing:', error)
    alert(`Failed to load hook: ${error.message}`)
  }
}

/**
 * Handle execute hook action
 * @param {Hook} hook - Hook to execute
 */
function handleExecute(hook) {
  currentView.value = 'execute'
  currentHook.value = hook
  executionData.value = ''
  executionResult.value = null
  executionError.value = ''
}

/**
 * Handle delete hook action
 * @param {string} hookId - Hook ID to delete
 */
function handleDelete(hookId) {
  console.log('Hook deleted:', hookId)
  // The HookList component handles the actual deletion
}

/**
 * Handle save hook action
 * @param {string} content - Hook content to save
 * @param {string} [hookId] - Optional hook ID for updates
 */
async function handleSave(content, hookId) {
  try {
    const endpoint = hookId ? `/api/hooks/${hookId}` : '/api/hooks/register'
    const method = hookId ? 'PUT' : 'POST'

    // Parse hook content to extract metadata
    // This is a simplified version - in production, use proper AST parsing
    const hookData = parseHookContent(content)

    const response = await fetch(endpoint, {
      method,
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        ...hookData,
        _source: content // Store original source
      })
    })

    if (!response.ok) {
      throw new Error(`Failed to save hook: ${response.statusText}`)
    }

    currentView.value = 'list'
    hookListRef.value?.loadHooks()
  } catch (error: any) {
    console.error('Failed to save hook:', error)
    alert(`Failed to save hook: ${error.message}`)
  }
}

function handleCancel() {
  currentView.value = 'list'
  currentHook.value = null
  currentHookId.value = undefined
  currentHookContent.value = undefined
  executionData.value = ''
  executionResult.value = null
  executionError.value = ''
}

async function executeHook() {
  if (!currentHook.value) return

  isExecuting.value = true
  executionResult.value = null
  executionError.value = ''

  try {
    const response = await fetch('/api/hooks/evaluate', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        hookId: currentHook.value.id,
        data: executionData.value,
        context: {
          metadata: {
            executedAt: new Date().toISOString()
          }
        }
      })
    })

    if (!response.ok) {
      throw new Error(`Execution failed: ${response.statusText}`)
    }

    const data = await response.json()
    executionResult.value = data.data
  } catch (error: any) {
    executionError.value = error.message
    console.error('Hook execution failed:', error)
  } finally {
    isExecuting.value = false
  }
}

function parseHookContent(content: string): any {
  // Simplified parser - extract basic hook metadata
  // In production, use a proper JavaScript parser like @babel/parser

  const nameMatch = content.match(/name:\s*['"]([^'"]+)['"]/)
  const descMatch = content.match(/description:\s*['"]([^'"]+)['"]/)

  return {
    id: nameMatch?.[1]?.replace(/[^a-zA-Z0-9-]/g, '-') || 'hook-' + Date.now(),
    select: 'SELECT * WHERE { ?s ?p ?o }', // Default SPARQL query
    predicates: [{ kind: 'ASK', query: 'ASK { ?s ?p ?o }' }],
    combine: 'AND',
    phase: 'commit'
  }
}

function generateHookSource(hook: any): string {
  // Generate source code from hook data structure
  return `import { defineHook } from '@unrdf/knowledge-engine'

export const hook = defineHook({
  meta: ${JSON.stringify(hook.meta, null, 2)},
  channel: ${JSON.stringify(hook.channel, null, 2)},
  when: ${JSON.stringify(hook.when, null, 2)},
  determinism: ${JSON.stringify(hook.determinism, null, 2)},
  receipt: ${JSON.stringify(hook.receipt, null, 2)},

  async before({ payload, context }) {
    return payload
  },

  async run({ payload, context }) {
    return { result: null }
  },

  async after({ result, cancelled, reason }) {
    return { result }
  }
})
`
}
</script>

<style scoped>
.hooks-page {
  min-height: 100vh;
  background: #0d0d0d;
  color: #fff;
  padding: 2rem;
}

.page-header {
  margin-bottom: 2rem;
}

.page-header h1 {
  margin: 0;
  font-size: 2rem;
  font-weight: 600;
}

.subtitle {
  margin: 0.5rem 0 0;
  color: #aaa;
  font-size: 1rem;
}

.view-container {
  background: #0d0d0d;
  border-radius: 8px;
  min-height: 600px;
}

.execution-panel {
  background: #1e1e1e;
  border: 1px solid #333;
  border-radius: 8px;
  padding: 2rem;
}

.execution-panel h2 {
  margin: 0 0 1.5rem;
  color: #fff;
}

.execution-form {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  margin-bottom: 2rem;
}

.execution-form label {
  font-weight: 600;
  color: #fff;
}

.data-input {
  width: 100%;
  padding: 1rem;
  background: #0d0d0d;
  border: 1px solid #333;
  border-radius: 4px;
  color: #fff;
  font-family: 'Monaco', 'Menlo', monospace;
  font-size: 0.875rem;
  resize: vertical;
}

.execution-actions {
  display: flex;
  gap: 0.5rem;
}

.execution-result, .execution-error {
  background: #0d0d0d;
  border: 1px solid #333;
  border-radius: 4px;
  padding: 1.5rem;
  margin-top: 1.5rem;
}

.execution-result h3, .execution-error h3 {
  margin: 0 0 1rem;
  color: #fff;
}

.execution-result pre {
  margin: 0;
  color: #4caf50;
  font-family: 'Monaco', 'Menlo', monospace;
  font-size: 0.875rem;
  overflow-x: auto;
}

.execution-error {
  border-color: #f44336;
}

.execution-error p {
  margin: 0;
  color: #f44336;
}

.btn-primary, .btn-outline {
  padding: 0.75rem 1.5rem;
  border-radius: 4px;
  font-size: 0.875rem;
  cursor: pointer;
  border: 1px solid;
  transition: all 0.2s;
}

.btn-primary {
  background: #007acc;
  color: white;
  border-color: #007acc;
}

.btn-primary:hover:not(:disabled) {
  background: #005a9e;
}

.btn-outline {
  background: transparent;
  color: #fff;
  border-color: #555;
}

.btn-outline:hover:not(:disabled) {
  background: #333;
}

button:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}
</style>
