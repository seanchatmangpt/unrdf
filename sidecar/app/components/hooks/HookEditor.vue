<template>
  <div class="hook-editor">
    <div class="editor-header">
      <h3>{{ isEditMode ? 'Edit' : 'Create' }} Knowledge Hook</h3>
      <div class="editor-actions">
        <button @click="handleFormat" :disabled="!isReady" class="btn-secondary">
          Format Code
        </button>
        <button @click="handleValidate" :disabled="!isReady" class="btn-secondary">
          Validate
        </button>
        <button @click="handleSave" :disabled="!isReady || isSaving" class="btn-primary">
          {{ isSaving ? 'Saving...' : 'Save Hook' }}
        </button>
        <button @click="handleCancel" class="btn-outline">
          Cancel
        </button>
      </div>
    </div>

    <div class="editor-container">
      <MonacoEditor
        v-model:value="editorContent"
        :language="language"
        :theme="theme"
        :options="editorOptions"
        @mount="handleEditorMount"
        class="monaco-editor-wrapper"
      />
    </div>

    <div v-if="validationMessage" class="validation-message" :class="validationClass">
      {{ validationMessage }}
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import { useMonacoHookEditor } from '~/composables/useMonacoHookEditor'

interface Props {
  hookId?: string
  initialContent?: string
  isEditMode?: boolean
}

interface Emits {
  (e: 'save', content: string, hookId?: string): void
  (e: 'cancel'): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emits>()

const {
  editorInstance,
  isReady,
  initializeEditor,
  getEditorValue,
  setEditorValue,
  formatCode
} = useMonacoHookEditor()

const editorContent = ref(props.initialContent || getDefaultHookTemplate())
const isSaving = ref(false)
const validationMessage = ref('')
const validationClass = ref('')

const language = 'knowledge-hook'
const theme = 'vs-dark'

const editorOptions = {
  fontSize: 14,
  minimap: { enabled: false },
  automaticLayout: true,
  tabSize: 2,
  formatOnPaste: true,
  formatOnType: true,
  scrollBeyondLastLine: false,
  wordWrap: 'on',
  lineNumbers: 'on',
  renderWhitespace: 'selection',
  bracketPairColorization: { enabled: true },
  suggest: {
    showMethods: true,
    showFunctions: true,
    showKeywords: true,
    showSnippets: true
  }
}

function getDefaultHookTemplate(): string {
  return `import { defineHook } from '@unrdf/knowledge-engine'

export const myHook = defineHook({
  meta: {
    name: 'example:hook',
    description: 'Example knowledge hook',
    ontology: ['fibo']
  },
  channel: {
    graphs: ['urn:graph:default'],
    view: 'delta'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/query.rq',
      sha256: 'hash-here',
      mediaType: 'application/sparql-query'
    }
  },
  determinism: { seed: 42 },
  receipt: { anchor: 'none' },

  async before({ payload, context }) {
    // Pre-execution validation
    return payload
  },

  async run({ payload, context }) {
    // Main execution logic
    return {
      result: { status: 'completed' }
    }
  },

  async after({ result, cancelled, reason }) {
    // Post-execution cleanup
    return { result }
  }
})
`
}

function handleEditorMount(editor: any, monaco: any) {
  initializeEditor(monaco, editor)
}

async function handleFormat() {
  await formatCode()
  validationMessage.value = 'Code formatted successfully'
  validationClass.value = 'success'
  setTimeout(() => {
    validationMessage.value = ''
  }, 3000)
}

async function handleValidate() {
  const content = getEditorValue()

  try {
    // Basic validation - check for required hook structure
    if (!content.includes('defineHook')) {
      throw new Error('Hook must use defineHook() function')
    }
    if (!content.includes('meta:')) {
      throw new Error('Hook must have meta property')
    }
    if (!content.includes('run')) {
      throw new Error('Hook must have run() lifecycle method')
    }

    validationMessage.value = 'Hook validation passed ✓'
    validationClass.value = 'success'
  } catch (error: any) {
    validationMessage.value = `Validation failed: ${error.message}`
    validationClass.value = 'error'
  }

  setTimeout(() => {
    validationMessage.value = ''
  }, 5000)
}

async function handleSave() {
  const content = getEditorValue()

  // Validate before saving
  try {
    if (!content.includes('defineHook')) {
      throw new Error('Invalid hook structure')
    }

    isSaving.value = true
    emit('save', content, props.hookId)
  } catch (error: any) {
    validationMessage.value = `Save failed: ${error.message}`
    validationClass.value = 'error'
  } finally {
    isSaving.value = false
  }
}

function handleCancel() {
  emit('cancel')
}

onMounted(() => {
  if (props.initialContent) {
    setEditorValue(props.initialContent)
  }
})
</script>

<style scoped>
.hook-editor {
  display: flex;
  flex-direction: column;
  height: 100%;
  gap: 1rem;
}

.editor-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem;
  background: #1e1e1e;
  border-bottom: 1px solid #333;
}

.editor-header h3 {
  margin: 0;
  color: #fff;
  font-size: 1.25rem;
}

.editor-actions {
  display: flex;
  gap: 0.5rem;
}

.editor-container {
  flex: 1;
  min-height: 500px;
  border: 1px solid #333;
  border-radius: 4px;
  overflow: hidden;
}

.monaco-editor-wrapper {
  height: 100%;
}

.validation-message {
  padding: 0.75rem 1rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.validation-message.success {
  background: #1e3a1e;
  color: #4caf50;
  border: 1px solid #4caf50;
}

.validation-message.error {
  background: #3a1e1e;
  color: #f44336;
  border: 1px solid #f44336;
}

.btn-primary, .btn-secondary, .btn-outline {
  padding: 0.5rem 1rem;
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

.btn-secondary {
  background: #333;
  color: white;
  border-color: #555;
}

.btn-secondary:hover:not(:disabled) {
  background: #444;
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
