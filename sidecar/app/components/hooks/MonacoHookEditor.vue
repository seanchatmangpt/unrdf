<template>
  <div class="monaco-hook-editor">
    <!-- Editor Toolbar -->
    <div class="editor-toolbar" role="toolbar" aria-label="Editor controls">
      <div class="toolbar-group">
        <button
          type="button"
          @click="formatCode"
          class="toolbar-btn"
          :disabled="!isReady"
          aria-label="Format code"
          title="Format code (Shift+Alt+F)"
        >
          <span class="icon">⚡</span> Format
        </button>
        <button
          type="button"
          @click="insertTemplate"
          class="toolbar-btn"
          :disabled="!isReady"
          aria-label="Insert hook template"
          title="Insert Knowledge Hook template"
        >
          <span class="icon">📋</span> Insert Template
        </button>
      </div>
      <div class="toolbar-group">
        <span class="editor-status" :class="{ ready: isReady }" aria-live="polite">
          {{ isReady ? 'Editor Ready' : 'Loading...' }}
        </span>
      </div>
    </div>

    <!-- Monaco Editor Container -->
    <div
      ref="editorContainer"
      class="editor-container"
      role="region"
      aria-label="Code editor"
    ></div>

    <!-- Editor Footer -->
    <div class="editor-footer">
      <div class="footer-info">
        <span class="info-item">Language: Knowledge Hook DSL</span>
        <span class="info-item">Lines: {{ lineCount }}</span>
        <span class="info-item">Chars: {{ charCount }}</span>
      </div>
    </div>
  </div>
</template>

<script setup>
/**
 * @fileoverview Monaco Editor component with Knowledge Hook DSL support
 * @module components/hooks/MonacoHookEditor
 */

import { ref, computed, watch, onMounted, onBeforeUnmount } from 'vue'
import { useMonacoHookEditor } from '~/composables/useMonacoHookEditor.mjs'

/**
 * Component props
 * @typedef {Object} MonacoHookEditorProps
 * @property {string} [modelValue=''] - V-model binding for editor content
 * @property {string} [initialValue=''] - Initial editor value
 * @property {boolean} [readOnly=false] - Whether editor is read-only
 * @property {string} [theme='vs-dark'] - Editor theme
 * @property {number} [height=400] - Editor height in pixels
 */
const props = defineProps({
  /** @type {string} */
  modelValue: {
    type: String,
    default: ''
  },
  /** @type {string} */
  initialValue: {
    type: String,
    default: ''
  },
  /** @type {boolean} */
  readOnly: {
    type: Boolean,
    default: false
  },
  /** @type {string} */
  theme: {
    type: String,
    default: 'vs-dark'
  },
  /** @type {number} */
  height: {
    type: Number,
    default: 400
  }
})

/**
 * Component emits
 */
const emit = defineEmits(['update:modelValue', 'change', 'ready'])

// Refs
/** @type {import('vue').Ref<HTMLElement|null>} */
const editorContainer = ref(null)

/** @type {import('vue').Ref<Object|null>} */
const monacoInstance = ref(null)

/** @type {import('vue').Ref<Object|null>} */
const editor = ref(null)

// Composable
const { editorInstance, isReady, initializeEditor, getEditorValue, setEditorValue, formatCode: formatEditorCode } = useMonacoHookEditor()

// Computed
const lineCount = computed(() => {
  if (!editor.value) return 0
  return editor.value.getModel()?.getLineCount() || 0
})

const charCount = computed(() => {
  if (!editor.value) return 0
  return getEditorValue().length
})

/**
 * Knowledge Hook template
 */
const hookTemplate = `/**
 * Knowledge Hook Template
 * @see https://unrdf.dev/docs/knowledge-hooks
 */
export const hook = defineHook({
  meta: {
    name: 'my-hook',
    description: 'Hook description',
    ontology: ['fibo']
  },
  channel: {
    graphs: ['urn:graph:default'],
    view: 'before'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/query.rq',
      sha256: 'hash',
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
    return { result: null }
  },

  async after({ result, cancelled, reason }) {
    // Post-execution cleanup
    return { result }
  }
})`

/**
 * Initialize Monaco Editor
 */
async function initMonaco() {
  try {
    // Dynamically import Monaco
    const monaco = await import('monaco-editor')
    monacoInstance.value = monaco

    if (!editorContainer.value) {
      console.error('[MonacoHookEditor] Container ref not available')
      return
    }

    // Create editor instance
    const editorInst = monaco.editor.create(editorContainer.value, {
      value: props.initialValue || props.modelValue,
      language: 'knowledge-hook',
      theme: props.theme,
      readOnly: props.readOnly,
      automaticLayout: true,
      minimap: { enabled: true },
      scrollBeyondLastLine: false,
      fontSize: 13,
      lineNumbers: 'on',
      renderWhitespace: 'selection',
      tabSize: 2,
      insertSpaces: true,
      wordWrap: 'on',
      formatOnPaste: true,
      formatOnType: true,
      suggestOnTriggerCharacters: true,
      quickSuggestions: {
        other: true,
        comments: false,
        strings: true
      }
    })

    editor.value = editorInst

    // Initialize custom language support
    initializeEditor(monaco, editorInst)

    // Listen to content changes
    editorInst.onDidChangeModelContent(() => {
      const value = getEditorValue()
      emit('update:modelValue', value)
      emit('change', value)
    })

    // Emit ready event
    emit('ready', editorInst)

    console.log('[MonacoHookEditor] Editor initialized successfully')
  } catch (err) {
    console.error('[MonacoHookEditor] Failed to initialize:', err)
  }
}

/**
 * Format code in editor
 */
async function formatCode() {
  await formatEditorCode()
}

/**
 * Insert hook template at cursor
 */
function insertTemplate() {
  if (!editor.value) return

  const selection = editor.value.getSelection()
  const id = { major: 1, minor: 1 }
  const op = {
    identifier: id,
    range: selection,
    text: hookTemplate,
    forceMoveMarkers: true
  }

  editor.value.executeEdits('insert-template', [op])
  editor.value.focus()
}

/**
 * Watch for external value changes
 */
watch(() => props.modelValue, (newValue) => {
  if (editor.value && newValue !== getEditorValue()) {
    setEditorValue(newValue)
  }
})

/**
 * Watch for read-only changes
 */
watch(() => props.readOnly, (newValue) => {
  if (editor.value) {
    editor.value.updateOptions({ readOnly: newValue })
  }
})

/**
 * Lifecycle: Mount
 */
onMounted(() => {
  initMonaco()
})

/**
 * Lifecycle: Unmount
 */
onBeforeUnmount(() => {
  if (editor.value) {
    editor.value.dispose()
  }
})
</script>

<style scoped>
.monaco-hook-editor {
  display: flex;
  flex-direction: column;
  border: 1px solid #3a3a3a;
  border-radius: 4px;
  background: #1a1a1a;
  overflow: hidden;
}

/* Toolbar */
.editor-toolbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.5rem 0.75rem;
  background: #2a2a2a;
  border-bottom: 1px solid #3a3a3a;
}

.toolbar-group {
  display: flex;
  gap: 0.5rem;
  align-items: center;
}

.toolbar-btn {
  display: flex;
  align-items: center;
  gap: 0.25rem;
  padding: 0.375rem 0.75rem;
  background: #3a3a3a;
  color: #e0e0e0;
  border: none;
  border-radius: 4px;
  font-size: 0.8125rem;
  cursor: pointer;
  transition: background 0.2s;
}

.toolbar-btn:hover:not(:disabled) {
  background: #4a4a4a;
}

.toolbar-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.toolbar-btn .icon {
  font-size: 1rem;
}

.editor-status {
  font-size: 0.75rem;
  color: #9ca3af;
  padding: 0.25rem 0.5rem;
  background: #1a1a1a;
  border-radius: 4px;
}

.editor-status.ready {
  color: #10b981;
}

/* Editor Container */
.editor-container {
  min-height: 400px;
  height: v-bind('`${height}px`');
  width: 100%;
}

/* Footer */
.editor-footer {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.375rem 0.75rem;
  background: #2a2a2a;
  border-top: 1px solid #3a3a3a;
}

.footer-info {
  display: flex;
  gap: 1rem;
}

.info-item {
  font-size: 0.75rem;
  color: #6b7280;
}
</style>
