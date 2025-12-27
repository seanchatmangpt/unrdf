/**
 * @file Monaco Editor Composable for Knowledge Hooks
 * @description Custom language support and auto-completion for Knowledge Hook DSL
 */

import { ref, onMounted } from 'vue'

/**
 * @typedef {Object} HookEditorConfig
 * @property {'vs-dark'|'vs-light'} [theme] - Editor theme
 * @property {string} [language] - Editor language
 * @property {boolean} [readOnly] - Whether editor is read-only
 */

/**
 * Hook DSL language configuration
 * @type {Object}
 */
const hookLanguageConfig = {
  id: 'knowledge-hook',
  extensions: ['.hook.mjs'],
  aliases: ['Knowledge Hook', 'hook'],
  mimetypes: ['application/javascript']
}

/**
 * Hook DSL monarch tokenizer
 * @type {Object}
 */
const hookLanguageDefinition = {
  keywords: [
    'defineHook', 'export', 'const', 'async', 'await', 'function',
    'return', 'if', 'else', 'true', 'false', 'null', 'undefined',
    'meta', 'channel', 'when', 'before', 'run', 'after',
    'determinism', 'receipt', 'payload', 'context', 'result'
  ],

  typeKeywords: [
    'KnowledgeHook', 'HookMeta', 'HookChannel', 'HookCondition',
    'HookEvent', 'HookResult', 'HookPayload', 'HookContext'
  ],

  operators: [
    '=', '>', '<', '!', '~', '?', ':',
    '==', '<=', '>=', '!=', '&&', '||', '++', '--',
    '+', '-', '*', '/', '&', '|', '^', '%', '<<',
    '>>', '>>>', '+=', '-=', '*=', '/=', '&=', '|=',
    '^=', '%=', '<<=', '>>=', '>>>='
  ],

  symbols: /[=><!~?:&|+\-*\/\^%]+/,

  tokenizer: {
    root: [
      [/[a-z_$][\w$]*/, {
        cases: {
          '@keywords': 'keyword',
          '@typeKeywords': 'type',
          '@default': 'identifier'
        }
      }],
      [/[A-Z][\w\$]*/, 'type.identifier'],
      { include: '@whitespace' },
      [/[{}()\[\]]/, '@brackets'],
      [/[<>](?!@symbols)/, '@brackets'],
      [/@symbols/, {
        cases: {
          '@operators': 'operator',
          '@default': ''
        }
      }],
      [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
      [/0[xX][0-9a-fA-F]+/, 'number.hex'],
      [/\d+/, 'number'],
      [/[;,.]/, 'delimiter'],
      [/"([^"\\]|\\.)*$/, 'string.invalid'],
      [/"/, 'string', '@string_double'],
      [/'([^'\\]|\\.)*$/, 'string.invalid'],
      [/'/, 'string', '@string_single'],
      [/`/, 'string', '@string_backtick']
    ],

    whitespace: [
      [/[ \t\r\n]+/, ''],
      [/\/\*/, 'comment', '@comment'],
      [/\/\/.*$/, 'comment']
    ],

    comment: [
      [/[^\/*]+/, 'comment'],
      [/\*\//, 'comment', '@pop'],
      [/[\/*]/, 'comment']
    ],

    string_double: [
      [/[^\\"]+/, 'string'],
      [/\\./, 'string.escape'],
      [/"/, 'string', '@pop']
    ],

    string_single: [
      [/[^\\']+/, 'string'],
      [/\\./, 'string.escape'],
      [/'/, 'string', '@pop']
    ],

    string_backtick: [
      [/[^\\`]+/, 'string'],
      [/\\./, 'string.escape'],
      [/`/, 'string', '@pop']
    ]
  }
}

/**
 * Auto-completion suggestions for hook lifecycle
 * @param {Object} monaco - Monaco editor instance
 * @returns {Array} Array of completion items
 */
const getHookCompletionItems = (monaco) => {
  const range = new monaco.Range(1, 1, 1, 1)

  return [
    {
      label: 'defineHook',
      kind: monaco.languages.CompletionItemKind.Function,
      insertText: `defineHook({
  meta: {
    name: '\${1:hookName}',
    description: '\${2:Hook description}',
    ontology: [\${3:'fibo'}]
  },
  channel: {
    graphs: ['\${4:urn:graph:default}'],
    view: '\${5|before,after,delta|}'
  },
  when: {
    kind: '\${6|sparql-ask,sparql-select,shacl|}',
    ref: {
      uri: '\${7:file://hooks/query.rq}',
      sha256: '\${8:hash}',
      mediaType: '\${9:application/sparql-query}'
    }
  },
  determinism: { seed: 42 },
  receipt: { anchor: 'none' },

  async before({ payload, context }) {
    \${10:// Pre-execution logic}
    return payload
  },

  async run({ payload, context }) {
    \${11:// Main execution logic}
    return { result: null }
  },

  async after({ result, cancelled, reason }) {
    \${12:// Post-execution logic}
    return { result }
  }
})`,
      insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
      documentation: 'Complete Knowledge Hook definition template',
      range
    },
    {
      label: 'before',
      kind: monaco.languages.CompletionItemKind.Method,
      insertText: `async before({ payload, context }) {
  \${1:// Pre-execution validation and transformation}
  if (!\${2:condition}) {
    return { cancel: true, reason: '\${3:Reason}' }
  }
  return payload
}`,
      insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
      documentation: 'Hook before lifecycle method',
      range
    },
    {
      label: 'run',
      kind: monaco.languages.CompletionItemKind.Method,
      insertText: `async run({ payload, context }) {
  \${1:// Main hook execution logic}
  return {
    result: \${2:null},
    assertions: [\${3}]
  }
}`,
      insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
      documentation: 'Hook run lifecycle method',
      range
    },
    {
      label: 'after',
      kind: monaco.languages.CompletionItemKind.Method,
      insertText: `async after({ result, cancelled, reason, context }) {
  if (cancelled) {
    \${1:// Handle cancellation}
  }
  \${2:// Post-execution cleanup and logging}
  return { result }
}`,
      insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
      documentation: 'Hook after lifecycle method',
      range
    }
  ]
}

/**
 * Composable for Monaco Editor with Knowledge Hook support
 * @param {HookEditorConfig} [config={}] - Editor configuration
 * @returns {Object} Editor instance and utility functions
 */
export function useMonacoHookEditor(config = {}) {
  /** @type {import('vue').Ref<Object|null>} */
  const editorInstance = ref(null)
  /** @type {import('vue').Ref<boolean>} */
  const isReady = ref(false)

  /**
   * Initialize the Monaco editor with custom language support
   * @param {Object} monaco - Monaco editor instance
   * @param {Object} editor - Editor instance
   */
  const initializeEditor = (monaco, editor) => {
    editorInstance.value = editor

    // Register custom language if not already registered
    const languages = monaco.languages.getLanguages()
    if (!languages.find(lang => lang.id === hookLanguageConfig.id)) {
      monaco.languages.register(hookLanguageConfig)
      monaco.languages.setMonarchTokensProvider(hookLanguageConfig.id, hookLanguageDefinition)

      // Register completion provider
      monaco.languages.registerCompletionItemProvider(hookLanguageConfig.id, {
        provideCompletionItems: (model, position) => {
          const word = model.getWordUntilPosition(position)
          const range = {
            startLineNumber: position.lineNumber,
            endLineNumber: position.lineNumber,
            startColumn: word.startColumn,
            endColumn: word.endColumn
          }

          return {
            suggestions: getHookCompletionItems(monaco).map(item => ({
              ...item,
              range
            }))
          }
        }
      })
    }

    isReady.value = true
  }

  /**
   * Get the current editor value
   * @returns {string} Current editor content
   */
  const getEditorValue = () => {
    return editorInstance.value?.getValue() || ''
  }

  /**
   * Set the editor value
   * @param {string} value - New editor content
   */
  const setEditorValue = (value) => {
    editorInstance.value?.setValue(value)
  }

  /**
   * Format the code in the editor
   * @returns {Promise<void>}
   */
  const formatCode = async () => {
    if (editorInstance.value) {
      await editorInstance.value.getAction('editor.action.formatDocument')?.run()
    }
  }

  return {
    editorInstance,
    isReady,
    initializeEditor,
    getEditorValue,
    setEditorValue,
    formatCode
  }
}


