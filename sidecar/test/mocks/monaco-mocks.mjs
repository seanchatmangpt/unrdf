/**
 * @file Monaco Editor Test Mocks
 * @description Mock implementations for testing Monaco Editor integration
 */

import { vi } from 'vitest'

/**
 * Mock Monaco Editor instance
 * @returns {Object} Mocked editor instance
 */
export function createMockMonacoEditor() {
  const content = { value: '' }
  const model = {
    getLanguageId: vi.fn(() => 'sparql'),
    getValue: vi.fn(() => content.value),
    setValue: vi.fn((value) => { content.value = value }),
  }

  return {
    getModel: vi.fn(() => model),
    getValue: vi.fn(() => content.value),
    setValue: vi.fn((value) => { content.value = value }),
    updateOptions: vi.fn(),
    getOption: vi.fn((key) => {
      if (key === 'theme') return 'vs-dark'
      return undefined
    }),
    onDidChangeModelContent: vi.fn((callback) => {
      return { dispose: vi.fn() }
    }),
    trigger: vi.fn(),
    dispose: vi.fn(),
  }
}

/**
 * Mock completion provider
 * @returns {Array} Mock completions
 */
export function getMockCompletions() {
  return [
    { label: 'SELECT', kind: 'Keyword', insertText: 'SELECT' },
    { label: 'WHERE', kind: 'Keyword', insertText: 'WHERE' },
    { label: 'DISTINCT', kind: 'Keyword', insertText: 'DISTINCT' },
    { label: 'FILTER', kind: 'Keyword', insertText: 'FILTER' },
    { label: 'rdf:type', kind: 'Property', insertText: 'rdf:type' },
    { label: '?subject', kind: 'Variable', insertText: '?subject' },
    { label: '?predicate', kind: 'Variable', insertText: '?predicate' },
    { label: '?object', kind: 'Variable', insertText: '?object' },
  ]
}

/**
 * Mock validation markers
 * @param {boolean} hasErrors - Whether to include error markers
 * @returns {Array} Mock markers
 */
export function getMockMarkers(hasErrors = false) {
  if (!hasErrors) return []

  return [
    {
      severity: 'error',
      startLineNumber: 1,
      startColumn: 1,
      endLineNumber: 1,
      endColumn: 10,
      message: 'SPARQL syntax error: Unexpected token',
    },
  ]
}

/**
 * Mock hook API responses
 */
export const mockHookAPI = {
  create: vi.fn((hookData) => Promise.resolve({
    status: 201,
    data: {
      hookId: hookData.id,
      phase: hookData.phase,
      predicateCount: hookData.predicates?.length || 0,
    },
  })),

  evaluate: vi.fn((hookConfig) => Promise.resolve({
    fired: true,
    timestamp: new Date().toISOString(),
    predicates: hookConfig.predicates?.map(p => ({
      kind: p.kind,
      passed: true,
      value: 42,
    })) || [],
    duration: {
      queryMs: 12.5,
      evaluationMs: 8.3,
      totalMs: 20.8,
    },
  })),

  update: vi.fn((hookId, updates) => Promise.resolve({
    success: true,
    updated: {
      id: hookId,
      ...updates,
    },
  })),

  delete: vi.fn((hookId) => Promise.resolve({
    deleted: true,
    hookId,
  })),

  get: vi.fn((hookId) => Promise.resolve({
    id: hookId,
    select: 'SELECT ?s WHERE { ?s ?p ?o }',
    predicates: [],
    phase: 'AFTER_UPDATE',
  })),
}

/**
 * Mock Nuxt composables
 */
export function mockNuxtComposables() {
  return {
    useFetch: vi.fn((url, options) => ({
      data: ref(null),
      error: ref(null),
      pending: ref(false),
      refresh: vi.fn(),
    })),

    useAsyncData: vi.fn((key, fn) => ({
      data: ref(null),
      error: ref(null),
      pending: ref(false),
      refresh: vi.fn(),
    })),

    useRuntimeConfig: vi.fn(() => ({
      public: {
        apiBase: 'http://localhost:3456',
      },
    })),

    useRoute: vi.fn(() => ({
      params: { id: 'test-hook' },
      query: {},
    })),

    useRouter: vi.fn(() => ({
      push: vi.fn(),
      replace: vi.fn(),
    })),

    useState: vi.fn((key, init) => ref(init?.() || null)),

    ref: (value) => ({ value }),
    computed: (fn) => ({ value: fn() }),
    watch: vi.fn(),
    onMounted: vi.fn((fn) => fn()),
    onUnmounted: vi.fn(),
  }
}

/**
 * Mock Monaco module
 */
export const mockMonacoModule = {
  editor: {
    create: vi.fn((container, options) => createMockMonacoEditor()),
    defineTheme: vi.fn(),
    setTheme: vi.fn(),
  },
  languages: {
    register: vi.fn(),
    setMonarchTokensProvider: vi.fn(),
    registerCompletionItemProvider: vi.fn(),
    registerDocumentFormattingEditProvider: vi.fn(),
  },
  Uri: {
    parse: vi.fn((uri) => ({ toString: () => uri })),
  },
}
