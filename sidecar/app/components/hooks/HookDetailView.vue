<template>
  <div class="hook-detail-view">
    <!-- Loading State -->
    <div v-if="loading" class="detail-skeleton">
      <div class="skeleton-header"></div>
      <div class="skeleton-content"></div>
      <div class="skeleton-content"></div>
      <div class="skeleton-editor"></div>
    </div>

    <!-- Error State -->
    <div v-else-if="error" class="detail-error" role="alert" aria-live="assertive">
      <h3>Error Loading Hook</h3>
      <p>{{ error.message }}</p>
      <button @click="$emit('retry')" class="btn-primary">Retry</button>
    </div>

    <!-- Hook Detail Content -->
    <div v-else-if="hook" class="hook-content">
      <!-- Header Section -->
      <header class="hook-header">
        <div class="header-main">
          <h2 class="hook-title">{{ hook.name }}</h2>
          <div class="hook-badges">
            <span class="badge phase-badge" :class="`phase-${hook.phase}`">
              {{ hook.phase.toUpperCase() }}
            </span>
            <span v-if="hook.disabled" class="badge disabled-badge">Disabled</span>
            <span class="badge priority-badge">Priority: {{ hook.priority }}</span>
          </div>
        </div>
        <div class="header-actions">
          <button
            @click="$emit('edit')"
            class="btn-secondary"
            aria-label="Edit hook"
          >
            Edit
          </button>
          <button
            @click="handleEvaluate"
            class="btn-primary"
            :disabled="evaluating"
            :aria-busy="evaluating"
          >
            {{ evaluating ? 'Evaluating...' : 'Evaluate' }}
          </button>
          <button
            @click="handleDelete"
            class="btn-danger"
            :disabled="deleting"
            aria-label="Delete hook"
          >
            {{ deleting ? 'Deleting...' : 'Delete' }}
          </button>
        </div>
      </header>

      <!-- Metadata Section -->
      <section class="detail-section" aria-labelledby="metadata-heading">
        <h3 id="metadata-heading">Metadata</h3>
        <div class="metadata-grid">
          <div class="metadata-item">
            <span class="metadata-label">ID:</span>
            <code class="metadata-value">{{ hook.id }}</code>
          </div>
          <div class="metadata-item">
            <span class="metadata-label">Phase:</span>
            <span class="metadata-value">{{ hook.phase }}</span>
          </div>
          <div class="metadata-item">
            <span class="metadata-label">Priority:</span>
            <span class="metadata-value">{{ hook.priority }}</span>
          </div>
          <div class="metadata-item">
            <span class="metadata-label">Status:</span>
            <span class="metadata-value" :class="{ disabled: hook.disabled }">
              {{ hook.disabled ? 'Disabled' : 'Enabled' }}
            </span>
          </div>
          <div v-if="hook.createdAt" class="metadata-item">
            <span class="metadata-label">Created:</span>
            <span class="metadata-value">{{ formatDate(hook.createdAt) }}</span>
          </div>
          <div v-if="hook.updatedAt" class="metadata-item">
            <span class="metadata-label">Updated:</span>
            <span class="metadata-value">{{ formatDate(hook.updatedAt) }}</span>
          </div>
        </div>

        <div v-if="hook.description" class="description">
          <h4>Description</h4>
          <p>{{ hook.description }}</p>
        </div>

        <div v-if="hook.tags.length" class="tags-section">
          <h4>Tags</h4>
          <div class="tag-list">
            <span v-for="tag in hook.tags" :key="tag" class="tag">{{ tag }}</span>
          </div>
        </div>
      </section>

      <!-- Predicates Section -->
      <section class="detail-section" aria-labelledby="predicates-heading">
        <h3 id="predicates-heading">Predicates ({{ hook.predicates.length }})</h3>
        <div
          v-for="(predicate, index) in hook.predicates"
          :key="index"
          class="predicate-card"
        >
          <div class="predicate-header">
            <span class="predicate-number">Predicate {{ index + 1 }}</span>
            <span class="predicate-type">{{ predicate.type.toUpperCase() }}</span>
          </div>
          <div class="predicate-content">
            <!-- SPARQL Predicate -->
            <div v-if="predicate.type === 'sparql' && predicate.query" class="query-display">
              <h5>SPARQL Query</h5>
              <pre class="code-block"><code>{{ predicate.query }}</code></pre>
            </div>

            <!-- Custom Predicate -->
            <div v-if="predicate.type === 'custom' && predicate.name" class="custom-function">
              <h5>Function</h5>
              <code>{{ predicate.name }}()</code>
              <div v-if="predicate.params && Object.keys(predicate.params).length" class="params">
                <h6>Parameters</h6>
                <pre class="code-block"><code>{{ JSON.stringify(predicate.params, null, 2) }}</code></pre>
              </div>
            </div>
          </div>
        </div>
      </section>

      <!-- SELECT Query Section -->
      <section v-if="hook.selectQuery" class="detail-section" aria-labelledby="query-heading">
        <h3 id="query-heading">SELECT Query</h3>
        <div class="query-container">
          <pre class="code-block"><code>{{ hook.selectQuery }}</code></pre>
          <div v-if="hook.selectQuerySha256" class="integrity-info">
            <span class="integrity-label">SHA-256:</span>
            <code class="integrity-hash">{{ hook.selectQuerySha256 }}</code>
          </div>
        </div>
      </section>

      <!-- Evaluation Result Section -->
      <section v-if="evaluationResult" class="detail-section evaluation-section" aria-labelledby="eval-heading">
        <h3 id="eval-heading">Latest Evaluation</h3>
        <div class="eval-summary" :class="{ success: evaluationResult.success, failure: !evaluationResult.success }">
          <div class="eval-status">
            <span class="status-icon">{{ evaluationResult.success ? '✓' : '✗' }}</span>
            <span class="status-text">
              {{ evaluationResult.success ? 'Passed' : 'Failed' }}
            </span>
          </div>
          <div class="eval-metadata">
            <span class="eval-item">Duration: {{ evaluationResult.duration }}ms</span>
            <span class="eval-item">Passed: {{ evaluationResult.passed ? 'Yes' : 'No' }}</span>
          </div>
        </div>

        <div v-if="evaluationResult.error" class="eval-error">
          <strong>Error:</strong> {{ evaluationResult.error }}
        </div>

        <div v-if="evaluationResult.results" class="predicate-results">
          <h4>Predicate Results</h4>
          <div
            v-for="(result, index) in evaluationResult.results"
            :key="index"
            class="result-item"
            :class="{ passed: result.passed, failed: !result.passed }"
          >
            <div class="result-header">
              <span>Predicate {{ index + 1 }}</span>
              <span class="result-status">{{ result.passed ? 'Passed' : 'Failed' }}</span>
            </div>
            <div v-if="result.error" class="result-error">{{ result.error }}</div>
          </div>
        </div>
      </section>

      <!-- Additional Metadata -->
      <section v-if="hook.metadata && Object.keys(hook.metadata).length" class="detail-section">
        <h3>Additional Metadata</h3>
        <pre class="code-block"><code>{{ JSON.stringify(hook.metadata, null, 2) }}</code></pre>
      </section>
    </div>
  </div>
</template>

<script setup>
/**
 * @fileoverview Read-only hook detail display component
 * @module components/hooks/HookDetailView
 */

import { ref, computed } from 'vue'
import { useKnowledgeHooks } from '~/composables/useKnowledgeHooks.mjs'

/**
 * Component props
 * @typedef {Object} HookDetailViewProps
 * @property {Object} hook - Hook data to display
 * @property {boolean} [loading=false] - Loading state
 * @property {Error|null} [error=null] - Error state
 */
const props = defineProps({
  /** @type {Object} */
  hook: {
    type: Object,
    required: true
  },
  /** @type {boolean} */
  loading: {
    type: Boolean,
    default: false
  },
  /** @type {Error|null} */
  error: {
    type: Object,
    default: null
  }
})

/**
 * Component emits
 */
const emit = defineEmits(['edit', 'delete', 'evaluate', 'retry'])

// Composables
const { evaluateHook, deleteHook } = useKnowledgeHooks()

// State
/** @type {import('vue').Ref<boolean>} */
const evaluating = ref(false)

/** @type {import('vue').Ref<boolean>} */
const deleting = ref(false)

/** @type {import('vue').Ref<Object|null>} */
const evaluationResult = ref(null)

/**
 * Format date to human-readable string
 * @param {string|number} date - Date to format
 * @returns {string} Formatted date
 */
function formatDate(date) {
  return new Date(date).toLocaleString('en-US', {
    year: 'numeric',
    month: 'long',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit'
  })
}

/**
 * Handle hook evaluation
 */
async function handleEvaluate() {
  if (!props.hook?.id) return

  evaluating.value = true
  try {
    const result = await evaluateHook(props.hook.id)
    evaluationResult.value = result
    emit('evaluate', result)
  } catch (err) {
    console.error('[HookDetailView] Evaluation failed:', err)
  } finally {
    evaluating.value = false
  }
}

/**
 * Handle hook deletion
 */
async function handleDelete() {
  if (!props.hook?.id) return

  if (!confirm(`Are you sure you want to delete "${props.hook.name}"?`)) {
    return
  }

  deleting.value = true
  try {
    const success = await deleteHook(props.hook.id)
    if (success) {
      emit('delete', props.hook.id)
    }
  } catch (err) {
    console.error('[HookDetailView] Deletion failed:', err)
  } finally {
    deleting.value = false
  }
}
</script>

<style scoped>
.hook-detail-view {
  width: 100%;
}

/* Loading Skeleton */
.detail-skeleton {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}

.skeleton-header,
.skeleton-content,
.skeleton-editor {
  background: linear-gradient(90deg, #2a2a2a 25%, #3a3a3a 50%, #2a2a2a 75%);
  background-size: 200% 100%;
  animation: skeleton-loading 1.5s ease-in-out infinite;
  border-radius: 4px;
}

.skeleton-header {
  height: 80px;
}

.skeleton-content {
  height: 120px;
}

.skeleton-editor {
  height: 300px;
}

@keyframes skeleton-loading {
  0% { background-position: 200% 0; }
  100% { background-position: -200% 0; }
}

/* Error State */
.detail-error {
  text-align: center;
  padding: 3rem;
  background: #2a2a2a;
  border: 1px solid #ef4444;
  border-radius: 8px;
}

.detail-error h3 {
  margin: 0 0 1rem 0;
  color: #ef4444;
}

.detail-error p {
  margin: 0 0 1.5rem 0;
  color: #9ca3af;
}

/* Hook Content */
.hook-content {
  display: flex;
  flex-direction: column;
  gap: 2rem;
}

/* Header */
.hook-header {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  padding-bottom: 1.5rem;
  border-bottom: 2px solid #3a3a3a;
}

.header-main {
  flex: 1;
}

.hook-title {
  margin: 0 0 0.75rem 0;
  font-size: 2rem;
  color: #e0e0e0;
}

.hook-badges {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.badge {
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: 500;
  text-transform: uppercase;
}

.phase-badge {
  background: #3a3a3a;
  color: #e0e0e0;
}

.phase-pre {
  background: #1e40af;
  color: #dbeafe;
}

.phase-post {
  background: #15803d;
  color: #dcfce7;
}

.phase-invariant {
  background: #9333ea;
  color: #f3e8ff;
}

.disabled-badge {
  background: #7f1d1d;
  color: #fecaca;
}

.priority-badge {
  background: #3a3a3a;
  color: #9ca3af;
}

.header-actions {
  display: flex;
  gap: 0.5rem;
}

/* Detail Sections */
.detail-section {
  background: #2a2a2a;
  border: 1px solid #3a3a3a;
  border-radius: 8px;
  padding: 1.5rem;
}

.detail-section h3 {
  margin: 0 0 1.5rem 0;
  font-size: 1.25rem;
  color: #e0e0e0;
  padding-bottom: 0.75rem;
  border-bottom: 1px solid #3a3a3a;
}

.detail-section h4 {
  margin: 1.5rem 0 0.75rem 0;
  font-size: 1rem;
  color: #9ca3af;
}

.detail-section h5 {
  margin: 0 0 0.5rem 0;
  font-size: 0.875rem;
  color: #6b7280;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

/* Metadata */
.metadata-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.metadata-item {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.metadata-label {
  font-size: 0.75rem;
  color: #6b7280;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.metadata-value {
  color: #e0e0e0;
  font-size: 0.875rem;
}

.metadata-value.disabled {
  color: #ef4444;
}

code.metadata-value {
  font-family: 'Monaco', 'Menlo', 'Consolas', monospace;
  font-size: 0.8125rem;
  background: #1a1a1a;
  padding: 0.125rem 0.375rem;
  border-radius: 2px;
}

.description p {
  margin: 0;
  color: #9ca3af;
  line-height: 1.6;
}

/* Tags */
.tag-list {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.tag {
  padding: 0.25rem 0.75rem;
  background: #3a3a3a;
  color: #e0e0e0;
  border-radius: 4px;
  font-size: 0.75rem;
}

/* Predicates */
.predicate-card {
  background: #1a1a1a;
  border: 1px solid #3a3a3a;
  border-radius: 4px;
  padding: 1rem;
  margin-bottom: 1rem;
}

.predicate-card:last-child {
  margin-bottom: 0;
}

.predicate-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid #3a3a3a;
}

.predicate-number {
  font-weight: 500;
  color: #e0e0e0;
}

.predicate-type {
  padding: 0.125rem 0.5rem;
  background: #3a3a3a;
  color: #9ca3af;
  border-radius: 4px;
  font-size: 0.75rem;
}

.predicate-content {
  color: #9ca3af;
}

/* Code Blocks */
.code-block {
  margin: 0.5rem 0;
  padding: 1rem;
  background: #1a1a1a;
  border: 1px solid #3a3a3a;
  border-radius: 4px;
  overflow-x: auto;
}

.code-block code {
  font-family: 'Monaco', 'Menlo', 'Consolas', monospace;
  font-size: 0.8125rem;
  color: #e0e0e0;
  line-height: 1.5;
}

/* Integrity Info */
.integrity-info {
  margin-top: 0.5rem;
  padding: 0.5rem;
  background: #1a1a1a;
  border-radius: 4px;
  font-size: 0.75rem;
}

.integrity-label {
  color: #6b7280;
  margin-right: 0.5rem;
}

.integrity-hash {
  color: #9ca3af;
  font-family: 'Monaco', 'Menlo', 'Consolas', monospace;
}

/* Evaluation Section */
.evaluation-section {
  border: 2px solid #3a3a3a;
}

.eval-summary {
  padding: 1rem;
  border-radius: 4px;
  margin-bottom: 1rem;
}

.eval-summary.success {
  background: #064e3b;
  border: 1px solid #10b981;
}

.eval-summary.failure {
  background: #7f1d1d;
  border: 1px solid #ef4444;
}

.eval-status {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.status-icon {
  font-size: 1.5rem;
}

.status-text {
  font-size: 1.125rem;
  font-weight: 600;
}

.eval-metadata {
  display: flex;
  gap: 1rem;
  font-size: 0.875rem;
  color: #9ca3af;
}

.eval-error {
  padding: 0.75rem;
  background: #7f1d1d;
  border: 1px solid #991b1b;
  border-radius: 4px;
  color: #fecaca;
  margin-bottom: 1rem;
}

.predicate-results {
  margin-top: 1rem;
}

.result-item {
  padding: 0.75rem;
  margin-bottom: 0.5rem;
  border-radius: 4px;
}

.result-item.passed {
  background: #064e3b;
  border: 1px solid #10b981;
}

.result-item.failed {
  background: #7f1d1d;
  border: 1px solid #ef4444;
}

.result-header {
  display: flex;
  justify-content: space-between;
  font-size: 0.875rem;
  font-weight: 500;
}

.result-status {
  color: #9ca3af;
}

.result-error {
  margin-top: 0.5rem;
  font-size: 0.8125rem;
  color: #fecaca;
}

/* Buttons */
.btn-primary,
.btn-secondary,
.btn-danger {
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 4px;
  font-size: 0.875rem;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.2s;
}

.btn-primary {
  background: #60a5fa;
  color: #1a1a1a;
}

.btn-primary:hover:not(:disabled) {
  background: #3b82f6;
}

.btn-primary:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.btn-secondary {
  background: #3a3a3a;
  color: #e0e0e0;
}

.btn-secondary:hover {
  background: #4a4a4a;
}

.btn-danger {
  background: #7f1d1d;
  color: #fecaca;
}

.btn-danger:hover:not(:disabled) {
  background: #991b1b;
}

.btn-danger:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}
</style>
