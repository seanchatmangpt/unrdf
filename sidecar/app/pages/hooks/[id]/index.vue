<template>
  <NuxtLayout name="dashboard">
    <div class="hook-detail-page">
      <!-- Navigation -->
      <nav class="page-nav" aria-label="Breadcrumb">
        <NuxtLink to="/hooks" class="nav-link">← Back to Hooks</NuxtLink>
      </nav>

      <!-- Loading State -->
      <div v-if="loading" class="page-loading">
        <div class="spinner"></div>
        <p>Loading hook details...</p>
      </div>

      <!-- Error State -->
      <div v-else-if="error" class="page-error" role="alert">
        <h3>Error Loading Hook</h3>
        <p>{{ error.message }}</p>
        <button @click="fetchCurrentHook" class="btn-primary">Retry</button>
        <button @click="router.push('/hooks')" class="btn-secondary">Back to Hooks</button>
      </div>

      <!-- Hook Detail -->
      <HookDetailView
        v-else-if="currentHook"
        :hook="currentHook"
        :loading="loading"
        :error="error"
        @edit="handleEdit"
        @delete="handleDelete"
        @evaluate="handleEvaluate"
        @retry="fetchCurrentHook"
      />

      <!-- Not Found State -->
      <div v-else class="page-not-found">
        <h3>Hook Not Found</h3>
        <p>The requested hook could not be found.</p>
        <button @click="router.push('/hooks')" class="btn-primary">Back to Hooks</button>
      </div>
    </div>
  </NuxtLayout>
</template>

<script setup>
/**
 * @fileoverview Knowledge Hook detail page
 * @module pages/hooks/[id]/index
 */

import { ref, onMounted, computed } from 'vue'
import { useRouter, useRoute } from 'vue-router'
import { useKnowledgeHooks } from '~/composables/useKnowledgeHooks.mjs'
import HookDetailView from '~/components/hooks/HookDetailView.vue'

// Router
const router = useRouter()
const route = useRoute()

// Composables
const { currentHook, loading, error, fetchHook } = useKnowledgeHooks()

// Set page metadata
useHead({
  title: computed(() => currentHook.value ? `${currentHook.value.name} - UNRDF Sidecar` : 'Hook Details - UNRDF Sidecar'),
  meta: [
    {
      name: 'description',
      content: computed(() => currentHook.value?.description || 'Knowledge hook details and configuration')
    }
  ]
})

/**
 * Fetch current hook data
 */
async function fetchCurrentHook() {
  const hookId = route.params.id
  if (!hookId) {
    console.error('[HookDetailPage] No hook ID provided')
    router.push('/hooks')
    return
  }

  console.log('[HookDetailPage] Fetching hook:', hookId)
  await fetchHook(hookId)

  if (!currentHook.value && !error.value) {
    console.warn('[HookDetailPage] Hook not found:', hookId)
  }
}

/**
 * Handle edit action
 */
function handleEdit() {
  router.push(`/hooks/${route.params.id}/edit`)
}

/**
 * Handle delete action
 * @param {string} hookId - ID of deleted hook
 */
function handleDelete(hookId) {
  console.log('[HookDetailPage] Hook deleted:', hookId)
  router.push('/hooks')
}

/**
 * Handle evaluate action
 * @param {Object} result - Evaluation result
 */
function handleEvaluate(result) {
  console.log('[HookDetailPage] Evaluation result:', result)
  // Result is displayed in HookDetailView component
}

/**
 * Lifecycle: Mount
 */
onMounted(() => {
  fetchCurrentHook()
})
</script>

<style scoped>
.hook-detail-page {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem 1rem;
}

/* Navigation */
.page-nav {
  margin-bottom: 1.5rem;
}

.nav-link {
  display: inline-flex;
  align-items: center;
  color: #60a5fa;
  text-decoration: none;
  font-size: 0.875rem;
  transition: color 0.2s;
}

.nav-link:hover {
  color: #3b82f6;
}

/* Loading State */
.page-loading {
  text-align: center;
  padding: 4rem 2rem;
}

.spinner {
  width: 50px;
  height: 50px;
  border: 4px solid #3a3a3a;
  border-top-color: #60a5fa;
  border-radius: 50%;
  animation: spin 1s linear infinite;
  margin: 0 auto 1rem;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}

.page-loading p {
  color: #9ca3af;
  margin: 0;
}

/* Error State */
.page-error,
.page-not-found {
  text-align: center;
  padding: 3rem 2rem;
  background: #2a2a2a;
  border: 1px solid #3a3a3a;
  border-radius: 8px;
}

.page-error {
  border-color: #ef4444;
}

.page-error h3,
.page-not-found h3 {
  margin: 0 0 1rem 0;
  font-size: 1.5rem;
}

.page-error h3 {
  color: #ef4444;
}

.page-not-found h3 {
  color: #e0e0e0;
}

.page-error p,
.page-not-found p {
  margin: 0 0 1.5rem 0;
  color: #9ca3af;
}

.page-error button,
.page-not-found button {
  margin: 0 0.5rem;
}

/* Buttons */
.btn-primary,
.btn-secondary {
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

.btn-primary:hover {
  background: #3b82f6;
}

.btn-secondary {
  background: #3a3a3a;
  color: #e0e0e0;
}

.btn-secondary:hover {
  background: #4a4a4a;
}
</style>
