<template>
  <NuxtLayout name="dashboard">
    <div class="edit-hook-page">
      <!-- Loading State -->
      <div v-if="loading" class="page-loading">
        <div class="spinner"></div>
        <p>Loading hook...</p>
      </div>

      <!-- Error State -->
      <div v-else-if="error" class="page-error" role="alert">
        <h3>Error Loading Hook</h3>
        <p>{{ error.message }}</p>
        <button @click="fetchCurrentHook" class="btn-primary">Retry</button>
        <button @click="router.push('/hooks')" class="btn-secondary">Back to Hooks</button>
      </div>

      <!-- Edit Form -->
      <div v-else-if="currentHook">
        <header class="page-header">
          <h2>Edit Hook: {{ currentHook.name }}</h2>
          <p class="page-description">
            Modify hook configuration, predicates, and implementation
          </p>
        </header>

        <HookForm
          :initial-data="currentHook"
          :is-edit-mode="true"
          @submit="handleSubmit"
          @cancel="handleCancel"
        />
      </div>
    </div>
  </NuxtLayout>
</template>

<script setup>
/**
 * @fileoverview Edit Knowledge Hook page
 * @module pages/hooks/[id]/edit
 */

import { ref, onMounted } from 'vue'
import { useRouter, useRoute } from 'vue-router'
import { useKnowledgeHooks } from '~/composables/useKnowledgeHooks.mjs'
import HookForm from '~/components/hooks/HookForm.vue'

// Router
const router = useRouter()
const route = useRoute()

// Composables
const { currentHook, loading, error, fetchHook } = useKnowledgeHooks()

// Set page metadata
useHead({
  title: computed(() => currentHook.value ? `Edit ${currentHook.value.name} - UNRDF Sidecar` : 'Edit Hook - UNRDF Sidecar'),
  meta: [
    {
      name: 'description',
      content: 'Edit knowledge hook configuration and implementation'
    }
  ]
})

/**
 * Fetch current hook data
 */
async function fetchCurrentHook() {
  const hookId = route.params.id
  if (!hookId) {
    console.error('[EditHookPage] No hook ID provided')
    router.push('/hooks')
    return
  }

  await fetchHook(hookId)

  if (!currentHook.value) {
    console.error('[EditHookPage] Hook not found:', hookId)
    // Error will be displayed via error state
  }
}

/**
 * Handle successful form submission
 * @param {Object} hook - Updated hook data
 */
function handleSubmit(hook) {
  console.log('[EditHookPage] Hook updated:', hook)
  // Navigation handled by HookForm component
}

/**
 * Handle form cancellation
 */
function handleCancel() {
  router.push(`/hooks/${route.params.id}`)
}

/**
 * Lifecycle: Mount
 */
onMounted(() => {
  fetchCurrentHook()
})
</script>

<style scoped>
.edit-hook-page {
  max-width: 1000px;
  margin: 0 auto;
  padding: 2rem 1rem;
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
.page-error {
  text-align: center;
  padding: 3rem 2rem;
  background: #2a2a2a;
  border: 1px solid #ef4444;
  border-radius: 8px;
}

.page-error h3 {
  margin: 0 0 1rem 0;
  color: #ef4444;
  font-size: 1.5rem;
}

.page-error p {
  margin: 0 0 1.5rem 0;
  color: #9ca3af;
}

.page-error button {
  margin: 0 0.5rem;
}

/* Header */
.page-header {
  margin-bottom: 2rem;
}

.page-header h2 {
  margin: 0 0 0.5rem 0;
  font-size: 2rem;
  color: #e0e0e0;
}

.page-description {
  margin: 0;
  color: #9ca3af;
  font-size: 1rem;
  line-height: 1.5;
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
