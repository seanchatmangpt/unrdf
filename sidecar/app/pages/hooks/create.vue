<template>
  <NuxtLayout name="dashboard">
    <div class="create-hook-page">
      <header class="page-header">
        <h2>Create Knowledge Hook</h2>
        <p class="page-description">
          Define a new knowledge hook to validate, transform, or monitor RDF operations
        </p>
      </header>

      <HookForm
        @submit="handleSubmit"
        @cancel="handleCancel"
      />
    </div>
  </NuxtLayout>
</template>

<script setup>
/**
 * @fileoverview Create Knowledge Hook page
 * @module pages/hooks/create
 */

import { useRouter } from 'vue-router'
import HookForm from '~/components/hooks/HookForm.vue'

// Router
const router = useRouter()

// Set page metadata
useHead({
  title: 'Create Hook - UNRDF Sidecar',
  meta: [
    {
      name: 'description',
      content: 'Create a new knowledge hook for RDF operations'
    }
  ]
})

/**
 * Handle successful form submission
 * @param {Object} hook - Created hook data
 */
function handleSubmit(hook) {
  console.log('[CreateHookPage] Hook created:', hook)
  // Navigation handled by HookForm component
}

/**
 * Handle form cancellation
 */
function handleCancel() {
  router.push('/hooks')
}
</script>

<style scoped>
.create-hook-page {
  max-width: 1000px;
  margin: 0 auto;
  padding: 2rem 1rem;
}

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
</style>
