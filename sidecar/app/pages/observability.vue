<template>
  <NuxtLayout name="dashboard">
    <div class="observability-page">
      <header class="page-header">
        <h2>OpenTelemetry Metrics</h2>
        <p class="page-description">
          Real-time monitoring of rate limiting, DDoS protection, query performance, and backpressure
        </p>
      </header>

      <MetricsDashboard />
    </div>
  </NuxtLayout>
</template>

<script setup>
/**
 * @file app/pages/observability.vue
 * @description Observability dashboard page
 */

import MetricsDashboard from '~/components/observability/MetricsDashboard.vue'

// Set page metadata
useHead({
  title: 'Observability - UNRDF Sidecar',
  meta: [
    {
      name: 'description',
      content: 'OpenTelemetry metrics dashboard for UNRDF sidecar monitoring'
    }
  ]
})
</script>

<style scoped>
.observability-page {
  max-width: 1400px;
  margin: 0 auto;
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
}
</style>
