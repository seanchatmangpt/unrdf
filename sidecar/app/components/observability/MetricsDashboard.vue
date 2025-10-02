<template>
  <div class="metrics-dashboard">
    <div v-if="loading" class="loading">
      <div class="spinner"></div>
      <p>Loading metrics...</p>
    </div>

    <div v-else-if="error" class="error">
      <p>Failed to load metrics: {{ error.message }}</p>
      <button @click="refresh" class="retry-btn">Retry</button>
    </div>

    <div v-else class="metrics-grid">
      <!-- Rate Limiting Card -->
      <div class="metric-card">
        <h3>Rate Limiting</h3>
        <div class="metric-value">{{ metrics.rateLimit.requests.toLocaleString() }}</div>
        <div class="metric-label">Total Requests</div>
        <div class="metric-stats">
          <div class="stat">
            <span class="stat-label">Allowed:</span>
            <span class="stat-value success">{{ metrics.rateLimit.allowed.toLocaleString() }}</span>
          </div>
          <div class="stat">
            <span class="stat-label">Blocked:</span>
            <span class="stat-value warning">{{ metrics.rateLimit.blocked.toLocaleString() }}</span>
          </div>
          <div class="stat">
            <span class="stat-label">Rate:</span>
            <span class="stat-value">{{ metrics.rateLimit.rate.toFixed(2) }}/s</span>
          </div>
        </div>
      </div>

      <!-- DDoS Protection Card -->
      <div class="metric-card">
        <h3>DDoS Protection</h3>
        <div class="metric-value" :class="getThreatClass(metrics.ddos.threatScore)">
          {{ (metrics.ddos.threatScore * 100).toFixed(1) }}%
        </div>
        <div class="metric-label">Threat Score</div>
        <div class="metric-stats">
          <div class="stat">
            <span class="stat-label">Blacklist Additions:</span>
            <span class="stat-value">{{ metrics.ddos.blacklistAdditions.toLocaleString() }}</span>
          </div>
          <div class="stat">
            <span class="stat-label">Blocked:</span>
            <span class="stat-value warning">{{ metrics.ddos.requestsBlocked.toLocaleString() }}</span>
          </div>
        </div>
      </div>

      <!-- Query Performance Card -->
      <div class="metric-card">
        <h3>Query Performance</h3>
        <div class="metric-value">{{ metrics.query.avgCost.toFixed(2) }}</div>
        <div class="metric-label">Average Cost</div>
        <div class="metric-stats">
          <div class="stat">
            <span class="stat-label">Total Queries:</span>
            <span class="stat-value">{{ metrics.query.total.toLocaleString() }}</span>
          </div>
          <div class="stat">
            <span class="stat-label">Rejected:</span>
            <span class="stat-value warning">{{ metrics.query.rejected.toLocaleString() }}</span>
          </div>
        </div>
      </div>

      <!-- Backpressure Card -->
      <div class="metric-card">
        <h3>Backpressure</h3>
        <div class="metric-value" :class="getLoadClass(metrics.backpressure.systemLoad)">
          {{ (metrics.backpressure.systemLoad * 100).toFixed(1) }}%
        </div>
        <div class="metric-label">System Load</div>
        <div class="metric-stats">
          <div class="stat">
            <span class="stat-label">Queue Depth:</span>
            <span class="stat-value">{{ metrics.backpressure.queueDepth.toLocaleString() }}</span>
          </div>
          <div class="stat">
            <span class="stat-label">Rejected:</span>
            <span class="stat-value warning">{{ metrics.backpressure.rejected.toLocaleString() }}</span>
          </div>
        </div>
      </div>
    </div>

    <div class="dashboard-controls">
      <button @click="refresh" class="btn-primary" :disabled="loading">
        Refresh
      </button>
      <label class="auto-refresh-toggle">
        <input type="checkbox" v-model="autoRefreshEnabled" @change="toggleAutoRefresh" />
        Auto-refresh (5s)
      </label>
    </div>
  </div>
</template>

<script setup>
/**
 * @file app/components/observability/MetricsDashboard.vue
 * @description OpenTelemetry metrics visualization dashboard
 */

import { useOTelMetrics } from '~/composables/useOTelMetrics.mjs'

const { metrics, loading, error, refresh, autoRefresh, stopAutoRefresh } = useOTelMetrics()

/** @type {import('vue').Ref<boolean>} */
const autoRefreshEnabled = ref(false)

/**
 * Toggle auto-refresh functionality
 */
function toggleAutoRefresh() {
  if (autoRefreshEnabled.value) {
    autoRefresh(5000)
  } else {
    stopAutoRefresh()
  }
}

/**
 * Get CSS class for threat score
 * @param {number} score - Threat score (0-1)
 * @returns {string}
 */
function getThreatClass(score) {
  if (score > 0.7) return 'danger'
  if (score > 0.4) return 'warning'
  return 'success'
}

/**
 * Get CSS class for system load
 * @param {number} load - System load (0-1)
 * @returns {string}
 */
function getLoadClass(load) {
  if (load > 0.8) return 'danger'
  if (load > 0.6) return 'warning'
  return 'success'
}

// Initial load
onMounted(() => {
  refresh()
})
</script>

<style scoped>
.metrics-dashboard {
  width: 100%;
}

.loading, .error {
  text-align: center;
  padding: 3rem;
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

.error {
  color: #ef4444;
}

.retry-btn {
  margin-top: 1rem;
  padding: 0.5rem 1rem;
  background: #3a3a3a;
  color: #e0e0e0;
  border: 1px solid #4a4a4a;
  border-radius: 4px;
  cursor: pointer;
}

.retry-btn:hover {
  background: #4a4a4a;
}

.metrics-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
  margin-bottom: 2rem;
}

.metric-card {
  background: #2a2a2a;
  border: 1px solid #3a3a3a;
  border-radius: 8px;
  padding: 1.5rem;
}

.metric-card h3 {
  margin: 0 0 1rem 0;
  color: #9ca3af;
  font-size: 0.875rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.metric-value {
  font-size: 2.5rem;
  font-weight: bold;
  margin-bottom: 0.5rem;
}

.metric-value.success {
  color: #10b981;
}

.metric-value.warning {
  color: #f59e0b;
}

.metric-value.danger {
  color: #ef4444;
}

.metric-label {
  color: #6b7280;
  font-size: 0.875rem;
  margin-bottom: 1rem;
}

.metric-stats {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  padding-top: 1rem;
  border-top: 1px solid #3a3a3a;
}

.stat {
  display: flex;
  justify-content: space-between;
  font-size: 0.875rem;
}

.stat-label {
  color: #9ca3af;
}

.stat-value {
  color: #e0e0e0;
  font-weight: 500;
}

.stat-value.success {
  color: #10b981;
}

.stat-value.warning {
  color: #f59e0b;
}

.dashboard-controls {
  display: flex;
  gap: 1rem;
  align-items: center;
  padding: 1rem;
  background: #2a2a2a;
  border-radius: 8px;
}

.btn-primary {
  padding: 0.5rem 1rem;
  background: #60a5fa;
  color: #1a1a1a;
  border: none;
  border-radius: 4px;
  font-weight: 500;
  cursor: pointer;
  transition: background 0.2s;
}

.btn-primary:hover:not(:disabled) {
  background: #3b82f6;
}

.btn-primary:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.auto-refresh-toggle {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  color: #9ca3af;
  cursor: pointer;
}

.auto-refresh-toggle input[type="checkbox"] {
  cursor: pointer;
}
</style>
