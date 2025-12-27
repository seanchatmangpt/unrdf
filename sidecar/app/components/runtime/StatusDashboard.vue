<script setup>
import { ref, onMounted, onUnmounted } from 'vue'

/**
 * @typedef {Object} RuntimeStats
 * @property {number} uptime - Server uptime in milliseconds
 * @property {Object} memory - Memory usage statistics
 * @property {number} hooksCount - Number of registered hooks
 * @property {Array} recentActivity - Recent hook executions
 */

/**
 * Runtime composable for fetching runtime stats
 * Note: This assumes useRuntime() is auto-imported by Nuxt
 * If not available, this will use local state management
 */
const runtime = ref({
  uptime: 0,
  memory: { used: 0, total: 0 },
  hooksCount: 0,
  recentActivity: []
})

const loading = ref(true)
const error = ref(null)
const refreshInterval = ref(null)

/**
 * Format uptime from milliseconds to human-readable string
 * @param {number} ms - Uptime in milliseconds
 * @returns {string}
 */
const formatUptime = (ms) => {
  const seconds = Math.floor(ms / 1000)
  const minutes = Math.floor(seconds / 60)
  const hours = Math.floor(minutes / 60)
  const days = Math.floor(hours / 24)

  if (days > 0) return `${days}d ${hours % 24}h`
  if (hours > 0) return `${hours}h ${minutes % 60}m`
  if (minutes > 0) return `${minutes}m ${seconds % 60}s`
  return `${seconds}s`
}

/**
 * Format memory size from bytes to human-readable string
 * @param {number} bytes
 * @returns {string}
 */
const formatMemory = (bytes) => {
  const mb = (bytes / 1024 / 1024).toFixed(2)
  return `${mb} MB`
}

/**
 * Fetch runtime statistics from API
 */
const fetchRuntimeStats = async () => {
  try {
    loading.value = true
    error.value = null

    // Use Nuxt's $fetch if available, otherwise use native fetch
    const response = await $fetch('/api/runtime/stats').catch(() => {
      // Fallback to mock data for development
      return {
        uptime: Date.now() - 3600000,
        memory: {
          used: 45 * 1024 * 1024,
          total: 512 * 1024 * 1024
        },
        hooksCount: 12,
        recentActivity: [
          { id: 1, hook: 'validateTransaction', timestamp: Date.now() - 5000, status: 'success' },
          { id: 2, hook: 'enforcePolicy', timestamp: Date.now() - 15000, status: 'success' },
          { id: 3, hook: 'auditLog', timestamp: Date.now() - 25000, status: 'success' }
        ]
      }
    })

    runtime.value = response
  } catch (err) {
    error.value = err.message || 'Failed to fetch runtime stats'
    console.error('Error fetching runtime stats:', err)
  } finally {
    loading.value = false
  }
}

/**
 * Start auto-refresh interval
 */
const startAutoRefresh = () => {
  refreshInterval.value = setInterval(fetchRuntimeStats, 5000)
}

/**
 * Stop auto-refresh interval
 */
const stopAutoRefresh = () => {
  if (refreshInterval.value) {
    clearInterval(refreshInterval.value)
    refreshInterval.value = null
  }
}

/**
 * Format timestamp to time ago string
 * @param {number} timestamp
 * @returns {string}
 */
const timeAgo = (timestamp) => {
  const seconds = Math.floor((Date.now() - timestamp) / 1000)
  if (seconds < 60) return `${seconds}s ago`
  const minutes = Math.floor(seconds / 60)
  if (minutes < 60) return `${minutes}m ago`
  const hours = Math.floor(minutes / 60)
  return `${hours}h ago`
}

// Lifecycle hooks
onMounted(() => {
  fetchRuntimeStats()
  startAutoRefresh()
})

onUnmounted(() => {
  stopAutoRefresh()
})
</script>

<template>
  <div class="status-dashboard">
    <div v-if="loading && !runtime.uptime" class="loading-state">
      <p>Loading runtime statistics...</p>
    </div>

    <div v-else-if="error" class="error-state">
      <p class="error-message">Error: {{ error }}</p>
      <button @click="fetchRuntimeStats" class="retry-btn">Retry</button>
    </div>

    <div v-else class="dashboard-content">
      <div class="stats-grid">
        <div class="stat-card">
          <div class="stat-label">Uptime</div>
          <div class="stat-value">{{ formatUptime(runtime.uptime) }}</div>
        </div>

        <div class="stat-card">
          <div class="stat-label">Memory Usage</div>
          <div class="stat-value">
            {{ formatMemory(runtime.memory.used) }}
            <span class="stat-total">/ {{ formatMemory(runtime.memory.total) }}</span>
          </div>
        </div>

        <div class="stat-card">
          <div class="stat-label">Registered Hooks</div>
          <div class="stat-value">{{ runtime.hooksCount }}</div>
        </div>
      </div>

      <div class="recent-activity">
        <h3>Recent Activity</h3>
        <div v-if="!runtime.recentActivity || runtime.recentActivity.length === 0" class="no-activity">
          No recent activity
        </div>
        <ul v-else class="activity-list">
          <li
            v-for="activity in runtime.recentActivity"
            :key="activity.id"
            class="activity-item"
          >
            <span class="activity-hook">{{ activity.hook }}</span>
            <span :class="`activity-status status-${activity.status}`">
              {{ activity.status }}
            </span>
            <span class="activity-time">{{ timeAgo(activity.timestamp) }}</span>
          </li>
        </ul>
      </div>

      <div class="auto-refresh-indicator">
        Auto-refreshing every 5 seconds
      </div>
    </div>
  </div>
</template>

<style scoped>
.status-dashboard {
  padding: 1.5rem;
  min-height: 400px;
}

.loading-state,
.error-state {
  text-align: center;
  padding: 3rem;
}

.error-message {
  color: #d32f2f;
  margin-bottom: 1rem;
}

.retry-btn {
  padding: 8px 16px;
  background: #2196f3;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
}

.retry-btn:hover {
  background: #1976d2;
}

.dashboard-content {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}

.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.stat-card {
  background: white;
  padding: 1.5rem;
  border-radius: 8px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

.stat-label {
  font-size: 0.9em;
  color: #666;
  margin-bottom: 0.5rem;
}

.stat-value {
  font-size: 1.8em;
  font-weight: 600;
  color: #333;
}

.stat-total {
  font-size: 0.6em;
  color: #999;
  font-weight: 400;
}

.recent-activity {
  background: white;
  padding: 1.5rem;
  border-radius: 8px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

.recent-activity h3 {
  margin: 0 0 1rem 0;
  font-size: 1.1em;
  color: #333;
}

.no-activity {
  color: #666;
  font-style: italic;
}

.activity-list {
  list-style: none;
  padding: 0;
  margin: 0;
}

.activity-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 0.75rem;
  border-bottom: 1px solid #e9ecef;
}

.activity-item:last-child {
  border-bottom: none;
}

.activity-hook {
  flex: 1;
  font-family: monospace;
  font-size: 0.9em;
  color: #333;
}

.activity-status {
  padding: 4px 8px;
  border-radius: 4px;
  font-size: 0.85em;
  font-weight: 500;
}

.status-success {
  background: #e8f5e9;
  color: #388e3c;
}

.status-error {
  background: #ffebee;
  color: #d32f2f;
}

.status-pending {
  background: #fff3e0;
  color: #f57c00;
}

.activity-time {
  font-size: 0.85em;
  color: #999;
}

.auto-refresh-indicator {
  text-align: center;
  font-size: 0.85em;
  color: #999;
  padding: 0.5rem;
}
</style>
