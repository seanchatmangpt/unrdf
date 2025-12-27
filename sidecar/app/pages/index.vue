<script setup>
/**
 * Dashboard Home Page
 * Main entry point for authenticated users
 * Displays system status and quick stats
 */

definePageMeta({
  layout: 'dashboard'
})

const { user } = useAuth()

/**
 * Quick stats for dashboard overview
 * @type {import('vue').Ref<Array<{label: string, value: string | number, icon: string}>>}
 */
const stats = ref([
  { label: 'Total Hooks', value: '0', icon: '🪝' },
  { label: 'Active Policies', value: '0', icon: '📋' },
  { label: 'Executions Today', value: '0', icon: '⚡' },
  { label: 'Success Rate', value: '0%', icon: '✓' }
])

/**
 * Loading state for async operations
 */
const loading = ref(true)

/**
 * Error state
 */
const error = ref(null)

/**
 * Fetch dashboard statistics
 */
const fetchStats = async () => {
  try {
    loading.value = true
    error.value = null

    // Fetch stats from API
    const response = await useFetch('/api/dashboard/stats')

    if (response.data.value) {
      stats.value = [
        { label: 'Total Hooks', value: response.data.value.totalHooks || 0, icon: '🪝' },
        { label: 'Active Policies', value: response.data.value.activePolicies || 0, icon: '📋' },
        { label: 'Executions Today', value: response.data.value.executionsToday || 0, icon: '⚡' },
        {
          label: 'Success Rate',
          value: `${response.data.value.successRate || 0}%`,
          icon: '✓'
        }
      ]
    }
  } catch (err) {
    console.error('Error fetching dashboard stats:', err)
    error.value = 'Failed to load dashboard statistics'
  } finally {
    loading.value = false
  }
}

/**
 * Initialize dashboard on mount
 */
onMounted(() => {
  fetchStats()
})
</script>

<template>
  <div class="dashboard-home">
    <!-- Welcome Section -->
    <section class="welcome-section">
      <h1 class="welcome-title">
        Welcome back, {{ user?.name || user?.email || 'User' }}
      </h1>
      <p class="welcome-subtitle">
        Your Knowledge Engine Dashboard
      </p>
    </section>

    <!-- Quick Stats Cards -->
    <section class="stats-section">
      <h2 class="section-title">Quick Stats</h2>

      <div v-if="loading" class="loading-state">
        Loading statistics...
      </div>

      <div v-else-if="error" class="error-state">
        {{ error }}
      </div>

      <div v-else class="stats-grid">
        <div
          v-for="stat in stats"
          :key="stat.label"
          class="stat-card"
        >
          <div class="stat-icon">{{ stat.icon }}</div>
          <div class="stat-content">
            <div class="stat-value">{{ stat.value }}</div>
            <div class="stat-label">{{ stat.label }}</div>
          </div>
        </div>
      </div>
    </section>

    <!-- Status Dashboard Component -->
    <section class="status-section">
      <h2 class="section-title">System Status</h2>
      <StatusDashboard />
    </section>

    <!-- Quick Actions -->
    <section class="actions-section">
      <h2 class="section-title">Quick Actions</h2>
      <div class="action-buttons">
        <NuxtLink to="/hooks/create" class="action-btn primary">
          Create Hook
        </NuxtLink>
        <NuxtLink to="/policies/create" class="action-btn secondary">
          Create Policy
        </NuxtLink>
        <NuxtLink to="/hooks" class="action-btn secondary">
          View Hooks
        </NuxtLink>
        <NuxtLink to="/policies" class="action-btn secondary">
          View Policies
        </NuxtLink>
      </div>
    </section>
  </div>
</template>

<style scoped>
.dashboard-home {
  display: flex;
  flex-direction: column;
  gap: 2rem;
}

.welcome-section {
  padding-bottom: 1rem;
  border-bottom: 1px solid #e0e0e0;
}

.welcome-title {
  font-size: 2rem;
  font-weight: 600;
  color: #333;
  margin: 0 0 0.5rem 0;
}

.welcome-subtitle {
  font-size: 1rem;
  color: #666;
  margin: 0;
}

.section-title {
  font-size: 1.3rem;
  font-weight: 600;
  color: #333;
  margin: 0 0 1rem 0;
}

.stats-section {
  margin-top: 1rem;
}

.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.stat-card {
  background-color: #f9f9f9;
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  padding: 1.5rem;
  display: flex;
  align-items: center;
  gap: 1rem;
  transition: transform 0.2s, box-shadow 0.2s;
}

.stat-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
}

.stat-icon {
  font-size: 2rem;
}

.stat-content {
  display: flex;
  flex-direction: column;
}

.stat-value {
  font-size: 1.8rem;
  font-weight: 700;
  color: #333;
}

.stat-label {
  font-size: 0.9rem;
  color: #666;
}

.loading-state,
.error-state {
  padding: 2rem;
  text-align: center;
  background-color: #f9f9f9;
  border-radius: 8px;
  color: #666;
}

.error-state {
  background-color: #fff5f5;
  color: #cc0000;
}

.status-section {
  margin-top: 1rem;
}

.actions-section {
  margin-top: 1rem;
}

.action-buttons {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
  gap: 1rem;
}

.action-btn {
  padding: 1rem;
  text-align: center;
  border-radius: 6px;
  font-weight: 500;
  text-decoration: none;
  transition: transform 0.2s, box-shadow 0.2s;
  display: block;
}

.action-btn:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
}

.action-btn.primary {
  background-color: #007bff;
  color: white;
}

.action-btn.primary:hover {
  background-color: #0056b3;
}

.action-btn.secondary {
  background-color: #6c757d;
  color: white;
}

.action-btn.secondary:hover {
  background-color: #545b62;
}

/* Responsive Design */
@media (max-width: 768px) {
  .welcome-title {
    font-size: 1.5rem;
  }

  .stats-grid {
    grid-template-columns: 1fr;
  }

  .action-buttons {
    grid-template-columns: 1fr;
  }
}
</style>
