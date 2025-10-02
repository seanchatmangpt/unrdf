<script setup>
/**
 * Knowledge Hooks List Page
 * Displays all knowledge hooks with actions to edit, delete, and evaluate
 * Highest traffic page in the application
 */

definePageMeta({
  layout: 'dashboard'
})

/**
 * Knowledge hooks composable for data management
 */
const {
  hooks,
  loading,
  error,
  fetchHooks,
  deleteHook,
  evaluateHook
} = useKnowledgeHooks()

/**
 * Search/filter state
 */
const searchQuery = ref('')
const filterStatus = ref('all')

/**
 * Computed filtered hooks based on search and filter
 */
const filteredHooks = computed(() => {
  let result = hooks.value || []

  // Filter by search query
  if (searchQuery.value) {
    const query = searchQuery.value.toLowerCase()
    result = result.filter(hook =>
      hook.name?.toLowerCase().includes(query) ||
      hook.description?.toLowerCase().includes(query) ||
      hook.id?.toLowerCase().includes(query)
    )
  }

  // Filter by status
  if (filterStatus.value !== 'all') {
    result = result.filter(hook => hook.status === filterStatus.value)
  }

  return result
})

/**
 * Handle edit hook navigation
 * @param {string} id - Hook ID
 */
const handleEdit = (id) => {
  navigateTo(`/hooks/${id}/edit`)
}

/**
 * Handle delete hook with confirmation
 * @param {string} id - Hook ID
 */
const handleDelete = async (id) => {
  const hook = hooks.value.find(h => h.id === id)
  const hookName = hook?.name || id

  if (confirm(`Are you sure you want to delete hook "${hookName}"?`)) {
    try {
      await deleteHook(id)
      // Refresh hooks list
      await fetchHooks()
    } catch (err) {
      console.error('Error deleting hook:', err)
      alert(`Failed to delete hook: ${err.message}`)
    }
  }
}

/**
 * Handle evaluate hook
 * @param {string} id - Hook ID
 * @param {object} context - Evaluation context
 */
const handleEvaluate = async (id, context = {}) => {
  try {
    const result = await evaluateHook(id, context)

    // Display result in modal or alert
    alert(`Evaluation Result:\n${JSON.stringify(result, null, 2)}`)
  } catch (err) {
    console.error('Error evaluating hook:', err)
    alert(`Failed to evaluate hook: ${err.message}`)
  }
}

/**
 * Handle view hook details
 * @param {string} id - Hook ID
 */
const handleViewDetails = (id) => {
  navigateTo(`/hooks/${id}`)
}

/**
 * Refresh hooks list
 */
const handleRefresh = async () => {
  await fetchHooks()
}

/**
 * Initialize hooks on mount
 */
onMounted(() => {
  fetchHooks()
})
</script>

<template>
  <div class="hooks-page">
    <!-- Page Header -->
    <header class="page-header">
      <div class="header-content">
        <h1 class="page-title">Knowledge Hooks</h1>
        <div class="header-actions">
          <button @click="handleRefresh" class="refresh-btn" :disabled="loading">
            {{ loading ? 'Refreshing...' : 'Refresh' }}
          </button>
          <NuxtLink to="/hooks/create" class="create-btn">
            Create Hook
          </NuxtLink>
        </div>
      </div>

      <!-- Filters and Search -->
      <div class="filters-section">
        <div class="search-box">
          <input
            v-model="searchQuery"
            type="text"
            placeholder="Search hooks by name, description, or ID..."
            class="search-input"
          />
        </div>
        <div class="filter-box">
          <label for="status-filter" class="filter-label">Status:</label>
          <select v-model="filterStatus" id="status-filter" class="filter-select">
            <option value="all">All</option>
            <option value="active">Active</option>
            <option value="inactive">Inactive</option>
            <option value="draft">Draft</option>
          </select>
        </div>
      </div>
    </header>

    <!-- Loading State -->
    <div v-if="loading && !hooks.length" class="loading-state">
      <div class="spinner"></div>
      <p>Loading knowledge hooks...</p>
    </div>

    <!-- Error State -->
    <div v-else-if="error" class="error-state">
      <div class="error-icon">⚠️</div>
      <h2>Error Loading Hooks</h2>
      <p>{{ error }}</p>
      <button @click="handleRefresh" class="retry-btn">
        Retry
      </button>
    </div>

    <!-- Empty State -->
    <div v-else-if="!filteredHooks.length" class="empty-state">
      <div class="empty-icon">🪝</div>
      <h2>No Hooks Found</h2>
      <p v-if="searchQuery || filterStatus !== 'all'">
        Try adjusting your search or filters.
      </p>
      <p v-else>
        Get started by creating your first knowledge hook.
      </p>
      <NuxtLink to="/hooks/create" class="create-btn-large">
        Create Your First Hook
      </NuxtLink>
    </div>

    <!-- Hooks List -->
    <div v-else class="hooks-content">
      <div class="results-count">
        Showing {{ filteredHooks.length }} of {{ hooks.length }} hooks
      </div>

      <HooksList
        :hooks="filteredHooks"
        @edit="handleEdit"
        @delete="handleDelete"
        @evaluate="handleEvaluate"
        @view="handleViewDetails"
      />
    </div>
  </div>
</template>

<style scoped>
.hooks-page {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}

.page-header {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.header-content {
  display: flex;
  justify-content: space-between;
  align-items: center;
  flex-wrap: wrap;
  gap: 1rem;
}

.page-title {
  font-size: 1.8rem;
  font-weight: 600;
  color: #333;
  margin: 0;
}

.header-actions {
  display: flex;
  gap: 0.5rem;
}

.refresh-btn {
  padding: 0.6rem 1.2rem;
  background-color: #6c757d;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.9rem;
  transition: background-color 0.2s;
}

.refresh-btn:hover:not(:disabled) {
  background-color: #545b62;
}

.refresh-btn:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

.create-btn {
  padding: 0.6rem 1.5rem;
  background-color: #007bff;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.9rem;
  text-decoration: none;
  display: inline-block;
  transition: background-color 0.2s;
}

.create-btn:hover {
  background-color: #0056b3;
}

.filters-section {
  display: flex;
  gap: 1rem;
  flex-wrap: wrap;
}

.search-box {
  flex: 1;
  min-width: 250px;
}

.search-input {
  width: 100%;
  padding: 0.6rem 1rem;
  border: 1px solid #ddd;
  border-radius: 6px;
  font-size: 0.9rem;
}

.search-input:focus {
  outline: none;
  border-color: #007bff;
}

.filter-box {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.filter-label {
  font-size: 0.9rem;
  color: #666;
}

.filter-select {
  padding: 0.6rem 1rem;
  border: 1px solid #ddd;
  border-radius: 6px;
  font-size: 0.9rem;
  cursor: pointer;
}

.filter-select:focus {
  outline: none;
  border-color: #007bff;
}

.loading-state {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 4rem 2rem;
  text-align: center;
}

.spinner {
  width: 40px;
  height: 40px;
  border: 4px solid #f3f3f3;
  border-top: 4px solid #007bff;
  border-radius: 50%;
  animation: spin 1s linear infinite;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

.error-state,
.empty-state {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 4rem 2rem;
  text-align: center;
}

.error-icon,
.empty-icon {
  font-size: 4rem;
  margin-bottom: 1rem;
}

.error-state h2,
.empty-state h2 {
  font-size: 1.5rem;
  color: #333;
  margin: 0 0 0.5rem 0;
}

.error-state p,
.empty-state p {
  color: #666;
  margin: 0 0 1.5rem 0;
}

.retry-btn {
  padding: 0.6rem 1.5rem;
  background-color: #007bff;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.9rem;
}

.retry-btn:hover {
  background-color: #0056b3;
}

.create-btn-large {
  padding: 0.8rem 2rem;
  background-color: #007bff;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 1rem;
  text-decoration: none;
  display: inline-block;
}

.create-btn-large:hover {
  background-color: #0056b3;
}

.hooks-content {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.results-count {
  font-size: 0.9rem;
  color: #666;
}

/* Responsive Design */
@media (max-width: 768px) {
  .header-content {
    flex-direction: column;
    align-items: flex-start;
  }

  .page-title {
    font-size: 1.5rem;
  }

  .filters-section {
    flex-direction: column;
  }

  .search-box {
    min-width: 100%;
  }

  .filter-box {
    width: 100%;
  }

  .filter-select {
    flex: 1;
  }
}
</style>
