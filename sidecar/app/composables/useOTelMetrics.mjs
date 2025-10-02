/**
 * @file app/composables/useOTelMetrics.mjs
 * @description Composable for fetching and managing OpenTelemetry metrics
 */

/**
 * OpenTelemetry Metrics Composable - Internal implementation
 * @returns {{ metrics: import('vue').Ref<Object>, loading: import('vue').Ref<boolean>, error: import('vue').Ref<Error|null>, refresh: Function, autoRefresh: Function }}
 */
const _useOTelMetrics = () => {
  /** @type {import('vue').Ref<Object>} */
  const metrics = ref({
    rateLimit: {
      requests: 0,
      allowed: 0,
      blocked: 0,
      rate: 0
    },
    ddos: {
      threatScore: 0,
      blacklistAdditions: 0,
      requestsBlocked: 0
    },
    query: {
      avgCost: 0,
      rejected: 0,
      total: 0
    },
    backpressure: {
      queueDepth: 0,
      systemLoad: 0,
      rejected: 0
    }
  })

  /** @type {import('vue').Ref<boolean>} */
  const loading = ref(false)

  /** @type {import('vue').Ref<Error|null>} */
  const error = ref(null)

  /** @type {import('vue').Ref<number|null>} */
  const refreshTimer = ref(null)

  /**
   * Fetch metrics from API
   * @returns {Promise<void>}
   */
  async function refresh() {
    loading.value = true
    error.value = null

    try {
      const data = await $fetch('/api/otel/metrics')
      metrics.value = data
    } catch (err) {
      error.value = err
      console.error('[useOTelMetrics] Failed to fetch metrics:', err)
    } finally {
      loading.value = false
    }
  }

  /**
   * Enable auto-refresh of metrics
   * @param {number} interval - Refresh interval in milliseconds (default: 5000)
   * @returns {void}
   */
  function autoRefresh(interval = 5000) {
    // Clear existing timer
    if (refreshTimer.value) {
      clearInterval(refreshTimer.value)
    }

    // Initial fetch
    refresh()

    // Setup periodic refresh
    refreshTimer.value = setInterval(() => {
      refresh()
    }, interval)
  }

  /**
   * Stop auto-refresh
   * @returns {void}
   */
  function stopAutoRefresh() {
    if (refreshTimer.value) {
      clearInterval(refreshTimer.value)
      refreshTimer.value = null
    }
  }

  // Cleanup on unmount
  onUnmounted(() => {
    stopAutoRefresh()
  })

  return {
    metrics,
    loading,
    error,
    refresh,
    autoRefresh,
    stopAutoRefresh
  }
}

/**
 * OpenTelemetry Metrics Composable
 * @type {() => { metrics: import('vue').Ref<Object>, loading: import('vue').Ref<boolean>, error: import('vue').Ref<Error|null>, refresh: Function, autoRefresh: Function, stopAutoRefresh: Function }}
 */
export const useOTelMetrics = createSharedComposable(_useOTelMetrics)
