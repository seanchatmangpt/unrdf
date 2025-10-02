/**
 * @file app/composables/useRuntime.mjs
 * @description Runtime status composable for monitoring system health
 */

import { ref, computed, onUnmounted } from 'vue'
import { RuntimeStatusSchema } from '~/schemas/runtime.mjs'

/**
 * Runtime status composable - Internal implementation
 * @returns {{
 *   status: import('vue').Ref<Object|null>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error|null>,
 *   fetchStatus: Function,
 *   startPolling: Function,
 *   stopPolling: Function,
 *   uptime: import('vue').ComputedRef<string>,
 *   memoryUsage: import('vue').ComputedRef<string>,
 *   hooksActive: import('vue').ComputedRef<number>
 * }}
 */
const _useRuntime = () => {
  /** @type {import('vue').Ref<Object|null>} */
  const status = ref(null)

  /** @type {import('vue').Ref<boolean>} */
  const loading = ref(false)

  /** @type {import('vue').Ref<Error|null>} */
  const error = ref(null)

  /** @type {import('vue').Ref<number|null>} */
  const pollInterval = ref(null)

  /**
   * Fetch runtime status from API
   * @returns {Promise<void>}
   */
  async function fetchStatus() {
    loading.value = true
    error.value = null

    try {
      const data = await $fetch('/api/runtime/status')

      // Validate response with Zod
      const validated = RuntimeStatusSchema.parse(data)
      status.value = validated
    } catch (err) {
      error.value = err
      console.error('[useRuntime] Failed to fetch status:', err)
      status.value = null
    } finally {
      loading.value = false
    }
  }

  /**
   * Start polling for status updates
   * @param {number} [interval=5000] - Poll interval in milliseconds
   * @returns {void}
   */
  function startPolling(interval = 5000) {
    // Clear existing interval
    if (pollInterval.value) {
      clearInterval(pollInterval.value)
    }

    // Initial fetch
    fetchStatus()

    // Setup periodic polling
    pollInterval.value = setInterval(() => {
      fetchStatus()
    }, interval)
  }

  /**
   * Stop polling for status updates
   * @returns {void}
   */
  function stopPolling() {
    if (pollInterval.value) {
      clearInterval(pollInterval.value)
      pollInterval.value = null
    }
  }

  /**
   * Computed: Formatted uptime string
   */
  const uptime = computed(() => {
    if (!status.value?.uptime) return 'N/A'

    const seconds = Math.floor(status.value.uptime)
    const days = Math.floor(seconds / 86400)
    const hours = Math.floor((seconds % 86400) / 3600)
    const minutes = Math.floor((seconds % 3600) / 60)
    const secs = seconds % 60

    const parts = []
    if (days > 0) parts.push(`${days}d`)
    if (hours > 0) parts.push(`${hours}h`)
    if (minutes > 0) parts.push(`${minutes}m`)
    parts.push(`${secs}s`)

    return parts.join(' ')
  })

  /**
   * Computed: Formatted memory usage string
   */
  const memoryUsage = computed(() => {
    if (!status.value?.memory?.heapUsed) return 'N/A'

    const heapUsedMB = (status.value.memory.heapUsed / 1024 / 1024).toFixed(2)
    const heapTotalMB = status.value.memory.heapTotal
      ? (status.value.memory.heapTotal / 1024 / 1024).toFixed(2)
      : '?'

    return `${heapUsedMB} MB / ${heapTotalMB} MB`
  })

  /**
   * Computed: Number of active hooks
   */
  const hooksActive = computed(() => {
    return status.value?.hooks?.active ?? 0
  })

  /**
   * Computed: Memory usage percentage
   */
  const memoryPercent = computed(() => {
    if (!status.value?.memory?.heapUsed || !status.value?.memory?.heapTotal) {
      return 0
    }
    return Math.round((status.value.memory.heapUsed / status.value.memory.heapTotal) * 100)
  })

  /**
   * Computed: Recent activity count
   */
  const recentActivityCount = computed(() => {
    return status.value?.recentActivity?.length ?? 0
  })

  // Cleanup on unmount
  onUnmounted(() => {
    stopPolling()
  })

  return {
    status,
    loading,
    error,
    fetchStatus,
    startPolling,
    stopPolling,
    uptime,
    memoryUsage,
    memoryPercent,
    hooksActive,
    recentActivityCount
  }
}

/**
 * Runtime status composable with shared state
 * @type {() => {
 *   status: import('vue').Ref<Object|null>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error|null>,
 *   fetchStatus: Function,
 *   startPolling: Function,
 *   stopPolling: Function,
 *   uptime: import('vue').ComputedRef<string>,
 *   memoryUsage: import('vue').ComputedRef<string>,
 *   memoryPercent: import('vue').ComputedRef<number>,
 *   hooksActive: import('vue').ComputedRef<number>,
 *   recentActivityCount: import('vue').ComputedRef<number>
 * }}
 */
export const useRuntime = createSharedComposable(_useRuntime)
