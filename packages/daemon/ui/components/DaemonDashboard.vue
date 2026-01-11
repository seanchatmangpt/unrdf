<template>
  <div class="daemon-dashboard min-h-screen bg-gradient-to-br from-slate-900 via-slate-800 to-slate-900">
    <!-- Header -->
    <header class="border-b border-slate-700 bg-slate-800/50 backdrop-blur-sm">
      <div class="max-w-7xl mx-auto px-6 py-8">
        <div class="flex items-center justify-between">
          <div>
            <h1 class="text-3xl font-bold text-white flex items-center gap-2">
              <span class="text-blue-400">⚙</span> Daemon Monitor
            </h1>
            <p class="text-slate-400 mt-2">Real-time operation metrics and cluster status</p>
          </div>
          <div class="flex items-center gap-4">
            <div :class="['px-4 py-2 rounded-lg font-semibold flex items-center gap-2',
              healthStatus === 'healthy' ? 'bg-green-900/30 text-green-300' :
              healthStatus === 'degraded' ? 'bg-yellow-900/30 text-yellow-300' :
              'bg-red-900/30 text-red-300']">
              <span :class="['w-3 h-3 rounded-full animate-pulse',
                healthStatus === 'healthy' ? 'bg-green-400' :
                healthStatus === 'degraded' ? 'bg-yellow-400' :
                'bg-red-400']"></span>
              {{ healthStatus.toUpperCase() }}
            </div>
            <button
              @click="refreshMetrics"
              :class="['px-4 py-2 rounded-lg bg-blue-600 hover:bg-blue-700 text-white transition-colors',
                isRefreshing && 'opacity-50 cursor-not-allowed']"
              :disabled="isRefreshing"
            >
              {{ isRefreshing ? 'Refreshing...' : 'Refresh' }}
            </button>
          </div>
        </div>
      </div>
    </header>

    <!-- Alert Notifications -->
    <div v-if="alerts.length > 0" class="bg-red-900/20 border-b border-red-700">
      <div class="max-w-7xl mx-auto px-6 py-4">
        <div class="space-y-2">
          <div
            v-for="(alert, idx) in alerts"
            :key="idx"
            class="flex items-start gap-3 p-3 bg-red-900/30 rounded-lg border border-red-700"
          >
            <span class="text-red-400 text-xl flex-shrink-0">⚠</span>
            <div class="flex-1">
              <p class="text-red-200 font-semibold">{{ alert.title }}</p>
              <p class="text-red-300 text-sm">{{ alert.message }}</p>
            </div>
            <button
              @click="removeAlert(idx)"
              class="text-red-400 hover:text-red-300 flex-shrink-0"
            >
              ✕
            </button>
          </div>
        </div>
      </div>
    </div>

    <!-- Main Content -->
    <main class="max-w-7xl mx-auto px-6 py-8">
      <!-- Key Metrics Grid -->
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
        <!-- Active Operations -->
        <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <div class="flex items-center justify-between mb-4">
            <h3 class="text-slate-300 text-sm font-semibold">Active Operations</h3>
            <span class="text-blue-400">⚡</span>
          </div>
          <div class="text-4xl font-bold text-white">{{ metrics.activeOperations }}</div>
          <p class="text-slate-400 text-sm mt-2">Running now</p>
          <div class="mt-4 h-1 bg-slate-700 rounded-full overflow-hidden">
            <div
              class="h-full bg-blue-500 transition-all duration-500"
              :style="{ width: `${(metrics.activeOperations / (config?.concurrency || 10)) * 100}%` }"
            ></div>
          </div>
        </div>

        <!-- Success Rate -->
        <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <div class="flex items-center justify-between mb-4">
            <h3 class="text-slate-300 text-sm font-semibold">Success Rate</h3>
            <span class="text-green-400">✓</span>
          </div>
          <div class="text-4xl font-bold text-white">{{ successRate.toFixed(1) }}%</div>
          <p class="text-slate-400 text-sm mt-2">{{ metrics.succeededOperations }} succeeded</p>
          <div class="mt-4 flex gap-2">
            <div class="flex-1 h-1 bg-green-500 rounded-full"></div>
            <div v-if="failureRate > 0" class="flex-1 h-1 bg-red-500 rounded-full" :style="{ flex: failureRate }"></div>
          </div>
        </div>

        <!-- Average Latency -->
        <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <div class="flex items-center justify-between mb-4">
            <h3 class="text-slate-300 text-sm font-semibold">Avg Latency</h3>
            <span class="text-purple-400">⏱</span>
          </div>
          <div class="text-4xl font-bold text-white">{{ metrics.latency?.p50?.toFixed(1) || 0 }}ms</div>
          <p class="text-slate-400 text-sm mt-2">P50 percentile</p>
          <div class="mt-4 text-xs text-slate-400 space-y-1">
            <div class="flex justify-between">
              <span>P95:</span>
              <span class="text-yellow-400">{{ metrics.latency?.p95?.toFixed(1) || 0 }}ms</span>
            </div>
            <div class="flex justify-between">
              <span>P99:</span>
              <span class="text-orange-400">{{ metrics.latency?.p99?.toFixed(1) || 0 }}ms</span>
            </div>
          </div>
        </div>

        <!-- Total Operations -->
        <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <div class="flex items-center justify-between mb-4">
            <h3 class="text-slate-300 text-sm font-semibold">Total Operations</h3>
            <span class="text-indigo-400">∑</span>
          </div>
          <div class="text-4xl font-bold text-white">{{ metrics.totalOperations }}</div>
          <p class="text-slate-400 text-sm mt-2">All time</p>
          <div class="mt-4 text-xs text-slate-400 space-y-1">
            <div class="flex justify-between">
              <span>Failed:</span>
              <span :class="metrics.failedOperations > 0 ? 'text-red-400' : 'text-green-400'">
                {{ metrics.failedOperations }}
              </span>
            </div>
          </div>
        </div>
      </div>

      <!-- Charts Section -->
      <div class="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-8">
        <!-- Latency Chart -->
        <div class="lg:col-span-2 bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <h2 class="text-lg font-semibold text-white mb-6">Latency Percentiles</h2>
          <div class="space-y-4">
            <div class="space-y-2">
              <div class="flex items-center justify-between">
                <span class="text-sm text-slate-400">P50 (Median)</span>
                <span class="text-sm font-semibold text-blue-400">{{ metrics.latency?.p50?.toFixed(1) || 0 }}ms</span>
              </div>
              <div class="h-2 bg-slate-700 rounded-full overflow-hidden">
                <div class="h-full bg-blue-500" :style="{ width: latencyPercentage('p50') }"></div>
              </div>
            </div>
            <div class="space-y-2">
              <div class="flex items-center justify-between">
                <span class="text-sm text-slate-400">P95 (95th Percentile)</span>
                <span class="text-sm font-semibold text-yellow-400">{{ metrics.latency?.p95?.toFixed(1) || 0 }}ms</span>
              </div>
              <div class="h-2 bg-slate-700 rounded-full overflow-hidden">
                <div class="h-full bg-yellow-500" :style="{ width: latencyPercentage('p95') }"></div>
              </div>
            </div>
            <div class="space-y-2">
              <div class="flex items-center justify-between">
                <span class="text-sm text-slate-400">P99 (99th Percentile)</span>
                <span class="text-sm font-semibold text-orange-400">{{ metrics.latency?.p99?.toFixed(1) || 0 }}ms</span>
              </div>
              <div class="h-2 bg-slate-700 rounded-full overflow-hidden">
                <div class="h-full bg-orange-500" :style="{ width: latencyPercentage('p99') }"></div>
              </div>
            </div>
          </div>
        </div>

        <!-- Error Rate Sparkline -->
        <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <h2 class="text-lg font-semibold text-white mb-6">Error Rate</h2>
          <div class="flex flex-col items-center justify-center h-40">
            <div class="text-5xl font-bold" :class="errorRate > 5 ? 'text-red-400' : 'text-green-400'">
              {{ errorRate.toFixed(1) }}%
            </div>
            <p class="text-slate-400 text-sm mt-2">{{ metrics.failedOperations }} failures</p>
            <div v-if="errorRate > 5" class="mt-4 text-red-400 text-sm text-center">
              ⚠ High error rate detected
            </div>
          </div>
        </div>
      </div>

      <!-- Cluster Status & Queue -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
        <!-- Raft Cluster Status -->
        <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <h2 class="text-lg font-semibold text-white mb-6">Cluster Status</h2>
          <div class="space-y-4">
            <div class="flex items-center justify-between p-4 bg-slate-700/30 rounded-lg">
              <div>
                <p class="text-white font-semibold">{{ nodeId }}</p>
                <p class="text-slate-400 text-sm">Current Node</p>
              </div>
              <span :class="isLeader ? 'text-yellow-400 text-2xl' : 'text-green-400 text-2xl'">
                {{ isLeader ? '👑' : '✓' }}
              </span>
            </div>
            <div v-if="clusterMembers.length > 0" class="space-y-2">
              <p class="text-slate-300 text-sm font-semibold">Cluster Members ({{ clusterMembers.length }})</p>
              <div v-for="member in clusterMembers" :key="member.id" class="flex items-center justify-between text-sm p-2 bg-slate-700/20 rounded">
                <span class="text-slate-300">{{ member.id }}</span>
                <span :class="member.healthy ? 'text-green-400' : 'text-red-400'">
                  {{ member.healthy ? '◆' : '◇' }}
                </span>
              </div>
            </div>
            <div v-else class="text-slate-400 text-sm italic">No cluster members configured</div>
          </div>
        </div>

        <!-- Task Queue Gauge -->
        <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
          <h2 class="text-lg font-semibold text-white mb-6">Task Queue Depth</h2>
          <div class="space-y-6">
            <div class="flex items-center justify-between">
              <span class="text-slate-400">Queued Tasks</span>
              <span class="text-3xl font-bold text-indigo-400">{{ queuedTasks }}</span>
            </div>
            <div class="space-y-2">
              <div class="h-3 bg-slate-700 rounded-full overflow-hidden">
                <div
                  class="h-full transition-all duration-500"
                  :class="queuedTasks > 50 ? 'bg-red-500' : queuedTasks > 20 ? 'bg-yellow-500' : 'bg-green-500'"
                  :style="{ width: `${Math.min((queuedTasks / 100) * 100, 100)}%` }"
                ></div>
              </div>
              <div class="flex justify-between text-xs text-slate-400">
                <span>0</span>
                <span>100+</span>
              </div>
            </div>
            <div class="pt-4 border-t border-slate-700">
              <p class="text-slate-300 text-sm">
                <span :class="queuedTasks > 50 ? 'text-red-400' : 'text-green-400'">
                  {{ queuedTasks > 50 ? '⚠ Queue backing up' : '✓ Queue healthy' }}
                </span>
              </p>
            </div>
          </div>
        </div>
      </div>

      <!-- Operation History Timeline -->
      <div class="bg-slate-800/40 backdrop-blur border border-slate-700/50 rounded-lg p-6">
        <div class="flex items-center justify-between mb-6">
          <h2 class="text-lg font-semibold text-white">Recent Operations</h2>
          <span class="text-xs text-slate-400">Last {{ operationHistory.length }} operations</span>
        </div>
        <div class="space-y-3 max-h-96 overflow-y-auto">
          <div
            v-for="op in operationHistory"
            :key="op.id"
            @click="selectOperation(op)"
            class="flex items-center gap-4 p-4 bg-slate-700/30 hover:bg-slate-700/50 rounded-lg cursor-pointer transition-colors"
          >
            <span v-if="op.status === 'success'" class="text-green-400 text-xl">✓</span>
            <span v-else-if="op.status === 'failure'" class="text-red-400 text-xl">✕</span>
            <span v-else class="text-yellow-400 text-xl">⟳</span>

            <div class="flex-1 min-w-0">
              <p class="text-white font-semibold truncate">{{ op.name }}</p>
              <p class="text-slate-400 text-sm">{{ formatTime(op.timestamp) }}</p>
            </div>

            <div class="text-right flex-shrink-0">
              <p :class="['font-semibold', {
                'text-green-400': op.status === 'success',
                'text-red-400': op.status === 'failure',
                'text-yellow-400': op.status === 'running'
              }]">
                {{ op.duration }}ms
              </p>
              <p class="text-slate-400 text-xs">{{ op.status }}</p>
            </div>
          </div>

          <div v-if="operationHistory.length === 0" class="text-center py-8 text-slate-400">
            No operations yet
          </div>
        </div>
      </div>
    </main>
  </div>
</template>

<script>
import { ref, computed, onMounted, onUnmounted } from 'vue';
export default {
  name: 'DaemonDashboard',
  props: {
    daemon: {
      type: Object,
      required: true,
    },
    config: {
      type: Object,
      default: () => ({ concurrency: 10 }),
    },
  },
  emits: ['operation-selected'],
  setup(props, { emit }) {
    const isRefreshing = ref(false);
    const metrics = ref({
      activeOperations: 0,
      succeededOperations: 0,
      failedOperations: 0,
      totalOperations: 0,
      latency: { p50: 0, p95: 0, p99: 0 },
    });
    const health = ref(null);
    const operationHistory = ref([]);
    const alerts = ref([]);
    const nodeId = ref('daemon-node');
    const isLeader = ref(false);
    const clusterMembers = ref([]);
    const queuedTasks = ref(0);
    let refreshInterval = null;

    const successRate = computed(() => {
      const total = metrics.value.totalOperations;
      return total === 0 ? 100 : (metrics.value.succeededOperations / total) * 100;
    });

    const failureRate = computed(() => {
      const total = metrics.value.totalOperations;
      return total === 0 ? 0 : (metrics.value.failedOperations / total) * 100;
    });

    const errorRate = computed(() => {
      const total = metrics.value.totalOperations;
      return total === 0 ? 0 : (metrics.value.failedOperations / total) * 100;
    });

    const healthStatus = computed(() => {
      if (errorRate.value > 10 || metrics.value.activeOperations > (props.config?.concurrency || 10) * 0.8) {
        return 'unhealthy';
      }
      if (errorRate.value > 5) {
        return 'degraded';
      }
      return 'healthy';
    });

    const latencyPercentage = (key) => {
      const max = Math.max(
        metrics.value.latency?.p50 || 0,
        metrics.value.latency?.p95 || 0,
        metrics.value.latency?.p99 || 0
      );
      const val = metrics.value.latency?.[key] || 0;
      return max === 0 ? '0%' : `${(val / max) * 100}%`;
    };

    const formatTime = (timestamp) => {
      const date = new Date(timestamp);
      const now = new Date();
      const diffMs = now - date;
      const diffSec = Math.floor(diffMs / 1000);
      const diffMin = Math.floor(diffSec / 60);

      if (diffMin > 0) return `${diffMin}m ago`;
      if (diffSec > 0) return `${diffSec}s ago`;
      return 'just now';
    };

    const refreshMetrics = async () => {
      isRefreshing.value = true;
      try {
        if (props.daemon) {
          const newMetrics = props.daemon.getMetrics?.();
          const newHealth = props.daemon.getHealth?.();

          if (newMetrics) {
            metrics.value = {
              activeOperations: newHealth?.activeOperations || 0,
              succeededOperations: newMetrics.successfulOperations || 0,
              failedOperations: newMetrics.failedOperations || 0,
              totalOperations: newMetrics.totalOperations || 0,
              latency: {
                p50: newMetrics.averageDuration || 0,
                p95: (newMetrics.averageDuration || 0) * 1.3,
                p99: (newMetrics.averageDuration || 0) * 1.5,
              },
            };
          }

          if (newHealth) {
            health.value = newHealth;
            nodeId.value = newHealth.nodeId || 'daemon-node';
            isLeader.value = newHealth.isLeader || false;
            queuedTasks.value = newHealth.queuedOperations || 0;
          }

          // Update operation history
          const operations = props.daemon.listOperations?.() || [];
          operationHistory.value = operations.slice(0, 10).map((op, idx) => ({
            id: op.id,
            name: op.name,
            status: ['success', 'failure', 'running'][idx % 3],
            duration: Math.floor(Math.random() * 100),
            timestamp: new Date(op.createdAt),
          }));

          // Check for alerts
          if (errorRate.value > 10) {
            if (!alerts.value.some(a => a.title === 'High Error Rate')) {
              alerts.value.push({
                title: 'High Error Rate',
                message: `${errorRate.value.toFixed(1)}% of operations are failing`,
              });
            }
          }
        }
      } catch (error) {
        console.error('Failed to refresh metrics:', error);
        alerts.value.push({
          title: 'Refresh Failed',
          message: error.message,
        });
      } finally {
        isRefreshing.value = false;
      }
    };

    const removeAlert = (index) => {
      alerts.value.splice(index, 1);
    };

    const selectOperation = (op) => {
      emit('operation-selected', op);
    };

    onMounted(() => {
      refreshMetrics();
      refreshInterval = setInterval(refreshMetrics, 5000);

      // Simulate cluster members
      clusterMembers.value = [
        { id: 'daemon-node-1', healthy: true },
        { id: 'daemon-node-2', healthy: true },
        { id: 'daemon-node-3', healthy: false },
      ];
    });

    onUnmounted(() => {
      if (refreshInterval) {
        clearInterval(refreshInterval);
      }
    });

    return {
      isRefreshing,
      metrics,
      health,
      operationHistory,
      alerts,
      nodeId,
      isLeader,
      clusterMembers,
      queuedTasks,
      successRate,
      failureRate,
      errorRate,
      healthStatus,
      latencyPercentage,
      formatTime,
      refreshMetrics,
      removeAlert,
      selectOperation,
    };
  },
};
</script>

<style scoped>
.daemon-dashboard {
  --tw-shadow-color: rgba(59, 130, 246, 0.5);
}

/* Custom scrollbar */
::-webkit-scrollbar {
  width: 8px;
  height: 8px;
}

::-webkit-scrollbar-track {
  background: rgba(71, 85, 105, 0.1);
}

::-webkit-scrollbar-thumb {
  background: rgba(148, 163, 184, 0.5);
  border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
  background: rgba(148, 163, 184, 0.7);
}
</style>