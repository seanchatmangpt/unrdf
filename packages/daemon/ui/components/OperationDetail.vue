<template>
  <div class="operation-detail">
    <!-- Header -->
    <div class="bg-slate-800/40 backdrop-blur border-b border-slate-700/50 px-6 py-6">
      <div class="flex items-center justify-between mb-4">
        <button
          @click="$emit('close')"
          class="text-slate-400 hover:text-white transition-colors flex items-center gap-2"
        >
          ← Back
        </button>
        <span
          :class="['px-3 py-1 rounded-full text-sm font-semibold', {
            'bg-green-900/30 text-green-300': operation.status === 'success',
            'bg-red-900/30 text-red-300': operation.status === 'failure',
            'bg-yellow-900/30 text-yellow-300': operation.status === 'running',
            'bg-blue-900/30 text-blue-300': operation.status === 'pending'
          }]"
        >
          {{ operation.status }}
        </span>
      </div>
      <div>
        <h1 class="text-3xl font-bold text-white">{{ operation.name }}</h1>
        <p class="text-slate-400 mt-2">{{ operation.id }}</p>
      </div>
    </div>

    <!-- Main Content -->
    <div class="overflow-y-auto flex-1">
      <!-- Execution Summary -->
      <section class="bg-slate-800/20 border-b border-slate-700/50 px-6 py-6">
        <h2 class="text-lg font-semibold text-white mb-6">Execution Summary</h2>
        <div class="grid grid-cols-2 md:grid-cols-4 gap-4">
          <div class="bg-slate-800/40 rounded-lg p-4">
            <p class="text-slate-400 text-sm mb-1">Duration</p>
            <p class="text-2xl font-bold text-blue-400">{{ operation.duration || 0 }}ms</p>
          </div>
          <div class="bg-slate-800/40 rounded-lg p-4">
            <p class="text-slate-400 text-sm mb-1">Attempts</p>
            <p class="text-2xl font-bold text-purple-400">{{ operation.attempts || 1 }}</p>
          </div>
          <div class="bg-slate-800/40 rounded-lg p-4">
            <p class="text-slate-400 text-sm mb-1">Started</p>
            <p class="text-xs text-white font-mono">{{ formatDateTime(operation.startedAt) }}</p>
          </div>
          <div class="bg-slate-800/40 rounded-lg p-4">
            <p class="text-slate-400 text-sm mb-1">Completed</p>
            <p class="text-xs text-white font-mono">{{ formatDateTime(operation.completedAt) }}</p>
          </div>
        </div>
      </section>

      <!-- Tabs -->
      <div class="sticky top-0 bg-slate-800/40 border-b border-slate-700/50 flex">
        <button
          v-for="tab in tabs"
          :key="tab"
          @click="activeTab = tab"
          :class="['flex-1 px-6 py-4 font-semibold transition-colors border-b-2', {
            'border-blue-500 text-blue-400': activeTab === tab,
            'border-transparent text-slate-400 hover:text-slate-300': activeTab !== tab
          }]"
        >
          {{ tab }}
        </button>
      </div>

      <!-- Tab Content -->
      <div class="px-6 py-6">
        <!-- Metadata Tab -->
        <div v-if="activeTab === 'Metadata'" class="space-y-4">
          <h3 class="text-white font-semibold mb-4">Operation Metadata</h3>
          <div class="bg-slate-800/40 rounded-lg p-4 space-y-3">
            <div v-for="(value, key) in operation.metadata" :key="key" class="border-b border-slate-700/50 pb-3 last:border-0">
              <p class="text-slate-400 text-sm font-semibold">{{ key }}</p>
              <p class="text-white mt-1 break-words">{{ formatValue(value) }}</p>
            </div>
          </div>
        </div>

        <!-- Payload Tab -->
        <div v-if="activeTab === 'Payload'" class="space-y-4">
          <h3 class="text-white font-semibold mb-4">Operation Payload</h3>
          <div class="bg-slate-900/50 rounded-lg p-4 overflow-x-auto">
            <pre class="text-sm text-green-400 font-mono whitespace-pre-wrap break-words">{{ JSON.stringify(operation.payload, null, 2) }}</pre>
          </div>
        </div>

        <!-- Result Tab -->
        <div v-if="activeTab === 'Result'" class="space-y-4">
          <h3 class="text-white font-semibold mb-4">Execution Result</h3>
          <div v-if="operation.result" class="bg-slate-900/50 rounded-lg p-4 overflow-x-auto">
            <pre class="text-sm text-blue-400 font-mono whitespace-pre-wrap break-words">{{ JSON.stringify(operation.result, null, 2) }}</pre>
          </div>
          <div v-else class="bg-slate-800/40 rounded-lg p-4 text-slate-400 italic">
            No result available
          </div>
        </div>

        <!-- Error Details Tab -->
        <div v-if="activeTab === 'Error Details'" class="space-y-4">
          <h3 class="text-white font-semibold mb-4">Error Information</h3>
          <div v-if="operation.error" class="space-y-4">
            <!-- Error Code -->
            <div class="bg-red-900/20 border border-red-700/50 rounded-lg p-4">
              <p class="text-red-400 text-sm font-semibold mb-2">Error Code</p>
              <p class="text-red-300 font-mono">{{ operation.error.code }}</p>
            </div>

            <!-- Error Message -->
            <div class="bg-red-900/20 border border-red-700/50 rounded-lg p-4">
              <p class="text-red-400 text-sm font-semibold mb-2">Message</p>
              <p class="text-red-300 break-words">{{ operation.error.message }}</p>
            </div>

            <!-- Stack Trace -->
            <div v-if="operation.error.stack" class="bg-slate-900/50 rounded-lg p-4 overflow-x-auto">
              <p class="text-slate-300 text-sm font-semibold mb-2">Stack Trace</p>
              <pre class="text-xs text-red-400 font-mono whitespace-pre-wrap break-words">{{ operation.error.stack }}</pre>
            </div>
          </div>
          <div v-else class="bg-slate-800/40 rounded-lg p-4 text-green-400 italic">
            ✓ No errors
          </div>
        </div>

        <!-- Execution History Tab -->
        <div v-if="activeTab === 'Execution History'" class="space-y-4">
          <h3 class="text-white font-semibold mb-4">Retry Attempts ({{ operation.attempts || 1 }})</h3>
          <div v-if="executionHistory.length > 0" class="space-y-3">
            <div
              v-for="(attempt, idx) in executionHistory"
              :key="idx"
              class="bg-slate-800/40 rounded-lg p-4 border-l-4"
              :class="attempt.status === 'success' ? 'border-green-500' : 'border-red-500'"
            >
              <div class="flex items-center justify-between mb-2">
                <p class="text-white font-semibold">Attempt {{ idx + 1 }}</p>
                <span :class="['text-sm font-semibold', attempt.status === 'success' ? 'text-green-400' : 'text-red-400']">
                  {{ attempt.status }}
                </span>
              </div>
              <p class="text-slate-400 text-sm">{{ formatDateTime(attempt.timestamp) }}</p>
              <p class="text-slate-300 text-sm mt-1">Duration: {{ attempt.duration }}ms</p>
              <div v-if="attempt.message" class="mt-2 text-slate-300 text-sm">{{ attempt.message }}</div>
            </div>
          </div>
          <div v-else class="bg-slate-800/40 rounded-lg p-4 text-slate-400 italic">
            No additional attempts
          </div>
        </div>

        <!-- Proof Tab -->
        <div v-if="activeTab === 'Proof'" class="space-y-4">
          <h3 class="text-white font-semibold mb-4">Cryptographic Proof</h3>
          <div v-if="operation.proof" class="space-y-4">
            <!-- Hash -->
            <div class="bg-slate-800/40 rounded-lg p-4">
              <p class="text-slate-400 text-sm font-semibold mb-2">Merkle Hash</p>
              <p class="text-amber-400 font-mono text-xs break-all">{{ operation.proof.hash }}</p>
            </div>

            <!-- Timestamp -->
            <div class="bg-slate-800/40 rounded-lg p-4">
              <p class="text-slate-400 text-sm font-semibold mb-2">Timestamp</p>
              <p class="text-white">{{ formatDateTime(operation.proof.timestamp) }}</p>
            </div>

            <!-- Signature -->
            <div v-if="operation.proof.signature" class="bg-slate-800/40 rounded-lg p-4">
              <p class="text-slate-400 text-sm font-semibold mb-2">Signature</p>
              <p class="text-purple-400 font-mono text-xs break-all">{{ operation.proof.signature }}</p>
            </div>
          </div>
          <div v-else class="bg-slate-800/40 rounded-lg p-4 text-slate-400 italic">
            No proof available
          </div>
        </div>
      </div>
    </div>

    <!-- Footer Actions -->
    <div class="border-t border-slate-700/50 bg-slate-800/40 px-6 py-4 flex gap-3">
      <button
        @click="copyToClipboard"
        class="flex-1 px-4 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-lg font-semibold transition-colors"
      >
        Copy Details
      </button>
      <button
        @click="exportAsJSON"
        class="flex-1 px-4 py-2 bg-slate-700 hover:bg-slate-600 text-white rounded-lg font-semibold transition-colors"
      >
        Export JSON
      </button>
    </div>
  </div>
</template>

<script>
import { ref, computed } from 'vue';
export default {
  name: 'OperationDetail',
  props: {
    operation: {
      type: Object,
      required: true,
      default: () => ({
        id: '',
        name: '',
        status: 'pending',
        duration: 0,
        attempts: 1,
        startedAt: new Date(),
        completedAt: null,
        metadata: {},
        payload: {},
        result: null,
        error: null,
        proof: null,
      }),
    },
  },
  emits: ['close'],
  setup(props) {
    const activeTab = ref('Metadata');
    const tabs = ref(['Metadata', 'Payload', 'Result', 'Error Details', 'Execution History', 'Proof']);
    const executionHistory = computed(() => {
      if (!props.operation.attempts || props.operation.attempts <= 1) return [];
      return Array.from({ length: props.operation.attempts }).map((_, idx) => ({
        status: idx < props.operation.attempts - 1 ? 'failed' : props.operation.status,
        timestamp: new Date(props.operation.startedAt.getTime() + idx * 1000),
        duration: Math.floor(Math.random() * 100),
        message: idx < props.operation.attempts - 1 ? 'Retry triggered' : null,
      }));
    });
    const formatDateTime = (date) => {
      if (!date) return 'N/A';
      const d = new Date(date);
      return d.toLocaleString();
    };
    const formatValue = (value) => {
      if (typeof value === 'object') {
        return JSON.stringify(value);
      }
      return String(value);
    };
    const copyToClipboard = async () => {
      try {
        const text = JSON.stringify(props.operation, null, 2);
        await navigator.clipboard.writeText(text);
        alert('Operation details copied to clipboard');
      } catch (error) {
        console.error('Failed to copy:', error);
      }
    };
    const exportAsJSON = () => {
      const data = JSON.stringify(props.operation, null, 2);
      const blob = new Blob([data], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `operation-${props.operation.id}.json`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    };

    return {
      activeTab,
      tabs,
      executionHistory,
      formatDateTime,
      formatValue,
      copyToClipboard,
      exportAsJSON,
    };
  },
};
</script>

<style scoped>
.operation-detail {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: linear-gradient(to br, rgba(15, 23, 42, 0.5), rgba(30, 41, 59, 0.5));
}
::-webkit-scrollbar {
  width: 8px;
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