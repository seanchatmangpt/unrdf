<template>
  <div class="hook-list">
    <div class="list-header">
      <h3>Knowledge Hooks</h3>
      <button @click="handleCreate" class="btn-primary">
        + Create Hook
      </button>
    </div>

    <div v-if="isLoading" class="loading">
      Loading hooks...
    </div>

    <div v-else-if="error" class="error-message">
      {{ error }}
    </div>

    <div v-else-if="hooks.length === 0" class="empty-state">
      <p>No hooks registered yet.</p>
      <button @click="handleCreate" class="btn-secondary">
        Create your first hook
      </button>
    </div>

    <div v-else class="hooks-grid">
      <div
        v-for="hook in hooks"
        :key="hook.id"
        class="hook-card"
      >
        <div class="hook-header">
          <h4>{{ hook.name }}</h4>
          <span class="hook-phase">{{ hook.phase }}</span>
        </div>

        <p class="hook-description">
          {{ hook.description || 'No description' }}
        </p>

        <div class="hook-meta">
          <span class="meta-item">
            <strong>Predicates:</strong> {{ hook.predicateCount }}
          </span>
          <span v-if="hook.ontology?.length" class="meta-item">
            <strong>Ontology:</strong> {{ hook.ontology.join(', ') }}
          </span>
        </div>

        <div class="hook-actions">
          <button @click="handleEdit(hook)" class="btn-sm btn-secondary">
            Edit
          </button>
          <button @click="handleExecute(hook)" class="btn-sm btn-success">
            Execute
          </button>
          <button @click="handleDelete(hook)" class="btn-sm btn-danger">
            Delete
          </button>
        </div>
      </div>
    </div>

    <!-- Delete confirmation modal -->
    <div v-if="showDeleteModal" class="modal-overlay" @click="cancelDelete">
      <div class="modal-content" @click.stop>
        <h3>Confirm Delete</h3>
        <p>Are you sure you want to delete hook "{{ hookToDelete?.name }}"?</p>
        <div class="modal-actions">
          <button @click="confirmDelete" class="btn-danger">
            Delete
          </button>
          <button @click="cancelDelete" class="btn-outline">
            Cancel
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'

interface Hook {
  id: string
  name: string
  description: string
  phase: string
  predicateCount: number
  ontology: string[]
  channel: any
  createdAt: string
}

interface Emits {
  (e: 'create'): void
  (e: 'edit', hook: Hook): void
  (e: 'execute', hook: Hook): void
  (e: 'delete', hookId: string): void
}

const emit = defineEmits<Emits>()

const hooks = ref<Hook[]>([])
const isLoading = ref(false)
const error = ref('')
const showDeleteModal = ref(false)
const hookToDelete = ref<Hook | null>(null)

async function loadHooks() {
  isLoading.value = true
  error.value = ''

  try {
    const response = await fetch('/api/hooks/list')
    if (!response.ok) {
      throw new Error(`Failed to load hooks: ${response.statusText}`)
    }

    const data = await response.json()
    hooks.value = data.data?.hooks || []
  } catch (err: any) {
    error.value = err.message
    console.error('Failed to load hooks:', err)
  } finally {
    isLoading.value = false
  }
}

function handleCreate() {
  emit('create')
}

function handleEdit(hook: Hook) {
  emit('edit', hook)
}

function handleExecute(hook: Hook) {
  emit('execute', hook)
}

function handleDelete(hook: Hook) {
  hookToDelete.value = hook
  showDeleteModal.value = true
}

async function confirmDelete() {
  if (!hookToDelete.value) return

  try {
    const response = await fetch(`/api/hooks/${hookToDelete.value.id}`, {
      method: 'DELETE'
    })

    if (!response.ok) {
      throw new Error(`Failed to delete hook: ${response.statusText}`)
    }

    emit('delete', hookToDelete.value.id)
    await loadHooks() // Reload the list
    showDeleteModal.value = false
    hookToDelete.value = null
  } catch (err: any) {
    error.value = err.message
    console.error('Failed to delete hook:', err)
  }
}

function cancelDelete() {
  showDeleteModal.value = false
  hookToDelete.value = null
}

onMounted(() => {
  loadHooks()
})

// Expose refresh method for parent component
defineExpose({
  loadHooks
})
</script>

<style scoped>
.hook-list {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
  padding: 1rem;
}

.list-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.list-header h3 {
  margin: 0;
  font-size: 1.5rem;
}

.loading, .error-message, .empty-state {
  padding: 2rem;
  text-align: center;
  background: #1e1e1e;
  border-radius: 8px;
}

.error-message {
  color: #f44336;
  background: #3a1e1e;
  border: 1px solid #f44336;
}

.empty-state {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  align-items: center;
}

.hooks-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
  gap: 1.5rem;
}

.hook-card {
  background: #1e1e1e;
  border: 1px solid #333;
  border-radius: 8px;
  padding: 1.5rem;
  display: flex;
  flex-direction: column;
  gap: 1rem;
  transition: border-color 0.2s;
}

.hook-card:hover {
  border-color: #007acc;
}

.hook-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.hook-header h4 {
  margin: 0;
  font-size: 1.125rem;
  color: #fff;
}

.hook-phase {
  padding: 0.25rem 0.5rem;
  background: #007acc;
  color: white;
  border-radius: 4px;
  font-size: 0.75rem;
  text-transform: uppercase;
}

.hook-description {
  margin: 0;
  color: #aaa;
  font-size: 0.875rem;
  line-height: 1.5;
}

.hook-meta {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  font-size: 0.875rem;
  color: #888;
}

.meta-item strong {
  color: #fff;
}

.hook-actions {
  display: flex;
  gap: 0.5rem;
  margin-top: auto;
}

.btn-primary, .btn-secondary, .btn-outline, .btn-sm, .btn-success, .btn-danger {
  padding: 0.5rem 1rem;
  border-radius: 4px;
  font-size: 0.875rem;
  cursor: pointer;
  border: 1px solid;
  transition: all 0.2s;
}

.btn-sm {
  padding: 0.375rem 0.75rem;
  font-size: 0.8125rem;
}

.btn-primary {
  background: #007acc;
  color: white;
  border-color: #007acc;
}

.btn-primary:hover {
  background: #005a9e;
}

.btn-secondary {
  background: #333;
  color: white;
  border-color: #555;
}

.btn-secondary:hover {
  background: #444;
}

.btn-success {
  background: #4caf50;
  color: white;
  border-color: #4caf50;
}

.btn-success:hover {
  background: #45a049;
}

.btn-danger {
  background: #f44336;
  color: white;
  border-color: #f44336;
}

.btn-danger:hover {
  background: #da190b;
}

.btn-outline {
  background: transparent;
  color: #fff;
  border-color: #555;
}

.btn-outline:hover {
  background: #333;
}

.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.modal-content {
  background: #1e1e1e;
  border: 1px solid #333;
  border-radius: 8px;
  padding: 2rem;
  max-width: 500px;
  width: 90%;
}

.modal-content h3 {
  margin: 0 0 1rem;
  color: #fff;
}

.modal-content p {
  margin: 0 0 1.5rem;
  color: #aaa;
}

.modal-actions {
  display: flex;
  gap: 0.5rem;
  justify-content: flex-end;
}
</style>
