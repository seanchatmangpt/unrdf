<script setup>
/**
 * @typedef {Object} Hook
 * @property {string} id - Unique hook identifier
 * @property {string} name - Hook name
 * @property {string} phase - Execution phase (pre-transaction, post-transaction, validation)
 * @property {Array} predicates - Array of predicates for the hook
 * @property {string} [description] - Optional hook description
 */

/**
 * Props for HooksList component
 * @type {{
 *   hooks: Hook[]
 * }}
 */
const props = defineProps({
  hooks: {
    type: Array,
    required: true,
    default: () => []
  }
})

/**
 * Events emitted by HooksList
 * - evaluate: Triggered when evaluate button is clicked
 * - edit: Triggered when edit button is clicked
 * - delete: Triggered when delete button is clicked
 */
const emit = defineEmits(['evaluate', 'edit', 'delete'])

/**
 * Handle evaluate action
 * @param {Hook} hook - Hook to evaluate
 */
const handleEvaluate = (hook) => {
  emit('evaluate', hook)
}

/**
 * Handle edit action
 * @param {Hook} hook - Hook to edit
 */
const handleEdit = (hook) => {
  emit('edit', hook)
}

/**
 * Handle delete action
 * @param {Hook} hook - Hook to delete
 */
const handleDelete = (hook) => {
  emit('delete', hook)
}

/**
 * Get count of predicates for a hook
 * @param {Hook} hook
 * @returns {number}
 */
const getPredicateCount = (hook) => {
  return Array.isArray(hook.predicates) ? hook.predicates.length : 0
}
</script>

<template>
  <div class="hooks-list">
    <div v-if="!hooks || hooks.length === 0" class="empty-state">
      <p>No hooks defined. Create your first hook to get started.</p>
    </div>

    <div v-else class="hooks-table-container">
      <table class="hooks-table">
        <thead>
          <tr>
            <th>ID</th>
            <th>Name</th>
            <th>Phase</th>
            <th>Predicates</th>
            <th>Description</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="hook in hooks" :key="hook.id" class="hook-row">
            <td class="hook-id">{{ hook.id }}</td>
            <td class="hook-name">{{ hook.name }}</td>
            <td class="hook-phase">
              <span :class="`phase-badge phase-${hook.phase}`">
                {{ hook.phase }}
              </span>
            </td>
            <td class="hook-predicates">{{ getPredicateCount(hook) }}</td>
            <td class="hook-description">{{ hook.description || '-' }}</td>
            <td class="hook-actions">
              <button
                @click="handleEvaluate(hook)"
                class="btn btn-evaluate"
                title="Evaluate hook"
              >
                Evaluate
              </button>
              <button
                @click="handleEdit(hook)"
                class="btn btn-edit"
                title="Edit hook"
              >
                Edit
              </button>
              <button
                @click="handleDelete(hook)"
                class="btn btn-delete"
                title="Delete hook"
              >
                Delete
              </button>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<style scoped>
.hooks-list {
  width: 100%;
  padding: 1rem;
}

.empty-state {
  text-align: center;
  padding: 2rem;
  color: #666;
}

.hooks-table-container {
  overflow-x: auto;
}

.hooks-table {
  width: 100%;
  border-collapse: collapse;
  background: white;
  border-radius: 8px;
  overflow: hidden;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

.hooks-table thead {
  background: #f8f9fa;
}

.hooks-table th {
  padding: 12px 16px;
  text-align: left;
  font-weight: 600;
  color: #333;
  border-bottom: 2px solid #e9ecef;
}

.hooks-table td {
  padding: 12px 16px;
  border-bottom: 1px solid #e9ecef;
}

.hook-row:hover {
  background: #f8f9fa;
}

.hook-id {
  font-family: monospace;
  color: #666;
  font-size: 0.9em;
}

.hook-name {
  font-weight: 500;
  color: #333;
}

.phase-badge {
  display: inline-block;
  padding: 4px 8px;
  border-radius: 4px;
  font-size: 0.85em;
  font-weight: 500;
}

.phase-pre-transaction {
  background: #e3f2fd;
  color: #1976d2;
}

.phase-post-transaction {
  background: #e8f5e9;
  color: #388e3c;
}

.phase-validation {
  background: #fff3e0;
  color: #f57c00;
}

.hook-actions {
  white-space: nowrap;
}

.btn {
  padding: 6px 12px;
  margin-right: 8px;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.9em;
  transition: all 0.2s;
}

.btn:last-child {
  margin-right: 0;
}

.btn-evaluate {
  background: #2196f3;
  color: white;
}

.btn-evaluate:hover {
  background: #1976d2;
}

.btn-edit {
  background: #4caf50;
  color: white;
}

.btn-edit:hover {
  background: #388e3c;
}

.btn-delete {
  background: #f44336;
  color: white;
}

.btn-delete:hover {
  background: #d32f2f;
}
</style>
