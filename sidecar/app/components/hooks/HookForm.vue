<template>
  <div class="hook-form" :class="{ 'is-mobile': isMobile }">
    <!-- Form Header -->
    <div class="form-header">
      <h2 class="form-title">
        {{ isEditMode ? 'Edit Hook' : 'Create New Hook' }}
      </h2>
      <button
        v-if="isMobile"
        @click="$emit('close')"
        class="btn-icon"
        aria-label="Close"
      >
        ✕
      </button>
    </div>

    <!-- Error Banner -->
    <div v-if="formError" class="error-banner" role="alert">
      {{ formError }}
    </div>

    <!-- Success Banner -->
    <div v-if="successMessage" class="success-banner" role="status">
      {{ successMessage }}
    </div>

    <form @submit.prevent="handleSubmit" class="hook-form-content">
      <!-- Metadata Section -->
      <fieldset class="form-section">
        <legend class="section-title">Hook Metadata</legend>

        <!-- Name Field -->
        <div class="form-field">
          <label for="hook-name" class="field-label required">
            Hook Name
          </label>
          <input
            id="hook-name"
            v-model="formData.name"
            type="text"
            class="field-input"
            :class="{ 'has-error': errors.name }"
            placeholder="e.g., example:my-hook"
            maxlength="100"
            required
            @blur="validateField('name')"
          />
          <span v-if="errors.name" class="field-error">
            {{ errors.name }}
          </span>
          <span class="field-hint">
            Unique identifier for this hook (1-100 characters)
          </span>
        </div>

        <!-- Description Field -->
        <div class="form-field">
          <label for="hook-description" class="field-label">
            Description
          </label>
          <textarea
            id="hook-description"
            v-model="formData.description"
            class="field-input field-textarea"
            :class="{ 'has-error': errors.description }"
            placeholder="Describe what this hook does..."
            maxlength="500"
            rows="3"
            @blur="validateField('description')"
          />
          <span v-if="errors.description" class="field-error">
            {{ errors.description }}
          </span>
          <span class="field-hint">
            {{ formData.description?.length || 0 }}/500 characters
          </span>
        </div>

        <!-- Phase and Priority Row -->
        <div class="form-row">
          <!-- Phase Field -->
          <div class="form-field">
            <label for="hook-phase" class="field-label required">
              Execution Phase
            </label>
            <select
              id="hook-phase"
              v-model="formData.phase"
              class="field-input field-select"
              :class="{ 'has-error': errors.phase }"
              required
              @change="validateField('phase')"
            >
              <option value="">Select phase...</option>
              <option value="pre">Pre-execution</option>
              <option value="post">Post-execution</option>
              <option value="invariant">Invariant Check</option>
            </select>
            <span v-if="errors.phase" class="field-error">
              {{ errors.phase }}
            </span>
          </div>

          <!-- Priority Field -->
          <div class="form-field">
            <label for="hook-priority" class="field-label">
              Priority ({{ formData.priority }})
            </label>
            <input
              id="hook-priority"
              v-model.number="formData.priority"
              type="range"
              class="field-range"
              min="0"
              max="100"
              step="1"
            />
            <div class="range-labels">
              <span>High (0)</span>
              <span>Low (100)</span>
            </div>
          </div>
        </div>

        <!-- Disabled Toggle -->
        <div class="form-field">
          <label class="field-checkbox">
            <input
              v-model="formData.disabled"
              type="checkbox"
              class="checkbox-input"
            />
            <span class="checkbox-label">Disable this hook</span>
          </label>
          <span class="field-hint">
            Disabled hooks will not execute
          </span>
        </div>
      </fieldset>

      <!-- Predicates Section -->
      <fieldset class="form-section">
        <legend class="section-title">
          Predicates
          <button
            type="button"
            @click="addPredicate"
            class="btn-add"
            aria-label="Add predicate"
          >
            + Add Predicate
          </button>
        </legend>

        <div
          v-if="formData.predicates.length === 0"
          class="empty-state"
        >
          <p>No predicates defined. Add at least one to continue.</p>
        </div>

        <!-- Predicate List -->
        <div
          v-for="(predicate, index) in formData.predicates"
          :key="`predicate-${index}`"
          class="predicate-card"
        >
          <div class="predicate-header">
            <span class="predicate-number">Predicate {{ index + 1 }}</span>
            <button
              type="button"
              @click="removePredicate(index)"
              class="btn-remove"
              :aria-label="`Remove predicate ${index + 1}`"
            >
              Remove
            </button>
          </div>

          <!-- Predicate Type -->
          <div class="form-field">
            <label :for="`predicate-type-${index}`" class="field-label required">
              Type
            </label>
            <select
              :id="`predicate-type-${index}`"
              v-model="predicate.type"
              class="field-input field-select"
              required
              @change="onPredicateTypeChange(index)"
            >
              <option value="">Select type...</option>
              <option value="sparql">SPARQL Query</option>
              <option value="custom">Custom Function</option>
            </select>
          </div>

          <!-- SPARQL Query Field -->
          <div v-if="predicate.type === 'sparql'" class="form-field">
            <label :for="`predicate-query-${index}`" class="field-label required">
              SPARQL Query
            </label>
            <textarea
              :id="`predicate-query-${index}`"
              v-model="predicate.query"
              class="field-input field-textarea code-editor"
              placeholder="ASK { ?s ?p ?o }"
              rows="4"
              required
              spellcheck="false"
            />
            <button
              type="button"
              @click="formatSPARQL(index)"
              class="btn-format"
            >
              Format Query
            </button>
          </div>

          <!-- Custom Function Field -->
          <div v-if="predicate.type === 'custom'" class="form-field">
            <label :for="`predicate-name-${index}`" class="field-label required">
              Function Name
            </label>
            <input
              :id="`predicate-name-${index}`"
              v-model="predicate.name"
              type="text"
              class="field-input"
              placeholder="e.g., validateTransaction"
              required
            />
          </div>

          <!-- Error Display -->
          <div v-if="errors[`predicates.${index}`]" class="field-error">
            {{ errors[`predicates.${index}`] }}
          </div>
        </div>
      </fieldset>

      <!-- SPARQL SELECT Query (Optional) -->
      <fieldset class="form-section">
        <legend class="section-title">
          Data Retrieval (Optional)
        </legend>

        <div class="form-field">
          <label for="select-query" class="field-label">
            SPARQL SELECT Query
          </label>
          <textarea
            id="select-query"
            v-model="formData.selectQuery"
            class="field-input field-textarea code-editor"
            placeholder="SELECT ?s ?p ?o WHERE { ... }"
            rows="6"
            spellcheck="false"
            @input="updateSelectQueryHash"
          />
          <span class="field-hint">
            Optional query to retrieve data when predicates are satisfied
          </span>
        </div>

        <!-- SHA-256 Hash Display -->
        <div v-if="formData.selectQuery" class="hash-display">
          <span class="hash-label">SHA-256:</span>
          <code class="hash-value">{{ selectQueryHash }}</code>
        </div>
      </fieldset>

      <!-- Tags Section -->
      <fieldset class="form-section">
        <legend class="section-title">Tags</legend>

        <div class="form-field">
          <label for="tags-input" class="field-label">
            Add Tags
          </label>
          <div class="tags-input-wrapper">
            <input
              id="tags-input"
              v-model="tagInput"
              type="text"
              class="field-input"
              placeholder="Press Enter to add tag"
              @keydown.enter.prevent="addTag"
              @keydown.comma.prevent="addTag"
            />
          </div>
          <span class="field-hint">
            Press Enter or comma to add tags
          </span>
        </div>

        <!-- Tag List -->
        <div v-if="formData.tags.length > 0" class="tags-list">
          <span
            v-for="(tag, index) in formData.tags"
            :key="`tag-${index}`"
            class="tag"
          >
            {{ tag }}
            <button
              type="button"
              @click="removeTag(index)"
              class="tag-remove"
              :aria-label="`Remove tag ${tag}`"
            >
              ✕
            </button>
          </span>
        </div>
      </fieldset>

      <!-- Form Actions -->
      <div class="form-actions">
        <button
          type="button"
          @click="handleCancel"
          class="btn btn-outline"
          :disabled="isSubmitting"
        >
          Cancel
        </button>
        <button
          type="submit"
          class="btn btn-primary"
          :disabled="isSubmitting || !isFormValid"
        >
          {{ isSubmitting ? 'Saving...' : (isEditMode ? 'Update Hook' : 'Create Hook') }}
        </button>
      </div>
    </form>
  </div>
</template>

<script setup>
import { ref, computed, watch, onMounted } from 'vue'
import { CreateHookSchema } from '~/schemas/hooks.mjs'

const props = defineProps({
  hookId: {
    type: String,
    default: null
  },
  initialData: {
    type: Object,
    default: null
  },
  isEditMode: {
    type: Boolean,
    default: false
  }
})

const emit = defineEmits(['submit', 'cancel', 'close'])

// Reactive state
const formData = ref({
  id: props.hookId || '',
  name: '',
  description: '',
  phase: '',
  predicates: [],
  selectQuery: '',
  selectQuerySha256: '',
  disabled: false,
  priority: 50,
  tags: [],
  metadata: {}
})

const errors = ref({})
const formError = ref('')
const successMessage = ref('')
const isSubmitting = ref(false)
const tagInput = ref('')
const selectQueryHash = ref('')

// Responsive design detection
const isMobile = ref(false)

// Computed properties
const isFormValid = computed(() => {
  return (
    formData.value.name &&
    formData.value.phase &&
    formData.value.predicates.length > 0 &&
    Object.keys(errors.value).length === 0
  )
})

// Methods
function validateField(fieldName) {
  try {
    // Clear previous error
    delete errors.value[fieldName]

    // Validate specific field
    if (fieldName === 'name' && !formData.value.name) {
      errors.value.name = 'Name is required'
    } else if (fieldName === 'name' && formData.value.name.length > 100) {
      errors.value.name = 'Name must be 100 characters or less'
    }

    if (fieldName === 'description' && formData.value.description?.length > 500) {
      errors.value.description = 'Description must be 500 characters or less'
    }

    if (fieldName === 'phase' && !formData.value.phase) {
      errors.value.phase = 'Phase is required'
    }
  } catch (err) {
    console.error('Validation error:', err)
  }
}

function validateForm() {
  errors.value = {}
  formError.value = ''

  try {
    // Validate with Zod schema
    CreateHookSchema.parse(formData.value)
    return true
  } catch (err) {
    if (err.errors) {
      err.errors.forEach(error => {
        const path = error.path.join('.')
        errors.value[path] = error.message
      })
    }
    formError.value = 'Please fix the validation errors below'
    return false
  }
}

function addPredicate() {
  formData.value.predicates.push({
    type: '',
    query: '',
    name: '',
    params: {}
  })
}

function removePredicate(index) {
  formData.value.predicates.splice(index, 1)
}

function onPredicateTypeChange(index) {
  // Clear type-specific fields when type changes
  const predicate = formData.value.predicates[index]
  if (predicate.type === 'sparql') {
    delete predicate.name
    delete predicate.params
  } else if (predicate.type === 'custom') {
    delete predicate.query
  }
}

function formatSPARQL(index) {
  // Basic SPARQL formatting
  const predicate = formData.value.predicates[index]
  if (predicate.query) {
    predicate.query = predicate.query
      .replace(/\s+/g, ' ')
      .replace(/\{\s*/g, '{\n  ')
      .replace(/\s*\}/g, '\n}')
      .replace(/\.\s*/g, '.\n  ')
      .trim()
  }
}

async function updateSelectQueryHash() {
  if (formData.value.selectQuery) {
    try {
      const encoder = new TextEncoder()
      const data = encoder.encode(formData.value.selectQuery)
      const hashBuffer = await crypto.subtle.digest('SHA-256', data)
      const hashArray = Array.from(new Uint8Array(hashBuffer))
      const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('')
      selectQueryHash.value = hashHex
      formData.value.selectQuerySha256 = hashHex
    } catch (err) {
      console.error('Failed to calculate hash:', err)
    }
  } else {
    selectQueryHash.value = ''
    formData.value.selectQuerySha256 = ''
  }
}

function addTag() {
  const tag = tagInput.value.trim().replace(/,$/, '')
  if (tag && !formData.value.tags.includes(tag)) {
    formData.value.tags.push(tag)
    tagInput.value = ''
  }
}

function removeTag(index) {
  formData.value.tags.splice(index, 1)
}

async function handleSubmit() {
  if (!validateForm()) {
    return
  }

  isSubmitting.value = true
  formError.value = ''
  successMessage.value = ''

  try {
    await emit('submit', formData.value)
    successMessage.value = `Hook ${props.isEditMode ? 'updated' : 'created'} successfully!`

    // Reset form if creating new hook
    if (!props.isEditMode) {
      setTimeout(() => {
        resetForm()
      }, 1500)
    }
  } catch (err) {
    formError.value = err.message || 'Failed to save hook'
  } finally {
    isSubmitting.value = false
  }
}

function handleCancel() {
  if (confirm('Discard changes?')) {
    emit('cancel')
  }
}

function resetForm() {
  formData.value = {
    id: '',
    name: '',
    description: '',
    phase: '',
    predicates: [],
    selectQuery: '',
    selectQuerySha256: '',
    disabled: false,
    priority: 50,
    tags: [],
    metadata: {}
  }
  errors.value = {}
  formError.value = ''
  successMessage.value = ''
}

function checkMobile() {
  isMobile.value = window.innerWidth < 768
}

// Lifecycle
onMounted(() => {
  // Load initial data if provided
  if (props.initialData) {
    formData.value = { ...formData.value, ...props.initialData }
    if (formData.value.selectQuery) {
      updateSelectQueryHash()
    }
  }

  // Check mobile on mount and resize
  checkMobile()
  window.addEventListener('resize', checkMobile)
})

// Watch for initial data changes
watch(() => props.initialData, (newData) => {
  if (newData) {
    formData.value = { ...formData.value, ...newData }
    if (formData.value.selectQuery) {
      updateSelectQueryHash()
    }
  }
})
</script>

<style scoped>
.hook-form {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: var(--bg-primary, #1e1e1e);
  color: var(--text-primary, #fff);
}

.form-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 1.5rem;
  border-bottom: 1px solid var(--border-color, #333);
}

.form-title {
  margin: 0;
  font-size: 1.5rem;
  font-weight: 600;
}

.hook-form-content {
  flex: 1;
  overflow-y: auto;
  padding: 1.5rem;
}

/* Form Sections */
.form-section {
  border: 1px solid var(--border-color, #333);
  border-radius: 8px;
  padding: 1.5rem;
  margin-bottom: 1.5rem;
}

.form-section legend.section-title {
  font-size: 1.125rem;
  font-weight: 600;
  padding: 0 0.5rem;
  display: flex;
  justify-content: space-between;
  align-items: center;
  width: 100%;
}

/* Form Fields */
.form-field {
  margin-bottom: 1.5rem;
}

.form-field:last-child {
  margin-bottom: 0;
}

.field-label {
  display: block;
  font-weight: 500;
  margin-bottom: 0.5rem;
  font-size: 0.875rem;
}

.field-label.required::after {
  content: ' *';
  color: #f44336;
}

.field-input {
  width: 100%;
  padding: 0.75rem;
  background: var(--bg-secondary, #2a2a2a);
  border: 1px solid var(--border-color, #444);
  border-radius: 4px;
  color: var(--text-primary, #fff);
  font-size: 0.875rem;
  transition: border-color 0.2s, box-shadow 0.2s;
  min-height: 44px; /* Mobile-friendly tap target */
}

.field-input:focus {
  outline: none;
  border-color: var(--primary-color, #007acc);
  box-shadow: 0 0 0 3px rgba(0, 122, 204, 0.1);
}

.field-input.has-error {
  border-color: #f44336;
}

.field-textarea {
  resize: vertical;
  font-family: inherit;
}

.code-editor {
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 0.8125rem;
}

.field-select {
  cursor: pointer;
}

.field-range {
  width: 100%;
  margin-top: 0.5rem;
}

.range-labels {
  display: flex;
  justify-content: space-between;
  font-size: 0.75rem;
  color: var(--text-secondary, #888);
  margin-top: 0.25rem;
}

.field-checkbox {
  display: flex;
  align-items: center;
  cursor: pointer;
}

.checkbox-input {
  width: 20px;
  height: 20px;
  margin-right: 0.5rem;
  cursor: pointer;
}

.checkbox-label {
  font-weight: 400;
}

.field-error {
  display: block;
  color: #f44336;
  font-size: 0.75rem;
  margin-top: 0.25rem;
}

.field-hint {
  display: block;
  color: var(--text-secondary, #888);
  font-size: 0.75rem;
  margin-top: 0.25rem;
}

/* Form Row (for side-by-side fields) */
.form-row {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem;
}

@media (max-width: 767px) {
  .form-row {
    grid-template-columns: 1fr;
  }
}

/* Banners */
.error-banner,
.success-banner {
  padding: 0.75rem 1.5rem;
  margin: 0 1.5rem 1rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.error-banner {
  background: rgba(244, 67, 54, 0.1);
  color: #f44336;
  border: 1px solid #f44336;
}

.success-banner {
  background: rgba(76, 175, 80, 0.1);
  color: #4caf50;
  border: 1px solid #4caf50;
}

/* Predicate Cards */
.predicate-card {
  background: var(--bg-secondary, #2a2a2a);
  border: 1px solid var(--border-color, #444);
  border-radius: 6px;
  padding: 1rem;
  margin-bottom: 1rem;
}

.predicate-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid var(--border-color, #444);
}

.predicate-number {
  font-weight: 600;
  font-size: 0.875rem;
}

/* Tags */
.tags-list {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-top: 0.5rem;
}

.tag {
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.25rem 0.75rem;
  background: var(--primary-color, #007acc);
  color: white;
  border-radius: 16px;
  font-size: 0.75rem;
}

.tag-remove {
  background: none;
  border: none;
  color: white;
  cursor: pointer;
  padding: 0;
  font-size: 1rem;
  line-height: 1;
  opacity: 0.8;
  transition: opacity 0.2s;
}

.tag-remove:hover {
  opacity: 1;
}

/* Hash Display */
.hash-display {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.75rem;
  background: var(--bg-secondary, #2a2a2a);
  border-radius: 4px;
  margin-top: 0.5rem;
  font-size: 0.75rem;
}

.hash-label {
  font-weight: 600;
  color: var(--text-secondary, #888);
}

.hash-value {
  flex: 1;
  font-family: 'Monaco', 'Menlo', monospace;
  color: var(--primary-color, #007acc);
  word-break: break-all;
}

/* Buttons */
.btn,
.btn-add,
.btn-remove,
.btn-format,
.btn-icon {
  padding: 0.5rem 1rem;
  border-radius: 4px;
  font-size: 0.875rem;
  font-weight: 500;
  cursor: pointer;
  border: 1px solid;
  transition: all 0.2s;
  min-height: 44px; /* Mobile-friendly */
  display: inline-flex;
  align-items: center;
  justify-content: center;
}

.btn-primary {
  background: var(--primary-color, #007acc);
  color: white;
  border-color: var(--primary-color, #007acc);
}

.btn-primary:hover:not(:disabled) {
  background: #005a9e;
  border-color: #005a9e;
}

.btn-outline {
  background: transparent;
  color: var(--text-primary, #fff);
  border-color: var(--border-color, #555);
}

.btn-outline:hover:not(:disabled) {
  background: var(--bg-secondary, #333);
}

.btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.btn-add {
  background: var(--primary-color, #007acc);
  color: white;
  border-color: var(--primary-color, #007acc);
  padding: 0.375rem 0.75rem;
  font-size: 0.8125rem;
}

.btn-remove {
  background: transparent;
  color: #f44336;
  border-color: #f44336;
  padding: 0.375rem 0.75rem;
  font-size: 0.8125rem;
}

.btn-format {
  background: var(--bg-secondary, #333);
  color: var(--text-primary, #fff);
  border-color: var(--border-color, #555);
  padding: 0.5rem 0.75rem;
  font-size: 0.8125rem;
  margin-top: 0.5rem;
}

.btn-icon {
  background: transparent;
  border: none;
  color: var(--text-primary, #fff);
  padding: 0.5rem;
  font-size: 1.5rem;
  min-height: auto;
  width: 44px;
  height: 44px;
}

/* Form Actions */
.form-actions {
  display: flex;
  gap: 1rem;
  justify-content: flex-end;
  padding: 1.5rem;
  border-top: 1px solid var(--border-color, #333);
}

.empty-state {
  text-align: center;
  padding: 2rem;
  color: var(--text-secondary, #888);
}

/* Mobile Optimizations */
@media (max-width: 767px) {
  .hook-form.is-mobile .form-header {
    position: sticky;
    top: 0;
    background: var(--bg-primary, #1e1e1e);
    z-index: 10;
  }

  .hook-form.is-mobile .form-actions {
    position: sticky;
    bottom: 0;
    background: var(--bg-primary, #1e1e1e);
    z-index: 10;
    flex-direction: column-reverse;
  }

  .hook-form.is-mobile .form-actions .btn {
    width: 100%;
  }

  .hook-form.is-mobile .predicate-card {
    padding: 0.75rem;
  }

  .form-title {
    font-size: 1.25rem;
  }

  .hook-form-content {
    padding: 1rem;
  }

  .form-section {
    padding: 1rem;
  }
}

/* Dark Mode Support (CSS Variables) */
@media (prefers-color-scheme: dark) {
  .hook-form {
    --bg-primary: #1e1e1e;
    --bg-secondary: #2a2a2a;
    --text-primary: #ffffff;
    --text-secondary: #888888;
    --border-color: #444444;
    --primary-color: #007acc;
  }
}

@media (prefers-color-scheme: light) {
  .hook-form {
    --bg-primary: #ffffff;
    --bg-secondary: #f5f5f5;
    --text-primary: #000000;
    --text-secondary: #666666;
    --border-color: #dddddd;
    --primary-color: #007acc;
  }
}
</style>
