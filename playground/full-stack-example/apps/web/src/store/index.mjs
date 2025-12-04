import { reactive } from 'vue'

/**
 * Pinia-like store for web application state
 */
export const store = reactive({
  quads: [],
  loading: false,
  error: null
})
