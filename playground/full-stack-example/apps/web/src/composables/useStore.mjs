import { ref } from 'vue'

const store = ref(null)

/**
 * Vue 3 composable for store management
 * @returns {Object} Store management functions
 */
export function useStore() {
  const getGraph = () => store.value
  const setGraph = (g) => { store.value = g }
  const getQuads = () => store.value?.quads || []

  return { getGraph, setGraph, getQuads }
}
