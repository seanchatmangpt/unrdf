import { ref } from 'vue'

/**
 * Vue 3 composable for store management with reactive quad storage
 * @returns {Object} Store management functions
 */
export function useStore() {
  const quads = ref([])

  const addQuad = async (quad) => {
    if (!quad.subject || !quad.predicate || !quad.object) {
      throw new Error('Invalid quad: missing required fields')
    }
    quads.value.push(quad)
  }

  const removeQuad = async (quad) => {
    const index = quads.value.findIndex(q =>
      q.subject === quad.subject &&
      q.predicate === quad.predicate &&
      q.object === quad.object
    )
    if (index !== -1) {
      quads.value.splice(index, 1)
    }
  }

  const query = async (sparql) => {
    return quads.value.filter(() => true)
  }

  const clear = async () => {
    quads.value = []
  }

  return {
    quads,
    addQuad,
    removeQuad,
    query,
    clear
  }
}
