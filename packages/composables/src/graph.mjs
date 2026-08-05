import { computed, unref } from 'vue';
import { useStoreContext } from './context/index.mjs';

/** Return the active RDF store context. */
export function useGraph() {
  return useStoreContext();
}

/** Create a reactive projection from the active graph context. */
export function useGraphProjection(project) {
  if (typeof project !== 'function') throw new TypeError('project must be a function');
  const graph = useGraph();
  return computed(() => project(unref(graph)));
}
