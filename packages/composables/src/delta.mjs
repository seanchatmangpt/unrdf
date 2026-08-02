import { ref, readonly } from 'vue';

/** Maintain an append-only reactive stream of admitted graph deltas. */
export function useDeltaStream(initial = []) {
  if (!Array.isArray(initial)) throw new TypeError('initial must be an array');
  const deltas = ref([...initial]);
  const append = delta => {
    if (delta == null || typeof delta !== 'object') throw new TypeError('delta must be an object');
    deltas.value.push(delta);
    return delta;
  };
  const clear = () => {
    deltas.value = [];
  };
  return { deltas: readonly(deltas), append, clear };
}
