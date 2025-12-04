/**
 * @fileoverview useQuadStore - Hook for quad store operations
 * @module react-hooks/storage/useQuadStore
 */

import { useState, useCallback, useMemo } from 'react';
import { Store } from 'n3';

/**
 *
 */
export function useQuadStore(initialQuads = []) {
  const [store] = useState(() => {
    const s = new Store();
    if (initialQuads.length > 0) s.addQuads(initialQuads);
    return s;
  });
  const [version, setVersion] = useState(0);

  const add = useCallback(
    (quads) => {
      store.addQuads(Array.isArray(quads) ? quads : [quads]);
      setVersion((v) => v + 1);
    },
    [store]
  );

  const remove = useCallback(
    (quads) => {
      store.removeQuads(Array.isArray(quads) ? quads : [quads]);
      setVersion((v) => v + 1);
    },
    [store]
  );

  const match = useCallback(
    (s, p, o, g) => {
      return store.getQuads(s, p, o, g);
    },
    [store, version]
  );

  const size = useMemo(() => store.size, [store, version]);

  return { store, add, remove, match, size };
}
