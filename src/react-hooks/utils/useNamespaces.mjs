/**
 * @fileoverview useNamespaces - Hook for namespace/prefix management
 * @module react-hooks/utils/useNamespaces
 */

import { useState, useCallback } from 'react';

/**
 *
 */
export function useNamespaces(initialPrefixes = {}) {
  const [prefixes, setPrefixes] = useState(initialPrefixes);

  const add = useCallback((prefix, namespace) => {
    setPrefixes(prev => ({ ...prev, [prefix]: namespace }));
  }, []);

  const remove = useCallback((prefix) => {
    setPrefixes(prev => {
      const updated = { ...prev };
      delete updated[prefix];
      return updated;
    });
  }, []);

  const expand = useCallback((prefixedName) => {
    const [prefix, localName] = prefixedName.split(':');
    const namespace = prefixes[prefix];
    if (!namespace) throw new Error(`Unknown prefix: ${prefix}`);
    return namespace + localName;
  }, [prefixes]);

  return { prefixes, add, remove, expand };
}
