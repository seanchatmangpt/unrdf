/**
 * @fileoverview useDebug - Hook for debugging utilities
 * @module react-hooks/utils/useDebug
 */

import { useEffect, useRef } from 'react';

/**
 *
 */
export function useDebug(componentName, props) {
  const renderCount = useRef(0);
  const prevProps = useRef(props);

  useEffect(() => {
    renderCount.current += 1;
    console.log(`[${componentName}] Render #${renderCount.current}`);

    const changed = Object.keys(props).filter(
      key => props[key] !== prevProps.current[key]
    );

    if (changed.length > 0) {
      console.log(`[${componentName}] Changed props:`, changed);
    }

    prevProps.current = props;
  });

  return { renderCount: renderCount.current };
}
