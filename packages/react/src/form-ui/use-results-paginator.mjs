/**
 * @file use-results-paginator.mjs
 * @description React hook for paginating query results
 */

import { useState, useCallback, useMemo } from 'react';

/**
 * Hook for paginating large result sets
 *
 * @param {Array} data - Data to paginate
 * @param {Object} config - Pagination configuration
 * @param {number} [config.pageSize=10] - Items per page
 * @returns {Object} Pagination state and operations
 *
 * @example
 * const {
 *   currentPage,
 *   pageData,
 *   nextPage,
 *   prevPage,
 *   goToPage,
 *   totalPages
 * } = useResultsPaginator(results, {
 *   pageSize: 10
 * });
 */
export function useResultsPaginator(data = [], config = {}) {
  const pageSize = config.pageSize || 10;
  const [currentPage, setCurrentPage] = useState(1);

  const totalPages = useMemo(() => {
    return Math.ceil(data.length / pageSize);
  }, [data.length, pageSize]);

  const pageData = useMemo(() => {
    const start = (currentPage - 1) * pageSize;
    const end = start + pageSize;
    return data.slice(start, end);
  }, [data, currentPage, pageSize]);

  const nextPage = useCallback(() => {
    setCurrentPage((prev) => Math.min(prev + 1, totalPages));
  }, [totalPages]);

  const prevPage = useCallback(() => {
    setCurrentPage((prev) => Math.max(prev - 1, 1));
  }, []);

  const goToPage = useCallback(
    (page) => {
      setCurrentPage(Math.max(1, Math.min(page, totalPages)));
    },
    [totalPages]
  );

  const reset = useCallback(() => {
    setCurrentPage(1);
  }, []);

  return {
    currentPage,
    pageData,
    nextPage,
    prevPage,
    goToPage,
    reset,
    totalPages,
    pageSize,
    hasNext: currentPage < totalPages,
    hasPrev: currentPage > 1,
  };
}
