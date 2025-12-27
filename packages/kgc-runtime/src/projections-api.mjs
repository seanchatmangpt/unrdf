/**
 * @fileoverview Π_api - REST/GraphQL API Projections
 * Transforms KGC structures into API-compatible formats with pagination and filtering
 */

import { z } from 'zod';

/**
 * API projection schema
 */
export const APIProjectionSchema = z.object({
  type: z.literal('api'),
  format: z.enum(['rest', 'graphql', 'json-api']),
  data: z.any(),
  meta: z.object({
    pagination: z.object({
      page: z.number(),
      pageSize: z.number(),
      total: z.number(),
      totalPages: z.number(),
    }).optional(),
    filters: z.record(z.any()).optional(),
    sort: z.array(z.object({
      field: z.string(),
      direction: z.enum(['asc', 'desc']),
    })).optional(),
  }).optional(),
  links: z.object({
    self: z.string().optional(),
    first: z.string().optional(),
    prev: z.string().optional(),
    next: z.string().optional(),
    last: z.string().optional(),
  }).optional(),
});

/**
 * @typedef {z.infer<typeof APIProjectionSchema>} APIProjection
 */

/**
 * Pagination options
 * @typedef {{page: number, pageSize: number}} PaginationOptions
 */

/**
 * Filter options
 * @typedef {Record<string, any>} FilterOptions
 */

/**
 * Sort options
 * @typedef {Array<{field: string, direction: 'asc' | 'desc'}>} SortOptions
 */

/**
 * Project receipts to paginated REST API format
 * @param {Array<import('./receipt.mjs').Receipt>} receipts - Receipts to project
 * @param {PaginationOptions} pagination - Pagination options
 * @param {FilterOptions} [filters] - Filter options
 * @param {SortOptions} [sort] - Sort options
 * @returns {APIProjection} API projection
 */
export function projectReceiptsToREST(receipts, pagination, filters = {}, sort = []) {
  // Apply filters
  let filtered = receipts;
  for (const [key, value] of Object.entries(filters)) {
    filtered = filtered.filter(r => r[key] === value);
  }

  // Apply sorting
  if (sort.length > 0) {
    filtered = filtered.sort((a, b) => {
      for (const { field, direction } of sort) {
        const aVal = a[field];
        const bVal = b[field];
        const comparison = aVal < bVal ? -1 : aVal > bVal ? 1 : 0;
        if (comparison !== 0) {
          return direction === 'asc' ? comparison : -comparison;
        }
      }
      return 0;
    });
  }

  // Apply pagination
  const total = filtered.length;
  const totalPages = Math.ceil(total / pagination.pageSize);
  const start = (pagination.page - 1) * pagination.pageSize;
  const end = start + pagination.pageSize;
  const paginated = filtered.slice(start, end);

  // Serialize receipts
  const data = paginated.map(r => ({
    id: r.id,
    type: 'receipt',
    attributes: {
      timestamp: r.timestamp,
      operation: r.operation,
      hash: r.hash,
      parentHash: r.parentHash,
    },
    relationships: r.parentHash ? {
      parent: {
        data: { type: 'receipt', id: r.parentHash },
      },
    } : undefined,
  }));

  return APIProjectionSchema.parse({
    type: 'api',
    format: 'rest',
    data,
    meta: {
      pagination: {
        page: pagination.page,
        pageSize: pagination.pageSize,
        total,
        totalPages,
      },
      filters,
      sort,
    },
    links: {
      self: `/receipts?page=${pagination.page}`,
      first: '/receipts?page=1',
      prev: pagination.page > 1 ? `/receipts?page=${pagination.page - 1}` : undefined,
      next: pagination.page < totalPages ? `/receipts?page=${pagination.page + 1}` : undefined,
      last: `/receipts?page=${totalPages}`,
    },
  });
}

/**
 * Project work items to GraphQL format
 * @param {Array<object>} workItems - Work items
 * @param {object} [args] - GraphQL query arguments
 * @returns {APIProjection} API projection
 */
export function projectWorkItemsToGraphQL(workItems, args = {}) {
  // Apply GraphQL-style filtering
  let filtered = workItems;

  if (args.where) {
    filtered = filtered.filter(item => {
      for (const [key, value] of Object.entries(args.where)) {
        if (item[key] !== value) return false;
      }
      return true;
    });
  }

  // Apply GraphQL-style ordering
  if (args.orderBy) {
    filtered = filtered.sort((a, b) => {
      const field = args.orderBy.field;
      const direction = args.orderBy.direction || 'ASC';
      const comparison = a[field] < b[field] ? -1 : a[field] > b[field] ? 1 : 0;
      return direction === 'ASC' ? comparison : -comparison;
    });
  }

  // Apply GraphQL-style pagination (skip/take)
  if (args.skip !== undefined) {
    filtered = filtered.slice(args.skip);
  }
  if (args.take !== undefined) {
    filtered = filtered.slice(0, args.take);
  }

  const data = {
    workItems: filtered.map(item => ({
      id: item.id,
      goal: item.goal,
      state: item.state,
      priority: item.priority,
      createdAt: item.createdAt,
      updatedAt: item.updatedAt,
    })),
    totalCount: workItems.length,
    filteredCount: filtered.length,
  };

  return APIProjectionSchema.parse({
    type: 'api',
    format: 'graphql',
    data,
    meta: {
      filters: args.where,
      sort: args.orderBy ? [args.orderBy] : undefined,
    },
  });
}

/**
 * Project single resource to JSON:API format
 * @param {object} resource - Resource object
 * @param {string} resourceType - Resource type
 * @param {Record<string, any>} [relationships] - Resource relationships
 * @returns {APIProjection} API projection
 */
export function projectResourceToJSONAPI(resource, resourceType, relationships = {}) {
  const { id, ...attributes } = resource;

  const relationshipData = {};
  for (const [key, value] of Object.entries(relationships)) {
    if (Array.isArray(value)) {
      relationshipData[key] = {
        data: value.map(v => ({ type: v.type, id: v.id })),
      };
    } else if (value) {
      relationshipData[key] = {
        data: { type: value.type, id: value.id },
      };
    }
  }

  return APIProjectionSchema.parse({
    type: 'api',
    format: 'json-api',
    data: {
      type: resourceType,
      id,
      attributes,
      relationships: Object.keys(relationshipData).length > 0 ? relationshipData : undefined,
    },
    links: {
      self: `/${resourceType}/${id}`,
    },
  });
}

/**
 * Apply filtering to array of objects
 * @param {Array<Record<string, any>>} items - Items to filter
 * @param {FilterOptions} filters - Filter criteria
 * @returns {Array<Record<string, any>>} Filtered items
 */
export function applyFilters(items, filters) {
  if (!filters || Object.keys(filters).length === 0) {
    return items;
  }

  return items.filter(item => {
    for (const [key, value] of Object.entries(filters)) {
      // Support operators: eq, ne, gt, lt, contains, in
      if (typeof value === 'object' && value !== null) {
        const itemValue = item[key];

        if ('eq' in value && itemValue !== value.eq) return false;
        if ('ne' in value && itemValue === value.ne) return false;
        if ('gt' in value && itemValue <= value.gt) return false;
        if ('lt' in value && itemValue >= value.lt) return false;
        if ('contains' in value && !String(itemValue).includes(value.contains)) return false;
        if ('in' in value && !value.in.includes(itemValue)) return false;
      } else {
        if (item[key] !== value) return false;
      }
    }
    return true;
  });
}

/**
 * Apply sorting to array of objects
 * @param {Array<Record<string, any>>} items - Items to sort
 * @param {SortOptions} sort - Sort criteria
 * @returns {Array<Record<string, any>>} Sorted items
 */
export function applySorting(items, sort) {
  if (!sort || sort.length === 0) {
    return items;
  }

  return [...items].sort((a, b) => {
    for (const { field, direction } of sort) {
      const aVal = a[field];
      const bVal = b[field];

      let comparison = 0;
      if (aVal < bVal) comparison = -1;
      else if (aVal > bVal) comparison = 1;

      if (comparison !== 0) {
        return direction === 'asc' ? comparison : -comparison;
      }
    }
    return 0;
  });
}

/**
 * Apply pagination to array
 * @param {Array<any>} items - Items to paginate
 * @param {PaginationOptions} pagination - Pagination options
 * @returns {{data: Array<any>, meta: object}} Paginated result
 */
export function applyPagination(items, pagination) {
  const total = items.length;
  const totalPages = Math.ceil(total / pagination.pageSize);
  const start = (pagination.page - 1) * pagination.pageSize;
  const end = start + pagination.pageSize;
  const data = items.slice(start, end);

  return {
    data,
    meta: {
      pagination: {
        page: pagination.page,
        pageSize: pagination.pageSize,
        total,
        totalPages,
      },
    },
  };
}

/**
 * Build HATEOAS links for resource
 * @param {string} resourceType - Resource type
 * @param {string} resourceId - Resource ID
 * @param {Array<{rel: string, href: string}>} [additional=[]] - Additional links
 * @returns {Record<string, string>} HATEOAS links
 */
export function buildHATEOASLinks(resourceType, resourceId, additional = []) {
  const links = {
    self: `/${resourceType}/${resourceId}`,
    collection: `/${resourceType}`,
  };

  for (const link of additional) {
    links[link.rel] = link.href;
  }

  return links;
}
