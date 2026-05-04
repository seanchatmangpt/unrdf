/**
 * Tests for API Projections (Π_api)
 */

import { describe, it, expect } from 'vitest';
import {
  projectReceiptsToREST,
  projectWorkItemsToGraphQL,
  projectResourceToJSONAPI,
  applyFilters,
  applySorting,
  applyPagination,
  buildHATEOASLinks,
  APIProjectionSchema,
} from '../src/projections-api.mjs';

describe('Π_api - API Projections', () => {
  describe('projectReceiptsToREST', () => {
    it('should project receipts to paginated REST format', () => {
      const receipts = [
        { id: '1', timestamp: '2024-01-01', operation: 'op1', hash: 'a'.repeat(64) },
        { id: '2', timestamp: '2024-01-02', operation: 'op2', hash: 'b'.repeat(64) },
        { id: '3', timestamp: '2024-01-03', operation: 'op3', hash: 'c'.repeat(64) },
      ];

      const projection = projectReceiptsToREST(
        receipts,
        { page: 1, pageSize: 2 }
      );

      expect(projection.type).toBe('api');
      expect(projection.format).toBe('rest');
      expect(projection.data).toHaveLength(2);
      expect(projection.meta.pagination.total).toBe(3);
      expect(projection.meta.pagination.totalPages).toBe(2);
      expect(projection.links.next).toContain('page=2');

      // Validate schema
      APIProjectionSchema.parse(projection);
    });

    it('should apply filters to receipts', () => {
      const receipts = [
        { id: '1', operation: 'create', hash: 'a'.repeat(64) },
        { id: '2', operation: 'update', hash: 'b'.repeat(64) },
        { id: '3', operation: 'create', hash: 'c'.repeat(64) },
      ];

      const projection = projectReceiptsToREST(
        receipts,
        { page: 1, pageSize: 10 },
        { operation: 'create' }
      );

      expect(projection.data).toHaveLength(2);
      expect(projection.meta.filters.operation).toBe('create');
    });

    it('should apply sorting to receipts', () => {
      const receipts = [
        { id: '3', timestamp: '2024-01-03', hash: 'c'.repeat(64) },
        { id: '1', timestamp: '2024-01-01', hash: 'a'.repeat(64) },
        { id: '2', timestamp: '2024-01-02', hash: 'b'.repeat(64) },
      ];

      const projection = projectReceiptsToREST(
        receipts,
        { page: 1, pageSize: 10 },
        {},
        [{ field: 'timestamp', direction: 'asc' }]
      );

      expect(projection.data[0].id).toBe('1');
      expect(projection.data[1].id).toBe('2');
      expect(projection.data[2].id).toBe('3');
    });
  });

  describe('projectWorkItemsToGraphQL', () => {
    it('should project work items to GraphQL format', () => {
      const workItems = [
        { id: '1', goal: 'Task 1', state: 'pending', priority: 1 },
        { id: '2', goal: 'Task 2', state: 'running', priority: 2 },
      ];

      const projection = projectWorkItemsToGraphQL(workItems);

      expect(projection.type).toBe('api');
      expect(projection.format).toBe('graphql');
      expect(projection.data.workItems).toHaveLength(2);
      expect(projection.data.totalCount).toBe(2);
    });

    it('should apply GraphQL where filters', () => {
      const workItems = [
        { id: '1', goal: 'Task 1', state: 'pending' },
        { id: '2', goal: 'Task 2', state: 'completed' },
        { id: '3', goal: 'Task 3', state: 'pending' },
      ];

      const projection = projectWorkItemsToGraphQL(workItems, {
        where: { state: 'pending' },
      });

      expect(projection.data.workItems).toHaveLength(2);
      expect(projection.data.filteredCount).toBe(2);
    });

    it('should apply GraphQL ordering', () => {
      const workItems = [
        { id: '3', goal: 'Task 3', priority: 3 },
        { id: '1', goal: 'Task 1', priority: 1 },
        { id: '2', goal: 'Task 2', priority: 2 },
      ];

      const projection = projectWorkItemsToGraphQL(workItems, {
        orderBy: { field: 'priority', direction: 'ASC' },
      });

      expect(projection.data.workItems[0].id).toBe('1');
      expect(projection.data.workItems[2].id).toBe('3');
    });

    it('should apply skip and take pagination', () => {
      const workItems = Array.from({ length: 10 }, (_, i) => ({
        id: String(i + 1),
        goal: `Task ${i + 1}`,
        state: 'pending',
      }));

      const projection = projectWorkItemsToGraphQL(workItems, {
        skip: 2,
        take: 3,
      });

      expect(projection.data.workItems).toHaveLength(3);
      expect(projection.data.workItems[0].id).toBe('3');
    });
  });

  describe('projectResourceToJSONAPI', () => {
    it('should project resource to JSON:API format', () => {
      const resource = {
        id: 'user-123',
        name: 'Alice',
        email: 'alice@example.com',
      };

      const projection = projectResourceToJSONAPI(resource, 'user');

      expect(projection.type).toBe('api');
      expect(projection.format).toBe('json-api');
      expect(projection.data.type).toBe('user');
      expect(projection.data.id).toBe('user-123');
      expect(projection.data.attributes.name).toBe('Alice');
      expect(projection.links.self).toBe('/user/user-123');
    });

    it('should include relationships', () => {
      const resource = { id: '1', title: 'Post' };
      const relationships = {
        author: { type: 'user', id: 'user-123' },
        comments: [
          { type: 'comment', id: 'c1' },
          { type: 'comment', id: 'c2' },
        ],
      };

      const projection = projectResourceToJSONAPI(resource, 'post', relationships);

      expect(projection.data.relationships.author.data).toEqual({
        type: 'user',
        id: 'user-123',
      });
      expect(projection.data.relationships.comments.data).toHaveLength(2);
    });
  });

  describe('applyFilters', () => {
    it('should filter with equality', () => {
      const items = [
        { id: 1, status: 'active' },
        { id: 2, status: 'inactive' },
        { id: 3, status: 'active' },
      ];

      const filtered = applyFilters(items, { status: 'active' });

      expect(filtered).toHaveLength(2);
    });

    it('should filter with operators', () => {
      const items = [
        { id: 1, count: 5 },
        { id: 2, count: 10 },
        { id: 3, count: 15 },
      ];

      const filtered = applyFilters(items, { count: { gt: 7 } });

      expect(filtered).toHaveLength(2);
      expect(filtered[0].count).toBe(10);
    });

    it('should filter with contains operator', () => {
      const items = [
        { id: 1, name: 'Alice' },
        { id: 2, name: 'Bob' },
        { id: 3, name: 'Charlie' },
      ];

      const filtered = applyFilters(items, { name: { contains: 'li' } });

      expect(filtered).toHaveLength(2); // Alice, Charlie
    });
  });

  describe('applySorting', () => {
    it('should sort ascending', () => {
      const items = [
        { id: 3, value: 30 },
        { id: 1, value: 10 },
        { id: 2, value: 20 },
      ];

      const sorted = applySorting(items, [{ field: 'value', direction: 'asc' }]);

      expect(sorted[0].value).toBe(10);
      expect(sorted[2].value).toBe(30);
    });

    it('should sort descending', () => {
      const items = [
        { id: 1, value: 10 },
        { id: 3, value: 30 },
        { id: 2, value: 20 },
      ];

      const sorted = applySorting(items, [{ field: 'value', direction: 'desc' }]);

      expect(sorted[0].value).toBe(30);
      expect(sorted[2].value).toBe(10);
    });

    it('should handle multi-field sorting', () => {
      const items = [
        { category: 'A', priority: 2 },
        { category: 'B', priority: 1 },
        { category: 'A', priority: 1 },
      ];

      const sorted = applySorting(items, [
        { field: 'category', direction: 'asc' },
        { field: 'priority', direction: 'asc' },
      ]);

      expect(sorted[0]).toEqual({ category: 'A', priority: 1 });
      expect(sorted[1]).toEqual({ category: 'A', priority: 2 });
    });
  });

  describe('applyPagination', () => {
    it('should paginate items', () => {
      const items = Array.from({ length: 10 }, (_, i) => ({ id: i + 1 }));

      const result = applyPagination(items, { page: 2, pageSize: 3 });

      expect(result.data).toHaveLength(3);
      expect(result.data[0].id).toBe(4);
      expect(result.meta.pagination.total).toBe(10);
      expect(result.meta.pagination.totalPages).toBe(4);
    });
  });

  describe('buildHATEOASLinks', () => {
    it('should build HATEOAS links', () => {
      const links = buildHATEOASLinks('users', '123');

      expect(links.self).toBe('/users/123');
      expect(links.collection).toBe('/users');
    });

    it('should include additional links', () => {
      const links = buildHATEOASLinks('users', '123', [
        { rel: 'posts', href: '/users/123/posts' },
        { rel: 'profile', href: '/users/123/profile' },
      ]);

      expect(links.posts).toBe('/users/123/posts');
      expect(links.profile).toBe('/users/123/profile');
    });
  });
});
