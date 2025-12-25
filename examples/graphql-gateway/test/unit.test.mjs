/**
 * @file Unit Tests
 * @description Unit tests for GraphQL gateway components
 */

import { describe, it, expect } from 'vitest';
import { GraphQLServer } from '../src/server.mjs';
import { schema } from '../src/schema.mjs';
import { resolvers } from '../src/resolvers.mjs';

describe('GraphQLServer', () => {
  it('should create server', () => {
    const server = new GraphQLServer({ port: 4001 });
    expect(server).toBeDefined();
    expect(server.config.port).toBe(4001);
  });

  it('should have default config', () => {
    const server = new GraphQLServer();
    expect(server.config.enableIntrospection).toBe(true);
    expect(server.config.enablePlayground).toBe(true);
  });
});

describe('GraphQL Schema', () => {
  it('should define schema', () => {
    expect(schema).toBeDefined();
    expect(schema).toContain('type Workflow');
    expect(schema).toContain('type Query');
    expect(schema).toContain('type Mutation');
    expect(schema).toContain('type Subscription');
  });

  it('should define resolvers', () => {
    expect(resolvers).toBeDefined();
    expect(resolvers.Query).toBeDefined();
    expect(resolvers.Mutation).toBeDefined();
    expect(resolvers.Subscription).toBeDefined();
  });
});
