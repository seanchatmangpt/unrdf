/**
 * @file Redis Testcontainer Test
 * @module redis-testcontainer
 * 
 * @description
 * A test to verify Redis container integration with the knowledge engine.
 * This test starts a Redis container and tests basic operations.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { GenericContainer } from 'testcontainers';
import { createClient } from 'redis';

describe('Redis Testcontainer Test', () => {
  let container;
  let redisClient;

  beforeAll(async () => {
    // Start Redis container
    container = await new GenericContainer('redis:7-alpine')
      .withExposedPorts(6379)
      .withStartupTimeout(30000)
      .start();

    // Create Redis client
    const host = container.getHost();
    const port = container.getMappedPort(6379);
    const url = `redis://${host}:${port}`;
    
    redisClient = createClient({ url });
    await redisClient.connect();
  }, 60000);

  afterAll(async () => {
    if (redisClient) {
      await redisClient.disconnect();
    }
    if (container) {
      await container.stop();
    }
  });

  it('should start Redis container successfully', () => {
    expect(container).toBeDefined();
    expect(container.getHost()).toBeDefined();
    expect(container.getMappedPort(6379)).toBeDefined();
  });

  it('should connect to Redis and perform basic operations', async () => {
    // Test basic Redis operations
    await redisClient.set('test:key', 'test:value');
    const value = await redisClient.get('test:key');
    expect(value).toBe('test:value');

    // Test hash operations
    await redisClient.hSet('test:hash', { field1: 'value1', field2: 'value2' });
    const hash = await redisClient.hGetAll('test:hash');
    expect(hash).toEqual({ field1: 'value1', field2: 'value2' });

    // Test list operations
    await redisClient.lPush('test:list', 'item1');
    await redisClient.lPush('test:list', 'item2');
    await redisClient.lPush('test:list', 'item3');
    const list = await redisClient.lRange('test:list', 0, -1);
    expect(list).toEqual(['item3', 'item2', 'item1']);
  });

  it('should handle Redis transactions', async () => {
    // Clear any existing keys first
    await redisClient.del('tx:key1', 'tx:key2');
    
    const multi = redisClient.multi();
    multi.set('tx:key1', 'value1');
    multi.set('tx:key2', 'value2');
    multi.get('tx:key1');
    multi.get('tx:key2');
    
    const results = await multi.exec();
    expect(results).toHaveLength(4);
    
    // Verify the values were set correctly
    const value1 = await redisClient.get('tx:key1');
    const value2 = await redisClient.get('tx:key2');
    expect(value1).toBe('value1');
    expect(value2).toBe('value2');
  });

  it('should handle Redis pub/sub', async () => {
    const subscriber = redisClient.duplicate();
    await subscriber.connect();

    const messages = [];
    await subscriber.subscribe('test:channel', (message) => {
      messages.push(message);
    });

    // Publish a message
    await redisClient.publish('test:channel', 'Hello Redis!');
    
    // Wait a bit for the message to be received
    await new Promise(resolve => setTimeout(resolve, 100));
    
    expect(messages).toContain('Hello Redis!');
    
    await subscriber.disconnect();
  });
});
