/**
 * @file Storage Tests - Memory, File, Database backends
 * @description Comprehensive tests for all storage implementations
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import { join } from 'path';
import {
  MemoryStorage,
  FileStorage,
  DatabaseStorage,
  createMemoryStorage,
  createFileStorage,
  createDatabaseStorage,
  createStorage
} from '../src/storage/index.mjs';

// ============================================================================
// TEST DATA
// ============================================================================

const testArtifact = {
  probe_run_id: 'test-run-1',
  id: 'artifact-1',
  observations: [
    { id: 'obs-1', agent: 'test', kind: 'test', severity: 'info' }
  ]
};

// ============================================================================
// MEMORY STORAGE TESTS
// ============================================================================

describe('MemoryStorage', () => {
  let storage;

  beforeEach(() => {
    storage = createMemoryStorage();
  });

  it('should create with type memory', () => {
    expect(storage.type).toBe('memory');
  });

  it('should set and get values', async () => {
    await storage.set('key1', { data: 'value1' });
    const value = await storage.get('key1');
    expect(value).toEqual({ data: 'value1' });
  });

  it('should return undefined for missing keys', async () => {
    const value = await storage.get('nonexistent');
    expect(value).toBeUndefined();
  });

  it('should delete values', async () => {
    await storage.set('key1', 'value1');
    const deleted = await storage.delete('key1');
    expect(deleted).toBe(true);
    expect(await storage.get('key1')).toBeUndefined();
  });

  it('should return false when deleting nonexistent key', async () => {
    const deleted = await storage.delete('nonexistent');
    expect(deleted).toBe(false);
  });

  it('should query by pattern', async () => {
    await storage.set('user:1', { name: 'Alice' });
    await storage.set('user:2', { name: 'Bob' });
    await storage.set('product:1', { name: 'Widget' });

    const users = await storage.query('user:*');
    expect(users.length).toBe(2);
    expect(users.map(u => u.key)).toContain('user:1');
    expect(users.map(u => u.key)).toContain('user:2');
  });

  it('should list all keys', async () => {
    await storage.set('a', 1);
    await storage.set('b', 2);
    await storage.set('c', 3);

    const keys = await storage.keys();
    expect(keys).toContain('a');
    expect(keys).toContain('b');
    expect(keys).toContain('c');
  });

  it('should count entries', async () => {
    expect(await storage.count()).toBe(0);
    await storage.set('a', 1);
    expect(await storage.count()).toBe(1);
    await storage.set('b', 2);
    expect(await storage.count()).toBe(2);
  });

  it('should clear all entries', async () => {
    await storage.set('a', 1);
    await storage.set('b', 2);
    await storage.clear();
    expect(await storage.count()).toBe(0);
  });

  it('should check if key exists', async () => {
    await storage.set('exists', true);
    expect(await storage.has('exists')).toBe(true);
    expect(await storage.has('missing')).toBe(false);
  });

  it('should throw for invalid key', async () => {
    await expect(storage.set('', 'value')).rejects.toThrow();
    await expect(storage.set(123, 'value')).rejects.toThrow();
  });

  it('should clone values (not reference)', async () => {
    const original = { nested: { value: 1 } };
    await storage.set('key', original);
    original.nested.value = 999;
    const retrieved = await storage.get('key');
    expect(retrieved.nested.value).toBe(1);
  });

  // Artifact API tests
  it('should save and load artifact', async () => {
    await storage.saveArtifact(testArtifact);
    const loaded = await storage.loadArtifact('test-run-1');
    expect(loaded.probe_run_id).toBe('test-run-1');
  });

  it('should fetch all shards', async () => {
    await storage.saveArtifact({ probe_run_id: 'run-1', data: 1 });
    await storage.saveArtifact({ probe_run_id: 'run-2', data: 2 });
    const shards = await storage.fetchShards();
    expect(shards.length).toBe(2);
  });

  it('should list artifact IDs', async () => {
    await storage.saveArtifact({ probe_run_id: 'run-1' });
    await storage.saveArtifact({ probe_run_id: 'run-2' });
    const ids = await storage.listArtifacts();
    expect(ids).toContain('run-1');
    expect(ids).toContain('run-2');
  });

  it('should delete artifact', async () => {
    await storage.saveArtifact({ probe_run_id: 'run-1' });
    const deleted = await storage.deleteArtifact('run-1');
    expect(deleted).toBe(true);
    await expect(storage.loadArtifact('run-1')).rejects.toThrow();
  });
});

// ============================================================================
// FILE STORAGE TESTS
// ============================================================================

describe('FileStorage', () => {
  const testDir = '/tmp/kgc-probe-test-storage';
  let storage;

  beforeEach(async () => {
    // Clean up before each test
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch {}
    storage = createFileStorage(testDir);
  });

  afterEach(async () => {
    // Clean up after each test
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch {}
  });

  it('should create with type file', () => {
    expect(storage.type).toBe('file');
  });

  it('should set and get values', async () => {
    await storage.set('key1', { data: 'value1' });
    const value = await storage.get('key1');
    expect(value).toEqual({ data: 'value1' });
  });

  it('should return undefined for missing keys', async () => {
    const value = await storage.get('nonexistent');
    expect(value).toBeUndefined();
  });

  it('should delete values', async () => {
    await storage.set('key1', 'value1');
    const deleted = await storage.delete('key1');
    expect(deleted).toBe(true);
    expect(await storage.get('key1')).toBeUndefined();
  });

  it('should return false when deleting nonexistent key', async () => {
    const deleted = await storage.delete('nonexistent');
    expect(deleted).toBe(false);
  });

  it('should list all keys', async () => {
    await storage.set('a', 1);
    await storage.set('b', 2);
    const keys = await storage.keys();
    expect(keys).toContain('a');
    expect(keys).toContain('b');
  });

  it('should count entries', async () => {
    expect(await storage.count()).toBe(0);
    await storage.set('a', 1);
    expect(await storage.count()).toBe(1);
  });

  it('should clear all entries', async () => {
    await storage.set('a', 1);
    await storage.set('b', 2);
    await storage.clear();
    expect(await storage.count()).toBe(0);
  });

  it('should check if key exists', async () => {
    await storage.set('exists', true);
    expect(await storage.has('exists')).toBe(true);
    expect(await storage.has('missing')).toBe(false);
  });

  it('should query by pattern', async () => {
    await storage.set('user_1', { name: 'Alice' });
    await storage.set('user_2', { name: 'Bob' });
    await storage.set('product_1', { name: 'Widget' });

    const users = await storage.query('user_*');
    expect(users.length).toBe(2);
  });

  // Artifact API tests
  it('should save and load artifact', async () => {
    await storage.saveArtifact({ probe_run_id: 'run-1', data: 'test' });
    const loaded = await storage.loadArtifact('run-1');
    expect(loaded.probe_run_id).toBe('run-1');
  });

  it('should throw for missing artifact', async () => {
    await expect(storage.loadArtifact('nonexistent')).rejects.toThrow();
  });
});

// ============================================================================
// DATABASE STORAGE TESTS
// ============================================================================

describe('DatabaseStorage', () => {
  let storage;

  beforeEach(() => {
    storage = createDatabaseStorage({ namespace: 'test' });
  });

  it('should create with type database', () => {
    expect(storage.type).toBe('database');
  });

  it('should set and get values', async () => {
    await storage.set('key1', { data: 'value1' });
    const value = await storage.get('key1');
    expect(value).toEqual({ data: 'value1' });
  });

  it('should use namespace prefix', async () => {
    await storage.set('key1', 'value1');
    // Internal check - the key should be prefixed
    expect(storage._data.has('test:key1')).toBe(true);
  });

  it('should return undefined for missing keys', async () => {
    const value = await storage.get('nonexistent');
    expect(value).toBeUndefined();
  });

  it('should delete values', async () => {
    await storage.set('key1', 'value1');
    const deleted = await storage.delete('key1');
    expect(deleted).toBe(true);
    expect(await storage.get('key1')).toBeUndefined();
  });

  it('should query by pattern', async () => {
    await storage.set('user:1', { name: 'Alice' });
    await storage.set('user:2', { name: 'Bob' });
    await storage.set('product:1', { name: 'Widget' });

    const users = await storage.query('user:*');
    expect(users.length).toBe(2);
  });

  it('should query by criteria object', async () => {
    await storage.set('user:1', { name: 'Alice', role: 'admin' });
    await storage.set('user:2', { name: 'Bob', role: 'user' });
    await storage.set('user:3', { name: 'Carol', role: 'admin' });

    const admins = await storage.query({ role: 'admin' });
    expect(admins.length).toBe(2);
  });

  it('should create and use indices', async () => {
    storage.createIndex('role');

    await storage.set('user:1', { name: 'Alice', role: 'admin' });
    await storage.set('user:2', { name: 'Bob', role: 'user' });
    await storage.set('user:3', { name: 'Carol', role: 'admin' });

    const admins = await storage.query({ role: 'admin' });
    expect(admins.length).toBe(2);
  });

  it('should list all keys', async () => {
    await storage.set('a', 1);
    await storage.set('b', 2);
    const keys = await storage.keys();
    expect(keys).toContain('a');
    expect(keys).toContain('b');
  });

  it('should count entries', async () => {
    expect(await storage.count()).toBe(0);
    await storage.set('a', 1);
    expect(await storage.count()).toBe(1);
  });

  it('should clear all entries', async () => {
    await storage.set('a', 1);
    await storage.set('b', 2);
    await storage.clear();
    expect(await storage.count()).toBe(0);
  });

  it('should track version', async () => {
    const v0 = storage.getVersion();
    await storage.set('a', 1);
    const v1 = storage.getVersion();
    await storage.set('b', 2);
    const v2 = storage.getVersion();

    expect(v1).toBeGreaterThan(v0);
    expect(v2).toBeGreaterThan(v1);
  });

  it('should batch set multiple values', async () => {
    await storage.batchSet([
      { key: 'a', value: 1 },
      { key: 'b', value: 2 },
      { key: 'c', value: 3 }
    ]);

    expect(await storage.get('a')).toBe(1);
    expect(await storage.get('b')).toBe(2);
    expect(await storage.get('c')).toBe(3);
  });

  it('should batch get multiple values', async () => {
    await storage.set('a', 1);
    await storage.set('b', 2);

    const results = await storage.batchGet(['a', 'b', 'c']);
    expect(results.get('a')).toBe(1);
    expect(results.get('b')).toBe(2);
    expect(results.get('c')).toBeUndefined();
  });

  // Artifact API tests
  it('should save and load artifact', async () => {
    await storage.saveArtifact({ probe_run_id: 'run-1', data: 'test' });
    const loaded = await storage.loadArtifact('run-1');
    expect(loaded.probe_run_id).toBe('run-1');
  });

  it('should fetch all shards', async () => {
    await storage.saveArtifact({ probe_run_id: 'run-1' });
    await storage.saveArtifact({ probe_run_id: 'run-2' });
    const shards = await storage.fetchShards();
    expect(shards.length).toBe(2);
  });

  it('should list artifact IDs', async () => {
    await storage.saveArtifact({ probe_run_id: 'run-1' });
    await storage.saveArtifact({ probe_run_id: 'run-2' });
    const ids = await storage.listArtifacts();
    expect(ids).toContain('run-1');
    expect(ids).toContain('run-2');
  });
});

// ============================================================================
// FACTORY FUNCTION TESTS
// ============================================================================

describe('createStorage Factory', () => {
  it('should create memory storage', () => {
    const storage = createStorage('memory');
    expect(storage.type).toBe('memory');
    expect(storage).toBeInstanceOf(MemoryStorage);
  });

  it('should create file storage', () => {
    const storage = createStorage('file', { rootDir: '/tmp/test' });
    expect(storage.type).toBe('file');
    expect(storage).toBeInstanceOf(FileStorage);
  });

  it('should create database storage', () => {
    const storage = createStorage('database', { namespace: 'test' });
    expect(storage.type).toBe('database');
    expect(storage).toBeInstanceOf(DatabaseStorage);
  });

  it('should throw for unknown type', () => {
    expect(() => createStorage('unknown')).toThrow('Unknown storage type');
  });
});

// ============================================================================
// ISOLATION TESTS
// ============================================================================

describe('Storage Isolation', () => {
  it('should isolate memory storage instances', async () => {
    const s1 = createMemoryStorage();
    const s2 = createMemoryStorage();

    await s1.set('key', 'value1');
    await s2.set('key', 'value2');

    expect(await s1.get('key')).toBe('value1');
    expect(await s2.get('key')).toBe('value2');
  });

  it('should isolate database namespaces', async () => {
    const s1 = createDatabaseStorage({ namespace: 'ns1' });
    const s2 = createDatabaseStorage({ namespace: 'ns2' });

    await s1.set('key', 'value1');
    await s2.set('key', 'value2');

    expect(await s1.get('key')).toBe('value1');
    expect(await s2.get('key')).toBe('value2');
  });
});
