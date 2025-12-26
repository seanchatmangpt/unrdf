/**
 * @fileoverview Tests for CustomerService
 * @generated Convention-preserving code generator (Agent 7)
 * Target coverage: 80%
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'assert';
import { createStore } from '@unrdf/oxigraph';
import { CustomerService } from './customer-service.mjs';
import { AppError } from './errors.mjs';

describe('CustomerService', () => {
  let service;
  let store;

  beforeEach(() => {
    store = createStore();
    service = new CustomerService(store);
  });

  describe('createCustomer', () => {
    it('should create customer successfully', async () => {
      const dto = { id: 'test-1', data: { name: 'Test' } };
      const result = await service.createCustomer(dto);

      assert.strictEqual(result.id, 'test-1');
      assert.strictEqual(result.created, true);
    });

    it('should throw error when ID is missing', async () => {
      await assert.rejects(
        async () => await service.createCustomer({}),
        AppError
      );
    });
  })

  describe('readCustomer', () => {
    it('should read existing customer', async () => {
      const dto = { id: 'test-2', data: {} };
      await service.createCustomer(dto);

      const result = await service.readCustomer('test-2');
      assert.strictEqual(result.id, 'test-2');
      assert.strictEqual(result.exists, true);
    });

    it('should throw error when customer not found', async () => {
      await assert.rejects(
        async () => await service.readCustomer('non-existent'),
        AppError
      );
    });
  })

  describe('updateCustomer', () => {
    it('should update existing customer', async () => {
      const dto = { id: 'test-3', data: {} };
      await service.createCustomer(dto);

      const result = await service.updateCustomer('test-3', { name: 'Updated' });
      assert.strictEqual(result.updated, true);
    });

    it('should throw error when customer not found', async () => {
      await assert.rejects(
        async () => await service.updateCustomer('non-existent', {}),
        AppError
      );
    });
  })

  describe('deleteCustomer', () => {
    it('should delete customer', async () => {
      const dto = { id: 'test-4', data: {} };
      await service.createCustomer(dto);

      const result = await service.deleteCustomer('test-4');
      assert.strictEqual(result.deleted, true);
    });

    it('should throw error when ID is missing', async () => {
      await assert.rejects(
        async () => await service.deleteCustomer(null),
        AppError
      );
    });
  })

  describe('listCustomers', () => {
    it('should list all customers', async () => {
      await service.createCustomer({ id: 'test-5', data: {} });
      await service.createCustomer({ id: 'test-6', data: {} });

      const results = await service.listCustomers();
      assert.ok(results.length >= 2);
    });

    it('should respect limit option', async () => {
      await service.createCustomer({ id: 'test-7', data: {} });
      await service.createCustomer({ id: 'test-8', data: {} });

      const results = await service.listCustomers({ limit: 1 });
      assert.strictEqual(results.length, 1);
    });
  })
});
