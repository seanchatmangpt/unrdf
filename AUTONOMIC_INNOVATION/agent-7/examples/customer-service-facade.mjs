/**
 * Customer Service Façade
 * Auto-generated from Customer Service spec
 * Conventions: target-org profile v1.0.0
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

import { logOperation } from './lens.mjs';
import { transformFields } from './logging.mjs';

const CustomerDataSchema = z.object({
  email: z.string().email(),
  name: z.string()
});

/**
 * Create a new customer
 * @param {CustomerData} data - Customer information
 * @returns {Customer} Created customer
 * @throws {ValidationError} If data is invalid
 */
export async function createCustomer(data) {
  try {
    const validated = CustomerDataSchema.parse(data);

    logOperation({
      customer_email: validated.email,
      customer_name: validated.name,
      operation: 'createCustomer'
    });

    const transformed = transformFields(validated);
    const customer = await store.create('Customer', transformed);

    return customer;
  } catch (err) {
    if (err instanceof NotFoundError) throw err;
    throw new ValidationError('Failed to create customer', { cause: err });
  }
}

/**
 * Retrieve customer by ID
 * @param {string} id - Customer ID
 * @returns {Customer} Customer record
 * @throws {NotFoundError} If customer does not exist
 */
export async function getCustomer(id) {
  try {
    logOperation({
      customer_id: id,
      operation: 'getCustomer'
    });

    const customer = await store.get('Customer', id);

    if (!customer) {
      throw new NotFoundError(`Customer ${id} not found`);
    }

    return customer;
  } catch (err) {
    if (err instanceof NotFoundError) throw err;
    throw new OperationError('Failed to retrieve customer', { cause: err });
  }
}
