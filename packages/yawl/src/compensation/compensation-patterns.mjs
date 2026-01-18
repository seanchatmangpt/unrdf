/**
 * @file Compensation Patterns - Common Undo Scenarios
 * @module yawl/compensation/patterns
 *
 * @description
 * Common compensation patterns for typical business scenarios.
 * Reusable compensation workflows that can be customized.
 */

// ============================================================================
// MONETARY COMPENSATION PATTERNS
// ============================================================================

/**
 * Create monetary refund compensation
 * @param {Object} config - Configuration
 * @param {Function} config.refundFn - Function to execute refund
 * @param {string} [config.amountField='amount'] - Field name for amount
 * @param {string} [config.transactionIdField='transactionId'] - Field name for transaction ID
 * @returns {Object} Compensation spec
 *
 * @example
 * const refund = createMonetaryRefund({
 *   refundFn: async (transactionId, amount) => {
 *     await paymentService.refund(transactionId, amount);
 *   }
 * });
 */
export function createMonetaryRefund(config) {
  const amountField = config.amountField || 'amount';
  const transactionIdField = config.transactionIdField || 'transactionId';

  return {
    tasks: [
      {
        id: 'refund-payment',
        name: 'Process Refund',
        execute: async (ctx) => {
          const amount = ctx.input[amountField];
          const transactionId = ctx.input[transactionIdField];

          if (!amount || !transactionId) {
            throw new Error('Missing refund parameters');
          }

          await config.refundFn(transactionId, amount);

          return {
            refundedAmount: amount,
            refundedTransactionId: transactionId,
            refundedAt: new Date().toISOString(),
          };
        },
      },
    ],
  };
}

// ============================================================================
// INVENTORY COMPENSATION PATTERNS
// ============================================================================

/**
 * Create inventory restock compensation
 * @param {Object} config - Configuration
 * @param {Function} config.restockFn - Function to restock inventory
 * @param {string} [config.itemIdField='itemId'] - Field name for item ID
 * @param {string} [config.quantityField='quantity'] - Field name for quantity
 * @returns {Object} Compensation spec
 *
 * @example
 * const restock = createInventoryRestock({
 *   restockFn: async (itemId, quantity) => {
 *     await inventory.add(itemId, quantity);
 *   }
 * });
 */
export function createInventoryRestock(config) {
  const itemIdField = config.itemIdField || 'itemId';
  const quantityField = config.quantityField || 'quantity';

  return {
    tasks: [
      {
        id: 'restock-inventory',
        name: 'Restock Inventory',
        execute: async (ctx) => {
          const itemId = ctx.input[itemIdField];
          const quantity = ctx.input[quantityField];

          if (!itemId || !quantity) {
            throw new Error('Missing restock parameters');
          }

          await config.restockFn(itemId, quantity);

          return {
            restockedItemId: itemId,
            restockedQuantity: quantity,
            restockedAt: new Date().toISOString(),
          };
        },
      },
    ],
  };
}

/**
 * Create reservation cancellation compensation
 * @param {Object} config - Configuration
 * @param {Function} config.cancelFn - Function to cancel reservation
 * @param {string} [config.reservationIdField='reservationId'] - Field name for reservation ID
 * @returns {Object} Compensation spec
 *
 * @example
 * const cancelReservation = createReservationCancellation({
 *   cancelFn: async (reservationId) => {
 *     await reservationService.cancel(reservationId);
 *   }
 * });
 */
export function createReservationCancellation(config) {
  const reservationIdField = config.reservationIdField || 'reservationId';

  return {
    tasks: [
      {
        id: 'cancel-reservation',
        name: 'Cancel Reservation',
        execute: async (ctx) => {
          const reservationId = ctx.input[reservationIdField];

          if (!reservationId) {
            throw new Error('Missing reservation ID');
          }

          await config.cancelFn(reservationId);

          return {
            cancelledReservationId: reservationId,
            cancelledAt: new Date().toISOString(),
          };
        },
      },
    ],
  };
}

// ============================================================================
// NOTIFICATION COMPENSATION PATTERNS
// ============================================================================

/**
 * Create notification retraction compensation
 * @param {Object} config - Configuration
 * @param {Function} config.retractFn - Function to send retraction notification
 * @param {string} [config.recipientField='recipient'] - Field name for recipient
 * @param {string} [config.messageField='message'] - Field name for message
 * @returns {Object} Compensation spec
 *
 * @example
 * const retract = createNotificationRetraction({
 *   retractFn: async (recipient, originalMessage) => {
 *     await emailService.send(recipient, {
 *       subject: 'Previous message retracted',
 *       body: `Please disregard: ${originalMessage}`
 *     });
 *   }
 * });
 */
export function createNotificationRetraction(config) {
  const recipientField = config.recipientField || 'recipient';
  const messageField = config.messageField || 'message';

  return {
    tasks: [
      {
        id: 'retract-notification',
        name: 'Retract Notification',
        execute: async (ctx) => {
          const recipient = ctx.input[recipientField];
          const message = ctx.input[messageField];

          if (!recipient) {
            throw new Error('Missing recipient');
          }

          await config.retractFn(recipient, message);

          return {
            retractedRecipient: recipient,
            retractedAt: new Date().toISOString(),
          };
        },
      },
    ],
  };
}

// ============================================================================
// DATABASE COMPENSATION PATTERNS
// ============================================================================

/**
 * Create database rollback compensation
 * @param {Object} config - Configuration
 * @param {Function} config.rollbackFn - Function to rollback database changes
 * @param {string} [config.recordIdField='recordId'] - Field name for record ID
 * @param {string} [config.previousStateField='previousState'] - Field for previous state
 * @returns {Object} Compensation spec
 *
 * @example
 * const rollback = createDatabaseRollback({
 *   rollbackFn: async (recordId, previousState) => {
 *     await db.update(recordId, previousState);
 *   }
 * });
 */
export function createDatabaseRollback(config) {
  const recordIdField = config.recordIdField || 'recordId';
  const previousStateField = config.previousStateField || 'previousState';

  return {
    tasks: [
      {
        id: 'rollback-database',
        name: 'Rollback Database Changes',
        execute: async (ctx) => {
          const recordId = ctx.input[recordIdField];
          const previousState = ctx.input[previousStateField];

          if (!recordId) {
            throw new Error('Missing record ID');
          }

          await config.rollbackFn(recordId, previousState);

          return {
            rolledBackRecordId: recordId,
            rolledBackAt: new Date().toISOString(),
          };
        },
      },
    ],
  };
}

/**
 * Create record deletion compensation (for create operations)
 * @param {Object} config - Configuration
 * @param {Function} config.deleteFn - Function to delete record
 * @param {string} [config.recordIdField='recordId'] - Field name for record ID
 * @returns {Object} Compensation spec
 *
 * @example
 * const deleteRecord = createRecordDeletion({
 *   deleteFn: async (recordId) => {
 *     await db.delete(recordId);
 *   }
 * });
 */
export function createRecordDeletion(config) {
  const recordIdField = config.recordIdField || 'recordId';

  return {
    tasks: [
      {
        id: 'delete-record',
        name: 'Delete Record',
        execute: async (ctx) => {
          const recordId = ctx.input[recordIdField];

          if (!recordId) {
            throw new Error('Missing record ID');
          }

          await config.deleteFn(recordId);

          return {
            deletedRecordId: recordId,
            deletedAt: new Date().toISOString(),
          };
        },
      },
    ],
  };
}

// ============================================================================
// COMPOSITE PATTERNS
// ============================================================================

/**
 * Create multi-step compensation
 * @param {Object[]} steps - Compensation steps
 * @returns {Object} Compensation spec
 *
 * @example
 * const multiStep = createMultiStepCompensation([
 *   {
 *     id: 'step-1',
 *     execute: async (ctx) => { ... }
 *   },
 *   {
 *     id: 'step-2',
 *     execute: async (ctx) => { ... }
 *   }
 * ]);
 */
export function createMultiStepCompensation(steps) {
  return {
    tasks: steps.map((step, index) => ({
      id: step.id || `compensation-step-${index + 1}`,
      name: step.name || `Compensation Step ${index + 1}`,
      execute: step.execute,
    })),
  };
}

/**
 * Create conditional compensation
 * @param {Function} conditionFn - Function to check condition
 * @param {Object} ifTrue - Compensation if condition is true
 * @param {Object} ifFalse - Compensation if condition is false
 * @returns {Object} Compensation spec
 *
 * @example
 * const conditional = createConditionalCompensation(
 *   async (ctx) => ctx.input.amount > 1000,
 *   createMonetaryRefund({ ... }),
 *   createNotificationRetraction({ ... })
 * );
 */
export function createConditionalCompensation(conditionFn, ifTrue, ifFalse) {
  return {
    tasks: [
      {
        id: 'conditional-compensation',
        name: 'Conditional Compensation',
        execute: async (ctx) => {
          const condition = await conditionFn(ctx);

          const compensationSpec = condition ? ifTrue : ifFalse;

          let output = ctx.input;
          for (const task of compensationSpec.tasks) {
            output = await task.execute({ input: output });
          }

          return output;
        },
      },
    ],
  };
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Create custom compensation from function
 * @param {Function} compensateFn - Compensation function
 * @param {Object} [options] - Options
 * @returns {Object} Compensation spec
 *
 * @example
 * const custom = createCustomCompensation(async (ctx) => {
 *   // Custom compensation logic
 *   await someOperation(ctx.input);
 * }, { id: 'custom-undo', name: 'Custom Undo Operation' });
 */
export function createCustomCompensation(compensateFn, options = {}) {
  return {
    tasks: [
      {
        id: options.id || 'custom-compensation',
        name: options.name || 'Custom Compensation',
        execute: compensateFn,
      },
    ],
  };
}

/**
 * Compose multiple compensation patterns
 * @param {...Object} patterns - Compensation patterns to compose
 * @returns {Object} Composite compensation spec
 *
 * @example
 * const composed = composeCompensations(
 *   createMonetaryRefund({ ... }),
 *   createInventoryRestock({ ... }),
 *   createNotificationRetraction({ ... })
 * );
 */
export function composeCompensations(...patterns) {
  const allTasks = patterns.flatMap(pattern => pattern.tasks);

  return {
    tasks: allTasks,
  };
}
