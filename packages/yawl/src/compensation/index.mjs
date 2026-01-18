/**
 * @file Compensation Framework - Saga Pattern Support
 * @module yawl/compensation
 *
 * @description
 * Complete compensation framework for YAWL enabling saga pattern
 * with transaction semantics and undo capability.
 *
 * Novel feature - Java YAWL has limited compensation support.
 */

export {
  CompensationRegistry,
  createCompensationRegistry,
  CompensationSpecSchema,
  CompensationMappingSchema,
} from './compensation-registry.mjs';

export {
  SagaCoordinator,
  createSagaCoordinator,
  SagaTaskSchema,
  SagaExecutionSchema,
} from './saga-coordinator.mjs';

export {
  CompensationExecutor,
  createCompensationExecutor,
  CompensationContextSchema,
  InvariantCheckSchema,
} from './compensation-executor.mjs';

export {
  SnapshotManager,
  createSnapshotManager,
  StateSnapshotSchema,
} from './state-snapshots.mjs';

export {
  createMonetaryRefund,
  createInventoryRestock,
  createReservationCancellation,
  createNotificationRetraction,
  createDatabaseRollback,
  createRecordDeletion,
  createMultiStepCompensation,
  createConditionalCompensation,
  createCustomCompensation,
  composeCompensations,
} from './compensation-patterns.mjs';
