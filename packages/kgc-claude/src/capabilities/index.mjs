/**
 * Hyper-Advanced Capabilities - Time Travel, Branching, Persistence, and Automation
 *
 * Exports all advanced capabilities for Claude Code.
 *
 * @module @unrdf/kgc-claude/capabilities
 */

// Time Travel
export {
  TimeTravelManager,
  createTimeTravelManager,
  NamedCheckpointSchema,
  CheckpointDiffSchema,
} from './time-travel.mjs';

// Execution Branches
export {
  ExecutionBranchManager,
  createExecutionBranchManager,
  ExecutionBranchSchema,
  MergeResultSchema,
} from './execution-branches.mjs';

// State Persistence
export {
  StatePersistenceManager,
  createStatePersistenceManager,
  createMemoryPersistenceManager,
  MemoryStorageBackend,
  FilesystemStorageBackend,
  IndexedDBStorageBackend,
  LocalStorageBackend,
  PersistedStateSchema,
  MigrationSchema,
} from './state-persistence.mjs';

// Headless Runner (Agent 6 α₆)
export {
  HeadlessRunner,
  createHeadlessRunner,
  execute,
  executeStream,
  OutputFormat,
  PermissionMode,
  HeadlessOptionsSchema,
  ExecutionResultSchema,
  StreamEventSchema,
} from './headless-runner.mjs';

// Batch Processor (Agent 6 α₆)
export {
  BatchProcessor,
  createBatchProcessor,
  executeBatch,
  TaskPriority,
  TaskStatus,
  BatchTaskSchema,
  BatchProcessorConfigSchema,
  BatchResultSchema,
  ProgressReportSchema,
} from './batch-processor.mjs';

// CI Integration (Agent 6 α₆)
export {
  CIIntegration,
  createCIIntegration,
  detectCI,
  runCITests,
  CIPlatform,
  CIEventSchema,
  TestResultSchema,
  PRAutomationConfigSchema,
} from './ci-integration.mjs';
