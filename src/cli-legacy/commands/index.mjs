/**
 * @file CLI Commands Index
 * @module cli/commands
 *
 * @description
 * Central export for all CLI command modules.
 */

export { parseCommand, parseCommandMeta } from './parse.mjs';
export { queryCommand, queryCommandMeta } from './query.mjs';
export { validateCommand, validateCommandMeta } from './validate.mjs';

// Hook commands
export {
  hookEvalCommand,
  hookListCommand,
  hookCreateCommand,
  hookGetCommand,
  hookHistoryCommand,
  hookCommandMeta
} from './hook.mjs';

// Graph commands
export {
  graphListCommand,
  graphGetCommand,
  graphCreateCommand,
  graphDeleteCommand,
  graphImportCommand,
  graphExportCommand,
  graphValidateCommand,
  graphStatsCommand,
  graphCommandMeta
} from './graph.mjs';

// Sidecar commands
export {
  sidecarStatusCommand,
  sidecarHealthCommand,
  sidecarMetricsCommand,
  sidecarConfigGetCommand,
  sidecarConfigSetCommand,
  sidecarCommandMeta
} from './sidecar.mjs';

// Store commands
export {
  storeImportCommand,
  storeExportCommand,
  storeQueryCommand,
  storeCommandMeta
} from './store.mjs';

// Policy commands
export {
  policyApplyCommand,
  policyListCommand,
  policyGetCommand,
  policyValidateCommand,
  policyAuditCommand,
  policyCommandMeta
} from './policy.mjs';
