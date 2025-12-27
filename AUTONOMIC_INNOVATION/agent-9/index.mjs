/**
 * @file Shadow mode system for safe migration
 * @description Main entry point for shadow mode functionality
 */

// Shadow mode operations
export { shadowWrite, shadowRead, partialServe } from './shadow.mjs';

// Mismatch reporting
export {
  mismatchReport,
  canonicalizeMismatchReport,
  hashMismatchReport
} from './mismatch-report.mjs';

// Routing
export {
  defineRoute,
  routingDecision,
  catchAllRoute,
  pathRoute,
  methodRoute,
  headerRoute,
  andRoute,
  orRoute
} from './routing.mjs';

// Demo scenarios
export {
  runMigrationDemo,
  runMismatchDemo,
  runRoutingDemo
} from './demo-scenario.mjs';
