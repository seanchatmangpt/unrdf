/**
 * @file Domain Layer - Central export point
 * @module cli/domain
 *
 * ARCHITECTURE: Three-Tiered Separation of Concerns
 *
 * 1. CLI COMMANDS (Presentation Layer) - cli/commands/**\/*.mjs
 *    - Parse CLI arguments using citty
 *    - Validate input (file exists, format supported, etc.)
 *    - Call domain services
 *    - Format output for console display
 *    - Handle process exit codes
 *
 * 2. DOMAIN SERVICES (Business Logic Layer) - cli/domain/**\/*.mjs ← YOU ARE HERE
 *    - Encapsulate business rules
 *    - Orchestrate operations across packages
 *    - Validate domain constraints
 *    - Transform data between layers
 *    - Provide rich error messages
 *
 * 3. PACKAGES (Data Access Layer) - packages/**\/*.mjs
 *    - CRUD operations on RDF store
 *    - Hook registry management
 *    - Low-level RDF parsing/serialization
 *    - No business logic
 *
 * BENEFITS:
 * - Commands are thin wrappers (5-20 LOC each)
 * - Business logic is testable without CLI
 * - Same services can be used by CLI, Next.js server actions, API endpoints
 * - Clear separation makes refactoring easy
 *
 * PATTERN:
 * ```javascript
 * // CLI Command (thin wrapper)
 * export const queryCommand = defineCommand({
 *   meta: { name: 'query', description: 'Execute SPARQL query' },
 *   args: { query: { type: 'string', required: true } },
 *   async run(ctx) {
 *     const service = getStoreService();
 *     const result = await service.executeQuery({ query: ctx.args.query });
 *     console.log(formatOutput(result.data));
 *   }
 * });
 * ```
 */

export { StoreService, getStoreService } from './store-service.mjs';
export { HookService, getHookService } from './hook-service.mjs';
export { GraphService, getGraphService } from './graph-service.mjs';
