#!/usr/bin/env node
/**
 * Legacy: buildCLISpine()
 * Creates CLI structure for backward compatibility
 *
 * @returns {Object} CLI structure
 */
export function buildCLISpine(): any;
/**
 * Legacy: executeCommand()
 * Execute a command by name for backward compatibility
 *
 * @param {string} commandName - Command name (e.g., 'receipt:create')
 * @param {Object} [args={}] - Command arguments
 * @returns {Promise<Object>} Result
 */
export function executeCommand(commandName: string, args?: any): Promise<any>;
export * from "./commands/delta.mjs";
export * from "./commands/receipt.mjs";
export * from "./commands/grammar.mjs";
export * from "./commands/thesis.mjs";
export * from "./spine.mjs";
export * from "./nouns.mjs";
export * from "./verbs.mjs";
/**
 * Legacy V6 commands registry for backward compatibility
 * @deprecated Use CLI extensions directly via buildV6CittyTree
 * @constant {Object}
 * @property {Object} receipt:create - Create a new receipt command
 * @property {Object} receipt:verify - Verify a receipt command
 * @property {Object} receipt:chain - Create a receipt chain command
 * @property {Object} delta:propose - Propose a delta command
 * @property {Object} delta:apply - Apply a delta command
 * @property {Object} grammar:compile - Compile grammar command
 * @property {Object} grammar:validate - Validate against grammar command
 */
export const V6_COMMANDS: {
    'receipt:create': {
        description: string;
    };
    'receipt:verify': {
        description: string;
    };
    'receipt:chain': {
        description: string;
    };
    'delta:propose': {
        description: string;
    };
    'delta:apply': {
        description: string;
    };
    'grammar:compile': {
        description: string;
    };
    'grammar:validate': {
        description: string;
    };
    'grammar:show': {
        description: string;
    };
    'v6:status': {
        description: string;
    };
};
/**
 * Initialize v6 registry and load extensions.
 *
 * @returns {Promise<{registry: Registry, spine: Object}>}
 */
export function initializeV6Registry(): Promise<{
    registry: Registry;
    spine: any;
}>;
/**
 * Build Citty command tree from v6 registry.
 *
 * @param {Registry} registry
 * @param {Object} spine
 * @returns {Object} Citty command definition
 */
export function buildV6CittyTree(registry: Registry, spine: any): any;
/**
 * Main entry point.
 */
export function main(): Promise<void>;
