/**
 * Get v6 alpha status
 * @returns {{ version: string, features: Object, status: string }}
 */
export function getV6Status(): {
    version: string;
    features: any;
    status: string;
};
/**
 * Check if a feature is enabled
 * @param {string} feature - Feature name
 * @returns {boolean}
 */
export function isFeatureEnabled(feature: string): boolean;
/**
 * @unrdf/v6-core - UNRDF v6 Alpha Entry Point
 *
 * Receipt-driven RDF with Merkle proofs, delta proposals, and versioned grammar.
 *
 * @module @unrdf/v6-core
 */
/**
 * UNRDF v6 version identifier
 * @constant {string}
 */
export const V6_VERSION: "6.0.0-alpha.1";
export namespace V6_FEATURES {
    let receipts: boolean;
    let delta: boolean;
    let cli: boolean;
    let grammar: boolean;
    let docs: boolean;
    let otel: boolean;
    let performance: boolean;
}
export * from "./receipts/index.mjs";
export * from "./delta/index.mjs";
export * from "./cli/index.mjs";
export * from "./grammar/index.mjs";
export * from "./docs/index.mjs";
