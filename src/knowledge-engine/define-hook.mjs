/**
 * @file 80/20 Knowledge Hook definition contract for autonomic systems.
 * @module newco/defineHook
 *
 * @description
 * This module provides the `defineHook` function, the sole entry point for
 * defining a Knowledge Hook. The contract enforces critical principles for
 * autonomic, deterministic, and provable systems:
 *
 * 1.  **Conditions are Addressed, Not Embedded**: The `when` clause MUST
 * reference an external, content-addressed SPARQL or SHACL file.
 * This forbids inline query strings, ensuring that governance logic is
 * a verifiable, standalone artifact.
 *
 * 2.  **Reflex Arc Lifecycle**: The `before`, `run`, and `after` functions
 * provide a minimal, complete lifecycle for autonomic reflexes:
 * - `before`: A pre-condition gate for payload normalization or cancellation.
 * - `run`: The core effect or analysis.
 * - `after`: A post-execution step for auditing and cleanup.
 *
 * 3.  **Declarative Configuration**: Determinism and receipting strategies
 * are declared as metadata, not implemented imperatively within the hook.
 *
 * 4.  **Comprehensive Validation**: Uses Zod schemas for complete type safety
 * and validation of all hook components.
 *
 * This API is designed to feel familiar to users of Nitro's `defineTask`
 * while being fundamentally adapted for a knowledge-native, policy-first runtime.
 */

/**
 * A content-addressed file reference. This is the only way to specify a
 * condition, ensuring that governance logic is a verifiable artifact and
 * not an inline string.
 *
 * @typedef {Object} Ref
 * @property {string} uri - The URI of the resource, typically a file path like "file://hooks/compliance/largeTx.ask.rq".
 * @property {string} sha256 - The SHA-256 hash of the file's content, for integrity and provenance.
 * @property {'application/sparql-query'|'text/shacl'|'text/turtle'} mediaType - The MIME type of the resource.
 */

/**
 * Descriptive metadata for cataloging, discovery, and tooling.
 *
 * @typedef {Object} HookMeta
 * @property {string} name - A unique, human-readable name for the hook (e.g., "compliance:largeTx").
 * @property {string} [description] - A brief explanation of the hook's purpose.
 * @property {string[]} [ontology] - A list of ontology prefixes this hook relates to (e.g., ["fibo", "prov"]).
 */

/**
 * Defines the knowledge surface the hook observes during its condition evaluation.
 *
 * @typedef {Object} HookChannel
 * @property {string[]} [graphs] - An array of named graph IRIs or labels to scope the query.
 * @property {'before'|'after'|'delta'} [view] - The state of the graph to evaluate against. 'before' is the state before the delta is applied, 'after' is the state after, and 'delta' is a graph containing only the additions and removals.
 */

/**
 * The declarative trigger condition for the hook, based on a content-addressed reference.
 *
 * @typedef {Object} HookCondition
 * @property {'sparql-ask'|'sparql-select'|'shacl'} kind - The type of evaluation to perform.
 * @property {Ref} ref - The content-addressed reference to the SPARQL or SHACL file.
 */

/**
 * The execution context passed to all lifecycle functions, providing access
 * to the graph and environment.
 *
 * @typedef {Object} HookContext
 * @property {any} [graph] - The active RDF/JS Store or Dataset instance.
 * @property {Object.<string, unknown>} [env] - Environment variables or runtime configuration.
 */

/**
 * The data payload for a hook event.
 *
 * @typedef {Object.<string, unknown>} HookPayload
 */

/**
 * The event object passed to each lifecycle function of a hook.
 *
 * @typedef {Object} HookEvent
 * @property {string} name - The name of the hook being executed.
 * @property {HookPayload} payload - The input data for the event.
 * @property {HookContext} context - The execution context.
 */

/**
 * The standardized result of a hook's `run` or `after` function.
 *
 * @typedef {Object} HookResult
 * @property {any} [result] - The primary output or return value of the hook.
 * @property {any} [assertions] - Optional RDF quads to be added to the graph as a result of the hook's execution.
 * @property {any} [deltas] - An optional, more complex delta (additions/removals) to be applied.
 * @property {boolean} [cancelled] - Indicates if the hook was cancelled during the `before` phase.
 * @property {string} [reason] - The reason for cancellation.
 */

/**
 * The complete, 80/20 contract for a Knowledge Hook. This object defines
 * the hook's identity, its trigger condition, its autonomic behavior, and
 * its operational guarantees.
 *
 * @typedef {Object} KnowledgeHook
 * @property {HookMeta} meta - Essential metadata for the hook.
 * @property {HookChannel} [channel] - The observation channel for the condition.
 * @property {HookCondition} when - The declarative, file-based trigger condition.
 * @property {{ seed?: number }} [determinism] - Configuration for deterministic operations. Defaults to a fixed seed of 42.
 * @property {{ anchor?: ('git-notes'|'none') }} [receipt] - Strategy for anchoring the transaction receipt. Defaults to 'none'.
 * @property {(e: HookEvent) => Promise<Partial<HookPayload>|{cancel:true,reason?:string}> | Partial<HookPayload>|{cancel:true,reason?:string}} [before] - A function that runs before the condition is checked. It can modify the payload or cancel the execution.
 * @property {(e: HookEvent) => Promise<HookResult> | HookResult} run - The main execution body of the hook.
 * @property {(e: HookEvent & { result?: any, cancelled?: boolean, reason?: string }) => Promise<HookResult> | HookResult} [after] - A function that runs after the `run` phase, for cleanup or auditing.
 */

/**
 * Defines a Knowledge Hook, validating its structure and enforcing the
 * 80/20 contract for autonomic systems using comprehensive Zod validation.
 *
 * @template T
 * @param {KnowledgeHook} def - The hook definition object.
 * @returns {KnowledgeHook} The validated and normalized hook definition.
 */
import { createKnowledgeHook } from "./schemas.mjs";
import { defaultSecurityValidator } from "./security-validator.mjs";

/**
 *
 */
export function defineHook(def) {
  // Use comprehensive Zod validation
  const validatedHook = createKnowledgeHook(def);

  // Apply security validation (warn only, don't block)
  // Security will be enforced at execution time via sandbox
  const securityValidation =
    defaultSecurityValidator.validateKnowledgeHook(validatedHook);
  if (!securityValidation.valid) {
    // Log warning in development (can't modify frozen object, so just log)
    if (process.env.NODE_ENV !== 'production') {
      console.warn(`[Security Warning] Hook "${validatedHook.meta.name}": ${securityValidation.blockReason}`);
    }
  }

  return validatedHook;
}

/* ------------------------- Example (Happy Path) ------------------------- */

/**
 * This is an example of how to use `defineHook` to create a compliance gate.
 * It demonstrates all the core features of the 80/20 contract.
 */
export const exampleComplianceHook = defineHook({
  meta: {
    name: "compliance:largeTx",
    description:
      "Alerts and creates an audit trail when a financial transaction exceeds a certain threshold.",
    ontology: ["fibo"],
  },
  channel: {
    graphs: ["urn:graph:fibo:prod"],
    view: "delta",
  },
  when: {
    kind: "sparql-ask",
    ref: {
      uri: "file://hooks/compliance/largeTx.ask.rq",
      sha256:
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", // Example hash
      mediaType: "application/sparql-query",
    },
  },
  determinism: { seed: 42 },
  receipt: { anchor: "git-notes" },

  async before({ payload }) {
    if (!payload || typeof payload.amount !== "number" || payload.amount <= 0) {
      return {
        cancel: true,
        reason: "Invalid or non-positive transaction amount.",
      };
    }
    // Normalize payload for the `run` step
    return { ...payload, validatedAt: new Date().toISOString() };
  },

  async run({ payload }) {
    console.log(
      `[RUN] Processing large transaction of ${payload.amount} validated at ${payload.validatedAt}`,
    );
    // The main result could be an alert object, an event to be emitted, etc.
    return {
      result: { status: "alert-dispatched", amount: payload.amount },
      // This hook also generates a new piece of knowledge (an audit triple).
      assertions: [
        /* an RDF quad like: [ a:tx, prov:wasGeneratedBy, this:hook ] */
      ],
    };
  },

  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`[AFTER] Hook execution was cancelled. Reason: ${reason}`);
    } else {
      console.log(
        `[AFTER] Hook successfully completed with status: ${result?.result?.status}`,
      );
    }
    // The 'after' hook always returns a result for logging/receipt purposes.
    return { result: { finalStatus: cancelled ? "cancelled" : "completed" } };
  },
});
