/**
 * Get documentation for a topic
 * @param {string} topic - Topic name
 * @returns {{title: string, content: string, examples?: string[]}}
 */
export function getDocumentation(topic: string): {
    title: string;
    content: string;
    examples?: string[];
};
/**
 * List all available documentation topics
 * @deprecated Legacy compatibility function
 * @returns {string[]} Array of available documentation topics
 * @example
 * const topics = listTopics();
 * // ['overview', 'receipts', 'delta', 'grammar', 'cli', 'adapters', 'migration', 'examples']
 */
export function listTopics(): string[];
export * from "./pipeline.mjs";
export * from "./latex-generator.mjs";
export * from "./thesis-builder.mjs";
export * from "./latex-generator.schema.mjs";
export * from "./thesis-builder.schema.mjs";
export namespace V6_DOCS {
    export let version: string;
    export let topics: string[];
    export { getDocumentation };
    export { listTopics };
}
