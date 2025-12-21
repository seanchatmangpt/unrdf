/**
 * Initialize OTEL provider with span processor
 *
 * **Poka-yoke**: Input validation prevents invalid validationId, type guards ensure state consistency
 *
 * @param {string} validationId - Validation ID to track spans (must be non-empty string)
 * @param {Function} onSpanEnd - Callback when span ends (receives span data object)
 * @throws {Error} If validationId is invalid
 */
export function ensureProviderInitialized(validationId: string, onSpanEnd: Function): Promise<any>;
/**
 * Shutdown provider and cleanup
 *
 * **Poka-yoke**: Input validation prevents invalid validationId
 *
 * @param {string} validationId - Validation ID to shutdown (must be non-empty string)
 * @throws {Error} If validationId is invalid
 */
export function shutdownProvider(validationId: string): Promise<void>;
/**
 * Force flush all pending spans
 *
 * **Poka-yoke**: Type guards prevent operations on null processor/provider
 *
 * @throws {Error} If processor or provider operations fail
 */
export function forceFlush(): Promise<void>;
/**
 * Get current provider
 */
export function getProvider(): any;
