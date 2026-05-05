export namespace deltaExtension {
    let id: string;
    namespace nouns {
        namespace delta {
            let description: string;
            namespace verbs {
                export namespace propose {
                    let description_1: string;
                    export { description_1 as description };
                    export { proposeDelta as handler };
                    export { ProposeArgsSchema as argsSchema };
                    export let meta: {};
                }
                export namespace apply {
                    let description_2: string;
                    export { description_2 as description };
                    export { applyDelta as handler };
                    export { ApplyArgsSchema as argsSchema };
                    let meta_1: {};
                    export { meta_1 as meta };
                }
                export namespace verify {
                    let description_3: string;
                    export { description_3 as description };
                    export { verifyDelta as handler };
                    export { VerifyArgsSchema as argsSchema };
                    let meta_2: {};
                    export { meta_2 as meta };
                }
                export namespace _export {
                    let description_4: string;
                    export { description_4 as description };
                    export { exportDelta as handler };
                    export { ExportArgsSchema as argsSchema };
                    let meta_3: {};
                    export { meta_3 as meta };
                }
                export { _export as export };
            }
        }
    }
    let priority: number;
}
export default deltaExtension;
/**
 * Propose a delta.
 *
 * Creates and validates a proposed state transition.
 *
 * @param {Object} args - Validated args
 * @param {Object} [context={}] - Execution context with t_ns for determinism
 * @returns {Promise<Object>} Proposal result
 */
declare function proposeDelta(args: any, context?: any): Promise<any>;
/**
 * Argument schemas for delta commands.
 */
declare const ProposeArgsSchema: z.ZodObject<{
    file: z.ZodOptional<z.ZodString>;
    delta: z.ZodOptional<z.ZodString>;
    description: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Apply a delta.
 *
 * Applies state transition with admissibility verification.
 *
 * @param {Object} args - Validated args
 * @param {Object} [context={}] - Execution context with t_ns for determinism
 * @returns {Promise<Object>} Application result
 */
declare function applyDelta(args: any, context?: any): Promise<any>;
declare const ApplyArgsSchema: z.ZodObject<{
    id: z.ZodString;
    force: z.ZodDefault<z.ZodOptional<z.ZodBoolean>>;
    dryRun: z.ZodDefault<z.ZodOptional<z.ZodBoolean>>;
}, z.core.$strip>;
/**
 * Verify delta application.
 *
 * Confirms delta was correctly applied to state.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Verification result
 */
declare function verifyDelta(args: any): Promise<any>;
declare const VerifyArgsSchema: z.ZodObject<{
    id: z.ZodString;
    against: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Export delta.
 *
 * Export delta in specified format.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Export result
 */
declare function exportDelta(args: any): Promise<any>;
declare const ExportArgsSchema: z.ZodObject<{
    id: z.ZodString;
    format: z.ZodDefault<z.ZodOptional<z.ZodEnum<{
        json: "json";
        rdf: "rdf";
        yaml: "yaml";
        patch: "patch";
    }>>>;
}, z.core.$strip>;
import { z } from 'zod';
