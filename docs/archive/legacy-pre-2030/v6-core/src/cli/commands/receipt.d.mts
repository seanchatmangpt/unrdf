export namespace receiptExtension {
    let id: string;
    namespace nouns {
        namespace receipt {
            let description: string;
            namespace verbs {
                export namespace verify {
                    let description_1: string;
                    export { description_1 as description };
                    export { verifyReceipt as handler };
                    export { VerifyArgsSchema as argsSchema };
                    export let meta: {};
                }
                export namespace chain {
                    let description_2: string;
                    export { description_2 as description };
                    export { chainReceipt as handler };
                    export { ChainArgsSchema as argsSchema };
                    let meta_1: {};
                    export { meta_1 as meta };
                }
                export namespace anchor {
                    let description_3: string;
                    export { description_3 as description };
                    export { anchorReceipt as handler };
                    export { AnchorArgsSchema as argsSchema };
                    let meta_2: {};
                    export { meta_2 as meta };
                }
                export namespace _export {
                    let description_4: string;
                    export { description_4 as description };
                    export { exportReceipt as handler };
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
export default receiptExtension;
/**
 * Verify receipt or receipt chain.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Verification result
 */
declare function verifyReceipt(args: any): Promise<any>;
/**
 * Argument schemas for receipt commands.
 */
declare const VerifyArgsSchema: z.ZodObject<{
    file: z.ZodString;
    id: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Show receipt chain.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Chain visualization
 */
declare function chainReceipt(args: any): Promise<any>;
declare const ChainArgsSchema: z.ZodObject<{
    file: z.ZodString;
    output: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Anchor merkle root to blockchain.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Anchor result
 */
declare function anchorReceipt(args: any): Promise<any>;
declare const AnchorArgsSchema: z.ZodObject<{
    root: z.ZodString;
    network: z.ZodDefault<z.ZodOptional<z.ZodString>>;
    output: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Export receipt to format.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Export result
 */
declare function exportReceipt(args: any): Promise<any>;
declare const ExportArgsSchema: z.ZodObject<{
    file: z.ZodString;
    format: z.ZodDefault<z.ZodOptional<z.ZodEnum<{
        json: "json";
        xml: "xml";
        yaml: "yaml";
    }>>>;
    output: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
import { z } from 'zod';
