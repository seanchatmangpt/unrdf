/**
 * Build thesis from package documentation
 * @param {Object} options - Command options
 * @param {string} [options.output='./thesis'] - Output directory
 * @param {string} [options.packages='packages/*'] - Package glob pattern
 * @param {boolean} [options.pdf=false] - Generate PDF
 * @returns {Promise<void>}
 */
export function thesisBuild(options?: {
    output?: string;
    packages?: string;
    pdf?: boolean;
}): Promise<void>;
/**
 * Render documentation from ontology
 * @param {Object} options - Command options
 * @param {string} options.ontology - Path to .ttl ontology
 * @param {string} [options.output='./rendered-docs'] - Output directory
 * @returns {Promise<void>}
 */
export function thesisRender(options: {
    ontology: string;
    output?: string;
}): Promise<void>;
/**
 * Export thesis to specified format
 * @param {Object} options - Command options
 * @param {string} [options.format='pdf'] - Export format (pdf|latex|html)
 * @param {string} [options.input='./thesis'] - Thesis directory
 * @param {string} [options.output] - Output file path
 * @returns {Promise<void>}
 */
export function thesisExport(options?: {
    format?: string;
    input?: string;
    output?: string;
}): Promise<void>;
export namespace thesisExtension {
    let id: string;
    namespace nouns {
        namespace thesis {
            let description: string;
            namespace verbs {
                export namespace render {
                    let description_1: string;
                    export { description_1 as description };
                    export function handler(args: any): Promise<{
                        rendered: boolean;
                    }>;
                    export { RenderArgsSchema as argsSchema };
                    export let meta: {};
                }
                export namespace compile {
                    let description_2: string;
                    export { description_2 as description };
                    export function handler_1(args: any): Promise<{
                        compiled: boolean;
                    }>;
                    export { handler_1 as handler };
                    export { BuildArgsSchema as argsSchema };
                    let meta_1: {};
                    export { meta_1 as meta };
                }
                export namespace validate {
                    let description_3: string;
                    export { description_3 as description };
                    export function handler_2(args: any): Promise<{
                        valid: boolean;
                        path: any;
                    }>;
                    export { handler_2 as handler };
                    export { ValidateArgsSchema as argsSchema };
                    let meta_2: {};
                    export { meta_2 as meta };
                }
                export namespace _export {
                    let description_4: string;
                    export { description_4 as description };
                    export function handler_3(args: any): Promise<{
                        exported: boolean;
                    }>;
                    export { handler_3 as handler };
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
export default thesisExtension;
declare const RenderArgsSchema: z.ZodObject<{
    ontology: z.ZodString;
    output: z.ZodDefault<z.ZodOptional<z.ZodString>>;
}, z.core.$strip>;
declare const BuildArgsSchema: z.ZodObject<{
    output: z.ZodDefault<z.ZodOptional<z.ZodString>>;
    packages: z.ZodDefault<z.ZodOptional<z.ZodString>>;
    pdf: z.ZodDefault<z.ZodOptional<z.ZodBoolean>>;
}, z.core.$strip>;
declare const ValidateArgsSchema: z.ZodObject<{
    input: z.ZodDefault<z.ZodOptional<z.ZodString>>;
}, z.core.$strip>;
declare const ExportArgsSchema: z.ZodObject<{
    format: z.ZodDefault<z.ZodOptional<z.ZodEnum<{
        pdf: "pdf";
        latex: "latex";
        html: "html";
    }>>>;
    input: z.ZodDefault<z.ZodOptional<z.ZodString>>;
    output: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
import { z } from 'zod';
