/**
 * Grammar compile command
 *
 * @param {string} filePath - Path to grammar file
 * @param {Object} [options] - Command options
 * @param {string} [options.type] - Grammar type (sparql|shacl|n3|owl|shex)
 * @param {boolean} [options.strict] - Strict bounds enforcement
 * @returns {Promise<void>}
 */
export function grammarCompileCommand(filePath: string, options?: {
    type?: string;
    strict?: boolean;
}): Promise<void>;
/**
 * Grammar validate command (syntax check only)
 *
 * @param {string} filePath - Path to grammar file
 * @param {Object} [options] - Command options
 * @returns {Promise<void>}
 */
export function grammarValidateCommand(filePath: string, options?: any): Promise<void>;
/**
 * Grammar complexity command (analysis only)
 *
 * @param {string} filePath - Path to grammar file
 * @param {Object} [options] - Command options
 * @returns {Promise<void>}
 */
export function grammarComplexityCommand(filePath: string, options?: any): Promise<void>;
export namespace grammarCommands {
    export { grammarCompileCommand as compile };
    export { grammarValidateCommand as validate };
    export { grammarComplexityCommand as complexity };
}
export namespace grammarExtension {
    let id: string;
    namespace nouns {
        namespace grammar {
            let description: string;
            namespace verbs {
                export namespace compile {
                    let description_1: string;
                    export { description_1 as description };
                    export function handler(args: any): Promise<{
                        compiled: boolean;
                    }>;
                    export { CompileArgsSchema as argsSchema };
                    export let meta: {};
                }
                export namespace validate {
                    let description_2: string;
                    export { description_2 as description };
                    export function handler_1(args: any): Promise<{
                        valid: boolean;
                    }>;
                    export { handler_1 as handler };
                    export { ValidateArgsSchema as argsSchema };
                    let meta_1: {};
                    export { meta_1 as meta };
                }
                export namespace parse {
                    let description_3: string;
                    export { description_3 as description };
                    export function handler_2(args: any): Promise<{
                        parsed: any;
                        grammarType: any;
                        complexity: any;
                    }>;
                    export { handler_2 as handler };
                    export { ParseArgsSchema as argsSchema };
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
                    export let argsSchema: z.ZodObject<{
                        file: z.ZodString;
                        type: z.ZodOptional<z.ZodEnum<{
                            sparql: "sparql";
                            shacl: "shacl";
                            n3: "n3";
                            owl: "owl";
                            shex: "shex";
                        }>>;
                    }, z.core.$strip>;
                    let meta_3: {};
                    export { meta_3 as meta };
                }
                export { _export as export };
            }
        }
    }
    let priority: number;
}
export default grammarExtension;
declare const CompileArgsSchema: z.ZodObject<{
    file: z.ZodString;
    type: z.ZodOptional<z.ZodEnum<{
        sparql: "sparql";
        shacl: "shacl";
        n3: "n3";
        owl: "owl";
        shex: "shex";
    }>>;
    strict: z.ZodDefault<z.ZodOptional<z.ZodBoolean>>;
    output: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
declare const ValidateArgsSchema: z.ZodObject<{
    file: z.ZodString;
    type: z.ZodOptional<z.ZodEnum<{
        sparql: "sparql";
        shacl: "shacl";
        n3: "n3";
        owl: "owl";
        shex: "shex";
    }>>;
}, z.core.$strip>;
declare const ParseArgsSchema: z.ZodObject<{
    file: z.ZodString;
    type: z.ZodOptional<z.ZodEnum<{
        sparql: "sparql";
        shacl: "shacl";
        n3: "n3";
        owl: "owl";
        shex: "shex";
    }>>;
}, z.core.$strip>;
import { z } from 'zod';
