/**
 * Schema for grammarCompileCommand
 */
export const grammarCompileCommandParamsSchema: z.ZodTuple<[z.ZodString, z.ZodOptional<z.ZodUnknown>], null>;
export const grammarCompileCommandReturnSchema: z.ZodUnknown;
export namespace grammarCompileCommandSchema {
    export { grammarCompileCommandParamsSchema as params };
    export { grammarCompileCommandReturnSchema as returns };
}
/**
 * Schema for grammarValidateCommand
 */
export const grammarValidateCommandParamsSchema: z.ZodTuple<[z.ZodString, z.ZodOptional<z.ZodUnknown>], null>;
export const grammarValidateCommandReturnSchema: z.ZodUnknown;
export namespace grammarValidateCommandSchema {
    export { grammarValidateCommandParamsSchema as params };
    export { grammarValidateCommandReturnSchema as returns };
}
/**
 * Schema for grammarComplexityCommand
 */
export const grammarComplexityCommandParamsSchema: z.ZodTuple<[z.ZodString, z.ZodOptional<z.ZodUnknown>], null>;
export const grammarComplexityCommandReturnSchema: z.ZodUnknown;
export namespace grammarComplexityCommandSchema {
    export { grammarComplexityCommandParamsSchema as params };
    export { grammarComplexityCommandReturnSchema as returns };
}
declare namespace _default {
    export { grammarCompileCommandSchema as grammarCompileCommand };
    export { grammarValidateCommandSchema as grammarValidateCommand };
    export { grammarComplexityCommandSchema as grammarComplexityCommand };
}
export default _default;
import { z } from 'zod';
