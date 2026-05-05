/**
 * Schema for compileGrammar
 */
export const compileGrammarParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const compileGrammarReturnSchema: z.ZodUnknown;
export namespace compileGrammarSchema {
    export { compileGrammarParamsSchema as params };
    export { compileGrammarReturnSchema as returns };
}
/**
 * Schema for rejectIfTooComplex
 */
export const rejectIfTooComplexParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const rejectIfTooComplexReturnSchema: z.ZodUnknown;
export namespace rejectIfTooComplexSchema {
    export { rejectIfTooComplexParamsSchema as params };
    export { rejectIfTooComplexReturnSchema as returns };
}
/**
 * Schema for emitDenialReceipt
 */
export const emitDenialReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown], null>;
export const emitDenialReceiptReturnSchema: z.ZodUnknown;
export namespace emitDenialReceiptSchema {
    export { emitDenialReceiptParamsSchema as params };
    export { emitDenialReceiptReturnSchema as returns };
}
/**
 * Schema for emitCompileReceipt
 */
export const emitCompileReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown, z.ZodString], null>;
export const emitCompileReceiptReturnSchema: z.ZodUnknown;
export namespace emitCompileReceiptSchema {
    export { emitCompileReceiptParamsSchema as params };
    export { emitCompileReceiptReturnSchema as returns };
}
declare namespace _default {
    export { compileGrammarSchema as compileGrammar };
    export { rejectIfTooComplexSchema as rejectIfTooComplex };
    export { emitDenialReceiptSchema as emitDenialReceipt };
    export { emitCompileReceiptSchema as emitCompileReceipt };
}
export default _default;
import { z } from 'zod';
