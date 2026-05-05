/**
 * Schema for testGrammarDeterminism
 */
export const testGrammarDeterminismParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const testGrammarDeterminismReturnSchema: z.ZodUnknown;
export namespace testGrammarDeterminismSchema {
    export { testGrammarDeterminismParamsSchema as params };
    export { testGrammarDeterminismReturnSchema as returns };
}
declare namespace _default {
    export { testGrammarDeterminismSchema as testGrammarDeterminism };
}
export default _default;
import { z } from 'zod';
