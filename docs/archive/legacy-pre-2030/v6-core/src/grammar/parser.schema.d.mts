/**
 * Schema for parseGrammar
 */
export const parseGrammarParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString], null>;
export const parseGrammarReturnSchema: z.ZodUnknown;
export namespace parseGrammarSchema {
    export { parseGrammarParamsSchema as params };
    export { parseGrammarReturnSchema as returns };
}
/**
 * Schema for getComplexityBounds
 */
export const getComplexityBoundsParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const getComplexityBoundsReturnSchema: z.ZodUnknown;
export namespace getComplexityBoundsSchema {
    export { getComplexityBoundsParamsSchema as params };
    export { getComplexityBoundsReturnSchema as returns };
}
declare namespace _default {
    export { parseGrammarSchema as parseGrammar };
    export { getComplexityBoundsSchema as getComplexityBounds };
}
export default _default;
import { z } from 'zod';
