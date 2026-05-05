/**
 * Schema for validateVerbs
 */
export const validateVerbsParamsSchema: z.ZodTuple<[], null>;
export const validateVerbsReturnSchema: z.ZodUnknown;
export namespace validateVerbsSchema {
    export { validateVerbsParamsSchema as params };
    export { validateVerbsReturnSchema as returns };
}
/**
 * Schema for getVerb
 */
export const getVerbParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const getVerbReturnSchema: z.ZodUnknown;
export namespace getVerbSchema {
    export { getVerbParamsSchema as params };
    export { getVerbReturnSchema as returns };
}
/**
 * Schema for getVerbsForNoun
 */
export const getVerbsForNounParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const getVerbsForNounReturnSchema: z.ZodUnknown;
export namespace getVerbsForNounSchema {
    export { getVerbsForNounParamsSchema as params };
    export { getVerbsForNounReturnSchema as returns };
}
/**
 * Schema for buildNounVerbMatrix
 */
export const buildNounVerbMatrixParamsSchema: z.ZodTuple<[], null>;
export const buildNounVerbMatrixReturnSchema: z.ZodUnknown;
export namespace buildNounVerbMatrixSchema {
    export { buildNounVerbMatrixParamsSchema as params };
    export { buildNounVerbMatrixReturnSchema as returns };
}
/**
 * Schema for isValidCombination
 */
export const isValidCombinationParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString], null>;
export const isValidCombinationReturnSchema: z.ZodBoolean;
export namespace isValidCombinationSchema {
    export { isValidCombinationParamsSchema as params };
    export { isValidCombinationReturnSchema as returns };
}
declare namespace _default {
    export { validateVerbsSchema as validateVerbs };
    export { getVerbSchema as getVerb };
    export { getVerbsForNounSchema as getVerbsForNoun };
    export { buildNounVerbMatrixSchema as buildNounVerbMatrix };
    export { isValidCombinationSchema as isValidCombination };
}
export default _default;
import { z } from 'zod';
