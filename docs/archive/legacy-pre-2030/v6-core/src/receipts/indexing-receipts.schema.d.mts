/**
 * Schema for testIndexingDeterminism
 */
export const testIndexingDeterminismParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const testIndexingDeterminismReturnSchema: z.ZodUnknown;
export namespace testIndexingDeterminismSchema {
    export { testIndexingDeterminismParamsSchema as params };
    export { testIndexingDeterminismReturnSchema as returns };
}
declare namespace _default {
    export { testIndexingDeterminismSchema as testIndexingDeterminism };
}
export default _default;
import { z } from 'zod';
