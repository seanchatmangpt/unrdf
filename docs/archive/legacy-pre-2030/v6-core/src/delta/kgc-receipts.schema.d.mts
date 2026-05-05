/**
 * Schema for testKGCDeterminism
 */
export const testKGCDeterminismParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const testKGCDeterminismReturnSchema: z.ZodUnknown;
export namespace testKGCDeterminismSchema {
    export { testKGCDeterminismParamsSchema as params };
    export { testKGCDeterminismReturnSchema as returns };
}
declare namespace _default {
    export { testKGCDeterminismSchema as testKGCDeterminism };
}
export default _default;
import { z } from 'zod';
