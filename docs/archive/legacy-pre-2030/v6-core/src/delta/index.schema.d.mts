/**
 * Schema for createDeltaSystem
 */
export const createDeltaSystemParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodUnknown>], null>;
export const createDeltaSystemReturnSchema: z.ZodUnknown;
export namespace createDeltaSystemSchema {
    export { createDeltaSystemParamsSchema as params };
    export { createDeltaSystemReturnSchema as returns };
}
/**
 * Schema for createDelta
 */
export const createDeltaParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString, z.ZodString, z.ZodString, z.ZodOptional<z.ZodUnknown>], null>;
export const createDeltaReturnSchema: z.ZodUnknown;
export namespace createDeltaSchema {
    export { createDeltaParamsSchema as params };
    export { createDeltaReturnSchema as returns };
}
declare namespace _default {
    export { createDeltaSystemSchema as createDeltaSystem };
    export { createDeltaSchema as createDelta };
}
export default _default;
import { z } from 'zod';
