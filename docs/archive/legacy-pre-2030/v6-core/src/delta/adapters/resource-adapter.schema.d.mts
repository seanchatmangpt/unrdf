/**
 * Schema for createResourceAdapter
 */
export const createResourceAdapterParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodUnknown>], null>;
export const createResourceAdapterReturnSchema: z.ZodUnknown;
export namespace createResourceAdapterSchema {
    export { createResourceAdapterParamsSchema as params };
    export { createResourceAdapterReturnSchema as returns };
}
declare namespace _default {
    export { createResourceAdapterSchema as createResourceAdapter };
}
export default _default;
import { z } from 'zod';
