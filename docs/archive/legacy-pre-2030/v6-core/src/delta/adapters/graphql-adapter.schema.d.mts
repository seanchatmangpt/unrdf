/**
 * Schema for createGraphQLAdapter
 */
export const createGraphQLAdapterParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodUnknown>], null>;
export const createGraphQLAdapterReturnSchema: z.ZodUnknown;
export namespace createGraphQLAdapterSchema {
    export { createGraphQLAdapterParamsSchema as params };
    export { createGraphQLAdapterReturnSchema as returns };
}
declare namespace _default {
    export { createGraphQLAdapterSchema as createGraphQLAdapter };
}
export default _default;
import { z } from 'zod';
