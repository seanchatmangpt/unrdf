/**
 * Schema for createWorkflowAdapter
 */
export const createWorkflowAdapterParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodUnknown>], null>;
export const createWorkflowAdapterReturnSchema: z.ZodUnknown;
export namespace createWorkflowAdapterSchema {
    export { createWorkflowAdapterParamsSchema as params };
    export { createWorkflowAdapterReturnSchema as returns };
}
declare namespace _default {
    export { createWorkflowAdapterSchema as createWorkflowAdapter };
}
export default _default;
import { z } from 'zod';
