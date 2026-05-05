/**
 * Schema for grammarClosurePipeline
 */
export const grammarClosurePipelineParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString, z.ZodUnknown, z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const grammarClosurePipelineReturnSchema: z.ZodUnknown;
export namespace grammarClosurePipelineSchema {
    export { grammarClosurePipelineParamsSchema as params };
    export { grammarClosurePipelineReturnSchema as returns };
}
declare namespace _default {
    export { grammarClosurePipelineSchema as grammarClosurePipeline };
}
export default _default;
import { z } from 'zod';
