/**
 * Schema for checkRuntimeComplexity
 */
export const checkRuntimeComplexityParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const checkRuntimeComplexityReturnSchema: z.ZodUnknown;
export namespace checkRuntimeComplexitySchema {
    export { checkRuntimeComplexityParamsSchema as params };
    export { checkRuntimeComplexityReturnSchema as returns };
}
/**
 * Schema for wrapWithTimeout
 */
export const wrapWithTimeoutParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodNumber, z.ZodOptional<z.ZodUnknown>], null>;
export const wrapWithTimeoutReturnSchema: z.ZodUnknown;
export namespace wrapWithTimeoutSchema {
    export { wrapWithTimeoutParamsSchema as params };
    export { wrapWithTimeoutReturnSchema as returns };
}
/**
 * Schema for emitDenialReceipt
 */
export const emitDenialReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodString, z.ZodOptional<z.ZodUnknown>], null>;
export const emitDenialReceiptReturnSchema: z.ZodUnknown;
export namespace emitDenialReceiptSchema {
    export { emitDenialReceiptParamsSchema as params };
    export { emitDenialReceiptReturnSchema as returns };
}
/**
 * Schema for executeWithGate
 */
export const executeWithGateParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown, z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const executeWithGateReturnSchema: z.ZodUnknown;
export namespace executeWithGateSchema {
    export { executeWithGateParamsSchema as params };
    export { executeWithGateReturnSchema as returns };
}
/**
 * Schema for getRuntimeBounds
 */
export const getRuntimeBoundsParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const getRuntimeBoundsReturnSchema: z.ZodUnknown;
export namespace getRuntimeBoundsSchema {
    export { getRuntimeBoundsParamsSchema as params };
    export { getRuntimeBoundsReturnSchema as returns };
}
declare namespace _default {
    export { checkRuntimeComplexitySchema as checkRuntimeComplexity };
    export { wrapWithTimeoutSchema as wrapWithTimeout };
    export { emitDenialReceiptSchema as emitDenialReceipt };
    export { executeWithGateSchema as executeWithGate };
    export { getRuntimeBoundsSchema as getRuntimeBounds };
}
export default _default;
import { z } from 'zod';
