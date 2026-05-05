/**
 * Schema for withReceipt
 */
export const withReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodString>], null>;
export const withReceiptReturnSchema: z.ZodUnknown;
export namespace withReceiptSchema {
    export { withReceiptParamsSchema as params };
    export { withReceiptReturnSchema as returns };
}
/**
 * Schema for createReceiptChain
 */
export const createReceiptChainParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const createReceiptChainReturnSchema: z.ZodUnknown;
export namespace createReceiptChainSchema {
    export { createReceiptChainParamsSchema as params };
    export { createReceiptChainReturnSchema as returns };
}
/**
 * Schema for verifyIdempotency
 */
export const verifyIdempotencyParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const verifyIdempotencyReturnSchema: z.ZodUnknown;
export namespace verifyIdempotencySchema {
    export { verifyIdempotencyParamsSchema as params };
    export { verifyIdempotencyReturnSchema as returns };
}
declare namespace _default {
    export { withReceiptSchema as withReceipt };
    export { createReceiptChainSchema as createReceiptChain };
    export { verifyIdempotencySchema as verifyIdempotency };
}
export default _default;
import { z } from 'zod';
