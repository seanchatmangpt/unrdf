/**
 * Schema for createReceipt
 */
export const createReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const createReceiptReturnSchema: z.ZodUnknown;
export namespace createReceiptSchema {
    export { createReceiptParamsSchema as params };
    export { createReceiptReturnSchema as returns };
}
/**
 * Schema for verifyReceipt
 */
export const verifyReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const verifyReceiptReturnSchema: z.ZodUnknown;
export namespace verifyReceiptSchema {
    export { verifyReceiptParamsSchema as params };
    export { verifyReceiptReturnSchema as returns };
}
/**
 * Schema for verifyChainLink
 */
export const verifyChainLinkParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown], null>;
export const verifyChainLinkReturnSchema: z.ZodUnknown;
export namespace verifyChainLinkSchema {
    export { verifyChainLinkParamsSchema as params };
    export { verifyChainLinkReturnSchema as returns };
}
declare namespace _default {
    export { createReceiptSchema as createReceipt };
    export { verifyReceiptSchema as verifyReceipt };
    export { verifyChainLinkSchema as verifyChainLink };
}
export default _default;
import { z } from 'zod';
