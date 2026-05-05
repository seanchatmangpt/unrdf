/**
 * Schema for anchorToChain
 */
export const anchorToChainParamsSchema: z.ZodTuple<[z.ZodString, z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const anchorToChainReturnSchema: z.ZodUnknown;
export namespace anchorToChainSchema {
    export { anchorToChainParamsSchema as params };
    export { anchorToChainReturnSchema as returns };
}
/**
 * Schema for verifyAnchor
 */
export const verifyAnchorParamsSchema: z.ZodTuple<[z.ZodString, z.ZodUnknown], null>;
export const verifyAnchorReturnSchema: z.ZodUnknown;
export namespace verifyAnchorSchema {
    export { verifyAnchorParamsSchema as params };
    export { verifyAnchorReturnSchema as returns };
}
/**
 * Schema for createAnchorReceipt
 */
export const createAnchorReceiptParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString, z.ZodOptional<z.ZodUnknown>, z.ZodOptional<z.ZodUnknown>], null>;
export const createAnchorReceiptReturnSchema: z.ZodUnknown;
export namespace createAnchorReceiptSchema {
    export { createAnchorReceiptParamsSchema as params };
    export { createAnchorReceiptReturnSchema as returns };
}
declare namespace _default {
    export { anchorToChainSchema as anchorToChain };
    export { verifyAnchorSchema as verifyAnchor };
    export { createAnchorReceiptSchema as createAnchorReceipt };
}
export default _default;
import { z } from 'zod';
