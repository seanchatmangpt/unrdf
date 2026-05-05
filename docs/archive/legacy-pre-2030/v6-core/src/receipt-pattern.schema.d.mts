/**
 * Schema for blake3Hash
 */
export const blake3HashParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const blake3HashReturnSchema: z.ZodString;
export namespace blake3HashSchema {
    export { blake3HashParamsSchema as params };
    export { blake3HashReturnSchema as returns };
}
/**
 * Schema for canonicalize
 */
export const canonicalizeParamsSchema: z.ZodTuple<[z.ZodAny], null>;
export const canonicalizeReturnSchema: z.ZodString;
export namespace canonicalizeSchema {
    export { canonicalizeParamsSchema as params };
    export { canonicalizeReturnSchema as returns };
}
/**
 * Schema for deterministicUUID
 */
export const deterministicUUIDParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const deterministicUUIDReturnSchema: z.ZodString;
export namespace deterministicUUIDSchema {
    export { deterministicUUIDParamsSchema as params };
    export { deterministicUUIDReturnSchema as returns };
}
/**
 * Schema for withReceipt
 */
export const withReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const withReceiptReturnSchema: z.ZodUnknown;
export namespace withReceiptSchema {
    export { withReceiptParamsSchema as params };
    export { withReceiptReturnSchema as returns };
}
/**
 * Schema for createContext
 */
export const createContextParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodString>], null>;
export const createContextReturnSchema: z.ZodUnknown;
export namespace createContextSchema {
    export { createContextParamsSchema as params };
    export { createContextReturnSchema as returns };
}
/**
 * Schema for chainReceipts
 */
export const chainReceiptsParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown], null>;
export const chainReceiptsReturnSchema: z.ZodUnknown;
export namespace chainReceiptsSchema {
    export { chainReceiptsParamsSchema as params };
    export { chainReceiptsReturnSchema as returns };
}
/**
 * Schema for verifyReceiptChain
 */
export const verifyReceiptChainParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const verifyReceiptChainReturnSchema: z.ZodUnknown;
export namespace verifyReceiptChainSchema {
    export { verifyReceiptChainParamsSchema as params };
    export { verifyReceiptChainReturnSchema as returns };
}
/**
 * Schema for compose
 */
export const composeParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const composeReturnSchema: z.ZodUnknown;
export namespace composeSchema {
    export { composeParamsSchema as params };
    export { composeReturnSchema as returns };
}
declare namespace _default {
    export { blake3HashSchema as blake3Hash };
    export { canonicalizeSchema as canonicalize };
    export { deterministicUUIDSchema as deterministicUUID };
    export { withReceiptSchema as withReceipt };
    export { createContextSchema as createContext };
    export { chainReceiptsSchema as chainReceipts };
    export { verifyReceiptChainSchema as verifyReceiptChain };
    export { composeSchema as compose };
}
export default _default;
import { z } from 'zod';
