/**
 * Schema for generateUUID
 */
export const generateUUIDParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodUnknown>], null>;
export const generateUUIDReturnSchema: z.ZodString;
export namespace generateUUIDSchema {
    export { generateUUIDParamsSchema as params };
    export { generateUUIDReturnSchema as returns };
}
/**
 * Schema for deterministicSerialize
 */
export const deterministicSerializeParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const deterministicSerializeReturnSchema: z.ZodString;
export namespace deterministicSerializeSchema {
    export { deterministicSerializeParamsSchema as params };
    export { deterministicSerializeReturnSchema as returns };
}
/**
 * Schema for computeBlake3
 */
export const computeBlake3ParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const computeBlake3ReturnSchema: z.ZodUnknown;
export namespace computeBlake3Schema {
    export { computeBlake3ParamsSchema as params };
    export { computeBlake3ReturnSchema as returns };
}
/**
 * Schema for computeChainHash
 */
export const computeChainHashParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodString], null>;
export const computeChainHashReturnSchema: z.ZodUnknown;
export namespace computeChainHashSchema {
    export { computeChainHashParamsSchema as params };
    export { computeChainHashReturnSchema as returns };
}
/**
 * Schema for verifyBaseReceipt
 */
export const verifyBaseReceiptParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const verifyBaseReceiptReturnSchema: z.ZodUnknown;
export namespace verifyBaseReceiptSchema {
    export { verifyBaseReceiptParamsSchema as params };
    export { verifyBaseReceiptReturnSchema as returns };
}
declare namespace _default {
    export { generateUUIDSchema as generateUUID };
    export { deterministicSerializeSchema as deterministicSerialize };
    export { computeBlake3Schema as computeBlake3 };
    export { computeChainHashSchema as computeChainHash };
    export { verifyBaseReceiptSchema as verifyBaseReceipt };
}
export default _default;
import { z } from 'zod';
