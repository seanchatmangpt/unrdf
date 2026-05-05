/**
 * Schema for verifyChain
 */
export const verifyChainParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const verifyChainReturnSchema: z.ZodUnknown;
export namespace verifyChainSchema {
    export { verifyChainParamsSchema as params };
    export { verifyChainReturnSchema as returns };
}
/**
 * Schema for findTamperedReceipts
 */
export const findTamperedReceiptsParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const findTamperedReceiptsReturnSchema: z.ZodUnknown;
export namespace findTamperedReceiptsSchema {
    export { findTamperedReceiptsParamsSchema as params };
    export { findTamperedReceiptsReturnSchema as returns };
}
/**
 * Schema for reconstructChainState
 */
export const reconstructChainStateParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const reconstructChainStateReturnSchema: z.ZodUnknown;
export namespace reconstructChainStateSchema {
    export { reconstructChainStateParamsSchema as params };
    export { reconstructChainStateReturnSchema as returns };
}
declare namespace _default {
    export { verifyChainSchema as verifyChain };
    export { findTamperedReceiptsSchema as findTamperedReceipts };
    export { reconstructChainStateSchema as reconstructChainState };
}
export default _default;
import { z } from 'zod';
