/**
 * Schema for receiptCreate
 */
export const receiptCreateParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const receiptCreateReturnSchema: z.ZodUnknown;
export namespace receiptCreateSchema {
    export { receiptCreateParamsSchema as params };
    export { receiptCreateReturnSchema as returns };
}
/**
 * Schema for receiptVerify
 */
export const receiptVerifyParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const receiptVerifyReturnSchema: z.ZodUnknown;
export namespace receiptVerifySchema {
    export { receiptVerifyParamsSchema as params };
    export { receiptVerifyReturnSchema as returns };
}
/**
 * Schema for deltaPropose
 */
export const deltaProposeParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const deltaProposeReturnSchema: z.ZodUnknown;
export namespace deltaProposeSchema {
    export { deltaProposeParamsSchema as params };
    export { deltaProposeReturnSchema as returns };
}
/**
 * Schema for deltaApply
 */
export const deltaApplyParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const deltaApplyReturnSchema: z.ZodUnknown;
export namespace deltaApplySchema {
    export { deltaApplyParamsSchema as params };
    export { deltaApplyReturnSchema as returns };
}
declare namespace _default {
    export { receiptCreateSchema as receiptCreate };
    export { receiptVerifySchema as receiptVerify };
    export { deltaProposeSchema as deltaPropose };
    export { deltaApplySchema as deltaApply };
}
export default _default;
import { z } from 'zod';
