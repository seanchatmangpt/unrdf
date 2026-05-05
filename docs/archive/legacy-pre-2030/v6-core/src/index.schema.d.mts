/**
 * Schema for getV6Status
 */
export const getV6StatusParamsSchema: z.ZodTuple<[], null>;
export const getV6StatusReturnSchema: z.ZodUnknown;
export namespace getV6StatusSchema {
    export { getV6StatusParamsSchema as params };
    export { getV6StatusReturnSchema as returns };
}
/**
 * Schema for isFeatureEnabled
 */
export const isFeatureEnabledParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const isFeatureEnabledReturnSchema: z.ZodBoolean;
export namespace isFeatureEnabledSchema {
    export { isFeatureEnabledParamsSchema as params };
    export { isFeatureEnabledReturnSchema as returns };
}
declare namespace _default {
    export { getV6StatusSchema as getV6Status };
    export { isFeatureEnabledSchema as isFeatureEnabled };
}
export default _default;
import { z } from 'zod';
