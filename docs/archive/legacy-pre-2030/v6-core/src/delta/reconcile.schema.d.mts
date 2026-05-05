/**
 * Schema for reconcile
 */
export const reconcileParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const reconcileReturnSchema: z.ZodUnknown;
export namespace reconcileSchema {
    export { reconcileParamsSchema as params };
    export { reconcileReturnSchema as returns };
}
/**
 * Schema for defaultConflictResolver
 */
export const defaultConflictResolverParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const defaultConflictResolverReturnSchema: z.ZodString;
export namespace defaultConflictResolverSchema {
    export { defaultConflictResolverParamsSchema as params };
    export { defaultConflictResolverReturnSchema as returns };
}
/**
 * Schema for currentWinsResolver
 */
export const currentWinsResolverParamsSchema: z.ZodTuple<[], null>;
export const currentWinsResolverReturnSchema: z.ZodUnknown;
export namespace currentWinsResolverSchema {
    export { currentWinsResolverParamsSchema as params };
    export { currentWinsResolverReturnSchema as returns };
}
/**
 * Schema for strictResolver
 */
export const strictResolverParamsSchema: z.ZodTuple<[], null>;
export const strictResolverReturnSchema: z.ZodUnknown;
export namespace strictResolverSchema {
    export { strictResolverParamsSchema as params };
    export { strictResolverReturnSchema as returns };
}
/**
 * Schema for customResolver
 */
export const customResolverParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const customResolverReturnSchema: z.ZodUnknown;
export namespace customResolverSchema {
    export { customResolverParamsSchema as params };
    export { customResolverReturnSchema as returns };
}
declare namespace _default {
    export { reconcileSchema as reconcile };
    export { defaultConflictResolverSchema as defaultConflictResolver };
    export { currentWinsResolverSchema as currentWinsResolver };
    export { strictResolverSchema as strictResolver };
    export { customResolverSchema as customResolver };
}
export default _default;
import { z } from 'zod';
