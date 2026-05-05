/**
 * Schema for validateNouns
 */
export const validateNounsParamsSchema: z.ZodTuple<[], null>;
export const validateNounsReturnSchema: z.ZodUnknown;
export namespace validateNounsSchema {
    export { validateNounsParamsSchema as params };
    export { validateNounsReturnSchema as returns };
}
/**
 * Schema for getNoun
 */
export const getNounParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const getNounReturnSchema: z.ZodUnknown;
export namespace getNounSchema {
    export { getNounParamsSchema as params };
    export { getNounReturnSchema as returns };
}
/**
 * Schema for getNounNames
 */
export const getNounNamesParamsSchema: z.ZodTuple<[], null>;
export const getNounNamesReturnSchema: z.ZodUnknown;
export namespace getNounNamesSchema {
    export { getNounNamesParamsSchema as params };
    export { getNounNamesReturnSchema as returns };
}
/**
 * Schema for buildNounPackageMap
 */
export const buildNounPackageMapParamsSchema: z.ZodTuple<[], null>;
export const buildNounPackageMapReturnSchema: z.ZodUnknown;
export namespace buildNounPackageMapSchema {
    export { buildNounPackageMapParamsSchema as params };
    export { buildNounPackageMapReturnSchema as returns };
}
declare namespace _default {
    export { validateNounsSchema as validateNouns };
    export { getNounSchema as getNoun };
    export { getNounNamesSchema as getNounNames };
    export { buildNounPackageMapSchema as buildNounPackageMap };
}
export default _default;
import { z } from 'zod';
