/**
 * Schema for buildV6Spine
 */
export const buildV6SpineParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const buildV6SpineReturnSchema: z.ZodUnknown;
export namespace buildV6SpineSchema {
    export { buildV6SpineParamsSchema as params };
    export { buildV6SpineReturnSchema as returns };
}
/**
 * Schema for generateSpineReport
 */
export const generateSpineReportParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const generateSpineReportReturnSchema: z.ZodString;
export namespace generateSpineReportSchema {
    export { generateSpineReportParamsSchema as params };
    export { generateSpineReportReturnSchema as returns };
}
/**
 * Schema for getNounVerbMatrix
 */
export const getNounVerbMatrixParamsSchema: z.ZodTuple<[], null>;
export const getNounVerbMatrixReturnSchema: z.ZodUnknown;
export namespace getNounVerbMatrixSchema {
    export { getNounVerbMatrixParamsSchema as params };
    export { getNounVerbMatrixReturnSchema as returns };
}
/**
 * Schema for wrapWithReceiptValidation
 */
export const wrapWithReceiptValidationParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString, z.ZodUnknown], null>;
export const wrapWithReceiptValidationReturnSchema: z.ZodUnknown;
export namespace wrapWithReceiptValidationSchema {
    export { wrapWithReceiptValidationParamsSchema as params };
    export { wrapWithReceiptValidationReturnSchema as returns };
}
/**
 * Schema for createV6Extension
 */
export const createV6ExtensionParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodUnknown], null>;
export const createV6ExtensionReturnSchema: z.ZodUnknown;
export namespace createV6ExtensionSchema {
    export { createV6ExtensionParamsSchema as params };
    export { createV6ExtensionReturnSchema as returns };
}
declare namespace _default {
    export { buildV6SpineSchema as buildV6Spine };
    export { generateSpineReportSchema as generateSpineReport };
    export { getNounVerbMatrixSchema as getNounVerbMatrix };
    export { wrapWithReceiptValidationSchema as wrapWithReceiptValidation };
    export { createV6ExtensionSchema as createV6Extension };
}
export default _default;
import { z } from 'zod';
