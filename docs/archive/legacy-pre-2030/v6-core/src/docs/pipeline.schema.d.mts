/**
 * Schema for collectPackageDocs
 */
export const collectPackageDocsParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodOptional<z.ZodUnknown>], null>;
export const collectPackageDocsReturnSchema: z.ZodUnknown;
export namespace collectPackageDocsSchema {
    export { collectPackageDocsParamsSchema as params };
    export { collectPackageDocsReturnSchema as returns };
}
/**
 * Schema for generateDiataxisOutput
 */
export const generateDiataxisOutputParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const generateDiataxisOutputReturnSchema: z.ZodUnknown;
export namespace generateDiataxisOutputSchema {
    export { generateDiataxisOutputParamsSchema as params };
    export { generateDiataxisOutputReturnSchema as returns };
}
/**
 * Schema for emitReceipt
 */
export const emitReceiptParamsSchema: z.ZodTuple<[z.ZodString, z.ZodUnknown, z.ZodUnknown, z.ZodUnknown], null>;
export const emitReceiptReturnSchema: z.ZodUnknown;
export namespace emitReceiptSchema {
    export { emitReceiptParamsSchema as params };
    export { emitReceiptReturnSchema as returns };
}
declare namespace _default {
    export { collectPackageDocsSchema as collectPackageDocs };
    export { generateDiataxisOutputSchema as generateDiataxisOutput };
    export { emitReceiptSchema as emitReceipt };
}
export default _default;
import { z } from 'zod';
