/**
 * Schema for thesisBuild
 */
export const thesisBuildParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodUnknown>], null>;
export const thesisBuildReturnSchema: z.ZodUnknown;
export namespace thesisBuildSchema {
    export { thesisBuildParamsSchema as params };
    export { thesisBuildReturnSchema as returns };
}
/**
 * Schema for thesisRender
 */
export const thesisRenderParamsSchema: z.ZodTuple<[z.ZodString], null>;
export const thesisRenderReturnSchema: z.ZodUnknown;
export namespace thesisRenderSchema {
    export { thesisRenderParamsSchema as params };
    export { thesisRenderReturnSchema as returns };
}
/**
 * Schema for thesisExport
 */
export const thesisExportParamsSchema: z.ZodTuple<[z.ZodOptional<z.ZodUnknown>], null>;
export const thesisExportReturnSchema: z.ZodUnknown;
export namespace thesisExportSchema {
    export { thesisExportParamsSchema as params };
    export { thesisExportReturnSchema as returns };
}
declare namespace _default {
    export { thesisBuildSchema as thesisBuild };
    export { thesisRenderSchema as thesisRender };
    export { thesisExportSchema as thesisExport };
}
export default _default;
import { z } from 'zod';
