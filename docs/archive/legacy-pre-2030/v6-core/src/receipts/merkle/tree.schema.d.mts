/**
 * Schema for buildMerkleTree
 */
export const buildMerkleTreeParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const buildMerkleTreeReturnSchema: z.ZodUnknown;
export namespace buildMerkleTreeSchema {
    export { buildMerkleTreeParamsSchema as params };
    export { buildMerkleTreeReturnSchema as returns };
}
/**
 * Schema for getMerkleRoot
 */
export const getMerkleRootParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const getMerkleRootReturnSchema: z.ZodString;
export namespace getMerkleRootSchema {
    export { getMerkleRootParamsSchema as params };
    export { getMerkleRootReturnSchema as returns };
}
/**
 * Schema for getProofPath
 */
export const getProofPathParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodString, z.ZodUnknown], null>;
export const getProofPathReturnSchema: z.ZodUnknown;
export namespace getProofPathSchema {
    export { getProofPathParamsSchema as params };
    export { getProofPathReturnSchema as returns };
}
/**
 * Schema for verifyInclusion
 */
export const verifyInclusionParamsSchema: z.ZodTuple<[z.ZodString, z.ZodUnknown, z.ZodUnknown], null>;
export const verifyInclusionReturnSchema: z.ZodUnknown;
export namespace verifyInclusionSchema {
    export { verifyInclusionParamsSchema as params };
    export { verifyInclusionReturnSchema as returns };
}
/**
 * Schema for getTreeInfo
 */
export const getTreeInfoParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const getTreeInfoReturnSchema: z.ZodUnknown;
export namespace getTreeInfoSchema {
    export { getTreeInfoParamsSchema as params };
    export { getTreeInfoReturnSchema as returns };
}
declare namespace _default {
    export { buildMerkleTreeSchema as buildMerkleTree };
    export { getMerkleRootSchema as getMerkleRoot };
    export { getProofPathSchema as getProofPath };
    export { verifyInclusionSchema as verifyInclusion };
    export { getTreeInfoSchema as getTreeInfo };
}
export default _default;
import { z } from 'zod';
