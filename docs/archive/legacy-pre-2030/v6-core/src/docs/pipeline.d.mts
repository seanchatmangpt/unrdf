/**
 * @typedef {Object} PackageDocs
 * @property {string} packageName - Package name
 * @property {string} version - Package version
 * @property {string} dir - Package directory
 * @property {DocFile[]} tutorials - Tutorial files
 * @property {DocFile[]} howtos - How-to files
 * @property {DocFile[]} reference - Reference files
 * @property {DocFile[]} explanation - Explanation files
 */
/**
 * @typedef {Object} DocFile
 * @property {string} path - Absolute file path
 * @property {string} name - File name
 * @property {Object} frontmatter - YAML frontmatter
 * @property {string} content - Markdown content
 * @property {string} hash - SHA256 hash
 */
/**
 * @typedef {Object} DiataxisOutput
 * @property {PackageDocs[]} packages - All package docs
 * @property {string} merkleRoot - Merkle root of all docs
 * @property {string} generatedAt - ISO timestamp
 * @property {Object} stats - Statistics
 */
/**
 * Collect documentation from all packages
 * @param {string[]} packageDirs - Array of package directory paths
 * @param {Object} [options={}] - Collection options
 * @param {string[]} [options.categories=['tutorials','how-to','reference','explanation']] - Categories to collect
 * @returns {Promise<PackageDocs[]>}
 */
export function collectPackageDocs(packageDirs: string[], options?: {
    categories?: string[];
}): Promise<PackageDocs[]>;
/**
 * Generate Diataxis output structure
 * @param {PackageDocs[]} packageDocs - Collected package docs
 * @returns {Promise<DiataxisOutput>}
 */
export function generateDiataxisOutput(packageDocs: PackageDocs[]): Promise<DiataxisOutput>;
/**
 * Emit receipt for pipeline stage
 * @param {string} stage - Pipeline stage name
 * @param {Object} input - Stage input
 * @param {Object} output - Stage output
 * @param {string} [outputPath] - Optional path to write receipt
 * @returns {Promise<Object>} Receipt object
 */
export function emitReceipt(stage: string, input: any, output: any, outputPath?: string): Promise<any>;
export type PackageDocs = {
    /**
     * - Package name
     */
    packageName: string;
    /**
     * - Package version
     */
    version: string;
    /**
     * - Package directory
     */
    dir: string;
    /**
     * - Tutorial files
     */
    tutorials: DocFile[];
    /**
     * - How-to files
     */
    howtos: DocFile[];
    /**
     * - Reference files
     */
    reference: DocFile[];
    /**
     * - Explanation files
     */
    explanation: DocFile[];
};
export type DocFile = {
    /**
     * - Absolute file path
     */
    path: string;
    /**
     * - File name
     */
    name: string;
    /**
     * - YAML frontmatter
     */
    frontmatter: any;
    /**
     * - Markdown content
     */
    content: string;
    /**
     * - SHA256 hash
     */
    hash: string;
};
export type DiataxisOutput = {
    /**
     * - All package docs
     */
    packages: PackageDocs[];
    /**
     * - Merkle root of all docs
     */
    merkleRoot: string;
    /**
     * - ISO timestamp
     */
    generatedAt: string;
    /**
     * - Statistics
     */
    stats: any;
};
