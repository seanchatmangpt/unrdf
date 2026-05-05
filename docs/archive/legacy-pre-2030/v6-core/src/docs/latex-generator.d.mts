/**
 * @typedef {Object} LatexOutput
 * @property {string} mainFile - Path to main .tex file
 * @property {string[]} chapterFiles - Paths to chapter files
 * @property {string} hash - SHA256 of all LaTeX content
 * @property {Object} lockfile - Deterministic lockfile
 */
/**
 * Generate LaTeX from Diataxis documentation
 * @param {import('./pipeline.mjs').DiataxisOutput} diataxisDocs - Diataxis docs
 * @param {string} outputDir - Output directory for LaTeX files
 * @returns {Promise<LatexOutput>}
 */
export function generateLatex(diataxisDocs: import("./pipeline.mjs").DiataxisOutput, outputDir: string): Promise<LatexOutput>;
/**
 * Compile LaTeX source to PDF using WASM engine
 * NOTE: Requires @swiftlatex/pdftex.wasm or texlive.js package (not included)
 * @param {string} source - LaTeX source code
 * @param {Object} [options] - Compilation options
 * @param {string} [options.engine='pdflatex'] - LaTeX engine to use
 * @param {number} [options.passes=2] - Number of compilation passes
 * @param {boolean} [options.shellEscape=false] - Enable shell escape
 * @param {string} [options.interaction='nonstopmode'] - Interaction mode
 * @param {number} [options.timeout=30000] - Compilation timeout in ms
 * @returns {Promise<Object>} WASM compilation result with integration instructions
 * @throws {Error} If validation fails
 * @example
 * const result = await compileLatexWithWasm('\\documentclass{article}...');
 * console.log(result.logOutput); // Shows integration instructions
 */
export function compileLatexWithWasm(source: string, options?: {
    engine?: string;
    passes?: number;
    shellEscape?: boolean;
    interaction?: string;
    timeout?: number;
}): Promise<any>;
/**
 * Compile LaTeX file to PDF
 * NOTE: Requires external LaTeX compiler or @swiftlatex/pdftex.wasm package
 * Returns expected PDF path. For actual compilation, use external tools like pdflatex/xelatex
 * @param {string} texFile - Path to .tex file
 * @param {Object} [options={}] - Compilation options
 * @param {string} [options.engine='pdflatex'] - LaTeX engine to use
 * @param {number} [options.passes=2] - Number of compilation passes
 * @param {number} [options.timeout=30000] - Compilation timeout in ms
 * @returns {Promise<string>} Path where PDF would be generated
 * @example
 * const pdfPath = await compileToPDF('./thesis.tex', { engine: 'xelatex' });
 * // Use: pdflatex thesis.tex (external tool) to create actual PDF
 */
export function compileToPDF(texFile: string, options?: {
    engine?: string;
    passes?: number;
    timeout?: number;
}): Promise<string>;
/**
 * Initialize WASM LaTeX engine
 * NOTE: Requires @swiftlatex/pdftex.wasm package (not included)
 * @param {Object} [config] - Engine configuration
 * @param {string} [config.enginePath] - Path to WASM engine binary
 * @param {number} [config.memorySize=268435456] - WASM memory size in bytes (default 256MB)
 * @returns {Promise<Object>} Engine instance with integration instructions
 * @example
 * const engine = await initWasmLatexEngine({ memorySize: 512 * 1024 * 1024 });
 * console.log(engine.version); // Shows 'not-integrated' until package installed
 */
export function initWasmLatexEngine(config?: {
    enginePath?: string;
    memorySize?: number;
}): Promise<any>;
export type LatexOutput = {
    /**
     * - Path to main .tex file
     */
    mainFile: string;
    /**
     * - Paths to chapter files
     */
    chapterFiles: string[];
    /**
     * - SHA256 of all LaTeX content
     */
    hash: string;
    /**
     * - Deterministic lockfile
     */
    lockfile: any;
};
