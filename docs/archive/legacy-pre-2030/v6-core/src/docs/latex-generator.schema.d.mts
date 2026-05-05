/**
 * Schema for generateLatex
 */
export const generateLatexParamsSchema: z.ZodTuple<[z.ZodUnknown, z.ZodString], null>;
export const generateLatexReturnSchema: z.ZodUnknown;
export namespace generateLatexSchema {
    export { generateLatexParamsSchema as params };
    export { generateLatexReturnSchema as returns };
}
/**
 * Schema for compileToPDF
 */
export const compileToPDFParamsSchema: z.ZodTuple<[z.ZodString, z.ZodOptional<z.ZodUnknown>], null>;
export const compileToPDFReturnSchema: z.ZodUnknown;
export namespace compileToPDFSchema {
    export { compileToPDFParamsSchema as params };
    export { compileToPDFReturnSchema as returns };
}
/**
 * LaTeX compiler options schema
 */
export const LatexCompilerOptionsSchema: z.ZodObject<{
    engine: z.ZodDefault<z.ZodEnum<{
        pdflatex: "pdflatex";
        xelatex: "xelatex";
        lualatex: "lualatex";
    }>>;
    passes: z.ZodDefault<z.ZodNumber>;
    shellEscape: z.ZodDefault<z.ZodBoolean>;
    interaction: z.ZodDefault<z.ZodEnum<{
        batchmode: "batchmode";
        nonstopmode: "nonstopmode";
        scrollmode: "scrollmode";
        errorstopmode: "errorstopmode";
    }>>;
    outputFormat: z.ZodDefault<z.ZodEnum<{
        pdf: "pdf";
        dvi: "dvi";
    }>>;
    halt_on_error: z.ZodDefault<z.ZodBoolean>;
    synctex: z.ZodDefault<z.ZodBoolean>;
    timeout: z.ZodDefault<z.ZodNumber>;
}, z.core.$strict>;
/**
 * WASM LaTeX compilation input schema
 */
export const WasmLatexInputSchema: z.ZodObject<{
    source: z.ZodString;
    mainFile: z.ZodDefault<z.ZodString>;
    additionalFiles: z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodString>>;
    options: z.ZodOptional<z.ZodObject<{
        engine: z.ZodDefault<z.ZodEnum<{
            pdflatex: "pdflatex";
            xelatex: "xelatex";
            lualatex: "lualatex";
        }>>;
        passes: z.ZodDefault<z.ZodNumber>;
        shellEscape: z.ZodDefault<z.ZodBoolean>;
        interaction: z.ZodDefault<z.ZodEnum<{
            batchmode: "batchmode";
            nonstopmode: "nonstopmode";
            scrollmode: "scrollmode";
            errorstopmode: "errorstopmode";
        }>>;
        outputFormat: z.ZodDefault<z.ZodEnum<{
            pdf: "pdf";
            dvi: "dvi";
        }>>;
        halt_on_error: z.ZodDefault<z.ZodBoolean>;
        synctex: z.ZodDefault<z.ZodBoolean>;
        timeout: z.ZodDefault<z.ZodNumber>;
    }, z.core.$strict>>;
}, z.core.$strict>;
/**
 * WASM LaTeX compilation output schema
 */
export const WasmLatexOutputSchema: z.ZodObject<{
    success: z.ZodBoolean;
    pdfBytes: z.ZodNullable<z.ZodCustom<Uint8Array<ArrayBuffer>, Uint8Array<ArrayBuffer>>>;
    logOutput: z.ZodString;
    warnings: z.ZodArray<z.ZodString>;
    errors: z.ZodArray<z.ZodString>;
    compilationTime: z.ZodNumber;
    wasmEngineVersion: z.ZodOptional<z.ZodString>;
}, z.core.$strict>;
/**
 * LaTeX file compilation result schema
 */
export const LatexCompilationResultSchema: z.ZodObject<{
    pdfPath: z.ZodString;
    success: z.ZodBoolean;
    logPath: z.ZodOptional<z.ZodString>;
    errors: z.ZodDefault<z.ZodArray<z.ZodString>>;
    warnings: z.ZodDefault<z.ZodArray<z.ZodString>>;
    stats: z.ZodObject<{
        pages: z.ZodOptional<z.ZodNumber>;
        fileSize: z.ZodOptional<z.ZodNumber>;
        compilationTime: z.ZodNumber;
    }, z.core.$strict>;
}, z.core.$strict>;
declare namespace _default {
    export { generateLatexSchema as generateLatex };
    export { compileToPDFSchema as compileToPDF };
}
export default _default;
import { z } from 'zod';
