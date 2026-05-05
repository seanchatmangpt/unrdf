/**
 * Schema for buildThesis
 */
export const buildThesisParamsSchema: z.ZodTuple<[z.ZodUnknown], null>;
export const buildThesisReturnSchema: z.ZodUnknown;
export namespace buildThesisSchema {
    export { buildThesisParamsSchema as params };
    export { buildThesisReturnSchema as returns };
}
/**
 * Schema for renderFromOntology
 */
export const renderFromOntologyParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString], null>;
export const renderFromOntologyReturnSchema: z.ZodUnknown;
export namespace renderFromOntologySchema {
    export { renderFromOntologyParamsSchema as params };
    export { renderFromOntologyReturnSchema as returns };
}
/**
 * Schema for exportThesis
 */
export const exportThesisParamsSchema: z.ZodTuple<[z.ZodString, z.ZodString, z.ZodString], null>;
export const exportThesisReturnSchema: z.ZodUnknown;
export namespace exportThesisSchema {
    export { exportThesisParamsSchema as params };
    export { exportThesisReturnSchema as returns };
}
/**
 * SPARQL CONSTRUCT query schema
 */
export const SparqlConstructQuerySchema: z.ZodObject<{
    query: z.ZodString;
    prefixes: z.ZodDefault<z.ZodRecord<z.ZodString, z.ZodString>>;
    outputFormat: z.ZodDefault<z.ZodEnum<{
        turtle: "turtle";
        ntriples: "ntriples";
        jsonld: "jsonld";
        rdfxml: "rdfxml";
    }>>;
}, z.core.$strict>;
/**
 * Ontology rendering configuration schema
 */
export const OntologyRenderConfigSchema: z.ZodObject<{
    ontologyPath: z.ZodString;
    outputDir: z.ZodString;
    queries: z.ZodOptional<z.ZodArray<z.ZodObject<{
        query: z.ZodString;
        prefixes: z.ZodDefault<z.ZodRecord<z.ZodString, z.ZodString>>;
        outputFormat: z.ZodDefault<z.ZodEnum<{
            turtle: "turtle";
            ntriples: "ntriples";
            jsonld: "jsonld";
            rdfxml: "rdfxml";
        }>>;
    }, z.core.$strict>>>;
    templateDir: z.ZodOptional<z.ZodString>;
    generateDiataxis: z.ZodDefault<z.ZodBoolean>;
}, z.core.$strict>;
/**
 * Ontology rendering result schema
 */
export const OntologyRenderResultSchema: z.ZodObject<{
    status: z.ZodEnum<{
        error: "error";
        success: "success";
        partial: "partial";
        not_implemented: "not_implemented";
    }>;
    ontologyPath: z.ZodString;
    outputDir: z.ZodString;
    generatedDocs: z.ZodDefault<z.ZodArray<z.ZodObject<{
        path: z.ZodString;
        type: z.ZodEnum<{
            tutorial: "tutorial";
            howto: "howto";
            reference: "reference";
            explanation: "explanation";
        }>;
        sourceQuery: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>>;
    errors: z.ZodDefault<z.ZodArray<z.ZodString>>;
    warnings: z.ZodDefault<z.ZodArray<z.ZodString>>;
}, z.core.$strict>;
/**
 * Documentation template schema
 */
export const DocTemplateSchema: z.ZodObject<{
    type: z.ZodEnum<{
        tutorial: "tutorial";
        howto: "howto";
        reference: "reference";
        explanation: "explanation";
    }>;
    templatePath: z.ZodOptional<z.ZodString>;
    queryBinding: z.ZodRecord<z.ZodString, z.ZodString>;
}, z.core.$strict>;
declare namespace _default {
    export { buildThesisSchema as buildThesis };
    export { renderFromOntologySchema as renderFromOntology };
    export { exportThesisSchema as exportThesis };
}
export default _default;
import { z } from 'zod';
