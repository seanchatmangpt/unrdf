/**
 * SPARQL Type Generator - Generate TypeScript/Zod types from RDF ontology
 * @module @unrdf/codegen/sparql-type-generator
 * @description
 * Innovation: Query RDF graph at build time to generate type-safe schemas
 *
 * Pattern 1 from Code Generation Research
 */

import { z } from 'zod';
import { createHash } from 'crypto';

const GenerateTypesOptionsSchema = z.object({
  namespace: z.string().default('ex:'),
  outputPath: z.string().optional(),
  includeComments: z.boolean().default(true),
  generateZod: z.boolean().default(true),
  generateTS: z.boolean().default(false),
});

/**
 * XSD datatype to Zod type mapping
 */
const XSD_TO_ZOD = new Map([
  ['http://www.w3.org/2001/XMLSchema#string', 'z.string()'],
  ['http://www.w3.org/2001/XMLSchema#integer', 'z.number().int()'],
  ['http://www.w3.org/2001/XMLSchema#int', 'z.number().int()'],
  ['http://www.w3.org/2001/XMLSchema#decimal', 'z.number()'],
  ['http://www.w3.org/2001/XMLSchema#boolean', 'z.boolean()'],
  ['http://www.w3.org/2001/XMLSchema#dateTime', 'z.string().datetime()'],
  ['http://www.w3.org/2001/XMLSchema#date', 'z.string().date()'],
  ['http://www.w3.org/2001/XMLSchema#anyURI', 'z.string().url()'],
]);

/**
 * Generate TypeScript/Zod types from SPARQL queries
 * @param {Object} store - RDF store with query method
 * @param {Object} options - Generation options
 * @returns {Promise<Object>} Generation result
 */
export async function generateTypesFromSPARQL(store, options = {}) {
  const config = GenerateTypesOptionsSchema.parse(options);

  // Query all classes
  const classQuery = `
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>

    SELECT DISTINCT ?class ?label ?comment WHERE {
      {
        ?class a rdfs:Class .
      } UNION {
        ?class a owl:Class .
      }
      OPTIONAL { ?class rdfs:label ?label }
      OPTIONAL { ?class rdfs:comment ?comment }
      FILTER(!isBlank(?class))
    }
  `;

  const classes = await executeQuery(store, classQuery);
  const typeDefinitions = [];
  const metadata = {
    classCount: 0,
    propertyCount: 0,
    timestamp: new Date().toISOString(),
  };

  for (const classBinding of classes) {
    const classIRI = classBinding.get('class')?.value;
    if (!classIRI) continue;

    const className = extractLocalName(classIRI);
    const comment = classBinding.get('comment')?.value;

    // Query properties for this class
    const propQuery = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

      SELECT ?prop ?propComment ?range ?minCardinality ?maxCardinality WHERE {
        ?prop rdfs:domain <${classIRI}> .
        OPTIONAL { ?prop rdfs:comment ?propComment }
        OPTIONAL { ?prop rdfs:range ?range }
        OPTIONAL { ?prop owl:minCardinality ?minCardinality }
        OPTIONAL { ?prop owl:maxCardinality ?maxCardinality }
      }
    `;

    const properties = await executeQuery(store, propQuery);
    const fields = [];

    for (const propBinding of properties) {
      const propIRI = propBinding.get('prop')?.value;
      if (!propIRI) continue;

      const propName = extractLocalName(propIRI);
      const range = propBinding.get('range')?.value;
      const propComment = propBinding.get('propComment')?.value;
      const minCard = propBinding.get('minCardinality')?.value;

      const zodType = mapXSDToZod(range);
      const isOptional = !minCard || parseInt(minCard) === 0;

      fields.push({
        name: propName,
        type: zodType,
        optional: isOptional,
        comment: propComment,
      });

      metadata.propertyCount++;
    }

    if (config.generateZod) {
      const zodSchema = generateZodObject(className, fields, comment, config);
      typeDefinitions.push(zodSchema);
    }

    metadata.classCount++;
  }

  // Generate file content
  const content = `
/**
 * Auto-generated types from RDF ontology
 * Generated: ${metadata.timestamp}
 * Classes: ${metadata.classCount}
 * Properties: ${metadata.propertyCount}
 *
 * DO NOT EDIT MANUALLY
 */

import { z } from 'zod';

${typeDefinitions.join('\n\n')}

export default {
  ${typeDefinitions.map(def => {
    const name = def.match(/export const (\w+)Schema/)?.[1];
    return name ? `${name}: ${name}Schema` : '';
  }).filter(Boolean).join(',\n  ')}
};
  `.trim();

  const contentHash = createHash('sha256').update(content).digest('hex').substring(0, 16);

  return {
    content,
    metadata: {
      ...metadata,
      hash: contentHash,
      linesGenerated: content.split('\n').length,
    },
    outputPath: config.outputPath,
  };
}

/**
 * Generate Zod object schema definition
 * @param {string} className - Class name
 * @param {Array} fields - Field definitions
 * @param {string} comment - Class comment
 * @param {Object} config - Configuration
 * @returns {string} Zod schema code
 */
function generateZodObject(className, fields, comment, config) {
  const fieldDefs = fields.map(field => {
    const typeStr = field.optional ? `${field.type}.optional()` : field.type;
    const commentStr = config.includeComments && field.comment
      ? ` // ${field.comment}`
      : '';
    return `  ${field.name}: ${typeStr}${commentStr}`;
  });

  const classComment = config.includeComments && comment
    ? `/**\n * ${comment}\n */\n`
    : '';

  return `
${classComment}export const ${className}Schema = z.object({
${fieldDefs.join(',\n')}
});

export type ${className} = z.infer<typeof ${className}Schema>;
  `.trim();
}

/**
 * Map XSD datatype IRI to Zod type
 * @param {string} rangeIRI - Range IRI
 * @returns {string} Zod type string
 */
function mapXSDToZod(rangeIRI) {
  if (!rangeIRI) {
    return 'z.unknown()';
  }

  // Check XSD types
  if (XSD_TO_ZOD.has(rangeIRI)) {
    return XSD_TO_ZOD.get(rangeIRI);
  }

  // Check if it's a reference to another class
  if (rangeIRI.includes('#') || rangeIRI.includes('/')) {
    const className = extractLocalName(rangeIRI);
    return `z.lazy(() => ${className}Schema)`;
  }

  return 'z.unknown()';
}

/**
 * Extract local name from IRI
 * @param {string} iri - Full IRI
 * @returns {string} Local name
 */
function extractLocalName(iri) {
  const match = iri.match(/[#/]([^#/]+)$/);
  if (!match) {
    // Fallback: use last segment
    const parts = iri.split('/');
    return parts[parts.length - 1] || iri;
  }

  // Convert to PascalCase
  const name = match[1];
  return name.charAt(0).toUpperCase() + name.slice(1);
}

/**
 * Execute SPARQL query on store
 * @param {Object} store - RDF store
 * @param {string} query - SPARQL query
 * @returns {Promise<Array>} Query results
 */
async function executeQuery(store, query) {
  try {
    // Support both sync and async query methods
    const results = store.query ? await store.query(query) : [];

    // Convert to array if iterator
    if (results[Symbol.iterator]) {
      return Array.from(results);
    }

    return results;
  } catch (error) {
    console.warn(`Query failed: ${error.message}`);
    return [];
  }
}

/**
 * Generate TypeScript interface (non-Zod)
 * @param {string} className - Class name
 * @param {Array} fields - Field definitions
 * @param {string} comment - Class comment
 * @returns {string} TypeScript interface
 */
export function generateTSInterface(className, fields, comment) {
  const fieldDefs = fields.map(field => {
    const optional = field.optional ? '?' : '';
    const type = zodTypeToTS(field.type);
    return `  ${field.name}${optional}: ${type};`;
  });

  const docComment = comment ? `/**\n * ${comment}\n */\n` : '';

  return `
${docComment}export interface ${className} {
${fieldDefs.join('\n')}
}
  `.trim();
}

/**
 * Convert Zod type to TypeScript type
 * @param {string} zodType - Zod type string
 * @returns {string} TypeScript type
 */
function zodTypeToTS(zodType) {
  const map = {
    'z.string()': 'string',
    'z.number()': 'number',
    'z.number().int()': 'number',
    'z.boolean()': 'boolean',
    'z.unknown()': 'unknown',
    'z.string().datetime()': 'string',
    'z.string().date()': 'string',
    'z.string().url()': 'string',
  };

  return map[zodType.replace('.optional()', '')] || 'unknown';
}

/**
 * Create receipt for type generation
 * @param {Object} result - Generation result
 * @param {Object} store - RDF store
 * @returns {Object} Receipt object
 */
export function createGenerationReceipt(result, store) {
  return {
    version: '6.0.0',
    operation: 'sparql-type-generation',
    timestamp: Date.now(),
    duration: 0, // Set by caller
    args: JSON.stringify({
      storeSize: store.size || 0,
    }),
    result: JSON.stringify({
      classCount: result.metadata.classCount,
      propertyCount: result.metadata.propertyCount,
      hash: result.metadata.hash,
      linesGenerated: result.metadata.linesGenerated,
    }),
    metadata: {
      deterministic: true,
      generator: 'sparql-type-generator',
    },
  };
}

export default generateTypesFromSPARQL;
