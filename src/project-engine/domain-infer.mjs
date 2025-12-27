/**
 * @file Domain model inference engine - extract entities, fields, relations from code
 * @module project-engine/domain-infer
 */

import { promises as fs } from 'fs';
import path from 'path';
import { Store, DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { z } from 'zod';

const { namedNode, literal } = DataFactory;

/* ========================================================================= */
/* Namespace prefixes                                                        */
/* ========================================================================= */

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  dom: 'http://example.org/unrdf/domain#',
};

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

const StackProfileSchema = z.object({
  hasZod: z.boolean().default(false),
  hasPrisma: z.boolean().default(false),
  hasTypeORM: z.boolean().default(false),
  hasSequelize: z.boolean().default(false),
  hasDrizzle: z.boolean().default(false),
  hasTypescript: z.boolean().default(false),
  sourceRoot: z.string().default('src'),
});

const InferOptionsSchema = z.object({
  fsStore: z.any(),
  stackProfile: StackProfileSchema.optional(),
  baseIri: z.string().default('http://example.org/unrdf/domain#'),
  projectRoot: z.string().optional(),
});

/**
 * @typedef {Object} DomainField
 * @property {string} name
 * @property {string} type
 * @property {boolean} optional
 * @property {boolean} array
 */

/**
 * @typedef {Object} DomainEntity
 * @property {string} name
 * @property {string} source - 'zod' | 'prisma' | 'typescript' | 'typeorm' | 'sequelize'
 * @property {DomainField[]} fields
 * @property {string[]} relations
 */

/**
 * @typedef {Object} DomainInferResult
 * @property {Store} store
 * @property {{entityCount: number, fieldCount: number, relationshipCount: number}} summary
 */

/* ========================================================================= */
/* Type mapping                                                              */
/* ========================================================================= */

/**
 * Map common type names to XSD types
 * @param {string} typeName
 * @returns {string}
 */
function mapToXsdType(typeName) {
  const typeMap = {
    string: `${NS.xsd}string`,
    number: `${NS.xsd}decimal`,
    int: `${NS.xsd}integer`,
    integer: `${NS.xsd}integer`,
    float: `${NS.xsd}float`,
    double: `${NS.xsd}double`,
    boolean: `${NS.xsd}boolean`,
    bool: `${NS.xsd}boolean`,
    date: `${NS.xsd}date`,
    datetime: `${NS.xsd}dateTime`,
    timestamp: `${NS.xsd}dateTime`,
    bigint: `${NS.xsd}integer`,
    json: `${NS.xsd}string`,
    uuid: `${NS.xsd}string`,
    email: `${NS.xsd}string`,
    url: `${NS.xsd}anyURI`,
  };

  const normalized = typeName.toLowerCase().replace(/[\[\]?]/g, '');
  return typeMap[normalized] || `${NS.xsd}string`;
}

/* ========================================================================= */
/* File reading utilities                                                    */
/* ========================================================================= */

/**
 * Read file content from filesystem
 * @param {string} filePath
 * @returns {Promise<string|null>}
 */
async function readFileContent(filePath) {
  try {
    return await fs.readFile(filePath, 'utf-8');
  } catch {
    return null;
  }
}

/**
 * Extract file paths from fsStore
 * @param {Store} fsStore
 * @returns {Set<string>}
 */
function extractFilePaths(fsStore) {
  const paths = new Set();
  const quads = fsStore.getQuads(
    null,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    null
  );
  for (const quad of quads) {
    paths.add(quad.object.value);
  }
  return paths;
}

/* ========================================================================= */
/* Stack profile detection                                                   */
/* ========================================================================= */

/**
 * Detect stack profile from package.json and file structure
 * @param {Store} fsStore
 * @param {string} [projectRoot]
 * @returns {Promise<z.infer<typeof StackProfileSchema>>}
 */
async function detectStackProfile(fsStore, projectRoot) {
  const profile = {
    hasZod: false,
    hasPrisma: false,
    hasTypeORM: false,
    hasSequelize: false,
    hasDrizzle: false,
    hasTypescript: false,
    sourceRoot: 'src',
  };

  const filePaths = extractFilePaths(fsStore);

  // Check for TypeScript
  if (filePaths.has('tsconfig.json') || filePaths.has('tsconfig.base.json')) {
    profile.hasTypescript = true;
  }

  // Check for Prisma
  if (filePaths.has('prisma/schema.prisma') || filePaths.has('schema.prisma')) {
    profile.hasPrisma = true;
  }

  // Try to read package.json if projectRoot provided
  if (projectRoot) {
    const pkgContent = await readFileContent(path.join(projectRoot, 'package.json'));
    if (pkgContent) {
      try {
        const pkg = JSON.parse(pkgContent);
        const allDeps = { ...pkg.dependencies, ...pkg.devDependencies };

        if (allDeps.zod) profile.hasZod = true;
        if (allDeps['@prisma/client'] || allDeps.prisma) profile.hasPrisma = true;
        if (allDeps.typeorm) profile.hasTypeORM = true;
        if (allDeps.sequelize) profile.hasSequelize = true;
        if (allDeps.drizzle) profile.hasDrizzle = true;
        if (allDeps.typescript) profile.hasTypescript = true;
      } catch {
        // Ignore JSON parse errors
      }
    }
  }

  return profile;
}

/* ========================================================================= */
/* Zod schema parsing                                                        */
/* ========================================================================= */

/**
 * Parse Zod schema definitions from file content
 * @param {string} content
 * @param {string} fileName
 * @returns {DomainEntity[]}
 */
function parseZodSchemas(content, _fileName) {
  const entities = [];

  // Match export const XxxSchema = z.object({ ... })
  const schemaPattern =
    /(?:export\s+)?(?:const|let)\s+(\w+)Schema\s*=\s*z\.object\(\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}\s*\)/g;

  let match;
  while ((match = schemaPattern.exec(content)) !== null) {
    const entityName = match[1];
    const fieldsBlock = match[2];

    const fields = parseZodFields(fieldsBlock);

    if (fields.length > 0) {
      entities.push({
        name: entityName,
        source: 'zod',
        fields,
        relations: extractZodRelations(fieldsBlock),
      });
    }
  }

  // Also match z.object inline (for simpler schemas)
  const inlinePattern =
    /(?:export\s+)?(?:const|let)\s+(\w+)\s*=\s*z\.object\(\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}\s*\)/g;

  while ((match = inlinePattern.exec(content)) !== null) {
    const entityName = match[1];
    if (entityName.endsWith('Schema')) continue; // Already matched above

    const fieldsBlock = match[2];
    const fields = parseZodFields(fieldsBlock);

    if (fields.length > 0) {
      entities.push({
        name: entityName,
        source: 'zod',
        fields,
        relations: extractZodRelations(fieldsBlock),
      });
    }
  }

  return entities;
}

/**
 * Parse individual Zod fields from object block
 * @param {string} fieldsBlock
 * @returns {DomainField[]}
 */
function parseZodFields(fieldsBlock) {
  const fields = [];

  // Match: fieldName: z.string(), z.number(), z.boolean(), z.array(), etc.
  const fieldPattern =
    /(\w+)\s*:\s*z\.(string|number|boolean|date|bigint|array|enum|object|union|optional|nullable)(\([^)]*\))?/g;

  let match;
  while ((match = fieldPattern.exec(fieldsBlock)) !== null) {
    const fieldName = match[1];
    let zodType = match[2];
    const _modifier = match[3] || '';

    // Check for optional/nullable chain
    const isOptional =
      fieldsBlock.includes(`${fieldName}:`) &&
      (fieldsBlock.includes('.optional()') || fieldsBlock.includes('.nullable()'));
    const isArray = zodType === 'array';

    // Map zod types to our types
    let type = 'string';
    if (zodType === 'number' || zodType === 'bigint') type = 'number';
    else if (zodType === 'boolean') type = 'boolean';
    else if (zodType === 'date') type = 'date';
    else if (zodType === 'array') type = 'array';
    else if (zodType === 'enum') type = 'enum';

    fields.push({
      name: fieldName,
      type,
      optional: isOptional,
      array: isArray,
    });
  }

  return fields;
}

/**
 * Extract relations from Zod schema
 * @param {string} fieldsBlock
 * @returns {string[]}
 */
function extractZodRelations(fieldsBlock) {
  const relations = [];

  // Match: z.array(OtherSchema), z.lazy(() => OtherSchema)
  const relationPattern = /z\.(?:array|lazy)\s*\(\s*(?:\(\)\s*=>\s*)?(\w+)Schema\s*\)/g;

  let match;
  while ((match = relationPattern.exec(fieldsBlock)) !== null) {
    const relatedEntity = match[1];
    if (!relations.includes(relatedEntity)) {
      relations.push(relatedEntity);
    }
  }

  return relations;
}

/* ========================================================================= */
/* Prisma schema parsing                                                     */
/* ========================================================================= */

/**
 * Parse Prisma schema file
 * @param {string} content
 * @returns {DomainEntity[]}
 */
function parsePrismaSchema(content) {
  const entities = [];

  // Match model blocks
  const modelPattern = /model\s+(\w+)\s*\{([^}]+)\}/g;

  let match;
  while ((match = modelPattern.exec(content)) !== null) {
    const modelName = match[1];
    const fieldsBlock = match[2];

    const { fields, relations } = parsePrismaFields(fieldsBlock);

    entities.push({
      name: modelName,
      source: 'prisma',
      fields,
      relations,
    });
  }

  return entities;
}

/**
 * Parse Prisma fields from model block
 * @param {string} fieldsBlock
 * @returns {{fields: DomainField[], relations: string[]}}
 */
function parsePrismaFields(fieldsBlock) {
  const fields = [];
  const relations = [];

  const lines = fieldsBlock.split('\n');

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('//') || trimmed.startsWith('@@')) continue;

    // Match: fieldName Type? @relation(...) or fieldName Type[]
    const fieldMatch = trimmed.match(/^(\w+)\s+(\w+)(\[\])?\??(.*)$/);

    if (fieldMatch) {
      const fieldName = fieldMatch[1];
      const fieldType = fieldMatch[2];
      const isArray = !!fieldMatch[3];
      const modifiers = fieldMatch[4] || '';

      // Skip internal Prisma fields
      if (fieldName.startsWith('_')) continue;

      // Check if it's a relation
      if (modifiers.includes('@relation')) {
        if (!relations.includes(fieldType)) {
          relations.push(fieldType);
        }
        continue;
      }

      // Check if type is another model (relation without @relation)
      const isPrimitiveType = [
        'String',
        'Int',
        'Float',
        'Boolean',
        'DateTime',
        'BigInt',
        'Decimal',
        'Json',
        'Bytes',
      ].includes(fieldType);

      if (!isPrimitiveType) {
        if (!relations.includes(fieldType)) {
          relations.push(fieldType);
        }
        continue;
      }

      const isOptional = trimmed.includes('?');

      fields.push({
        name: fieldName,
        type: fieldType.toLowerCase(),
        optional: isOptional,
        array: isArray,
      });
    }
  }

  return { fields, relations };
}

/* ========================================================================= */
/* TypeScript type/interface parsing                                         */
/* ========================================================================= */

/**
 * Parse TypeScript interfaces and types
 * @param {string} content
 * @param {string} fileName
 * @returns {DomainEntity[]}
 */
function parseTypeScriptTypes(content, _fileName) {
  const entities = [];

  // Match interface declarations
  const interfacePattern =
    /(?:export\s+)?interface\s+(\w+)(?:\s+extends\s+[\w,\s]+)?\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}/g;

  let match;
  while ((match = interfacePattern.exec(content)) !== null) {
    const entityName = match[1];
    const fieldsBlock = match[2];

    // Skip common non-entity interfaces
    if (
      entityName.endsWith('Props') ||
      entityName.endsWith('Options') ||
      entityName.endsWith('Config')
    ) {
      continue;
    }

    const { fields, relations } = parseTypeScriptFields(fieldsBlock);

    if (fields.length > 0) {
      entities.push({
        name: entityName,
        source: 'typescript',
        fields,
        relations,
      });
    }
  }

  // Match type declarations with object shape
  const typePattern = /(?:export\s+)?type\s+(\w+)\s*=\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}/g;

  while ((match = typePattern.exec(content)) !== null) {
    const entityName = match[1];
    const fieldsBlock = match[2];

    // Skip utility types
    if (
      entityName.endsWith('Props') ||
      entityName.endsWith('Options') ||
      entityName.endsWith('Config')
    ) {
      continue;
    }

    const { fields, relations } = parseTypeScriptFields(fieldsBlock);

    if (fields.length > 0) {
      entities.push({
        name: entityName,
        source: 'typescript',
        fields,
        relations,
      });
    }
  }

  return entities;
}

/**
 * Parse TypeScript fields from interface/type body
 * @param {string} fieldsBlock
 * @returns {{fields: DomainField[], relations: string[]}}
 */
function parseTypeScriptFields(fieldsBlock) {
  const fields = [];
  const relations = [];

  // Match: fieldName: Type, fieldName?: Type, readonly fieldName: Type
  const fieldPattern = /(?:readonly\s+)?(\w+)(\?)?:\s*([^;,\n]+)/g;

  let match;
  while ((match = fieldPattern.exec(fieldsBlock)) !== null) {
    const fieldName = match[1];
    const isOptional = !!match[2];
    let rawType = match[3].trim();

    // Check for array types
    const isArray = rawType.endsWith('[]') || rawType.startsWith('Array<');

    // Clean up the type
    let baseType = rawType
      .replace(/\[\]$/, '')
      .replace(/^Array<(.+)>$/, '$1')
      .replace(/\s*\|\s*null$/, '')
      .replace(/\s*\|\s*undefined$/, '')
      .trim();

    // Check if it's a primitive type
    const primitiveTypes = [
      'string',
      'number',
      'boolean',
      'Date',
      'bigint',
      'symbol',
      'any',
      'unknown',
      'never',
      'void',
    ];
    const isPrimitive =
      primitiveTypes.includes(baseType) || primitiveTypes.includes(baseType.toLowerCase());

    if (
      !isPrimitive &&
      /^[A-Z]/.test(baseType) &&
      !baseType.includes('|') &&
      !baseType.includes('&')
    ) {
      // Looks like a relation to another entity
      if (!relations.includes(baseType)) {
        relations.push(baseType);
      }
      continue;
    }

    fields.push({
      name: fieldName,
      type: baseType.toLowerCase(),
      optional: isOptional,
      array: isArray,
    });
  }

  return { fields, relations };
}

/* ========================================================================= */
/* TypeORM entity parsing                                                    */
/* ========================================================================= */

/**
 * Parse TypeORM entity decorators
 * @param {string} content
 * @param {string} fileName
 * @returns {DomainEntity[]}
 */
function parseTypeORMEntities(content, _fileName) {
  const entities = [];

  // Check if this looks like a TypeORM entity file
  if (!content.includes('@Entity') && !content.includes('typeorm')) {
    return entities;
  }

  // Match @Entity() class declarations
  const entityPattern =
    /@Entity\([^)]*\)\s*(?:export\s+)?class\s+(\w+)(?:\s+extends\s+\w+)?\s*\{([^]*?)(?=\n\}|$)/g;

  let match;
  while ((match = entityPattern.exec(content)) !== null) {
    const entityName = match[1];
    const classBody = match[2];

    const { fields, relations } = parseTypeORMFields(classBody);

    entities.push({
      name: entityName,
      source: 'typeorm',
      fields,
      relations,
    });
  }

  return entities;
}

/**
 * Parse TypeORM fields from class body
 * @param {string} classBody
 * @returns {{fields: DomainField[], relations: string[]}}
 */
function parseTypeORMFields(classBody) {
  const fields = [];
  const relations = [];

  // Match @Column() fieldName: Type
  const columnPattern = /@Column\([^)]*\)\s*(\w+)(?:\?)?:\s*([^;\n]+)/g;

  let match;
  while ((match = columnPattern.exec(classBody)) !== null) {
    const fieldName = match[1];
    const fieldType = match[2].trim();

    fields.push({
      name: fieldName,
      type: fieldType.toLowerCase(),
      optional: classBody.includes(`${fieldName}?:`),
      array: fieldType.endsWith('[]'),
    });
  }

  // Match relation decorators
  const relationPattern =
    /@(?:OneToMany|ManyToOne|OneToOne|ManyToMany)\([^)]*\)\s*\w+(?:\?)?:\s*(\w+)/g;

  while ((match = relationPattern.exec(classBody)) !== null) {
    const relatedType = match[1];
    if (!relations.includes(relatedType)) {
      relations.push(relatedType);
    }
  }

  return { fields, relations };
}

/* ========================================================================= */
/* File discovery                                                            */
/* ========================================================================= */

/**
 * Find schema/type files in the project
 * @param {Set<string>} filePaths
 * @param {z.infer<typeof StackProfileSchema>} stackProfile
 * @returns {string[]}
 */
function findSchemaFiles(filePaths, stackProfile) {
  const schemaFiles = [];

  for (const filePath of filePaths) {
    // Skip node_modules, dist, etc.
    if (
      filePath.includes('node_modules') ||
      filePath.includes('/dist/') ||
      filePath.includes('/build/')
    ) {
      continue;
    }

    const ext = path.extname(filePath);
    const basename = path.basename(filePath);

    // Check for Zod schema files
    if (stackProfile.hasZod) {
      if (
        filePath.includes('/schemas/') ||
        filePath.includes('/schema/') ||
        (filePath.includes('/types/') && (ext === '.ts' || ext === '.mjs')) ||
        basename.includes('schema') ||
        basename.includes('validation')
      ) {
        schemaFiles.push(filePath);
      }
    }

    // Check for TypeScript type files
    if (stackProfile.hasTypescript) {
      if (
        filePath.includes('/types/') ||
        filePath.includes('/interfaces/') ||
        filePath.includes('/models/') ||
        basename.endsWith('.d.ts') ||
        basename.includes('.types.')
      ) {
        schemaFiles.push(filePath);
      }
    }

    // Check for TypeORM entity files
    if (stackProfile.hasTypeORM) {
      if (
        filePath.includes('/entities/') ||
        filePath.includes('/entity/') ||
        basename.includes('.entity.')
      ) {
        schemaFiles.push(filePath);
      }
    }

    // Check for Prisma schema
    if (stackProfile.hasPrisma) {
      if (basename === 'schema.prisma') {
        schemaFiles.push(filePath);
      }
    }
  }

  // Dedupe
  return [...new Set(schemaFiles)];
}

/* ========================================================================= */
/* Store building                                                            */
/* ========================================================================= */

/**
 * Add entity to RDF store
 * @param {Store} store
 * @param {DomainEntity} entity
 * @param {string} baseIri
 */
function addEntityToStore(store, entity, baseIri) {
  const entityIri = namedNode(`${baseIri}${entity.name}`);

  // Add entity type
  store.addQuad(entityIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`));

  // Add label
  store.addQuad(entityIri, namedNode(`${NS.rdfs}label`), literal(entity.name));

  // Add source
  store.addQuad(entityIri, namedNode(`${NS.dom}source`), literal(entity.source));

  // Add fields
  for (const field of entity.fields) {
    const fieldIri = namedNode(`${baseIri}${entity.name}.${field.name}`);

    // Link entity to field
    store.addQuad(entityIri, namedNode(`${NS.dom}hasField`), fieldIri);

    // Add field type
    store.addQuad(fieldIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Field`));

    // Add field name
    store.addQuad(fieldIri, namedNode(`${NS.dom}fieldName`), literal(field.name));

    // Add field type (XSD)
    store.addQuad(fieldIri, namedNode(`${NS.dom}fieldType`), namedNode(mapToXsdType(field.type)));

    // Add optional flag
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}isOptional`),
      literal(field.optional, namedNode(`${NS.xsd}boolean`))
    );

    // Add array flag
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}isArray`),
      literal(field.array, namedNode(`${NS.xsd}boolean`))
    );
  }

  // Add relations
  for (const relation of entity.relations) {
    const relationIri = namedNode(`${baseIri}${relation}`);

    store.addQuad(entityIri, namedNode(`${NS.dom}relatesTo`), relationIri);
  }
}

/* ========================================================================= */
/* Main API                                                                  */
/* ========================================================================= */

/**
 * Infer domain model from filesystem store
 *
 * @param {Object} options
 * @param {Store} options.fsStore - FS store from scanFileSystemToStore
 * @param {Object} [options.stackProfile] - Pre-computed stack profile
 * @param {string} [options.baseIri] - Base IRI for domain resources
 * @param {string} [options.projectRoot] - Project root path for reading files
 * @returns {Promise<DomainInferResult>}
 */
export async function inferDomainModel(options) {
  const validated = InferOptionsSchema.parse(options);
  const { fsStore, baseIri, projectRoot } = validated;

  // Detect or use provided stack profile
  const stackProfile = validated.stackProfile || (await detectStackProfile(fsStore, projectRoot));

  const store = await createStore();
  const allEntities = [];

  // Get file paths from fsStore
  const filePaths = extractFilePaths(fsStore);

  // Find schema files
  const schemaFiles = findSchemaFiles(filePaths, stackProfile);

  // Process each schema file
  for (const relativePath of schemaFiles) {
    if (!projectRoot) continue;

    const fullPath = path.join(projectRoot, relativePath);
    const content = await readFileContent(fullPath);

    if (!content) continue;

    const ext = path.extname(relativePath);

    // Parse based on file type and stack
    if (relativePath.endsWith('.prisma')) {
      const entities = parsePrismaSchema(content);
      allEntities.push(...entities);
    } else if (ext === '.ts' || ext === '.tsx') {
      // Check for Zod schemas first
      if (
        stackProfile.hasZod &&
        (content.includes("from 'zod'") || content.includes('from "zod"'))
      ) {
        const zodEntities = parseZodSchemas(content, relativePath);
        allEntities.push(...zodEntities);
      }

      // Check for TypeORM entities
      if (stackProfile.hasTypeORM && content.includes('@Entity')) {
        const typeormEntities = parseTypeORMEntities(content, relativePath);
        allEntities.push(...typeormEntities);
      }

      // Parse TypeScript types/interfaces
      if (stackProfile.hasTypescript) {
        const tsEntities = parseTypeScriptTypes(content, relativePath);
        allEntities.push(...tsEntities);
      }
    } else if (ext === '.mjs' || ext === '.js') {
      // Check for Zod schemas in JS/MJS files
      if (
        stackProfile.hasZod &&
        (content.includes("from 'zod'") || content.includes('from "zod"'))
      ) {
        const zodEntities = parseZodSchemas(content, relativePath);
        allEntities.push(...zodEntities);
      }
    }
  }

  // Deduplicate entities by name (prefer richer sources)
  const entityMap = new Map();
  const sourcePriority = { prisma: 4, typeorm: 3, zod: 2, typescript: 1 };

  for (const entity of allEntities) {
    const existing = entityMap.get(entity.name);
    if (
      !existing ||
      (sourcePriority[entity.source] || 0) > (sourcePriority[existing.source] || 0)
    ) {
      entityMap.set(entity.name, entity);
    }
  }

  // Add entities to store
  let fieldCount = 0;
  let relationshipCount = 0;

  for (const entity of entityMap.values()) {
    addEntityToStore(store, entity, baseIri);
    fieldCount += entity.fields.length;
    relationshipCount += entity.relations.length;
  }

  return {
    store,
    summary: {
      entityCount: entityMap.size,
      fieldCount,
      relationshipCount,
    },
  };
}

/**
 * Convenience function: infer from project path
 *
 * @param {string} projectRoot - Path to project root
 * @param {Object} [options]
 * @param {string} [options.baseIri] - Base IRI for domain resources
 * @returns {Promise<DomainInferResult>}
 */
export async function inferDomainModelFromPath(projectRoot, options = {}) {
  // Import fs-scan dynamically to avoid circular deps
  const { scanFileSystemToStore } = await import('./fs-scan.mjs');

  const { store: fsStore } = await scanFileSystemToStore({ root: projectRoot });

  return inferDomainModel({
    fsStore,
    projectRoot,
    baseIri: options.baseIri,
  });
}

/* ========================================================================= */
/* Domain Model Lens                                                         */
/* ========================================================================= */

/**
 * Ontology lens for domain model changes
 *
 * @param {import('../diff.mjs').DiffTriple} triple
 * @param {'added' | 'removed'} direction
 * @returns {import('../diff.mjs').OntologyChange | null}
 */
export function DomainModelLens(triple, direction) {
  const { subject, predicate, object } = triple;

  // Entity added/removed
  if (predicate === `${NS.rdf}type` && object === `${NS.dom}Entity`) {
    const entityName = subject.split('#').pop() || subject.split('/').pop();
    return {
      kind: direction === 'added' ? 'EntityAdded' : 'EntityRemoved',
      entity: subject,
      details: { name: entityName },
    };
  }

  // Field added/removed
  if (predicate === `${NS.dom}hasField`) {
    const entityName = subject.split('#').pop() || subject.split('/').pop();
    const fieldName = object.split('.').pop();
    return {
      kind: direction === 'added' ? 'FieldAdded' : 'FieldRemoved',
      entity: subject,
      role: fieldName,
      details: { entityName, fieldName },
    };
  }

  // Relation added/removed
  if (predicate === `${NS.dom}relatesTo`) {
    const fromEntity = subject.split('#').pop() || subject.split('/').pop();
    const toEntity = object.split('#').pop() || object.split('/').pop();
    return {
      kind: direction === 'added' ? 'RelationAdded' : 'RelationRemoved',
      entity: subject,
      details: { from: fromEntity, to: toEntity },
    };
  }

  // Field type changed (detected as removed + added)
  if (predicate === `${NS.dom}fieldType`) {
    const fieldPath = subject.split('#').pop() || subject.split('/').pop();
    return {
      kind: direction === 'added' ? 'FieldTypeSet' : 'FieldTypeUnset',
      entity: subject,
      details: { field: fieldPath, type: object },
    };
  }

  return null;
}
