/**
 * KGC Multiverse - Schema Morphisms
 * Specialized morphisms for RDF schema/ontology transformations
 *
 * @module @unrdf/kgc-multiverse/schema-morphism
 */

import { createMorphism, MorphismType } from './morphism.mjs';

/**
 * Create Class Rename Morphism
 * Renames all instances of a class
 *
 * @param {string} oldClass - Old class URI
 * @param {string} newClass - New class URI
 * @returns {Promise<Object>} Class rename morphism
 *
 * @example
 * import { createClassRenameMorphism } from './schema-morphism.mjs';
 * const phi = await createClassRenameMorphism(
 *   'http://old.com/Person',
 *   'http://new.com/Person'
 * );
 */
export async function createClassRenameMorphism(oldClass, newClass) {
  return createMorphism({
    type: MorphismType.SCHEMA,
    name: `rename-class-${oldClass.split('/').pop()}`,
    description: `Rename class ${oldClass} to ${newClass}`,
    transform: (quads) => {
      const deltas = [];
      const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

      for (const quad of quads) {
        // Find all rdf:type oldClass triples
        if (quad.predicate.value === RDF_TYPE && quad.object.value === oldClass) {
          // Delete old type assertion
          deltas.push({
            type: 'delete',
            subject: quad.subject.value,
            predicate: RDF_TYPE,
            object: {
              type: 'NamedNode',
              value: oldClass,
            },
          });

          // Add new type assertion
          deltas.push({
            type: 'add',
            subject: quad.subject.value,
            predicate: RDF_TYPE,
            object: {
              type: 'NamedNode',
              value: newClass,
            },
          });
        }
      }

      return deltas;
    },
    metadata: {
      oldClass,
      newClass,
    },
  });
}

/**
 * Create Property Rename Morphism
 * Renames all occurrences of a property/predicate
 *
 * @param {string} oldProperty - Old property URI
 * @param {string} newProperty - New property URI
 * @returns {Promise<Object>} Property rename morphism
 *
 * @example
 * const phi = await createPropertyRenameMorphism(
 *   'http://schema.org/name',
 *   'http://schema.org/fullName'
 * );
 */
export async function createPropertyRenameMorphism(oldProperty, newProperty) {
  return createMorphism({
    type: MorphismType.SCHEMA,
    name: `rename-property-${oldProperty.split('/').pop()}`,
    description: `Rename property ${oldProperty} to ${newProperty}`,
    transform: (quads) => {
      const deltas = [];

      for (const quad of quads) {
        if (quad.predicate.value === oldProperty) {
          // Delete old property
          deltas.push({
            type: 'delete',
            subject: quad.subject.value,
            predicate: oldProperty,
            object: {
              type: quad.object.termType,
              value: quad.object.value,
              ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
              ...(quad.object.language && { language: quad.object.language }),
            },
          });

          // Add new property
          deltas.push({
            type: 'add',
            subject: quad.subject.value,
            predicate: newProperty,
            object: {
              type: quad.object.termType,
              value: quad.object.value,
              ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
              ...(quad.object.language && { language: quad.object.language }),
            },
          });
        }
      }

      return deltas;
    },
    metadata: {
      oldProperty,
      newProperty,
    },
  });
}

/**
 * Create Namespace Migration Morphism
 * Migrates all URIs from old namespace to new namespace
 *
 * @param {string} oldNamespace - Old namespace prefix (e.g., 'http://old.com/')
 * @param {string} newNamespace - New namespace prefix (e.g., 'http://new.com/')
 * @returns {Promise<Object>} Namespace migration morphism
 *
 * @example
 * const phi = await createNamespaceMigrationMorphism(
 *   'http://old-schema.org/',
 *   'http://new-schema.org/'
 * );
 */
export async function createNamespaceMigrationMorphism(oldNamespace, newNamespace) {
  return createMorphism({
    type: MorphismType.SCHEMA,
    name: 'migrate-namespace',
    description: `Migrate namespace from ${oldNamespace} to ${newNamespace}`,
    transform: (quads) => {
      const deltas = [];

      for (const quad of quads) {
        const needsUpdate =
          quad.subject.value.startsWith(oldNamespace) ||
          quad.predicate.value.startsWith(oldNamespace) ||
          (quad.object.termType === 'NamedNode' && quad.object.value.startsWith(oldNamespace));

        if (needsUpdate) {
          // Delete old quad
          deltas.push({
            type: 'delete',
            subject: quad.subject.value,
            predicate: quad.predicate.value,
            object: {
              type: quad.object.termType,
              value: quad.object.value,
              ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
              ...(quad.object.language && { language: quad.object.language }),
            },
          });

          // Add updated quad with new namespace
          deltas.push({
            type: 'add',
            subject: quad.subject.value.replace(oldNamespace, newNamespace),
            predicate: quad.predicate.value.replace(oldNamespace, newNamespace),
            object: {
              type: quad.object.termType,
              value: quad.object.termType === 'NamedNode'
                ? quad.object.value.replace(oldNamespace, newNamespace)
                : quad.object.value,
              ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
              ...(quad.object.language && { language: quad.object.language }),
            },
          });
        }
      }

      return deltas;
    },
    metadata: {
      oldNamespace,
      newNamespace,
    },
  });
}

/**
 * Create Datatype Conversion Morphism
 * Converts literal datatypes (e.g., xsd:string → xsd:token)
 *
 * @param {string} oldDatatype - Old datatype URI
 * @param {string} newDatatype - New datatype URI
 * @param {Function} [converter] - Optional value converter function
 * @returns {Promise<Object>} Datatype conversion morphism
 *
 * @example
 * const phi = await createDatatypeConversionMorphism(
 *   'http://www.w3.org/2001/XMLSchema#string',
 *   'http://www.w3.org/2001/XMLSchema#token',
 *   (value) => value.trim()
 * );
 */
export async function createDatatypeConversionMorphism(
  oldDatatype,
  newDatatype,
  converter = (v) => v
) {
  return createMorphism({
    type: MorphismType.SCHEMA,
    name: `convert-datatype-${oldDatatype.split('#').pop()}`,
    description: `Convert datatype ${oldDatatype} to ${newDatatype}`,
    transform: (quads) => {
      const deltas = [];

      for (const quad of quads) {
        if (
          quad.object.termType === 'Literal' &&
          quad.object.datatype &&
          quad.object.datatype.value === oldDatatype
        ) {
          // Delete old literal
          deltas.push({
            type: 'delete',
            subject: quad.subject.value,
            predicate: quad.predicate.value,
            object: {
              type: 'Literal',
              value: quad.object.value,
              datatype: oldDatatype,
            },
          });

          // Add new literal with converted datatype
          deltas.push({
            type: 'add',
            subject: quad.subject.value,
            predicate: quad.predicate.value,
            object: {
              type: 'Literal',
              value: converter(quad.object.value),
              datatype: newDatatype,
            },
          });
        }
      }

      return deltas;
    },
    metadata: {
      oldDatatype,
      newDatatype,
    },
  });
}

/**
 * Create Property Chain Morphism
 * Replaces property chain (p1 → p2 → p3) with direct property
 *
 * @param {Array<string>} propertyChain - Array of property URIs forming chain
 * @param {string} targetProperty - Direct property to replace chain
 * @returns {Promise<Object>} Property chain morphism
 *
 * @example
 * // Replace person → address → city with person → cityOfResidence
 * const phi = await createPropertyChainMorphism(
 *   ['http://ex.com/address', 'http://ex.com/city'],
 *   'http://ex.com/cityOfResidence'
 * );
 */
export async function createPropertyChainMorphism(propertyChain, targetProperty) {
  return createMorphism({
    type: MorphismType.SCHEMA,
    name: 'property-chain-collapse',
    description: `Collapse property chain to ${targetProperty}`,
    transform: (quads) => {
      const deltas = [];

      // Build index of intermediate nodes
      const intermediates = new Map(); // subject → { predicate → object }

      for (const quad of quads) {
        if (!intermediates.has(quad.subject.value)) {
          intermediates.set(quad.subject.value, new Map());
        }
        intermediates.get(quad.subject.value).set(quad.predicate.value, quad.object.value);
      }

      // Find chains and collapse
      for (const quad of quads) {
        if (quad.predicate.value === propertyChain[0]) {
          let current = quad.object.value;
          let valid = true;

          // Traverse chain
          for (let i = 1; i < propertyChain.length; i++) {
            const nextMap = intermediates.get(current);
            if (!nextMap || !nextMap.has(propertyChain[i])) {
              valid = false;
              break;
            }
            current = nextMap.get(propertyChain[i]);
          }

          if (valid) {
            // Add direct property linking start to end of chain
            deltas.push({
              type: 'add',
              subject: quad.subject.value,
              predicate: targetProperty,
              object: {
                type: 'NamedNode',
                value: current,
              },
            });
          }
        }
      }

      return deltas;
    },
    metadata: {
      propertyChain,
      targetProperty,
    },
  });
}
