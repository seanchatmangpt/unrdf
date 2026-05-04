/**
 * @file Golden structure ontologies - canonical best-practice structures
 * @module project-engine/golden-structure
 */

import { promises as _fs } from 'fs';
import { createStore as _createStore } from '@unrdf/oxigraph';
import { parseTurtle } from '@unrdf/knowledge-engine';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf/golden-structure');

const GoldenStructureOptionsSchema = z.object({
  profile: z.enum([
    'react-feature-v1',
    'next-app-router-v1',
    'next-pages-v1',
    'nest-api-v1',
    'express-api-v1',
  ]),
});

/**
 * Generate or load golden structure from profile
 *
 * @param {Object} options
 * @param {string} options.profile - Profile ID (e.g., 'react-feature-v1')
 * @returns {Promise<Store>} Store with golden structure
 */
export async function generateGoldenStructure(options) {
  const validated = GoldenStructureOptionsSchema.parse(options);
  const { profile } = validated;

  return tracer.startActiveSpan('golden.structure.generate', async span => {
    try {
      span.setAttribute('golden.profile', profile);

      const ttl = getGoldenStructureTtl(profile);
      const store = await parseTurtle(ttl, 'http://example.org/unrdf/golden#');

      span.setAttribute('golden.store_size', store.size);
      span.setStatus({ code: SpanStatusCode.OK });

      return store;
    } catch (error) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    }
  });
}

/**
 * Get golden structure TTL for a profile
 *
 * @private
 */
function getGoldenStructureTtl(profile) {
  const baseGolden = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix unproj: <http://example.org/unrdf/project#> .
@prefix golden: <http://example.org/unrdf/golden#> .

golden:project a unproj:Project ;
  rdfs:label "Golden Structure" .
`;

  // Profile-specific structures
  if (profile === 'react-feature-v1') {
    return (
      baseGolden +
      `
golden:featureExample a unproj:Feature ;
  rdfs:label "Example Feature" ;
  rdfs:comment "Standard React feature structure" .

golden:featureExample unproj:hasRole unproj:Component ;
  unproj:hasRole unproj:Hook ;
  unproj:hasRole unproj:Schema ;
  unproj:hasRole unproj:Test ;
  unproj:hasRole unproj:Doc .

golden:pathComponentPattern a rdf:Property ;
  rdfs:value "src/features/*/components/*.tsx" .

golden:pathHookPattern a rdf:Property ;
  rdfs:value "src/features/*/hooks/use*.ts" .

golden:pathTestPattern a rdf:Property ;
  rdfs:value "src/features/**/*.test.tsx" .
`
    );
  } else if (profile === 'next-app-router-v1') {
    return (
      baseGolden +
      `
golden:project unproj:webFramework "next-app-router" .

golden:pageExample a unproj:Page ;
  rdfs:label "Page Route" .

golden:layoutExample a unproj:Component ;
  rdfs:label "Layout Component" .

golden:apiExample a unproj:Api ;
  rdfs:label "API Route" .

golden:pathPagePattern a rdf:Property ;
  rdfs:value "src/app/**/page.tsx" .

golden:pathLayoutPattern a rdf:Property ;
  rdfs:value "src/app/**/layout.tsx" .

golden:pathApiPattern a rdf:Property ;
  rdfs:value "src/app/**/route.ts" .
`
    );
  } else if (profile === 'nest-api-v1') {
    return (
      baseGolden +
      `
golden:project unproj:webFramework "nest" ;
  unproj:apiFramework "nest" .

golden:controllerExample a unproj:Api ;
  rdfs:label "NestJS Controller" .

golden:serviceExample a unproj:Service ;
  rdfs:label "NestJS Service" .

golden:schemaExample a unproj:Schema ;
  rdfs:label "NestJS DTO" .

golden:pathControllerPattern a rdf:Property ;
  rdfs:value "src/**/*.controller.ts" .

golden:pathServicePattern a rdf:Property ;
  rdfs:value "src/**/*.service.ts" .

golden:pathDtoPattern a rdf:Property ;
  rdfs:value "src/**/dto/*.dto.ts" .
`
    );
  } else {
    // Default fallback
    return baseGolden;
  }
}
