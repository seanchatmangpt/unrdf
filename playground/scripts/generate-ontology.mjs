#!/usr/bin/env node
/**
 * Generate ontology files from templates
 * @module scripts/generate-ontology
 */

import { writeFileSync, mkdirSync, readFileSync, existsSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Base ontology prefixes
 */
const PREFIXES = `@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix schema: <http://schema.org/> .
@prefix playground: <http://example.org/playground#> .
`;

/**
 * Generate playground ontology
 * @returns {string} Turtle ontology content
 */
function generatePlaygroundOntology() {
  return `${PREFIXES}

# Playground Ontology
# Defines concepts for the autonomic knowledge graph system

playground: a owl:Ontology ;
    dcterms:title "Playground Ontology"@en ;
    dcterms:description "An ontology for autonomic knowledge graph systems"@en ;
    dcterms:created "2024-01-01"^^xsd:date ;
    owl:versionInfo "1.0.0" .

# Classes

playground:KnowledgeHook a owl:Class ;
    rdfs:label "Knowledge Hook"@en ;
    rdfs:comment "A hook that intercepts and processes knowledge operations"@en ;
    rdfs:subClassOf owl:Thing .

playground:PolicyPack a owl:Class ;
    rdfs:label "Policy Pack"@en ;
    rdfs:comment "A collection of policies for knowledge validation"@en ;
    rdfs:subClassOf owl:Thing .

playground:ValidationRule a owl:Class ;
    rdfs:label "Validation Rule"@en ;
    rdfs:comment "A rule used to validate knowledge graphs"@en ;
    rdfs:subClassOf owl:Thing .

playground:TransformOperation a owl:Class ;
    rdfs:label "Transform Operation"@en ;
    rdfs:comment "An operation that transforms RDF data"@en ;
    rdfs:subClassOf owl:Thing .

playground:QueryTemplate a owl:Class ;
    rdfs:label "Query Template"@en ;
    rdfs:comment "A reusable SPARQL query template"@en ;
    rdfs:subClassOf owl:Thing .

# Properties

playground:hasHook a owl:ObjectProperty ;
    rdfs:label "has hook"@en ;
    rdfs:domain playground:PolicyPack ;
    rdfs:range playground:KnowledgeHook .

playground:hasRule a owl:ObjectProperty ;
    rdfs:label "has rule"@en ;
    rdfs:domain playground:PolicyPack ;
    rdfs:range playground:ValidationRule .

playground:hookPriority a owl:DatatypeProperty ;
    rdfs:label "hook priority"@en ;
    rdfs:domain playground:KnowledgeHook ;
    rdfs:range xsd:integer .

playground:isEnabled a owl:DatatypeProperty ;
    rdfs:label "is enabled"@en ;
    rdfs:domain playground:KnowledgeHook ;
    rdfs:range xsd:boolean .

playground:sparqlQuery a owl:DatatypeProperty ;
    rdfs:label "SPARQL query"@en ;
    rdfs:domain playground:QueryTemplate ;
    rdfs:range xsd:string .
`;
}

/**
 * Generate SHACL shapes
 * @returns {string} Turtle SHACL content
 */
function generateShaclShapes() {
  return `${PREFIXES}
@prefix sh: <http://www.w3.org/ns/shacl#> .

# SHACL Shapes for Playground Ontology

playground:KnowledgeHookShape a sh:NodeShape ;
    sh:targetClass playground:KnowledgeHook ;
    sh:property [
        sh:path rdfs:label ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Knowledge Hook must have a label"@en ;
    ] ;
    sh:property [
        sh:path playground:hookPriority ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100 ;
        sh:message "Hook priority must be between 0 and 100"@en ;
    ] ;
    sh:property [
        sh:path playground:isEnabled ;
        sh:datatype xsd:boolean ;
        sh:maxCount 1 ;
    ] .

playground:PolicyPackShape a sh:NodeShape ;
    sh:targetClass playground:PolicyPack ;
    sh:property [
        sh:path rdfs:label ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
    ] ;
    sh:property [
        sh:path playground:hasHook ;
        sh:class playground:KnowledgeHook ;
    ] ;
    sh:property [
        sh:path playground:hasRule ;
        sh:class playground:ValidationRule ;
    ] .

playground:QueryTemplateShape a sh:NodeShape ;
    sh:targetClass playground:QueryTemplate ;
    sh:property [
        sh:path playground:sparqlQuery ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Query template must have a SPARQL query"@en ;
    ] .
`;
}

/**
 * Main function
 */
async function main() {
  const ontologiesDir = join(__dirname, '..', 'ontologies');

  try {
    mkdirSync(ontologiesDir, { recursive: true });

    // Generate playground ontology
    const ontology = generatePlaygroundOntology();
    writeFileSync(join(ontologiesDir, 'playground.ttl'), ontology);
    console.log('Generated ontology: ontologies/playground.ttl');

    // Generate SHACL shapes
    const shapes = generateShaclShapes();
    writeFileSync(join(ontologiesDir, 'playground-shapes.ttl'), shapes);
    console.log('Generated SHACL shapes: ontologies/playground-shapes.ttl');

    console.log('\nOntology generation complete!');
    console.log('Files generated in ontologies/ directory');

  } catch (error) {
    console.error('Failed to generate ontology:', error.message);
    process.exit(1);
  }
}

main();
