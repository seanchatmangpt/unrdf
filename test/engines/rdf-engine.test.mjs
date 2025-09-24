/**
 * @fileoverview Main RDF engine test suite
 * 
 * This file imports and runs all RDF engine test modules organized by capability:
 * - Core functionality (constructor, store access, terms)
 * - Parsing and serialization
 * - SHACL validation
 * - SPARQL query and update
 * - Reasoning and JSON-LD
 * - Set operations and utilities
 * - Edge cases and error handling
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

// Import all test modules
import "./rdf-engine/core.test.mjs";
import "./rdf-engine/parsing.test.mjs";
import "./rdf-engine/validation.test.mjs";
import "./rdf-engine/sparql.test.mjs";
import "./rdf-engine/reasoning.test.mjs";
import "./rdf-engine/utilities.test.mjs";
import "./rdf-engine/edge-cases.test.mjs";
