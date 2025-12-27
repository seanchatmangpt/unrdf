import { DataFactory } from "n3";

const { namedNode, literal, blankNode } = DataFactory;

/**
 * Ensure any input is a NamedNode
 * @param {string|import('n3').NamedNode} iri - IRI string or NamedNode
 * @returns {import('n3').NamedNode} NamedNode instance
 */
export const asNamedNode = (iri) => {
  if (iri == undefined) {
    throw new Error("asNamedNode: IRI cannot be null or undefined");
  }
  return iri?.termType === "NamedNode" ? iri : namedNode(String(iri));
};

/**
 * Ensure any input is a Literal
 * @param {string|number|boolean} value - The literal value
 * @param {string} [datatype="http://www.w3.org/2001/XMLSchema#string"] - The datatype IRI
 * @returns {import('n3').Literal} Literal instance
 */
export const asLiteral = (value, datatype = "http://www.w3.org/2001/XMLSchema#string") => {
  if (value == undefined) {
    throw new Error("asLiteral: value cannot be null or undefined");
  }
  return literal(String(value), datatype);
};

/**
 * Ensure any input is a BlankNode
 * @param {string} [id] - Optional blank node identifier
 * @returns {import('n3').BlankNode} BlankNode instance
 */
export const asBlankNode = (id) => blankNode(id);

/**
 * Safe string coercion from RDF terms
 * @param {import('n3').Term|string} term - RDF term or string
 * @returns {string} String representation
 */
export const asString = (term) =>
  term?.termType === "Literal" ? term.value : term?.value || String(term);

/**
 * Check if a term is a NamedNode
 * @param {import('n3').Term} term - RDF term to check
 * @returns {boolean} True if term is a NamedNode
 */
export const isNamedNode = (term) => term?.termType === "NamedNode";

/**
 * Check if a term is a Literal
 * @param {import('n3').Term} term - RDF term to check
 * @returns {boolean} True if term is a Literal
 */
export const isLiteral = (term) => term?.termType === "Literal";

/**
 * Check if a term is a BlankNode
 * @param {import('n3').Term} term - RDF term to check
 * @returns {boolean} True if term is a BlankNode
 */
export const isBlankNode = (term) => term?.termType === "BlankNode";

/**
 * Get the IRI value from a NamedNode, or return the input if already a string
 * @param {string|import('n3').NamedNode} term - IRI string or NamedNode
 * @returns {string} IRI string
 */
export const getIRI = (term) => 
  term?.termType === "NamedNode" ? term.value : String(term);

/**
 * Create a literal with appropriate datatype based on value type
 * @param {any} value - The value to convert to literal
 * @returns {import('n3').Literal} Literal with appropriate datatype
 */
export const smartLiteral = (value) => {
  if (typeof value === "boolean") {
    return literal(String(value), "http://www.w3.org/2001/XMLSchema#boolean");
  }
  if (typeof value === "number") {
    return literal(String(value), "http://www.w3.org/2001/XMLSchema#decimal");
  }
  if (value instanceof Date) {
    return literal(value.toISOString(), "http://www.w3.org/2001/XMLSchema#dateTime");
  }
  if (typeof value === "object" && value !== null) {
    return literal(JSON.stringify(value), "http://www.w3.org/2001/XMLSchema#string");
  }
  return asLiteral(value);
};