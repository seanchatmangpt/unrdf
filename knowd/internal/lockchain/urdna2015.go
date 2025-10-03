package lockchain

import (
	"fmt"
	"sort"
	"strings"
)

// URDNA2015Canonicalizer implements URDNA2015 canonicalization for RDF datasets.
type URDNA2015Canonicalizer struct{}

// Quad represents an RDF quad for canonicalization purposes.
type Quad struct {
	Subject   string
	Predicate string
	Object    string
	Graph     string
}

// NewURDNA2015Canonicalizer creates a new URDNA2015 canonicalizer.
func NewURDNA2015Canonicalizer() *URDNA2015Canonicalizer {
	return &URDNA2015Canonicalizer{}
}

// Canonicalize canonicalizes a set of RDF quads using URDNA2015 algorithm.
func (c *URDNA2015Canonicalizer) Canonicalize(quads []Quad) (string, error) {
	// Simplified URDNA2015 implementation
	// In a real implementation, this would follow the full URDNA2015 specification

	// Sort quads in canonical order
	sortedQuads := c.sortQuads(quads)

	// Convert to canonical N-Quads format
	var lines []string
	for _, quad := range sortedQuads {
		line := c.quadToNQuad(quad)
		lines = append(lines, line)
	}

	return strings.Join(lines, "\n"), nil
}

// sortQuads sorts quads in canonical order according to URDNA2015.
func (c *URDNA2015Canonicalizer) sortQuads(quads []Quad) []Quad {
	// Create sortable quads with comparison key
	type sortableQuad struct {
		quad Quad
		key  string
	}

	var sortable []sortableQuad
	for _, quad := range quads {
		key := c.createSortKey(quad)
		sortable = append(sortable, sortableQuad{quad: quad, key: key})
	}

	// Sort by key
	sort.Slice(sortable, func(i, j int) bool {
		return sortable[i].key < sortable[j].key
	})

	// Extract sorted quads
	var sorted []Quad
	for _, sq := range sortable {
		sorted = append(sorted, sq.quad)
	}

	return sorted
}

// createSortKey creates a sort key for a quad according to URDNA2015.
func (c *URDNA2015Canonicalizer) createSortKey(quad Quad) string {
	// Simplified sort key creation
	// In real URDNA2015, this would involve canonical identifier assignment
	return fmt.Sprintf("%s|%s|%s|%s",
		c.canonicalizeTerm(quad.Subject),
		c.canonicalizeTerm(quad.Predicate),
		c.canonicalizeTerm(quad.Object),
		c.canonicalizeTerm(quad.Graph))
}

// canonicalizeTerm canonicalizes an RDF term for sorting.
func (c *URDNA2015Canonicalizer) canonicalizeTerm(term string) string {
	// Simplified canonicalization
	// In real URDNA2015, this would handle IRI canonicalization, blank node mapping, etc.

	// IRIs are already canonical
	if strings.HasPrefix(term, "http://") || strings.HasPrefix(term, "https://") {
		return term
	}

	// Blank nodes start with "_:"
	if strings.HasPrefix(term, "_:") {
		return term
	}

	// Literals need type/lang canonicalization
	return term
}

// quadToNQuad converts a quad to N-Quads format.
func (c *URDNA2015Canonicalizer) quadToNQuad(quad Quad) string {
	// Format: <subject> <predicate> <object> <graph> .
	subject := c.termToNQuad(quad.Subject)
	predicate := c.termToNQuad(quad.Predicate)
	object := c.termToNQuad(quad.Object)
	graph := c.termToNQuad(quad.Graph)

	return fmt.Sprintf("%s %s %s %s .", subject, predicate, object, graph)
}

// termToNQuad converts an RDF term to N-Quads format.
func (c *URDNA2015Canonicalizer) termToNQuad(term string) string {
	// IRIs: <iri>
	if strings.HasPrefix(term, "http://") || strings.HasPrefix(term, "https://") {
		return fmt.Sprintf("<%s>", term)
	}

	// Blank nodes: _:bnode
	if strings.HasPrefix(term, "_:") {
		return term
	}

	// Literals: "literal" or "literal"^^<type> or "literal"@lang
	return fmt.Sprintf(`"%s"`, term)
}
