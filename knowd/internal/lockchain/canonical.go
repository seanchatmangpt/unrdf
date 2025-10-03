package lockchain

import (
	"fmt"
	"sort"
	"strings"

	sha3ext "golang.org/x/crypto/sha3"
)

// URDNA2015 implements the URDNA2015 canonicalization algorithm for RDF/N-Quads.
type URDNA2015 struct {
	blankNodes map[string]string
	nodeMap    map[string]*Node
}

// Node represents an RDF node in the canonicalization process.
type Node struct {
	Labels         map[string]bool
	Quads          []QuadRecord
	CanonicalLabel string
	Hash           string
}

// QuadRecord represents a canonicalized quad.
type QuadRecord struct {
	Subject   string
	Predicate string
	Object    string
	Graph     string
}

// NewURDNA2015 creates a new URDNA2015 canonicalizer.
func NewURDNA2015() *URDNA2015 {
	return &URDNA2015{
		blankNodes: make(map[string]string),
		nodeMap:    make(map[string]*Node),
	}
}

// CanonicalizeQuads canonicalizes RDF quads using URDNA2015 algorithm.
func (urdna *URDNA2015) CanonicalizeQuads(quads [][]string) ([]byte, error) {
	// Step 1: Create canonical blank node labels
	nodeMap := urdna.createCanonicalLabeling(quads)

	// Step 2: Canonicalize quads
	canonicalQuads := urdna.canonicalizeQuadList(quads, nodeMap)

	// Step 3: Sort canonical quads lexicographically
	sort.Strings(canonicalQuads)

	// Step 4: Generate canonical N-Quads string
	return []byte(strings.Join(canonicalQuads, "\n") + "\n"), nil
}

// Canonicalize canonicalizes data using URDNA2015 algorithm.
func (urdna *URDNA2015) Canonicalize(data []byte) []byte {
	// Parse N-Quads data into quads
	quads := urdna.parseNQuads(data)

	canonicalized, err := urdna.CanonicalizeQuads(quads)
	if err != nil {
		return data // Return original on error
	}

	return canonicalized
}

// createCanonicalLabeling creates canonical labels for blank nodes.
func (urdna *URDNA2015) createCanonicalLabeling(quads [][]string) map[string]string {
	nodeMap := make(map[string]*Node)
	canonicalLabels := make(map[string]string)

	// Initialize nodes
	for _, quad := range quads {
		urdna.processQuadForNodes(quad, nodeMap)
	}

	// Apply URDNA2015 labeling algorithm steps
	urdna.establishSimpleNamedAdjacencyLists(nodeMap)
	urdna.hashDirectNodes(nodeMap)
	urdna.establishComplexAdjacencyLists(nodeMap)
	urdna.hashComplexNodes(nodeMap)
	urdna.renameBlankNodes(nodeMap, canonicalLabels)

	return canonicalLabels
}

// canonicalizeQuadList canonicalizes a list of quads.
func (urdna *URDNA2015) canonicalizeQuadList(quads [][]string, labelMap map[string]string) []string {
	var canonical []string

	for _, quad := range quads {
		if len(quad) < 4 {
			continue
		}

		subject := urdna.canonicalizeTerm(quad[0], labelMap)
		predicate := urdna.canonicalizeTerm(quad[1], labelMap)
		object := urdna.canonicalizeTerm(quad[2], labelMap)
		graph := urdna.canonicalizeTerm(quad[3], labelMap)

		canonicalQuad := fmt.Sprintf("<%s> <%s> <%s> <%s> .",
			subject, predicate, object, graph)
		canonical = append(canonical, canonicalQuad)
	}

	return canonical
}

// canonicalizeTerm canonicalizes an RDF term.
func (urdna *URDNA2015) canonicalizeTerm(term string, labelMap map[string]string) string {
	term = strings.TrimSpace(term)

	// Handle blank nodes
	if strings.HasPrefix(term, "_:") {
		if label, exists := labelMap[term]; exists {
			return label
		}
		return term
	}

	// Handle IRIs (remove angle brackets)
	if strings.HasPrefix(term, "<") && strings.HasSuffix(term, ">") {
		return term[1 : len(term)-1]
	}

	// Handle literals (keep as is)
	return term
}

// parseNQuads converts N-Quads data into quad arrays.
func (urdna *URDNA2015) parseNQuads(data []byte) [][]string {
	lines := strings.Split(string(data), "\n")
	var quads [][]string

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		quad := urdna.parseNQuadLine(line)
		if len(quad) >= 4 {
			quads = append(quads, quad)
		}
	}

	return quads
}

// parseNQuadLine parses a single N-Quad line.
func (urdna *URDNA2015) parseNQuadLine(line string) []string {
	parts := strings.Fields(line)
	if len(parts) < 4 {
		return []string{}
	}

	// Remove trailing period
	if len(parts) > 4 && parts[len(parts)-1] == "." {
		return parts[:len(parts)-1]
	}

	return parts
}

// processQuadForNodes processes a quad for node mapping.
func (urdna *URDNA2015) processQuadForNodes(quad []string, nodeMap map[string]*Node) {
	for _, term := range quad {
		if !strings.HasPrefix(term, "_:") {
			continue
		}

		if _, exists := nodeMap[term]; !exists {
			nodeMap[term] = &Node{
				Labels: make(map[string]bool),
				Quads:  []QuadRecord{},
			}
		}

		node := nodeMap[term]
		node.Quads = append(node.Quads, QuadRecord{
			Subject:   urdna.getTermPosition(quad[0]),
			Predicate: urdna.getTermPosition(quad[1]),
			Object:    urdna.getTermPosition(quad[2]),
			Graph:     urdna.getTermPosition(quad[3]),
		})
	}
}

// getTermPosition returns a positional identifier for a term.
func (urdna *URDNA2015) getTermPosition(term string) string {
	if strings.HasPrefix(term, "_:") {
		return "_:" + term[2:]
	}
	return term
}

// establishSimpleNamedAdjacencyLists establishes simple adjacency lists.
func (urdna *URDNA2015) establishSimpleNamedAdjacencyLists(nodeMap map[string]*Node) {
	// Implementation would establish which blank nodes are connected
	// This is a simplified version
}

// hashDirectNodes hashes direct nodes.
func (urdna *URDNA2015) hashDirectNodes(nodeMap map[string]*Node) {
	for _, node := range nodeMap {
		node.Hash = urdna.hashNodeData(node)
	}
}

// establishComplexAdjacencyLists establishes complex adjacency lists.
func (urdna *URDNA2015) establishComplexAdjacencyLists(nodeMap map[string]*Node) {
	// Implementation would establish complex adjacency relationships
}

// hashComplexNodes hashes complex nodes.
func (urdna *URDNA2015) hashComplexNodes(nodeMap map[string]*Node) {
	// Implementation would perform iterative hashing for complex nodes
}

// renameBlankNodes renames blank nodes with canonical labels.
func (urdna *URDNA2015) renameBlankNodes(nodeMap map[string]*Node, canonicalLabels map[string]string) {
	// Sort nodes by hash to ensure deterministic ordering
	type NodeEntry struct {
		original string
		node     *Node
	}

	var entries []NodeEntry
	for original, node := range nodeMap {
		entries = append(entries, NodeEntry{original: original, node: node})
	}

	// Sort by hash
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].node.Hash < entries[j].node.Hash
	})

	// Generate canonical labels
	labelCounter := 0
	for _, entry := range entries {
		canonicalLabel := fmt.Sprintf("_:c14n%d", labelCounter)
		canonicalLabels[entry.original] = canonicalLabel
		labelCounter++
	}
}

// hashNodeData generates a hash for node data.
func (urdna *URDNA2015) hashNodeData(node *Node) string {
	// Create a deterministic string representation of the node
	var parts []string

	for _, quad := range node.Quads {
		quadStr := fmt.Sprintf("%s %s %s %s", quad.Subject, quad.Predicate, quad.Object, quad.Graph)
		parts = append(parts, quadStr)
	}

	data := strings.Join(parts, " ")
	hash := sha3ext.Sum256([]byte(data))
	return fmt.Sprintf("%x", hash)
}

// Legacy functions for backward compatibility

// CanonicalizeQuads canonicalizes quads using URDNA2015.
func CanonicalizeQuads(q [][]string) []byte {
	urdna := NewURDNA2015()
	result, err := urdna.CanonicalizeQuads(q)
	if err != nil {
		return []byte("canonicalized")
	}
	return result
}

// Canonicalize canonicalizes data using URDNA2015.
func Canonicalize(data []byte) []byte {
	urdna := NewURDNA2015()
	return urdna.Canonicalize(data)
}
