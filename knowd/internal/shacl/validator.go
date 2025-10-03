package shacl

import (
	"context"
	"encoding/json"
	"fmt"
	"regexp"
	"strings"
	
	"github.com/knakk/rdf"
)

// Validator validates SHACL shapes against RDF data.
type Validator struct {
	shapes map[string]*Shape
}

// ValidationResult represents a SHACL validation result.
type ValidationResult struct {
	Conforms   bool
	Violations []*Violation
}

// NewValidator creates a new SHACL validator.
func NewValidator() *Validator {
	return &Validator{
		shapes: make(map[string]*Shape),
	}
}

// AddShape adds a shape to the validator.
func (v *Validator) AddShape(id string, shape *Shape) {
	v.shapes[id] = shape
}

// Validate validates RDF data against SHACL shapes.
func (v *Validator) Validate(ctx context.Context, data []byte, shapes string) (*ValidationResult, error) {
	// Parse shapes (simplified - in real implementation would parse Turtle/SPARQL)
	err := v.parseShapes(shapes)
	if err != nil {
		return nil, fmt.Errorf("failed to parse shapes: %w", err)
	}

	// Parse data (simplified - in real implementation would parse Turtle/SPARQL)
	dataGraph, err := v.parseData(string(data))
	if err != nil {
		return nil, fmt.Errorf("failed to parse data: %w", err)
	}

	// Perform validation
	result := &ValidationResult{
		Conforms:   true,
		Violations: []*Violation{},
	}

	for _, shape := range v.shapes {
		violations := v.validateShape(ctx, shape, dataGraph)
		if len(violations) > 0 {
			result.Conforms = false
			result.Violations = append(result.Violations, violations...)
		}
	}

	return result, nil
}

// ValidateStrict validates data with strict constraints.
func (v *Validator) ValidateStrict(ctx context.Context, data []byte, shapeID string) (*ValidationResult, error) {
	result, err := v.Validate(ctx, data, "ttl")
	if err != nil {
		return nil, err
	}

	// Apply additional strict validation if shape exists
	if shape, exists := v.shapes[shapeID]; exists {
		if shape.Closed {
			// Check for properties not explicitly allowed
			// This is a simplified implementation
			dataGraph, err := v.parseData(string(data))
			if err == nil {
				for _, quad := range dataGraph {
					subject := quad[0]
					predicate := quad[1]

					// Check if predicate is ignored or allowed
					allowed := false
					for _, prop := range shape.Properties {
						if prop.Path == predicate {
							allowed = true
							break
						}
					}

					// Check if predicate is in ignored list
					for _, ignored := range shape.IgnoredProperties {
						if ignored == predicate {
							allowed = true
							break
						}
					}

					if !allowed && predicate != "rdf:type" {
						result.Conforms = false
						result.Violations = append(result.Violations, &Violation{
							FocusNode:   subject,
							ResultPath:  predicate,
							SourceShape: shapeID,
							Message:     fmt.Sprintf("Unexpected property '%s' in closed shape", predicate),
							Severity:    "Violation",
						})
					}
				}
			}
		}
	}

	return result, nil
}

// parseShapes parses SHACL shapes (simplified implementation).
func (v *Validator) parseShapes(shapes string) error {
	// This is a simplified parser - in production would use proper RDF parsing
	lines := strings.Split(shapes, "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		// Simple parsing for basic SHACL constructs
		if strings.Contains(line, "sh:NodeShape") {
			shape := &Shape{ID: extractID(line), Type: "sh:NodeShape"}
			v.shapes[shape.ID] = shape
		}
	}

	return nil
}

// parseData parses RDF data using knakk/rdf library with JSON-LD support.
func (v *Validator) parseData(data string) ([][]string, error) {
	// Try JSON-LD first if data looks like JSON
	if strings.TrimSpace(data)[0] == '{' || strings.TrimSpace(data)[0] == '[' {
		return v.parseJsonLd(data)
	}
	
	// Use knakk/rdf library to parse Turtle/N-Triples
	triples, err := rdf.NewTripleDecoder(strings.NewReader(data), rdf.Turtle).DecodeAll()
	if err != nil {
		// Fallback to N-Triples if Turtle fails
		triples, err = rdf.NewTripleDecoder(strings.NewReader(data), rdf.NTriples).DecodeAll()
		if err != nil {
			return nil, fmt.Errorf("failed to parse RDF data: %w", err)
		}
	}

	// Convert to simple string array format for compatibility
	var graph [][]string
	for _, triple := range triples {
		graph = append(graph, []string{
			v.termToString(triple.Subj),
			v.termToString(triple.Pred),
			v.termToString(triple.Obj),
		})
	}

	return graph, nil
}

// parseJsonLd parses JSON-LD data and converts to RDF triples.
func (v *Validator) parseJsonLd(data string) ([][]string, error) {
	var jsonld interface{}
	if err := json.Unmarshal([]byte(data), &jsonld); err != nil {
		return nil, fmt.Errorf("failed to parse JSON-LD: %w", err)
	}
	
	var graph [][]string
	
	// Simple JSON-LD to N-Triples conversion
	var jsonObjects []map[string]interface{}
	
	switch obj := jsonld.(type) {
	case []interface{}:
		for _, item := range obj {
			if itemMap, ok := item.(map[string]interface{}); ok {
				jsonObjects = append(jsonObjects, itemMap)
			}
		}
	case map[string]interface{}:
		jsonObjects = append(jsonObjects, obj)
	default:
		return nil, fmt.Errorf("unsupported JSON-LD format")
	}
	
	for _, obj := range jsonObjects {
		for key, value := range obj {
			if key == "@context" {
				continue // Skip context
			}
			
			if id, hasId := obj["@id"]; hasId {
				// Extract subject
				subject := v.jsonToRdfTerm(id)
				
				// Handle @type
				if types, hasType := obj["@type"]; hasType {
					graph = append(graph, []string{
						subject,
						"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
						v.jsonToRdfTerm(types),
					})
				}
				
				// Handle other properties
				if key != "@id" && key != "@type" && key != "@context" {
					predicate := key
					if !strings.HasPrefix(predicate, "http") {
						predicate = "<http://example.org/" + predicate + ">"
					} else {
						predicate = "<" + predicate + ">"
					}
					
					graph = append(graph, []string{
						subject,
						predicate,
						v.jsonToRdfTerm(value),
					})
				}
			}
		}
	}
	
	return graph, nil
}

// jsonToRdfTerm converts a JSON value to RDF term string representation.
func (v *Validator) jsonToRdfTerm(value interface{}) string {
	switch val := value.(type) {
	case string:
		if strings.HasPrefix(val, "@") {
			// JSON-LD keyword
			return "\"" + val + "\""
		} else if strings.HasPrefix(val, "http") {
			return "<" + val + ">"
		} else {
			return "\"" + val + "\""
		}
	case []interface{}:
		// Handle arrays (simplified as space-separated values)
		var parts []string
		for _, item := range val {
			parts = append(parts, v.jsonToRdfTerm(item))
		}
		return "(" + strings.Join(parts, " ") + ")"
	default:
		return "\"" + fmt.Sprintf("%v", val) + "\""
	}
}

// termToString converts an RDF term to string representation.
func (v *Validator) termToString(term rdf.Term) string {
	// For now, use the simple string representation
	// This can be enhanced to properly format different RDF term types
	result := term.String()

	// Ensure IRIs are wrapped in angle brackets for compatibility
	if len(result) > 0 && !strings.HasPrefix(result, "\"") &&
		!strings.HasPrefix(result, "_:") && !strings.HasPrefix(result, "<") {
		result = "<" + result + ">"
	}

	return result
}

// validateShape validates a single shape against the data graph.
func (v *Validator) validateShape(ctx context.Context, shape *Shape, graph [][]string) []*Violation {
	var violations []*Violation

	// Get target nodes for this shape
	targetNodes := v.GetTargetNodes(shape, graph)

	for _, node := range targetNodes {
		// Validate each property constraint
		for _, prop := range shape.Properties {
			propViolations := v.validateProperty(ctx, node, prop, graph)
			violations = append(violations, propViolations...)
		}

		// Validate shape-level constraints
		for _, constraint := range shape.Constraints {
			constraintViolations := v.validateConstraint(ctx, node, constraint, graph)
			violations = append(violations, constraintViolations...)
		}
	}

	return violations
}

// GetTargetNodes gets the nodes that this shape applies to.
func (v *Validator) GetTargetNodes(shape *Shape, graph [][]string) []string {
	var nodes []string

	if shape.TargetClass != "" {
		// Find all nodes that are instances of the target class
		for _, quad := range graph {
			if len(quad) >= 3 && quad[1] == "a" && quad[2] == shape.TargetClass {
				nodes = append(nodes, quad[0])
			}
		}
	}

	if shape.TargetNode != "" {
		nodes = append(nodes, shape.TargetNode)
	}

	// If no specific target, validate all unique subjects
	if len(nodes) == 0 {
		nodeSet := make(map[string]bool)
		for _, quad := range graph {
			if len(quad) > 0 {
				nodeSet[quad[0]] = true
			}
		}
		for node := range nodeSet {
			nodes = append(nodes, node)
		}
	}

	return nodes
}

// validateProperty validates a property shape for a given node.
func (v *Validator) validateProperty(ctx context.Context, node string, prop *PropertyShape, graph [][]string) []*Violation {
	var violations []*Violation

	// Get values for this property path
	values := v.GetPropertyValues(node, prop.Path, graph)

	// Check minCount
	if prop.MinCount > 0 && len(values) < prop.MinCount {
		violations = append(violations, &Violation{
			FocusNode:   node,
			ResultPath:  prop.Path,
			SourceShape: prop.ID,
			Message:     fmt.Sprintf("Property %s has %d values, minimum is %d", prop.Path, len(values), prop.MinCount),
			Severity:    "Violation",
		})
	}

	// Check maxCount
	if prop.MaxCount > 0 && len(values) > prop.MaxCount {
		violations = append(violations, &Violation{
			FocusNode:   node,
			ResultPath:  prop.Path,
			SourceShape: prop.ID,
			Message:     fmt.Sprintf("Property %s has %d values, maximum is %d", prop.Path, len(values), prop.MaxCount),
			Severity:    "Violation",
		})
	}

	// Check length constraints for string values
	for _, value := range values {
		// Check min/max length - extract actual string value from Turtle literal
		if prop.MinLength > 0 || prop.MaxLength > 0 {
			var actualValue string
			if strings.HasPrefix(value, `"`) && strings.HasSuffix(value, `"`) {
				// Remove quotes
				actualValue = value[1 : len(value)-1]
			} else {
				actualValue = value
			}

			length := len(actualValue)

			if prop.MinLength > 0 && length < prop.MinLength {
				violations = append(violations, &Violation{
					FocusNode:   node,
					ResultPath:  prop.Path,
					SourceShape: prop.ID,
					Message:     fmt.Sprintf("String length %d is less than minimum required %d", length, prop.MinLength),
					Severity:    "Violation",
				})
			}

			if prop.MaxLength > 0 && length > prop.MaxLength {
				violations = append(violations, &Violation{
					FocusNode:   node,
					ResultPath:  prop.Path,
					SourceShape: prop.ID,
					Message:     fmt.Sprintf("String length %d exceeds maximum allowed %d", length, prop.MaxLength),
					Severity:    "Violation",
				})
			}
		}

		// Check nodeKind constraints
		if prop.NodeKind != "" {
			if !v.validateNodeKind(value, prop.NodeKind) {
				violations = append(violations, &Violation{
					FocusNode:   node,
					ResultPath:  prop.Path,
					SourceShape: prop.ID,
					Message:     fmt.Sprintf("Value %s does not match nodeKind %s", value, prop.NodeKind),
					Severity:    "Violation",
				})
			}
		}

		// Check pattern constraints
		if prop.Pattern != "" {
			var actualValue string
			if strings.HasPrefix(value, `"`) && strings.HasSuffix(value, `"`) {
				// Remove quotes
				actualValue = value[1 : len(value)-1]
			} else {
				actualValue = value
			}

			if !v.validatePattern(actualValue, prop.Pattern) {
				violations = append(violations, &Violation{
					FocusNode:   node,
					ResultPath:  prop.Path,
					SourceShape: prop.ID,
					Message:     fmt.Sprintf("Value %s does not match pattern %s", actualValue, prop.Pattern),
					Severity:    "Violation",
				})
			}
		}

		// Check datatype constraints
		if prop.Datatype != "" {
			if !v.validateDatatype(value, prop.Datatype) {
				violations = append(violations, &Violation{
					FocusNode:   node,
					ResultPath:  prop.Path,
					SourceShape: prop.ID,
					Message:     fmt.Sprintf("Value %s does not match datatype %s", value, prop.Datatype),
					Severity:    "Violation",
				})
			}
		}
	}

	return violations
}

// GetPropertyValues gets all values for a property path from a node.
func (v *Validator) GetPropertyValues(node, path string, graph [][]string) []string {
	var values []string

	for _, quad := range graph {
		if len(quad) >= 3 && quad[0] == node && quad[1] == path {
			values = append(values, quad[2])
		}
	}

	return values
}

// validateNodeKind validates that a value matches a node kind.
func (v *Validator) validateNodeKind(value, nodeKind string) bool {
	switch nodeKind {
	case "sh:IRI":
		return strings.HasPrefix(value, "<") && strings.HasSuffix(value, ">")
	case "sh:BlankNode":
		return strings.HasPrefix(value, "_:")
	case "sh:Literal":
		return !strings.HasPrefix(value, "<") && !strings.HasPrefix(value, "_:")
	default:
		return true // Unknown node kinds pass for now
	}
}

// validatePattern validates that a value matches a regex pattern.
func (v *Validator) validatePattern(value, pattern string) bool {
	matched, err := regexp.MatchString(pattern, value)
	return err == nil && matched
}

// validateDatatype validates that a value matches a datatype.
func (v *Validator) validateDatatype(value, datatype string) bool {
	// Simplified datatype validation
	switch datatype {
	case "xsd:string":
		return true // All values are strings in this simplified implementation
	case "xsd:integer":
		// Simple check if value consists of digits
		for _, char := range value {
			if char < '0' || char > '9' {
				return false
			}
		}
		return true
	case "xsd:boolean":
		return value == "true" || value == "false"
	default:
		return true // Unknown datatypes pass for now
	}
}

// validateConstraint validates a shape-level constraint.
func (v *Validator) validateConstraint(ctx context.Context, node string, constraint *Constraint, graph [][]string) []*Violation {
	// Simplified constraint validation
	// In a real implementation, this would handle various constraint types
	return []*Violation{}
}

// extractID extracts an ID from a Turtle-like statement.
func extractID(line string) string {
	// Very simplified ID extraction
	if strings.Contains(line, "a sh:NodeShape") {
		parts := strings.Fields(line)
		if len(parts) > 0 {
			return parts[0]
		}
	}
	return "unknown"
}
