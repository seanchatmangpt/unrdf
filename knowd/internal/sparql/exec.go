// Package sparql provides SPARQL query execution for knowd.
package sparql

import (
	"context"
	"fmt"
	"strings"

	"github.com/unrdf/knowd/internal/store"
)

// Quad represents an RDF quad for local use
type Quad struct {
	Subject   string
	Predicate string
	Object    string
	Graph     string
}

// QueryResponse represents a query response.
type QueryResponse struct {
	Rows []map[string]interface{} `json:"rows"`
	Kind string                   `json:"kind"`
}

// Executor executes compiled SPARQL plans against a store.
type Executor struct{}

// NewExecutor creates a new SPARQL executor.
func NewExecutor() *Executor {
	return &Executor{}
}

// Execute executes a plan against a store.
func (e *Executor) Execute(ctx context.Context, storeInstance store.Interface, queryKind string, plan *Plan) (*QueryResponse, error) {
	switch plan.Type {
	case "SELECT":
		return e.executeSelect(ctx, storeInstance, plan)
	case "ASK":
		return e.executeAsk(ctx, storeInstance, plan)
	case "CONSTRUCT":
		return e.executeConstruct(ctx, storeInstance, plan)
	default:
		return nil, fmt.Errorf("unsupported query type: %s", plan.Type)
	}
}

// executeSelect executes a SELECT query.
func (e *Executor) executeSelect(ctx context.Context, storeInstance store.Interface, plan *Plan) (*QueryResponse, error) {
	var rows []map[string]interface{}

	for _, bgp := range plan.Patterns {
		for _, triple := range bgp.Triples {
			// Build search pattern
			pattern := Quad{
				Subject:   e.resolveTerm(triple.Subject),
				Predicate: e.resolveTerm(triple.Predicate),
				Object:    e.resolveTerm(triple.Object),
				Graph:     "default",
			}

			// Find matching quads
			matches, err := storeInstance.FindQuads(ctx, pattern)
			if err != nil {
				continue
			}

			for _, quad := range matches {
				row := make(map[string]interface{})

				// Add variables to result
				if strings.HasPrefix(triple.Subject, "?") {
					row[triple.Subject] = quad.Subject
				}
				if strings.HasPrefix(triple.Predicate, "?") {
					row[triple.Predicate] = quad.Predicate
				}
				if strings.HasPrefix(triple.Object, "?") {
					row[triple.Object] = quad.Object
				}

				// Apply filters if any
				if e.applyFilters(row, plan.Filters) {
					rows = append(rows, row)
				}
			}

			// Apply LIMIT/OFFSET
			if plan.Offset > 0 && len(rows) > plan.Offset {
				rows = rows[plan.Offset:]
			}
			if plan.Limit > 0 && len(rows) > plan.Limit {
				rows = rows[:plan.Limit]
			}
		}
	}

	return &QueryResponse{
		Rows: rows,
		Kind: "sparql-select",
	}, nil
}

// executeAsk executes an ASK query.
func (e *Executor) executeAsk(ctx context.Context, storeInstance store.Interface, plan *Plan) (*QueryResponse, error) {
	for _, bgp := range plan.Patterns {
		for _, triple := range bgp.Triples {
			// Check if pattern exists
			exists, err := storeInstance.HasQuad(ctx, store.Quad{
				Subject:   e.resolveTerm(triple.Subject),
				Predicate: e.resolveTerm(triple.Predicate),
				Object:    e.resolveTerm(triple.Object),
				Graph:     "default",
			})

			if err != nil {
				continue
			}

			if exists {
				return &QueryResponse{
					Rows: []map[string]interface{}{
						{"result": true},
					},
					Kind: "sparql-ask",
				}, nil
			}
		}
	}

	return &QueryResponse{
		Rows: []map[string]interface{}{
			{"result": false},
		},
		Kind: "sparql-ask",
	}, nil
}

// executeConstruct executes a CONSTRUCT query.
func (e *Executor) executeConstruct(ctx context.Context, storeInstance store.Interface, plan *Plan) (*QueryResponse, error) {
	var constructed []map[string]interface{}
	for _, bgp := range plan.Patterns {
		for _, triple := range bgp.Triples {
			// Build search pattern
			pattern := Quad{
				Subject:   e.resolveTerm(triple.Subject),
				Predicate: e.resolveTerm(triple.Predicate),
				Object:    e.resolveTerm(triple.Object),
				Graph:     "default",
			}

			// Find matching quads
			matches, err := storeInstance.FindQuads(ctx, pattern)
			if err != nil {
				continue
			}

			for _, quad := range matches {
				tripleMap := map[string]interface{}{
					"subject":   quad.Subject,
					"predicate": quad.Predicate,
					"object":    quad.Object,
				}
				constructed = append(constructed, tripleMap)
			}
		}
	}

	var rows []map[string]interface{}
	for _, item := range constructed {
		rows = append(rows, item)
	}

	return &QueryResponse{
		Rows: rows,
		Kind: "sparql-construct",
	}, nil
}

// resolveTerm resolves a SPARQL term (variable or literal).
func (e *Executor) resolveTerm(term string) string {
	if strings.HasPrefix(term, "?") {
		// Variable - return empty to match any
		return ""
	}
	if strings.HasPrefix(term, "<") && strings.HasSuffix(term, ">") {
		// IRI - return as is except for angle brackets
		return term[1 : len(term)-1]
	}
	// Literal
	return term
}

// applyFilters applies FILTER expressions to a result row.
func (e *Executor) applyFilters(row map[string]interface{}, filters []Filter) bool {
	for _, filter := range filters {
		if !e.evaluateFilter(filter.Expression, row) {
			return false
		}
	}
	return true
}

// evaluateFilter evaluates a FILTER expression (simplified implementation).
func (e *Executor) evaluateFilter(expression string, row map[string]interface{}) bool {
	// Simple equality check
	if strings.Contains(expression, "=") {
		parts := strings.Split(expression, "=")
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])

			if value, exists := row[left]; exists {
				return fmt.Sprintf("%v", value) == right
			}
		}
	}

	// Default to true for unsupported filters
	return true
}
