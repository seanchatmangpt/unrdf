// Package sparql provides SPARQL query execution for knowd.
package sparql

import (
	"context"
	"fmt"
	"strings"

	"github.com/unrdf/knowd/internal/store"
	"github.com/unrdf/knowd/internal/telemetry"
	"go.opentelemetry.io/otel/attribute"
)

// QueryResponse represents a query response.
type QueryResponse struct {
	Rows []map[string]interface{} `json:"rows"`
	Kind string                   `json:"kind"`
}

// Executor executes compiled SPARQL plans against a store.
type Executor struct {
	cache *PlanCache
}

// NewExecutor creates a new SPARQL executor.
func NewExecutor() *Executor {
	return &Executor{}
}

// NewExecutorWithCache creates a new SPARQL executor with a plan cache.
func NewExecutorWithCache(cache *PlanCache) *Executor {
	return &Executor{
		cache: cache,
	}
}

// Query executes a SPARQL query string against a store, using caching when possible.
func (e *Executor) Query(ctx context.Context, queryStr string, store store.Interface) (*QueryResponse, error) {
	// Parse and execute the query
	parser := NewParser()
	plan, err := parser.Parse(queryStr)
	if err != nil {
		return nil, err
	}

	return e.Execute(ctx, store, plan.Type, plan)
}

// Execute executes a plan against a store.
func (e *Executor) Execute(ctx context.Context, storeInstance store.Interface, queryKind string, plan *Plan) (*QueryResponse, error) {
	ctx, span := telemetry.StartSpan(ctx, "sparql.execute")
	defer span.End()

	telemetry.AddEvent(ctx, "execute.started",
		attribute.String("query.type", plan.Type),
		attribute.String("query.kind", queryKind),
		attribute.Int("patterns.count", len(plan.Patterns)),
		attribute.Int("filters.count", len(plan.Filters)))

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
	ctx, span := telemetry.StartSpan(ctx, "sparql.execute.select")
	defer span.End()

	telemetry.AddEvent(ctx, "select.started",
		attribute.Int("columns.count", len(plan.Columns)),
		attribute.Int("patterns.count", len(plan.Patterns)))

	var rows []map[string]interface{}

	// For each basic graph pattern
	for _, bgp := range plan.Patterns {
		// For each triple pattern in the BGP
		for _, triple := range bgp.Triples {
			// Build search pattern
			pattern := store.Quad{
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

			// For each matching quad, create a result row
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
				if len(plan.Filters) == 0 || e.applyFilters(row, plan.Filters) {
					rows = append(rows, row)
				}
			}
		}
	}

	// Apply GROUP BY if specified
	if len(plan.GroupBy) > 0 {
		rows = e.applyGroupBy(rows, plan.GroupBy)
	}

	// Apply ORDER BY if specified
	if len(plan.OrderBy) > 0 {
		rows = e.applyOrderBy(rows, plan.OrderBy)
	}

	// Apply HAVING if specified
	if len(plan.Having) > 0 {
		rows = e.applyHaving(rows, plan.Having)
	}

	// Apply DISTINCT/REDUCED if specified
	if plan.Type == "SELECT" && strings.Contains(strings.Join(plan.Columns, " "), "DISTINCT") {
		rows = e.applyDistinct(rows)
	}

	// Apply LIMIT/OFFSET
	rows = e.applySlice(rows, plan.Offset, plan.Limit)

	return &QueryResponse{
		Rows: rows,
		Kind: "sparql-select",
	}, nil
}

// executeAsk executes an ASK query.
func (e *Executor) executeAsk(ctx context.Context, storeInstance store.Interface, plan *Plan) (*QueryResponse, error) {
	// ASK queries check if the pattern has any matches
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

	// Find all matching triples for the WHERE clause
	for _, bgp := range plan.Patterns {
		for _, triple := range bgp.Triples {
			// Build search pattern
			pattern := store.Quad{
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
	expression = strings.TrimSpace(expression)

	// Handle greater than
	if strings.Contains(expression, ">") {
		parts := strings.Split(expression, ">")
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])

			if value, exists := row[left]; exists {
				rightValue := right
				if strings.HasPrefix(right, "\"") && strings.HasSuffix(right, "\"") {
					rightValue = right[1 : len(right)-1]
				}

				// Try to parse as numbers
				leftNum, leftErr := parseNumber(fmt.Sprintf("%v", value))
				rightNum, rightErr := parseNumber(rightValue)

				if leftErr == nil && rightErr == nil {
					return leftNum > rightNum
				}

				// Fallback to string comparison
				return fmt.Sprintf("%v", value) > rightValue
			}
		}
	}

	// Handle equality
	if strings.Contains(expression, "=") {
		parts := strings.Split(expression, "=")
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])

			if value, exists := row[left]; exists {
				// Handle quoted strings in the right side
				rightValue := right
				if strings.HasPrefix(right, "\"") && strings.HasSuffix(right, "\"") {
					rightValue = right[1 : len(right)-1]
				}
				return fmt.Sprintf("%v", value) == rightValue
			}
		}
	}

	// Default to true for unsupported filters
	return true
}

// applyGroupBy applies GROUP BY clause to results.
func (e *Executor) applyGroupBy(rows []map[string]interface{}, groupBy []string) []map[string]interface{} {
	if len(rows) == 0 || len(groupBy) == 0 {
		return rows
	}

	// Simple implementation - group by first variable and aggregate others
	groups := make(map[string][]map[string]interface{})
	for _, row := range rows {
		key := ""
		for _, groupVar := range groupBy {
			if val, exists := row[groupVar]; exists {
				key += fmt.Sprintf("%v", val)
			}
		}
		groups[key] = append(groups[key], row)
	}

	// Return first row from each group (simplified)
	var result []map[string]interface{}
	for _, group := range groups {
		if len(group) > 0 {
			result = append(result, group[0])
		}
	}
	return result
}

// applyOrderBy applies ORDER BY clause to results.
func (e *Executor) applyOrderBy(rows []map[string]interface{}, orderBy []OrderByItem) []map[string]interface{} {
	if len(rows) == 0 || len(orderBy) == 0 {
		return rows
	}

	// Simple bubble sort implementation
	for i := 0; i < len(rows); i++ {
		for j := 0; j < len(rows)-i-1; j++ {
			left := rows[j]
			right := rows[j+1]

			shouldSwap := false
			for _, item := range orderBy {
				leftVal, leftExists := left[item.Variable]
				rightVal, rightExists := right[item.Variable]

				if !leftExists && !rightExists {
					continue
				}
				if !leftExists {
					shouldSwap = true
					break
				}
				if !rightExists {
					shouldSwap = false
					break
				}

				if item.Descending {
					if fmt.Sprintf("%v", leftVal) < fmt.Sprintf("%v", rightVal) {
						shouldSwap = true
						break
					}
				} else {
					if fmt.Sprintf("%v", leftVal) > fmt.Sprintf("%v", rightVal) {
						shouldSwap = true
						break
					}
				}
			}

			if shouldSwap {
				rows[j], rows[j+1] = rows[j+1], rows[j]
			}
		}
	}

	return rows
}

// applyHaving applies HAVING clause to results.
func (e *Executor) applyHaving(rows []map[string]interface{}, having []Filter) []map[string]interface{} {
	var filtered []map[string]interface{}
	for _, row := range rows {
		if e.evaluateFilter(having[0].Expression, row) {
			filtered = append(filtered, row)
		}
	}
	return filtered
}

// applyDistinct applies DISTINCT to results.
func (e *Executor) applyDistinct(rows []map[string]interface{}) []map[string]interface{} {
	seen := make(map[string]bool)
	var distinct []map[string]interface{}

	for _, row := range rows {
		key := fmt.Sprintf("%v", row)
		if !seen[key] {
			seen[key] = true
			distinct = append(distinct, row)
		}
	}

	return distinct
}

// applySlice applies LIMIT and OFFSET to results.
func (e *Executor) applySlice(rows []map[string]interface{}, offset, limit int) []map[string]interface{} {
	start := offset
	if start > len(rows) {
		start = len(rows)
	}

	end := len(rows)
	if limit > 0 && start+limit < len(rows) {
		end = start + limit
	}

	return rows[start:end]
}
