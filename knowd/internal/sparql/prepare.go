package sparql

import (
	"context"
	"encoding/json"
	"fmt"
	"regexp"

	"github.com/unrdf/knowd/internal/store"
)

// Prepare is a prepared SPARQL query.
type Prepare struct {
	plan         *Plan
	variables    []string
	placeholders []Placeholder
}

// Placeholder represents a placeholder in a prepared query.
type Placeholder struct {
	Name   string
	Index  int
	Type   string // variable, literal, iri
	Column string
}

// PreparedBinding represents bindings for a prepared query.
type PreparedBinding map[string]interface{}

// NewPrepare creates a new prepared query from a plan.
func NewPrepare(plan *Plan) (*Prepare, error) {
	p := &Prepare{
		plan:         plan,
		variables:    []string{},
		placeholders: []Placeholder{},
	}

	// Extract placeholders from the query
	if err := p.extractPlaceholders(); err != nil {
		return nil, err
	}

	return p, nil
}

// extractPlaceholders extracts placeholders from the query plan.
func (p *Prepare) extractPlaceholders() error {
	// Simple placeholder extraction - look for patterns like :param
	placeholderRe := regexp.MustCompile(`:([a-zA-Z][a-zA-Z0-9_]*)`)

	// Process SELECT columns
	for _, column := range p.plan.Columns {
		matches := placeholderRe.FindAllStringSubmatch(column, -1)
		for i, match := range matches {
			p.placeholders = append(p.placeholders, Placeholder{
				Name:   match[1],
				Index:  i,
				Type:   "variable",
				Column: column,
			})
			p.variables = append(p.variables, match[1])
		}
	}

	// Process basic graph patterns
	for _, bgp := range p.plan.Patterns {
		for _, triple := range bgp.Triples {
			// Check subject, predicate, object for placeholders
			if triple.Subject != "" {
				p.checkPlaceholder(triple.Subject)
			}
			if triple.Predicate != "" {
				p.checkPlaceholder(triple.Predicate)
			}
			if triple.Object != "" {
				p.checkPlaceholder(triple.Object)
			}
		}
	}

	return nil
}

// checkPlaceholder checks if a term contains placeholders.
func (p *Prepare) checkPlaceholder(term string) {
	placeholderRe := regexp.MustCompile(`:([a-zA-Z][a-zA-Z0-9_]*)`)
	matches := placeholderRe.FindAllStringSubmatch(term, -1)

	for i, match := range matches {
		p.placeholders = append(p.placeholders, Placeholder{
			Name:   match[1],
			Index:  i,
			Type:   "variable",
			Column: term,
		})
		p.variables = append(p.variables, match[1])
	}
}

// Execute executes the prepared query with bindings.
func (p *Prepare) Execute(bindings PreparedBinding, executor *Executor, store store.Interface) (*QueryResponse, error) {
	// Create a copy of the plan for execution
	executionPlan := p.copyPlan()

	// Apply bindings to placeholders
	if err := p.applyBindings(executionPlan, bindings); err != nil {
		return nil, err
	}

	// Execute the plan
	executor := NewExecutor() // No cache for prepared execution
	return executor.Execute(context.Background(), store, "sparql-select", p.plan)
}

// applyBindings applies bindings to the execution plan.
func (p *Prepare) applyBindings(plan *Plan, bindings PreparedBinding) error {
	// Apply bindings to SELECT columns
	for i, column := range plan.Columns {
		for varName, value := range bindings {
			placeholderRe := regexp.MustCompile(fmt.Sprintf("\\b:%s\\b", varName))
			if placeholderRe.MatchString(column) {
				// Replace placeholder with bound value
				newColumn := placeholderRe.ReplaceAllString(column, fmt.Sprintf("\"%v\"", value))
				plan.Columns[i] = newColumn
			}
		}
	}

	// Apply bindings to basic graph patterns
	for bgpIndex, bgp := range plan.Patterns {
		for tripleIndex, triple := range bgp.Triples {
			newTriple := p.applyBindingToTriple(triple, bindings)
			plan.Patterns[bgpIndex].Triples[tripleIndex] = newTriple
		}
	}

	return nil
}

// applyBindingToTriple applies bindings to a triple.
func (p *Prepare) applyBindingToTriple(triple Triple, bindings PreparedBinding) Triple {
	newTriple := triple

	for varName, value := range bindings {
		if placeholderRe := regexp.MustCompile(fmt.Sprintf("\\b:%s\\b", varName)); placeholderRe.MatchString(triple.Subject) {
			newTriple.Subject = placeholderRe.ReplaceAllString(triple.Subject, fmt.Sprintf("\"%v\"", value))
		}

		if placeholderRe := regexp.MustCompile(fmt.Sprintf("\\b:%s\\b", varName)); placeholderRe.MatchString(triple.Predicate) {
			newTriple.Predicate = placeholderRe.ReplaceAllString(triple.Predicate, fmt.Sprintf("\"%v\"", value))
		}

		if placeholderRe := regexp.MustCompile(fmt.Sprintf("\\b:%s\\b", varName)); placeholderRe.MatchString(triple.Object) {
			newTriple.Object = placeholderRe.ReplaceAllString(triple.Object, fmt.Sprintf("\"%v\"", value))
		}
	}

	return newTriple
}

// copyPlan creates a deep copy of the plan.
func (p *Prepare) copyPlan() *Plan {
	// Simple copy for now - could be enhanced for deep copying
	newPlan := &Plan{
		Type:     p.plan.Type,
		Columns:  make([]string, len(p.plan.Columns)),
		Patterns: make([]BasicGraphPattern, len(p.plan.Patterns)),
		Filters:  make([]Filter, len(p.plan.Filters)),
		Limit:    p.plan.Limit,
		Offset:   p.plan.Offset,
	}

	copy(newPlan.Columns, p.plan.Columns)
	copy(newPlan.Patterns, p.plan.Patterns)
	copy(newPlan.Filters, p.plan.Filters)

	return newPlan
}

// GetVariables returns the variables in the prepared query.
func (p *Prepare) GetVariables() []string {
	return p.variables
}

// Marshal returns serialized form of the prepared query.
func (p *Prepare) Marshal() ([]byte, error) {
	data := map[string]interface{}{
		"variables":    p.variables,
		"placeholders": p.placeholders,
		"plan":         p.plan,
	}

	return json.Marshal(data)
}

// Unmarshal loads a prepared query from serialized data.
func UnmarshalPrepare(data []byte, parser *Parser) (*Prepare, error) {
	var marshaled map[string]interface{}
	if err := json.Unmarshal(data, &marshaled); err != nil {
		return nil, fmt.Errorf("failed to unmarshal prepared query: %w", err)
	}

	// This is simplified - in production you'd want full restoration
	return &Prepare{
		plan:         &Plan{},
		variables:    []string{},
		placeholders: []Placeholder{},
	}, nil
}
