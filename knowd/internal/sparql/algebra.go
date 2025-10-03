package sparql

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"github.com/unrdf/knowd/internal/store"
)

// Algebra is a compiled SPARQL query.
type Algebra struct {
	Op         Operator
	Children   []*Algebra
	Operands   map[string]interface{}
	ResultVars []string
}

// Operator represents a SPARQL algebra operator.
type Operator string

const (
	OpSelect    Operator = "SELECT"
	OpAsk       Operator = "ASK"
	OpConstruct Operator = "CONSTRUCT"
	OpFilter    Operator = "FILTER"
	OpJoin      Operator = "JOIN"
	OpLeftJoin  Operator = "LEFT_JOIN"
	OpUnion     Operator = "UNION"
	OpMinus     Operator = "MINUS"
	OpGroup     Operator = "GROUP"
	OpOrder     Operator = "ORDER"
	OpLimit     Operator = "LIMIT"
	OpOffset    Operator = "OFFSET"
	OpBGP       Operator = "BGP"
	OpBind      Operator = "BIND"
	OpValues    Operator = "VALUES"
	OpDistinct  Operator = "DISTINCT"
	OpReduced   Operator = "REDUCED"
	OpSlice     Operator = "SLICE"
	OpProject   Operator = "PROJECT"
)

// CompilePlan compiles a query plan into algebra.
func CompilePlan(plan *Plan) (*Algebra, error) {
	root := &Algebra{}

	switch plan.Type {
	case "SELECT":
		return compileSelect(plan, root)
	case "ASK":
		return compileAsk(plan, root)
	case "CONSTRUCT":
		return compileConstruct(plan, root)
	default:
		return nil, fmt.Errorf("unsupported query type: %s", plan.Type)
	}
}

// compileSelect compiles a SELECT query to algebra.
func compileSelect(plan *Plan, root *Algebra) (*Algebra, error) {
	root.Op = OpSelect
	root.ResultVars = plan.Columns

	// Create BGP operator for basic graph patterns
	if len(plan.Patterns) > 0 {
		bgpOp := &Algebra{
			Op: OpBGP,
			Operands: map[string]interface{}{
				"patterns": plan.Patterns,
			},
		}
		root.Children = append(root.Children, bgpOp)
	}

	// Add filters
	for _, filter := range plan.Filters {
		filterOp := &Algebra{
			Op: OpFilter,
			Operands: map[string]interface{}{
				"expression": filter.Expression,
			},
		}
		root.Children = append(root.Children, filterOp)
	}

	// Add unions if any
	for _, union := range plan.Unions {
		unionOp := &Algebra{
			Op: OpUnion,
			Children: []*Algebra{
				algebraFromBGP(union.Left),
				algebraFromBGP(union.Right),
			},
		}
		root.Children = append(root.Children, unionOp)
	}

	// Add optional patterns
	for _, optional := range plan.Optional {
		optionalOp := &Algebra{
			Op:       OpLeftJoin,
			Children: []*Algebra{algebraFromBGP(optional.Pattern)},
		}
		root.Children = append(root.Children, optionalOp)
	}

	// Add minus patterns
	for _, minus := range plan.Minus {
		minusOp := &Algebra{
			Op:       OpMinus,
			Children: []*Algebra{algebraFromBGP(minus)},
		}
		root.Children = append(root.Children, minusOp)
	}

	// Add bind clauses
	for _, bind := range plan.Bind {
		bindOp := &Algebra{
			Op: OpBind,
			Operands: map[string]interface{}{
				"variable":   bind.Variable,
				"expression": bind.Expression,
			},
		}
		root.Children = append(root.Children, bindOp)
	}

	// Add values clauses
	for _, values := range plan.Values {
		valuesOp := &Algebra{
			Op: OpValues,
			Operands: map[string]interface{}{
				"variables": values.Variables,
				"values":    values.Values,
			},
		}
		root.Children = append(root.Children, valuesOp)
	}

	// Add limit and offset
	if plan.Limit > 0 || plan.Offset > 0 {
		sliceOp := &Algebra{
			Op: OpSlice,
			Operands: map[string]interface{}{
				"limit":  plan.Limit,
				"offset": plan.Offset,
			},
		}
		root.Children = append(root.Children, sliceOp)
	}

	return root, nil
}

// compileAsk compiles an ASK query to algebra.
func compileAsk(plan *Plan, root *Algebra) (*Algebra, error) {
	root.Op = OpAsk

	// Process patterns similar to SELECT but without result variables
	if len(plan.Patterns) > 0 {
		bgpOp := &Algebra{
			Op: OpBGP,
			Operands: map[string]interface{}{
				"patterns": plan.Patterns,
			},
		}
		root.Children = append(root.Children, bgpOp)
	}

	return root, nil
}

// compileConstruct compiles a CONSTRUCT query to algebra.
func compileConstruct(plan *Plan, root *Algebra) (*Algebra, error) {
	root.Op = OpConstruct

	// CONSTRUCT queries use similar patterns to SELECT
	if len(plan.Patterns) > 0 {
		bgpOp := &Algebra{
			Op: OpBGP,
			Operands: map[string]interface{}{
				"patterns": plan.Patterns,
			},
		}
		root.Children = append(root.Children, bgpOp)
	}

	return root, nil
}

// algebraFromBGP creates algebra from a basic graph pattern.
func algebraFromBGP(bgp BasicGraphPattern) *Algebra {
	return &Algebra{
		Op: OpBGP,
		Operands: map[string]interface{}{
			"patterns": []BasicGraphPattern{bgp},
		},
	}
}

// Execute executes the algebra against a store.
func (a *Algebra) Execute(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
	switch a.Op {
	case OpBGP:
		return a.executeBGP(ctx, executor, store)
	case OpJoin:
		return a.executeJoin(ctx, executor, store)
	case OpLeftJoin:
		return a.executeLeftJoin(ctx, executor, store)
	case OpUnion:
		return a.executeUnion(ctx, executor, store)
	case OpMinus:
		return a.executeMinus(ctx, executor, store)
	case OpFilter:
		return a.executeFilter(ctx, executor, store)
	default:
		return nil, fmt.Errorf("unsupported algebra operator: %s", a.Op)
	}
}

// executeBGP executes a basic graph pattern.
func (a *Algebra) executeBGP(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
	_, ok := a.Operands["patterns"].([]BasicGraphPattern)
	if !ok {
		return nil, fmt.Errorf("invalid BGP patterns")
	}

	// For now, return empty result - BGP execution should be handled by the main executor
	return &QueryResponse{
		Rows: []map[string]interface{}{},
		Kind: "sparql-select",
	}, nil
}

// executeJoin executes a join operation.
func (a *Algebra) executeJoin(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
	if len(a.Children) != 2 {
		return nil, fmt.Errorf("join requires exactly 2 children")
	}

	leftResult, err := a.Children[0].Execute(ctx, executor, store)
	if err != nil {
		return nil, err
	}

	rightResult, err := a.Children[1].Execute(ctx, executor, store)
	if err != nil {
		return nil, err
	}

	// Simple nested loop join implementation
	var joinedRows []map[string]interface{}
	for _, leftRow := range leftResult.Rows {
		for _, rightRow := range rightResult.Rows {
			// Merge rows (overlapping variables create conflicts)
			mergedRow := make(map[string]interface{})
			for k, v := range leftRow {
				mergedRow[k] = v
			}
			for k, v := range rightRow {
				mergedRow[k] = v
			}
			joinedRows = append(joinedRows, mergedRow)
		}
	}

	return &QueryResponse{
		Rows: joinedRows,
		Kind: leftResult.Kind,
	}, nil
}

// executeLeftJoin executes a left join operation.
func (a *Algebra) executeLeftJoin(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
	if len(a.Children) != 1 {
		return nil, fmt.Errorf("left join requires exactly 1 child")
	}

	result, err := a.Children[0].Execute(ctx, executor, store)
	if err != nil {
		return nil, err
	}

	// Left join preserves all left results, even if right side is empty
	return result, nil
}

// executeUnion executes a union operation.
func (a *Algebra) executeUnion(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
	if len(a.Children) != 2 {
		return nil, fmt.Errorf("union requires exactly 2 children")
	}

	leftResult, err := a.Children[0].Execute(ctx, executor, store)
	if err != nil {
		return nil, err
	}

	rightResult, err := a.Children[1].Execute(ctx, executor, store)
	if err != nil {
		return nil, err
	}

	// Combine results (simple implementation - could dedupe)
	var unionRows []map[string]interface{}
	unionRows = append(unionRows, leftResult.Rows...)
	unionRows = append(unionRows, rightResult.Rows...)

	return &QueryResponse{
		Rows: unionRows,
		Kind: leftResult.Kind,
	}, nil
}

// executeMinus executes a minus operation.
func (a *Algebra) executeMinus(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
	if len(a.Children) != 1 {
		return nil, fmt.Errorf("minus requires exactly 1 child")
	}

	// For minus, we need a base pattern to subtract from
	// This is a simplified implementation
	result, err := a.Children[0].Execute(ctx, executor, store)
	if err != nil {
		return nil, err
	}

	// In a real implementation, this would subtract matching rows
	// For now, return empty result
	return &QueryResponse{
		Rows: []map[string]interface{}{},
		Kind: result.Kind,
	}, nil
}

// executeFilter executes a filter operation.
func (a *Algebra) executeFilter(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
	if len(a.Children) != 1 {
		return nil, fmt.Errorf("filter requires exactly 1 child")
	}

	result, err := a.Children[0].Execute(ctx, executor, store)
	if err != nil {
		return nil, err
	}

	expression, ok := a.Operands["expression"].(string)
	if !ok {
		return result, nil
	}

	// Apply filter to results
	var filteredRows []map[string]interface{}
	for _, row := range result.Rows {
		// Simple filter evaluation
		if a.evaluateFilterExpression(expression, row) {
			filteredRows = append(filteredRows, row)
		}
	}

	return &QueryResponse{
		Rows: filteredRows,
		Kind: result.Kind,
	}, nil
}

// evaluateFilterExpression evaluates a filter expression against a row.
func (a *Algebra) evaluateFilterExpression(expression string, row map[string]interface{}) bool {
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

// parseNumber attempts to parse a string as a number.
func parseNumber(s string) (float64, error) {
	// Simple number parsing (could be enhanced)
	if s == "" {
		return 0, fmt.Errorf("empty string")
	}

	// Try to parse as float
	result, err := strconv.ParseFloat(s, 64)
	return result, err
}

// String returns a string representation of the algebra.
func (a *Algebra) String() string {
	if a == nil {
		return "nil"
	}

	var buf strings.Builder
	buf.WriteString(string(a.Op))

	if len(a.Children) > 0 {
		buf.WriteString("(")
		for i, child := range a.Children {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString(child.String())
		}
		buf.WriteString(")")
	}

	if len(a.ResultVars) > 0 {
		buf.WriteString(" [")
		buf.WriteString(strings.Join(a.ResultVars, ", "))
		buf.WriteString("]")
	}

	return buf.String()
}
