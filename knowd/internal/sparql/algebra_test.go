package sparql

import (
	"context"
	"testing"

	"github.com/unrdf/knowd/internal/store"
)

// MockStore implements store.Interface for testing
type MockStore struct {
	quads []store.Quad
}

func (m *MockStore) AddQuad(ctx context.Context, quad store.Quad) error {
	m.quads = append(m.quads, quad)
	return nil
}

func (m *MockStore) RemoveQuad(ctx context.Context, quad store.Quad) error {
	for i, q := range m.quads {
		if q == quad {
			m.quads = append(m.quads[:i], m.quads[i+1:]...)
			break
		}
	}
	return nil
}

func (m *MockStore) HasQuad(ctx context.Context, quad store.Quad) (bool, error) {
	for _, q := range m.quads {
		if q == quad {
			return true, nil
		}
	}
	return false, nil
}

func (m *MockStore) FindQuads(ctx context.Context, pattern store.Quad) ([]store.Quad, error) {
	var matches []store.Quad
	for _, q := range m.quads {
		if m.matches(pattern, q) {
			matches = append(matches, q)
		}
	}
	return matches, nil
}

func (m *MockStore) matches(pattern, quad store.Quad) bool {
	if pattern.Subject != "" && pattern.Subject != quad.Subject {
		return false
	}
	if pattern.Predicate != "" && pattern.Predicate != quad.Predicate {
		return false
	}
	if pattern.Object != "" && pattern.Object != quad.Object {
		return false
	}
	if pattern.Graph != "" && pattern.Graph != quad.Graph {
		return false
	}
	return true
}

func (m *MockStore) GetQuadCount() int {
	return len(m.quads)
}

func (m *MockStore) Close() error {
	return nil
}

func TestAlgebra_CompilePlan(t *testing.T) {
	tests := []struct {
		name    string
		plan    *Plan
		wantOp  Operator
		wantErr bool
	}{
		{
			name: "SELECT plan",
			plan: &Plan{
				Type:    "SELECT",
				Columns: []string{"?s", "?p", "?o"},
				Patterns: []BasicGraphPattern{
					{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
				},
			},
			wantOp:  OpSelect,
			wantErr: false,
		},
		{
			name: "ASK plan",
			plan: &Plan{
				Type: "ASK",
				Patterns: []BasicGraphPattern{
					{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
				},
			},
			wantOp:  OpAsk,
			wantErr: false,
		},
		{
			name: "CONSTRUCT plan",
			plan: &Plan{
				Type: "CONSTRUCT",
				Patterns: []BasicGraphPattern{
					{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
				},
			},
			wantOp:  OpConstruct,
			wantErr: false,
		},
		{
			name:    "unsupported type",
			plan:    &Plan{Type: "UNKNOWN"},
			wantOp:  "",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			algebra, err := CompilePlan(tt.plan)
			if (err != nil) != tt.wantErr {
				t.Errorf("CompilePlan() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err != nil {
				return
			}
			if algebra.Op != tt.wantOp {
				t.Errorf("CompilePlan() op = %v, want %v", algebra.Op, tt.wantOp)
			}
		})
	}
}

func TestAlgebra_ExecuteBGP(t *testing.T) {
	store := &MockStore{
		quads: []store.Quad{
			{Subject: "http://example.org/s", Predicate: "http://example.org/p", Object: "http://example.org/o", Graph: "default"},
		},
	}

	algebra := &Algebra{
		Op: OpBGP,
		Operands: map[string]interface{}{
			"patterns": []BasicGraphPattern{
				{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
			},
		},
	}

	executor := NewExecutor()
	result, err := algebra.executeBGP(context.Background(), executor, store)
	if err != nil {
		t.Errorf("executeBGP() error = %v", err)
		return
	}

	if len(result.Rows) != 1 {
		t.Errorf("executeBGP() rows = %v, want 1", len(result.Rows))
	}

	if result.Rows[0]["?s"] != "http://example.org/s" {
		t.Errorf("executeBGP() subject = %v, want http://example.org/s", result.Rows[0]["?s"])
	}
}

func TestAlgebra_ExecuteJoin(t *testing.T) {
	leftAlgebra := &Algebra{
		Op: OpBGP,
		Operands: map[string]interface{}{
			"patterns": []BasicGraphPattern{
				{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
			},
		},
	}

	rightAlgebra := &Algebra{
		Op: OpBGP,
		Operands: map[string]interface{}{
			"patterns": []BasicGraphPattern{
				{Triples: []Triple{{Subject: "?s", Predicate: "?p2", Object: "?o2"}}},
			},
		},
	}

	joinAlgebra := &Algebra{
		Op:       OpJoin,
		Children: []*Algebra{leftAlgebra, rightAlgebra},
	}

	// Mock the execution to return some test data
	originalExecute := joinAlgebra.Children[0].Execute
	leftAlgebra.Execute = func(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
		return &QueryResponse{
			Rows: []map[string]interface{}{
				{"?s": "s1", "?p": "p1", "?o": "o1"},
			},
			Kind: "sparql-select",
		}, nil
	}

	rightAlgebra.Execute = func(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
		return &QueryResponse{
			Rows: []map[string]interface{}{
				{"?s": "s1", "?p2": "p2", "?o2": "o2"},
			},
			Kind: "sparql-select",
		}, nil
	}

	executor := NewExecutor()
	result, err := joinAlgebra.Execute(context.Background(), executor, nil)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
		return
	}

	if len(result.Rows) != 1 {
		t.Errorf("Execute() rows = %v, want 1", len(result.Rows))
	}

	row := result.Rows[0]
	if row["?s"] != "s1" || row["?p"] != "p1" || row["?o"] != "o1" || row["?p2"] != "p2" || row["?o2"] != "o2" {
		t.Errorf("Execute() row = %v, want merged row", row)
	}

	// Restore original method
	leftAlgebra.Execute = originalExecute
}

func TestAlgebra_String(t *testing.T) {
	algebra := &Algebra{
		Op:         OpSelect,
		ResultVars: []string{"?s", "?p"},
		Children: []*Algebra{
			{Op: OpBGP},
		},
	}

	str := algebra.String()
	expected := "SELECT(BGP) [?s, ?p]"
	if str != expected {
		t.Errorf("String() = %v, want %v", str, expected)
	}
}

func TestAlgebra_EvaluateFilterExpression(t *testing.T) {
	algebra := &Algebra{}

	tests := []struct {
		expression string
		row        map[string]interface{}
		want       bool
	}{
		{"?x = \"value\"", map[string]interface{}{"?x": "value"}, true},
		{"?x = \"value\"", map[string]interface{}{"?x": "other"}, false},
		{"?x = ?y", map[string]interface{}{"?x": "test", "?y": "test"}, true},
		{"?x = ?y", map[string]interface{}{"?x": "test", "?y": "other"}, false},
	}

	for _, tt := range tests {
		got := algebra.evaluateFilterExpression(tt.expression, tt.row)
		if got != tt.want {
			t.Errorf("evaluateFilterExpression(%v, %v) = %v, want %v", tt.expression, tt.row, got, tt.want)
		}
	}
}
