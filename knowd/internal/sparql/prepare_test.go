package sparql

import (
	"testing"

	"github.com/unrdf/knowd/internal/store"
)

func TestPrepare_NewPrepare(t *testing.T) {
	plan := &Plan{
		Type:    "SELECT",
		Columns: []string{"?s", ":param"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: ":param", Object: "?o"}}},
		},
	}

	prepared, err := NewPrepare(plan)
	if err != nil {
		t.Errorf("NewPrepare() error = %v", err)
		return
	}

	if len(prepared.placeholders) == 0 {
		t.Errorf("NewPrepare() should find at least one placeholder")
		return
	}

	// Check that we found the param placeholder
	found := false
	for _, ph := range prepared.placeholders {
		if ph.Name == "param" {
			found = true
			break
		}
	}

	if !found {
		t.Errorf("NewPrepare() should find param placeholder")
	}
}

func TestPrepare_Execute(t *testing.T) {
	plan := &Plan{
		Type:    "SELECT",
		Columns: []string{"?s", "?o"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: ":param", Object: "?o"}}},
		},
	}

	prepared, err := NewPrepare(plan)
	if err != nil {
		t.Errorf("NewPrepare() error = %v", err)
		return
	}

	store := &MockStore{
		quads: []store.Quad{
			{Subject: "http://example.org/s", Predicate: "http://example.org/p", Object: "http://example.org/o", Graph: "default"},
		},
	}

	bindings := PreparedBinding{
		"param": "http://example.org/p",
	}

	executor := NewExecutor()
	result, err := prepared.Execute(bindings, executor, store)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
		return
	}

	if len(result.Rows) != 1 {
		t.Errorf("Execute() rows = %v, want 1", len(result.Rows))
	}
}

func TestPrepare_ApplyBindings(t *testing.T) {
	plan := &Plan{
		Type:    "SELECT",
		Columns: []string{"?s", ":param"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: ":param", Object: "?o"}}},
		},
	}

	prepared, err := NewPrepare(plan)
	if err != nil {
		t.Errorf("NewPrepare() error = %v", err)
		return
	}

	bindings := PreparedBinding{
		"param": "bound_value",
	}

	executionPlan := prepared.copyPlan()
	err = prepared.applyBindings(executionPlan, bindings)
	if err != nil {
		t.Errorf("applyBindings() error = %v", err)
		return
	}

	// Check if binding was applied
	if executionPlan.Columns[1] != "\"bound_value\"" {
		t.Errorf("applyBindings() column = %v, want \"bound_value\"", executionPlan.Columns[1])
	}
}

func TestPrepare_ExtractPlaceholders(t *testing.T) {
	plan := &Plan{
		Type:    "SELECT",
		Columns: []string{":param1", "?s"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: ":param2", Object: ":param3"}}},
		},
	}

	prepared := &Prepare{
		plan:         plan,
		variables:    []string{},
		placeholders: []Placeholder{},
	}

	err := prepared.extractPlaceholders()
	if err != nil {
		t.Errorf("extractPlaceholders() error = %v", err)
		return
	}

	if len(prepared.placeholders) != 3 {
		t.Errorf("extractPlaceholders() placeholders = %v, want 3", len(prepared.placeholders))
	}

	expectedNames := []string{"param1", "param2", "param3"}
	for i, placeholder := range prepared.placeholders {
		if placeholder.Name != expectedNames[i] {
			t.Errorf("extractPlaceholders() placeholder[%d].Name = %v, want %v", i, placeholder.Name, expectedNames[i])
		}
	}
}

func TestPrepare_CopyPlan(t *testing.T) {
	original := &Plan{
		Type:    "SELECT",
		Columns: []string{"?s", "?p"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
		},
		Filters: []Filter{
			{Expression: "?s = \"test\""},
		},
		Limit:  10,
		Offset: 5,
	}

	prepared := &Prepare{plan: original}
	copy := prepared.copyPlan()

	// Verify it's a copy, not the same reference
	if copy == original {
		t.Error("copyPlan() returned same reference, not a copy")
	}

	// Verify contents are equal
	if copy.Type != original.Type {
		t.Errorf("copyPlan() Type = %v, want %v", copy.Type, original.Type)
	}

	if len(copy.Columns) != len(original.Columns) {
		t.Errorf("copyPlan() Columns length = %v, want %v", len(copy.Columns), len(original.Columns))
	}

	if copy.Limit != original.Limit {
		t.Errorf("copyPlan() Limit = %v, want %v", copy.Limit, original.Limit)
	}
}

func TestPrepare_GetVariables(t *testing.T) {
	prepared := &Prepare{
		variables: []string{"param1", "param2", "param3"},
	}

	variables := prepared.GetVariables()
	if len(variables) != 3 {
		t.Errorf("GetVariables() = %v, want 3", len(variables))
	}

	for i, v := range variables {
		if v != prepared.variables[i] {
			t.Errorf("GetVariables()[%d] = %v, want %v", i, v, prepared.variables[i])
		}
	}
}

func TestPrepare_MarshalUnmarshal(t *testing.T) {
	original := &Prepare{
		plan: &Plan{
			Type:    "SELECT",
			Columns: []string{"?s", ":param"},
		},
		variables:    []string{"param"},
		placeholders: []Placeholder{{Name: "param", Type: "variable"}},
	}

	data, err := original.Marshal()
	if err != nil {
		t.Errorf("Marshal() error = %v", err)
		return
	}

	parser := NewParser()
	unmarshaled, err := UnmarshalPrepare(data, parser)
	if err != nil {
		t.Errorf("UnmarshalPrepare() error = %v", err)
		return
	}

	if unmarshaled.variables[0] != original.variables[0] {
		t.Errorf("UnmarshalPrepare() variables = %v, want %v", unmarshaled.variables, original.variables)
	}
}
