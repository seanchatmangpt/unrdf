package sparql

import (
	"context"
	"testing"

	"github.com/unrdf/knowd/internal/store"
)

func TestExecutor_NewExecutor(t *testing.T) {
	executor := NewExecutor()
	if executor == nil {
		t.Error("NewExecutor() returned nil")
	}
}

func TestExecutor_ExecuteSelect(t *testing.T) {
	store := &MockStore{
		quads: []store.Quad{
			{Subject: "http://example.org/s1", Predicate: "http://example.org/p1", Object: "http://example.org/o1", Graph: "default"},
			{Subject: "http://example.org/s2", Predicate: "http://example.org/p2", Object: "http://example.org/o2", Graph: "default"},
		},
	}

	executor := NewExecutor()
	plan := &Plan{
		Type:    "SELECT",
		Columns: []string{"?s", "?p", "?o"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
		},
	}

	result, err := executor.Execute(context.Background(), store, "sparql-select", plan)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
		return
	}

	if len(result.Rows) != 2 {
		t.Errorf("Execute() rows = %v, want 2", len(result.Rows))
	}

	if result.Kind != "sparql-select" {
		t.Errorf("Execute() kind = %v, want sparql-select", result.Kind)
	}
}

func TestExecutor_ExecuteAsk(t *testing.T) {
	store := &MockStore{
		quads: []store.Quad{
			{Subject: "http://example.org/s", Predicate: "http://example.org/p", Object: "http://example.org/o", Graph: "default"},
		},
	}

	executor := NewExecutor()
	plan := &Plan{
		Type: "ASK",
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
		},
	}

	result, err := executor.Execute(context.Background(), store, "sparql-ask", plan)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
		return
	}

	if len(result.Rows) != 1 {
		t.Errorf("Execute() rows = %v, want 1", len(result.Rows))
	}

	if result.Rows[0]["result"] != true {
		t.Errorf("Execute() result = %v, want true", result.Rows[0]["result"])
	}

	if result.Kind != "sparql-ask" {
		t.Errorf("Execute() kind = %v, want sparql-ask", result.Kind)
	}
}

func TestExecutor_ExecuteConstruct(t *testing.T) {
	store := &MockStore{
		quads: []store.Quad{
			{Subject: "http://example.org/s", Predicate: "http://example.org/p", Object: "http://example.org/o", Graph: "default"},
		},
	}

	executor := NewExecutor()
	plan := &Plan{
		Type: "CONSTRUCT",
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{{Subject: "?s", Predicate: "?p", Object: "?o"}}},
		},
	}

	result, err := executor.Execute(context.Background(), store, "sparql-construct", plan)
	if err != nil {
		t.Errorf("Execute() error = %v", err)
		return
	}

	if len(result.Rows) != 1 {
		t.Errorf("Execute() rows = %v, want 1", len(result.Rows))
	}

	row := result.Rows[0]
	if row["subject"] != "http://example.org/s" {
		t.Errorf("Execute() subject = %v, want http://example.org/s", row["subject"])
	}

	if result.Kind != "sparql-construct" {
		t.Errorf("Execute() kind = %v, want sparql-construct", result.Kind)
	}
}

func TestExecutor_ResolveTerm(t *testing.T) {
	executor := NewExecutor()

	tests := []struct {
		input string
		want  string
	}{
		{"?var", ""},
		{"<http://example.org/iri>", "http://example.org/iri"},
		{"\"literal\"", "\"literal\""},
	}

	for _, tt := range tests {
		got := executor.resolveTerm(tt.input)
		if got != tt.want {
			t.Errorf("resolveTerm(%v) = %v, want %v", tt.input, got, tt.want)
		}
	}
}

func TestExecutor_ApplyFilters(t *testing.T) {
	executor := NewExecutor()

	row := map[string]interface{}{
		"?x": "value1",
		"?y": "value2",
	}

	tests := []struct {
		filters []Filter
		want    bool
	}{
		{
			filters: []Filter{
				{Expression: "?x = \"value1\""},
			},
			want: true,
		},
		{
			filters: []Filter{
				{Expression: "?x = \"value1\""},
				{Expression: "?y = \"value2\""},
			},
			want: true,
		},
		{
			filters: []Filter{
				{Expression: "?x = \"wrong\""},
			},
			want: false,
		},
	}

	for _, tt := range tests {
		got := executor.applyFilters(row, tt.filters)
		if got != tt.want {
			t.Errorf("applyFilters(%v, %v) = %v, want %v", row, tt.filters, got, tt.want)
		}
	}
}

func TestExecutor_ApplySlice(t *testing.T) {
	executor := NewExecutor()

	rows := []map[string]interface{}{
		{"col1": "row1"},
		{"col1": "row2"},
		{"col1": "row3"},
		{"col1": "row4"},
		{"col1": "row5"},
	}

	tests := []struct {
		offset int
		limit  int
		want   int
	}{
		{0, 0, 5},  // no limit/offset
		{2, 0, 3},  // offset 2, no limit
		{0, 3, 3},  // no offset, limit 3
		{1, 2, 2},  // offset 1, limit 2
		{10, 5, 0}, // offset beyond length
	}

	for _, tt := range tests {
		got := executor.applySlice(rows, tt.offset, tt.limit)
		if len(got) != tt.want {
			t.Errorf("applySlice(%v, %v) = %v, want %v", tt.offset, tt.limit, len(got), tt.want)
		}
	}
}

func TestExecutor_ApplyDistinct(t *testing.T) {
	executor := NewExecutor()

	rows := []map[string]interface{}{
		{"col1": "value1", "col2": "valueA"},
		{"col1": "value1", "col2": "valueA"}, // duplicate
		{"col1": "value2", "col2": "valueB"},
		{"col1": "value1", "col2": "valueA"}, // another duplicate
		{"col1": "value3", "col2": "valueC"},
	}

	result := executor.applyDistinct(rows)

	if len(result) != 3 {
		t.Errorf("applyDistinct() = %v, want 3", len(result))
	}
}

func TestExecutor_ApplyGroupBy(t *testing.T) {
	executor := NewExecutor()

	rows := []map[string]interface{}{
		{"?group": "A", "?value": "1"},
		{"?group": "A", "?value": "2"},
		{"?group": "B", "?value": "3"},
		{"?group": "B", "?value": "4"},
	}

	result := executor.applyGroupBy(rows, []string{"?group"})

	if len(result) != 2 {
		t.Errorf("applyGroupBy() = %v, want 2", len(result))
	}
}

func TestExecutor_ApplyOrderBy(t *testing.T) {
	executor := NewExecutor()

	rows := []map[string]interface{}{
		{"?x": "3"},
		{"?x": "1"},
		{"?x": "2"},
	}

	orderBy := []OrderByItem{
		{Variable: "?x", Descending: false},
	}

	result := executor.applyOrderBy(rows, orderBy)

	if len(result) != 3 {
		t.Errorf("applyOrderBy() = %v, want 3", len(result))
	}

	// Check if sorted ascending
	if result[0]["?x"] != "1" || result[1]["?x"] != "2" || result[2]["?x"] != "3" {
		t.Errorf("applyOrderBy() not sorted correctly: %v", result)
	}
}

func TestExecutor_ApplyHaving(t *testing.T) {
	executor := NewExecutor()

	rows := []map[string]interface{}{
		{"?group": "A", "?count": "5"},
		{"?group": "B", "?count": "3"},
		{"?group": "C", "?count": "7"},
	}

	having := []Filter{
		{Expression: "?count > \"4\""},
	}

	result := executor.applyHaving(rows, having)

	if len(result) != 2 {
		t.Errorf("applyHaving() = %v, want 2", len(result))
	}
}
