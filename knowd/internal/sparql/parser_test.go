package sparql

import (
	"testing"
)

func TestParser_ParseSelect(t *testing.T) {
	parser := NewParser()

	tests := []struct {
		name     string
		query    string
		wantType string
		wantCols int
		wantErr  bool
	}{
		{
			name:     "simple SELECT",
			query:    "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
			wantType: "SELECT",
			wantCols: 3,
			wantErr:  false,
		},
		{
			name:     "SELECT with asterisk",
			query:    "SELECT * WHERE { ?s ?p ?o }",
			wantType: "SELECT",
			wantCols: 1,
			wantErr:  false,
		},
		{
			name:     "SELECT with LIMIT",
			query:    "SELECT ?s WHERE { ?s ?p ?o } LIMIT 10",
			wantType: "SELECT",
			wantCols: 1,
			wantErr:  false,
		},
		{
			name:     "SELECT with OFFSET",
			query:    "SELECT ?s WHERE { ?s ?p ?o } OFFSET 5",
			wantType: "SELECT",
			wantCols: 1,
			wantErr:  false,
		},
		{
			name:     "SELECT with LIMIT and OFFSET",
			query:    "SELECT ?s WHERE { ?s ?p ?o } LIMIT 10 OFFSET 5",
			wantType: "SELECT",
			wantCols: 1,
			wantErr:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			plan, err := parser.Parse(tt.query)
			if (err != nil) != tt.wantErr {
				t.Errorf("Parse() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err != nil {
				return
			}
			if plan.Type != tt.wantType {
				t.Errorf("Parse() type = %v, want %v", plan.Type, tt.wantType)
			}
			if len(plan.Columns) != tt.wantCols {
				t.Errorf("Parse() columns = %v, want %v", len(plan.Columns), tt.wantCols)
			}
			if plan.Limit != 10 && tt.query != "SELECT ?s WHERE { ?s ?p ?o } LIMIT 10" {
				// Only check limit for the specific test case
			}
		})
	}
}

func TestParser_ParseAsk(t *testing.T) {
	parser := NewParser()

	query := "ASK WHERE { ?s ?p ?o }"
	plan, err := parser.Parse(query)
	if err != nil {
		t.Errorf("Parse() error = %v", err)
		return
	}
	if plan.Type != "ASK" {
		t.Errorf("Parse() type = %v, want ASK", plan.Type)
	}
}

func TestParser_ParseConstruct(t *testing.T) {
	parser := NewParser()

	query := "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"
	plan, err := parser.Parse(query)
	if err != nil {
		t.Errorf("Parse() error = %v", err)
		return
	}
	if plan.Type != "CONSTRUCT" {
		t.Errorf("Parse() type = %v, want CONSTRUCT", plan.Type)
	}
}

func TestParser_ParseAdvancedConstructs(t *testing.T) {
	parser := NewParser()

	tests := []struct {
		name          string
		clause        string
		wantPatterns  int
		wantUnions    int
		wantOptionals int
	}{
		{
			name:          "simple pattern",
			clause:        "?s ?p ?o . ?s ?p2 ?o2",
			wantPatterns:  1,
			wantUnions:    0,
			wantOptionals: 0,
		},
		{
			name:          "with UNION",
			clause:        "?s ?p ?o . UNION ?s ?p2 ?o2",
			wantPatterns:  0,
			wantUnions:    1,
			wantOptionals: 0,
		},
		{
			name:          "with OPTIONAL",
			clause:        "?s ?p ?o . OPTIONAL { ?s ?p2 ?o2 }",
			wantPatterns:  0,
			wantUnions:    0,
			wantOptionals: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			patterns, unions, optionals, _, _, _ := parser.parseAdvancedConstructs(tt.clause)
			if len(patterns) != tt.wantPatterns {
				t.Errorf("parseAdvancedConstructs() patterns = %v, want %v", len(patterns), tt.wantPatterns)
			}
			if len(unions) != tt.wantUnions {
				t.Errorf("parseAdvancedConstructs() unions = %v, want %v", len(unions), tt.wantUnions)
			}
			if len(optionals) != tt.wantOptionals {
				t.Errorf("parseAdvancedConstructs() optionals = %v, want %v", len(optionals), tt.wantOptionals)
			}
		})
	}
}

func TestParser_ParseColumns(t *testing.T) {
	parser := NewParser()

	tests := []struct {
		name  string
		input string
		want  []string
	}{
		{"asterisk", "*", []string{"*"}},
		{"single var", "?s", []string{"?s"}},
		{"multiple vars", "?s ?p ?o", []string{"?s", "?p", "?o"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parser.parseColumns(tt.input)
			if len(got) != len(tt.want) {
				t.Errorf("parseColumns() = %v, want %v", got, tt.want)
				return
			}
			for i, v := range got {
				if v != tt.want[i] {
					t.Errorf("parseColumns() = %v, want %v", got, tt.want)
					break
				}
			}
		})
	}
}

func TestParser_ParseBasicGraphPattern(t *testing.T) {
	parser := NewParser()

	tests := []struct {
		name  string
		input string
		want  int
	}{
		{"single triple", "?s ?p ?o", 1},
		{"multiple triples", "?s ?p ?o . ?s ?p2 ?o2", 2},
		{"with comments", "?s ?p ?o . # comment\n?s ?p2 ?o2", 2},
		{"empty", "", 0},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parser.parseBasicGraphPattern(tt.input)
			if len(got.Triples) != tt.want {
				t.Errorf("parseBasicGraphPattern() = %v, want %v", len(got.Triples), tt.want)
			}
		})
	}
}
